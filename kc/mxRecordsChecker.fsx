#r "nuget: DnsClient, 1.6.0"
#r "nuget: FSharp.Data"

open System
open System.Text
open System.Linq
open DnsClient
open FSharp.Data


let columnName = "companyhomepageurl"

let args = fsi.CommandLineArgs |> Array.tail

let options = new LookupClientOptions()
options.ContinueOnDnsError <- true
let client = new LookupClient(options)

let getMxExchange (client: LookupClient) (hostName: string) =
    async {
        let host =
            hostName
                .Replace("https://", String.Empty)
                .Replace("http://", String.Empty)
                .Replace("www.", String.Empty)
                .Replace("/", String.Empty)

        let! r =
            client.QueryAsync(host, QueryType.MX)
            |> Async.AwaitTask

        return
            r
                .Answers
                .MxRecords()
                .OrderBy(fun x -> x.Preference)
            |> Seq.tryHead
            |> Option.map (fun x -> x.Exchange)
    }

let testHost = getMxExchange client

let color = Console.ForegroundColor

let createCsv file =
    if (String.IsNullOrEmpty(file)) then
        failwith "File name is empty"
    else
        let csv = CsvFile.Load(file, ignoreErrors = true)
        csv

let mutable counter = 0
let quote = '"'

let findWords (csv: CsvFile) (newFile: string) =
    counter <- 0
    let listOfRows = csv.Rows |> List.ofSeq
    let seperators = csv.Separators

    let headers =
        match csv.Headers with
        | Some h -> [| "MXRecord"; yield! h |]
        | None -> [||]

    let seed =
        (StringBuilder())
            .AppendJoin(seperators, headers)
            .AppendLine()

    let tasks =
        listOfRows
        |> List.mapi (fun index row ->
            async {
                try
                    let website = row[columnName]
                    let! mxRecord = testHost website

                    let mxStr =
                        match mxRecord with
                        | Some x -> sprintf "%A" x
                        | None -> String.Empty

                    let arr =
                        [| mxStr; yield! row.Columns |]
                        |> Array.map (fun x ->
                            let s = x.Trim(quote)
                            String.Format("{0}{1}{0}", quote, s))

                    printf "%s: %-s" website mxStr

                    let str = String.Join(seperators, arr)
                    return index, str
                with
                | _ -> return index, String.Empty
            })

    let results = tasks |> Async.Parallel |> Async.RunSynchronously

    let sortedResult = results |> Array.sortBy fst |> Array.map snd
    let sb = seed.AppendLine(String.Join(Environment.NewLine, sortedResult))
    let newCsv = CsvFile.Parse(sb.ToString())
    newCsv.Save(newFile)

    Console.ForegroundColor <- ConsoleColor.Green
    printfn "Job Done!"
    printfn $"Saved File: {newFile}"
    Console.ForegroundColor <- color

let work () =
    match args with
    | [||] -> printfn "Please provide a file name"
    | _ ->
        let fileName = args.[0]

        if not (fileName.EndsWith(".csv")) then
            Console.ForegroundColor <- ConsoleColor.Red
            printfn "Please provide a csv file"
            Console.ForegroundColor <- color
        else
            try
                let csvObj = createCsv (fileName)

                match csvObj.Headers with
                | None ->
                    Console.ForegroundColor <- ConsoleColor.Red
                    printfn "csv file doesn't have headers!"
                    Console.ForegroundColor <- color
                | Some h ->
                    if not (h.Contains(columnName)) then
                        Console.ForegroundColor <- ConsoleColor.Red
                        printfn "csv file doesn't have column name: %s" columnName
                        Console.ForegroundColor <- ConsoleColor.Red
                    else
                        let newFile = fileName.Replace(".csv", "_new_mxrecord.csv")
                        findWords (csvObj) (newFile)
            with
            | ex ->
                Console.ForegroundColor <- ConsoleColor.Red
                printfn "Error(%A)" ex
                Console.ForegroundColor <- ConsoleColor.Red

work ()
