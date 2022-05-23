#r "nuget: FSharp.Data"
#r "nuget: AngleSharp"
#r "nuget: AngleSharp.Js"
#r "nuget: AngleSharp.IO"

open System
open System.Text
open System.Linq
open FSharp.Data
open AngleSharp.Js
open System.Threading
open AngleSharp
open AngleSharp.Dom
open AngleSharp.Io
open System.Net
open System.Net.Security

ServicePointManager.DefaultConnectionLimit <- 100
ServicePointManager.ServerCertificateValidationCallback <- (fun _ _ _ _ -> true)

let r =
    new DefaultHttpRequester("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:100.0) Gecko/20100101 Firefox/100.0")

r.Timeout <- TimeSpan.FromSeconds(60)

let config =
    Configuration
        .Default
        .WithRequester(r)
        .WithDefaultLoader(LoaderOptions(IsResourceLoadingEnabled = true))
        .WithJs()
        .WithEventLoop()

let columnName = "companyhomepageurl"

let words =
    set [ "Contact"
          "Demo"
          "Contact Sales"
          "Talk to Sales"
          "Talk to an Expert" ]

(*******************************************************************************)

let color = Console.ForegroundColor
let quote = '"'
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let args = fsi.CommandLineArgs |> Array.tail

type RequestGate(n: int) =
    let semaphore = new Semaphore(initialCount = n, maximumCount = n)

    member _.AcquireAsync(?timeout) =
        async {
            let! ok = Async.AwaitWaitHandle(semaphore, ?millisecondsTimeout = timeout)

            if ok then
                return
                    { new IDisposable with
                        member x.Dispose() = semaphore.Release() |> ignore }
            else
                return! failwith "Semaphore couldn't be aquired..."

        }

let webRequestGate = RequestGate(20)

let fetchUrl (url: string) =
    async {
        use! gate = webRequestGate.AcquireAsync()

        try
            use! doc =
                BrowsingContext
                    .New(config)
                    .OpenAsync(url)
                    .WaitUntilAvailable()
                |> Async.AwaitTask

            return doc.ToHtml()
        with
        | _ -> return String.Empty
    }

let checkWords (html: string) =
    let wordsFound =
        words
        |> Set.filter (fun word ->
            html
                .ToLowerInvariant()
                .Contains(word.ToLowerInvariant()))

    String.Format("{0}{1}{0}", quote, (String.Join(", ", wordsFound)))

let createCsv file =
    if (String.IsNullOrEmpty(file)) then
        failwith "File name is empty"
    else
        let csv = CsvFile.Load(file, ignoreErrors = true)
        csv

let createUrl (link: string) =
    if (link.StartsWith("http")) then
        link
    else
        $"http://{link}"

let mutable counter = 0

let findWords (csv: CsvFile) (newFile: string) =
    counter <- 0
    let listOfRows = csv.Rows |> List.ofSeq
    let seperators = csv.Separators

    let headers =
        match csv.Headers with
        | Some h -> [| "keywords"; yield! h |]
        | None -> [||]


    let seed =
        (StringBuilder())
            .AppendJoin(seperators, headers)
            .AppendLine()

    let totalRows = listOfRows.Length

    let tasks =
        listOfRows
        |> List.mapi (fun index row ->
            async {
                try
                    let website = row[columnName]
                    let url = createUrl (website)
                    let! html = fetchUrl (url)
                    let words = checkWords (html)

                    let arr =
                        [| words; yield! row.Columns |]
                        |> Array.map (fun x ->
                            let s = x.Trim(quote)
                            String.Format("{0}{1}{0}", quote, s))

                    printf "Processing: "
                    Console.ForegroundColor <- ConsoleColor.Magenta
                    printf "%-s" website
                    printf "%-s" " "
                    Console.ForegroundColor <- color
                    let cnt = Interlocked.Increment(&counter)
                    Console.ForegroundColor <- ConsoleColor.Yellow
                    printf "%-d/%-d" cnt totalRows
                    Console.ForegroundColor <- color
                    Console.WriteLine()

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
                        let newFile = fileName.Replace(".csv", "_new.csv")
                        findWords (csvObj) (newFile)
            with
            | ex ->
                Console.ForegroundColor <- ConsoleColor.Red
                printfn "Error(%A)" ex
                Console.ForegroundColor <- ConsoleColor.Red

work ()
