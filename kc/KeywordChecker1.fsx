#r "nuget: FSharp.Data"
#r "nuget: AngleSharp"
#r "nuget: AngleSharp.Js"
#r "nuget: AngleSharp.IO"

open System
open System.Text
open System.Text.RegularExpressions
open System.Linq
open FSharp.Data
open System.Threading
open AngleSharp
open AngleSharp.Io
open System.Net
(******************************************************************************)
let columnName = "companyhomepageurl"

let words =
    set [ "Contact"
          "Demo"
          "Contact Sales"
          "Talk to Sales"
          "Talk to an Expert" ]

(*******************************************************************************)
let tick = "\u2713"
let cross = "\u1763"
ServicePointManager.DefaultConnectionLimit <- 5
ServicePointManager.ServerCertificateValidationCallback <- (fun _ _ _ _ -> true)
let wordBoundary = @"\b"

let r =
    new DefaultHttpRequester("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:100.0) Gecko/20100101 Firefox/100.0")

r.Timeout <- TimeSpan.FromSeconds(30)

let config = Configuration.Default.WithDefaultLoader()

let color = Console.ForegroundColor
let quote = '"'
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let args = fsi.CommandLineArgs |> Array.tail

let getLanguage (html: string) =
    async {
        try
            let doc = HtmlDocument.Parse(html)

            let attr =
                doc.CssSelect("html")
                |> List.choose (fun x ->
                    x.TryGetAttribute("lang")
                    |> Option.map (fun a -> a.Value().Trim()))

            let lang =
                match attr with
                | [ x ] -> x
                | [] -> ""
                | _ -> String.Join(",", (attr |> Set.ofSeq))

            return $"{quote}{lang}{quote}"
        with
        | _ -> return ""
    }

let fetchUrl (url: string) =
    async {
        try
            use! doc =
                BrowsingContext.New(config).OpenAsync(url)
                |> Async.AwaitTask

            return doc.ToHtml()
        with
        | _ -> return String.Empty
    }

let checkWords (html: string) =
    let wordsFound =
        words
        |> Set.filter (fun word ->
            Regex.IsMatch(
                html,
                $"{wordBoundary}{word}{wordBoundary}",
                RegexOptions.IgnoreCase
                ||| RegexOptions.Singleline
            ))

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
let locker = obj ()

let findWords (csv: CsvFile) (newFile: string) =
    counter <- 0
    let listOfRows = csv.Rows |> List.ofSeq
    let seperators = csv.Separators

    let headers =
        match csv.Headers with
        | Some h -> [| "keywords"; "language"; yield! h |]
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
                    let! language = getLanguage (html)

                    let arr =
                        [| words
                           language
                           yield! row.Columns |]
                        |> Array.map (fun x ->
                            let s = x.Trim(quote)
                            String.Format("{0}{1}{0}", quote, s))

                    lock locker (fun _ ->
                        let c, symbol =
                            if not (String.IsNullOrWhiteSpace(words.Trim('\"'))) then
                                ConsoleColor.DarkGreen, tick
                            else
                                ConsoleColor.DarkRed, cross

                        Console.ForegroundColor <- c
                        printf "Found %s " symbol
                        Console.ForegroundColor <- color
                        printf "Processed:"
                        let cnt = Interlocked.Increment(&counter)

                        Console.ForegroundColor <- ConsoleColor.Yellow
                        printf "%3d/%3d" cnt totalRows
                        Console.ForegroundColor <- color

                        Console.ForegroundColor <- ConsoleColor.Magenta
                        printf "  %-s" website
                        Console.ForegroundColor <- color

                        Console.ForegroundColor <- ConsoleColor.DarkCyan
                        printf " %-s" language
                        Console.ForegroundColor <- color

                        Console.ForegroundColor <- ConsoleColor.DarkGray
                        printf " %-s" words
                        Console.ForegroundColor <- color
                        Console.WriteLine())

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
