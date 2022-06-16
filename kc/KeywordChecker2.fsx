#r "nuget: FSharp.Data"
#r "nuget: PuppeteerSharp"
#r "nuget: AngleSharp"

open System
open System.Text
open System.Text.RegularExpressions
open System.Linq
open System.Threading
open System.IO
open System.Net
open System.Net.Http
open FSharp.Data
open PuppeteerSharp
open AngleSharp
open AngleSharp.Html.Parser

module Setup =
    let columnName = "companyhomepageurl"

    let words =
        set [ "Contact"
              "Demo"
              "Contact Sales"
              "Talk to Sales"
              "Talk to an Expert" ]

    (******************************************************************************************)

    let tick = "\u2713"
    let cross = "\u1763"
    let color = Console.ForegroundColor
    let quote = '"'
    let wordBoundary = @"\b"
    let chromeFolder = "Chromium"
    Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
    let currentDirectory = __SOURCE_DIRECTORY__
    let downloadPath = Path.Combine(currentDirectory, chromeFolder)
    let config = Configuration.Default.WithDefaultLoader()


    // To handle Server certificate errors
    ServicePointManager.ServerCertificateValidationCallback <- (fun sender certificate chain sslPolicyErrors -> true)

    let handler = new HttpClientHandler()
    handler.AllowAutoRedirect <- true
    let httpClient = new HttpClient(handler)

    httpClient.DefaultRequestHeaders.UserAgent.ParseAdd(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"
    )

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

let threads = 3
let webRequestGate = RequestGate(threads)

module Chrome =
    open Setup

    let SetUpChromeAndGetBrowser () =
        async {
            Console.WriteLine($"Attemping to set up puppeteer to use Chromium found under directory {downloadPath} ")

            if not (Directory.Exists(downloadPath)) then
                Console.WriteLine("Custom directory not found. Creating directory")
                Directory.CreateDirectory(downloadPath) |> ignore

            Console.WriteLine("Downloading Chromium")
            let browserFetcherOptions = new BrowserFetcherOptions(Path = downloadPath)
            let browserFetcher = new BrowserFetcher(browserFetcherOptions)

            let! revisionInfo =
                browserFetcher.DownloadAsync(BrowserFetcher.DefaultChromiumRevision)
                |> Async.AwaitTask

            let executablePath = browserFetcher.GetExecutablePath(revisionInfo.Revision)

            if not (String.IsNullOrEmpty(executablePath)) then
                Console.WriteLine($"Attemping to start Chromium using executable path: {executablePath}")
                let options = new LaunchOptions(Headless = true, ExecutablePath = executablePath)

                return! Puppeteer.LaunchAsync(options) |> Async.AwaitTask
            else
                return! failwith "Couldn't find Chromium executable"
        }

    let fetchUrl (browser: Browser) (url: string) =
        async {

            try
                if not (String.IsNullOrWhiteSpace(url)) then
                    let! page = browser.NewPageAsync() |> Async.AwaitTask
                    let timeout = int (TimeSpan.FromSeconds(60).TotalMilliseconds)
                    page.DefaultNavigationTimeout <- timeout
                    page.DefaultTimeout <- timeout

                    try
                        do!
                            page.GoToAsync(url, timeout, waitUntil = [| WaitUntilNavigation.DOMContentLoaded |])
                            |> Async.AwaitTask
                            |> Async.Ignore

                        let! html =
                            page.EvaluateExpressionAsync("document.documentElement.outerHTML")
                            |> Async.AwaitTask

                        return Some(page, html.ToString())
                    with
                    | _ ->
                        page.CloseAsync()
                        |> Async.AwaitTask
                        |> Async.RunSynchronously

                        return None
                else
                    return None

            with
            | _ -> return None
        }

module Csv =
    let createCsv file =
        if (String.IsNullOrEmpty(file)) then
            failwith "File name is empty"
        else
            let csv = CsvFile.Load(file, ignoreErrors = true)
            csv

module HttpHelper =
    open Setup

    let createUrl (link: string) =
        async {
            let url =
                if (link.StartsWith("http")) then
                    link
                else
                    $"https://{link}"

            try
                use! response =
                    httpClient.GetAsync(url, HttpCompletionOption.ResponseHeadersRead)
                    |> Async.AwaitTask

                if (response.IsSuccessStatusCode) then
                    let x = response.RequestMessage.RequestUri.ToString()
                    return x
                else
                    return $"http://{link}"
            with
            | _ -> return String.Empty
        }

let args = fsi.CommandLineArgs |> Array.tail

module SiteStats =
    open Setup
    let mutable counter = 0
    let locker = obj ()

    let checkWords (html: string) =
        if not (String.IsNullOrWhiteSpace(html)) then
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
        else
            "\"\""

    let getLanguage (html: string) =
        async {
            if not (String.IsNullOrEmpty(html)) then
                let m =
                    Regex.Match(
                        html,
                        "lang=\"(.*?)\"",
                        RegexOptions.IgnoreCase
                        ||| RegexOptions.Singleline
                    )

                if (m.Success) then
                    return m.Groups[1].Value
                else
                    return String.Empty
            else
                return String.Empty
        }

    type InputToParse =
        | Attribute of tag: string * attribute: string
        | Meta

    let parseData (html: string) (inputsToParse: InputToParse list) =
        async {
            if not (String.IsNullOrEmpty(html)) then
                use context = BrowsingContext.New(config)
                let parser = context.GetService<IHtmlParser>()
                use! doc = parser.ParseDocumentAsync(html) |> Async.AwaitTask

                let data =
                    inputsToParse
                    |> List.map (fun input ->
                        match input with
                        | Attribute (tag, attribute) ->
                            let a = doc.QuerySelector($"{tag}[{attribute}]")

                            if (a = null) then
                                String.Empty
                            else
                                let v = a.GetAttribute(attribute)
                                if (v = null) then String.Empty else v
                        | Meta ->
                            let m = doc.QuerySelector($"meta[name='description']")

                            if (m = null) then
                                let temp = doc.QuerySelector("meta[property='og:description']")

                                if (temp = null) then
                                    String.Empty
                                else
                                    let v = temp.GetAttribute("content")
                                    if (v = null) then String.Empty else v
                            else
                                let v = m.GetAttribute("content")
                                if (v = null) then String.Empty else v)

                return data
            else
                return []
        }

    let findWords (browser: Browser) (csv: CsvFile) (newFile: string) =
        counter <- 0
        let listOfRows = csv.Rows |> List.ofSeq
        let seperators = csv.Separators

        let headers =
            match csv.Headers with
            | Some h ->
                [| "keywords"
                   "language"
                   "meta-description"
                   yield! h |]
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

                    use! holder = webRequestGate.AcquireAsync()

                    try
                        let website = row[columnName]
                        let! url = HttpHelper.createUrl (website)
                        let! opt = Chrome.fetchUrl browser (url)

                        match opt with
                        | None ->
                            let arr =
                                [| ""; ""; ""; yield! row.Columns |]
                                |> Array.map (fun x ->
                                    let s = x.Trim(quote).Replace("\"", "\\\"")
                                    String.Format("{0}{1}{0}", quote, s))

                            let str = String.Join(seperators, arr)
                            printfn ">>>>>>>>>> :: Failed To Load :: %A <<<<<<<<<<" website
                            return index, str

                        | Some (page, html) ->
                            let words = checkWords (html)

                            try
                                let! lang =
                                    page.EvaluateExpressionAsync<string>("document.documentElement.lang")
                                    |> Async.AwaitTask

                                let! meta =
                                    page.EvaluateFunctionAsync<string>(
                                        """()=> {
                                            try{ 
                                                return document.querySelector('meta[name=description]').getAttribute('content');
                                                } catch(e)
                                                {
                                                try{ return document.querySelector('meta[property=og:description]').getAttribute('content');} 
                                                catch(e){return '';}
                                                }
                                            }
                                        """
                                    )
                                    |> Async.AwaitTask

                                page.CloseAsync()
                                |> Async.AwaitTask
                                |> Async.RunSynchronously

                                let arr =
                                    [| words
                                       lang
                                       meta
                                       yield! row.Columns |]
                                    |> Array.map (fun x ->
                                        let s = x.Trim(quote).Replace("\"", "\\\"")
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

                                    Console.ForegroundColor <- ConsoleColor.DarkYellow
                                    printf "%3d/%3d" cnt totalRows

                                    Console.ForegroundColor <- ConsoleColor.Magenta
                                    printf "  %-s" website

                                    Console.ForegroundColor <- ConsoleColor.DarkCyan
                                    printf " %-s" lang

                                    Console.ForegroundColor <- ConsoleColor.DarkGray
                                    printf " %-s" words

                                    Console.ForegroundColor <- ConsoleColor.Gray
                                    printf " %-s" meta

                                    Console.WriteLine())

                                let str = String.Join(seperators, arr)
                                return index, str
                            finally
                                if not (page.IsClosed) then
                                    page.Dispose()
                    with
                    | _ -> return index, String.Empty
                })

        let results = tasks |> Async.Parallel |> Async.RunSynchronously

        browser.CloseAsync() |> ignore

        let sortedResult = results |> Array.sortBy fst |> Array.map snd
        let sb = seed.AppendLine(String.Join(Environment.NewLine, sortedResult))
        File.WriteAllText(newFile, sb.ToString())
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
                    let csvObj = Csv.createCsv (fileName)

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

                            let browser =
                                Chrome.SetUpChromeAndGetBrowser()
                                |> Async.RunSynchronously

                            try
                                findWords browser (csvObj) (newFile)
                            finally
                                browser.CloseAsync()
                                |> Async.AwaitTask
                                |> Async.RunSynchronously
                with
                | ex ->
                    Console.ForegroundColor <- ConsoleColor.Red
                    printfn "Error(%A)" ex
                    Console.ForegroundColor <- ConsoleColor.Red

SiteStats.work ()
