#r "nuget: FSharp.Data"
#r "nuget: PuppeteerSharp"
#r "nuget: AngleSharp"

open System
open System.Text
open System.Text.RegularExpressions
open System.Linq
open FSharp.Data
open System.Threading
open PuppeteerSharp
open System.IO
open AngleSharp
open AngleSharp.Dom
open AngleSharp.Io
open AngleSharp.Html.Parser
open System.Net
open System.Net.Http
(******************************************************************************)
let columnName = "companyhomepageurl"

let words =
    set [ "Contact"
          "Demo"
          "Contact Sales"
          "Talk to Sales"
          "Talk to an Expert" ]

(*******************************************************************************)
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
//Number of simultaneous pages
let numberOfPages = 3
let webRequestGate = RequestGate(numberOfPages)

let tick = "\u2713"
let cross = "\u1763"

//Constants
let color = Console.ForegroundColor
let quote = '"'
let wordBoundary = @"\b"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let currentDirectory = __SOURCE_DIRECTORY__
let downloadPath = Path.Combine(currentDirectory, "Chromium")
let config = Configuration.Default.WithDefaultLoader()

/// <summary>
/// Setup Chrome options
/// </summary>
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
            //let options = new LaunchOptions(Headless = false, ExecutablePath = executablePath)
            return! Puppeteer.LaunchAsync(options) |> Async.AwaitTask
        else
            return! failwith "Couldn't find Chromium executable"
    }

//Arguments
let args = fsi.CommandLineArgs |> Array.tail

/// <summary>
/// Setup the browser
/// </summary>
/// <returns>Page language</returns>
/// <param name="html">page html</param>
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
    | Meta of name: string

let pageLanguage = Attribute(tag = "html", attribute = "lang")
let metaDescription = Meta(name = "description")

/// <summary>
/// Parse the page
/// </summary>
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
                    | Meta (name) ->
                        let m = doc.QuerySelector($"meta[name={quote}{name}{quote}]")

                        if (m = null) then
                            String.Empty
                        else
                            let v = m.GetAttribute("content")
                            if (v = null) then String.Empty else v)

            return data
        else
            return []
    }

/// <summary>
/// Get page DOM html
/// </summary>
let fetchUrl (browser: Browser) (url: string) =
    async {
        try
            if not (String.IsNullOrWhiteSpace(url)) then
                use! holder = webRequestGate.AcquireAsync()
                use! page = browser.NewPageAsync() |> Async.AwaitTask

                do!
                    page.GoToAsync(url, waitUntil = [| WaitUntilNavigation.Networkidle0 |])
                    |> Async.AwaitTask
                    |> Async.Ignore

                let! html =
                    page.EvaluateExpressionAsync("document.documentElement.outerHTML")
                    |> Async.AwaitTask

                return html.ToString()
            else
                return String.Empty
        with
        | _ -> return String.Empty
    }

/// <summary>
/// Checks words inside DOM
/// </summary>
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

/// <summary>
/// Create CSV object
/// </summary>
let createCsv file =
    if (String.IsNullOrEmpty(file)) then
        failwith "File name is empty"
    else
        let csv = CsvFile.Load(file, ignoreErrors = true)
        csv

// To handle Server certificate errors
ServicePointManager.ServerCertificateValidationCallback <- (fun sender certificate chain sslPolicyErrors -> true)

let handler = new HttpClientHandler()
handler.AllowAutoRedirect <- true
let httpClient = new HttpClient(handler)

httpClient.DefaultRequestHeaders.UserAgent.ParseAdd(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"
)

/// <summary>
/// Create url based on httpClient redirect
/// </summary>
let createUrl (link: string) =
    async {
        let url =
            if (link.StartsWith("http")) then
                link
            else
                $"https://www.{link}"

        try
            use! response =
                httpClient.GetAsync(url, HttpCompletionOption.ResponseHeadersRead)
                |> Async.AwaitTask

            if (response.IsSuccessStatusCode) then
                let x = response.RequestMessage.RequestUri.ToString()
                return x
            else
                return $"http://www.{link}"
        with
        | _ -> return String.Empty
    }

let mutable counter = 0
let locker = obj ()

/// <summary>
/// Find words
/// </summary>
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
                try
                    let website = row[columnName]
                    let! url = createUrl (website)
                    let! html = fetchUrl browser (url)
                    let words = checkWords (html)
                    let! language = getLanguage (html)
                    let! data = parseData (html) (pageLanguage :: metaDescription :: [])

                    let pl, metaDescription =
                        match data with
                        | l :: d :: [] when String.IsNullOrWhiteSpace(l) ->
                            language, $"{quote}{d.Replace(',', ' ')}{quote}"
                        | [ lang; desc ] -> lang, $"{quote}{desc.Replace(',', ' ')}{quote}"
                        | _ -> language, String.Empty

                    let arr =
                        [| words
                           pl
                           metaDescription
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

                        Console.ForegroundColor <- ConsoleColor.DarkYellow
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

                        Console.ForegroundColor <- ConsoleColor.Yellow
                        printf " %-s" metaDescription
                        Console.ForegroundColor <- color

                        Console.WriteLine())

                    let str = String.Join(seperators, arr)
                    return index, str
                with
                | _ -> return index, String.Empty
            })

    let results = tasks |> Async.Parallel |> Async.RunSynchronously

    browser.CloseAsync() |> ignore

    let sortedResult = results |> Array.sortBy fst |> Array.map snd
    let sb = seed.AppendLine(String.Join(Environment.NewLine, sortedResult))
    let newCsv = CsvFile.Parse(sb.ToString())
    newCsv.Save(newFile)

    Console.ForegroundColor <- ConsoleColor.Green
    printfn "Job Done!"
    printfn $"Saved File: {newFile}"
    Console.ForegroundColor <- color

/// <summary>
/// Work begins here
/// </summary>
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

                        let browser =
                            SetUpChromeAndGetBrowser()
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

work ()
