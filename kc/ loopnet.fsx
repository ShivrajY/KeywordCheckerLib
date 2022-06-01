#r "nuget: FSharp.Data"
#r "nuget: PuppeteerSharp"
#r "nuget: AngleSharp"
#r "nuget: AngleSharp.XPath"

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
open AngleSharp.Html.Parser
open AngleSharp.XPath
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

let webRequestGate = RequestGate(3)

let tick = "\u2713"
let cross = "\u1763"

let color = Console.ForegroundColor
let quote = '"'
let wordBoundary = @"\b"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let currentDirectory = __SOURCE_DIRECTORY__
let downloadPath = Path.Combine(currentDirectory, "Chromium")

let config = Configuration.Default.WithDefaultLoader()


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
            //let options = new LaunchOptions(Headless = true, ExecutablePath = executablePath)
            let options = new LaunchOptions(Headless = false, ExecutablePath = executablePath)
            return! Puppeteer.LaunchAsync(options) |> Async.AwaitTask
        else
            return! failwith "Couldn't find Chromium executable"
    }

let parseData (html: string) =
    async {
        use context = BrowsingContext.New(config)
        let parser = context.GetService<IHtmlParser>()
        use! doc = parser.ParseDocumentAsync(html) |> Async.AwaitTask

        return doc
    }

let fetchUrl (browser: Browser) (url: string) =
    async {
        try
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
        with
        | _ -> return String.Empty
    }

let work () =
    async {
        let! browser = SetUpChromeAndGetBrowser()

        try
            use! page = browser.NewPageAsync() |> Async.AwaitTask

            let urls =
                //Next pagenavigate_next
                Seq.initInfinite (fun i ->
                    $"https://www.sourcewell-mn.gov/member-lookup?location=&title=&field_type=All&page={i}")
                |> Seq.take (10)

            do!
                page.SetViewportAsync(viewport = new ViewPortOptions(Width = 800, Height = 600))
                |> Async.AwaitTask

            let js =
                """[...[...document.querySelectorAll(".member-lookup__item > .container-fluid")].map(x=>x.innerText)]"""

            let mutable cnt = 1

            for url in urls do
                printfn ">> Page %d <<" cnt
                cnt <- cnt + 1
                printfn "_________________________"

                do!
                    page.GoToAsync(url)
                    |> Async.AwaitTask
                    |> Async.Ignore

                let! nodes =
                    page.EvaluateExpressionAsync<string []>(js)
                    |> Async.AwaitTask

                for node in nodes do
                    printfn "%s" node
                    printfn ""

                printfn ""

            ()

        finally
            browser.CloseAsync()
            |> Async.AwaitTask
            |> Async.RunSynchronously

    //return! browser.CloseAsync() |> Async.AwaitTask
    }

work () |> Async.RunSynchronously
