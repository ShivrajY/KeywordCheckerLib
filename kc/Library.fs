module kc.KeywordChecker

open System
open System.Threading
open FSharp.Data
open FSharp.Data.CsvExtensions
open AngleSharp
open AngleSharp.Io

let getCsvObject (csvFile: string) =
    async {
        if (String.IsNullOrEmpty(csvFile)) then
            return! failwith "No file specified"
        else
            return! (CsvFile.AsyncLoad(csvFile, ignoreErrors = true))
    }
//"companyhomepageurl"
let getWebsitesLinks (columnName: string) (csvFile: CsvFile) =
    async {
        let websites =
            csvFile.Rows
            |> Seq.map (fun row ->
                let website = row?columnName

                match (Uri.TryCreate(website, UriKind.Absolute)) with
                | true, uri -> uri.ToString()
                | false, _ -> $"https://www.{website}")

        return websites |> Set.ofSeq
    }

let htmlRequester = new DefaultHttpRequester()
htmlRequester.Timeout <- TimeSpan.FromSeconds(100)

htmlRequester.Headers[ "User-Agent" ] <-
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36"

let config =
    Configuration
        .Default
        .WithDefaultLoader()
        .WithRequester(htmlRequester)
        .WithCss()
        .WithJs()

let checkWords (words: Set<string>) (cancellationToken: CancellationToken) (website: string) =
    async {
        use context = BrowsingContext.New(config)

        use! doc =
            context.OpenAsync(website, cancellationToken)
            |> Async.AwaitTask

        let html =
            if not (String.IsNullOrEmpty(doc.DocumentElement.TextContent)) then
                doc.DocumentElement.TextContent
            else if not (String.IsNullOrEmpty(doc.Body.OuterHtml)) then
                doc.Body.OuterHtml
            else
                ""

        return
            words
            |> Set.map (fun word ->
                html
                    .ToLowerInvariant()
                    .Contains(word.ToLowerInvariant()),
                word)
            |> Set.fold
                (fun (acc: string) (x: bool * string) ->
                    let t, w = x
                    if (t) then $"{acc},{w}" else acc)
                String.Empty
    }

let c =
    getCsvObject "/Users/shiv/Projects/kc/SeedList.csv"
    |> Async.RunSynchronously
    |> getWebsitesLinks "companyhomepageurl"
    |> Async.RunSynchronously

let words =
    set [ "Contact"
          "Demo"
          "Contact Sales"
          "Talk to Sales"
          "Talk to an Expert" ]

let check site =
    Seq.map (fun ww -> checkWords words (CancellationToken.None) site)

//"Demo" OR "Contact Sales" OR "Talk to Sales" OR "Talk to an Expert"
