module kc.KeywordChecker

open System
open System.Threading
open System.Threading.Tasks
open FSharp.Data
open FSharp.Data.CsvExtensions
open AngleSharp
open AngleSharp.Css
open AngleSharp.Js
open AngleSharp.Io
open AngleSharp.Html.Parser

let getCsvObject (csvFile: string) =
    async {
        if (String.IsNullOrEmpty(csvFile)) then
            return! failwith "No file specified"
        else
            return! (CsvFile.AsyncLoad(csvFile))
    }

let getWebsitesLinks (columnName: string) (csvFile: CsvFile) =
    async {
        let cn =
            if (String.IsNullOrEmpty(columnName)) then
                "companyhomepageurl"
            else
                columnName

        let websites =
            csvFile.Rows
            |> Seq.map (fun row ->
                let website = row?cn

                match (Uri.TryCreate(website, UriKind.Absolute)) with
                | true, uri -> uri.ToString()
                | false, _ -> $"https://www.{website}")

        return websites
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

let getHtmlDocument (website: string, cancellationToken: CancellationToken) =
    async {
        use context = BrowsingContext.New(config)

        use! doc =
            context.OpenAsync(website, cancellationToken)
            |> Async.AwaitTask

        return doc
    }
//"Demo" OR "Contact Sales" OR "Talk to Sales" OR "Talk to an Expert"
