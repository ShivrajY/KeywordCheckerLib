#r "nuget: DnsClient, 1.6.0"
#r "nuget: FSharp.Data"

open System
open System.Text
open System.Linq
open DnsClient

let options = new LookupClientOptions()
options.ContinueOnDnsError <- true
let client = new LookupClient(options)

let getMxExchange (client: LookupClient) (hostName: string) =
    async {
        //"https://www.actualhq.com"
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

printfn
    "%A"
    (testHost "https://www.actualhq.com"
     |> Async.RunSynchronously)
