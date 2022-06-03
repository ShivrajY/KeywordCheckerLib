#r "nuget: DnsClient, 1.6.0"
#r "nuget: FSharp.Data"

open System
open System.Linq
open DnsClient

let options = new LookupClientOptions()
options.ContinueOnDnsError <- true
let client = new LookupClient(options)

let getMxExchange (client: LookupClient) (hostName: string) =
    async {

        let! r =
            client.QueryAsync(hostName, QueryType.MX)
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

printfn "%A" (testHost "roofr.com" |> Async.RunSynchronously)
