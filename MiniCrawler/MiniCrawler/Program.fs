module MiniCrawler

open System
open System.Net.Http
open System.Text.RegularExpressions

/// extracts only http:// links from html
let extractLinks (html: string) : string list = 
    let rx = Regex("<a\\s+[^>]*href=\"(http://[^\"]+)\"", RegexOptions.IgnoreCase)
    rx.Matches(html)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Groups.[1].Value)
    |> Seq.distinct
    |> Seq.toList

/// downloads a page async using HttpClient
let private download (client: HttpClient) (url: string) = async {
    let! s = client.GetStringAsync(url) |> Async.AwaitTask
    return s
}

/// downloads root page, extract links, download each in parallel and reports sizes
let crawlAndReportWith (client: HttpClient) (printer: string -> unit) (url: string) = async {
    let! main = download client url
    let links = extractLinks main

    let! results =
        links
        |> List.map (fun u -> async {
            try
                let! content = download client u
                return Some (u, content.Length)
            with ex ->
                printer (sprintf "Failed: %s (%s)" u ex.Message)
                return None
        })
        |> Async.Parallel

    results
    |> Array.choose id
    |> Array.iter (fun (u, len) -> printer (sprintf "%s - %d" u len))
}

/// wrapper that creates its own HttpClient and prints to console
let crawlAndPrint (url: string) =
    async {
        use client = new HttpClient()
        return! crawlAndReportWith client (printfn "%s") url
    }
