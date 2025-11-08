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

/// downloads root page, extract links, download each in parallel and print sizes
let crawlAndPrintWith (client: HttpClient) (url: string) = async {
    let! main = download client url
    let links = extractLinks main

    let! results = 
        links
        |> List.map (fun u -> async {
            let! content = download client u
            return (u, content.Length)
        })
        |> Async.Parallel

    results |> Array.iter (fun (u, len) -> printfn "%s - %d" u len)
}

/// wrapper that creates its own HttpClient
let crawlAndPrint (url: string) = 
    use client = new HttpClient()
    crawlAndPrintWith client url