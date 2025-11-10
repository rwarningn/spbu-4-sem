module MiniCrawlerTests

open MiniCrawler
open NUnit.Framework
open FsUnit
open System
open System.Net
open System.Net.Http
open RichardSzalay.MockHttp

[<TestFixture>]
type ``MiniCrawler tests`` () =

    // creates mock HttpClient
    let makeMockClient (map: (string * HttpResponseMessage) list) =
        let handler = new MockHttpMessageHandler()
        for (url, resp) in map do
            handler.When(url).Respond(fun _ -> resp) |> ignore
        handler.ToHttpClient()

    [<Test>]
    member _.``extractLinks finds only http links`` () =
        let html = """
            <a href="http://a.com">A</a>
            <a href="https://b.com">B</a>
            <a href="/relative">C</a>
        """
        extractLinks html |> should equal ["http://a.com"]

    [<Test>]
    member _.``extractLinks returns distinct results`` () =
        let html = """<a href="http://x.com">x</a><a href="http://x.com">dup</a>"""
        extractLinks html |> should equal ["http://x.com"]

    [<Test>]
    member _.``crawlAndReportWith downloads linked pages and reports their lengths`` () =
        let root = "http://root.com"
        let c1 = "http://c1.com"
        let c2 = "http://c2.com"
        let rootHtml = $"""<a href="{c1}">1</a><a href="{c2}">2</a>"""

        let client =
            makeMockClient [
                root, new HttpResponseMessage(HttpStatusCode.OK, Content = new StringContent(rootHtml))
                c1,  new HttpResponseMessage(HttpStatusCode.OK, Content = new StringContent("AAA"))    
                c2,  new HttpResponseMessage(HttpStatusCode.OK, Content = new StringContent("HelloWorld")) 
            ]

        // collect output via injected printer
        let out = System.Collections.Generic.List<string>()
        let printer (s:string) = out.Add s

        Async.RunSynchronously (crawlAndReportWith client printer root)

        out |> Seq.exists ((=) (sprintf "%s - %d" c1 3))  |> should equal true
        out |> Seq.exists ((=) (sprintf "%s - %d" c2 10)) |> should equal true

    [<Test>]
    member _.``crawlAndReportWith handles errors and keeps going`` () =
        let root = "http://root.com"
        let okUrl = "http://ok.com"
        let badUrl = "http://bad.com"
        let rootHtml = $"""<a href="{okUrl}">ok</a><a href="{badUrl}">bad</a>"""

        let client =
            makeMockClient [
                root,   new HttpResponseMessage(HttpStatusCode.OK, Content = new StringContent(rootHtml))
                okUrl,  new HttpResponseMessage(HttpStatusCode.OK, Content = new StringContent("DATA"))
                badUrl, new HttpResponseMessage(HttpStatusCode.BadRequest)
            ]

        let out = System.Collections.Generic.List<string>()
        let printer (s:string) = out.Add s

        Async.RunSynchronously (crawlAndReportWith client printer root)

        out |> Seq.exists (fun s -> s.StartsWith("Failed: " + badUrl)) |> should equal true
        out |> Seq.exists ((=) (sprintf "%s - %d" okUrl 4)) |> should equal true