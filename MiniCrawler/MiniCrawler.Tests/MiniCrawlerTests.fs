module MiniCrawlerTests

open MiniCrawler
open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Net
open System.Net.Http
open RichardSzalay.MockHttp

[<TestFixture>]
type ``MiniCrawler tests`` () =

    // creates mock HttpClient
    let makeMockClient (map: (string * string) list) =
        let handler = new MockHttpMessageHandler()
        for (url, content) in map do
            handler.When(url).Respond(HttpStatusCode.OK, new StringContent(content)) |> ignore
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
    member _.``crawlAndPrintWith downloads linked pages and prints their lengths`` () =
        let root = "http://root.com"
        let c1 = "http://c1.com"
        let c2 = "http://c2.com"
        let rootHtml = $"""<a href="{c1}">1</a><a href="{c2}">2</a>"""

        let client = makeMockClient [
            root, rootHtml
            c1, "AAA"
            c2, "HelloWorld"
        ]

        use sw = new StringWriter()
        let old = Console.Out
        Console.SetOut(sw)

        try
            Async.RunSynchronously (crawlAndPrintWith client root)
        finally
            Console.SetOut(old)

        let output = sw.ToString().Replace("\r\n", "\n")
        output |> should contain (sprintf "%s - %d" c1 3)
        output |> should contain (sprintf "%s - %d" c2 10)