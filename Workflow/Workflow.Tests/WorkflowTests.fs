module WorkflowTests

open Workflow
open NUnit.Framework
open FsCheck
open FsUnit

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.Pass()
