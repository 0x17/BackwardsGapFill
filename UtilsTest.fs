namespace RCPSP

open NUnit.Framework

open Utils

module UtilsTest =
    [<Test>]
    let testShuffle () =
        Assert.AreEqual((set [1..5]), (shuffle [1..5] |> Set.ofList))

