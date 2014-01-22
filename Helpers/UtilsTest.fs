namespace RCPSP

open NUnit.Framework

open Utils

module UtilsTest =
    [<Test>]
    let testShuffle () =
        Assert.AreEqual((set [1..5]), (shuffle [1..5] |> Set.ofList))

    [<Test>]
    let testFoldItselfConverge () =
        Assert.AreEqual(5, foldItselfConverge (fun n -> if n = 5 then 5 else n+1) 0)

    [<Test>]
    let testTransitiveHull () =
        let relFunc node =
            if node < 4 then set [node+1]
            else set []
        let tfunc = transitiveHull relFunc
        Assert.AreEqual(set [1..4], tfunc 0)