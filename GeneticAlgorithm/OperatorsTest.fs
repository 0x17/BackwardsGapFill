namespace RCPSP

open NUnit.Framework

open Operators

module OperatorsTest =
    [<Test>]
    let testExchange () =
        Assert.AreEqual([3;2;1], swap [1;2;3] 0 2)
        Assert.AreEqual([1;2;3], swap [1;2;3] 0 0)
