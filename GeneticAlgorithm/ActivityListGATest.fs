namespace RCPSP

open NUnit.Framework

open ActivityListGA

module ActivityListGATest =
    [<Test>]
    let testExchange () =
        Assert.AreEqual([3;2;1], exchange [1;2;3] 0 2)
        Assert.AreEqual([1;2;3], exchange [1;2;3] 0 0)
