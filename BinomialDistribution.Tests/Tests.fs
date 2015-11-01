namespace BinomialDistribution.Tests

open FsUnit 
open NUnit.Framework
open BinomialDistribution.Program

[<TestFixture>]
type ``Given the Binomial Function``() =

    [<TestCase(1,10,0.5,0.0107421875)>]
    [<TestCase(2,10,0.5,0.0546875)>]
    [<TestCase(3,10,0.5,0.171875)>]
    [<TestCase(4,10,0.5,0.376953125)>]
    [<TestCase(5,10,0.5,0.623046875)>]
    [<TestCase(5,13,0.5,0.29052734375)>]
    member t.``the result is calculated correctly``(x, n, p, expected) =
        let errorMargin = 1e-9
        let actual = Binomial x n p
        actual |> should (equalWithin errorMargin) expected
