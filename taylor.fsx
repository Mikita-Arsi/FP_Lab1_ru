// Print a table of a given function f, computed by taylor series

// function to compute
let f x = x / (9.0 + (x ** 2.0))

let a = -1.0
let b = 1.0
let n = 10
let eps = 0.001

// Define a function to compute f using naive taylor series method
let rec TaylorNaive x n term sum =
    let nextTerm = ((-1.0) ** float n) * (x ** float (2 * n + 1)) / (9.0 ** float (n + 1))
    if abs nextTerm < eps then sum
    else TaylorNaive x (n + 1) nextTerm (sum + nextTerm)

let TaylorNaiveWrapper x =
    TaylorNaive x 0 0.0 0.0

// Define a function to do the same in a more efficient way
let rec TaylorEfficient x term sum n =
    let nextTerm = term * (-x * x) / (9.0 * float (n + 1))
    if abs nextTerm < eps then sum
    else TaylorEfficient x nextTerm (sum + nextTerm) (n + 1)

let TaylorEfficientWrapper x =
    let initialTerm = x / 9.0
    TaylorEfficient x initialTerm initialTerm 0

let main =
    for i=0 to n do
        let x = a+(float i)/(float n)*(b-a)
        let xRes = f x
        let TNaive = TaylorNaiveWrapper x
        let TEfficient = TaylorEfficientWrapper x
        printfn "%5.2f  %10.6f  %10.6f   %10.6f" x xRes TNaive TEfficient
// make sure to improve this table to include the required number of iterations
// for each of the methods

main 