open System


let eps = 1e-7

let dichotomy f a b =
    let rec loop a b =
        let mid = (a + b) / 2.0
        if (b - a) < eps then mid
        elif f mid = 0.0 then mid
        elif f a * f mid < 0.0 then loop a mid
        else loop mid b
    loop a b

let iterations phi x0 =
    let rec loop xn =
        let xn1 = phi xn
        if abs (xn1 - xn) < eps then xn1
        else loop xn1
    loop x0

let newton f f' x0 =
    let rec loop xn =
        let fxn = f xn
        let fxn' = f' xn
        if abs fxn < eps then xn
        else loop (xn - fxn / fxn')
    loop x0

let f1 x = exp x + log x - 10.0 * x
let f2 x = cos x - exp (x * 2.0 / -2.0) + x - 1.0
let f3 x = 1.0 - x + sin x - log(1.0 + x)

let f1' x = exp x + 1.0 / x - 10.0
let f2' x = -sin x - x * exp (x * 2.0 / -2.0) + 1.0
let f3' x = -1.0 + cos x - 1.0 / (1.0 + x)

let phi1 x = (exp x + log x) / 10.0
let phi2 x = cos x - exp (x * 2.0 / -2.0) + 1.0
let phi3 x = x - (1.0 - x + sin x - log(1.0 + x))

let main = 

    printfn "%10s  %10s  %10s" "Dichotomy" "Iterations" "Newton"
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 3.0 4.0) (iterations phi1 3.5) (newton f1 f1' 3.5)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 1.0 2.0) (iterations phi2 1.0) (newton f2 f2' 1.0)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 1.0 1.5) (iterations phi3 1.0) (newton f3 f3' 1.0)

main
