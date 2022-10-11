// Task:
// Write a function that multiplies 2 matrices.
// taken from: https://www.youtube.com/watch?v=KdZccqaS6Nw

/// Takes two matrices (as type `Array2D`) of type `float` and multiplies them.
/// 
/// Raises `System.Exception` when the number of columns of the first matrix does not match the number of rows of the second.</exception>
let multiplyMatrices (m1 : float [,]) (m2 : float [,]) =
    let nR1 = m1.GetLength 0
    let nC1 = m1.GetLength 1
    let nR2 = m2.GetLength 0
    let nC2 = m2.GetLength 1
    if nC1 <> nR2 then failwith "Matrix multiplication can only be performed if the number of columns of the first matrix matches the number of rows of the second."

    Array2D.init nR1 nC2 (
        fun i j -> 
            (m1[i,0 ..], m2[0 ..,j]) 
            ||> Array.fold2 (fun acc v1 v2 -> acc + v1 * v2) 0.
    )

let testMatrix1 = array2D [
    [0.; 5.; 6.]
    [1.; 2.; 4.]
]

let testMatrix2 = array2D [
    [7.; 7.]
    [4.; 8.]
    [10.; 3.]
]

let testMatrix3 = array2D [
    [7.; 7.; 0.]
    [4.; 8.; -5.]
    [10.; 3.; 15.]
]

multiplyMatrices testMatrix1 testMatrix1
multiplyMatrices testMatrix1 testMatrix2
multiplyMatrices testMatrix1 testMatrix3