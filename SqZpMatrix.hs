-- Author: Hunter Vallejos
-- Date: 02-15-2019


-- SqZpMatrix is a matrix of ZpInt's with the main purpose of this module being Gaussian elimination and kernel calculation.

module SqZpMatrix where
import Data.Maybe
import Data.List
import ZpInt

newtype SqZpMatrix = SqZpMatrix [[ZpInt]] -- p must be prime!

getMatrixData :: SqZpMatrix -> [[ZpInt]]
getMatrixData (SqZpMatrix matrix_data) = matrix_data


--------------------------------------------------------------------------------
----                            Matrix Instances                            ----
--------------------------------------------------------------------------------

instance Show SqZpMatrix where
    show matrix = let
        matrix_data = getMatrixData matrix
        p = modulus $ (matrix_data !! 0) !! 0
        neutered_matrix = [map value row | row <- matrix_data] -- matrix with just values, losing modulus information
        matrix_str = (show $ head neutered_matrix) ++ (foldl1 (++) (map (\a -> ("\n") ++ (show a)) (tail neutered_matrix)))
        in
        matrix_str ++ "\t(mod " ++ (show p) ++ ")"

--------------------------------------------------------------------------------
----                            Matrix Functions                            ----
--------------------------------------------------------------------------------

-- Constructs a SqZpMatrix given a 2D array of ints and desired modulus
buildMatrix :: [[Int]] -> Int -> SqZpMatrix
buildMatrix matrix_data p = SqZpMatrix [[ZpInt (entry `mod` p) p | entry <- row] | row <- matrix_data]

-- Perform Gaussian elimination until matrix is in reduced echelon form with indexes for pivot columns
rref :: SqZpMatrix -> (SqZpMatrix, [Int])
rref matrix = let
    matrix_data = getMatrixData matrix
    p = modulus ((matrix_data !! 0) !! 0)
    n = length matrix_data
    zero = ZpInt 0 p
    one = ZpInt 1 p

    -- Organizes rows so that pivots are set up for Gaussian elimination
    rowSwappedMatrix m = let
        -- Returns whether given row has a pivot in the jth column
        hasPivot row j = (row !! j) /= zero  && (foldl (&&) True (map ((==zero) . (row !! )) [0..j-1]))

        -- Returns whether there is a pivot in the jth column
        hasColPivot j = [row | row <- m, hasPivot row j] /= []

        -- Get column indexes where there is a pivot
        pivot_cols = filter (hasColPivot) [0..(length m)-1]

        -- Organized rows by pivots as in Gaussian elimination
        pivotRows = map (\j -> head $ filter (\row -> hasPivot row j) m) pivot_cols

        -- Get the row indexes which have pivots
        pivotRowNums = checkPivotRow 0 [] where
            checkPivotRow rowNum addedRows
                | rowNum == n = []
                | (m !! rowNum) `elem` pivotRows && not ((m !! rowNum) `elem` addedRows) = [rowNum] ++ checkPivotRow (rowNum+1) (addedRows ++ [(m !! rowNum)])
                | otherwise = [] ++ checkPivotRow (rowNum+1) addedRows
        in
        (pivotRows ++ [m !! j | j <- [0..n-1], not (j `elem` pivotRowNums)], pivot_cols)

    -- Takes a row and pivot index and adds the row to rows above and below it as in Gaussian elimination.
    partialRowReduce m rowNum colNum = (partialUpRowReduce (partialDownRowReduce m rowNum colNum) rowNum colNum) where
        -- Adds r1 to r2 such that colnum index of r2 is eliminated.
        addRow colnum r1 r2
            | r2 !! colnum == zero = r2
            | otherwise = let
                -- scale r1 so that r1+r2 eliminates nonzero j entry of r2
                pivot_scalar = (ZpInt (-1) p) * ((r2 !! colnum) `divide` (r1 !! colnum))
                scaled_r1 = map (*pivot_scalar) r1
                in
                -- (pivot_scalar, r1, scaled_r1, r2, zipWith (+) scaled_r1 r2)
                zipWith (+) scaled_r1 r2

        partialDownRowReduce mat rn cn = [mat !! j | j <- [0..rn]] ++ (map (addRow cn (mat !! rn)) [mat !! j | j <- [rn+1..n-1]])

        partialUpRowReduce mat rn cn = (map (addRow cn (mat !! rn)) [mat !! j | j <- [0..rn-1]]) ++ [mat !! j | j <- [rn..n-1]]

    -- Performs Gaussian elimination for a single pivot, and recursively executes to next pivot
    singleRowReduce m pivot_cols currentIndex
        | currentIndex == length pivot_cols = m
        | otherwise = singleRowReduce (partialRowReduce m currentIndex (pivot_cols !! currentIndex)) pivot_cols (currentIndex+1)

    -- Alternates row swaps and single row reductions until RREF is reached
    rowReduce m = let
        rsm = rowSwappedMatrix m
        rrm = singleRowReduce (fst rsm) (snd rsm) 0
        in
        -- rrm
        if rrm == m then m else rowReduce rrm

    -- Appropriately applies final scaling setting first nonzero entry of each row to 1
    scaleReduce m pivot_cols = let
        num_pivots = length pivot_cols
        -- Scale row by dividing by given entry index
        scaleRow (row, index) = map (`divide` (row !! index)) row

        nonzero_rows = map scaleRow (zip m pivot_cols)

        zero_rows = [[ZpInt 0 p | j <- [0..(n-1)]] | i <- [num_pivots .. (n-1)]]
        in
        nonzero_rows ++ zero_rows

    unscaled_matrix_data = rowSwappedMatrix $ rowReduce matrix_data

    pivot_cols = snd unscaled_matrix_data

    final_matrix_data  = scaleReduce (fst unscaled_matrix_data) (pivot_cols)

    in
    (SqZpMatrix final_matrix_data, pivot_cols)


-- Returns (num_rows, num_cols)
dimensions :: SqZpMatrix -> (Int, Int)
dimensions matrix = (length $ getMatrixData matrix, length $ ((getMatrixData matrix) !! 0))

-- Transposes a matrix
sqTranspose :: SqZpMatrix -> SqZpMatrix
sqTranspose matrix = let
    transpose m
        | length (m !! 0) == 1 = [map head m]
        | otherwise = [(map head m)] ++ (transpose $ map tail m)
    in
    SqZpMatrix (transpose $ getMatrixData matrix)


-- Calculates the kernel, or nullspace, of a ZpInt Matrix
nullspace :: SqZpMatrix -> [[ZpInt]]
nullspace matrix = let
    zero :: ZpInt
    zero = ZpInt 0 p

    -- Get reduced echelon form data
    pivot_data = rref matrix

    rref_matrix = fst pivot_data
    rref_matrix_data = getMatrixData rref_matrix

    p = modulus $ (rref_matrix_data !! 0) !! 0

    pivot_cols = snd pivot_data

    free_cols = [i | i<-[0..matLen-1], not (i `elem` pivot_cols)]

    matLen = fst $ dimensions matrix

    -- Get free variables from each row
    -- getParametricVectorForm :: [ZpInt] -> (Int, [ZpInt])
    getParametricVectorForm row = let
        firstNonZeroEntry = fromMaybe (-1) $ findIndex (/=zero) row
        targets = zip row [0..matLen-1]

        -- Sees if a variable is free or not
        isParam :: (ZpInt, Int) -> Bool
        isParam (r, i) = (i > firstNonZeroEntry) && (r /= zero)
        in
        if firstNonZeroEntry == (-1) then (-1, []) else (firstNonZeroEntry, (map snd $ filter isParam targets))

    -- Get all dependent variables in terms of free variables
    parametric_vector_forms = map getParametricVectorForm rref_matrix_data

    -- Formulate nullspace vector
    translateParam index = [fst a | a <- parametric_vector_forms, index `elem` (snd a)]

    -- Add remaining free variables to each nullspace vector
    finishNulVec (freeVar, posEntries) = [f i | i <- [0..matLen-1]] where
        f i
            | i == freeVar = (ZpInt 1 p)
            | i `elem` posEntries = (ZpInt (-1) p)*((rref_matrix_data !! i) !! freeVar)
            | otherwise = (ZpInt 0 p)
    in
    map finishNulVec $ zip free_cols $ map translateParam free_cols



--------------------------------------------------------------------------------
----                            Matrix Tests                                ----
--------------------------------------------------------------------------------

testMatrix1 :: SqZpMatrix
testMatrix1 = buildMatrix [[1, 2, 3], [4, 5, 6], [7, 8, 9]] 5

testMatrix2 :: SqZpMatrix
testMatrix2= buildMatrix [[0, 0, 3], [0, 0, 0], [0, 0, 0]] 11

testMatrix3 :: SqZpMatrix
testMatrix3= buildMatrix [[0, 0, 4], [4, 0, 0], [0, 0, -1]] 5