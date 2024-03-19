{- |
Module      :  ZpCEdga
Description :  <DGA Module>
Copyright   :  (c) Hunter A. Vallejos
License     :  N/A

Maintainer  :  <hunterandresvallejos@gmail.com>
Stability   :  experimental
Portability :  non-portable (dependencies on custom modules)

<Computational Interface for handling Chekanov-Eliashberg differential graded algebras directly. Only gradings allowed are Z_p[t^(-1), t] for prime p>
-}
-- Author: Hunter Vallejos
-- Date: April 17, 2019

module ZpCEdga where

import Data.List.Split
import Data.List
import Data.Maybe
import ZpInt
import SqZpMatrix
import ZpExpression


type Augmentation = [ZpInt]
newtype Differential = Differential {getDiffPairs :: [(Expression, Expression)]}
data DGA = DGA {gradings :: [ZpInt], differentials :: Differential}

--------------------------------------------------------------------------------
----                        Differential Functions                          ----
--------------------------------------------------------------------------------

instance Show Differential where
    show (Differential pairs) = let
        m = getExprModulus $ snd $ head $ pairs
        getDiffString pair = (show $ fst pair) ++ " -> " ++ (show $ snd pair)
        printString p
            | length p == 0 = ""
            | length p == 1 = getDiffString (head p)
            | otherwise = (getDiffString (head p)) ++ "\n" ++ (printString (tail p))
        in
        printString pairs

-- | Returns the generators which can be differentiated
getFroms :: Differential -> [Expression]
getFroms (Differential pairs) = map fst pairs

-- | Returns the results of the differential applied the generators
getTos :: Differential -> [Expression]
getTos (Differential pairs) = map snd pairs

-- | Constructs a Differential type given a string [(to's, from's)] applying expressionFromString to each element element of each pair
differentialFromStrings :: Int -> [(String, String)] -> Differential
differentialFromStrings m list = Differential $ map (\(e1, e2) -> (expressionFromString e1 m, expressionFromString e2 m)) list

--------------------------------------------------------------------------------
----                          General Functions                             ----
--------------------------------------------------------------------------------

-- doKTimes :: (a -> a) -> Int -> (a -> a)
-- doKTimes f k
--     | k == 0 = error "can't do zero times!"
--     | k == 1 = f
--     | otherwise = f . (doKTimes f (k-1))

-- | Takes expressions and returns a list of terms up to given length where any element in returned list is from any term in the list
exprGen :: [Expression] -> Int -> [Expression]
exprGen exprlst k
    | k == 1 = exprlst
    | otherwise = smallerTerms ++ [a*b | a <- exprlst, b <- smallerTerms, notReplicate a b] where
        smallerTerms = exprGen exprlst (k-1)
        notReplicate x y = not ((x * y) `elem` smallerTerms)



--------------------------------------------------------------------------------
----                            DGA Functions                               ----
--------------------------------------------------------------------------------
-- | Can only differentiate single elements. If you want to differentiate in multi-element terms linearly, use diff_linear. Otherwise, use substitute.
diff :: DGA -> Expression -> Expression
diff dga expr
    | numTerms expr == 0 = expr
    | numTerms expr == 1 = let
        pairs = getDiffPairs $ differentials dga
        c = head $ coefficients expr
        t = head $ terms expr
        one = ZpInt 1 (modulus c)

        -- List of from's and to's in the differntials
        from_terms = map (head . terms . fst) pairs
        to_exprs = map snd pairs

        -- Differentiate a single element given the index of that element
        -- Here, diff(T) = 0 and diff(1) = 0
        diffElement index = substitute (["T"] : ["~"] : from_terms) ((Expression [] []) : (Expression [] []) : to_exprs) $ Expression [c] [[t !! index]]
        
        -- Differentiate a given index in a term
        getIndexDiff index = leibniz_alternating_expr * first_part_expr * (diffElement index) * last_part_expr where
            first_part = let
                possible_first_part = fst $ splitAt (index) (t)
                in
                if possible_first_part == [] then ["~"] else possible_first_part
            last_part = let
                possible_last_part = snd $ splitAt (index+1) (t)
                in
                if possible_last_part == [] then ["~"] else possible_last_part
            first_part_expr = Expression [one] [first_part]
            last_part_expr = Expression [one] [last_part]
            leibniz_alternating_product = ZpInt ((value $ negate one)^(abs $ value $ grading dga first_part_expr)) (modulus one)
            leibniz_alternating_expr = Expression [leibniz_alternating_product] [["~"]]
        in
        if length t == 1 then substitute (["T"] : ["~"] : from_terms) ((Expression [] []) : (Expression [] []) : to_exprs) expr
        else foldl1 (+) $ map (getIndexDiff) [0..(length t)-1]
    | otherwise = (diff (dga) (headExpr expr)) + (diff (dga) (tailExpr expr))

-- | Differentiates but only linearly, meaning no chain-rule! This can handle products of terms that aren't explicitly listed in the Differential. The purpose of this function is to double-check that d^2 = 0 in a polynomial DGA
diff_linear :: Differential -> Expression -> Expression
diff_linear (Differential pairs) expr = let
    m = getExprModulus expr
    from_terms =  ["T"] : ["~"] : map (head . terms . fst) pairs
    to_exprs = (expressionFromString "0" m) : (expressionFromString "0" m) : map snd pairs
    in
    substitute from_terms to_exprs expr

instance Show DGA where
    show dga = let
        diffs = differentials dga
        in
        "DIFFERENTIALS" ++ "\n" ++ (show diffs) ++ "\nGRADINGS: " ++ (show $ map value $ gradings dga) ++" (mod " ++ (show $ getDGAModulus dga) ++ ")"

-- | Gets the modulus of the first term in the first element of the generator of the DGA
getDGAModulus :: DGA -> Int
getDGAModulus dga = getExprModulus $ head $ getFroms $ differentials dga

-- | Builds a DGA given gradings, modulus, (tos, froms)
buildDGA :: Int -> [Int] -> [(String, String)] -> DGA
buildDGA m grads d_info = let
    gs = map ((\a -> ZpInt a m)) grads
    in
    DGA gs (differentialFromStrings m d_info)

-- | Computes the grading of an expression in the DGA. WARNING: Only actually computes the grading of the first term in an expression, since such determines the grading of the whole expression
grading :: DGA -> Expression -> ZpInt
grading dga expr = let
    m = getExprModulus expr
    -- Get the grading of a given element of DGA
    getIndividualGrading gen = let
        froms = getFroms $ differentials dga
        in
        if gen `elem` froms
            -- fst $ head $ zipWith (\g dgen -> dgen == gen) (gradings dga) froms
            then fst $ head (filter (\(g, dgen) ->  dgen == gen) $ zip (gradings dga) froms)
        else if (head . head . terms) gen == "T" || (head . head . terms) gen == "~"
            then ZpInt 0 m -- the grading of T is always 0
        else
            error((show gen) ++ " is not in differentials!")
    target_expr = headExpr expr
    head_term = Expression [ZpInt 1 m] [[head $ head $ terms target_expr]]
    tail_term = Expression [ZpInt 1 m] [tail $ head $ terms target_expr]
    term_len = length $ head $ terms target_expr
    in
    if numTerms expr == 0
        then ZpInt 0 m
    else if term_len == 1
        then getIndividualGrading head_term
    else
        -- Sum up the gradings for multiplied terms
        (getIndividualGrading $ head_term) + (grading dga tail_term)
    

-- | Computes all possible graded augmentations for given DGA
augmentations :: DGA -> [Augmentation]
augmentations dga = let
    num_gens = (length . getFroms . differentials) dga
    m = getDGAModulus dga

    -- Checks to see if possible augmentation is graded and evaluates to zero on a given expression in differentials
    testAugOnExpr aug expr = let

        -- Argument for substitute in ZpExpression
        -- The last element in the substitutions list is to account for sending T -> -1 in every augmentation (since Zp is a field)
        substitutions = (map (\a -> Expression [a] [["~"]]) $ aug) ++ [Expression [ZpInt (-1) m] [["~"]]]


        -- Test to see if substituting in proposed augmentation evaluates to 0
        subbed = substitute ((map (head . terms) $ getFroms $ differentials dga) ++ [["T"]]) substitutions expr
        equalsZero = subbed == expressionFromString "0" m || subbed == (Expression [ZpInt 0 m] [["~"]])
        -- equalsZero = (substitute ((map (head . terms) $ getFroms $ differentials dga) ++ [["T"]]) substitutions expr)

        -- Tests whether the augmentation is graded -- i.e. only elements which do not get sent to 0 have grading 0
        graded = foldl (&&) True $ zipWith (\a b -> if (value $ abs $ grading dga b) /= 0 then a == ZpInt 0 m else True) aug (getFroms $ differentials dga)
        in
        equalsZero && graded

    -- Checks to see if map is a graded augmentation (evaluates to zero on given differential and is graded)
    testAug aug = foldl (&&) True (map (testAugOnExpr aug) ((getTos . differentials) dga))
    -- testAug aug = (map (testAugOnExpr aug) ((getTos . differentials) dga))
    getAug aug = (map (testAugOnExpr aug) ((getTos . differentials) dga))

    -- Create all possible sequences of augmentations
    possible_augs = sequence [[ZpInt i m | i<- [0..m-1]] | k<-[0..num_gens-1]]

    in
    filter testAug possible_augs
    -- map testAug possible_augs
    
-- | Takes a DGA and increases num of generators appropriately, as well as well as restricts boundary map to words of length specified
-- TODO: Function fails on polyDGA (kUnknot_iso1 3) (head $ augmentations $ kUnknot_iso1 3) 2
polyDGA :: DGA -> Augmentation -> Int -> DGA
polyDGA dga aug k = let
    
    m = getDGAModulus dga

    -- Get generators of DGA
    gens = getFroms $ differentials dga

    -- Get polynomial generators
    poly_gens = exprGen gens k

    -- Keep track of where the augmentation sends each generator
    pairs = zip aug gens

    -- send q_i -> q_i + e(q_i), where e is the augmentation map
    phiAug :: Expression -> Expression
    phiAug expr = substitute froms tos expr where
        -- Insert that T -> (+1) when augmenting
        froms = ["T"] : (map (head . terms) gens)
        tos = (expressionFromString "-1" m) : (map (\(a, g) -> g + (Expression [a] [["~"]])) pairs)

    -- send q_i -> q_i - e(q_i), where e is the augmentation map
    unphiAug :: Expression -> Expression
    unphiAug expr = substitute froms tos expr where
        -- Insert that T -> (-1) when un-augmenting
        froms = ["T"] : (map (head . terms) gens)
        tos = (expressionFromString "1" m) : (map (\(a, g) -> g + (Expression [negate a] [["~"]])) pairs)

    -- Augment the new (poly)generators
    un_augd_poly_gens = map unphiAug poly_gens

    -- Differentiate the augmented (poly)generators
    diffd_un_augd_poly_gens = map (diff dga) un_augd_poly_gens

    -- Perform the inverse operation for augmenting so that we have d^2 = 0
    -- (Note that the inverse operation sends T -> +1 instead of -1)
    augd_diffd_un_augd_poly_gens = map (\p -> restrictTermLen p k) $ map phiAug diffd_un_augd_poly_gens

    -- New Differential for polyDGA. Note that now to differentiate, we must use diff_linear
    new_differential = Differential $ zip poly_gens augd_diffd_un_augd_poly_gens

    d_squared_zero = foldl (&&) (True) (map checkDSquared $ getTos new_differential) where
        checkDSquared :: Expression -> Bool
        checkDSquared expr = (expressionFromString "0" (getDGAModulus dga)) == (diff_linear new_differential expr)
    in
    if d_squared_zero 
        then DGA (gradings dga) new_differential 
    else error("polyDGA differential fails d^2 = 0 test. Perhaps augmentation is not graded?\nDifferential:\n" ++ (show new_differential) ++ "\nd^2 gives...\n" ++ 
        printLine (zip (getFroms new_differential) (map (diff_linear new_differential) $ getTos new_differential))) where
            printLine epairs
                | length epairs == 0 = ""
                | length epairs == 1 = (show $ fst $ head epairs) ++ " -> " ++ (show $ snd $ head $ epairs)
                | otherwise = (show $ fst $ head epairs) ++ " -> " ++ (show $ snd $ head $ epairs) ++ "\n" ++ (printLine $ tail epairs)

-- | Computes the homology of a DGA which has already been passed to polyDGA
homology :: DGA -> Int -> Int
homology dga k = let
    -- Get consts
    diffs = differentials dga
    m = getDGAModulus dga
    gens = getFroms diffs
    dgens = getTos diffs

    -- Set up matrix where columns are the expressions (we are interested in linear combinations of the differentials)
    occur_matrix = SqZpMatrix [[matrixEntry i j | j <- [0..(length gens) - 1]] | i <- [0.. (length gens)-1]] where        
        matrixEntry i j = let
            gen_term = head $ terms $ gens !! i
            dgen_expr = dgens !! j
            in
            if termInExpr gen_term dgen_expr 
                then getTermCoeffInExpression gen_term dgen_expr
            else
                ZpInt 0 m

    -- Perform Gaussian Elimination
    rref_matrix = rref occur_matrix

    -- Extract kernel from matrix
    kernel_vecs = map getExpression (nullspace $ fst $ rref_matrix) where
        n = fst $ dimensions $ fst rref_matrix
        getExpression nulcol = foldl (+) (Expression [] []) [gens !! j | j <- [0..n-1], (value $ nulcol !! j) /= 0]

    image_vecs = map (dgens !!) (snd rref_matrix)

    restrictGrading l = filter (\a -> grading dga a == ZpInt l m)
    grad_kernel_vecs = restrictGrading k kernel_vecs
    grad_image_vecs = restrictGrading k image_vecs

    in
    -- occur_matrix
    (length grad_kernel_vecs) - (length grad_image_vecs)

-- | Calculates lth homology of (polyDGA k) with first augmentation found
testHomology :: DGA -> Int -> Int -> Int
testHomology dga k l = homology (polyDGA dga (head $ augmentations $ dga) k) l


-- | Computes all gradings of homology of all possible augmentations of polyDGA given degree
polyHomologies :: DGA -> Int -> [[Int]]
polyHomologies dga deg = let
    augs = augmentations dga
    m = getDGAModulus dga
    pdgas = map (\a -> polyDGA dga a deg) augs

    -- map (\some_dga -> map (homology some_dga) [0..(m-1)]) pdgas
    in
    if deg > 0
        then map (\some_dga -> map (homology some_dga) [0..(m-1)]) pdgas
    else error("Such degree homology doesn't exist")


--------------------------------------------------------------------------------
----                           Preloaded DGAs                               ----
--------------------------------------------------------------------------------

kUnknot :: Int -> DGA
kUnknot m = buildDGA m gs ds where
    gs = [1] -- Gradings
    ds =    [ -- Differentials
            ("q1", "0")
            ]

kUnknot_iso1 :: Int -> DGA
kUnknot_iso1 m = buildDGA m gs ds where
    gs = [1, 0, 1] -- Gradings
    ds =    [ -- Differentials
            ("q1", "T + q2"),
            ("q2", "0"),
            ("q3", "1+ (-1)*q2")
            ]

kTrefoil :: Int -> DGA
kTrefoil m = buildDGA m gs ds where
    gs = [1, 1, 0, 0, 0]
    ds =    [
            ("q1", "T + q5 + q3 + q5*q4*q3"),
            ("q2", "1 + (-1)*q5 + (-1)*q3 + (-1)*q3*q4*q5"),
            ("q3", "0"),
            ("q4", "0"),
            ("q5", "0")
            ]




km10_140 :: Int -> DGA
km10_140 m = buildDGA m gs ds where
    gs = [1, 1, 1, 1, 1, 0, 0, 2, -1, 1, 0, -2, 2, 0, 1]
    ds =    [
            ("q1", "T + q14*q6"),
            ("q2", "1 + q6*q7 + (-1)*q15*q12*q10"),
            ("q3", "1 + q7*q11"),
            ("q4", "1 + (-1)*q10*q9 + (-1)*q14 + (-1)*q8*q12*q14"),
            ("q5", "1 + (-1)*q11 + (-1)*q11*q12*q13 + (-1)*q9*q15"),
            ("q6", "0"),
            ("q7", "0"),
            ("q8", "(-1)*q10*q11"),
            ("q9", "(-1)*q11*q12*q14"),
            ("q10", "0"),
            ("q11", "0"),
            ("q12", "0"),
            ("q13", "q14*q15"),
            ("q14", "0"),
            ("q15", "0")
            ]

kK2 :: Int -> DGA
kK2 m = buildDGA m gs ds where
    gs = [1, 1, 1, 0, 0, 1, -1, 0, 0, 1, 0, -1, 0, 1, 0]
    ds =    [
            ("q1", "T + (-1)*q12*q7 + (-1)*p2*p1*q11*q7 + p2*p2*q12*q7 + p1*q4"),
            ("q2", "1 + (-1)*q5 + q11*q6*q7 + (-1)*q11*q8*q5 + (-1)*p3*p1*q5 + p3*p1*q11*q6*q7 + (-1)*p3*p1*q11*q8*q5 + p3*p2*q7 + (-1)*p3*p2*q12*q6*q7 + p3*p2*q12*q8*q5 + (-1)*q4 + q4*q10*q7 + (-1)*q4*q9*q5"),
            ("q3", "1 + p3 + p3*q8*q11 + (-1)*p3*q6*q12 + q11 + q5*q9*q11 + (-1)*q5*q10*q12"),
            ("q4", "q11*q7 + p3*p1*q11*q7 + (-1)*p3*p2*q12*q7"),
            ("q5", "p3*q7"),
            ("q6", "1 + (-1)*q7*q10 + q8*p3"),
            ("q7", "0"),
            ("q8", "(-1)*q7*q9"),
            ("q9", "0"),
            ("q10", "q9*p3"),
            ("q11", "p3*q12"),
            ("q12", "0"),
            ("p1", "0"),
            ("p2", "1 + p1*p3"),
            ("p3", "0")
            ]