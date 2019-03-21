module ZpCEdga where

import Data.List.Split
import Data.List
import Data.Maybe
import ZpInt
import SqZpMatrix
import ZpExpression


type Augmentation = [ZpInt]
data DGA = DGA {gradings :: [ZpInt], differentials :: Differential}

--------------------------------------------------------------------------------
----                          General Functions                             ----
--------------------------------------------------------------------------------

doKTimes :: (a -> a) -> Int -> (a -> a)
doKTimes f k
    | k == 0 = error "can't do zero times!"
    | k == 1 = f
    | otherwise = f . (doKTimes f (k-1))

-- Takes expressions and returns a list of terms up to given length where any element in returned list is from any term in the list
exprGen :: [Expression] -> Int -> [Expression]
exprGen exprlst k
    | k == 1 = exprlst
    | otherwise = smallerTerms ++ [a*b | a <- exprlst, b <- smallerTerms, notReplicate a b] where
        smallerTerms = exprGen exprlst (k-1)
        notReplicate x y = not ((x * y) `elem` smallerTerms)

--------------------------------------------------------------------------------
----                            DGA Functions                               ----
--------------------------------------------------------------------------------

instance Show DGA where
    show dga = let
        diffs = differentials dga
        in
        "DIFFERENTIALS" ++ "\n" ++ (show diffs) ++ "\nGRADINGS: " ++ (show $ map value $ gradings dga) ++" (mod " ++ (show $ getDGAModulus dga) ++ ")"

getDGAModulus :: DGA -> Int
getDGAModulus dga = getExprModulus $ head $ getFroms $ differentials dga

-- Builds a DGA given gradings, modulus, (tos, froms)
buildDGA :: Int -> [Int] -> [(String, String)] -> DGA
buildDGA m grads d_info = let
    gs = map (\a -> ZpInt a m) grads
    in
    DGA gs (differentialFromStrings m d_info)

-- Computes the grading of an expression in the DGA
-- WARNING: Only actually computes the grading of the first term in an expression, since such determines the grading of the whole expression
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
        then getIndividualGrading expr
    else
        -- Sum up the gradings for multiplied terms
        (getIndividualGrading $ head_term) + (grading dga tail_term)
    

-- Computes all possible graded augmentations for given DGA
augmentations :: DGA -> [Augmentation]
augmentations dga = let
    num_gens = (length . getFroms . differentials) dga
    m = getDGAModulus dga

    -- Checks to see if possible augmentation is graded and evaluates to zero on a given expression in differentials
    testAugOnExpr aug expr = let
        -- Argument for substitute in ZpExpression
        -- The last element in the substitutions list is to account for sending 1 -> -1 in every augmentation (since Zp is a field)
        substitutions = (map (\a -> Expression [a] [["~"]]) $ aug) ++ [Expression [ZpInt (-1) m] [["~"]]]
        -- Test to see if substituting in proposed augmentation evaluates to 0
        equalsZero = (substitute ((map (head . terms) $ getFroms $ differentials dga) ++ [["~"]]) substitutions expr) == expressionFromString "0" m
        -- Tests whether the augmentation is graded -- i.e. only elements which do not get sent to 0 have grading 0
        graded = foldl (&&) True $ zipWith (\a b -> if (value $ grading dga b) /= 0 then a == ZpInt 0 m else True) aug (getFroms $ differentials dga)
        in
        equalsZero && graded
        -- equalsZero

    -- Checks to see if map is a graded augmentation (evaluates to zero on given differential and is graded)
    testAug aug = foldl (&&) True (map (testAugOnExpr aug) ((getTos . differentials) dga))
    getAug aug = (map (testAugOnExpr aug) ((getTos . differentials) dga))

    -- Create all possible sequences of augmentations
    possible_augs = sequence [[ZpInt i m | i<- [0..m-1]] | k<-[0..num_gens-1]]

    in
    -- m
    -- map getAug possible_augs
    filter testAug possible_augs



-- CODE BELOW THIS POINT UNDER DEVELOPMENT...

--------------------------------------------------------------------- 
    
-- -- Takes a DGA and increases num of generators appropriately, as well as well as restricts boundary map to words of length specified
-- polyDGA :: DGA -> Augmentation -> Int -> DGA
-- polyDGA dga aug k = let
--     gens = exprGen (fst $ differentials dga) k
--     terms = zip aug gens
--     phiAug expr = substitute augDiffs gens expr where
--         augDiffs = map (\(a, b) -> b + (Expression [[(getStr a)]])) terms where
--             getStr a = if a == 0 then "0" else "1"
--     -- d_aug = map (( (applySnd restrictTermLen) k) . phiAug . (diff $ differentials dga) . phiAug) gens where
--         -- applySnd f y x = f x y
--     unAugDiffs = map (diff $ differentials dga) gens
--     -- singleNewDiffs = (diff $ differentials dga)
--     preDiffAugs = map phiAug gens
--     postDiffAugs = map (diff (gens, unAugDiffs)) preDiffAugs
--     newDiffs = map (\d -> restrictTermLen d k) $ map phiAug postDiffAugs
--     -- newDiffs = (snd $ differentials dga)
--     in
--     DGA (gradings dga) (gens, newDiffs)

-- homology :: DGA -> Int -> Int
-- homology dga k = let
--     diffs = differentials dga
--     gens = fst diffs
--     dgens = snd diffs
--     occurMatrix = [[matrixEntry i j | j <- [0..(length gens) - 1]] | i <- [0.. (length gens)-1]] where
--         matrixEntry i j = if (head $ word $ gens !! i) `elem` (word $ dgens !! j) then 1 else 0
--     rrefMatrix = rref occurMatrix
--     kernelVecs = map getExpression (nullspace $ fst $ rrefMatrix) where
--         n = length $ fst rrefMatrix
--         getExpression nulcol = foldl1 (+) [gens !! j | j <- [0..n-1], (nulcol !! j) == 1]
--     imageVecs = map (dgens !!) (snd rrefMatrix)
--     restrictGrading l = filter (\a -> (grading dga a) == (l `mod` 2))
--     gradKernelVecs = restrictGrading k kernelVecs
--     gradImageVecs = restrictGrading k imageVecs
--     getDiff :: Expression -> Expression
--     getDiff expr = let
--         getSingleDiff singleExpr = foldl1 (+) $ zipWith (\a b -> if a == singleExpr then b else (Expression [["0"]])) gens dgens
--         in 
--         foldl1 (+) [getSingleDiff (Expression [e]) | e <- word expr]

--     checkDSquared = foldl (&&) True $ map (==Expression [["0"]]) (map getDiff (snd diffs))
--     answer = (length gradKernelVecs) - (length gradImageVecs)
--     in
--     if checkDSquared then answer else error ("d^2 is not 0 when computing homology!" ++ show gradKernelVecs ++ "->" ++ show (map (diff diffs) gradKernelVecs) ++ "\nOriginal Answer: " ++ show answer)
--     answer

-- -- Calculates homology of (polyDGA k) with first augmentation found
-- testHomology :: DGA -> Int -> Int -> Int
-- testHomology dga k l = homology (polyDGA dga (head $ augmentations $ dga) k) l

-- -- Computes 0 & 1 homology of all possible augmentations of polyDGA given degree
-- polyHomologies :: DGA -> Int -> [(Int, Int)]
-- polyHomologies dga deg = let
--     augs = augmentations dga
--     pDGAs = map (\a -> polyDGA dga a deg) augs
--     getZeroHomlogy someDGA = homology someDGA 0
--     getOneHomology someDGA = homology someDGA 1
--     in
--     zip (map getZeroHomlogy pDGAs) (map getOneHomology pDGAs)



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
            ("q1", "1+q2"),
            ("q2", "0"),
            ("q3", "1+q2")
            ]

kUnknot_iso2 :: Int -> DGA
kUnknot_iso2 m = buildDGA m gs ds where
    gs = [1, 1, 1, 0, 0] -- Gradings
    ds =    [ -- Differentials
            ("q1", "1+q4*q5"),
            ("q2", "1+q4"),
            ("q3", "1+q5"),
            ("q4", "0"),
            ("q5", "0")
            ]


-- kUnknot_iso2 :: DGA
-- kUnknot_iso2 = DGA gradings differentials where
--     differentials = (
--                         [
--                         expressionFromString "q1",
--                         expressionFromString "q2",
--                         expressionFromString "q3",
--                         expressionFromString "q4",
--                         expressionFromString "q5"
--                         ],
--                         [
--                         expressionFromString "1 + q4*q5",
--                         expressionFromString "1 + q4",
--                         expressionFromString "1 + q5",
--                         expressionFromString "0",
--                         expressionFromString "0"
--                         ]
--                     )
--     gradings = [1, 1, 1, 0, 0]

-- kTrefoil :: DGA
-- kTrefoil = DGA gradings differentials where
--     differentials = (
--                         [
--                         expressionFromString "q1",
--                         expressionFromString "q2",
--                         expressionFromString "q3",
--                         expressionFromString "q4",
--                         expressionFromString "q5"
--                         ],
--                         [
--                         expressionFromString "1 + q3 + q5 + q5*q4*q3",
--                         expressionFromString "1 + q3 + q5 + q3*q4*q5",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0"
--                         ]
--                     )
--     gradings = [1, 1, 0, 0, 0]

-- k4_1 :: DGA
-- k4_1 = DGA gradings differentials where
--     differentials = (
--                         [
--                         expressionFromString "q1",
--                         expressionFromString "q2",
--                         expressionFromString "q3",
--                         expressionFromString "q4",
--                         expressionFromString "q5",
--                         expressionFromString "q6",
--                         expressionFromString "q7"
--                         ],
--                         [
--                         expressionFromString "1 + q3 + q7*q6*q3",
--                         expressionFromString "1 + q3 + q3*q5*q7",
--                         expressionFromString "0",
--                         expressionFromString "q6 + q5 + q5*q7*q6",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0"
--                         ]
--                     )
--     gradings = [1, 1, 0, 0, 1, 1, 1]

-- km5_1 :: DGA
-- km5_1 = DGA gradings differentials where
--     differentials = (
--                         [
--                         expressionFromString "q1",
--                         expressionFromString "q2",
--                         expressionFromString "q3",
--                         expressionFromString "q4",
--                         expressionFromString "q5",
--                         expressionFromString "q6",
--                         expressionFromString "q7",
--                         expressionFromString "q8",
--                         expressionFromString "q9"
--                         ],
--                         [
--                         expressionFromString "1 + q5 + q7 + q9 + q5*q6*q7 + q5*q6*q9 + q5*q8*q9 + q7*q8*q9 + q5*q6*q7*q8*q9",
--                         expressionFromString "1 + q4 + q6 + q8 + q4*q5*q6 + q4*q5*q8 + q4*q7*q8 + q6*q7*q8 + q4*q5*q6*q7*q8",
--                         expressionFromString "1 + q4*q5 + q4*q7 + q4*q9 + q6*q7 + q6*q9 + q8*q9 + q4*q5*q6*q7 + q4*q5*q6*q9 + q4*q5*q8*q9 + q4*q7*q8*q9 + q6*q7*q8*q9 + q4*q5*q6*q7*q8*q9",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0"
--                         ]
--                     )
--     gradings = [1, 1, 1, 0, 0, 0, 0, 0, 0]

-- km5_2_one :: DGA
-- km5_2_one = DGA gradings differentials where
--     differentials = (
--                         [
--                         expressionFromString "q1",
--                         expressionFromString "q2",
--                         expressionFromString "q3",
--                         expressionFromString "q4",
--                         expressionFromString "q5",
--                         expressionFromString "q6",
--                         expressionFromString "q7",
--                         expressionFromString "q8",
--                         expressionFromString "q9"
--                         ],
--                         [
--                         expressionFromString "1 + q4 + q8*q9*q4",
--                         expressionFromString "1 + q4*q5",
--                         expressionFromString "1 + q5 + q5*q7*q8",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "q9 + q7 + q7*q8*q9",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0"
--                         ]
--                     )
--     gradings = [1, 1, 1, 0, 0, 1, 0, 0, 0]

-- km5_2_two :: DGA
-- km5_2_two = DGA gradings differentials where
--     differentials = (
--                         [
--                         expressionFromString "q1",
--                         expressionFromString "q2",
--                         expressionFromString "q3",
--                         expressionFromString "q4",
--                         expressionFromString "q5",
--                         expressionFromString "q6",
--                         expressionFromString "q7",
--                         expressionFromString "q8",
--                         expressionFromString "q9",
--                         expressionFromString "q10",
--                         expressionFromString "q11"
--                         ],
--                         [
--                         expressionFromString "1 + q4 + q9*q10*q4",
--                         expressionFromString "1 + q5 + q10 + q10*q11*q5 + q6 + q6*q9*q10",
--                         expressionFromString "1 + q5*q4 + q10*q4 + q10*q11 + q10*q11*q5*q4 + q6*q4 + q6*q9*q10*q4",
--                         expressionFromString "0",
--                         expressionFromString "q7 + q7*q9*q10",
--                         expressionFromString "q7 + q10*q11*q7",
--                         expressionFromString "0",
--                         expressionFromString "q11 + q9 + q9*q10*q11",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0"
--                         ]
--                     )
--     gradings = [1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0]

-- km10_140 :: DGA
-- km10_140 = DGA gradings differentials where
--     differentials = (
--                         [
--                         expressionFromString "q1",
--                         expressionFromString "q2",
--                         expressionFromString "q3",
--                         expressionFromString "q4",
--                         expressionFromString "q5",
--                         expressionFromString "q6",
--                         expressionFromString "q7",
--                         expressionFromString "q8",
--                         expressionFromString "q9",
--                         expressionFromString "q10",
--                         expressionFromString "q11",
--                         expressionFromString "q12",
--                         expressionFromString "q13",
--                         expressionFromString "q14",
--                         expressionFromString "q15"
--                         ],
--                         [
--                         expressionFromString "1 + q14*q6",
--                         expressionFromString "1 + q6*q7 + q15*q12*q10",
--                         expressionFromString "1 + q7*q11",
--                         expressionFromString "1 + q10*q9 + q14 + q8*q12*q14",
--                         expressionFromString "1 + q11 + q11*q12*q13 + q9*q15",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "q10*q11",
--                         expressionFromString "q11*q12*q14",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "q14*q15",
--                         expressionFromString "0",
--                         expressionFromString "0"
--                         ]
--                     )
--     gradings = [1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1]

-- k11n_139 :: DGA
-- k11n_139 = DGA gradings differentials where
--     differentials = (
--                         [
--                         expressionFromString "q1",
--                         expressionFromString "q2",
--                         expressionFromString "q3",
--                         expressionFromString "q4",
--                         expressionFromString "q5",
--                         expressionFromString "q6",
--                         expressionFromString "q7",
--                         expressionFromString "q8",
--                         expressionFromString "q9",
--                         expressionFromString "q10",
--                         expressionFromString "q11",
--                         expressionFromString "q12",
--                         expressionFromString "q13",
--                         expressionFromString "q14",
--                         expressionFromString "q15",
--                         expressionFromString "q16",
--                         expressionFromString "q17",
--                         expressionFromString "q18",
--                         expressionFromString "q19",
--                         expressionFromString "q20",
--                         expressionFromString "q21",
--                         expressionFromString "q22",
--                         expressionFromString "q23",
--                         expressionFromString "q24",
--                         expressionFromString "q25"
--                         ],
--                         [
--                         expressionFromString "1 + q15*q7",
--                         expressionFromString "1 + q7*q8",
--                         expressionFromString "1 + q8*q9",
--                         expressionFromString "1 + q9*10",
--                         expressionFromString "1 + q10 + q10*q19*q22 + q10*q14*q17",
--                         expressionFromString "1 + q18 + q13*q19*q24 + q13*q14*q18 + q11*q19",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "q17 + q13*q19*q22 + q13*q14*q17 + q13",
--                         expressionFromString "1 + q16*q19 + q15*q18",
--                         expressionFromString "0",
--                         expressionFromString "q19*q20",
--                         expressionFromString "0",
--                         expressionFromString "q15*q17",
--                         expressionFromString "0",
--                         expressionFromString "q17*q19",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "q15 + q16*q19*q20 + q15*q18*q20",
--                         expressionFromString "q20*q17",
--                         expressionFromString "q15*q18*q22 + q16 + q16*q19*q22 + q21*q17",
--                         expressionFromString "1 + q20*q18 + q22*q19",
--                         expressionFromString "0"
--                         ]
--                     )
--     gradings = [1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0]
--     -- gradings = [1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0]

-- kK2 :: DGA
-- kK2 = DGA gradings differentials where
--     differentials = (
--                         [
--                         expressionFromString "q1",
--                         expressionFromString "q2",
--                         expressionFromString "q3",
--                         expressionFromString "q4",
--                         expressionFromString "q5",
--                         expressionFromString "q6",
--                         expressionFromString "q7",
--                         expressionFromString "q8",
--                         expressionFromString "q9",
--                         expressionFromString "q10",
--                         expressionFromString "q11",
--                         expressionFromString "q12",
--                         expressionFromString "p1",
--                         expressionFromString "p2",
--                         expressionFromString "p3"
--                         ],
--                         [
--                         expressionFromString "1 + q12*q7 + p2*p3*q11*q7 + p2*p2*q12*q7 + p3*q2",
--                         expressionFromString "q11*q7 + p1*p3*q11*q7 + p1*p2*q12*q7",
--                         expressionFromString "1 + q4 + q11*q6*q7 + q11*q8*q4 + p1*p3*q4 + p1*p3*q11*q6*q7 + p1*p3*q11*q8*q4 + p1*p2*q7 + p1*p2*q12*q6*q7 + p1*p2*q12*q8*q4 + q2 + q2*q10*q7 + q2*q9*q4",
--                         expressionFromString "p1*q7",
--                         expressionFromString "1 + p1 + p1*q6*q12 + q11 + q4*q9*q11 + q4*q10*q12 + p1*q8*q11",
--                         expressionFromString "1 + q7*q10 + q8*p1",
--                         expressionFromString "0",
--                         expressionFromString "q7*q9",
--                         expressionFromString "0",
--                         expressionFromString "q9*p1",
--                         expressionFromString "p1*q12",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "1 + p3*p1",
--                         expressionFromString "0"
--                         ]
--                     )
--     gradings = [1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0]

-- kK2_iso :: DGA
-- kK2_iso = DGA gradings differentials where
--     differentials = (
--                         [
--                         expressionFromString "q1",
--                         expressionFromString "q2",
--                         expressionFromString "q3",
--                         expressionFromString "q4",
--                         expressionFromString "q5",
--                         expressionFromString "q6",
--                         expressionFromString "q7",
--                         expressionFromString "q8",
--                         expressionFromString "q9",
--                         expressionFromString "q10",
--                         expressionFromString "q11",
--                         expressionFromString "q12",
--                         expressionFromString "q13",
--                         expressionFromString "q14",
--                         expressionFromString "q15",
--                         expressionFromString "q16",
--                         expressionFromString "p1",
--                         expressionFromString "p2",
--                         expressionFromString "p3"
--                         ],
--                         [
--                         expressionFromString "1 + p3*q2 + q15*q13 + p2*p2*q15*q13 + p2*p3*q14*q13",
--                         expressionFromString "q14*q13 + p1*p3*q14*q13 + p1*p2*q15*q13",
--                         expressionFromString "1 + q4 + q14*q8 + q14*q11*q4 + p1*p3*q4 + p1*p3*q14*q8 + p1*p3*q14*q11*q4 + p1*p2*q9 + p1*p2*q15*q8 + p1*p2*q15*q11*q4 + q2 + q2*q10*q9 + q2*q12*q4",
--                         expressionFromString "p1*q9",
--                         expressionFromString "1 + p1*q16 + q4*q10*q15 + q4*q12*q14 + q14",
--                         expressionFromString "1 + q13*q10*q16 + q11*p1*q16 + q11*q14 + q8*q12*q14 + q8*q10*q15",
--                         expressionFromString "1 + q13*q10 + q11*p1",
--                         expressionFromString "q13 + q13*q10*q9 + q11*p1*q9",
--                         expressionFromString "0",
--                         expressionFromString "q12*p1",
--                         expressionFromString "q13*q12",
--                         expressionFromString "0",
--                         expressionFromString "0",
--                         expressionFromString "p1*q15",
--                         expressionFromString "0",
--                         expressionFromString "15 + q9*q12*q14 + q9*q10*q15",
--                         expressionFromString "0",
--                         expressionFromString "1 + p3*p1",
--                         expressionFromString "0"
--                         ]
--                     )
--     gradings = [1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0]
