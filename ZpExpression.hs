{- |
Module      :  ZpExpression
Description :  <Utility module holding various useful general functions>
Copyright   :  (c) Hunter A. Vallejos
License     :  N/A

Maintainer  :  <hunterandresvallejos@gmail.com>
Stability   :  experimental
Portability :  non-portable (dependencies on custom modules)

<Module containing all of the relevant algebraic manipulations to expressions, such as addition, division, substitution, etc. ZpExpression requires coefficients to be of the ZpInt type.>
-}


module ZpExpression where

-- IMPORTANT!!!! Expressions are taken mod 2 -- i.e. Expressions take coefficients in Z/2Z.


-- Note that ["~"] is the constant term
import Data.List.Split
import Data.List
import Data.Maybe
import Text.Read
import Utility
import ZpInt

type Term = [String]
-- data Expression = Expression {coefficients :: [ZpInt], terms :: [Term]}
data Expression = Expression [ZpInt] [Term]

-- TODO: Make a type called Laurent Coefficient
-- TODO: Abstract the type Expression so that it deals with arbitrary instances of Num

--------------------------------------------------------------------------------
----                         Expression Functions                           ----
--------------------------------------------------------------------------------

-- | Gets coefficients of expression
coefficients :: Expression -> [ZpInt]
coefficients (Expression cs ts) = cs

-- | Gets terms of expression
terms :: Expression -> [Term]
terms (Expression cs ts) = ts

-- | Gets list of pairs of (coeff, term) from expression
getCoeffTermPairs :: Expression -> [(ZpInt, Term)]
getCoeffTermPairs (Expression cs1 ts1) = zip cs1 ts1

-- | Calculates the number of terms in an expression
numTerms :: Expression -> Int
numTerms = length . coefficients

-- | Gets the coefficient of a given term in an expression. Returns -1 mod -1 if no coefficient is found 
getTermCoeffInExpression :: Term -> Expression -> ZpInt
getTermCoeffInExpression term expr = let
    index = getTermIndex term expr
    in
    if index == -1 then error ("Term not found in expression!") else ((coefficients expr) !! index)

-- | Gets the index of a given term in an expression
getTermIndex :: Term -> Expression -> Int
getTermIndex term expr = fromMaybe (-1) $ findIndex (==term) $ terms expr

-- | Tells whether or not a certain term is in an expression. WARNING: This checks for the entire term. For example, termInExpr "q1" "q1*q2" = FALSE but termInExpr "q1" "q1+q2" = TRUE
termInExpr :: Term -> Expression -> Bool
termInExpr t expr = t `elem` (terms expr)

-- | Gets modulus of the coefficient of the first term
getExprModulus :: Expression -> Int
getExprModulus (Expression cs ts) = if length cs > 0 then modulus $ head cs else 0

-- | Gets the first term in an Expression and returns it as its own expression
headExpr :: Expression -> Expression
headExpr (Expression cs ts) = Expression [(head cs)] [(head ts)]

-- | Returns the expression minus the first term
tailExpr :: Expression -> Expression
tailExpr (Expression cs ts) = Expression (tail cs) (tail ts)


instance Num Expression where
    -- (+) :: Expression -> Expression -> Expression
    expr1 + expr2
        | ((coefficients expr1 == []) `xor` (terms expr1 == [])) = error ("Bad form of expression: " ++ (show expr1) ++ " in " ++ (show expr1) ++ " + " ++ (show expr2))
        | ((coefficients expr2 == []) `xor` (terms expr2 == [])) = error ("Bad form of expression: " ++ (show expr2) ++ " in " ++ (show expr1) ++ " + " ++ (show expr2))
        | (length $ coefficients expr1) /= (length $ terms expr1) = error ("Bad form of expression: Coefficient and Term lengths do not match.")
        | (length $ coefficients expr2) /= (length $ terms expr2) = error ("Bad form of expression: Coefficient and Term lengths do not match.")
        | (numTerms expr1) == 0 = expr2
        | (numTerms expr2) == 0 = expr1
        | otherwise = let
            addTermToExpr :: ZpInt -> Term -> Expression -> Expression
            addTermToExpr c t e = let -- c = coefficient of term, t = term, e = expression
                t_index = getTermIndex t e
                e_coeff = getTermCoeffInExpression t e 
                cs = coefficients e
                ts = terms e
                -- Function for last else clause
                alterCoefficient i = if i == t_index then (c + e_coeff) else cs !! i
                in
                if (value c) == 0 -- coefficient of term added is 0
                    then e
                else if not $ termInExpr t e -- term added is not in expression
                    then Expression (cs ++ [c]) (ts ++ [t])
                else if value (c + e_coeff) == 0 -- term added cancels out term in expression
                    then Expression [(cs) !! i | i <- [0..(length cs)-1], i /= t_index] [ts !! i | i <- [0..(length ts)-1], i /= t_index]
                -- Term added is in expression and does not cancel
                else Expression [alterCoefficient i | i <- [0..(length cs)-1]] ts
            
            -- Recursive function to add terms one by one to e1
            addExprs e1 e2 = let
                cs2 = coefficients e2
                ts2 = terms e2
                in
                if (numTerms e2) == 0 then e1 else addExprs (addTermToExpr (head cs2) (head ts2) e1) (Expression (tail cs2) (tail ts2))
            in
            addExprs expr1 expr2

    -- (*) :: Expression -> Expression -> Expression
    expr1 * expr2
        -- Bad inputs
        | ((coefficients expr1 == []) `xor` (terms expr1 == [])) = error ("Bad form of expression: " ++ (show expr1) ++ " in " ++ (show expr1) ++ " + " ++ (show expr2))
        | ((coefficients expr2 == []) `xor` (terms expr2 == [])) = error ("Bad form of expression: " ++ (show expr2) ++ " in " ++ (show expr1) ++ " + " ++ (show expr2))
        | (length $ coefficients expr1) /= (length $ terms expr1) = error ("Bad form of expression: Coefficient and Term lengths do not match.")
        | (length $ coefficients expr2) /= (length $ terms expr2) = error ("Bad form of expression: Coefficient and Term lengths do not match.")
        -- multiplying by zero
        | (numTerms expr1) == 0 = Expression [] []
        | (numTerms expr2) == 0 = Expression [] []
        | otherwise = let
            multiplyTerms (c1, t1) (c2, t2) = 
                if t1 == ["~"] then
                    if t2 == ["~"] then
                        Expression [c1*c2] [["~"]]
                    else
                        Expression [c1*c2] [t2]
                else if t2 == ["~"] then
                    Expression [c1*c2] [t1]
                else
                    Expression [c1*c2] [t1 ++ t2]
            zs1 = getCoeffTermPairs expr1
            zs2 = getCoeffTermPairs expr2
            in
            foldl1 (+) [multiplyTerms z1 z2 | z1 <- zs1, z2 <- zs2]

    abs expr = error ("Not Implemented!")

    signum num = error ("Not Implemented!")

    fromInteger int = error ("Not Implemented!")

    negate (Expression cs ts) = Expression (map negate cs) ts

instance Eq Expression where
    expr1 == expr2 = let

        m1 = getExprModulus expr1
        m2 = getExprModulus expr2

        zs1 = getCoeffTermPairs expr1

        -- See if term t with coefficient c exists in expr2

        coeffMatchesInExpr2 (c, t) = (getTermCoeffInExpression t expr2) == c

        coeffs_match = foldl (&&) (True) (map coeffMatchesInExpr2 zs1)

        terms_match = foldl (&&) (True) $ map (\(c, t) -> termInExpr t expr2) zs1
        in
        (m1 == m2) && terms_match && coeffs_match

instance Show Expression where
    show (Expression cs ts) = let
        expr = Expression cs ts
        expr_len = length cs
        first_value = value $ abs $ head cs
        first_term = if (head ts) == ["~"] then "" else (foldl1 (++) (head ts))
        in
        case expr_len of
            0 -> "0"
            1 -> if first_value == 1 then
                    if first_term == "" then 
                        show first_value
                    else first_term
                else (show first_value) ++ first_term ++ " (mod " ++ (show $ getExprModulus expr) ++ ")"
            _ -> if first_value == 1 then
                    if first_term == "" then 
                        show first_value ++ " + " ++ (show $ tailExpr expr) 
                    else first_term ++ " + " ++ (show $ tailExpr expr)
                else (show first_value) ++ first_term ++ " + " ++ (show $ tailExpr expr)



-- | Converts a string of the form "q1*q2 + 2*q3*3" into (Expression [1, 6] [["q1", "q2"], ["q3"]]) and reduced given modulus
expressionFromString :: String -> Int -> Expression
expressionFromString string modulus = let
    getAddStr = splitOn ['+']
    getMultStr = splitOn ['*']
    raw_data = map getMultStr (getAddStr $ filter (/=' ') string)

    isNumeric s = (readMaybe s :: Maybe Int) /= Nothing

    getCoeff t = foldl (*) (1) (map (\a -> read a :: Int) (filter isNumeric t))
    getTerm t = if foldl (&&) True (map isNumeric t) then ["~"] else [x | x<-t, not $ isNumeric x]

    cs = map (\t -> abs $ ZpInt (getCoeff t) modulus) raw_data
    ts = map getTerm raw_data
    zs = zip cs ts
    in
    if string == "0" then (Expression [] []) else foldl (+) (Expression [] []) [Expression [fst z] [snd z] | z <- zs, (value $ fst z) /= 0] -- So as to eliminate duplicate terms


-- | Substitutes corresponding terms with given expressions (a homomorphism-type map with [Term] -> [Expression]). First attempts to substitute entire terms. If such fails, substitute single elements of each term one by one.
substitute :: [Term] -> [Expression] -> Expression -> Expression
substitute ts es e = let
    pairs = zip ts es
    m = getExprModulus e

    -- Substitutes an entire term with given expression
    substituteTermInExpr t =
                        if t `elem` ts -- If entire term is in [Term] to substitute
                            then snd ((filter (\(t0, e0) -> t0==t) pairs)!!0) -- If multiple substitutes exist, choose 1st
                        else Expression [ZpInt 1 m] [t] -- If no substitute exists, just return Expression of single term.

    wholeTermSubstitutable t = t `elem` ts

    -- Substitute each element of a single term one by one
    substituteSingleInExpr t = 
        if length t == 1
            then substituteTermInExpr [head t]
        else
            (substituteTermInExpr [head t]) * (substituteSingleInExpr (tail t))

    fullSubstitute t = if wholeTermSubstitutable t then substituteTermInExpr t else substituteSingleInExpr t

    new_exprs = map fullSubstitute $ terms e
    multipliers = [Expression [c] [["~"]] | c <- coefficients e]
    in
    foldl (+) (Expression [] []) (zipWith (*) multipliers new_exprs)

-- | Restricts expression to terms of given length
restrictTermLen :: Expression -> Int -> Expression
restrictTermLen (Expression cs ts) maxlen = let 
    answer = filter (\(c, t) -> length t <= maxlen) (zip cs ts)
    answercs = map fst answer
    answerts = map snd answer
    in
    if length answer == 0 then Expression [] [] else Expression answercs answerts