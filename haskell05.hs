-- Prática 05 de Haskell
-- Nome: Leonardo Cargnin Krugel


-- 1)
bmi :: Float -> Float -> String
bmi peso alt =
    let bmi = peso / (alt^2)
    in if bmi >= 30 then "Acima"
        else if bmi <= 18.5 then "Abaixo"
        else "Normal"


-- 2)
bmi' :: Float -> Float -> String
bmi' peso alt = 
        if bmi >= 30 then "Acima"
        else if bmi <= 18.5 then "Abaixo"
        else "Normal"
    where bmi = peso / (alt^2)


-- 3)
cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
    where digits = take 9 cpf
          dv1 = cpfDV digits [10,9..]
          dv2 = cpfDV (digits ++ [dv1]) [11,10..]


cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults =
        let expr = (sum $ zipWith (*) digits mults) `mod` 11
        in if expr < 2 then 0 else 11-expr


-- 4)
andTable :: [(Bool,Bool,Bool)]
andTable = [(a,b,a && b) | a <- p, b <- q]
    where p = [True,False]
          q = [True,False]