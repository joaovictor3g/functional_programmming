data Quest = Yes | No | IDontKnow

getQuest Yes = "Vamo nessa!"
getQuest No = "Eu hein!"
getQuest IDontKnow = "Tô pensando..."

-- data Maybe = Nothing | Just a

div' _ 0 = Nothing
div' a b = Just (div a b)

data TwoDegree = 
   Invalid 
 | Complex 
 | Roots Float Float
 deriving (Show)

twoDegreFn a b c
 | a == 0 = Invalid
 | delta < 0 = Complex
 | otherwise = Roots x1 x2
  where delta = b ** 2 - 4 * a * c
        x1 = (-b + (sqrt delta)) / (2 * a)
        x2 = (-b - (sqrt delta)) /  (2 * a)
