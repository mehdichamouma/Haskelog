
module Main where
import Data.Map (Map)
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

data Value = Const String | Var String deriving (Eq, Show)
type Expression = [Value]
type Fact = [String]
type Clause = Expression
type Condition = Expression
type Query = [String]
type Binding = (Value, Value)
type Bindings = [Binding]

bind :: Binding -> Expression -> Expression
bind (k, c) exp = map (\w -> if (w == k) then c else w) exp

substitute :: Bindings -> Expression -> Expression
substitute bindings exp = foldl (flip bind) exp bindings

substituteAll :: Bindings -> [Expression] -> [Expression]
substituteAll b e = map (substitute b) e

match :: Value -> String -> Bool
match (Var _) _ = True
match (Const v1) s = (v1 == s)

matchFact :: Expression -> Fact -> Bool
matchFact exp fact
  | differentLength exp fact = False
  | otherwise = all (uncurry match) (zip exp fact)
  where differentLength a b = (length a) /= (length b)

genBindings :: Expression -> Fact -> Bindings
genBindings [] [] = []
genBindings ((Const _):xe) (_:xf) = genBindings xe xf
genBindings ((Var e):xe) (f:xf) = (Var e, (Const f)) : genBindings xe xf

solve :: Expression -> [Fact] -> [Bindings]
solve _ [] = []
solve exp (f:xf)
  | matchFact exp f = (genBindings exp f) : (solve exp xf)
  | otherwise = solve exp xf

combine :: Bindings -> Bindings -> Bindings
combine a b = a ++ b

nextConditions :: [Condition] -> [Fact] -> [[Condition]]
nextConditions (c:xc) f = map (\b -> (substituteAll b xc))(solve c f)


resolve :: [Condition] -> [Fact] -> [Bindings]
resolve (c:[]) f = solve c f
resolve (c:xc) f = concatMap (\b -> map (combine b) (resolve (substituteAll b xc) f)) (solve c f)


queryBindings :: Query -> Expression -> Bindings
queryBindings [] _ = []
queryBindings ("?":xq) (c:xc) = queryBindings xq xc
queryBindings (q:xq) (Var c:xc) = (Var c, Const q) : queryBindings xq xc
queryBindings (q:xq) (c:xc) = queryBindings xq xc

queryParams :: Query -> Clause -> [Value]
queryParams [] _ = []
queryParams ("?":xq) (c:xc) = c : queryParams xq xc
queryParams (q:xq) (c:xc) = queryParams xq xc

filterBindings :: [Value] -> Bindings -> [String]
filterBindings _ [] = []
filterBindings f ((Var k, Const c):xb)
  | elem (Var k) f = c : filterBindings f xb
  | otherwise = filterBindings f xb

doQuery :: [Fact] -> Expression -> [Condition] -> Query -> [[String]]
doQuery facts clause cond q = map (filterBindings (queryParams q clause)) (resolve (preparedConditions) facts)
  where preparedConditions = substituteAll (queryBindings q clause) cond

parseValue :: String -> Value
parseValue (':':xs) = Var xs
parseValue (x) = Const x

parseExpression :: String -> Expression
parseExpression str = map parseValue (words str)


haskelog :: [String] -> String -> [String] -> String -> [[String]]
haskelog facts clause conds qs = doQuery (map words facts) (parseExpression clause) (map parseExpression conds) (words qs)

facts =  ["likes romeo juliette"
         ,"likes romeo deborah"
         ,"likes romeo mathilde"
         ,"likes batman juliette"
         ,"likes juliette bob"
         ,"likes bob romeo"
         ,"isNice romeo"
         ,"isNice bob"]

clause = "likes :x :y"

conditions = ["likes :x :y"]

conditions' = ["likes :y :x"
             ,"isNice :y"]



conditions'' = ["likes :x :z"
               ,"likes :z :y"
               ,"isNice :x"]


facts_family = ["father a b"
                ,"father b c"
                ,"father c d"
                ,"brother c a"
                ,"uncle c d"]

clause_family_gf = "grandfather :x :y"

conditions_family_gf = ["father :x :z"
                        ,"father :z :y"]

clause_family_relations = "relations :x :y"
conditions_family_relations = [":z :x :y"]

main = print "ok"
