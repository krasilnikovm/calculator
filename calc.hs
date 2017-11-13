import Data.Char (isDigit)
import Data.Typeable

solveListExpr expr = (solvePolandExpresion (getPNotation expr [] [] ) [])

solveRawExpr expr = (solvePolandExpresion (getPNotation (parseString expr [] []) [] [] ) [])

getPNotation expr pExpr ops
    | null expr && null ops = pExpr
    | null expr && not (null ops) = getPNotation expr (pExpr ++ (last ops):[]) (init ops)
    | isDigit (head (head expr)) = getPNotation (drop 1 expr) (pExpr ++ (head expr):[]) ops
    | head expr == "(" = getPNotation (drop 1 expr) pExpr (ops ++ (head expr):[])
    | head expr == ")" = popOps expr pExpr ops
    | null ops = getPNotation (drop 1 expr) pExpr (ops ++ (head expr):[])
    | getPriorety (head expr) <= getPriorety (last ops) =
    getPNotation (drop 1 expr) (pExpr ++ (last ops):[]) ((init ops) ++ (head expr):[])
    | otherwise = getPNotation (drop 1 expr) pExpr (ops ++ (head expr):[])

popOps expr pExpr ops
    | last ops == "(" = getPNotation expr pExpr (init ops)
    | otherwise = popOps (drop 1 expr) (pExpr ++ (last ops):[]) (init ops)

getPriorety op
    | op == "*" = 2
    | op == "/" = 2
    | op == "+" = 1
    | op == "-" = 1
    | otherwise = -1

solvePolandExpresion expr stack
    | null expr = head stack
    | isOperation (head expr) = solvePolandExpresion (drop 1 expr) ((init (init stack)) ++ (perform (last (init stack)) (last stack)  (head(head expr):[]) ):[])
    | otherwise = solvePolandExpresion (drop 1 expr) (stack ++ (read (head expr) :: Float):[])

perform num1 num2 op
    | op == "+" = num1 + num2
    | op == "-" = num1 - num2
    | op == "*" = num1 * num2
    | op == "/" = num1 / num2
    | otherwise = 0

isOperation op = op == "*" || op == "/" || op == "+" || op == "-"

parseString expr exprList numb
    | null expr && not (null numb) = parseString (drop 1 expr) (exprList ++ (numb:[])) ""
    | null expr = exprList
    | head expr == ' ' = parseString (drop 1 expr) exprList numb
    | head expr == '(' =  parseString (drop 1 expr) (exprList ++ (((head expr):[]):[])) numb
    | head expr == ')' =  parseString (drop 1 expr) ((exprList ++ (numb:[])) ++ (((head expr):[])):[]) ""
    | isOperation ((head expr):[]) && (null numb) = parseString (drop 1 expr) ((exprList) ++ (((head expr):[])):[]) ""
    | isOperation ((head expr):[]) = parseString (drop 1 expr) ((exprList ++ (numb:[])) ++ (((head expr):[])):[]) ""
    | otherwise = parseString (drop 1 expr) exprList (numb ++ ((head expr):[]))

