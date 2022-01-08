module Analyzer.ExpressionCalculator where

import IntermediateCode.Definitions.AbstractSyntaxTree
import Types
import Analyzer.Leftovers

calculateBoolOperation :: Operation -> Bool -> Bool-> Maybe Bool
calculateBoolOperation And x y = Just $ x && y
calculateBoolOperation Or x y = Just $ x || y
calculateBoolOperation _ _ _ = Nothing

calculateIntOperation :: Operation -> Integer -> Integer -> Maybe Integer
calculateIntOperation Plus x y = Just $ x + y
calculateIntOperation Minus x y = Just $ x - y
calculateIntOperation Times x y = Just $ x * y
calculateIntOperation Div  x y = Just $ div x y
calculateIntOperation Mod x y = Just $ mod x y
calculateIntOperation _ _ _ = Nothing 

calculateStringOperation :: Operation -> String -> String -> Maybe String
calculateStringOperation Plus x y = Just $ x ++ y
calculateStringOperation _ _ _ = Nothing

calculateExpression :: Expression' a -> Maybe Value
calculateExpression Variable {} = Nothing 
calculateExpression (Value _ value) = Just value
calculateExpression Application {} = Nothing 
calculateExpression (Neg _ expr) = do
  value <- calculateExpression expr
  case value of 
    IntegerValue  x -> return $ IntegerValue x
    _ -> Nothing
calculateExpression (Not _ expr) = do
  value <- calculateExpression expr
  case value of 
    BoolValue x -> return $ BoolValue (not x)
    _ -> Nothing
calculateExpression (Operation _ firstExpr op secondExpr) = do
  firstValue <- calculateExpression firstExpr
  secondValue <- calculateExpression secondExpr
  case (firstValue, secondValue) of 
    (BoolValue  x, BoolValue y) -> BoolValue <$> calculateBoolOperation op x y
    (IntegerValue x, IntegerValue y) -> IntegerValue <$> calculateIntOperation op x y
    (StringValue x, StringValue y) -> StringValue <$> calculateStringOperation op x y
    _ -> Nothing
calculateExpression (Compare  _ firstExpr op secondExpr) = do
  firstValue <- calculateExpression firstExpr 
  secondValue <- calculateExpression secondExpr 
  case (firstValue, secondValue) of 
    (BoolValue x, BoolValue y) -> return $ BoolValue $ getCompareFunction op x y
    (IntegerValue  x, IntegerValue y) -> return $ BoolValue $ getCompareFunction op x y
    (StringValue x, StringValue y) -> return $ BoolValue $ getCompareFunction op x y
    _ -> Nothing


calculateBoolExpression :: Expression' a -> Maybe Bool
calculateBoolExpression expression =
  case calculateExpression expression of
    Just (BoolValue value) -> Just value
    _ -> Nothing
