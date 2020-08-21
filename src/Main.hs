{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Main where

import Parser
import DualNum

import Numeric (readFloat)
import Text.Read (readMaybe)
import Control.Monad (join)

main :: IO ()
main = do
  putStrLn "\nWelcome to the derivative calculator! Enter your function [f(x)], the order [n] of the derivative you'd like to compute, and the point [a] at which you'd like to compute the derivative."
  expr <- repeatUntilRight $ putStr "\nf(x) = " >> parseExpr <$> getLine
  order :: Int <- repeatUntilRight $ do
    putStr "\nn = "
    note "Invalid integer." . join <$> (fmap (validate (>=0)) . readMaybe <$> getLine)
  val <- repeatUntilRight $ do
    putStr "\na = "
    note "Invalid float." <$> getFloat
  putStrLn $ "\nThe result is: " ++ show (ndiff order (eval expr) val)

repeatUntilRight :: Show err => IO (Either err a) -> IO a
repeatUntilRight io = do
  result <- io
  case result of
    Right val -> return val
    Left err -> putStrLn (show "Error: " ++ show err)
             >> repeatUntilRight io

getFloat :: Floating a => IO (Maybe a)
getFloat = do
  reading <- readFloat <$> getLine
  return $ if null reading
    then Nothing
    else Just (fromRational . toRational . fst . head $ reading)

validate :: (a -> Bool) -> a -> Maybe a
validate p a = if p a then Just a else Nothing

note :: e -> Maybe a -> Either e a
note err = \case
  Just a -> Right a
  Nothing -> Left err
