{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.List
import Language.Haskell.Exts
import System.Environment
import System.Process
import System.Exit

data Query = FreeVariables
           | LambdaBody
           | LambdaArgs

main :: IO ()
main = do
  code                <- getContents
  queryArg:filePath:_ <- getArgs
  let maybeQuery      = parseQueryArg queryArg
  case maybeQuery of
    Just query -> putStrLn =<< runQuery query filePath code
    Nothing    -> error "Unknown Query"

parseQueryArg :: String -> Maybe Query
parseQueryArg s | s == "freeVariables" = Just FreeVariables
                | s == "lambdaBody"    = Just LambdaBody
                | s == "lambdaArgs"    = Just LambdaArgs
                | otherwise            = Nothing 

runQuery :: Query -> FilePath ->  String -> IO String
runQuery FreeVariables srcFile code = freeVariables srcFile code
runQuery LambdaBody _ code = return $ lambdaBody code
runQuery LambdaArgs _ code = return $ lambdaArgs code

------------------------------------------------------------------------------
freeVariables :: String -> String -> IO String 
freeVariables srcPath code = case parseExp code of
  ParseOk ast -> do 
   names <- dropModuleVariableNames srcPath $ extractFreeVarialbes ast
   return . dropCommas $ show names
  _ -> return "[error]"

isInModuleScope :: String -> String -> IO Bool
isInModuleScope srcPath symName = do 
  (exitCode, _, stderr) <- search symName
  case exitCode of
    ExitFailure _ -> return $ "Not in scope" `isInfixOf` stderr
    _             -> return False
  where
    search sn = readProcessWithExitCode "hdevtools"
                                        ["info", srcPath, sn]
                                        ""
                                        
lambdaBody ::  String -> String
lambdaBody code = case parseExp code of
  ParseOk ast -> show . extractLambdaBody $ ast
  _           -> "[]"  
  

lambdaArgs :: String -> String
lambdaArgs code = case parseExp code of
  ParseOk ast -> extractLambdaArgs ast
  _           -> "[Hello]" 
  
dropModuleVariableNames :: FilePath -> [String] -> IO [String]
dropModuleVariableNames srcPath = filterM (isInModuleScope srcPath)


extractFreeVarialbes :: GenericQ [String]
extractFreeVarialbes ast = allNames (allVariables ast) \\ 
                           allNames (allBindings ast)

extractLambdaArgs :: Exp -> String
extractLambdaArgs (Lambda _ ast _) = dropCommas . show $ allNames ast
extractLambdaArgs _                = "[]"                 

extractLambdaBody :: Exp -> String
extractLambdaBody (Lambda _ _ ast) = prettyPrint ast
extractLambdaBody _ = "[]"
                                     
allVariables :: GenericQ [Exp]
allVariables = listify isVar

allBindings :: GenericQ [Pat]
allBindings = listify isBinding

allNames :: GenericQ [String]
allNames = everything (++) ([] `mkQ` fmap (: []) getStringFromName)

isVar :: Exp -> Bool
isVar (Var  _) = True
isVar  _       = False

isBinding :: Pat -> Bool
isBinding (PVar _) = True
isBinding _        = False

getStringFromName :: Name -> String
getStringFromName (Symbol str) = str
getStringFromName (Ident str) = str

dropCommas :: String -> String
dropCommas = filter (/= ',')

fn :: String -> String -> String
fn c d = concatSomeStuff c d "Eric " "Says "

concatSomeStuff :: [a] -> [a] -> [a] -> [a] -> [a]
concatSomeStuff c d a b = concat [a, b, c, d]
