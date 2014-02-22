{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Data
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.List
import Language.Haskell.Exts.Annotated
import System.Environment

data Query = FreeVariables
           | LambdaBody
           | LambdaArgs


main :: IO ()
main = do
  code       <- getContents
  queryArg:_ <- getArgs
  let maybeQuery  = parseQueryArg queryArg
  case maybeQuery of
    Just query -> putStrLn $ runQuery query code
    Nothing    -> error "Unknown Query"

parseQueryArg :: String -> Maybe Query
parseQueryArg s | s == "freeVariables" = Just FreeVariables
                | s == "lambdaBody"    = Just LambdaBody
                | s == "lambdaArgs"    = Just LambdaArgs
                | otherwise            = Nothing 

runQuery :: Query -> String -> String
runQuery FreeVariables = freeVariables 
runQuery LambdaBody    = lambdaBody

------------------------------------------------------------------------------
-- queries on expression

lambdaBody ::  String -> String
lambdaBody code = case parseExp code of
  ParseOk ast -> extractLambdaBody ast
  
lambdaArgs :: String -> String
lambdaArgs code = case parseExp code of
  ParseOk ast -> extractLambdaArgsSpan ast
  
extractLambdaArgsSpan :: Data l => Exp l -> String
extractLambdaArgsSpan (Lambda _ ast _) = dropCommas . show $ allNames ast                 

extractLambdaBody :: Exp SrcSpanInfo -> String
extractLambdaBody (Lambda _ _ ast) = serializeSrcSpanInfo $ ann ast
extractLambdaBody _ = "[]"

freeVariables :: String -> String 
freeVariables code = case parseExp code of
  ParseOk ast -> dropCommas . show $ allNames (allVariables ast) \\
                                     allNames (allBindings ast)
                                     
allVariables :: GenericQ [Exp SrcSpanInfo]
allVariables = listify isVar

allBindings :: GenericQ [Pat SrcSpanInfo]
allBindings = listify isBinding

allNames :: GenericQ [String]
allNames = everything (++) ([] `mkQ` fmap (: []) getStringOffName)

isVar :: Exp l -> Bool
isVar (Var _ _) = True
isVar  _        = False

isBinding :: Pat l -> Bool
isBinding (PVar _ _) = True
isBinding _          = False

getStringOffName :: Name SrcSpanInfo -> String
getStringOffName (Symbol _ str) = str
getStringOffName (Ident _ str) = str

serializeSrcSpanInfo :: SrcSpanInfo -> String
serializeSrcSpanInfo (SrcSpanInfo (SrcSpan{..}) _) = dropCommas . show $
  [ show srcSpanStartLine
  , show srcSpanStartColumn
  , show srcSpanEndLine
  , show srcSpanEndColumn
  ]                 

dropCommas :: String -> String
dropCommas = filter (/= ',')
