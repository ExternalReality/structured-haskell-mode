{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Take in Haskell code and output a vector of source spans and
-- their associated node type and case.

module Main (main) where

import Control.Applicative
import Data.Data
import Data.Maybe
import Language.Haskell.Exts.Annotated
import System.Environment
import Temporary.API

-- | A generic Dynamic-like constructor -- but more convenient to
-- write and pattern match on.
data D = forall a. Data a => D a

-- | A parser. Presently there is only 'parseTopLevel', but in the
-- past, and in the future, there will be the facility to parse
-- specific parse of declarations, rather than re-parsing the whole
-- declaration which can be both slow and brittle.
type Parser = ParseMode -> String -> ParseResult D

-- | The 'empty' method isn't (shouldn't be) used, so this isn't a
-- real Alternative instance (perhaps a Semigroup might do?). But it's
-- handy.
instance Alternative ParseResult where
  empty = ParseFailed undefined undefined
  ParseFailed{} <|> x = x
  x <|> _             = x

--- | Main entry point.
main :: IO ()
main = do
  code <- getContents
  action:typ:_ <- getArgs
  let act = fromMaybe (error "unknown action") (fromString action)
  outputWith act typ code

data Action = Check | Parse | Lint
            deriving Eq

fromString :: String -> Maybe Action
fromString s | s == "check"  = Just Check
             | s == "parse"  = Just Parse
             | s == "lint"   = Just Lint
             | otherwise     = Nothing

-- | Output some result with the given action (check/parse/etc.),
-- parsing the given type of AST node. In the past, the type was any
-- kind of AST node. Today, it's just a "decl" which is covered by
-- 'parseTopLevel'.
outputWith :: Action -> String -> String -> IO ()
outputWith action typ code =
  case typ of
    "decl" ->
        output action parseTopLevel code
    _ -> error "Unknown parser type."

-- | Output AST info for the given Haskell code.
output :: Action -> Parser -> String -> IO ()
output action parser code =
  case parser parseMode code of
    ParseFailed _    e -> if action == Lint
                            then putStrLn "[]"
                            else error e                
    ParseOk (D ast) -> runAction action ast code

runAction :: Data a => Action -> a -> String -> IO ()
runAction Check _ _      = return ()
runAction Parse ast _    = putStrLn ("[" ++ concat (genHSE ast) ++ "]")
runAction Lint _ code = do
  ideas <- genIdeas code
  if length ideas > 0 
     then putStrLn $ "[" ++ (concat $ serializeIdea <$> ideas) ++ "]" 
     else putStrLn "[]" 

serializeIdea ::  Idea -> String
serializeIdea idea = 
  case ideaTo idea of
    Just to  -> filter (/= ',') $ show (ideaContent to (ideaSpan idea))
    Nothing  -> ""
 where ideaContent to SrcSpan{..} =  [ show (ideaSeverity idea) 
                                     , findSuggestion (ideaHint idea)
                                     , ideaFrom idea 
                                     , to
                                     , show srcSpanStartLine
                                     , show srcSpanStartColumn
                                     , show srcSpanEndLine
                                     , show srcSpanEndColumn
                                     ]

-- FIXME: memoize, efficient data structure, i18n, get complete list from hlint code.
findSuggestion :: String -> String
findSuggestion i = fromMaybe "Error: No suggestion string mapping yet" $ 
                   lookup i [ ("Redundant lambda", "remove redundant lambda")
                            , ("Avoid lambda", "move lambda to top level")
                            , ("Collapse lambdas", "collapse nested lambdas") 
                            , ("Redundant bracket", "remove redundant bracket")
                            ]

genIdeas :: String -> IO [Idea] 
genIdeas code =  do 
  (parseFlags, classifications, hint) <- autoSettings
  eitherErrorModule <- parseModuleEx parseFlags "" (Just code)
  case eitherErrorModule of
     Left _    -> error "error parsing code" 
     Right msi -> return $ applyHints classifications hint [msi]

-- | An umbrella parser to parse:
--
-- * A declaration.
--
-- * An import line (not normally counted as a declaration).
--
-- * A module header (not normally counted either).
--
-- * A module pragma (normally part of the module header).
--
parseTopLevel :: ParseMode -> String -> ParseResult D
parseTopLevel mode code =
  D . fix <$> parseDeclWithMode mode code   <|>
  D       <$> parseImport mode code         <|>
  D . fix <$> parseModuleWithMode mode code <|>
  D       <$> parseModulePragma mode code

  where
    fix :: AppFixity ast => ast SrcSpanInfo -> ast SrcSpanInfo
    fix ast = fromMaybe ast (applyFixities baseFixities ast)


-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode =
  defaultParseMode { extensions = allExtensions
                   , fixities   = Nothing
                   }
 where allExtensions = filter isDisabledExtention knownExtensions
       isDisabledExtention (DisableExtension _) = False
       isDisabledExtention _                    = True

-- | Generate a list of spans from the HSE AST.
genHSE :: Data a => a -> [String]
genHSE x =
  case gmapQ D x of
    zs@(D y:ys) ->
      case cast y of
        Just s ->
          spanHSE (show (show (typeOf x)))
                  (showConstr (toConstr x))
                  (srcInfoSpan s) :
          concatMap (\(i,D d) -> pre x i ++ genHSE d)
                    (zip [0..] ys)
        _ ->
          concatMap (\(D d) -> genHSE d) zs
    _ -> []

-- | Pre-children tweaks for a given parent at index i.
--
pre :: (Typeable a) => a -> Integer -> [String]
pre x i =
  case cast x of
    -- <foo { <foo = 1> }> becomes <foo <{ <foo = 1> }>>
    Just (RecUpdate SrcSpanInfo{srcInfoPoints=(start:_),srcInfoSpan=end} _ _)
      | i == 1 ->
        [spanHSE (show "RecUpdates")
                 "RecUpdates"
                 (SrcSpan (srcSpanFilename start)
                          (srcSpanStartLine start)
                          (srcSpanStartColumn start)
                          (srcSpanEndLine end)
                          (srcSpanEndColumn end))]
    _ -> case cast x :: Maybe (Deriving SrcSpanInfo)  of
           -- <deriving (X,Y,Z)> becomes <deriving (<X,Y,Z>)
           Just (Deriving _ ds@(_:_)) ->
             [spanHSE (show "InstHeads")
                      "InstHeads"
                      (SrcSpan (srcSpanFilename start)
                               (srcSpanStartLine start)
                               (srcSpanStartColumn start)
                               (srcSpanEndLine end)
                               (srcSpanEndColumn end))
             |Just (IHead (SrcSpanInfo start _) _ _) <- [listToMaybe ds]
             ,Just (IHead (SrcSpanInfo end _) _ _) <- [listToMaybe (reverse ds)]]
           _ -> []

-- | Generate a span from a HSE SrcSpan.
spanHSE :: String -> String -> SrcSpan -> String
spanHSE typ cons SrcSpan{..} = "[" ++ spanContent ++ "]"
  where unqualify   = dropUntilLast '.'
        spanContent =
          unwords [unqualify typ
                  ,cons
                  ,show srcSpanStartLine
                  ,show srcSpanStartColumn
                  ,show srcSpanEndLine
                  ,show srcSpanEndColumn]

------------------------------------------------------------------------------
-- General Utility

-- | Like 'dropWhile', but repeats until the last match.
dropUntilLast :: Char -> String -> String
dropUntilLast ch = go []
  where
    go _ (c:cs) | c == ch = go [] cs
    go acc (c:cs)         = go (c:acc) cs
    go acc []             = reverse acc

--------------------------------------------------------------------------------
-- Parsers that HSE hackage doesn't have

parseImport :: ParseMode -> String -> ParseResult (ImportDecl SrcSpanInfo)
parseImport mode code =
  case parseModuleWithMode mode code of
    ParseOk (Module _ _ _ [i] _) -> return i
    ParseOk _ -> ParseFailed noLoc "parseImport"
    ParseFailed x y -> ParseFailed x y

parseModulePragma :: ParseMode -> String -> ParseResult (ModulePragma SrcSpanInfo)
parseModulePragma mode code =
  case parseModuleWithMode mode (code ++ "\nmodule X where") of
    ParseOk (Module _ _ [p] _ _) -> return p
    ParseOk _ -> ParseFailed noLoc "parseModulePragma"
    ParseFailed x y -> ParseFailed x y
