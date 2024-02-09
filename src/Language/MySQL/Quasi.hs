module Language.MySQL.Quasi where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.MySQL.Parser
import Language.MySQL.ParserUtils
import Language.MySQL.AST
import Control.Monad.Writer.Strict
import Language.MySQL.Typechecker
import Text.Parsec
import Data.Char
import Language.Haskell.TH.Syntax
import Language.MySQL.Renderer
import qualified Data.Map as M

-- TODO: move to a separate package

getSchema :: FilePath -> Q [(TableName, [(ColumnName, ColumnInfo)])]
getSchema file = do
  addDependentFile file
  f <- liftIO $ readFile file
  mapM helper (lines f)
 where
  columnParser = do
    ai <- withSpace $ option False $ True <$ string "AI"
    cname <- withSpace $ many1 $ satisfy (\x -> isAsciiLower x || x == '_' || isNumber x)
    t <- typeGenParser
    return (ColumnName cname, ColumnInfo { columnInfoType = t, columnInfoAI = ai })
  schemaParser = do
    tname <- many1 $ satisfy (\x -> isAsciiLower x || x == '_' || isNumber x)
    _ <- withSpace $ char ':'
    cols <- sepBy1 columnParser commaParser
    return (TableName tname, cols)
  helper :: String -> Q (TableName, [(ColumnName, ColumnInfo)])
  helper s = case runParser schemaParser () file s of
    Left err -> fail (show err)
    Right v -> return v

getSqlFuns :: FilePath -> Q SqlFuns
getSqlFuns file = do
  addDependentFile file
  f <- liftIO $ readFile file
  SqlFuns . M.fromList <$> mapM helper (lines f)
 where
  schemaParser = do
    fname <- funNameParser
    _ <- withSpace (char ':')
    args <- sepBy1 typeGenParser commaParser
    _ <- withSpace (string "->")
    ret <- typeGenParser
    return (fname, (args, ret))
  helper :: String -> Q (FunName, ([ParsedType], ParsedType))
  helper s = case runParser schemaParser () file s of
    Left err -> fail (show err)
    Right v -> return v

getDefColumns :: FilePath -> Q DefColumns
getDefColumns file = do
  addDependentFile file
  f <- liftIO $ readFile file
  DefColumns . M.fromList <$> mapM helper (lines f)
 where
  columnParser = DefColumnsCol <$> option DontUpdate (DoUpdate <$ char '^') <*> withSpace (ColumnName <$> many1 (satisfy (\x -> isAsciiLower x || x == '_')))
    <|> DefColumnsType <$> typeNameParser
  schemaParser = do
    typeNames <- many1 $ satisfy isAlphaNum -- TODO: remove repetition
    spaces
    _ <- char ','
    spaces
    tname <- many1 $ satisfy (\x -> isAsciiLower x || x == '_' || isNumber x)
    _ <- char ':'
    spaces
    cols <- sepBy1 columnParser (spaces >> char ',' >> spaces)
    return (TypeName typeNames, (TableName tname, cols))
  helper :: String -> Q (TypeName, (TableName, [DefColumnsCol]))
  helper s = case runParser schemaParser () file s of
    Left err -> fail (show err)
    Right v -> return v

mysql :: QuasiQuoter
mysql = QuasiQuoter
  { quoteExp = \str -> do
      loc <- location
      case parseFromLoc (parserEntire anyQueryParser) loc str of
        Left e -> fail (show e)
        Right q -> do
          schemaAll <- getSchema "./res/schema.txt"
          dc <- getDefColumns "./res/def_columns.txt"
          funs <- getSqlFuns "./res/functions.txt"
          (q', tc) <- runWriterT (typecheckAnyQuery schemaAll funs dc q)
          renderAnyQuery q' tc
  , quoteDec = undefined
  , quotePat = undefined
  , quoteType = undefined
  }
