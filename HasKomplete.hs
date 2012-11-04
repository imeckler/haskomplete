module HasKomplete where

import Language.Haskell.Interpreter
import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>))
import Data.List (isPrefixOf, isInfixOf)
import Data.Either
import Data.Either.Utils (fromRight)
import Data.String.Utils (strip)
import Data.List.Split (splitOn)
import TypeString
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser hiding (parse)
import qualified Language.Haskell.Exts.Extension as E
import Hoogle
import Data.CaseInsensitive (mk)
import Data.ByteString.UTF8 (fromString)
import System.IO.Unsafe


{-# LANGUAGE NoMonomorphismRestriction #-}



--makeCleanFile :: FilePath -> IO ()
--makeCleanFile path = do
--    text <- readFile path

--type Import = (ModuleName, Maybe String)

capitalized :: Parser String
capitalized = (:) <$> upper <*> many letter

parseModuleName = do
    part <- capitalized
    rest <- option "" ((:) <$> char '.' <*> parseModuleName)
    return $ part ++ rest

parseConstructor = parseModuleName

--parseImport :: Parser Import
parseImport = do
    string "import"
    spaces
    isQualified <- optionMaybe (string "qualified")
    spaces
    name <- parseModuleName
    spaces
    qualAs <- optionMaybe (string "as" >> spaces >> capitalized)

    let qualification = case (isQualified, qualAs) of {
        (_, Just s)                 -> Just s;
        (Just "qualified", Nothing) -> Just name;
        (Nothing, Nothing)          -> Nothing;
    }

    return (name, qualification)

getImports' src = imps
    where (Module _ _ _ _ _ imps _) = fromParseResult . parseModule . unlines . filter ("import" `isPrefixOf`) . lines $ src


--currentApplication :: String -> String
--currentApplication = 

--makeContext :: (MonadInterpreter m) => FilePath -> IO (m ())
--makeContext path = 

--getImports :: T.Text -> 
getImports = rights . map (parse parseImport "import") . filter ("import" `isPrefixOf`) . lines

{--
    should be able to suggest types when the function being defined is given a type signature
--}

--makeContext :: (MonadInterpreter m) => String -> m ()
--makeContext = setImportsQ . getImports

wrapExpr :: String -> String
wrapExpr e = "(" ++ e ++ ")"

--recommendations :: (MonadInterpreter m) => m () -> String -> [String]
--recommendations ctx e = 
--    where expType = fromParseResult . parseType . runInterpreter $ ctx >> typeOf (wrapExpr e)

defParseMode = defaultParseMode {extensions = [E.MultiParamTypeClasses]}

simpleTypeOf :: String -> Type
simpleTypeOf expr = fromParseResult . parseTypeWithMode defParseMode . fromRight . unsafePerformIO . runInterpreter $ setImports ["Prelude", "Text.ParserCombinators.Parsec", "Language.Haskell.Exts.Parser"] >> typeOf expr

firstArgString :: Type -> String
firstArgString (TyForall _ rs t) = contextString rs ++ " => " ++ (firstArgString t)
firstArgString (TyFun first _) = typeString first

--nthArgString :: Int -> Type -> String
--nthArgString n (TyForall _ rs t) = contextString rs ++ " => " ++ nthArgString n t
--nthArgString 1 (TyFun first _) = typeString first
--nthArgString n (TyFun _ rest)  = nthArgString (n - 1) rest

nthArgString = typeString . nthArg

nthArg :: Int -> Type -> Type
nthArg 1 (TyFun first _) = first
nthArg n (TyFun _ rest) = nthArg (n - 1) rest
nthArg n (TyForall b rs t) = TyForall b rs $ nthArg n t

returnsFirstArg :: Type -> Type
returnsFirstArg (TyFun first rest) = (TyFun (TyVar (Ident "poo")) first)
returnsFirstArg (TyForall x rs t@(TyFun _ _)) = (TyForall x rs (returnsFirstArg t))

databasePath :: FilePath
databasePath = "/Users/izzy/Library/Haskell/ghc-7.4.1/lib/hoogle-4.2.13/share/databases/default.hoo"

database :: IO Database
database = loadDatabase databasePath

queryHoogle :: Type -> IO [Result]
queryHoogle t = map snd . flip search query <$> database
    where query = fromRight $ parseQuery Haskell (firstArgString t)

searchHoogle :: String -> IO [Result]
searchHoogle s = map snd . flip search query <$> database
    where query = fromRight $ parseQuery Haskell s

complHoogle s = flip completions s <$> database
debCompleHoogle = unsafePerformIO . complHoogle
debSearchHoogle = unsafePerformIO . searchHoogle
debQueryHoogle = unsafePerformIO . queryHoogle

resultsToStrings = map (showTagText . self) . filter isTypedResult

isTypedResult :: Result -> Bool
isTypedResult = (isInfixOf "::") . showTagText . self

suggest :: (Type -> Type) -> String -> [String]
--suggest strategy = map (showTagText . self) . filter isTypedResult . debQueryHoogle . unQualType . strategy . simpleTypeOf
suggest strategy = map (showTagText . self) . debSearchHoogle . typeString . unQualType . strategy . simpleTypeOf

-- don't bother suggesting anything for a completely unrestricted type.
-- ie if a type variable does not occur in the restriction, and it is the whole query,
-- don't make the query.

ims = "import Language.Haskell.Interpreter\nimport qualified Data.Text as T\nimport Text.ParserCombinators.Parsec\nimport Control.Applicative ((<$>), (<*>))\nimport Data.List (isPrefixOf)\nimport Data.Either\n"

