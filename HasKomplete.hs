{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Haskell.Interpreter hiding (ModuleName)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser hiding (parse)
import qualified Language.Haskell.Exts.Extension as E
import Hoogle
import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>))
import Data.List (isPrefixOf, isInfixOf)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)
import Data.Maybe (fromMaybe)
import Data.Either
import Data.Either.Utils (fromRight)
--import Data.ByteString.UTF8 (fromString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import qualified Data.ByteString.Lazy.Char8 as T
import System.IO (openTempFile)
import System.IO.Unsafe
import System.Directory (getTemporaryDirectory)
import System.Environment
import TypeOps
import Strategies
--import Foreign hiding (unsafePerformIO)
--import Foreign.C.Types
--import Foreign.C.String
--import Foreign.Marshal.Array
import qualified Data.Attoparsec.Text as A
import Data.Char


fromModuleName :: ModuleName -> String
fromModuleName (ModuleName s) = s

-- change name
convertImportDecl :: ImportDecl -> (String, Maybe String)
convertImportDecl i = (modName, qualName)
    where modName  = fromModuleName $ importModule i
          qualName = case importQualified i of
            False -> Nothing
            True  -> Just $ fromMaybe modName (fromModuleName <$> importAs i)
-- hmm. What to  do about unqualified "as" imports. Don't think setImports supports this

getImports :: String -> [(String, Maybe String)]
getImports src = map convertImportDecl imps
    where (Module _ _ _ _ _ imps _) = fromParseResult . parseModule $ importLines
          importLines = unlines . filter ("import" `isPrefixOf`) . lines $ src

getImports' src = imps
    where (Module _ _ _ _ _ imps _) = fromParseResult . parseModule . unlines . filter ("import" `isPrefixOf`) . lines $ src


--currentApplication :: String -> String
--currentApplication = 

--makeContext :: (MonadInterpreter m) => FilePath -> IO (m ())
--makeContext path = 

{--
    should be able to suggest types when the function being defined is given a type signature
--}

makeContext :: (MonadInterpreter m) => String -> m ()
makeContext = setImportsQ . getImports

wrapExpr :: String -> String
wrapExpr e = "(" ++ e ++ ")"

--recommendations :: (MonadInterpreter m) => m () -> String -> [String]
--recommendations ctx e = 
--    where expType = fromParseResult . parseType . runInterpreter $ ctx >> typeOf (wrapExpr e)

defParseMode = defaultParseMode {extensions = [E.MultiParamTypeClasses]}

debTypeOf :: String -> Type
debTypeOf expr = fromParseResult . parseTypeWithMode defParseMode . fromRight . unsafePerformIO . runInterpreter $ lookupType
    where interpreter = setImports ["Prelude", "Text.ParserCombinators.Parsec", "Language.Haskell.Exts.Parser"] 
          lookupType = interpreter >> typeOf expr

firstArgString :: Type -> String
firstArgString (TyForall _ rs t) = contextString rs ++ " => " ++ (firstArgString t)
firstArgString (TyFun first _) = typeString first

nthArgString :: Int -> Type -> String
nthArgString n = typeString . nthArg n

databasePath :: String
databasePath = "/Users/izzy/Library/Haskell/ghc-7.4.1/lib/hoogle-4.2.13/share/databases/default.hoo"

database :: IO Database
database = loadDatabase databasePath

queryHoogle :: Type -> IO [Result]
queryHoogle t = map snd . flip search query <$> database
    where query = fromRight $ parseQuery Haskell (firstArgString t)

searchHoogle :: String -> IO [Result]
searchHoogle s = map snd . flip search query <$> database
    where query = fromRight $ parseQuery Haskell s

complHoogle s   = flip completions s <$> database
debCompleHoogle = unsafePerformIO . complHoogle
debSearchHoogle = unsafePerformIO . searchHoogle
debQueryHoogle  = unsafePerformIO . queryHoogle

resultsToStrings :: [Result] -> [String]
resultsToStrings = map (showTagText . self) . filter isTypedResult

isTypedResult :: Result -> Bool
isTypedResult = (isInfixOf "::") . showTagText . self

suggestWith :: Strategy -> String -> [String]
suggestWith strategy = map (showTagText . self) . debSearchHoogle . typeString . unQualType . strategy . debTypeOf

--foreign export ccall suggest :: CString -> IO (Ptr CString)
--suggest :: CString -> IO (Ptr CString)
--suggest str = do
--    s <- peekCString str
--    cs <- mapM newCString $ suggestWith returnsFirstArg s
--    newArray cs

--parseTypeAnnoation :: A.Parser T.Text
--parseTypeAnnoation = let valid x = x /= '=' && x /= '\n' in do
--    name <- A.takeWhile1 isAlpha
--    A.skipSpace
--    A.string "::"
--    typ <- A.takeWhile1 valid
--    return $ name ++ " :: " ++ typ ++ "\n" ++ name ++ " = undefined"

--stubSource :: T.Text -> T.Text
--stubSource = T.unlines . rights . map (A.parseOnly parseTypeAnnoation) . T.lines

--stubAndWrite :: String -> IO String
--stubAndWrite f = do
--    tmpD <- getTemporaryDirectory
--    (tmp, tmpH) <- openTempFile tmpD "foo.hs"
--    T.readFile f >>= (T.hPutStr tmpH . stubSource)
--    return tmp



main = do
    args <- getArgs
    let expr = head args
    mapM_ putStrLn . take 7 $ suggestWith returnsFirstArg expr

-- don't bother suggesting anything for a completely unrestricted type.
-- ie if a type variable does not occur in the restriction, and it is the whole query,
-- don't make the query.

