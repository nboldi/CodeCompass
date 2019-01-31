{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Development.CodeCompass.Parser.Plugin where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Control.Monad.IO.Class
import DynFlags (getDynFlags)
import Plugins
import HscTypes
import TcRnTypes
import HsExtension
import HsDecls
import HsExpr
import HsImpExp
import Name
import SrcLoc
import FastString
import Avail
import Outputable
import HsDoc
import Data.List (nubBy, sortOn)
import Data.Function
import qualified Data.Map as Map
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.IO.Unlift
import           Data.Text hiding (drop, dropWhile, filter, length)
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Conduit

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
HsFile
    filename String
    deriving Show
    deriving Eq
    deriving Ord
HsSourceLoc
    file HsFileId
    startRow Int
    startCol Int
    endRow Int
    endCol Int
    deriving Show
    deriving Eq
    deriving Ord
HsName
    file HsFileId
    nameStr String
    nameLocation HsSourceLocId
    definedAt HsSourceLocId Maybe
    deriving Show
    deriving Eq
    deriving Ord
|]

plugin :: Plugin
plugin = defaultPlugin {
    -- parsedResultAction = parsed
  -- typeCheckResultAction = typecheckPlugin
  -- , spliceRunAction = spliceRun
  -- , interfaceLoadAction = interfaceLoad
  renamedResultAction = renamedAction
  }

type DB = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

type ParseM = StateT ParseState DB

data ParseState = PS { fileCache :: Map.Map HsFile (Key HsFile)
                     , sourceLocCache :: Map.Map HsSourceLoc (Key HsSourceLoc)
                     }

-- TODO: use lenses

onFileCache :: (Map.Map HsFile (Key HsFile) -> Map.Map HsFile (Key HsFile)) -> ParseState -> ParseState
onFileCache f s = s { fileCache = f (fileCache s) }

onSourceLocCache :: (Map.Map HsSourceLoc (Key HsSourceLoc) -> Map.Map HsSourceLoc (Key HsSourceLoc)) -> ParseState -> ParseState
onSourceLocCache f s = s { sourceLocCache = f (sourceLocCache s) }



initState :: ParseState
initState = PS Map.empty Map.empty


renamedAction :: [CommandLineOption]
                    -> TcGblEnv -> HsGroup GhcRn
                    -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction [] _ _
  = error "CodeCompass plugin needs a connection string to a database to store the data."
renamedAction (connString:_) tc gr
  = liftIO $ runSqlite (pack (drop 1 $ dropWhile (/='=') connString)) $ do
       runMigration migrateAll
       let names = universeBi gr
           sortedNames = nubBy ((==) `on` getLoc) $ sortOn getLoc names
       case filter (isGoodSrcSpan . getLoc) sortedNames of
         n:_ -> do
           let Just fileName = srcSpanFileName_maybe (getLoc n)
           fl <- selectList [HsFileFilename ==. unpackFS fileName] []
           case fl of
             file:_ -> do
               deleteWhere [HsNameFile ==. entityKey file]
               deleteWhere [HsSourceLocFile ==. entityKey file]
               deleteWhere [HsFileFilename ==. unpackFS fileName]
             [] -> return ()
           evalStateT (mapM_ storeName sortedNames) initState
         [] -> return ()
       return (tc, gr)

storeName :: Located Name -> ParseM ()
storeName (L l n) 
  = if isGoodSrcSpan l
    then void $ do 
           Just (myLoc, file) <- insertLoc l
           defLoc <- insertLoc (nameSrcSpan n)
           let nameStr = showSDocUnsafe (ppr n)
           lift $ insert $ HsName file nameStr myLoc (fmap fst defLoc)
    else return ()

insertLoc :: SrcSpan -> ParseM (Maybe (Key HsSourceLoc, Key HsFile))
insertLoc (RealSrcSpan rsp) = do
  file <- insertFile (unpackFS (srcSpanFile rsp))
  let sloc = HsSourceLoc file (srcSpanStartLine rsp) (srcSpanStartCol rsp) 
                         (srcSpanEndLine rsp) (srcSpanEndCol rsp)
  srcLoc <- gets (Map.lookup sloc . sourceLocCache)
  sl <- case srcLoc of Nothing -> do slKey <- lift $ insert sloc
                                     modify (onSourceLocCache (Map.insert sloc slKey))
                                     return slKey
                       Just sl -> return sl
  return $ Just (sl,file)
insertLoc _ = return Nothing

insertFile :: String -> ParseM (Key HsFile)
insertFile str = do
  let fl = HsFile str
  file <- gets (Map.lookup fl . fileCache)
  case file of Just f -> return f
               Nothing -> do fileKey <- lift $ insert fl
                             modify (onFileCache (Map.insert fl fileKey))
                             return fileKey

-- typecheckPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
-- typecheckPlugin _ _ tc
--   = do dflags <- getDynFlags
--        liftIO $ putStrLn $ "typeCheckPlugin (rn): \n" ++ (showSDoc dflags $ ppr $ tcg_rn_decls tc)
--        liftIO $ putStrLn $ "typeCheckPlugin (tc): \n" ++ (showSDoc dflags $ ppr $ tcg_binds tc)
--        return tc