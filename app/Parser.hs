{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

{-|
Module      : Cli.Main
Description : Cli main interface module
Copyright   : 
License     : GPL-2
Maintainer  : 
Stability   : experimental
Portability : POSIX

-}
module Parser ( getProgOpts ) where

import           Data.Budget
import           Types
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Time
import           Data.Char              ( isSpace )
import           Data.String
import qualified Control.Monad          as CM
import           Database.MongoDB

import qualified Options.Applicative    as OA

import           Options.Applicative    ( (<>), idm )

class Parseable a where
    parse :: OA.Parser a

instance Parseable MainProg where
    parse = MainProg <$> parse <*> parse

instance Parseable MainOpts where
    parse = MainOpts <$> parseDBOpt

instance Parseable Prog where
    parse = OA.hsubparser $
        cmd' "budget"
             "Commands to interface with the budget"
             (BudgetProg <$> parse)

class CommandGroup a where
    cmds :: [OA.Mod OA.CommandFields a]

    parseCmds :: OA.Parser a
    parseCmds = OA.hsubparser (mconcat cmds)

instance Parseable BudgetCommand where
    parse = parseCmds

instance CommandGroup BudgetCommand where
    cmds = [ cmd' "bal" "print the current balance of the budget" $
               BudgetBalance <$> parseBudgetName
           , cmd' "new" "create new budget" $ NewBudget <$> parseBudgetName
           , cmd' "stat" "check status of the budget" $ BudgetStatus <$> parseBudgetName
           ]

instance Parseable Amount where
    parse = OA.argument OA.auto $
        OA.help "A dollar amount as a floating point number"
            <> OA.metavar "AMNT"

instance Parseable Day where
    parse = OA.argument getDay $
        OA.help "A day in the format mm/dd/yy"
            <> OA.metavar "DAY"
      where
        -- use our parser to parse the date
        getDay = OA.str >>= parseDate

getProgOpts :: (MonadIO m) => m MainProg
getProgOpts = liftIO $ OA.customExecParser prefs opts
  where
    opts = OA.info (OA.helper <*> parse)
                   (OA.fullDesc
                        <> OA.header "Yet Another Budgeting Program")
    prefs = OA.prefs $ OA.showHelpOnError

cmd' n h ops = OA.command n $ OA.info ops (OA.fullDesc <> OA.progDesc h)

parseDBOpt = fmap fromString . OA.strOption $
    OA.value "yab"
        <> OA.short 'd'
        <> OA.long "database"
        <> OA.metavar "DATABASE"
        <> OA.help "The database to use"

parseString :: (IsString a) => String -> String -> OA.Parser a
parseString meta h = fmap fromString . OA.strArgument $
    OA.help h <> OA.metavar meta

parseBudgetName = parseString "BUDGET-NAME" "The name of the budget"

dayFormats = [ "%" ++ u ++ "m" ++ s ++ "%" ++ u ++ "d" ++ s ++ "%" ++ y
             | u <- [ "", "_", "0", "-" ] 
             , y <- [ "y", "Y" ] 
             , s <- [ "/", "-" ] ]

parseDate :: (CM.MonadPlus m) => String -> m Day
parseDate s = case parseTimes s of
    [] -> CM.mzero
    (x : xs) -> return x
  where
    parseTimes s = mconcat $ parseFormat <$> dayFormats <*> pure s
    parseFormat f s = [ t
                      | (t, r) <- readSTime True defaultTimeLocale f s 
                      , all isSpace r ]
