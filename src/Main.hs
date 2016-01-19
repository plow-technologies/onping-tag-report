{- |
Module      :  <Main.hs>
Description :  <Executable for Onping Tag Report >
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable

<Reads configs and pass it to the report generating function>
-}

{-# LANGUAGE OverloadedStrings #-}

--General
import           Control.Applicative   ((<$>))
import           Data.Text             (pack)
import qualified Data.Traversable      as T
-- Database
import           Plowtech.Config
import           Plowtech.Request.Haxl
-- Internal
import           Onping.Tag.Report

main :: IO ()
main = do
  (Right mongoConf) <- readDBConf "mongoDBConfig.yml"
  (Right clientConf) <- readDataClientConf "onpingDataClient.yml"
  (Right singleWellRocConfig) <- readSingleWellRocConfig "singleWellRocConfig.yml"
  (Right singleWellMicrologixConfig) <- readSingleWellMicrologixConfig "singleWellMicrologixConfig.yml"
  connectionPool <- buildConnectionPool mongoConf 3 256
  let plowStateStore = createHaxlStateStore $ PlowtechHaxlConf connectionPool clientConf singleWellRocConfig singleWellMicrologixConfig
  putStrLn "Welcome to Onping Tag Report Generator, Please choose an option"
  putStrLn "1. Generate Tag Report for a Company."
  input <- getLine
  case input of
    "1" -> do
           putStrLn "Input a Company Name :"
           companyname <- getLine
           companySTemplate <- buildCompanyTemplate plowStateStore (pack companyname)
           putStrLn companySTemplate
           writeFile "onpingTagReport.md" companySTemplate
           print ("Successfully Write to File: onpingTagReport.md"::String)
           return ()
    _ -> print ("Not available option."::String)
