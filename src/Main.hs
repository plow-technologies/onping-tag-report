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

import           Data.Text             (pack)

-- Database
import           Plowtech.Config
import           Plowtech.Request.Haxl
-- Internal
import           Onping.Tag.Report

main :: IO ()
main = do
  let printErrorEither = either fail  return
  mongoConf <- printErrorEither =<< readDBConf "mongoConfig.yml"
  clientConf <- printErrorEither =<< readDataClientConf "onpingDataClient.yml"
  singleWellRocConfig <- printErrorEither =<< readSingleWellRocConfig "singleWellRocConfig.yml"
  singleWellMicrologixConfig <- printErrorEither =<< readSingleWellMicrologixConfig "singleWellMicrologixConfig.yml"
  connectionPool <- buildConnectionPool mongoConf 3 256

  let plowStateStore = createHaxlStateStore $ PlowtechHaxlConf connectionPool clientConf singleWellRocConfig singleWellMicrologixConfig
  putStrLn "Welcome to Onping Tag Report Generator, Please choose an option"
  putStrLn "1. Generate Html Tag Report for a Company."
  putStrLn "2. Generate | S V Report for a Company"
  input <- getLine
  case input of
    "1" -> handleInputAndRunAction plowStateStore buildCompanyTemplate "-onping-tag-report.html"
    "2" -> handleInputAndRunAction plowStateStore buildCompanyTemplateCSV "-onping-tag-report.csv"
    _ -> print ("Not available option."::String)
  where
    handleInputAndRunAction plowStateStore action outputFile = do
                                                               putStrLn "Input a Company Name :"
                                                               companyname <- getLine
                                                               companySTemplate <- action plowStateStore (pack companyname)
                                                               putStrLn companySTemplate
                                                               writeFile (companyname ++ outputFile) companySTemplate
                                                               print ("Successfully Write to File"::String)
                                                               return ()
