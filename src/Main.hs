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
import           Control.Applicative    ((<$>))
import           Data.Text              (pack)
import qualified Data.Traversable       as T
-- Database
import           Persist.Mongo.Settings
-- Internal
import           Onping.Tag.Report

main :: IO ()
main = do
  mongoConf <- readDBConf "config.yml"
  putStrLn "Welcome to Onping Tag Report Generator, Please choose an option"
  putStrLn "1. Generate Tag Report for a Company."
  input <- getLine
  case input of
    "1" -> do
           putStrLn "Input a Company Name :"
           companyname <- getLine
           ecompanySTemplate <- T.sequence $ (\conf -> buildCompanyTemplate conf (pack companyname)) <$> mongoConf
           case ecompanySTemplate of
             Left e -> print e
             Right companySTemplate -> do
                                    putStrLn companySTemplate
                                    writeFile "onpingTagReport.MD" companySTemplate
                                    print ("Successfully Write to File: onpingTagReport.MD"::String)
                                    return ()
    _ -> print ("Not available option."::String)
