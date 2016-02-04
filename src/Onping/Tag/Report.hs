{- |
Module      :  <Onping.Tag.Report>
Description :  <Executable for Onping Tag Report >
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable

<Grab a company by its name and generate reports for all its sites , locations and tags.>
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Onping.Tag.Report where

-- General

import           Data.Text                               (Text)

-- Database
import           Plowtech.Persist.Settings

import           Database.Persist
import           Plowtech.Request.Haxl                   (PlowStateStore)
import qualified Plowtech.Request.Haxl                   as Haxl

import qualified Plowtech.Request.Haxl.Company           as Haxl
import qualified Plowtech.Request.Haxl.Location          as Haxl
import qualified Plowtech.Request.Haxl.OnpingTagCombined as Haxl
import qualified Plowtech.Request.Haxl.Site              as Haxl

-- String Template
import           Text.StringTemplate

data ReportStyle = HTML | CSV

-- Output Format
-- # Onping Register Report

-- ## Company: <company name>

-- ## Site: <site name>
-- ### Location: <location name> SlaveId: <locationSlaveId>
-- #### <location name> Parameter Tags
-- <table>
-- <tr>
-- <th>Tag Name</th> , <th>Slave Parameter Id </th>, <th>Parameter Tag ID</th>
-- <tr>
-- <tag name>, <slave_parameter_id>, <parameter tag Id>


-- fetchCompanies :: PlowStateStore -> Text -> IO [Company]
fetchCompanies ::  PlowStateStore  -> Text   -> IO [Company]
fetchCompanies plowStateStore companyName' = do
   companyEntities <- Haxl.runHaxlWithPlowState plowStateStore $ Haxl.getCompaniesByName [companyName']
   return $ entityVal <$> companyEntities

fetchSites :: PlowStateStore -> Int -> IO [Site]
fetchSites plowStateStore companyId = do
  siteEntities <- Haxl.runHaxlWithPlowState  plowStateStore $ Haxl.getSitesByCompanyIdRefs [companyId]
  return $ entityVal <$> siteEntities

fetchLocations :: PlowStateStore -> Int -> IO [Location]
fetchLocations plowStateStore siteId = do
   locationEntities <- Haxl.runHaxlWithPlowState plowStateStore $ Haxl.getLocationsByAltKeys altkeys
   return $ entityVal <$> locationEntities
    where
     altkeys = Haxl.LocationLookupId Nothing [] [siteId]


fetchOnpingTagCombineds :: PlowStateStore -> Int -> IO [OnpingTagCombined]
fetchOnpingTagCombineds plowStateStore locationId = do
         otcEntities <- Haxl.runHaxlWithPlowState plowStateStore $ Haxl.getParametersByAltKeys altkeys
         return otcEntities
           where
             altkeys = Haxl.ParameterLookupId [locationId] [] []



buildOTCSTemplate :: OnpingTagCombined -> String
buildOTCSTemplate otc = render otcSTemplate
   where otcName = onpingTagCombinedDescription otc
         otcSlaveParameterId = onpingTagCombinedSlave_parameter_id otc
         otcParameterTagId = onpingTagCombinedParameter_tag_id otc
         otcPID = onpingTagCombinedPid otc
         baseTemplate = (newSTMP "<tr><td>$pid$</td><td>$tag_name$</td> <td>$slave_parameter_id$</td> <td>$parameter_tag_Id$</td></tr>") :: StringTemplate String
         otcSTemplate = setAttribute "tag_name" otcName .
                        setAttribute "slave_parameter_id" otcSlaveParameterId .
                        setAttribute "parameter_tag_Id" otcParameterTagId .
                        setAttribute "pid" otcPID $ baseTemplate

buildTableSTemplate :: [OnpingTagCombined] -> String
buildTableSTemplate otcList = render tableSTemplate
   where otcSTemplates = unlines $ buildOTCSTemplate <$> otcList
         baseTemplate = (newSTMP "<table class='table table-condensed table-striped'> \n\
                                \<tr> \n\
                                 \<th>Global Parameter Id </th> \n\
                                 \<th>Tag Name</th> \n\
                                 \<th>Slave Parameter Id </th> \n\
                                 \<th>Parameter Tag ID</th> \n\
                                 \</tr>\n\
                                 \$otcSTemplate$\
                                 \</table>") :: StringTemplate String
         tableSTemplate = setAttribute "otcSTemplate" otcSTemplates $ baseTemplate

buildLocationTemplate :: PlowStateStore -> Location -> IO String
buildLocationTemplate plowStateStore l = do
       otcList <- fetchOnpingTagCombineds plowStateStore (locationRefId l)
       let tableTemplate = buildTableSTemplate otcList
           locName = locationName l
           locSlaveId = locationSlaveId l
           baseTemplate = (newSTMP "\n### Location: $location_name$ SlaveId: $locationSlaveId$ \n\
                                   \#### $location_name$ Parameter Tags \n\
                                   \$tableTemplate$ \n") :: StringTemplate String
           locationSTemplate = setAttribute "location_name" locName .
                               setAttribute "locationSlaveId" locSlaveId .
                               setAttribute "tableTemplate" tableTemplate $ baseTemplate
       return $ render locationSTemplate

buildSiteTemplate :: PlowStateStore -> Site -> IO String
buildSiteTemplate plowStateStore s = do
     locList <- fetchLocations plowStateStore (siteSiteIdRef s)
     locationTemplates <- traverse (buildLocationTemplate plowStateStore ) locList
     let sName = siteName s
         baseTemplate = (newSTMP "\n## Site: $site_name$ \n\
                                 \$location_template$ \n") :: StringTemplate String
         siteSTemplate = setAttribute "site_name" sName .
                         setAttribute "location_template" (unlines locationTemplates) $ baseTemplate
     return $ render siteSTemplate

buildCompanyTemplate :: PlowStateStore -> Text -> IO String
buildCompanyTemplate plowStateStore cName = do
     companyList <- fetchCompanies plowStateStore cName
     case companyList of
       [] -> return "Error: There is no company records match the name given."
       _ -> do
         siteList <-fetchSites plowStateStore (companyCompanyIdRef . head $ companyList)
         siteTemplates <- traverse (buildSiteTemplate plowStateStore ) siteList
         let baseTemplate = (newSTMP "<head>\n\
                                     \<link rel='stylesheet' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css'>\n\
                                     \</head>\n\n\
                                     \# Onping Register Report \n\n\
                                     \## Company: $company_name$ \n\n\
                                     \$site_template$ \n") :: StringTemplate String
             companySTemplate = setAttribute "company_name" cName .
                                setAttribute "site_template" (unlines siteTemplates) $ baseTemplate
         return $ render companySTemplate



-- --------------------------------------------------

buildOTCSTemplateCSV :: Text -> Text -> Text -> OnpingTagCombined -> String
buildOTCSTemplateCSV cName sName locName otc = render otcSTemplate
   where otcName = onpingTagCombinedDescription otc
         otcSlaveParameterId = onpingTagCombinedSlave_parameter_id otc
         otcParameterTagId = onpingTagCombinedParameter_tag_id otc
         otcPID = onpingTagCombinedPid otc
         baseTemplate = (newSTMP "$company_name$ | $site_name$ | $location_name$ | $pid$ | $tag_name$ | $slave_parameter_id$ | $parameter_tag_Id$ \n") :: StringTemplate String
         otcSTemplate = setAttribute "tag_name" otcName .
                        setAttribute "slave_parameter_id" otcSlaveParameterId .
                        setAttribute "parameter_tag_Id" otcParameterTagId .
                        setAttribute "company_name" cName.
                        setAttribute "site_name" sName.
                        setAttribute "location_name" locName.
                        setAttribute "pid" otcPID $ baseTemplate
buildTableSTemplateCSV :: Text -> Text -> Text -> [OnpingTagCombined] -> String
buildTableSTemplateCSV cName sName lName otcList = render tableSTemplate
   where otcSTemplates = unlines $ buildOTCSTemplateCSV cName sName lName <$> otcList
         baseTemplate = (newSTMP "$otcSTemplate$ \n") :: StringTemplate String
         tableSTemplate = setAttribute "otcSTemplate" otcSTemplates $ baseTemplate

buildLocationTemplateCSV :: PlowStateStore -> Text -> Text -> Location -> IO String
buildLocationTemplateCSV plowStateStore cName sName l = do
       let locName = locationName l
       otcList <- fetchOnpingTagCombineds plowStateStore (locationRefId l)
       let tableTemplate = buildTableSTemplateCSV cName sName locName otcList
           baseTemplate = (newSTMP "$tableTemplate$ \n") :: StringTemplate String
           locationSTemplate = setAttribute "tableTemplate" tableTemplate $ baseTemplate
       return $ render locationSTemplate

buildSiteTemplateCSV :: PlowStateStore -> Text -> Site -> IO String
buildSiteTemplateCSV plowStateStore cName s = do
     let sName = siteName s
     locList <- fetchLocations plowStateStore (siteSiteIdRef s)
     locationTemplates <- traverse (buildLocationTemplateCSV plowStateStore cName sName) locList
     let
         baseTemplate = (newSTMP "$location_template$") :: StringTemplate String
         siteSTemplate = setAttribute "location_template" (unlines locationTemplates) $ baseTemplate
     return $ render siteSTemplate

buildCompanyTemplateCSV :: PlowStateStore -> Text -> IO String
buildCompanyTemplateCSV plowStateStore cName = do
     companyList <- fetchCompanies plowStateStore cName
     case companyList of
       [] -> return "Error: There is no company records match the name given."
       _ -> do
         siteList <-fetchSites plowStateStore (companyCompanyIdRef . head $ companyList)
         siteTemplates <- traverse (buildSiteTemplateCSV plowStateStore cName) siteList
         let baseTemplate = (newSTMP "Company Name | Site Name | Location Name | Global Parameter Id | Tag Name | Slave Parameter Id | Parameter Tag ID \n $site_template$ \n") :: StringTemplate String
             companySTemplate = setAttribute "company_name" cName .
                                setAttribute "site_template" (unlines siteTemplates) $ baseTemplate
         return $ render companySTemplate

