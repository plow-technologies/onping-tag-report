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
import           Control.Applicative                     ((<$>))
import           Data.Text                               (Text)
import           Data.Traversable                        (traverse)

-- Database
import           Plowtech.Persist.Settings

import           Database.Persist
import           Plowtech.Request.Haxl                   (PlowStateStore)
import qualified Plowtech.Request.Haxl                   as Haxl
import qualified Plowtech.Request.Haxl                   as Haxl
import qualified Plowtech.Request.Haxl.Company           as Haxl
import qualified Plowtech.Request.Haxl.OnpingTagCombined as Haxl

-- String Template
import           Text.StringTemplate

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
fetchCompanies plowStateStore companyName = do
   companyEntities <- Haxl.runHaxlWithPlowState plowStateStore $ Haxl.getCompaniesByName [companyName]
   return $ entityVal <$> companyEntities

-- fetchSites :: MongoDBConf -> Int -> IO [Site]
-- fetchSites mongoConf companyId = do
--   siteEntities <- runDBConf mongoConf $ selectList [SiteCompanyIdRef ==. companyId] []
--   return $ entityVal <$> siteEntities

-- fetchLocations :: MongoDBConf -> Int -> IO [Location]
-- fetchLocations mongoConf siteId = do
--   locationEntities <- runDBConf mongoConf $ selectList [LocationSiteIdRef ==. siteId] []
--   return $ entityVal <$> locationEntities

-- fetchOnpingTagCombineds :: MongoDBConf -> Int -> IO [OnpingTagCombined]
-- fetchOnpingTagCombineds mongoConf locationId = do
--         otcEntities <- runDBConf mongoConf $ selectList [OnpingTagCombinedLocation_id ==. (Just locationId)] []
--         return $ entityVal <$> otcEntities

-- buildOTCSTemplate :: OnpingTagCombined -> String
-- buildOTCSTemplate otc = render otcSTemplate
--   where otcName = onpingTagCombinedDescription otc
--         otcSlaveParameterId = onpingTagCombinedSlave_parameter_id otc
--         otcParameterTagId = onpingTagCombinedParameter_tag_id otc
--         baseTemplate = (newSTMP "<tr><td>$tag_name$</td> <td>$slave_parameter_id$</td> <td>$parameter_tag_Id$</td></tr>") :: StringTemplate String
--         otcSTemplate = setAttribute "tag_name" otcName .
--                        setAttribute "slave_parameter_id" otcSlaveParameterId .
--                        setAttribute "parameter_tag_Id" otcParameterTagId $ baseTemplate

-- buildTableSTemplate :: [OnpingTagCombined] -> String
-- buildTableSTemplate otcList = render tableSTemplate
--   where otcSTemplates = unlines $ buildOTCSTemplate <$> otcList
--         baseTemplate = (newSTMP "<table class='table table-condensed table-striped'> \n\
--                                 \<tr> \n\
--                                 \<th>Tag Name</th> \n\
--                                 \<th>Slave Parameter Id </th> \n\
--                                 \<th>Parameter Tag ID</th> \n\
--                                 \</tr>\n\
--                                 \$otcSTemplate$\
--                                 \</table>") :: StringTemplate String
--         tableSTemplate = setAttribute "otcSTemplate" otcSTemplates $ baseTemplate

-- buildLocationTemplate :: MongoDBConf -> Location -> IO String
-- buildLocationTemplate mongoConf l = do
--        otcList <- fetchOnpingTagCombineds mongoConf (locationRefId l)
--        let tableTemplate = buildTableSTemplate otcList
--            locName = locationName l
--            locSlaveId = locationSlaveId l
--            baseTemplate = (newSTMP "\n### Location: $location_name$ SlaveId: $locationSlaveId$ \n\
--                                    \#### $location_name$ Parameter Tags \n\
--                                    \$tableTemplate$ \n") :: StringTemplate String
--            locationSTemplate = setAttribute "location_name" locName .
--                                setAttribute "locationSlaveId" locSlaveId .
--                                setAttribute "tableTemplate" tableTemplate $ baseTemplate
--        return $ render locationSTemplate

-- buildSiteTemplate :: MongoDBConf -> Site -> IO String
-- buildSiteTemplate mongoConf s = do
--      locList <- fetchLocations mongoConf (siteSiteIdRef s)
--      locationTemplates <- traverse (\loc -> buildLocationTemplate mongoConf loc) locList
--      let sName = siteName s
--          baseTemplate = (newSTMP "\n## Site: $site_name$ \n\
--                                  \$location_template$ \n") :: StringTemplate String
--          siteSTemplate = setAttribute "site_name" sName .
--                          setAttribute "location_template" (unlines locationTemplates) $ baseTemplate
--      return $ render siteSTemplate

-- buildCompanyTemplate :: MongoDBConf -> Text -> IO String
-- buildCompanyTemplate mongoConf cName = do
--      companyList <- fetchCompanies mongoConf cName
--      case companyList of
--        [] -> return "Error: There is no company records match the name given."
--        _ -> do
--          siteList <-fetchSites mongoConf (companyCompanyIdRef . head $ companyList)
--          siteTemplates <- traverse (\site' -> buildSiteTemplate mongoConf site') siteList
--          let baseTemplate = (newSTMP "<head>\n\
--                                      \<link rel='stylesheet' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css'>\n\
--                                      \</head>\n\n\
--                                      \# Onping Register Report \n\n\
--                                      \## Company: $company_name$ \n\n\
--                                      \$site_template$ \n") :: StringTemplate String
--              companySTemplate = setAttribute "company_name" cName .
--                                 setAttribute "site_template" (unlines siteTemplates) $ baseTemplate
--          return $ render companySTemplate

