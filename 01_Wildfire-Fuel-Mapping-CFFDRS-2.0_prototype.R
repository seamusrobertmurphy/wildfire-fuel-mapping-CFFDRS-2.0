
# Pipeline for next-gen CFFDRS using cffdrs package

library(conflicted)
library(readr)
library(tibble)
library(sf)
library(dplyr)
library(caret)
library(raster)
library(terra)
library(rgdal)
library(cffdrs)
library(bcmaps)
library(htmlwidgets)
library(leaflet)
library(tidyverse)
library(vroom)
library(xmlparsedata)
library(rex)
library(cyclocomp)
library(lintr)
conflict_prefer("mutate", "dplyr")
# RUN STATIC LINTING OF SUBSEQUENT SCRIPT SYNTAX
lintr::lint("./01_Wildfire-Fuel-Mapping-CFFDRS-2.0_prototype.R")


## IMPORT AOI FILES
# Okanagan Watershe
aoi = sf::read_sf("./Data/hydrology/watersheds/watershed_okanagan.shp")
aoi = dplyr::rename(aoi, Okanagan_Watershed = WTRSHDGRPC)
aoi = aoi[1, "Okanagan_Watershed"]
base::plot(aoi)

# BC by Fire Districts
#aoi_bc_fire_centres = sf::read_sf("./Data/fire-centres/fire-centres-bc/DRPMFFRCNT_polygon.shp")
#aoi_bc_fire_centres = dplyr::rename(aoi_bc_fire_centres, BC_FIRE_CENTRES = MFFRCNTRNM)
#aoi_bc_fire_centres = aoi_bc_fire_centres["BC_FIRE_CENTRES"]
#base::plot(aoi_bc_fire_centres)

# Kamloops Fire Centre District
#aoi = sf::read_sf("./Data/fire-centres/fire-centres-kamloops/DRPMFFRCNT_polygon.shp")
#aoi = dplyr::rename(aoi, Kamloops_Fire_Centre = MFFRCNTRNM)
#aoi = aoi[1, "Kamloops_Fire_Centre"]
#base::plot(aoi)

#watersheds = sf::read_sf("./Data/hydrology/watersheds/watersheds_bc/watersheds_bc.shp")
#watersheds_kamloopsFC = sf::st_intersection(sf::st_make_valid(watersheds), aoi)
#plot(st_geometry(watersheds_kamloopsFC))
#plot(st_geometry(watersheds))

## IMPORT MFLNRO DISTURBANCE DATASETS
cutblocks = sf::read_sf("./Data/cutblocks/cutblocks_kamloops_fire_centre/CNS_CUT_BL_polygon.shp")
cutblocks_kamloopsFC = sf::st_intersection(sf::st_make_valid(cutblocks), aoi)
cutblocks_kamloopsFC = cutblocks_kamloopsFC %>% 
  dplyr::select[c("CUTBLOCKID", "HARVESTYR", "AREAHA", "DSTRBEDDT")] 


burns_BC = sf::read_sf("./Data/burns/burns-historical/H_FIRE_PLY_polygon.shp")
burns_kamloopsFC = sf::st_intersection(sf::st_make_valid(burns_BC), aoi)
burns_kamloopsFC = burns_kamloopsFC %>% 
  dplyr::select[c("", "", "")]


mpb = sf::read_sf("./Data/pests/beetle-mpb/FADM_MPBSA_polygon.shp")
mpb_kamloopsFC = sf::st_intersection(sf::st_make_valid(mpb), aoi)
mpb_kamloopsFC = mpb_kamloopsFC %>% 
  dplyr::select[c("", "", "")]

## FUEL TYPING ALGORITHM (Perrakis et al 2015)
# Import VRI current and historical datasets
vri_ok_2020 = sf::read_sf("./Data/vri/vri-ok-2020/VEG_R1_PLY_polygon.shp")
vri_ok_2020 = vri_ok_2020[c("BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4", "BCLCS_LV_5", "SPEC_CD_1", "SPEC_CD_2", "SPEC_PCT_1", "SPEC_PCT_2", "PROJ_HT_1", "PROJ_AGE_1", "CR_CLOSURE", "BEC_ZONE", "BEC_SZONE", "HRVSTDT", "DEAD_PCT", "LIVE_STEMS", "DEAD_STEMS", "N_LOG_DIST", "N_LOG_DATE", "LAND_CD_1", "INV_STD_CD", "NP_CODE")]
vri_ok_2020 = sf::st_intersection(sf::st_make_valid(vri_ok_2020), aoi)
glimpse(vri_ok_2020)

vri_ok_2020$NP_CODE

vri_ok_2020$BCLCS_LV_1 = as.factor(vri_ok_2020$BCLCS_LV_1)
vri_ok_2020$BCLCS_LV_2 = as.factor(vri_ok_2020$BCLCS_LV_2)
vri_ok_2020$BCLCS_LV_3 = as.factor(vri_ok_2020$BCLCS_LV_3)
vri_ok_2020$BCLCS_LV_4 = as.factor(vri_ok_2020$BCLCS_LV_4)
vri_ok_2020$BCLCS_LV_5 = as.factor(vri_ok_2020$BCLCS_LV_5)
vri_ok_2020$SPEC_CD_1 = as.factor(vri_ok_2020$SPEC_CD_1)
vri_ok_2020$SPEC_CD_2 = as.factor(vri_ok_2020$SPEC_CD_2)
vri_ok_2020$BEC_ZONE = as.factor(vri_ok_2020$BEC_ZONE)
vri_ok_2020$BEC_SZONE = as.factor(vri_ok_2020$BEC_SZONE)
vri_ok_2020$N_LOG_DIST = as.factor(vri_ok_2020$N_LOG_DIST)
vri_ok_2020$LAND_CD_1 = as.factor(vri_ok_2020$LAND_CD_1)
vri_ok_2020$INV_STD_CD = as.factor(vri_ok_2020$INV_STD_CD)
vri_ok_2020$NP_CODE = as.factor(vri_ok_2020$NP_CODE)
vri_ok_2020$HRVSTDT = as.numeric(vri_ok_2020$HRVSTDT)
vri_ok_2020$LIVE_STEMS = as.numeric(vri_ok_2020$LIVE_STEMS)
vri_ok_2020$DEAD_STEMS = as.numeric(vri_ok_2020$DEAD_STEMS)

summary.factor(vri_ok_2020$BEC_SZONE)
summary.factor(vri_ok_2020$BEC_ZONE)
summary.factor(vri_ok_2020$BCLCS_LV_1)
summary.factor(vri_ok_2020$BCLCS_LV_2)
summary.factor(vri_ok_2020$BCLCS_LV_3)
summary.factor(vri_ok_2020$BCLCS_LV_4)
summary.factor(vri_ok_2020$BCLCS_LV_5)
summary.factor(vri_ok_2020$SPEC_CD_1)
summary.factor(vri_ok_2020$N_LOG_DIST)




vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_N = case_when(
  BCLCS_LV_1 == "N" ~ "1",
  BCLCS_LV_1 == "U" ~ "1",
  BCLCS_LV_1 == "V" ~ "0")
  )


vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C1 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="SWB" & SPEC_PCT_1 >= 80 & SPEC_CD_1=="SB" & HRVSTDT>=20100000  & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="SWB" & SPEC_PCT_1 >= 80 & SPEC_CD_1=="SW" & HRVSTDT>=20100000  & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="SWB" & SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & HRVSTDT>=20130000  & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="SWB" & SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & HRVSTDT>=20130000  & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="BWBS" & SPEC_PCT_1 >= 80 & SPEC_CD_1=="SB" & HRVSTDT>=20100000  & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="BWBS" & SPEC_PCT_1 >= 80 & SPEC_CD_1=="SW" & HRVSTDT>=20100000  & BCLCS_LV_5=="SP" | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="BWBS" & SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & HRVSTDT>=20130000  & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="BWBS" & SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & HRVSTDT>=20130000  & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="BWBS" & SPEC_PCT_1 >= 20 & SPEC_CD_1=="SB" & HRVSTDT>=20140000  & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="BWBS" & SPEC_PCT_1 >= 20 & SPEC_CD_1=="SX" & HRVSTDT>=20130000  & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="BWBS" & SPEC_PCT_1 >= 20 & SPEC_CD_1=="SW" & HRVSTDT>=20130000  & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BEC_ZONE=="BWBS" & SPEC_PCT_1 >= 20 & SPEC_CD_1=="S" & HRVSTDT>=20130000  & BCLCS_LV_5=="SP" ~"1")
  )

vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C2 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & BEC_ZONE=="BWBS" &  !is.na(SPEC_CD_1) & HRVSTDT<=19950000 |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="BL" & SPEC_CD_2=="SE" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="BL" & SPEC_CD_2=="SW" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="BL" & SPEC_CD_2=="S" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SX" & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" & DEAD_PCT<=34 |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" & DEAD_PCT<=34 |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SW" & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" & DEAD_PCT<=34 |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" & DEAD_PCT<=34 |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="S" & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" & DEAD_PCT<=34 |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" & DEAD_PCT<=34 |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SX" & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SW" & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="S" & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SB" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SB" & HRVSTDT<=20140000& BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SB" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SB" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SS" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SS" & HRVSTDT<=20140000& BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SS" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 20 & SPEC_CD_1=="SS" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="BL" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="B" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="PL" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="P" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="PLI" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="BL" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="B" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="PL" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="P" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="PLI" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PL" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PL" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PL" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PL" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PL" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PLI" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PLI" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PLI" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PLI" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PLI" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PLC" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PLC" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PLC" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PLC" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PLC" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PJ" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PJ" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PJ" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PJ" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PJ" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="P" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="P" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="P" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="P" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="P" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & HRVSTDT <= 20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=4 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=4 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & HRVSTDT <= 20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=4 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=4 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & HRVSTDT <= 20140000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & HRVSTDT <= 20140000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & HRVSTDT <= 20140000 & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & HRVSTDT <= 20140000 & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & HRVSTDT <= 20140000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & HRVSTDT <= 20140000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & HRVSTDT <= 20140000 & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & HRVSTDT <= 20140000 & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SB" & HRVSTDT <= 20100000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SB" & HRVSTDT <= 20100000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SB" & HRVSTDT <= 20100000 & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SB" & HRVSTDT <= 20100000 & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SB" & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SB" & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SB" & is.na(HRVSTDT) & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SB" & is.na(HRVSTDT) & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SW" & HRVSTDT <= 20100000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SW" & HRVSTDT <= 20100000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SW" & HRVSTDT <= 20100000 & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SW" & HRVSTDT <= 20100000 & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SE" & HRVSTDT <= 20100000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SE" & HRVSTDT <= 20100000 & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SE" & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="SE" & is.na(HRVSTDT) & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PL" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="P" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PL" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="P" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT<=1995000 & BEC_ZONE=="BWBS" ~ "1" )
)



vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C3 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT <=19950000 & BEC_ZONE=="SWB" | 
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & is.na(HRVSTDT) & HRVSTDT <=19950000 & !is.na(SPEC_CD_1) & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & is.na(SPEC_CD_1) & HRVSTDT <=19950000 & BEC_ZONE=="SBS" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="ICH" & BEC_SZONE == "d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT <=19950000 & BEC_ZONE=="ESSF" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" &  !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="IDF" & BEC_SZONE == "w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="SBS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="B" & SPEC_PCT_1 >=20 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1== "BL" & SPEC_PCT_1 >=20 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="B" & SPEC_PCT_1 >=20 & SPEC_CD_2=="SE" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="B" & SPEC_PCT_1 >=20 & SPEC_CD_2=="SW" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="B" & SPEC_PCT_1 >=20 & SPEC_CD_2=="S" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="BL" & SPEC_PCT_1 >=20 & SPEC_CD_2=="SE" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="BL" & SPEC_PCT_1 >=20 & SPEC_CD_2=="SW" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="BL" & SPEC_PCT_1 >=20 & SPEC_CD_2=="S" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="BA" & SPEC_PCT_1 >=20 & SPEC_CD_2=="SE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="BA" & SPEC_PCT_1 >=20 & SPEC_CD_2=="SW" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="BA" & SPEC_PCT_1 >=20 & SPEC_CD_2=="S" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H*" & SPEC_PCT_1 >=20 & HRVSTDT<=20140000 & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H*" & SPEC_PCT_1 >=20 & is.na(HRVSTDT) & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2 == "T" & SPEC_CD_1 == "H*" & SPEC_PCT_1 >= 20 & HRVSTDT<=20140000 & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H*" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="C*" & SPEC_PCT_1 >=20 & HRVSTDT<=20140000 & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="C*" & SPEC_PCT_1 >=20 & is.na(HRVSTDT) & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="C*" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="C*" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y*" & SPEC_PCT_1 >=20 & HRVSTDT<=20140000 & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y*" & SPEC_PCT_1 >=20 & is.na(HRVSTDT) & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y*" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y*" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SX" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  DEAD_PCT <= 34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SX" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  is.na(DEAD_PCT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SX" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  DEAD_PCT <= 34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SX" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  is.na(DEAD_PCT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  DEAD_PCT <= 34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  is.na(DEAD_PCT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  DEAD_PCT <= 34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  is.na(DEAD_PCT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="S" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  DEAD_PCT <= 34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="S" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  is.na(DEAD_PCT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="S" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  DEAD_PCT <= 34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="S" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" &  is.na(DEAD_PCT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SX" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SX" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="S" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="S" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SB" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & BEC_ZONE!="BWBS" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SB" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 !="H*" & SPEC_CD_2 !="C*" & SPEC_CD_2 !="Y*" & SPEC_CD_2 !="B" & SPEC_CD_2 !="BL" & SPEC_CD_2 !="PL" & SPEC_CD_2 !="P" & SPEC_CD_2 !="PLI"  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 !="H*" & SPEC_CD_2 !="C*" & SPEC_CD_2 !="Y*" & SPEC_CD_2 !="B" & SPEC_CD_2 !="BL" & SPEC_CD_2 !="PL" & SPEC_CD_2 !="P" & SPEC_CD_2 !="PLI"  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 !="H*" & SPEC_CD_2 !="C*" & SPEC_CD_2 !="Y*" & SPEC_CD_2 !="B" & SPEC_CD_2 !="BL" & SPEC_CD_2 !="PL" & SPEC_CD_2 !="P" & SPEC_CD_2 !="PLI" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 !="H*" & SPEC_CD_2 !="C*" & SPEC_CD_2 !="Y*" & SPEC_CD_2 !="B" & SPEC_CD_2 !="BL" & SPEC_CD_2 !="PL" & SPEC_CD_2 !="P" & SPEC_CD_2 !="PLI" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="H*" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="H*" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="CW" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="CW" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="YC" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="YC" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="BL" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="BL" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="B" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="B" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="PL" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="PL" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="P" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="P" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="PLI" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="PLI" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="d*" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="d*" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="d*" & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="d*" & SPEC_CD_2!="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & SPEC_CD_2!="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & SPEC_CD_2!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & SPEC_CD_2!="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & HRVSTDT<=2013000 & PROJ_HT_1 >4 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & is.na(HRVSTDT) & PROJ_HT_1 >4 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="S*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=2013000 & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2=="B*" & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="S*" & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=2013000 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & SPEC_CD_2=="B*" & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="B*" & SPEC_CD_1!="BG" & SPEC_CD_1!="BA" & BCLCS_LV_5!="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & HRVSTDT <=201400000 & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & HRVSTDT <=201400000 & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & HRVSTDT <=201400000 & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & HRVSTDT <=201400000 & BCLCS_LV_5=="D" & PROJ_AGE_1 <15 & PROJ_HT_1 <=4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <15 & PROJ_HT_1 <=4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & HRVSTDT <=201400000 & BCLCS_LV_5=="D" & PROJ_AGE_1 <15 & PROJ_HT_1 <=4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 & PROJ_HT_1 <=4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & HRVSTDT <=201400000 & BCLCS_LV_5=="D" & PROJ_AGE_1 <15 & PROJ_HT_1 <=4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <15 & PROJ_HT_1 <=4|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT <= 20130000 & BCLCS_LV_5=="OP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT <= 20130000 & BCLCS_LV_5=="OP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & HRVSTDT <= 20100000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & HRVSTDT <=20100000 & PROJ_HT_1>=12 & PROJ_HT_1<=17 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=17 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="N" & HRVSTDT <= 19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="d*"|
    BCLCS_LV_1=='N' & HRVSTDT <= 19950000 & BEC_ZONE =="IDF" & BEC_SZONE=="m*"|
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="SBS" ~ "1" )
  )


vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C4 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="d*" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="d*" & DEAD_PCT>34 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P*" & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P*" & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="S*" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P*" & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P*" & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="B*" & LIVE_STEMS + DEAD_STEMS > 6000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="d*" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="d*" & DEAD_PCT>34 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & HRVSTDT<20100000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & HRVSTDT<20100000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="OP" |

    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1!="PY" & SPEC_CD_1=="P*" & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1!="PY" & SPEC_CD_1=="P*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1!="PY" & SPEC_CD_1=="P*" & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1!="PY" & SPEC_CD_1=="P*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="OP" ~ "1" )
  )


vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C5 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & HRVSTDT<20140000 & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & is.na(HRVSTDT) & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |

    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & HRVSTDT<20140000 & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & is.na(HRVSTDT) & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |

    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & HRVSTDT<20140000 & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & is.na(HRVSTDT) & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SW" & HRVSTDT<20140000 & BEC_ZONE=="CWH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BEC_ZONE=="CWH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SW" & HRVSTDT<20140000 & BEC_ZONE=="CWH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BEC_ZONE=="CWH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SW" & HRVSTDT<20140000 & BEC_ZONE=="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BEC_ZONE=="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SW" & HRVSTDT<20140000 & BEC_ZONE=="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BEC_ZONE=="ICH" & BCLCS_LV_5=="OP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT<20140000 & BEC_ZONE=="CWH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE=="CWH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT<20140000 & BEC_ZONE=="CWH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE=="CWH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT<20140000 & BEC_ZONE=="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE=="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT<20140000 & BEC_ZONE=="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE=="ICH" & BCLCS_LV_5=="OP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT<20140000 & BEC_ZONE=="CWH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE=="CWH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT<20140000 & BEC_ZONE=="CWH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE=="CWH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT<20140000 & BEC_ZONE=="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE=="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT<20140000 & BEC_ZONE=="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE=="ICH" & BCLCS_LV_5=="OP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SS" & HRVSTDT<20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SS" & is.na(HRVSTDT) |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & HRVSTDT<20140000 & BCLCS_LV_5!="DE" & SPEC_CD_2=="HW" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & is.na(HRVSTDT) & BCLCS_LV_5!="DE" & SPEC_CD_2=="HW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & HRVSTDT<20140000 & BCLCS_LV_5!="DE" & SPEC_CD_2=="HM" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & is.na(HRVSTDT) & BCLCS_LV_5!="DE" & SPEC_CD_2=="HM" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & HRVSTDT<20140000 & BCLCS_LV_5!="DE" & SPEC_CD_2=="CW" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & is.na(HRVSTDT) & BCLCS_LV_5!="DE" & SPEC_CD_2=="CW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & HRVSTDT<20140000 & BCLCS_LV_5!="DE" & SPEC_CD_2=="YC" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & is.na(HRVSTDT) & BCLCS_LV_5!="DE" & SPEC_CD_2=="YC" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="CWH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="MH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="CDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="S*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="S*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    

    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="B*" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & SPEC_CD_2=="B*" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="T*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="T*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="H*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="H*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="C*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="C*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="Y*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="Y*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SS"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SS"|
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="PA" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="PF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="PF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="PW" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="PW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="T*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & HRVSTDT<20140000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & HRVSTDT<20140000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & HRVSTDT<20140000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & BEC_ZONE=="CDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & BEC_ZONE=="CDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SS" & HRVSTDT<20140000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SS" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |

    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SS" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SS" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="CWH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="CDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="MH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="CDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="w*" ~ "1" )
  )
    

vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C6 = case_when(
  BCLCS_LV_1=="U" & BCLCS_LV_4=="TC" |
    is.na(BCLCS_LV_1) & BCLCS_LV_4=="TC" ~ "1" )
  )
   
vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C7 = case_when(
  BCLCS_LV_1=="U" & BCLCS_LV_4=="TC" |
    is.na(BCLCS_LV_1) & BCLCS_LV_4=="TC" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="x*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ESSF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="x*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="x*" |    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="x*" |    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="J*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="BL" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="SX" & BEC_ZONE!="BWBS" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="SX" & BEC_ZONE!="BWBS" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="SW" & BEC_ZONE!="BWBS" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="SW" & BEC_ZONE!="BWBS" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="S" & BEC_ZONE!="BWBS" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE!="BWBS" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="SE" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="SE" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="d*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="d*" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="d*" & SPEC_CD_2=="PY" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PY" & PROJ_HT_1>=4 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PY" & PROJ_HT_1>=4  & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PY" & PROJ_HT_1>=4 & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PY" & PROJ_HT_1>=4  & BCLCS_LV_5=="SP" |
  
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2!="S*" & SPEC_CD_2!="B*" & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & CR_CLOSURE<40 & SPEC_CD_2=="B*" & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="P" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="P" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & SPEC_CD_1=="J*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & SPEC_CD_1=="J*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & SPEC_CD_1=="B*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & SPEC_CD_1=="B*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" & BCLCS_LV_5!="DE" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & SPEC_CD_1=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & SPEC_CD_1=="PY" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="BL" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="SX" & HRVSTDT<20130000 & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="S" & HRVSTDT<20130000 & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="PP" |

    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="MS" |

    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="IDF" |

    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="x*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="x*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="SP" & DEAD_PCT<40 & HRVSTDT<20100000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="SP" & DEAD_PCT<40 & is.na(HRVSTDT) |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="DE" & HRVSTDT<20100000 & PROJ_HT_1>17 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="DE" & is.na(HRVSTDT) & PROJ_HT_1>17 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="OP" & HRVSTDT<20100000 & PROJ_HT_1>17 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="OP" & is.na(HRVSTDT) & PROJ_HT_1>17 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="OP" & HRVSTDT<20100000 & PROJ_HT_1<=17 & PROJ_HT_1>=12 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="OP" & is.na(HRVSTDT) & PROJ_HT_1<=17 & PROJ_HT_1>=12 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PY" & PROJ_HT_1>4 & PROJ_HT_1<12 & (LIVE_STEMS + DEAD_STEMS < 3000) & HRVSTDT<20100000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PY" & PROJ_HT_1>4 & PROJ_HT_1<12 & (LIVE_STEMS + DEAD_STEMS < 3000) & is.na(HRVSTDT) |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
  
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" ~ "1" )
  )


vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_D1D2 = case_when(
  BCLCS_LV_1=="U" & BCLCS_LV_4=="TB" |
    is.na(BCLCS_LV_1) & BCLCS_LV_4=="TB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="SL" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="SL" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="SL" & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="ST" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="ST" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="ST" & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="HE" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="HE" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="HE" & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="HF" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="HF" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="HF" & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & is.na(LAND_CD_1) & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & is.na(LAND_CD_1) & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & is.na(LAND_CD_1) & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="BY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="BM" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & LAND_CD_1=="BL" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & is.na(NP_CODE) & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & is.na(NP_CODE) & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & is.na(NP_CODE) & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & is.na(NP_CODE) & BEC_ZONE=="BAFA" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="11" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="11" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="11" & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="12" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="12" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="12" & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="13" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="13" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="13" & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & !is.na(SPEC_CD_1) & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & !is.na(SPEC_CD_1) & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & !is.na(SPEC_CD_1) & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & !is.na(SPEC_CD_1) & BEC_ZONE=="BAFA" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="BAFA" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT>19960000 & HRVSTDT<20120000 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT>19960000 & HRVSTDT<20120000 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT>19960000 & HRVSTDT<20120000 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT>19960000 & HRVSTDT<20120000 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE=="CDF" |

    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BCLCS_LV_4=="TB" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BCLCS_LV_4=="TB" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BCLCS_LV_4=="TM" & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H*" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H*" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="C*" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="C*" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y*" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y*" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="C*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="C*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |

    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SS" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SS" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & HRVSTDT<20100000 & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="N" & is.na(HRVSTDT) & BCLCS_LV_2=="L" & BCLCS_LV_3!="A" & !is.na(SPEC_CD_1) & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="N" & is.na(HRVSTDT) & is.na(BCLCS_LV_2) & BCLCS_LV_3!="A" & !is.na(SPEC_CD_1) & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="N" & is.na(HRVSTDT) & BCLCS_LV_2=="L" & BCLCS_LV_3!="A" & !is.na(SPEC_CD_1) & BEC_ZONE=="MH" |
    BCLCS_LV_1=="N" & is.na(HRVSTDT) & is.na(BCLCS_LV_2) & BCLCS_LV_3!="A" & !is.na(SPEC_CD_1) & BEC_ZONE=="MH" |
    BCLCS_LV_1=="N" & is.na(HRVSTDT) & BCLCS_LV_2=="L" & BCLCS_LV_3!="A" & !is.na(SPEC_CD_1) & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="N" & is.na(HRVSTDT) & is.na(BCLCS_LV_2) & BCLCS_LV_3!="A" & !is.na(SPEC_CD_1) & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="BAFA" |
    
    BCLCS_LV_1=="N" & HRVSTDT>19960000 & HRVSTDT<20130000 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="N" & HRVSTDT>19960000 & HRVSTDT<20130000 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="N" & HRVSTDT>19960000 & HRVSTDT<20130000 & BEC_ZONE=="ICH" ~ "1" ) 
  )
    
    
vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_M1M2 = case_when(
  BCLCS_LV_1=="U" & BCLCS_LV_4=="TM" |
    is.na(BCLCS_LV_1) & BCLCS_LV_4=="TM" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="SWB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="x*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="BA" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="S" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="C*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y*" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SX" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SX" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SX" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SX" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SX" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SX" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SX" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SX" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SX" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SX" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SX" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SX" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SX" & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SX" & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SX" & BEC_ZONE=="SWB" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SX" & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SX" & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SX" & BEC_ZONE=="SWB" & BCLCS_LV_5=="SP" |
    
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="S" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="S" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="S" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="S" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="S" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="S" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE=="BWBS" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SW" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SE" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SE" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SE" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SE" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SE" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SE" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="F*" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="F*" & BCLCS_LV_5=="DE" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1!="S" & SPEC_PCT_1<40 & HRVSTDT<20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1!="S" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="S" & SPEC_PCT_1<40 & HRVSTDT<20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="S" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SB" & SPEC_PCT_1<40 & HRVSTDT<20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SB" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & SPEC_PCT_1<40 & HRVSTDT<20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SX" & SPEC_PCT_1<40 & HRVSTDT<20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SX" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1<40 & HRVSTDT<20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="BA" & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H*" & SPEC_PCT_1>80 & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H*" & SPEC_PCT_1>80 & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="C*" & SPEC_PCT_1>80 & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="C*" & SPEC_PCT_1>80 & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y*" & SPEC_PCT_1>80 & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y*" & SPEC_PCT_1>80 & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SB" & HRVSTDT<2010000 & BCLCS_LV_5=="SP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SB" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & HRVSTDT<2010000 & BCLCS_LV_5=="SP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="SWB" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="d*" ~ "1" ) 
  )

vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_M3M4 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & SPEC_CD_2=="S*" & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 ~ "1" ) 
  )
  
    
vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_S1 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & SPEC_CD_1!="P*" & SPEC_CD_1!="S*" & SPEC_CD_1!="B*" & SPEC_CD_1!="CW" & SPEC_CD_1!="YC" & SPEC_CD_1!="H*" & SPEC_CD_1!="FD" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & SPEC_CD_1=="P*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & SPEC_CD_1=="FD" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="F*" & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="x*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PY" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PLI" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PLC" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PJ" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="P" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<40 & HRVSTDT>=20140000 & BCLCS_LV_4=="TB" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT>=20140000 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT>=20140000 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT>=20140000 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT>=20140000 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="d*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="F*" & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="x*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="SP" & DEAD_PCT<40 & HRVSTDT<=20100000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="DE" & HRVSTDT<=20100000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="OP" & HRVSTDT<=20100000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="P" & HRVSTDT<=20130000 |
    
    BCLCS_LV_1=="N" & HRVSTDT<=20140000 ~ "1" ) 
  )


    
    
    
  
        


summary.factor(vri_ok_2020$BCLCS_LV_4)











# Fire Weather Maps
climate_vars = read.csv("./Data/climate/power_nasa_kelowna.csv")
climate_vars_sf = st_as_sf(climate_vars, coords = c("LAT", "LON"))
raster_template = raster(xmn=49.25, xmx=51.25, ymn=-122.25, ymx=-116.25, res=20, crs = "EPSG:3005")

temp = climate_vars_sf["T2M"]
temp = dplyr::rename(temp, temp = T2M)
temp = rasterize(temp, raster_template, res=20)

rh = climate_vars_sf["RH2M"]
rh = dplyr::rename(rh, rh = RH2M)
rh = rasterize(rh, raster_template, res=20)

ws = climate_vars_sf["WS10M"]
ws = dplyr::rename(ws, ws = WS10M)
ws = rasterize(ws, raster_template, res=20)

prec = climate_vars_sf["PRECTOTCORR"]
prec = dplyr::rename(prec, prec = PRECTOTCORR)
prec = rasterize(prec, raster_template, res=20)

stack = stack(temp, rh, ws, prec)
names(stack)
fwiRasters = fwiRaster(stack)
plot(fwiRasters)

bc <- bc_bound()
region = regional_districts()
kelowna <- region[region$ADMIN_AREA_NAME == "Regional District of Central Okanagan", ]
kelowna = vect(kelowna$geometry)
kelowna = rast(kelowna)
plot(st_geometry(bc))
plot(st_geometry(kelowna), col = "lightseagreen", add = TRUE)

