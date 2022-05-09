
library(curl)
library(tibble)
library(dplyr)
library(sf)
library(raster)
library(terra)
library(stars)
library(rgdal)
library(gstat)
library(cffdrs)
library(nasapower)
library(elevatr)
library(leaflet)
library(forcats)
library(units)
library(gstat)
library(rgeos)

## Import AOI
#aoi_bc = sf::read_sf("./Data/fire-centres/fire-centres-bc/DRPMFFRCNT_polygon.shp")
#aoi = dplyr::rename(aoi, Okanagan_Watershed = WTRSHDGRPC)
#aoi = aoi[1, "Okanagan_Watershed"]

# Import VRI
vri_ok_2020 = read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/vri/vri_bc_2020_cffdrs.shp")
vri_ok_2020 = dplyr::rename(vri_ok_2020, BCLCS_LV_1 = BCLCS_LEVE)
vri_ok_2020 = dplyr::rename(vri_ok_2020, BCLCS_LV_2 = BCLCS_LE_1)
vri_ok_2020 = dplyr::rename(vri_ok_2020, BCLCS_LV_3 = BCLCS_LE_2)
vri_ok_2020 = dplyr::rename(vri_ok_2020, BCLCS_LV_4 = BCLCS_LE_3)
vri_ok_2020 = dplyr::rename(vri_ok_2020, BCLCS_LV_5 = BCLCS_LE_4)
vri_ok_2020 = dplyr::rename(vri_ok_2020, SPEC_CD_1 = SPECIES__1)
vri_ok_2020 = dplyr::rename(vri_ok_2020, SPEC_CD_2 = SPECIES_CD)
vri_ok_2020 = dplyr::rename(vri_ok_2020, SPEC_PCT_1 = SPECIES_PC)
vri_ok_2020 = dplyr::rename(vri_ok_2020, SPEC_PCT_2 = SPECIES__2)
vri_ok_2020 = dplyr::rename(vri_ok_2020, PROJ_HT_1 = PROJ_HEI_1)
vri_ok_2020 = dplyr::rename(vri_ok_2020, PROJ_AGE_1 = PROJ_AGE_1)
vri_ok_2020 = dplyr::rename(vri_ok_2020, CR_CLOSURE = CROWN_CLOS)
vri_ok_2020 = dplyr::rename(vri_ok_2020, BEC_ZONE = BEC_ZONE_C)
vri_ok_2020 = dplyr::rename(vri_ok_2020, BEC_SZONE = BEC_SUBZON)
vri_ok_2020 = dplyr::rename(vri_ok_2020, HRVSTDT = HARVEST_DA)
vri_ok_2020 = dplyr::rename(vri_ok_2020, DEAD_PCT = STAND_PERC)
vri_ok_2020 = dplyr::rename(vri_ok_2020, LIVE_STEMS = VRI_LIVE_S)
vri_ok_2020 = dplyr::rename(vri_ok_2020, DEAD_STEMS = VRI_DEAD_S)
vri_ok_2020 = dplyr::rename(vri_ok_2020, N_LOG_DIST = EARLIEST_N)
vri_ok_2020 = dplyr::rename(vri_ok_2020, N_LOG_DATE = EARLIEST_1)
vri_ok_2020 = dplyr::rename(vri_ok_2020, LAND_CD_1 = LAND_COVER)
vri_ok_2020 = dplyr::rename(vri_ok_2020, INV_STD_CD = INVENTORY_)
vri_ok_2020 = dplyr::rename(vri_ok_2020, NP_CODE = NON_PRODUC)
vri_ok_2020 = dplyr::rename(vri_ok_2020, COMPARTMNT = COMPARTMEN)

vri_ok_2020 = vri_ok_2020[c("FEATURE_ID", "BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4", "BCLCS_LV_5", 
                            "SPEC_CD_1", "SPEC_CD_2", "SPEC_PCT_1", "SPEC_PCT_2", "PROJ_HT_1", "PROJ_AGE_1", 
                            "CR_CLOSURE", "BEC_ZONE", "BEC_SZONE", "HRVSTDT", "DEAD_PCT", "LIVE_STEMS", "DEAD_STEMS", 
                            "N_LOG_DIST", "N_LOG_DATE", "LAND_CD_1", "INV_STD_CD", "NP_CODE", "COMPARTMNT")]

#vri_ok_2020 = sf::st_intersection(sf::st_make_valid(vri_ok_2020), aoi)
#plot(st_geometry(vri_ok_2020))
#glimpse(vri_ok_2020)

#vri_ok_2020$BCLCS_LV_1 = as.factor(vri_ok_2020$BCLCS_LV_1)
#vri_ok_2020$BCLCS_LV_2 = as.factor(vri_ok_2020$BCLCS_LV_2)
#vri_ok_2020$BCLCS_LV_3 = as.factor(vri_ok_2020$BCLCS_LV_3)
#vri_ok_2020$BCLCS_LV_4 = as.factor(vri_ok_2020$BCLCS_LV_4)
#vri_ok_2020$BCLCS_LV_5 = as.factor(vri_ok_2020$BCLCS_LV_5)
#vri_ok_2020$SPEC_CD_1 = as.factor(vri_ok_2020$SPEC_CD_1)
#vri_ok_2020$SPEC_CD_2 = as.factor(vri_ok_2020$SPEC_CD_2)
#vri_ok_2020$BEC_ZONE = as.factor(vri_ok_2020$BEC_ZONE)
#vri_ok_2020$BEC_SZONE = as.factor(vri_ok_2020$BEC_SZONE)
#vri_ok_2020$N_LOG_DIST = as.factor(vri_ok_2020$N_LOG_DIST)
#vri_ok_2020$LAND_CD_1 = as.factor(vri_ok_2020$LAND_CD_1)
#vri_ok_2020$INV_STD_CD = as.factor(vri_ok_2020$INV_STD_CD)
#vri_ok_2020$NP_CODE = as.factor(vri_ok_2020$NP_CODE)
#vri_ok_2020$HRVSTDT = as.Date(vri_ok_2020$HRVSTDT)
#vri_ok_2020$LIVE_STEMS = as.numeric(vri_ok_2020$LIVE_STEMS)
#vri_ok_2020$DEAD_STEMS = as.numeric(vri_ok_2020$DEAD_STEMS)

## Generate Panel 1 indicator: Fuel Type using MFLNRO algo (Perrakis et al 2015)
vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_N = case_when(
  BCLCS_LV_1 == "N" & HRVSTDT<=19950101 & (BEC_ZONE=="CMA" | BEC_ZONE=="IMA") | 
    BCLCS_LV_1 == "N" & is.na(HRVSTDT) & (BEC_ZONE=="CMA" | BEC_ZONE=="IMA") | 
    BCLCS_LV_1 == "N" & is.na(HRVSTDT) & (BCLCS_LV_2=="L" | is.na(BCLCS_LV_2)) & BCLCS_LV_3=="A" |
    BCLCS_LV_1 == "N" & is.na(HRVSTDT) & (BCLCS_LV_2=="L" | is.na(BCLCS_LV_2)) & is.na(SPEC_CD_1) |
    BCLCS_LV_1 == "N" & is.na(HRVSTDT) & BCLCS_LV_2!="L" & BCLCS_LV_2!="W" & !is.na(BCLCS_LV_2) |
    BCLCS_LV_2=="W" |
    is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & (NP_CODE=="35" | NP_CODE=="42") |
    is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & is.na(NP_CODE) & (BEC_ZONE=="CMA" & BEC_ZONE=="IMA") | 
    is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE!="11" & NP_CODE!="12" & NP_CODE!="13" & NP_CODE!="60" & NP_CODE!="62" & NP_CODE!="63" |
    is.na(HRVSTDT) & is.na(SPEC_CD_1) & (INV_STD_CD=="V" | INV_STD_CD=="I") & (LAND_CD_1=="LA" | LAND_CD_1=="RE" | LAND_CD_1=="RI" | LAND_CD_1=="OC") |
    is.na(HRVSTDT) & is.na(SPEC_CD_1) & (INV_STD_CD=="V" | INV_STD_CD=="I") & (LAND_CD_1=="SL" | LAND_CD_1=="ST" | LAND_CD_1=="HE" | LAND_CD_1=="HF" | is.na(LAND_CD_1)) & (BEC_ZONE=="CMA" | BEC_ZONE=="IMA") |
    is.na(HRVSTDT) & is.na(SPEC_CD_1) & (INV_STD_CD=="V" | INV_STD_CD=="I") & (LAND_CD_1=="SI" | LAND_CD_1=="GL" | LAND_CD_1=="PN" | LAND_CD_1=="RO" | LAND_CD_1=="BR" | LAND_CD_1=="TA" | LAND_CD_1=="BI" | LAND_CD_1=="MZ" | LAND_CD_1=="LB" | LAND_CD_1=="EL" | LAND_CD_1=="GL" | LAND_CD_1=="RS" | LAND_CD_1=="ES" | LAND_CD_1=="LS" | LAND_CD_1=="RM" | LAND_CD_1=="BE" | LAND_CD_1=="BU" | LAND_CD_1=="RZ" | LAND_CD_1=="MU" | LAND_CD_1=="CU" | LAND_CD_1=="MN" | LAND_CD_1=="GP" | LAND_CD_1=="TZ" | LAND_CD_1=="RN" | LAND_CD_1=="MI" | LAND_CD_1=="OT" | LAND_CD_1=="LA" | LAND_CD_1=="RE" | LAND_CD_1=="RI") |
    BCLCS_LV_1 == "U" ~ "1" ) 
)

summary.factor(vri_ok_2020$BCLCS_LV_1)
summary.factor(vri_ok_2020$BCLCS_LV_2)
summary.factor(vri_ok_2020$BCLCS_LV_5)
summary.factor(vri_ok_2020$BEC_ZONE)
summary.factor(vri_ok_2020$SPEC_PCT_1)
summary.factor(vri_ok_2020$SPEC_CD_1)
summary.factor(vri_ok_2020$HRVSTDT)
summary.factor(vri_ok_2020$fuel_N)

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
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="PA" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="PLI" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="BL" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="B" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="PL" |
    SPEC_PCT_1 <= 20 & SPEC_CD_1=="SE" & HRVSTDT<=20140000 & BCLCS_LV_5=="DE" & SPEC_CD_2=="PA" |
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
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PA" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PA" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PA" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE <2015000 & DEAD_PCT >50 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PA" & HRVSTDT >= 20130000 & BCLCS_LV_5=="OP" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 & DEAD_PCT >25 |
    SPEC_PCT_1 >= 80 & SPEC_CD_1=="PA" & HRVSTDT >= 20130000 & BCLCS_LV_5=="DE" & N_LOG_DIST=="IBM" & N_LOG_DATE >2015000 & DEAD_PCT <50 |
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
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PA" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE<=20150000 & DEAD_PCT>=50 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PL" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PA" & HRVSTDT<=20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    SPEC_PCT_1 >=80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1 >=12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & DEAD_PCT<50 & DEAD_PCT>25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT<=1995000 & BEC_ZONE=="BWBS" ~ "1" ) 
)



vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C3 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT <=19950000 & BEC_ZONE=="SWB" | 
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & is.na(HRVSTDT) & HRVSTDT <=19950000 & !is.na(SPEC_CD_1) & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & is.na(SPEC_CD_1) & HRVSTDT <=19950000 & BEC_ZONE=="SBS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="ICH" & BEC_SZONE == "dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="ICH" & BEC_SZONE == "dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="ICH" & BEC_SZONE == "dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="ICH" & BEC_SZONE == "dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="ICH" & BEC_SZONE == "dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="ICH" & BEC_SZONE == "xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="ICH" & BEC_SZONE == "xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="ICH" & BEC_SZONE == "xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="ICH" & BEC_SZONE == "xk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" & !is.na(SPEC_CD_1) & HRVSTDT <=19950000 & BEC_ZONE=="ESSF" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" &  !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="IDF" & BEC_SZONE == "mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" &  !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="IDF" & BEC_SZONE == "mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" &  !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="IDF" & BEC_SZONE == "mw" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" &  !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="IDF" & BEC_SZONE == "w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_5=="N" &  !is.na(SPEC_CD_1) & HRVSTDT<=1995000 & BEC_ZONE=="IDF" & BEC_SZONE == "v*" |
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
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H" & SPEC_PCT_1 >=20 & HRVSTDT<=20140000 & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H" & SPEC_PCT_1 >=20 & is.na(HRVSTDT) & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2 == "T" & SPEC_CD_1 == "H" & SPEC_PCT_1 >= 20 & HRVSTDT<=20140000 & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="CW" & SPEC_PCT_1 >=20 & HRVSTDT<=20140000 & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="CW" & SPEC_PCT_1 >=20 & is.na(HRVSTDT) & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="CW" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="CW" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y" & SPEC_PCT_1 >=20 & HRVSTDT<=20140000 & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y" & SPEC_PCT_1 >=20 & is.na(HRVSTDT) & PROJ_HT_1 >=15 & PROJ_AGE_1 < 60 & BCLCS_LV_5 !="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & PROJ_HT_1 >=4 & PROJ_HT_1 <=15 & BCLCS_LV_5=="DE" |
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
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 !="H" & SPEC_CD_2 !="CW" & SPEC_CD_2 !="Y" & SPEC_CD_2 !="B" & SPEC_CD_2 !="BL" & SPEC_CD_2 !="PL" & SPEC_CD_2 !="PA" & SPEC_CD_2 !="PLI"  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 !="H" & SPEC_CD_2 !="CW" & SPEC_CD_2 !="Y" & SPEC_CD_2 !="B" & SPEC_CD_2 !="BL" & SPEC_CD_2 !="PL" & SPEC_CD_2 !="PA" & SPEC_CD_2 !="PLI"  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 !="H" & SPEC_CD_2 !="CW" & SPEC_CD_2 !="Y" & SPEC_CD_2 !="B" & SPEC_CD_2 !="BL" & SPEC_CD_2 !="PL" & SPEC_CD_2 !="PA" & SPEC_CD_2 !="PLI" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 !="H" & SPEC_CD_2 !="CW" & SPEC_CD_2 !="Y" & SPEC_CD_2 !="B" & SPEC_CD_2 !="BL" & SPEC_CD_2 !="PL" & SPEC_CD_2 !="PA" & SPEC_CD_2 !="PLI" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="H" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="H" & BCLCS_LV_5=="DE" |
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
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="PA" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="PA" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & SPEC_CD_2 =="PLI" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1>=20 & is.na(HRVSTDT) & SPEC_CD_2 =="PLI" & BCLCS_LV_5!="DE" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dc" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dc" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dcp" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dcp" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dcw" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dcw" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dk" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dk" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dm" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dm" & SPEC_CD_2!="PY" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dc" & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dc" & SPEC_CD_2!="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dcp" & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dcp" & SPEC_CD_2!="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dcw" & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dcw" & SPEC_CD_2!="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dk" & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dk" & SPEC_CD_2!="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dm" & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="dm" & SPEC_CD_2!="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xc" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xc" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xcp" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xcp" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xh" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xh" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xk" & SPEC_CD_2!="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xk" & SPEC_CD_2!="PY" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xc" & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xc" & SPEC_CD_2!="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xcp" & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xcp" & SPEC_CD_2!="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xh" & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xh" & SPEC_CD_2!="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xk" & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_SZONE=="xk" & SPEC_CD_2!="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & SPEC_CD_2!="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & SPEC_CD_2!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & HRVSTDT<=20140000 & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>=20 & is.na(HRVSTDT) & CR_CLOSURE >55 & PROJ_HT_1 >=4 & PROJ_HT_1 <=12 & SPEC_CD_2!="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & HRVSTDT<=20130101 & PROJ_HT_1 >4 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & is.na(HRVSTDT) & PROJ_HT_1 >4 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") && LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE >40  & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=4 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & CR_CLOSURE <40  & BEC_ZONE!="IDF" & BEC_ZONE!="PP" & BEC_ZONE!="BG" & BEC_ZONE!="SBPS" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="ICH" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST!="IBM" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="DE" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE < 20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & N_LOG_DIST=="IBM" & N_LOG_DATE >=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT < 25 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130101 & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1 >=12 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & CR_CLOSURE <40 & BEC_ZONE!="BG" & BEC_ZONE!="PP" & BEC_ZONE!="IDF" & BEC_ZONE!="MS" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" & BEC_ZONE!="MH"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & LIVE_STEMS + DEAD_STEMS < 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130101 & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1<=12 & PROJ_HT_1 >=4 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(LIVE_STEMS) + is.na(DEAD_STEMS) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="B" | SPEC_CD_1=="BL") & SPEC_CD_1!="BG" & SPEC_CD_1!="BA" & BCLCS_LV_5!="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & HRVSTDT <=20140101 & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & HRVSTDT <=20140101 & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & HRVSTDT <=20140101 & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & HRVSTDT <=20140101 & BCLCS_LV_5=="D" & PROJ_AGE_1 <15 & PROJ_HT_1 <=4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <15 & PROJ_HT_1 <=4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & HRVSTDT <=20140101 & BCLCS_LV_5=="D" & PROJ_AGE_1 <15 & PROJ_HT_1 <=4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <60 & PROJ_HT_1 <=4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & HRVSTDT <=20140101 & BCLCS_LV_5=="D" & PROJ_AGE_1 <15 & PROJ_HT_1 <=4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & is.na(HRVSTDT) & BCLCS_LV_5=="D" & PROJ_AGE_1 <15 & PROJ_HT_1 <=4|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT <= 20130101 & BCLCS_LV_5=="OP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT <= 20130101 & BCLCS_LV_5=="OP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & HRVSTDT <= 20100101 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SE" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") && HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="xk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT <=20140000 & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_SZONE!="xk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & DEAD_PCT<34 & PROJ_HT_1>4 & PROJ_HT_1<12 & CR_CLOSURE > 55 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
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
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST!="IBM" & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE <= 20150000 & DEAD_PCT < 50 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & CR_CLOSURE >40 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT <=20130000 & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=12 & N_LOG_DIST=="IBM" & N_LOG_DATE >= 20150000 & DEAD_PCT < 25 & is.na(CR_CLOSURE) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT <=20130000 & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE <40 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT <=20130000 & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & LIVE_STEMS>=6000 & LIVE_STEMS<=8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=12 & PROJ_HT_1<=4 & is.na(LIVE_STEMS) & is.na(DEAD_STEMS) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="N" & HRVSTDT <= 19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="dc"|
    BCLCS_LV_1=="N" & HRVSTDT <= 19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="dcp"|
    BCLCS_LV_1=="N" & HRVSTDT <= 19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="dcw"|
    BCLCS_LV_1=="N" & HRVSTDT <= 19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="dk"|
    BCLCS_LV_1=="N" & HRVSTDT <= 19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="dm"|
    
    BCLCS_LV_1=="N" & HRVSTDT <= 19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="xc"|
    BCLCS_LV_1=="N" & HRVSTDT <= 19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="xcp"|
    BCLCS_LV_1=="N" & HRVSTDT <= 19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="xh"|
    BCLCS_LV_1=="N" & HRVSTDT <= 19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="xk"|
    
    BCLCS_LV_1=='N' & HRVSTDT <= 19950000 & BEC_ZONE =="IDF" & BEC_SZONE=="mh"|
    BCLCS_LV_1=='N' & HRVSTDT <= 19950000 & BEC_ZONE =="IDF" & BEC_SZONE=="mk"|
    BCLCS_LV_1=='N' & HRVSTDT <= 19950000 & BEC_ZONE =="IDF" & BEC_SZONE=="mw"|
    
    BCLCS_LV_1=='N' & HRVSTDT <= 19950000 & BEC_ZONE =="IDF" & BEC_SZONE=="w"|
    BCLCS_LV_1=='N' & HRVSTDT <= 19950000 & BEC_ZONE =="IDF" & BEC_SZONE=="v"|
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="SBS" ~ "1" )
)

vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C4 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="dc" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="dc" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="dcp" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="dcp" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="dcw" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="dcw" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="dk" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="dk" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="dm" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="dm" & DEAD_PCT>34 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="xc" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="xc" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="xcp" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="xcp" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="xh" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="xh" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="xk" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>20 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>=55 & BEC_SZONE=="xk" & DEAD_PCT>34 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="S" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="S" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="SE" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="SE" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="SW" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="SW" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="SX" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="SX" & LIVE_STEMS + DEAD_STEMS > 6000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="S" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="S" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="SE" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="SE" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="SW" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="SW" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="SX" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="Sx" & LIVE_STEMS + DEAD_STEMS > 6000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="B" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="B" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="BA" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="BA" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="BL" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="DE" & SPEC_CD_2=="BL" & LIVE_STEMS + DEAD_STEMS > 6000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="B" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="B" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="BA" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="BA" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="BL" & LIVE_STEMS + DEAD_STEMS > 6000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & BCLCS_LV_5=="OP" & SPEC_CD_2=="BL" & LIVE_STEMS + DEAD_STEMS > 6000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="dc" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="dc" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="dcp" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="dcp" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="dcw" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="dcw" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="dk" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="dk" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="dm" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="dm" & DEAD_PCT>34 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="xc" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="xc" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="xcp" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="xcp" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="xh" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="xh" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="xk" & DEAD_PCT>34 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & CR_CLOSURE>55 & BEC_SZONE=="xk" & DEAD_PCT>34 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & HRVSTDT<20100000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & HRVSTDT<20100000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="OP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1!="PY" & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1!="PY" & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1!="PY" & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI") & HRVSTDT<20130000 & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1!="PY" & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & PROJ_HT_1<=12 & LIVE_STEMS + DEAD_STEMS > 8000 & BCLCS_LV_5=="OP" ~ "1" )
)


vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C5 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="mh" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="mk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="mw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="mw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="w*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & HRVSTDT<20140000 & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & is.na(HRVSTDT) & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & HRVSTDT<20140000 & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & is.na(HRVSTDT) & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & HRVSTDT<20140000 & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & is.na(HRVSTDT) & PROJ_HT_1>15 & PROJ_AGE_1>60 & BCLCS_LV_5=="DE" |
    
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
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="CWH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="MH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE>55 & PROJ_HT_1>=12 & BEC_ZONE=="CDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX" ) & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI"& (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI"& (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") &HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI"& (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="T*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="T*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="H" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="H" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="CW" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="CW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="Y" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="Y" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="SS"|
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SS"|
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="PA" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="PF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="PF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140000 & SPEC_CD_1=="PW" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="PW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="T*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & HRVSTDT<20140000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & HRVSTDT<20140000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & HRVSTDT<20140000 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=15 & PROJ_AGE_1>=99 |
    
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
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="CWH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="CDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="MH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="CDF" |
    
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
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1>=12 & CR_CLOSURE<40 & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="mh" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="mk" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="mw" |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="v*" ~ "1" )
)


vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C6 = case_when(
  BCLCS_LV_1=="U" & BCLCS_LV_4=="TC" |
    is.na(BCLCS_LV_1) & BCLCS_LV_4=="TC" ~ "1" )
)

vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_C7 = case_when(
  BCLCS_LV_1=="U" & BCLCS_LV_4=="TC" |
    is.na(BCLCS_LV_1) & BCLCS_LV_4=="TC" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="xk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="ESSF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="xk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="xc" |    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="xcp" |    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="xh" |    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="CDF" & BEC_SZONE=="xk" |    
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT<19950000 & BEC_ZONE=="IDF" & BEC_SZONE=="xk" |
    
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
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="dm" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="xk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & CR_CLOSURE>=26 & CR_CLOSURE<=55 & BEC_SZONE=="xk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="dm" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="xk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_SZONE=="xk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1>=12 & CR_CLOSURE>=55 & BEC_ZONE=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="dc" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="dc" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="dcp" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="dcp" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="dcw" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="dcw" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="dk" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="dk" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="dm" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="dm" & SPEC_CD_2=="PY" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="xc" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="xc" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="xcp" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="xcp" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="xh" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="xh" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="xk" & SPEC_CD_2=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & PROJ_HT_1<12 & PROJ_HT_1>=4 & CR_CLOSURE>=55 & BEC_SZONE=="xk" & SPEC_CD_2=="PY" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PY" & PROJ_HT_1>=4 & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PY" & PROJ_HT_1>=4  & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PY" & PROJ_HT_1>=4 & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PY" & PROJ_HT_1>=4  & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="SBPS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & SPEC_CD_2!="S" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="SX" & SPEC_CD_2!="B" & SPEC_CD_2!="BA" & SPEC_CD_2!="BL" & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & CR_CLOSURE<40 & (SPEC_CD_2=="B" | SPEC_CD_2=="BA" | SPEC_CD_2=="BL") & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PL" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLI" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PLC" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PJ" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT<20130000 & SPEC_CD_1=="PA" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & is.na(HRVSTDT) & SPEC_CD_1=="PA" & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & SPEC_CD_1=="J*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & SPEC_CD_1=="J*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & (SPEC_CD_1=="B" | SPEC_CD_1=="BA" | SPEC_CD_1=="BL") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & (SPEC_CD_1=="B" | SPEC_CD_1=="BA" | SPEC_CD_1=="BL") |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE!="ICH" & BEC_SZONE!="w*" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE!="ICH" & BEC_SZONE!="w*" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE!="ICH" & BEC_SZONE!="v*" & BCLCS_LV_5!="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BEC_ZONE!="ICH" & BEC_SZONE!="v*" & BCLCS_LV_5!="DE" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & HRVSTDT<20140000 & SPEC_CD_1=="PY" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<80 & SPEC_PCT_1>65 & is.na(HRVSTDT) & SPEC_CD_1=="PY" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="BL" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="SX" & HRVSTDT<20130000 & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="S" & HRVSTDT<20130000 & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="PP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="MS" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE>26 & CR_CLOSURE<55 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & is.na(CR_CLOSURE) & BEC_ZONE=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="dm" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="xk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>12 & BEC_ZONE=="ICH" & BEC_SZONE=="xk" |
    
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
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1>12 & CR_CLOSURE<40 & BEC_ZONE=="BG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="w*" | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="ICH" & BEC_SZONE!="v*" ~ "1" )
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
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT>19960000 & HRVSTDT<20120000 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw")  |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT>19960000 & HRVSTDT<20120000 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(SPEC_CD_1) & HRVSTDT>19960000 & HRVSTDT<20120000 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Cw" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1<4 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1<4 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE=="CDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BCLCS_LV_4=="TB" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BCLCS_LV_4=="TB" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & BCLCS_LV_4=="TM" & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="CW" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="CW" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="CW" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="CW" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SS" & HRVSTDT<20140000 & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SS" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & HRVSTDT<20100000 & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & CR_CLOSURE<26 & SPEC_PCT_1>80 & PROJ_HT_1>=4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & SPEC_PCT_1>80 & PROJ_HT_1<4 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="CDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & HRVSTDT<20130000 & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & is.na(HRVSTDT) & SPEC_PCT_1>80 & BCLCS_LV_5=="SP" & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
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
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & !is.na(HRVSTDT) & !is.na(SPEC_CD_1) & HRVSTDT<=19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="xk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="BA" & SPEC_CD_2!="SE" & SPEC_CD_2!="SW" & SPEC_CD_2!="S" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    
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
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140101 & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140101 & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140101 & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="S" & BEC_ZONE=="SWB" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140101 & SPEC_CD_1=="SB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140101 & SPEC_CD_1=="SW" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140101 & SPEC_CD_1=="SE" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SE" & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140101 & SPEC_CD_1=="SE" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SE" & BCLCS_LV_5=="OP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & HRVSTDT<20140101 & SPEC_CD_1=="SE" & BCLCS_LV_5=="SP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>65 & SPEC_PCT_1<80 & is.na(HRVSTDT) & SPEC_CD_1=="SE" & BCLCS_LV_5=="SP" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=65 & SPEC_PCT_1<=80 & HRVSTDT<20140101 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=65 & SPEC_PCT_1<=80 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & BCLCS_LV_5=="DE" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=65 & SPEC_PCT_1<=80 & HRVSTDT<20140101 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PJ") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=65 & SPEC_PCT_1<=80 & is.na(HRVSTDT) & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PJ") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=65 & SPEC_PCT_1<=80 & HRVSTDT<20140101 & (SPEC_CD_1=="S" | SPEC_CD_1=="SE" | SPEC_CD_1=="SW" | SPEC_CD_1=="SX") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>=65 & SPEC_PCT_1<=80 & is.na(HRVSTDT) & (SPEC_CD_1=="S" | SPEC_CD_1=="SE" | SPEC_CD_1=="SW" | SPEC_CD_1=="SX") |
    
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<65 & HRVSTDT<20140101 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI" | SPEC_CD_1=="B" | SPEC_CD_1=="BA" | SPEC_CD_1=="BL" | SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY" | SPEC_CD_1=="S" | SPEC_CD_1=="SE" | SPEC_CD_1=="SW" | SPEC_CD_1=="SX" | SPEC_CD_1=="H") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<65 & is.na(HRVSTDT) & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI" | SPEC_CD_1=="B" | SPEC_CD_1=="BA" | SPEC_CD_1=="BL" | SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY" | SPEC_CD_1=="S" | SPEC_CD_1=="SE" | SPEC_CD_1=="SW" | SPEC_CD_1=="SX" | SPEC_CD_1=="H") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1!="S" & SPEC_PCT_1<40 & HRVSTDT<20140101 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1!="S" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="S" & SPEC_PCT_1<40 & HRVSTDT<20140101 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="S" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SB" & SPEC_PCT_1<40 & HRVSTDT<20140101 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SB" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & SPEC_PCT_1<40 & HRVSTDT<20140101 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SX" & SPEC_PCT_1<40 & HRVSTDT<20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SX" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1<40 & HRVSTDT<20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SE" & SPEC_PCT_1<40 & is.na(HRVSTDT) |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="BA" & SPEC_PCT_1>80 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H" & SPEC_PCT_1>80 & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="H" & SPEC_PCT_1>80 & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="CW" & SPEC_PCT_1>80 & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="CW" & SPEC_PCT_1>80 & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y" & SPEC_PCT_1>80 & HRVSTDT<20140000 & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="Y" & SPEC_PCT_1>80 & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1>15 & PROJ_AGE_1>=60 & PROJ_AGE_1<=99 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SB" & HRVSTDT<2010000 & BCLCS_LV_5=="SP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SB" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & HRVSTDT<2010000 & BCLCS_LV_5=="SP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="SW" & is.na(HRVSTDT) & BCLCS_LV_5=="SP" & BEC_ZONE!="BWBS" & BEC_ZONE!="SWB" |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="SWB" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="dc" | 
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="dcp" | 
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="dcw" | 
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="dk" | 
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="dm" | 
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="xc" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="xh" |
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & BEC_ZONE=="CWH" & BEC_SZONE=="xk" ~ "1" ) 
)

vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_M3M4 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="OP" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & (SPEC_CD_2=="S" | SPEC_CD_2=="SE" | SPEC_CD_2=="SW" | SPEC_CD_2=="SX") & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & BCLCS_LV_5=="DE" & DEAD_PCT>50 |
    
    
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
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & CR_CLOSURE>40 & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & PROJ_HT_1>12 & N_LOG_DIST=="IBM" & N_LOG_DATE>=20150000 & is.na(CR_CLOSURE) & DEAD_PCT>50 ~ "1" ) 
)


vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_S1 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & SPEC_CD_1!="PA" & SPEC_CD_1!="PL" & SPEC_CD_1!="PLC" & SPEC_CD_1!="PLI" & SPEC_CD_1!="PY" & SPEC_CD_1!="S" & SPEC_CD_1!="SE" & SPEC_CD_1!="SW" & SPEC_CD_1!="SX" & SPEC_CD_1!="B" & SPEC_CD_1!="BA" & SPEC_CD_1!="BL" & SPEC_CD_1!="CW" & SPEC_CD_1!="YC" & SPEC_CD_1!="H" & SPEC_CD_1!="FD" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & (SPEC_CD_1=="PA" | SPEC_CD_1=="PL" | SPEC_CD_1=="PLC" | SPEC_CD_1=="PLI" | SPEC_CD_1=="PY") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & SPEC_CD_1=="FD" & BEC_ZONE!="CWH" & BEC_ZONE!="ICH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="xk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PY" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PL" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PLI" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PLC" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PJ" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_CD_1=="PA" & SPEC_PCT_1>80 & HRVSTDT>=20130000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1<40 & HRVSTDT>=20140000 & BCLCS_LV_4=="TB" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="IDF" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="dc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="dcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="dcw" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="dk" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="dm" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="xc" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="xcp" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="xh" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="xk" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="SP" & DEAD_PCT<40 & HRVSTDT<=20100000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="DE" & HRVSTDT<=20100000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="OP" & HRVSTDT<=20100000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<=20130000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<=20130000 |
    
    BCLCS_LV_1=="N" & HRVSTDT<=20140000 ~ "1" ) 
)


vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_S2 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & (SPEC_CD_1=="S" | SPEC_CD_1=="SE" | SPEC_CD_1=="SW" | SPEC_CD_1=="SX") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & (SPEC_CD_1=="B" | SPEC_CD_1=="BA" | SPEC_CD_1=="BL") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT>=20140000 & (SPEC_CD_1=="S" | SPEC_CD_1=="SE" | SPEC_CD_1=="SW" | SPEC_CD_1=="SX") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT>=20140000 & (SPEC_CD_1=="S" | SPEC_CD_1=="SE" | SPEC_CD_1=="SW" | SPEC_CD_1=="SX") |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT>=20130000 & SPEC_CD_1=="SX" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT>=20130000 & SPEC_CD_1=="S" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT>=20100000 & SPEC_CD_1=="SB" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT>=20100000 & SPEC_CD_1=="SW" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & HRVSTDT>=20100000 & SPEC_CD_1=="SE" ~ "1" ) 
)


vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_S3 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & SPEC_CD_1=="FD" & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & SPEC_CD_1=="FD" & BEC_ZONE=="ICH" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & SPEC_CD_1=="CW" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & SPEC_CD_1=="YC" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT>=20130000 & SPEC_CD_1=="H" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & HRVSTDT>=20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & HRVSTDT>=20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & HRVSTDT>=20140000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="CDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="H" & HRVSTDT>=20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="CW" & HRVSTDT>=20140000 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="Y" & HRVSTDT>=20140000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SS" & HRVSTDT>=20140000 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="CWH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="MH" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & (BEC_ZONE=="ICH" & BEC_SZONE=="mh") & (BEC_ZONE=="ICH" & BEC_SZONE=="mk") & (BEC_ZONE=="ICH" & BEC_SZONE=="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="w*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="ICH" & BEC_SZONE=="v*" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT>=20140000 & BEC_ZONE=="CDF" ~ "1" ) 
)

vri_ok_2020 = vri_ok_2020 %>% dplyr::mutate(fuel_O1 = case_when(
  BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="SL" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="SL" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="SL" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="SL" & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="ST" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="ST" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="ST" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="ST" & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="HE" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="HE" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="HE" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="HE" & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="HF" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="HF" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="HF" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="HF" & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & is.na(LAND_CD_1) & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & is.na(LAND_CD_1) & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & is.na(LAND_CD_1) & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & is.na(LAND_CD_1) & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD!="F" & LAND_CD_1=="HG" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & is.na(NP_CODE) & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & is.na(NP_CODE) & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & is.na(NP_CODE) & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & is.na(NP_CODE) & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="60" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="62" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="63" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="11" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="11" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="11" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="11" & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="12" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="12" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="12" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="12" & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="13" & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="13" & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="13" & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & is.na(SPEC_CD_1) & INV_STD_CD=="F" & NP_CODE=="13" & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & !is.na(SPEC_CD_1) & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & !is.na(SPEC_CD_1) & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & !is.na(SPEC_CD_1) & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & is.na(HRVSTDT) & !is.na(SPEC_CD_1) & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT<20120000 & HRVSTDT>19960000 & !is.na(SPEC_CD_1) & BEC_ZONE=="PP" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT<20120000 & HRVSTDT>19960000 & !is.na(SPEC_CD_1) & BEC_ZONE=="MS" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT<20120000 & HRVSTDT>19960000 & !is.na(SPEC_CD_1) & BEC_ZONE=="BG" |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="N" & HRVSTDT<20120000 & HRVSTDT>19960000 & !is.na(SPEC_CD_1) & BEC_ZONE=="IDF" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & SPEC_PCT_1>80 & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1<4 | 
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLI" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1<4 | 
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PLC" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1<4 | 
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PJ" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1<4 | 
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1<4 | 
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PA" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1<4 | 
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="J*" |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & HRVSTDT<20130000 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="SX" & is.na(HRVSTDT) & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & HRVSTDT<20130000 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="S" & is.na(HRVSTDT) & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & PROJ_HT_1<4 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1>=4 & CR_CLOSURE<26 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & HRVSTDT<20140000 & PROJ_HT_1<4 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & (SPEC_CD_1=="FD" | SPEC_CD_1=="FDC" | SPEC_CD_1=="FDI") & is.na(HRVSTDT) & PROJ_HT_1<4 & BEC_ZONE!="CWH" & BEC_ZONE!="CDF" & BEC_ZONE!="MH" & (BEC_ZONE!="ICH" & BEC_SZONE!="mh") & (BEC_ZONE!="ICH" & BEC_SZONE!="mk") & (BEC_ZONE!="ICH" & BEC_SZONE!="mw") |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="SP" & DEAD_PCT>=40 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="DE" & HRVSTDT<=20100000 & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="DE" & is.na(HRVSTDT) & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="OP" & HRVSTDT<=20100000 & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PY" & BCLCS_LV_5=="OP" & is.na(HRVSTDT) & PROJ_HT_1<4 |
    
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="DE" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & HRVSTDT<20130000 & BCLCS_LV_5=="OP" & PROJ_HT_1<4 |
    BCLCS_LV_1=="V" & BCLCS_LV_2=="T" & SPEC_PCT_1>80 & SPEC_CD_1=="PL" & is.na(HRVSTDT) & BCLCS_LV_5=="OP" & PROJ_HT_1<4 |
    
    BCLCS_LV_1=="N" & is.na(HRVSTDT) & BCLCS_LV_2=="L" & BCLCS_LV_3!="A" & !is.na(SPEC_CD_1) & (BEC_ZONE=="PP" | BEC_ZONE=="MS" | BEC_ZONE=="BG" | BEC_ZONE=="IDF") |
    
    BCLCS_LV_1=="N" & HRVSTDT<19950000 & (BEC_ZONE=="PP" | BEC_ZONE=="BG") |
    
    BCLCS_LV_1=="N" & HRVSTDT<20130000 & HRVSTDT>19960000 & (BEC_ZONE=="PP" | BEC_ZONE=="MS" | BEC_ZONE=="BG" | BEC_ZONE=="IDF") ~ "1" ) 
)

vri_ok_2020 = vri_ok_2020 %>% 
  dplyr::mutate(fueltype = case_when(
    fuel_N=="1" ~ "N",
    fuel_C1=="1" ~ "C1",
    fuel_C2=="1" ~ "C2",
    fuel_C3=="1" ~ "C3",
    fuel_C4=="1" ~ "C4",
    fuel_C5=="1" ~ "C5",
    fuel_C6=="1" ~ "C6",
    fuel_C7=="1" ~ "C7",
    fuel_D1D2=="1" ~ "D1/2",
    fuel_M1M2=="1" ~ "M1/2",
    fuel_M3M4=="1" ~ "M3/4",
    fuel_S1=="1" ~ "S1",
    fuel_S2=="1" ~ "S2",
    fuel_S3=="1" ~ "S3",
    fuel_O1=="1" ~ "O1a/b" )
  )


#vri_ok_2020$fuel_N = forcats::fct_explicit_na(vri_ok_2020$fuel_N, na_level = "1")
vri_ok_2020$fueltype = forcats::fct_explicit_na(vri_ok_2020$fueltype, na_level = "N")
summary.factor(vri_ok_2020$fueltype)
st_write(vri_ok_2020, "./app/cabin_fueltype_internal.shp")



#cabin_cffdrs_ext = full_join(as_tibble(vri_ok_2020), as_tibble(fire_danger_rating_20220508), by = "geometry")
#cabin_cffdrs_ext = st_as_sf(cabin_cffdrs_ext)
#summary(fire_danger_rating_20220508$DNGR_RT)
#summary(cabin_cffdrs_ext$DNGR_RT)

#cabin_cffdrs_ext = st_join(vri_ok_2020, fire_danger_rating_20220508, largest = T)
#summary(fire_danger_rating_20220508$DNGR_RT)
#summary(cabin_cffdrs_ext$DNGR_RT)

#cabin_cffdrs_ext = st_join(fire_danger_rating_20220508, vri_ok_2020, largest = T)
#summary(fire_danger_rating_20220508$DNGR_RT)
#summary(cabin_cffdrs_ext$DNGR_RT)

master_sf_interp = read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/wildfire/cffdrs_layers_bc4.shp")
master_sf_interp = master_sf_interp[c("fueltype", "dngr_rt", "mffrznnm", "mffrcntrnm")]
summary.factor(master_sf_interp$fueltype)
summary.factor(master_sf_interp$dngr_rt)
summary.factor(master_sf_interp$mffrcntrnm)
summary.factor(master_sf_interp$mffrznd)
summary.factor(master_sf_interp$hdqrtrsctn)
str(master_sf_interp)
master_sf_interp = read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/wildfire/cffdrs_layers_bc5.shp")



