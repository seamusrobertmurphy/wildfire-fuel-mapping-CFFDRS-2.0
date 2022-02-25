
# Pipeline for next-gen CFFDRS using cffdrs package

library(conflicted)
library(readr)
library(tibble)
library(sf)
library(dplyr)
library(raster)
library(terra)
library(rgdal)
library(cffdrs)
library(bcmaps)

climate_vars = read.csv("./Data/power_nasa_kelowna.csv")
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
par(mfrow = c(1, 2)) 
kcrs = crs(kelowna)
crs(fwi_outputs) = kcrs

plot(st_geometry(bc))
plot(st_geometry(kelowna), col = "lightseagreen", add = TRUE)
plot(st_geometry(kelowna), col = "lightseagreen")
plot(fwi_outputs$FFMC)
plot(kelowna, add=TRUE)


kelowna = vect(kelowna$geometry)
kelowna = rast(kelowna)

FFMC = raster(fwi_outputs$FFMC)



kelowna = vect(kelowna)
kelowna = 

rast(fwi_outputs)

crs(temp)
crs(kelowna)
plot(kelowna$geometry)
plot(temp$temp, add=TRUE)
compareGeom(temp, kelowna)



test_fwi


plot(peachland$geometry)
plot(stand$stand, add = TRUE)
plot(density$geometry, add = TRUE)
