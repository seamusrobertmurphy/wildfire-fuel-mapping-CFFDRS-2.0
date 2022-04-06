# Wildfire-Mapping-CFFDRS-2.0 Prototype
Proof of Concept:
================
CabinGIS (see latest draft here: https://github.com/seamusrobertmurphy/Wildfire-Fuel-Mapping-CFFDRS-2.0/blob/main/01_Wildfire-Fuel-Mapping-CFFDRS-2.0_prototype.pdf)
24/02/2022

-   [Action](#action)
-   [1 ![](Data/CFFDRS%20Prototype.png)BC WildFire Fuel Typing and
    VRI-Layer Classification
    Algorithm](#bc-wildfire-fuel-typing-and-vri-layer-classification-algorithm)
    -   [1.1 Import Site Data](#import-site-data)
    -   [1.2 Import Topographical Data](#import-topographical-data)
    -   [1.3 Import Climate Data](#import-climate-data)
    -   [1.4 Derive CFFDRS Wildfire Weather
        Rasters](#derive-cffdrs-wildfire-weather-rasters)
-   [2 Import VRI Data](#import-vri-data)

## Action

Building on the momentum and ideas of our last meeting, the following
pipeline was attempted to produce the wildfire fuel mapping outputs
described in the NRC grant “High-Resolution Mapping”:

-   NRC Grant: <https://www.ic.gc.ca/eic/site/101.nsf/eng/00157.html>

Using the new [cffdrs
R-package](https://cran.r-project.org/web/packages/cffdrs/cffdrs.pdf)
(Wang et al. (2017); Van Wagner and Pickett (1985); Van Wagner (1987)),
we developed rasters of forest fuel moisture code and wildfire weather
indices (Table 1) that were used to fit the stand-adjusted fine-fuel
model (Wotton and Beverly (2007)) applied to vegetation rasters
classified according to 16 fuel classes of the BC forest fuel typing
algorithm ( Perrakis, Eade, and Hicks (2018)). REeult rasters were then
used to fit the Canadian Forest Fire Behaviour Prediction model from
which we drafted raster maps representing Head Fire Index (HFI) and Fire
Intensity maps (FI) (Figure 1) for the Okanagan Watershed Basin for the
day of June 30th 2021.

# 1 ![](Data/CFFDRS%20Prototype.png)BC WildFire Fuel Typing and VRI-Layer Classification Algorithm

## 1.1 Import Site Data

We applied the Okanagan Watershed boundary (FWA ID:212) as our area of
interest, which was downloaded from the BC Geographic Warehouse. We
tested potential base mapping services from [Google Cloud
API](https://cloud.google.com/maps-platform/) and the ggmap package with
a free personal account key token. Four basemaps were derived at a zoom
setting of 9 at the latitude and longitude location of (-119, 50.0).
Likely that Google Cloud services are working off EPSG 3857, though this
needs confirming. With the default ‘statellite’ basemap, we used Lobo’s
script
[(2014)](https://rstudio-pubs-static.s3.amazonaws.com/16660_7d1ab1b355344578bbacb0747fd485c8.html)
to transform the RGB raster to a matrix object and a SpatRaster before
applying different mapping styles. Note that chunk outputs require
substantial system memory and may cause crashes.
