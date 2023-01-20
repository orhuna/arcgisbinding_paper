library(arcgisbinding)
library(leaflet, quietly = TRUE)
library(leaflet.esri, quietly = TRUE)
library(sf, quietly = TRUE)
library(hpiR, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(raster, quietly = TRUE)
library(reticulate, quietly = TRUE)
print(paste0("This demo uses ", R.Version()$version.string))

arc.check_product()

wd <- getwd()
feature_service <- 'https://services3.arcgis.com/oZfKvdlWHN1MwS48/arcgis/rest/services/King_County_House_Prices/FeatureServer/0'
## Define Feature Metadata
arc.fs <- arc.open(feature_service)
print(arc.fs)

## Read Data as an Arc Data Frame
kc.house.arc <- arc.select(arc.fs)
## Convert Arc Data Frame to SF Data Frame
kc.house.sf <- arc.data2sf(kc.house.arc)
head(kc.house.sf, 2)


pal <- colorQuantile("YlOrRd", kc.house.sf$price)

map.1 <- leaflet(kc.house.sf) %>%
  setView(-122.3321, 47.5063, 9.5) %>%
  addEsriBasemapLayer(esriBasemapLayers$DarkGray, autoLabels = TRUE) %>%
  addCircles(color = ~pal(kc.house.sf$price))
map.1
## Define the Raster Data Path
raster.dir <- file.path(wd, "sample_R.gdb/d2water_ProjectRaster")
## Define Raster Meta Data
arc.dist2water <- arc.open(raster.dir)
print(arc.dist2water)
## Read Raster as an Arc Data Frame
raster_arc <- arc.raster(arc.dist2water)
## Convert the Arc Raster to raster* Object
raster_R <- as.raster(raster_arc)
plot(raster_R)


census.tract.dir <- file.path(wd, "data/kc_census_blocks_22.shp")
arc.census <- arc.open(census.tract.dir)
census.data <- arc.select(arc.census)
census.data.sf <- arc.data2sf(census.data)
plot(st_geometry(census.data.sf))

## Change the Projection of the Census Tracks to that of House Price Data
census.data.sf.proj <- st_transform(census.data.sf, st_crs(kc.house.sf))

## Make the Geometry Valid for SF Geometry Operations
## In this case the task entails removing duplicate vertices
census.data.sf.proj <- st_make_valid(census.data.sf.proj)
kc.joined <- st_join(census.data.sf.proj, kc.house.sf, join = st_intersects)

## Drop Polygons with No Data
kc.joined <- kc.joined[!is.na(kc.joined$price),]

## Plot the Joined Data with GGPLOT
ggplot() + 
  geom_sf(data = kc.joined, aes(fill = price)) + 
  lims(x = c(-122.6, -121.9), y = c(47.1, 47.8))

st_crs(kc.house.sf)
crs(raster_R) <- CRS(st_crs(kc.house.sf)$wkt)
kc.w.dist <- extract(raster_R, method = 'bilinear', kc.house.sf, na.rm=TRUE)
## Visualize Histogram of Distance to Nearest Water Body for Houses
hist(kc.w.dist, main="Histogram of Distance to Nearest Water Body")

## Append the field to the KC House Dataframe
kc.house.sf["dist2Water"] <- kc.w.dist

kc.house.sf['Date'] <- as.Date(kc.house.sf$date, format = "%Y%m%dT000000")

hedonic.df <- hedCreateTrans(kc.house.sf, "id", "OBJECTID", "price", date= "Date")

hed_model <- hedModel(estimator = structure('robust', class = 'base'),
                      hed_df = hedonic.df, periodicity = 'monthly',
                      hed_spec = as.formula(log(price) ~ bedrooms+ bathrooms + 
                                              sqft_living + sqft_lot + dist2Water))

hedonic.df['Predicted'] <- exp(hed_model$fitted.values)
log.res <- log(hedonic.df$price) - hed_model$fitted.values
log.res.std <- ( log.res - mean(log.res) ) / sd(log.res)
hedonic.df['Std_Log_Residual'] <- log.res.std 

## Write the Data Frame of House Price Predictions to a new ESRI Feature Class

write.path <- file.path(file.path(wd, "data/sample_gdb.gdb/kc_predic"))

arc.write(write.path, hedonic.df, overwrite = TRUE)