####################################################################################
###
### This script provides an overview of working with spatial data in R
### for the GLSC-LSC all hands meeting in Cleveland, OH
### 
### Created  2020-02-18 by Daniel B. Fitzgerald
####################################################################################

# set directory
#setwd("~/Courses and Training/LSC All Hands")

# install packages if neccessary
#install.packages(c("sf", "raster", "tmap", "spData"))

# attach packages
library("sf")              # functions for vector data
library("raster")          # functions for raster data
library("tmap")            # provides mapping functionality
library("spData")          # provides spatial datasets for illustration purposes

# load package data
data("coffee_data")
data("rivers")
data("us_states")

# generate file path for reading in shapefile
path.to.file <- system.file("shapes/world.gpkg", package="spData")

# load shapefile
world <- st_read(path.to.file)

# typing the variable name at the console will provide a brief overview of the object
world

# explore the class of world (both a data.frame and an sf object)
class(world)

# all the normal functionality for data frames is retained
head(world)
str(world)
colnames(world)
subset(world, name_long == "United States")

# notice that the geometry column is "sticky"
world[ , 1:4]
world[ , 1:2]

# this geometry column is what allows us to retain the spatial representation as we subset the data frame
plot(world[ , 2])
plot(world[5 , 2])

# get a feel for how sf handles complicated geometries using the US as example
str(world$geom[5])
head(st_coordinates(world[5, "geom"]), n=15)

# identify coordinate reference system
st_crs(world)

# reproject layer to new coordinate reference system
world.proj <- st_transform(world, crs = 3857)

# plot comparison graph
par(mfrow = c(1,2))
plot(st_geometry(world), main = "WGS84 (EPSG: 4326)")
plot(st_geometry(world.proj), main = "Mercator (EPSG: 3957)")

# reset graphics parameters
par(mfrow = c(1,1))

# join non spatial data frame (coffee) with spatial layer (world) note: warning OK for our purposes
world.coffee <- dplyr::left_join(world, coffee_data, by = "name_long")

# plot 2016 coffee production using base plotting functions
plot(world.coffee["coffee_production_2016"])

# Begin exploring tmap plotting 
coffee.map <- tm_shape(world.coffee) +
              tm_polygons(col = "coffee_production_2016", title = "2016 Coffee Production", palette = "YlOrRd") +
              tm_layout(legend.bg.color = "white", legend.frame = TRUE)

print(coffee.map)


# change tmap mode to interactive
tmap_mode("view")
print(coffee.map)

# return to plot mode
tmap_mode("plot")


### Bring in another vector layer to show to show spatial subsets, joins, and zonal stats with rasters

# reproject US States Data
us <- st_transform(us_states, crs=4326)

# the rivers layer has a larger spatial extent than we need
tm_shape(rivers) +tm_lines(col="blue") +
tm_shape(us) + tm_polygons(col="grey", alpha = 0.3)

# We can clip (spatial subsetting) one spatial feature by another using the normal [ , ] notation
us.rivers <- rivers[us, ]

# plot clipped layer
tm_shape(us) + tm_polygons(col="grey") + 
tm_shape(us.rivers) +tm_lines(col="blue")

# generate random points of "species" occurrence
points <- st_sample(us, size = 1000)

# view generated points
tm_shape(us) + tm_polygons(col="grey") + 
tm_shape(us.rivers) + tm_lines(col="blue") +
tm_shape(points) + tm_symbols(col="black")

# calculate number of occurrences by state using spatial intersection
us$npoints <- lengths(st_intersects(us, points))

# plot map showing numbe of records by state
tm_shape(us) + tm_polygons(col="npoints", palette = "Reds", title = "Number of Records")

# add points overlaying and demostrate transparency
tm_shape(us) + tm_polygons(col="npoints", palette = "Reds", title = "Number of Records") +
tm_shape(points) + tm_symbols(col = "grey", alpha = 0.4)

# generate a random rater layer of "land cover"
lu <- raster(xmn = st_bbox(us)[1], xmx=st_bbox(us)[3], ymn = st_bbox(us)[2], ymx=st_bbox(us)[4], resolution = 0.2)
lu <- setValues(lu, sample(1:20, ncell(lu), replace = TRUE))

plot(lu)
plot(st_geometry(us), lwd=2, add=TRUE)

# rasterize a polygon layer to speed processing (e.g., if we have numerous small catchments or watersheds)
us$GEOID <- as.numeric(us$GEOID)
us.rast <- rasterize(us, lu, field = "GEOID")

plot(us.rast)

# Cross-tabulate number of LU pixels
tab <- crosstab(us.rast, lu)
head(tab)

# convert to percentage of each LU type
lu.prct <- t(apply(tab, 1, FUN=function(x) x/sum(x)))
lu.prct <- round(lu.prct, 4)
head(lu.prct)

# reorder polygons by HUC10 code to match raster ordering 
rastord <- order(us$GEOID)

# merge landscape composition results with US polygon layer for LU 1:4
us[rastord, c("lu1", "lu2", "lu3", "lu4")] <- lu.prct[ , 1:4]

# plot percentage of Land Use 1 by state
tm_shape(us) + tm_polygons(col = "lu1", palette = "Reds", title = "Perctange of LU1")

# save results to shapefile in working directory
st_write(us, "Example_shapefile.shp")

### END OF SPATIAL DEMO
