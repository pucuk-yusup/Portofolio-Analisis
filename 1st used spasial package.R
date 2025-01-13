##The 1st tutorial used package raster, terra, gstat, geoR & splancs##

##popular package spasial##
library(sp)
library(spatialreg)
library(raster)
library(terra)
library(spacetime)
library(spdep)
library(gstat)
library(geoR)
library(gdalcubes)
library(splancs)

##Spatial Dataset: Example of Points and Regions##
# Create spatial points data
coordinates <- data.frame(
  id = 1:10,
  x = runif(10, -100, 100),
  y = runif(10, -50, 50),
  value = rnorm(10, mean = 5, sd = 2)
)
coordinates_sp <- SpatialPointsDataFrame(
  coords = coordinates[, c("x", "y")],
  data = coordinates,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

##Raster Dataset##
r <- raster(nrow = 100, ncol = 100, xmn = -100, xmx = 100, ymn = -50, ymx = 50)
values(r) <- runif(ncell(r), min = 0, max = 100)
writeRaster(r, "example_raster.tif", format = "GTiff", overwrite = TRUE)

##Advanced Workflow Examples##
##Raster Analysis with raster and terra##
# Resample raster
r_resampled <- resample(r, raster(nrow = 50, ncol = 50))
# Terra advanced raster analysis
terra_r <- rast("example_raster.tif")
aggregate(terra_r, fact = 2, fun = mean)

##Geostatistics with gstat and geoR##
# Kriging interpolation
kriging_model <- gstat(id = "value", formula = value ~ 1, locations = coordinates_sp)
kriging_result <- interpolate(r, kriging_model)
plot(kriging_result)

# Variogram with geoR
variogram_model <- variog(coords = as.matrix(coordinates[, c("x", "y")]), data = coordinates$value)
plot(variogram_model)

##Spatial Point Patterns with splancs##
# Kernel density estimation
kernel <- kernel2d(as.points(coordinates[, c("x", "y")]), poly = bbox(coordinates_sp), h0 = 1)
image(kernel)

