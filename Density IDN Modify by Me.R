#package
library(tidyverse)
library(ggmap)
library(terra)
library(giscoR)
library(rayshader)

# Cek apakah file sudah ada sebelum download
worldpop_file <- "D:/Portofolio/Spasial 1/idn_ppp_2022_1km_UNadj.tif"
if (!file.exists(worldpop_file)) {
  download.file("https://data.worldpop.org/GIS/Population/Global_2021_2022_1km_UNadj/unconstrained/2022/IDN/", 
                destfile = worldpop_file, mode = "wb")
}

# Load data raster
population_1km <- terra::rast(worldpop_file)
terra::plot(population_1km)

#GGplot Custom
theme_custom <- function() {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 10, color = "grey20", vjust = -1, hjust = .5),
      legend.text = element_text(size = 8, color = "grey20"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
}

#Overvlay Data Spasial
library(leaflet)

# Buat color palette untuk leaflet
pal <- colorNumeric(
  palette = "viridis",  # Bisa diganti dengan "magma", "inferno", dll.
  domain = values(population_1km),  # Rentang nilai dari raster
  na.color = "transparent"  # Warna untuk nilai NA
)

leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addRasterImage(population_1km, colors = pal, opacity = 0.7) %>%
  addLegend(pal = pal, values = values(population_1km),
            title = "Population Density",
            position = "bottomright")

#Bivariate Map
# Ambil data elevasi dari geodata
elevation <- geodata::elevation_global(res = 10, path = tempdir())

# Definisikan batas wilayah Indonesia (Koordinat: 95E - 141E, -11S - 6N)
indonesia_extent <- ext(95, 141, -11, 6)

# Crop untuk Indonesia
elevation_id <- crop(elevation, indonesia_extent)

# Konversi ke DataFrame
elevation_df <- as.data.frame(terra::as.points(elevation_id), xy = TRUE)

# Konversi raster populasi ke dataframe dengan koordinat
populationdf <- as.data.frame(terra::as.points(population_1km), xy = TRUE)

# Pastikan kolom memiliki nama yang benar
colnames(populationdf) <- c("x", "y", "pop")  # "pop" adalah nilai populasi

# Gabungkan data dengan populasi
df <- inner_join(populationdf, elevation_df, by = c("x", "y"))

# Bivariate Color Mapping
df <- df %>%
  mutate(
    pop_cat = cut(pop, breaks = quantile(pop, probs = seq(0, 1, length.out = 4), na.rm = TRUE), include.lowest = TRUE),
    elev_cat = cut(elevation, breaks = quantile(elevation, probs = seq(0, 1, length.out = 4), na.rm = TRUE), include.lowest = TRUE)
  ) %>%
  mutate(bivariate_class = paste0(as.numeric(pop_cat), "-", as.numeric(elev_cat)))

# Plot Bivariate Map
ggplot(df, aes(x = x, y = y, color = bivariate_class)) +
  geom_point(size = 2) +
  scale_color_manual(values = biv_colors, name = "Bivariate Class", drop = FALSE) +
  theme_minimal() +
  labs(title = "Bivariate Map: Populasi & Elevasi di Indonesia")

#Visualiasasi 3D
rayshader::plot_gg(
  ggobj = p1,
  width = 5, 
  height = 5,
  scale = 300,  # Turunkan sedikit buat performa lebih baik
  solid = TRUE,
  shadow = TRUE,
  sunangle = 135
)