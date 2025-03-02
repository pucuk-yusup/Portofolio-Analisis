# Load Packages
library(tidyverse)
library(ggmap)
library(terra)
library(giscoR)
library(rayshader)
library(sf)

# Get Data Indonesia
url <- "https://data.worldpop.org/GIS/Population/Global_2021_2022_1km_UNadj/constrained/2022/IDN/idn_ppp_2022_1km_UNadj_constrained.tif"
population_1km <- terra::rast(url)

#  Convert Raster to Dataframe
populationdf <- population_1km |> as.data.frame(xy = T)
names(populationdf)[3] <- "pop"

#  **Filter Data untuk Kota-Kota Besar (Jakarta, Surabaya, Bandung, Medan, Semarang)**
big_cities <- populationdf %>%
  filter(
    (x >= 106 & x <= 107 & y >= -7 & y <= -6) |  # Jakarta
      (x >= 112 & x <= 113 & y >= -8 & y <= -7) |  # Surabaya
      (x >= 107 & x <= 108 & y >= -7 & y <= -6.5) | # Bandung
      (x >= 98 & x <= 99 & y >= 2 & y <= 3) |  # Medan
      (x >= 110.3 & x <= 110.7 & y >= -7.1 & y <= -6.9) # Semarang
  )

# Theme & Color
theme_for_the_win <- function() {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 8, color = "grey20", vjust = -1, hjust = .5),
      legend.text = element_text(size = 8, color = "grey20"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines")
    )
}

cols <- hcl.colors(5, "Viridis")
pal <- colorRampPalette(cols)(512)

#️ **Plot 2D Choropleth Map**
map1 <- ggplot() +
  geom_raster(
    data = big_cities,  # Fokus ke kota besar
    aes(x = x, y = y, fill = pop)
  ) +
  scale_fill_gradientn(
    name = expression(people ~ "per 1" ~ km^2),
    colors = pal,
    na.value = NA
  ) +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = 12,
      barheight = 0.25,
      title.position = "top"
    )
  ) +
  theme_for_the_win()

print(map1)

# **Convert Raster to Matrix for 3D Visualization**
elev_matrix <- raster_to_matrix(population_1km)

# *Optimized 3D Render for Urbanization**
rayshader::plot_gg(
  ggobj = map1,
  width = max(5, ncol(big_cities) / 1000),  # Ukuran lebih kecil agar tidak error
  height = max(5, nrow(big_cities) / 1000),
  windowsize = c(600, 400),  # Ukuran lebih kecil
  scale = 150,  # Skala lebih kecil agar cepat
  solid = FALSE,  # Tidak perlu solid untuk lebih ringan
  shadow = TRUE,
  shadow_intensity = 0.3,  # Lebih ringan
  sunangle = 225,
  phi = 35,  # Sudut yang lebih optimal
  theta = 15,  
  zoom = 0.9  # Zoom lebih dekat agar lebih fokus
)

# **Save High-Quality Render**
rayshader::render_highquality(
  filename = "urbanization_3d.png",
  preview = TRUE,
  interactive = FALSE,
  light = TRUE,
  lightdirection = 225,
  lightintensity = 800,  # Kurangi intensitas cahaya untuk keseimbangan
  lightaltitude = 70,
  width = 1200,  # Sesuaikan agar tidak terlalu besar
  height = 800
)

# **Save as Video Simulation**
rayshader::render_movie("urbanization_simulation.mp4", frames = 200)
