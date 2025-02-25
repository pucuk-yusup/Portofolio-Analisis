#package
library(tidyverse)
library(ggmap)
library(terra)
library(giscoR)
library(rayshader)

#Get Data Indonesia
url<-"https://data.worldpop.org/GIS/Population/Global_2021_2022_1km_UNadj/unconstrained/2022/IDN/"
population_1km<-terra::rast(url)
terra::plot(population_1km)

#raster df
populationdf<-population_1km|>as.data.frame(xy=T)
head(populationdf)
names(populationdf)[3]<-"pop"

#theme&color
theme_for_the_win<-function(){theme_minimal()+theme(axis.line = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),legend.position = "top",legend.title = element_text(size = 8,color = "grey20",vjust = -1,hjust = .5),legend.text = element_text(size = 8,color = "grey20"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_rect(fill="white",color = NA),plot.margin = unit(c(t=0,r=0,b=0,l=0),"lines"))}

cols<-hcl.colors(5,"Viridis")
pal<-colorRampPalette(cols)(512)

#chorploth map

library(ggplot2)

p1 <- ggplot() +
  geom_raster(
    data = populationdf,
    aes(
      x = x,
      y = y,
      fill = pop
    )
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

print(p1)

#metode spikes
w<-ncol(population_1km)
h<-nrow(population_1km)

library(rayshader)

#from milos
rayshader::plot_gg(
  ggobj = p1,
  width = w / 50,
  height = h / 50,
  windowsize = c(w, h),
  scale = 750,
  solid = FALSE,
  shadow = TRUE,
  shadow_intensity = 1,
  sunangle = 225,
  phi = 85,
  theta = 0,
  zoom = 0.55
)

#moduf bt me
rayshader::plot_gg(
  ggobj = p1,
  width = w / 100, # Lebih kecil untuk kinerja yang lebih baik
  height = h / 100,
  scale = 500, # Sesuaikan skala
  solid = TRUE,
  shadow = TRUE,
  shadow_intensity = 0.7, # Lebih realistis
  sunangle = 225,
  phi = 60, # Perspektif lebih seimbang
  theta = 30, # Sedikit sudut horizontal
  zoom = 0.75
)

#moif by me
rayshader::render_highquality(
  filename = "population2.png",
  preview = TRUE,
  interactive = FALSE,
  light = TRUE,
  lightdirection = 225,
  lightintensity = 1000, # Kurangi intensitas cahaya
  lightaltitude = 80,
  width = 1920, # Resolusi lebih wajar
  height = 1080
)

#from milos
rayshader::render_highquality(
  filename = "population2.png",
  preview = TRUE,
  interactive = FALSE,
  light = TRUE,
  lightdirection = 225,
  lightintensity = 1250,
  lightaltitude = 90,
  width = w*10,
  height = h*10
)

#4. Background
main_dir<-getwd()
country_sf<-giscoR::gisco_get_countries(country = "NL",resolution = "3")
country_bbox<-sf::st_bbox(country_sf)

country_coords<-c(country_bbox[["xmin"]],
                  country_bbox[["ymin"]],  
                  country_bbox[["xmax"]],
                  country_bbox[["ymax"]])

# Masukkan API Key Anda
register_stadiamaps(key = "5f0fbba1-ce85-4c7b-9141-c73d548615ae")

# Unduh peta dengan Stadia Maps
country_layer <- ggmap::get_stadiamap(
  bbox = country_coords, # Koordinat area
  zoom = 8,              # Level zoom
  maptype = "stamen_terrain", # Tipe peta
  color = "bw",          # Hitam putih
  force = TRUE           # Paksa unduh ulang
)

# Plot peta dengan ggmap
ggmap(country_layer) +
  labs(title = "Population Density Map",
       subtitle = "Using Stadia Maps with ggmap",
       caption = "Source: Stadia Maps")

#overvlay data spasial
ggmap(country_layer) +
  geom_tile(data = populationdf, aes(x = x, y = y, fill = pop), alpha = 0.6) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Viridis"),
    name = "Population Density"
  ) +
  theme_minimal() +
  labs(title = "Population Density Overlay",
       subtitle = "Raster data overlaid on Stadia Maps",
       caption = "Source: Stadia Maps & WorldPop")

#simpan
ggsave("Population_Density_Map.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

#3D
# Buat plot dasar dengan ggmap
gg_base <- ggmap(country_layer) +
  geom_raster(data = populationdf, aes(x = x, y = y, fill = pop), alpha = 0.6) +
  scale_fill_gradientn(colors = hcl.colors(10, "Viridis"), name = "Population Density") +
  theme_void()

# Plot dalam 3D
rayshader::plot_gg(
  ggobj = gg_base,
  width = 5, 
  height = 5,
  scale = 250,
  solid = TRUE,
  shadow = TRUE,
  sunangle = 135
)

#simpan
rayshader::render_snapshot("Population_Density_3D.png")

#5. Kontur
p3 <- ggmap::ggmap(country_layer) +
  geom_contour(
    data = populationdf,
    aes(x = x, y = y, z = pop, colour = after_stat(level)),
    linewidth = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    name = expression(people ~ "per 1" ~ km^2),
    colors = pal
  ) +
  guides(colour = guide_legend(
    direction = "horizontal",
    barwidth = 12,
    barheight = 0.25
  )) +
  theme_for_the_win()

print(p3)

#other contur
p4 <- ggmap::ggmap(country_layer) +
  geom_contour(
    data = populationdf,
    aes(
      x = x, 
      y = y, 
      z = pop, 
      colour = after_stat(level)
    ),
    linewidth = 0.2,
    inherit.aes = FALSE
  ) +
  scale_color_gradientn(
    name = "People per 1~km^2",
    colors = pal
  ) +
  guides(
    color = guide_colorbar(
      direction = "horizontal",
      barwidth = 12,
      barheight = 0.25
    )
  ) +
  coord_cartesian() +
  theme_for_the_win()

print(p4)