#package
library(tidyverse)
library(ggmap)
library(terra)
library(giscoR)
library(rayshader)

#Get Data ISL
url<-"https://data.worldpop.org/GIS/Population/Global_2021_2022_1km_UNadj/constrained/2022/ISL/isl_ppp_2022_1km_UNadj_constrained.tif"
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

map1 <- ggplot() +
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

print(map1)

#metode spikes
w<-ncol(population_1km)
h<-nrow(population_1km)

#modif by me
rayshader::plot_gg(
  ggobj = map1,
  width = w / 250,  # Lebih kecil, lebih cepat
  height = h / 200, 
  scale = 300,  # Turunkan skala untuk efisiensi
  solid = FALSE,  # Nonaktifkan solid untuk lebih ringan
  shadow = TRUE,
  shadow_intensity = 0.5,  # Lebih ringan
  sunangle = 225,
  phi = 45,  # Perspektif lebih nyaman
  theta = 20,  
  zoom = 0.85  # Lebih dekat
)

#modif by me
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
