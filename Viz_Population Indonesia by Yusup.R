# –– INSTALL & LOAD LIBRARIES
install.packages("pacman")
pacman::p_load(
  terra, elevatr, sf, tidyverse,
  ggplot2, ggspatial, ggnewscale,
  scales, grid
)

# –– LOAD POPULATION DATA
pop_rast <- terra::rast("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/IDN/idn_ppp_2020_constrained.tif")

# –– LOAD INDONESIA BOUNDARY
idn_sf <- geodata::gadm("IDN", level = 0, path = tempdir()) |> st_as_sf()
idn_vect <- terra::vect(idn_sf)

# –– DOWNLOAD DEM FOR INDONESIA
dem <- elevatr::get_elev_raster(locations = idn_sf, z = 5, clip = "locations") |> terra::rast()
dem_idn <- terra::crop(dem, idn_vect) |> terra::mask(idn_vect)
dem_exagg <- dem_idn * 1.3

# –– GENERATE HILLSHADE (CINEMATIC)
slope <- terra::terrain(dem_exagg, v = "slope", unit = "radians")
aspect <- terra::terrain(dem_exagg, v = "aspect", unit = "radians")
hillshade <- terra::shade(slope, aspect, angle = 40, direction = 225)

# –– ALIGN POPULATION TO HILLSHADE GRID
pop_aligned <- terra::resample(pop_rast, hillshade, method = "bilinear")
hillshade_masked <- terra::ifel(is.na(pop_aligned), hillshade, NA)

# –– CONVERT TO DATA.FRAME FOR GGPLOT
pop_df <- as.data.frame(pop_aligned, xy = TRUE, na.rm = TRUE)
names(pop_df)[3] <- "pop"
pop_df$pop[pop_df$pop <= 0.1] <- NA

hillshade_df <- as.data.frame(hillshade_masked, xy = TRUE, na.rm = TRUE)
names(hillshade_df)[3] <- "shade"

# –– LEGEND BREAKS
brks <- c(1, 10, 100, 1000)

# –– BUILD THE MAP
p <- ggplot() +
  # 1. Hillshade background
  geom_raster(data = hillshade_df, aes(x, y, fill = shade)) +
  scale_fill_gradient(low = "grey80", high = "grey10", guide = "none") +
  
  # 2. New fill scale for population
  ggnewscale::new_scale_fill() +
  geom_raster(data = pop_df, aes(x, y, fill = pop), alpha = 0.95) +
  scale_fill_viridis_c(
    name = "Population (log)",
    option = "plasma",
    trans = "log10",
    breaks = brks,
    labels = comma,
    guide = guide_colorbar(
      barheight = unit(35, "mm"),
      barwidth = unit(3, "mm"),
      title.position = "top"
    )
  ) +
  
  # 3. Boundary
  geom_sf(data = idn_sf, fill = NA, color = "white", linewidth = 0.3) +
  
  # 4. Decorations
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true",
                                    style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", height = unit(2, "mm")) +
  
  # 5. Title & theme
  labs(
    title = "Indonesia Population Density (2020)",
    subtitle = "100m resolution · Data from WorldPop.org",
    caption = "Design: Yusup Makes Maps · Data: WorldPop & SRTM"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.01),
    plot.subtitle = element_text(size = 15, hjust = 0.01),
    plot.caption = element_text(size = 10, hjust = 0.5, color = "gray40"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

# Tampilkan peta
print(p)
