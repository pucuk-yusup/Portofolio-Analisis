library(sf)
library(ggplot2)
library(dplyr)
library(classInt)  # Untuk bivariate color scales

# Gunakan dataset bawaan sf (contoh: North Carolina shapefile)
nc <- st_read(system.file("shape/nc.shp", package="sf"))

# Pilih dua variabel bivariate (misal AREA dan BIR74)
nc <- nc %>%
  mutate(
    area_cat = cut(AREA, breaks = quantile(AREA, probs = seq(0, 1, length.out = 4), na.rm = TRUE), include.lowest = TRUE),
    birth_cat = cut(BIR74, breaks = quantile(BIR74, probs = seq(0, 1, length.out = 4), na.rm = TRUE), include.lowest = TRUE)
  ) %>%
  mutate(bivariate_class = paste0(as.numeric(area_cat), "-", as.numeric(birth_cat)))

# Definisikan skema warna bivariate manual
biv_colors <- c("1-1" = "#e8e8e8", "1-2" = "#b8d6be", "1-3" = "#73ae80",
                "2-1" = "#d8b365", "2-2" = "#a6611a", "2-3" = "#5e3c99",
                "3-1" = "#f5f5f5", "3-2" = "#dfc27d", "3-3" = "#80cdc1")

# Plot Bivariate Map
ggplot(nc) +
  geom_sf(aes(fill = bivariate_class), color = "white", size = 0.2) +
  scale_fill_manual(values = biv_colors, name = "Bivariate Classes") +
  theme_minimal() +
  labs(title = "Bivariate Map")