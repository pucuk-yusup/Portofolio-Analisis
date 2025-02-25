library(raster)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(geodaData)
library(geodata)

# 1️⃣ Download data suhu & curah hujan dari WorldClim dengan geodata
temp <- worldclim_global(var = "tmax", res = 10, path = tempdir())  # Suhu maksimum
precip <- worldclim_global(var = "prec", res = 10, path = tempdir())  # Curah hujan

# 2️⃣ Subset untuk Indonesia (Koordinat 95E - 141E, -11S - 6N)
indonesia_extent <- ext(95, 141, -11, 6)
temp_id <- crop(temp, indonesia_extent)
precip_id <- crop(precip, indonesia_extent)

# Konversi ke dataframe untuk plotting
temp_df <- as.data.frame(terra::as.points(temp_id), xy = TRUE)
precip_df <- as.data.frame(terra::as.points(precip_id), xy = TRUE)


# 1️⃣ Cek apakah ada NA di kolom lon & lat
sum(is.na(temp_df$lon))  # Cek NA di lon
sum(is.na(temp_df$lat))  # Cek NA di lat
sum(is.na(precip_df$lon))  # Cek NA di lon
sum(is.na(precip_df$lat))  # Cek NA di lat

# 2️⃣ Hapus baris yang mengandung NA sebelum join
temp_df <- temp_df %>% drop_na(lon, lat)
precip_df <- precip_df %>% drop_na(lon, lat)

# 3️⃣ Pastikan kolom lon & lat unik
temp_df <- temp_df %>% distinct(lon, lat, .keep_all = TRUE)
precip_df <- precip_df %>% distinct(lon, lat, .keep_all = TRUE)

# 4️⃣ Lakukan join ulang setelah pembersihan
df <- inner_join(temp_df, precip_df, by = c("lon", "lat"))

# 3️⃣ Klasifikasi bivariate
# Buat ulang kategori bivariate dengan memastikan nilai numerik benar
df <- df %>%
  mutate(
    temp_cat = as.numeric(cut(temp, breaks = quantile(temp, probs = seq(0, 1, length.out = 4), na.rm = TRUE), include.lowest = TRUE)),
    precip_cat = as.numeric(cut(precip, breaks = quantile(precip, probs = seq(0, 1, length.out = 4), na.rm = TRUE), include.lowest = TRUE))
  ) %>%
  mutate(bivariate_class = paste0(temp_cat, "-", precip_cat))

# Cek lagi kategori bivariate setelah diperbaiki
unique(df$bivariate_class)

# Skema warna bivariate manual
biv_colors <- c("1-1" = "#e8e8e8", "1-2" = "#b8d6be", "1-3" = "#73ae80",
                "2-1" = "#d8b365", "2-2" = "#a6611a", "2-3" = "#5e3c99",
                "3-1" = "#f5f5f5", "3-2" = "#dfc27d", "3-3" = "#80cdc1")

# 4️⃣ Plot Bivariate Map (Tanpa Shapefile)
ggplot(df, aes(x = lon, y = lat, color = bivariate_class)) +
  geom_point(size = 2) +
  scale_color_manual(values = biv_colors, name = "Bivariate Class", drop = FALSE) +
  theme_minimal() +
  labs(title = "Bivariate Map: Suhu vs Curah Hujan di Indonesia (WorldClim)",
       x = "Longitude", y = "Latitude")
