#1.Muat library
library(sf)
library(ggplot2)
library(dplyr)
library(leaflet)
library(tidyr)
library(leaflet)
library(ggspatial)
library(viridis)
library(raster)

#2.dataset hipotesis
# Definisikan wilayah dengan koordinat
coords <- list(
  matrix(c(0, 0, 4, 0, 4, 4, 0, 4, 0, 0), ncol = 2, byrow = TRUE),  # Wilayah 1
  matrix(c(3, 3, 6, 3, 6, 6, 3, 6, 3, 3), ncol = 2, byrow = TRUE),  # Wilayah 2
  matrix(c(2, -1, 5, -1, 5, 2, 2, 2, 2, -1), ncol = 2, byrow = TRUE) # Wilayah 3
)

# Konversi menjadi objek sf
regions <- st_sf(
  name = c("Wilayah 1", "Wilayah 2", "Wilayah 3"),
  resources = c(100, 50, 75),
  population = c(500, 800, 600),
  geometry = st_sfc(lapply(coords, function(coord) st_polygon(list(coord))))
)

# Tampilkan hasil
print(regions)

# Plot wilayah
ggplot(regions) +
  geom_sf(aes(fill = resources)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Distribusi Sumber Daya Antarwilayah", fill = "Sumber Daya") +
  theme_minimal()

#simulasi konflik
# Tentukan tetangga antarwilayah
neighbors <- st_relate(regions, regions, pattern = "F***T****")  # Relasi topologi: bersentuhan

# Tampilkan hubungan antarwilayah
neighbors_matrix <- as.matrix(neighbors)
print(neighbors_matrix)

# Simulasi konflik sederhana
set.seed(42)

simulate_conflict <- function(regions) {
  for (i in 1:nrow(regions)) {
    for (j in 1:nrow(regions)) {
      if (i != j && neighbors_matrix[i, j]) {  # Jika wilayah bertetangga
        diff_resources <- regions$resources[i] - regions$resources[j]
        if (abs(diff_resources) > 20) {  # Jika perbedaan sumber daya signifikan
          # Probabilitas kemenangan berdasarkan populasi
          prob_i <- regions$population[i] / (regions$population[i] + regions$population[j])
          if (runif(1) < prob_i) {
            # Wilayah i menang
            regions$resources[i] <- regions$resources[i] + 10
            regions$resources[j] <- regions$resources[j] - 10
          } else {
            # Wilayah j menang
            regions$resources[j] <- regions$resources[j] + 10
            regions$resources[i] <- regions$resources[i] - 10
          }
        }
      }
    }
  }
  return(regions)
}

# Jalankan simulasi
regions_after <- simulate_conflict(regions)

# Tampilkan hasil
print(regions_after)

#3.visualisasi hasil simulasi
# Plot setelah konflik
ggplot(regions_after) +
  geom_sf(aes(fill = resources)) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Distribusi Sumber Daya Setelah Konflik", fill = "Sumber Daya") +
  theme_minimal()

# Visualisasi interaktif
leaflet(regions_after) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric("YlOrRd", resources)(resources),
    weight = 1,
    popup = ~paste0(name, "<br>Sumber Daya: ", resources, "<br>Populasi: ", population)
  )

#polusi udara#
# Membuat dataset titik sensor polusi
sensor_data <- data.frame(
  id = 1:5,
  lat = c(-6.2, -6.3, -6.25, -6.35, -6.4),
  lon = c(106.8, 106.85, 106.75, 106.7, 106.9),
  pm25 = c(45, 60, 30, 70, 55)  # Nilai PM2.5
)

# Konversi ke sf object
sensor_sf <- st_as_sf(sensor_data, coords = c("lon", "lat"), crs = 4326)

# Tampilkan data
print(sensor_sf)

