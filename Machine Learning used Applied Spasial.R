#package#
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

#dataset
set.seed(123)
trainData <- data.frame(
  Temperature = runif(1000, min = 20, max = 35), # Suhu
  Humidity = runif(1000, min = 50, max = 100),   # Kelembapan
  Pressure = runif(1000, min = 900, max = 1100), # Tekanan udara
  WindSpeed = runif(1000, min = 0, max = 20),    # Kecepatan angin
  Rainfall = sample(c("Normal", "Extreme"), 1000, replace = TRUE, prob = c(0.7, 0.3)) # Curah hujan
)

trainData <- trainData %>%
  mutate(
    Longitude = runif(n(), min = 95, max = 141),   # Koordinat bujur (Indonesia)
    Latitude = runif(n(), min = -11, max = 6)     # Koordinat lintang (Indonesia)
  )

# Dataset untuk testData
testData <- data.frame(
  Temperature = runif(200, min = 20, max = 35), # Suhu
  Humidity = runif(200, min = 50, max = 100),   # Kelembapan
  Pressure = runif(200, min = 900, max = 1100), # Tekanan udara
  WindSpeed = runif(200, min = 0, max = 20),    # Kecepatan angin
  Rainfall = sample(c("Normal", "Extreme"), 200, replace = TRUE, prob = c(0.7, 0.3)) # Curah hujan
)

testData <- testData %>%
  mutate(
    Longitude = runif(n(), min = 95, max = 141),   # Koordinat bujur (Indonesia)
    Latitude = runif(n(), min = -11, max = 6)     # Koordinat lintang (Indonesia)
  )

head(trainData)
head(testData)

#prediction testing
testData$Prediction <- predict(model_rf, newdata = testData)

#map
world <- ne_countries(scale = "medium", returnclass = "sf")

#visualization prediction
ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +  # Peta dasar
  coord_sf(xlim = c(95, 141), ylim = c(-11, 6), expand = FALSE) +  # Fokus wilayah
  geom_point(data = testData, aes(x = Longitude, y = Latitude, color = Prediction), size = 3) +  # Plot prediksi
  scale_color_manual(values = c("Normal" = "blue", "Extreme" = "red")) +  # Warna untuk kategori
  labs(
    title = "Prediksi Curah Hujan Ekstrem Secara Spasial",
    x = "Longitude",
    y = "Latitude",
    color = "Rainfall"
  ) +
  theme_minimal()

## hitung ketidakpastian prediksi model ##
# Menghitung probabilitas prediksi
testData$ProbExtreme <- predict(model_rf, newdata = testData, type = "prob")[, "Extreme"]

# Visualisasi ketidakpastian sebagai peta
ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +
  coord_sf(xlim = c(95, 141), ylim = c(-11, 6), expand = FALSE) +
  geom_point(data = testData, aes(x = Longitude, y = Latitude, color = ProbExtreme), size = 3) +
  scale_color_gradient(low = "yellow", high = "red", name = "Prob Extreme") +
  labs(
    title = "Peta Ketidakpastian Prediksi Curah Hujan Ekstrem",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

## Moran's I ##
# Membuat matriks bobot spasial berdasarkan koordinat lokasi
library(spdep)
coords <- cbind(testData$Longitude, testData$Latitude)  # Koordinat lokasi
nb <- knn2nb(knearneigh(coords, k = 4))  # Tetangga terdekat
lw <- nb2listw(nb, style = "W")          # Bobot spasial

# Hitung autokorelasi spasial untuk residual model
residuals <- as.numeric(testData$Rainfall == "Extreme") - predict(model_rf, newdata = testData, type = "prob")[, "Extreme"]
moran_test <- moran.test(residuals, lw)
print(moran_test)

## SPATIO-TEMPORAL #3
# Menambahkan dimensi waktu (bulan)
testData$Month <- sample(1:12, nrow(testData), replace = TRUE)

# Menghitung rata-rata prediksi berdasarkan wilayah dan bulan
spatio_temporal_summary <- testData %>%
  group_by(Longitude, Latitude, Month) %>%
  summarise(MeanProbExtreme = mean(ProbExtreme))

# Visualisasi temporal dengan facet untuk bulan
ggplot(spatio_temporal_summary) +
  geom_sf(data = world, fill = "white", color = "black") +
  coord_sf(xlim = c(95, 141), ylim = c(-11, 6), expand = FALSE) +
  geom_point(aes(x = Longitude, y = Latitude, color = MeanProbExtreme), size = 3) +
  scale_color_gradient(low = "blue", high = "red", name = "Mean Prob Extreme") +
  facet_wrap(~ Month) +
  labs(
    title = "Peta Spatio-Temporal Curah Hujan Ekstrem",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

##Integrasi to QGIS & ArcGIS bila perlu##
# Install dan load library untuk ekspor GeoJSON
library(geojsonio)

# Konversi dataset ke format spatial
spatial_data <- st_as_sf(testData, coords = c("Longitude", "Latitude"), crs = 4326)

# Simpan ke GeoJSON
geojson_write(spatial_data, file = "rainfall_prediction.geojson")