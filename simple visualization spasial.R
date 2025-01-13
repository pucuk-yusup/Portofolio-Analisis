#data
ecommerce_data <- data.frame(
  Store_ID = 1:5,
  Store_Name = c("Alpha Mart", "Beta Shop", "Gamma Store", "Delta Market", "Epsilon Boutique"),
  Latitude = c(-6.200000, -7.257472, -6.914744, -8.409518, -7.795580),
  Longitude = c(106.816666, 112.752090, 107.609810, 115.188919, 110.369490),
  Sales = c(10.5, 7.2, 12.3, 6.7, 8.9),
  Category = c("Electronics", "Fashion", "Groceries", "Home Appliances", "Fashion")
)

write.csv(ecommerce_data, "ecommerce_data.csv", row.names = FALSE)

##visualisasi secara spasial peta interaktif##
leaflet(data = ecommerce_data) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude, 
                   label = ~Store_Name, 
                   color = ~ifelse(Category == "Fashion", "blue", "red"),
        
                              radius = ~Sales)

##visulisasi secara spasial peta statis##
library(ggplot2)
library(sf)
# Convert data menjadi objek sf
ecommerce_sf <- st_as_sf(ecommerce_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Visualisasi
ggplot(data = ecommerce_sf) +
  geom_sf(aes(size = Sales, color = Category)) +
  theme_minimal() +
  ggtitle("Distribusi Toko E-commerce")
