# Instal paket reticulate (jika belum terinstal)
library(reticulate)
library(dplyr)

#data
my.datasim<-read.csv("D:/RevoU/Tugas/Adv/simulated_ride_hailing_data.csv")

library(ggplot2)

#1. Click-Through Rate (CTR) untuk Jenis Layanan
# Hitung persentase CTR per layanan
promo_conversion <- my.datasim %>%
  dplyr::group_by(Service) %>%
  dplyr::summarise(Count = dplyr::n()) %>%
  dplyr::mutate(Conversion_Rate = Count / sum(Count) * 100)

# Visualisasikan CTR
ggplot(promo_conversion, aes(x = Service, y = Conversion_Rate, fill = Service)) +
  geom_bar(stat = "identity") +
  labs(title = "Click-Through Rate (CTR) per Layanan", x = "Layanan", y = "CTR (%)") +
  theme_minimal()

#2. Conversion Rate dari Promo
# Hitung Conversion Rate promo
promo_conversion1 <- my.datasim %>%
  dplyr::group_by(Promo_Used) %>%
  dplyr::summarise(Count = dplyr::n()) %>%
  dplyr::mutate(Conversion_Rate = Count / sum(Count) * 100)

# Visualisasi Conversion Rate promo
ggplot(promo_conversion1, aes(x = Promo_Used, y = Conversion_Rate, fill = as.factor(Promo_Used))) +
  geom_bar(stat = "identity") +
  labs(title = "Conversion Rate Penggunaan Promo", x = "Promo Digunakan", y = "Conversion Rate (%)") +
  theme_minimal()

#3. Distribusi Waktu Pemesanan
# Visualisasi distribusi waktu pemesanan
ggplot(my.datasim, aes(x = Booking_Time_Seconds)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribusi Waktu Pemesanan", x = "Waktu Pemesanan (detik)", y = "Frekuensi") +
  theme_minimal()

#4. Harga Rata-Rata per Jenis Layanan
# Hitung harga rata-rata per layanan
avg_price_service <- my.datasim %>%
  group_by(Service) %>%
  summarise(Average_Price = mean(Price_IDR))

# Visualisasi harga rata-rata
ggplot(avg_price_service, aes(x = Service, y = Average_Price, fill = Service)) +
  geom_bar(stat = "identity") +
  labs(title = "Harga Rata-Rata per Jenis Layanan", x = "Layanan", y = "Harga Rata-Rata (IDR)") +
  theme_minimal()

#5.pola penggunaan waktu
# Konversi timestamp ke waktu (jam)
my.datasim$hour <- format(as.POSIXct(my.datasim$Timestamp), "%H")

# Hitung frekuensi penggunaan per jam
hourly_usage <- my.datasim %>%
  group_by(hour) %>%
  summarise(Count = n())

# Visualisasi pola waktu penggunaan
ggplot(hourly_usage, aes(x = as.numeric(hour), y = Count)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Pola Waktu Penggunaan Layanan", x = "Jam", y = "Jumlah Penggunaan") +
  theme_minimal()

#6.Kombinasi Penggunaan Promo dan Jenis Layanan
# Crosstab Promo dan Service
promo_service <- my.datasim %>%
  group_by(Service, Promo_Used) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Visualisasi kombinasi penggunaan promo dan layanan
ggplot(promo_service, aes(x = Service, y = Percentage, fill = Promo_Used)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Penggunaan Promo per Jenis Layanan", x = "Layanan", y = "Persentase (%)") +
  theme_minimal()

#7.Segmentasi Pengguna
# Segmentasi berdasarkan waktu pemesanan
data <- my.datasim %>%
  mutate(Segment = case_when(
    Booking_Time_Seconds <= 100 ~ "Cepat",
    Booking_Time_Seconds > 100 & Booking_Time_Seconds <= 200 ~ "Sedang",
    TRUE ~ "Lama"
  ))

# Visualisasi segmentasi
ggplot(data, aes(x = Segment, fill = Segment)) +
  geom_bar() +
  labs(title = "Segmentasi Pengguna Berdasarkan Waktu Pemesanan", x = "Segmentasi", y = "Jumlah Pengguna") +
  theme_minimal()