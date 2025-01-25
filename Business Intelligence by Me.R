#data#
library(readxl)
data<-read.csv("D:/Portofolio/Amazon Sale Report/Amazon Sale Report.csv")
# Cek missing values
colSums(is.na(data))
#cek distribusi
boxplot(data$Amount,col = "green")
boxplot(data$ship.postal.code,col="red")
# Mengisi missing value Amount dg median
data$Amount[is.na(data$Amount)] <- median(data$Amount, na.rm = TRUE)
#mengisi missing value ship postal code dg modus
library(dplyr)
mode_value <- data %>% count(ship.postal.code) %>% filter(n == max(n)) %>% pull(ship.postal.code)
data$ship.postal.code[is.na(data$ship.postal.code)] <- mode_value
#Cek Total Missing Values di Seluruh Dataset
sum(is.na(data))
#Cek Missing Values per Kolom
colSums(is.na(data))
#Visualisasi to see distribusi MV
library(VIM)
aggr(data, col=c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data", "Pattern"))

#analisis deskriptif#
#statistik deskriptif
summary(data)
#tabel distribusi kategori
table(data$Category)
#next visualisasi
library(dplyr)
data %>%
  group_by(Category) %>%
  summarise(Total_Amount = sum(Amount), Avg_Amount = mean(Amount))

#EDA#
#1.Analisis Penjualan
##analisis tern waktu haria
library(ggplot2)
data$Date <- as.Date(data$Date, format="%m-%d-%y")
ggplot(data, aes(x = Date, y = Amount)) +
  geom_line(color = "blue") +
  labs(title = "Tren Penjualan Harian", x = "Tanggal", y = "Pendapatan")

##Tren Penjualan Bulanan
# Pastikan kolom Date dalam format Date
data$Date <- as.Date(data$Date, format = "%m-%d-%y")

# Tambahkan kolom bulan dan tahun
library(dplyr)
data <- data %>%
  mutate(Year = format(Date, "%Y"),
         Month = format(Date, "%Y-%m")) # Format YYYY-MM

# Hitung total pendapatan bulanan:
monthly_sales <- data %>%
  group_by(Month) %>%
  summarise(Total_Amount = sum(Amount))

#Visualisasi tren penjualan bulanan
library(ggplot2)
ggplot(monthly_sales, aes(x = as.Date(paste0(Month, "-01")), y = Total_Amount)) +
  geom_line(color = "blue") +
  labs(title = "Tren Penjualan Bulanan", x = "Bulan", y = "Total Pendapatan") +
  theme_minimal()

##tren tahunan:
# Hitung total pendapatan tahunan
yearly_sales <- data %>%
  group_by(Year) %>%
  summarise(Total_Amount = sum(Amount))

# Visualisasi tren penjualan tahunan
ggplot(yearly_sales, aes(x = Year, y = Total_Amount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Tren Penjualan Tahunan", x = "Tahun", y = "Total Pendapatan") +
  theme_minimal()
#distribusi Penjualan Berdasarkan Kategori
ggplot(data, aes(x = Category, y = Amount, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribusi Pendapatan per Kategori", x = "Kategori", y = "Pendapatan") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##Analisis Time Series untuk Pola Penjualan
# Hitung total pendapatan bulanan
monthly_sales <- data %>%
  group_by(Month) %>%
  summarise(Total_Amount = sum(Amount))
# Konversi ke format time series
ts_sales <- ts(monthly_sales$Total_Amount, start = c(as.numeric(format(min(data$Date), "%Y")), 
                                                     as.numeric(format(min(data$Date), "%m"))), frequency = 12)

# Plot time series
plot(ts_sales, main = "Analisis Time Series Penjualan Bulanan", xlab = "Tahun", ylab = "Pendapatan", col = "blue")

##analisis wilayah dengan pendapatan tertinggi
library(dplyr)
region_analysis <- data %>%
  group_by(ship.state) %>%
  summarise(Total_Amount = sum(Amount))

ggplot(region_analysis, aes(x = reorder(ship.state, Total_Amount), y = Total_Amount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Pendapatan Berdasarkan Negara Bagian", x = "Negara Bagian", y = "Pendapatan") +
  coord_flip()

#2.Analisis Kinerja Produk
##Kontribusi Kategori Produk
# Hitung kontribusi kategori produk
category_contribution <- data %>%
  group_by(Category) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
  mutate(Percentage = (Total_Amount / sum(Total_Amount)) * 100) %>%
  arrange(desc(Total_Amount))

# Lihat hasilnya
head(category_contribution)

# Visualisasi kontribusi kategori
ggplot(category_contribution, aes(x = reorder(Category, -Total_Amount), y = Total_Amount, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Kontribusi Kategori terhadap Total Pendapatan", x = "Kategori", y = "Total Pendapatan") +
  theme_minimal()

#3.analisis pelanggan
library(dplyr)
##segnentasi pelanggan
# Tambahkan kolom total pembelian per pelanggan
customer_segmentation <- data %>%
  group_by(Order.ID) %>% # Ganti dengan nama kolom ID pelanggan Anda
  summarise(
    Total_Transactions = n(),                          # Frekuensi pembelian
    Total_Spent = sum(Amount, na.rm = TRUE),           # Nilai total pembelian
    Preferred_Category = names(sort(table(Category), decreasing = TRUE))[1] # Kategori favorit
  )

# Lihat hasil segmentasi
head(customer_segmentation)

# Segmentasi berdasarkan Total_Spent (contoh: high, medium, low spender)
customer_segmentation <- customer_segmentation %>%
  mutate(Spending_Segment = case_when(
    Total_Spent >= quantile(Total_Spent, 0.75) ~ "High Spender",
    Total_Spent >= quantile(Total_Spent, 0.50) ~ "Medium Spender",
    TRUE ~ "Low Spender"
  ))

# Lihat hasil dengan segmen
head(customer_segmentation)

# Visualisasi segmentasi berdasarkan Total_Spent
library(ggplot2)
ggplot(customer_segmentation, aes(x = Spending_Segment, fill = Spending_Segment)) +
  geom_bar() +
  labs(title = "Segmentasi Pelanggan Berdasarkan Nilai Pembelian", 
       x = "Segmen Pembelanja", 
       y = "Jumlah Pelanggan") +
  theme_minimal()

##loyalitas pelanggan
# Hitung jumlah transaksi per pelanggan
loyalty_analysis <- data %>%
  group_by(Order.ID) %>%
  summarise(
    Total_Transactions = n()
  )

# Tambahkan kolom untuk menandai pelanggan yang melakukan pembelian ulang
loyalty_analysis <- loyalty_analysis %>%
  mutate(Repeat_Customer = ifelse(Total_Transactions > 1, "Yes", "No"))

# Hitung persentase pelanggan yang loyal
loyalty_summary <- loyalty_analysis %>%
  group_by(Repeat_Customer) %>%
  summarise(
    Count = n()
  ) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Lihat hasilnya
loyalty_summary

# Visualisasi persentase pelanggan loyal
ggplot(loyalty_summary, aes(x = Repeat_Customer, y = Percentage, fill = Repeat_Customer)) +
  geom_bar(stat = "identity") +
  labs(title = "Persentase Loyalitas Pelanggan", x = "Pelanggan Loyal", y = "Persentase") +
  theme_minimal()

#4.waktu & pola pembelian
data <- data %>%
  mutate(
    Day_of_Week = weekdays(as.Date(Date, format = "%m-%d-%y")),  # Hari dalam minggu
    Hour = format(as.POSIXct(Date, format = "%m-%d-%y %H:%M:%S"), "%H") # Jam (jika waktu tersedia)
  )

# Hitung total penjualan berdasarkan hari
sales_by_day <- data %>%
  group_by(Day_of_Week) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
  arrange(desc(Total_Amount))

# Lihat hasilnya
sales_by_day

# Visualisasi penjualan berdasarkan hari
ggplot(sales_by_day, aes(x = reorder(Day_of_Week, Total_Amount), y = Total_Amount, fill = Day_of_Week)) +
  geom_bar(stat = "identity") +
  labs(title = "Penjualan Berdasarkan Hari", x = "Hari", y = "Total Penjualan") +
  theme_minimal()

##berdasarkan jam##
# Jika data waktu tersedia, analisis penjualan per jam
sales_by_hour <- data %>%
  group_by(Hour) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
  arrange(desc(Total_Amount))

# Visualisasi penjualan berdasarkan jam
ggplot(sales_by_hour, aes(x = Hour, y = Total_Amount, fill = Hour)) +
  geom_bar(stat = "identity") +
  labs(title = "Penjualan Berdasarkan Jam", x = "Jam", y = "Total Penjualan") +
  theme_minimal()

#promosi
# Tandai transaksi dengan atau tanpa promosi
data <- data %>%
  mutate(Promotion_Applied = ifelse(is.na(promotion.ids), "No Promotion", "With Promotion"))

# Hitung total penjualan untuk transaksi dengan dan tanpa promosi
promo_effectiveness <- data %>%
  group_by(Promotion_Applied) %>%
  summarise(
    Total_Transactions = n(),
    Total_Amount = sum(Amount, na.rm = TRUE),
    Avg_Amount = mean(Amount, na.rm = TRUE)
  )

# Lihat hasilnya
promo_effectiveness

# Visualisasi efektivitas promosi
ggplot(promo_effectiveness, aes(x = Promotion_Applied, y = Total_Amount, fill = Promotion_Applied)) +
  geom_bar(stat = "identity") +
  labs(title = "Efektivitas Kampanye Promosi", x = "Promosi", y = "Total Penjualan") +
  theme_minimal()

# bandingkan periode sebelum dan setelah promosi dimulai
data <- data %>%
  mutate(Promotion_Period = ifelse(Date < as.Date("2023-01-01"), "Before Promotion", "After Promotion"))

promo_period_effect <- data %>%
  group_by(Promotion_Period) %>%
  summarise(
    Total_Transactions = n(),
    Total_Amount = sum(Amount, na.rm = TRUE),
    Avg_Amount = mean(Amount, na.rm = TRUE)
  )

# Visualisasi periode sebelum dan sesudah promosi
ggplot(promo_period_effect, aes(x = Promotion_Period, y = Total_Amount, fill = Promotion_Period)) +
  geom_bar(stat = "identity") +
  labs(title = "Penjualan Sebelum dan Sesudah Promosi", x = "Periode", y = "Total Penjualan") +
  theme_minimal()

#4.pREDIKTIF
##Time Series Forecasting
# Pastikan data dalam format time series
library(dplyr)
library(forecast)

# Hitung total penjualan bulanan (jika belum dilakukan)
data <- data %>%
  mutate(Date = as.Date(Date, format = "%m-%d-%y")) %>%
  group_by(Month = format(Date, "%Y-%m")) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE))

# Konversi data ke format time series
ts_data <- ts(data$Total_Amount, start = c(as.numeric(substr(data$Month[1], 1, 4)), as.numeric(substr(data$Month[1], 6, 7))), frequency = 12)

# Plot data time series
plot(ts_data, main = "Time Series Penjualan Bulanan", ylab = "Pendapatan", xlab = "Tahun")

# Forecast menggunakan ARIMA
model <- auto.arima(ts_data)
forecast_data <- forecast(model, h = 12)  # Prediksi 12 bulan ke depan

# Plot hasil prediksi
plot(forecast_data, main = "Prediksi Penjualan 12 Bulan ke Depan")

# Tampilkan prediksi
forecast_data

#Analisis Lanjutan#
#Analisis Korelasi
cor(data[c("Qty", "Amount")], use = "complete.obs")
#visualiz
library(corrplot)
corrplot(cor(data[c("Qty", "Amount")]), method = "circle")

#Analisis Promosi
promo_analysis <- data %>%
  group_by(!is.na(promotion.ids)) %>%
  summarise(Total_Amount = sum(Amount))
print(promo_analysis)

#tabel evaluasi promosis
library(dplyr)
promo_analysis <- data %>%
  mutate(Promo_Applied = ifelse(is.na(promotion.ids), "No Promotion", "With Promotion")) %>%
  group_by(Promo_Applied) %>%
  summarise(Total_Amount = sum(Amount), 
            Avg_Amount = mean(Amount),
            Total_Transactions = n())
promo_analysis

#bar chart
ggplot(promo_analysis, aes(x = Promo_Applied, y = Total_Amount, fill = Promo_Applied)) +
  geom_bar(stat = "identity") +
  labs(title = "Evaluasi Pengaruh Promosi terhadap Pendapatan", 
       x = "Promosi", 
       y = "Total Pendapatan") +
  theme_minimal()

#pie chart
promo_analysis <- promo_analysis %>%
  mutate(Percentage = (Total_Amount / sum(Total_Amount)) * 100)

ggplot(promo_analysis, aes(x = "", y = Percentage, fill = Promo_Applied)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Persentase Pendapatan dengan/ tanpa Promosi", 
       x = NULL, 
       y = NULL) +
  theme_void()

#boxplot distribusi pendapatan
ggplot(data, aes(x = ifelse(is.na(promotion.ids), "No Promotion", "With Promotion"), y = Amount)) +
  geom_boxplot(fill = c("skyblue", "pink")) +
  labs(title = "Distribusi Pendapatan dengan/ tanpa Promosi", 
       x = "Promosi", 
       y = "Pendapatan") +
  theme_minimal()

##K-means clsutering
library(ggplot2)
library(dplyr)
# 1. Siapkan data numerik untuk clustering
clustering_data <- data %>%
  group_by(Order.ID) %>%  # Ganti Customer_ID dengan ID pelanggan Anda
  summarise(
    Total_Spent = sum(Amount, na.rm = TRUE),
    Total_Qty = sum(Qty, na.rm = TRUE)
  )
# 2. Normalisasi data numerik (untuk menjaga skala yang konsisten)
clustering_data_scaled <- scale(clustering_data[, c("Total_Spent", "Total_Qty")])

# 3. Terapkan K-Means Clustering
set.seed(123)  # Untuk hasil yang konsisten
kmeans_result <- kmeans(clustering_data_scaled, centers = 3)  # Pilih 3 cluster (ubah sesuai kebutuhan)

# 4. Tambahkan hasil cluster ke data asli
clustering_data$Cluster <- as.factor(kmeans_result$cluster)

# 5. Lihat hasil clustering
head(clustering_data)

# 6. Visualisasi hasil clustering
ggplot(clustering_data, aes(x = Total_Spent, y = Total_Qty, color = Cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering: Segmentasi Pelanggan", x = "Total Spent", y = "Total Qty") +
  theme_minimal()

#Menentukan Jumlah Cluster yang Optimal (elbow method)
# Elbow Method untuk menentukan jumlah cluster optimal
wss <- sapply(1:10, function(k) {
  kmeans(clustering_data_scaled, centers = k, nstart = 10)$tot.withinss
})
# Visualisasi hasil Elbow Method
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Jumlah Cluster",
     ylab = "Total Within Sum of Squares",
     main = "Elbow Method untuk Menentukan Jumlah Cluster")
#resut
clustering_data %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Spent = mean(Total_Spent),
    Avg_Qty = mean(Total_Qty)
  )

## Decision Tree (Status Pesanan atau Profitability)
#package
library(rpart)
library(rpart.plot)
# 1. Siapkan data: Pilih kolom target dan prediktor
decision_tree_data <- data %>%
  select(Status, Category, Amount, ship.state) %>%
  filter(!is.na(Status))  
# 2. Split data menjadi training dan testing set
set.seed(123)
train_idx <- sample(1:nrow(decision_tree_data), 0.8 * nrow(decision_tree_data))
train_data <- decision_tree_data[train_idx, ]
test_data <- decision_tree_data[-train_idx, ]
# 3. Bangun model Decision Tree
decision_tree <- rpart(
  Status ~ Category + Amount + ship.state,
  data = train_data,
  method = "class",control=rpart.control(minsplit=20,cp=0))
# 4. Visualisasi Decision Tree
rpart.plot(decision_tree, type = 2, extra = 4, main = "Decision Tree: Faktor Pengaruh Status Pesanan")

# 5. Evaluasi Model: Prediksi pada data testing
predicted <- predict(decision_tree, test_data, type = "class")

# 6. Confusion Matrix
table(Predicted = predicted, Actual = test_data$Status)

