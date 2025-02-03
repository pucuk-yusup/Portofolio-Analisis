#package
library(quantmod)
stocks<-c("BBCA.JK","BBRI.JK","BBNI.JK","BMRI.JK")

#tarik data
data_list<-lapply(stocks, function(stock){getSymbols(stock,src = "yahoo",from="2005-01-01",to="2025-01-24",auto.assign = FALSE)})

#pe harian
returns <- lapply(data_list, function(data) {
  dailyReturn(Cl(data)) # Gunakan fungsi Cl() untuk mengambil closing prices
})

head(returns[[1]])

#merge data
combined_returns<-do.call(merge,returns)
names(combined_returns)<-stocks
combined_returns<-na.omit(combined_returns)
head(combined_returns)

#DCC GARCH Analyst
library(rugarch)
library(rmgarch)
#Garch
unispec<-ugarchspec(mean.model = list(armaorder=c(0,0)),
                    variance.model = list(model="gjrGARCH",garchorder=c(1,1)),distribution.model = "norm")

#jumlah aset
n_aset<-ncol(combined_returns)
garch_spec<-multispec(replicate(n_aset,unispec))

#DCC model
dcc_spec<-dccspec(uspec = garch_spec,dccOrder = c(1,1),distribution = "mvnorm")

#dcc fit
dcc.fit<-dccfit(dcc_spec,data = combined_returns,fit.control = list(scale=TRUE))
dcc.fit

#interpretasi
#1.kovarians conditions
kov<-rcov(dcc.fit)
dim(kov)
kov[,,1:4]

#2.volatility conditional
vol<-sigma(dcc.fit)
head(vol)
plot(vol$BBCA.JK)
plot(vol$BBRI.JK)
plot(vol$BBNI.JK)
plot(vol$BMRI.JK)

#3.corelation conditional
cor<-rcor(dcc.fit)
cor[,,1:4]

date<-row.names(data.frame(cor[1,1,]))
cor2<-xts(cor[1,2,],order.by = as.Date(date))
plot(cor2)

##Analisis lanjutan yg Komprehensif##
#1.cluster volatility regimes#
library(xts)
library(cluster)
# Konversi ke dataframe untuk clustering
vol_df <- data.frame(date = index(vol), coredata(vol))

# Normalisasi data volatilitas agar clustering lebih efektif
vol_scaled <- scale(vol_df[, -1]) # Hanya gunakan data volatilitas (tanpa tanggal)

# K-Means Clustering
set.seed(123) # Untuk hasil yang konsisten
k <- 2 # Tentukan jumlah cluster (misalnya: 2 cluster untuk high & low volatility)
kmeans_result <- kmeans(vol_scaled, centers = k, nstart = 25)

# Tambahkan hasil cluster ke data
vol_df$Cluster <- kmeans_result$cluster

# Visualisasi hasil cluster
library(ggplot2)
vol_df_long <- reshape2::melt(vol_df, id.vars = c("date", "Cluster"), 
                              variable.name = "Stock", value.name = "Volatility")

ggplot(vol_df_long, aes(x = date, y = Volatility, color = as.factor(Cluster))) +
  geom_line() +
  facet_wrap(~Stock, scales = "free_y") +
  labs(title = "Clustered Volatility Regimes",
       x = "Year", y = "Volatility",
       color = "Cluster") +
  theme_minimal()

# Analisis hasil cluster
table(vol_df$Cluster) # Distribusi jumlah hari dalam masing-masing cluster

#kelanjutannya 
#menentukan jumlah kluster optimal
wss <- sapply(1:10, function(k) kmeans(vol_scaled, centers = k, nstart = 25)$tot.withinss)
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares")

#cluter labeling
cluster_means <- aggregate(vol_df[, -c(1, ncol(vol_df))], by = list(vol_df$Cluster), FUN = mean)
print(cluster_means) # Rata-rata volatilitas dalam setiap cluster

#2.Perubahan Volatilitas Sebelum dan Selama Krisis#
# Tentukan periode krisis pasar
crisis_start <- as.Date("2020-03-01") # Contoh: awal pandemi COVID-19
crisis_end <- as.Date("2020-06-30")   # Contoh: akhir Q2 2020

# Tambahkan kolom untuk menandai periode krisis
vol_df$CrisisPeriod <- ifelse(vol_df$date >= crisis_start & vol_df$date <= crisis_end, "Crisis", "Non-Crisis")

# Hitung rata-rata volatilitas untuk setiap periode (Crisis vs Non-Crisis)
avg_volatility <- aggregate(vol_df[, -c(1, ncol(vol_df))], 
                            by = list(CrisisPeriod = vol_df$CrisisPeriod), 
                            FUN = mean)

# Tampilkan hasil rata-rata volatilitas
print(avg_volatility)

# Visualisasi perubahan volatilitas sebelum/dalam periode krisis
library(ggplot2)
ggplot(vol_df, aes(x = date)) +
  geom_line(aes(y = BBCA.JK, color = CrisisPeriod)) +
  labs(title = "Volatilitas BBCA Sebelum dan Selama Krisis",
       x = "Tahun", y = "Volatilitas", color = "Periode") +
  theme_minimal()

ggplot(vol_df, aes(x = date)) +
  geom_line(aes(y = BBRI.JK, color = CrisisPeriod)) +
  labs(title = "Volatilitas BBRI Sebelum dan Selama Krisis",
       x = "Tahun", y = "Volatilitas", color = "Periode") +
  theme_minimal()

ggplot(vol_df, aes(x = date)) +
  geom_line(aes(y = BBNI.JK, color = CrisisPeriod)) +
  labs(title = "Volatilitas BBNI Sebelum dan Selama Krisis",
       x = "Tahun", y = "Volatilitas", color = "Periode") +
  theme_minimal()

ggplot(vol_df, aes(x = date)) +
  geom_line(aes(y = BMRI.JK, color = CrisisPeriod)) +
  labs(title = "Volatilitas BMRI Sebelum dan Selama Krisis",
       x = "Tahun", y = "Volatilitas", color = "Periode") +
  theme_minimal()

#3.Hubungan Cluster dengan Peristiwa Makroekonomi Besar#
# Tambahkan anotasi peristiwa makroekonomi
vol_df$Event <- ifelse(vol_df$date >= as.Date("2020-03-01") & vol_df$date <= as.Date("2020-06-30"), "COVID-19",
                       ifelse(vol_df$date >= as.Date("2022-03-01") & vol_df$date <= as.Date("2022-12-31"), "The Fed Rate Hike",
                              ifelse(vol_df$date >= as.Date("2021-01-01") & vol_df$date <= as.Date("2021-12-31"), "Commodity Boom", "Normal")))

# Analisis distribusi cluster selama peristiwa
event_cluster <- table(vol_df$Event, vol_df$Cluster)
print(event_cluster)

# Visualisasi distribusi cluster per peristiwa
library(reshape2)
event_cluster_df <- as.data.frame(event_cluster)
colnames(event_cluster_df) <- c("Event", "Cluster", "Count")

ggplot(event_cluster_df, aes(x = Event, y = Count, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribusi Cluster Volatilitas Berdasarkan Peristiwa Makroekonomi",
       x = "Peristiwa", y = "Jumlah Hari", fill = "Cluster") +
  theme_minimal()

#4. Identifikasi Saham dengan Kontribusi Risiko Terbesar#
# Ambil matriks kovarians bersyarat dari model DCC-GARCH
cov_matrix <- rcov(dcc.fit)

# Hitung volatilitas portofolio berbobot sama
n_assets <- dim(cov_matrix)[1]
weights <- rep(1 / n_assets, n_assets) # Bobot sama untuk setiap saham

# Kontribusi risiko untuk setiap saham
risk_contribution <- lapply(1:dim(cov_matrix)[3], function(t) {
  cov_t <- cov_matrix[,,t]                  # Matriks kovarians pada waktu t
  port_variance <- t(weights) %*% cov_t %*% weights  # Variansi portofolio
  marginal_contrib <- cov_t %*% weights / as.numeric(port_variance) # Kontribusi marginal
  contrib <- weights * marginal_contrib     # Kontribusi total
  return(data.frame(Date = index(combined_returns)[t], Contrib = contrib))
})

str(risk_contribution[[1]])

#5.Periksa Stabilitas Matriks Kovarians#
# Periode yang ingin dianalisis
pre_crisis <- which(index(combined_returns) >= as.Date("2019-01-01") & index(combined_returns) <= as.Date("2020-02-29"))
during_crisis <- which(index(combined_returns) >= as.Date("2020-03-01") & index(combined_returns) <= as.Date("2020-06-30"))
post_crisis <- which(index(combined_returns) >= as.Date("2020-07-01") & index(combined_returns) <= as.Date("2021-12-31"))

# Hitung matriks kovarians rata-rata untuk setiap periode
cov_pre <- apply(cov_matrix[,,pre_crisis], c(1, 2), mean)
cov_during <- apply(cov_matrix[,,during_crisis], c(1, 2), mean)
cov_post <- apply(cov_matrix[,,post_crisis], c(1, 2), mean)

# Tampilkan perubahan matriks kovarians
print("Kovarians Rata-rata Sebelum Krisis:")
print(cov_pre)

print("Kovarians Rata-rata Selama Krisis:")
print(cov_during)

print("Kovarians Rata-rata Setelah Krisis:")
print(cov_post)

# Analisis perubahan struktural dengan visualisasi heatmap
library(ggcorrplot)
ggcorrplot(cov_pre, lab = TRUE, title = "Kovarians Sebelum Krisis")
ggcorrplot(cov_during, lab = TRUE, title = "Kovarians Selama Krisis")
ggcorrplot(cov_post, lab = TRUE, title = "Kovarians Setelah Krisis")

#6.Backtesting#
#Strategi Hedging#
# Ambil matriks korelasi dinamis dari model DCC-GARCH
dynamic_correlation <- rcor(dcc.fit)

# Identifikasi pasangan saham dengan korelasi rendah
low_correlation_pairs <- function(cor_matrix, threshold = 0.2) {
  n_assets <- dim(cor_matrix)[1]
  pairs <- list()
  
  for (i in 1:(n_assets - 1)) {
    for (j in (i + 1):n_assets) {
      avg_correlation <- mean(cor_matrix[i, j, ], na.rm = TRUE)
      if (avg_correlation < threshold) {
        pairs <- append(pairs, list(c(stocks[i], stocks[j], avg_correlation)))
      }
    }
  }
  
  return(pairs)
}

# Cari pasangan dengan korelasi rata-rata rendah
low_corr_pairs <- low_correlation_pairs(dynamic_correlation, threshold = 0.2)
print("Pasangan saham dengan korelasi rata-rata rendah:")
print(low_corr_pairs)

# Visualisasi korelasi dinamis untuk salah satu pasangan saham
pair_to_plot <- c(1, 2) # Misalnya pasangan BBCA dan BBRI
cor_pair <- xts(dynamic_correlation[pair_to_plot[1], pair_to_plot[2], ], order.by = index(combined_returns))

cor_pair_df <- fortify.zoo(cor_pair)
colnames(cor_pair_df) <- c("Date", "Correlation")
cor_pair_df$Pair <- paste(stocks[pair_to_plot[1]], "vs", stocks[pair_to_plot[2]])

# Plot dengan warna berbeda untuk pasangan saham BCA & BRI
ggplot(data = cor_pair_df, aes(x = Date, y = Correlation, color = Pair)) +
  geom_line(size = 1) +
  labs(title = paste("Korelasi Dinamis:", stocks[pair_to_plot[1]], "vs", stocks[pair_to_plot[2]]),
       x = "Tahun", y = "Korelasi", color = "Pasangan Saham") +
  theme_minimal()

library(ggplot2)
ggplot(data = fortify.zoo(cor_pair), aes(x = Index, y = cor_pair)) +
  geom_line() +
  labs(title = paste("Korelasi Dinamis:", stocks[pair_to_plot[1]], "vs", stocks[pair_to_plot[2]]),
       x = "Tahun", y = "Korelasi") +
  theme_minimal()

#Value at Risk (VaR)#
# Matriks kovarians bersyarat dari model DCC-GARCH
cov_matrix <- rcov(dcc.fit)

# Simulasi return portofolio berbobot sama
n_assets <- dim(cov_matrix)[1]
weights <- rep(1 / n_assets, n_assets) # Bobot sama untuk semua saham

# Hitung return portofolio harian
portfolio_return <- rowSums(combined_returns * weights, na.rm = TRUE)

# Hitung VaR menggunakan distribusi bersyarat
VaR <- function(cov_matrix, weights, alpha = 0.05) {
  n_time <- dim(cov_matrix)[3]
  var_results <- numeric(n_time)
  
  for (t in 1:n_time) {
    cov_t <- cov_matrix[,,t]                   # Matriks kovarians pada waktu t
    port_variance <- t(weights) %*% cov_t %*% weights # Variansi portofolio
    port_sd <- sqrt(port_variance)             # Standar deviasi portofolio
    var_results[t] <- -qnorm(alpha) * port_sd  # VaR berdasarkan distribusi normal
  }
  
  return(var_results)
}

# Hitung VaR untuk setiap hari
var_results <- VaR(cov_matrix, weights)

# Gabungkan VaR dengan return portofolio untuk analisis
VaR_df <- data.frame(Date = index(combined_returns), 
                     PortfolioReturn = portfolio_return, 
                     VaR = var_results)

# Visualisasi VaR dan return portofolio
ggplot(VaR_df, aes(x = Date)) +
  geom_line(aes(y = PortfolioReturn), color = "blue", size = 0.8) +
  geom_line(aes(y = -VaR), color = "red", linetype = "dashed") +
  labs(title = "Portfolio Return dan Value at Risk (VaR)",
       x = "Tahun", y = "Nilai",
       color = "Legend") +
  theme_minimal()

#7.bobot portofolio optimum#
#package
library(quadprog)  # Untuk optimasi portofolio

# Ambil matriks kovarians bersyarat dari model DCC-GARCH
cov_matrix <- rcov(dcc.fit)

# Jumlah aset dalam portofolio
n_assets <- dim(cov_matrix)[1]

# Fungsi untuk menghitung bobot portofolio minimum variansi
min_var_portfolio <- function(cov_matrix) {
  Dmat <- cov_matrix  # Matriks kovarians
  dvec <- rep(0, n_assets)  # Tidak ada return yang dioptimalkan
  Amat <- cbind(rep(1, n_assets))  # Kendala bobot total = 1
  bvec <- 1
  
  # Optimasi dengan metode quadratic programming
  result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  return(result$solution)
}

# Hitung bobot portofolio minimum variansi untuk setiap hari
optimal_weights <- t(sapply(1:dim(cov_matrix)[3], function(t) {
  min_var_portfolio(cov_matrix[,,t])
}))

# Konversi hasil ke dalam data frame
optimal_weights_df <- data.frame(Date = index(combined_returns), optimal_weights)
colnames(optimal_weights_df) <- c("Date", stocks)

# Visualisasi bobot portofolio optimum dari waktu ke waktu
optimal_weights_long <- reshape2::melt(optimal_weights_df, id.vars = "Date", 
                                       variable.name = "Stock", value.name = "Weight")

ggplot(optimal_weights_long, aes(x = Date, y = Weight, fill = Stock)) +
  geom_area(alpha = 0.7) +
  labs(title = "Bobot Portofolio Optimum (Minimum Variance)",
       x = "Tahun", y = "Bobot Portofolio") +
  theme_minimal()

#8.Hedging Effectiveness#
# Ambil matriks korelasi bersyarat dari model DCC-GARCH
cor_matrix <- rcor(dcc.fit)

# Pilih pasangan saham untuk hedging (misalnya BBCA & BBRI)
hedge_pair <- c(1, 2)  # Indeks pasangan saham dalam stocks

# Hitung rasio hedging optimal berdasarkan regresi
hedge_ratio <- cor_matrix[hedge_pair[1], hedge_pair[2], ] * 
  (sigma(dcc.fit)[, hedge_pair[2]] / sigma(dcc.fit)[, hedge_pair[1]])

# Hitung return portofolio tanpa hedging (portofolio berbobot sama)
unhedged_portfolio_return <- rowSums(combined_returns * (1 / n_assets), na.rm = TRUE)

# Hitung return portofolio dengan hedging
hedged_portfolio_return <- unhedged_portfolio_return - hedge_ratio * combined_returns[, hedge_pair[2]]

# Hitung variansi portofolio sebelum & sesudah hedging
var_unhedged <- var(unhedged_portfolio_return, na.rm = TRUE)
var_hedged <- var(hedged_portfolio_return, na.rm = TRUE)

# Hitung efektivitas hedging
hedging_effectiveness <- 1 - (var_hedged / var_unhedged)

# Tampilkan hasil
print(paste("Hedging Effectiveness:", round(hedging_effectiveness * 100, 2), "%"))

# Visualisasi return portofolio sebelum dan sesudah hedging
hedging_df$Date <- data.frame(Date = index(combined_returns),
                         Unhedged = unhedged_portfolio_return,
                         Hedged = hedged_portfolio_return)

ggplot(hedging_df$Date, aes(x = Date)) +
  geom_line(aes(y = Unhedged, color = "Tanpa Hedging"), size = 1) +
  geom_line(aes(y = hedged_portfolio_return, color = "Dengan Hedging"), size = 1, linetype = "dashed") +
  labs(title = "Perbandingan Return Portofolio: Tanpa vs Dengan Hedging",
       x = "Tahun", y = "Return Portofolio", color = "Status") +
  theme_minimal()

