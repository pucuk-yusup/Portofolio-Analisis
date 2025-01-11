#Data#
datafin<-read.csv("D:/Portofolio/deutsche_bank_financial_performance.csv")

##1.Menghitung korelasi antara Debt-to-Equity dan Net Income##
correlation <- cor(datafin$Debt_to_Equity, datafin$Net_Income, use = "complete.obs")
print(paste("Korelasi antara Debt-to-Equity dan Net Income:", round(correlation, 2)))

#scater plot#
library(ggplot2)
ggplot(datafin, aes(x = Debt_to_Equity, y = Net_Income)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Hubungan Debt-to-Equity dan Net Income", x = "Debt-to-Equity", y = "Net Income")

##2.Berapa persentase kontribusi Operating Income terhadap peningkatan Equity selama 5 tahun terakhir?##
# Filter data untuk 5 tahun terakhir#
library(lubridate)
library(dplyr)
datafin$Date <- mdy(datafin$Date)
recent_data <- datafin %>% filter(Date >= max(Date) - 5)

# Menghitung kontribusi rata-rata Operating Income terhadap Equity#
average_contribution <- mean(recent_data$Operating_Income / recent_data$Equity) * 100
print(paste("Persentase rata-rata kontribusi Operating Income terhadap Equity:", round(average_contribution, 2), "%"))

##3.Dapatkah perubahan Profit Margin diprediksi berdasarkan Expenses dan Revenue?##
# Membuat model regresi linear
model <- lm(Profit_Margin ~ Expenses + Revenue, data = datafin)
summary(model)

# Melihat koefisien model
print(coef(model))

# Membuat plot residual untuk memeriksa kesesuaian model
plot(model, which = 1)

##4.Bagaimana arus kas memengaruhi kemampuan bank dalam membayar dividen secara konsisten?##
# Korelasi antara Cash Flow dan Dividend Payout
correlation_dividend <- cor(datafin$Cash_Flow, datafin$Dividend_Payout, use = "complete.obs")
print(paste("Korelasi antara Cash Flow dan Dividend Payout:", round(correlation_dividend, 2)))

# Membuat scatter plot
ggplot(datafin, aes(x = Cash_Flow, y = Dividend_Payout)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Hubungan Cash Flow dan Dividend Payout", x = "Cash Flow", y = "Dividend Payout")

##5.Apa tren ROA dalam 10 tahun terakhir, dan bagaimana perubahan ini terkait dengan strategi aset?##
# Menghitung rata-rata ROA per tahun
roa_trend <- datafin %>%
  group_by(Date) %>%
  summarise(mean_roa = mean(ROA, na.rm = TRUE))

# Membuat plot tren ROA
ggplot(roa_trend, aes(x = Date, y = mean_roa)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Tren ROA dalam 2 Tahun Terakhir", x = "Tahun", y = "Rata-rata ROA")

# Analisis hubungan antara ROA dan Assets
correlation_roa_assets <- cor(datafin$ROA, datafin$Assets, use = "complete.obs")
print(paste("Korelasi antara ROA dan Assets:", round(correlation_roa_assets, 2)))