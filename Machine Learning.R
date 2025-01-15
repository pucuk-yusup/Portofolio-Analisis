#package#
library(caret)
library(randomForest)

#simple dataset#
set.seed(123)
data <- data.frame(
  Temperature = runif(1000, min = 20, max = 35), # Suhu (dalam derajat Celcius)
  Humidity = runif(1000, min = 50, max = 100),   # Kelembapan (%)
  Pressure = runif(1000, min = 900, max = 1100), # Tekanan udara (hPa)
  WindSpeed = runif(1000, min = 0, max = 20),    # Kecepatan angin (km/h)
  Rainfall = sample(c("Normal", "Extreme"), 1000, replace = TRUE, prob = c(0.7, 0.3)) # Target
)

data$Rainfall <- as.factor(data$Rainfall)
head(data)

# dataset to training dan testing
set.seed(123)
trainIndex <- createDataPartition(data$Rainfall, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# model Random Forest
model_rf <- randomForest(Rainfall ~ ., data = trainData, ntree = 100, importance = TRUE)

# Melihat pentingnya variabel
importance(model_rf)
varImpPlot(model_rf)

# Evaluasi model pada data testing
predictions <- predict(model_rf, newdata = testData)
conf_matrix <- confusionMatrix(predictions, testData$Rainfall)
print(conf_matrix)

# Simpan model jika diperlukan
saveRDS(model_rf, "rainfall_prediction_model.rds")