#call pacakge
library(caret)

#load data
library(readxl)
data<-read_excel("D:/RevoU/DEEP/DEEP_Chrun Bank/Dataset_Churn.xlsx")
View(data)

#1.Hitung frekuensi masing-masing kelas
class_freq <- table(training$Exited)

#2.Hitung bobot seimbang (kebalikan dari proporsi kelas)
weights <- ifelse(training$Exited == 1,
                  1 / class_freq["1"],
                  1 / class_freq["0"])

#prediksi data ke testing
kolom_drop <- c("Surname", "RowNumber")
training <- training[ , !(names(training) %in% kolom_drop)]
testing <- testing[ , !(names(testing) %in% kolom_drop)]

#3.Fit model dengan bobot
model_weighted <- glm(Exited ~ ., data = training, family = binomial(), weights = weights)

#4.Cek hasilnya
summary(model_weighted)

#5.Prediksi probabilitas
prediksi_prob <- predict(model_weighted, newdata = testing, type = "response")

#6.Konversi probabilitas ke kelas (threshold 0.5)
prediksi_kelas <- ifelse(prediksi_prob > 0.5, 1, 0)

#7.evaluasi model
# Ubah Exited testing ke faktor agar cocok
testing$Exited <- as.factor(testing$Exited)
prediksi_kelas <- as.factor(prediksi_kelas)

# Confusion Matrix
library(caret)
confusionMatrix(prediksi_kelas, testing$Exited, positive = "1")

#8.evaluasi lanjutan
library(pROC)
roc_obj <- roc(testing$Exited, prediksi_prob)
auc(roc_obj)  # Tampilkan nilai AUC

# Plot ROC
plot(roc_obj, main = "ROC Curve")

#9.Interpretasi Kofesioen
exp(coef(model_weighted))  # Odds ratio

