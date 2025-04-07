#package
library(readxl)
library(rpart)
library(caret)
library(rpart.plot)

#data
data_1<-read_excel("D:/Machine Learning/DATA KLASIFIKASI.xlsx")
View(data_1)
ds_1<-sample(1:nrow(data_1),0.75*nrow(data_1))
training<-data.frame(data_1)[ds_1,]
testing<-data.frame(data_1)[-ds_1,]

#model decision tree
model<-rpart(Keputusan~.,data = training,method = "class",control = rpart.control(minsplit = 20,cp=0))

#visualisasi
prp(model,extra = 4,box.col = c("lightgreen","grey","pink"))

# Visualisasi dengan styling tambahan
prp(model,
    type = 4,              # tipe 4 = label di bawah node
    extra = 104,           # 100 = class name + probs, 4 = probs per class
    under = TRUE,          # tampilkan label class di bawah kotak
    faclen = 0,            # nama faktor ditampilkan lengkap
    fallen.leaves = TRUE,  # biar daunnya rata di bawah
    shadow.col = "gray",   # kasih bayangan di node
    box.col = c("lightgreen", "lightblue", "pink")[model$frame$yval], # warnain berdasarkan kelas
    split.font = 2,        # font tebal untuk teks split
    varlen = 0             # tampilkan nama variabel lengkap
)

#prediksi
prediksi<-predict(model,testing)
prediksi

#ubah prediksi angka menjadi kategorik
kategorik<-colnames(prediksi)[max.col(prediksi,ties.method = c("random"))]
kategorik

#evaluasi model
kelas<-table(kategorik,testing$Keputusan)
confusionMatrix(kelas)
