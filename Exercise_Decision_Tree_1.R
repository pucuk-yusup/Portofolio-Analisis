# 1. Load library
library(rpart)
library(rpart.plot)

# 2. Load data
data(mtcars)

# 3. Ubah 'am' jadi faktor karena kita mau klasifikasi
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))

# 4. Buat model decision tree
tree_mtcars <- rpart(am ~ mpg + hp + wt + cyl + disp, data = mtcars, method = "class")

# 5. Visualisasi decision tree
prp(tree_mtcars,
    type = 4,
    extra = 104,
    under = TRUE,
    faclen = 0,
    fallen.leaves = TRUE,
    shadow.col = "gray",
    box.col = c("lightcoral", "lightblue")[tree_mtcars$frame$yval],
    split.font = 2,
    varlen = 0)

# Tambahkan warna gradasi atau soft tone lebih menarik
library(RColorBrewer)
colors <- brewer.pal(3, "Set2")  # bisa juga coba "Pastel1" atau "Accent"

# Visualisasi yang lebih stylish
prp(tree_mtcars,
    type = 4,
    extra = 104,
    under = TRUE,
    faclen = 0,
    fallen.leaves = TRUE,
    shadow.col = "darkgray",
    box.col = colors[tree_mtcars$frame$yval],  # auto-warna dari palette
    split.font = 1,
    varlen = 0,
    tweak = 1.2,                 # perbesar teks dikit
    nn = FALSE,                   # tampilkan nomor node
    branch.lty = 2,              # garis putus-putus untuk branch
    branch.col = "gray40",      # warna garis branch
    border.col = "gray50",      # outline box
    split.cex = 1.1             # ukuran teks split
)

library(partykit)
library(ggparty)
# Konversi model ke format partykit
tree_party <- as.party(tree_mtcars)

ggparty(tree_party) +
  geom_edge() +
  geom_edge_label(size = 3, fontface = "italic") +
  geom_node_splitvar(fontface = "bold", size = 4, color = "#336699") +
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = factor(info)), width = 0.6)),
                 scales = "fixed", ids = "terminal", shared_axis_labels = TRUE) +
  geom_node_label(aes(label = paste("Node", id, "\n", info)),
                  ids = "terminal", size = 3, label.padding = unit(0.5, "lines"),
                  fill = "lightblue", color = "black") +
  theme_minimal() +
  ggtitle("Decision Tree - Predicting Transmission Type") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Prediksi
pred <- predict(tree_mtcars, type = "class")

# Confusion matrix
table(Predicted = pred, Actual = mtcars$am)

# Hitung akurasi
mean(pred == mtcars$am)
