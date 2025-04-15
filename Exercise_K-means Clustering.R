# Load library
library(ggplot2)
library(plotly)

#data
data(iris)
head(iris)

iris_data <- iris[, -5]  # Hilangkan kolom Species (agar sesuai dg latihan)

#K-means Clustering
set.seed(123)  # Supaya hasil konsisten
kmeans_result <- kmeans(iris_data, centers = 3, nstart = 25)

#tambahkan hasil cluster pada dataset
iris$Cluster <- as.factor(kmeans_result$cluster)

#visualisasi 2D
p <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "K-Means Clustering on Iris Dataset",
       x = "Petal Length", y = "Petal Width") +
  theme_minimal()

ggplotly(p)

#visualisasi 3D
plot_ly(iris, 
        x = ~Sepal.Length, 
        y = ~Petal.Length, 
        z = ~Petal.Width,
        color = ~Cluster,
        colors = c('#636EFA', '#EF553B', '#00CC96'),
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 5)) %>%
  layout(title = "3D K-Means Clustering on Iris Dataset",
         scene = list(
           xaxis = list(title = 'Sepal Length'),
           yaxis = list(title = 'Petal Length'),
           zaxis = list(title = 'Petal Width')
         ))


#Bandingkan dengan Species Asli
table(iris$Species, iris$Cluster) #cek seberapa cocok hasil klastering

