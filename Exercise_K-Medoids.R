#instalpackages
library(cluster)

#call data
data(iris)
iris_data <- iris[, -5]  # Buang label Species

#Model K-Medoids
set.seed(123)
kmedoids_result <- pam(iris_data, k = 3)
iris$Cluster_KMedoids <- as.factor(kmedoids_result$clustering)

#Visualisasi
library(plotly)

plot_ly(iris,
        x = ~Sepal.Length,
        y = ~Petal.Length,
        z = ~Petal.Width,
        color = ~Cluster_KMedoids,
        colors = c('#AB63FA', '#FFA15A', '#19D3F3'),
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5)) %>%
  layout(title = "3D K-Medoids Clustering (Iris Dataset)",
         scene = list(
           xaxis = list(title = 'Sepal Length'),
           yaxis = list(title = 'Petal Length'),
           zaxis = list(title = 'Petal Width')
         ))

#Evaluasi Model
table(iris$Species, iris$Cluster_KMedoids)

