#call library
library(quantmod)
library(vars)
library(Spillover)

# Unduh data dari Yahoo Finance
getSymbols(c("SPY", "TLT", "GLD", "EURUSD=X", "BTC-USD"), src = "yahoo", from = "2015-01-01", to = "2025-01-01")

# Ambil harga penutupan harian (adjusted close)
data <- merge(Cl(SPY), Cl(TLT), Cl(GLD), Cl(`EURUSD=X`), Cl(`BTC-USD`))
colnames(data) <- c("Stocks", "Bonds", "Commodities", "Forex", "Crypto")

# Hitung return log
log_returns <- diff(log(data))[-1, ]
head(log_returns)

log_returns1 <- na.omit(log_returns)  # Hapus baris dengan NA
head(log_returns1)

# Estimasi model VAR
var_model <- VAR(log_returns1, p = 2, type = "const")
summary(var_model)

# Konversi ke format spillover
spillover_data <- as.matrix(log_returns1)

# Analisis Spillover menggunakan Diebold-Yilmaz Framework
sp<-G.spillover(var_model,n.ahead = 10,standardized = F)
sp

# Visualisasi Spillover Index
plot(sp)

library(igraph)
# Buat matriks adjacency dari spillover results
Spillover::net(sp)

#connetdetnes
sp<-G.spillover(var_model,n.ahead = 10,standardized = F)
datanet<-Spillover::net(sp)
datanet

#data frame node
node_df<-data.frame(row.names (datanet),row.names(datanet),datanet$Net)
names(node_df)<-c("id","label","size")
head(node_df)

sp<-sp[1:5,1:5]
sp

#data frame edge
m1<-melt(sp)[melt(upper.tri(sp))$value,]
m2<-melt(sp)[melt(lower.tri(sp))$value,]
m1<-m1[order(m1$X1),]
m2<-m2[order(m1$X2),]

edge_df<-data.frame("to"=m1[,2],"from"=m1[,1],"weight"=m1$value-m2$value)

library(dplyr)

#edge_df = dataframe
edge_df_positif<-edge_df%>%filter(weight>=0)
edge_df_negatif<-edge_df%>%filter(weight<0)

#balikan nlai weight
edge_df_negatif<-edge_df_negatif%>%mutate(weight=-weight)%>%rename(to=from,from=to)

#gabungkan
edge_df<-bind_rows(edge_df_positif,edge_df_negatif)

positive_weight<-edge_df$weight[edge_df$weight>0]
negative_weight<-edge_df$weight[edge_df$weight<0]
positive_size<-node_df$size[node_df$size>0]
negative_size<-node_df$size[node_df$size<0]

#beri warna setiap node&edge
library(RColorBrewer)
transmitter_color<-"#2ca25f"
else_color<-"#de2d26"
color_vec1<-ifelse(edge_df$weight>0,transmitter_color,else_color)
color_vec2<-ifelse(node_df$size>0,transmitter_color,else_color)

#grafik
graph<-graph_from_data_frame(edge_df,directed = TRUE,vertices = node_df)

#warna to grafik
E(graph)$color<-"black"#edge
V(graph)$color<-color_vec2#node

#nilai absolout
E(graph)$weight<-abs(edge_df$weight)
V(graph)$size<-abs(node_df$size)

#edge&note
E(graph)$weight<-E(graph)$weight/max(E(graph)$weight)*2
V(graph)$size<-V(graph)$size/max(E(graph)$size)*5

plot(graph,edge.width=E(graph)$weight,layout=layout_in_circle(graph),edge.arrow.mode=2,edge.arrow.size=0.1)
