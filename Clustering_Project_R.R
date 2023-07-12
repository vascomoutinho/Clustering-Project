setwd("C:/Users/utilizador/Desktop")
retail <- read.csv("OR.txt", sep = "\t")


str(retail)
summary(retail)
colnames(retail)

#Remove credit notes
retail=retail[!grepl("C", retail$InvoiceNo),]
substr(retail$InvoiceNo,1,1) == "C"

#Remove values = 0
retail = retail[retail$UnitPrice != 0,]

#Separate date and time and add date column
library(dplyr)
retail$Data<-substring(retail$InvoiceDate,1,10)
retail<- retail[,c(1,2,3,4,6,7,8,9)]
retail$Data <- as.Date(retail$Data, format = "%d/%m/%Y")
min(retail$Data)
max(retail$Data)

#Reduce time spectrum to 1 year
retail <- retail %>%
  filter(retail$Data >= as.Date("2010-12-09") & retail$Data <= as.Date("2011-12-09"))
min(retail$Data)
max(retail$Data)

#Variable Caracterization
retail$InvoiceNo <- as.factor(retail$InvoiceNo) 
retail$StockCode <- as.factor(retail$StockCode)
retail$Description <- as.factor(retail$Description)
retail$Country <- as.factor(retail$Country)
retail$CustomerID <- as.factor(retail$CustomerID)

retail$UnitPrice <- sub(",",".",retail$UnitPrice)
retail$UnitPrice <- as.numeric(retail$UnitPrice)
retail$Quantity <- as.numeric(retail$Quantity)

#Additio of new column Sales
retail$Sales <- retail$Quantity*retail$UnitPrice

#Most sold Products
Qt_P_Agg <- aggregate(Sales ~ Description, data = retail,sum)
aux <- aggregate(Quantity ~ Description, data = retail, sum)
Qt_P_Agg$Quantity <- aux$Quantity

Qt_P_Agg$UnitPriceM <- Qt_P_Agg$Sales/Qt_P_Agg$Quantity

#Creation of new column: frequency of product purchase
library(plyr)
Aux1 <- count(retail$Description)
Qt_P_Agg$FreqCompra <- Aux1$freq
Qt_P_Agg$FreqCompra <- as.numeric(Qt_P_Agg$FreqCompra)

#Remove outliers
boxplot(Qt_P_Agg$Quantity)
outliers<-boxplot.stats(Qt_P_Agg$Quantity)$out
Qt_P_Agg <- Qt_P_Agg[!Qt_P_Agg$Quantity %in% outliers,]

#Clients with most purchases
Qt_C_Agg <- aggregate(Sales ~ CustomerID + Quantity, data=retail, sum)

boxplot(Qt_P_Agg$Quantity)
str(Qt_P_Agg)
str(Qt_C_Agg)

#### Descriptive Analysis #####

#Mean and Standard Error 
mean <- unlist(lapply(Qt_P_Agg[,-c(1)], mean))
sd <- unlist(lapply(Qt_P_Agg[,-c(1)], sd))
mean
sd

#Min and Max
max <- unlist(lapply(Qt_P_Agg[,-c(1)], max))
min <- unlist(lapply(Qt_P_Agg[,-c(1)], min))
max
min


#Analysis of top 10 products that generate most revenue
library(dplyr)
library(ggplot2)
library(gtable)

top_10_sales <- Qt_P_Agg %>% 
  arrange(desc(Sales)) %>% 
  head(10)
  
ggplot(top_10_sales, aes(x=reorder(Description, Sales), y=Sales)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Top 10 Produtos que geram mais receita", x="Product Description", y="Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Analysis of top 10 clients that generate most revenue
top_10_clients <- Qt_C_Agg %>% 
  arrange(desc(Sales)) %>% 
  head(10)

ggplot(top_10_clients, aes(x=reorder(CustomerID, desc(Sales)), y=Sales)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Top 10 Clientes que geram mais receita", x="CustomerID", y="Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Understand which countrys generated the most revenue
Sales_per_Country<-aggregate(Sales ~ Country, data = retail, sum)

top_10_Countries <- head(Sales_per_Country[order(Sales_per_Country$Sales, decreasing = TRUE), ], 10)

ggplot(top_10_Countries, aes(x=reorder(Country, Sales), y=Sales, fill=Country)) +
  geom_bar(stat="identity") +
  xlab("Country") +
  ylab("Sales") +
  ggtitle("Top 10 Países que geram mais receita")

#Sales per month
retail$InvoiceDate <- as.Date(retail$Data, format="%d/%m/%Y") 
retail$mês <- format(retail$Data, "%Y-%m")
vendas_por_mês <- aggregate(Sales ~ mês, data = retail, sum)
str(vendas_por_mês)

Sales_Month<-ggplot(vendas_por_mês, aes(x = "", y = Sales, fill = mês)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  xlab("") + ylab("") + ggtitle("Vendas por mês")
Sales_Month


#Correlation
install.packages("GGally")
library(GGally)

cor(Qt_P_Agg$FreqCompra,Qt_P_Agg$UnitPriceM)
cor(Qt_P_Agg$FreqCompra,Qt_P_Agg$Quantity)
cor(Qt_P_Agg$Quantity,Qt_P_Agg$UnitPriceM)
plot(Qt_P_Agg$FreqCompra,Qt_P_Agg$UnitPriceM,xlab="Frequência de Compra",ylab="Preço Unitário Médio",main="Relação entre a Frequência de Compra e o Preço Unitário Médio")
plot(Qt_P_Agg$FreqCompra,Qt_P_Agg$Quantity,xlab="Frequência de Compra",ylab="Quantidade",main="Relação entre a Frequência de Compra e a Quantidade")
plot(Qt_P_Agg$Quantity,Qt_P_Agg$UnitPriceM,xlab="Quantidade",ylab="Preço Unitário Médio",main="Relação entre a Quantidade e o Preço Unitário Médio")


###Normalization
z_score_function <- function(x) {
  (x - mean(x, na.rm=TRUE))/(sd(x,na.rm=TRUE))
}

normed <- data.frame(apply(Qt_P_Agg[,-c(1,2)],2, z_score_function))
str(normed)

### Clustering ####

#### Partitioning algorithms heuristic method: k-means

#Elbow curve
ss_vector <- c()
for (i in 1:10) {
  set.seed(7)
  model <- kmeans(normed,i, iter.max = 1000)
  ss_vector <- c(ss_vector,model$tot.withinss)
}

plot(ss_vector, type="b",xlab="Number of Clusters", ylab="Within groups sum of squares") 

#Davies-Bouldin method
library(clusterSim)

db_vector <- c()
for (i in 2:10) {
  set.seed(7)
  model <- kmeans(normed,i)
  DB<-index.DB(normed,model$cluster)$DB
  db_vector <- c(db_vector,DB)
}
 
DB 
plot(2:10,db_vector, type="b",xlab="Number of Clusters", ylab="Davies Bouldin")

#Use of k-means with optimal k value
set.seed(7)
k_means_model <- kmeans(normed,6)
str(k_means_model)
k_means_model$cluster
k_means_model$centers
k_means_model$withinss

#Creation of two columns:description and cluster
normed$Description <- Qt_P_Agg$Description  
normed$cluster <- k_means_model$cluster

#Hierarquical method
dist <- dist(normed[ ,-c(4,5)])
hc <- hclust(dist, method = "complete") #single or complete
plot(hc)
rect.hclust(hc,4)
normed$clusternumber <- cutree(hc,4)

#Comparison bethween aglomerative and hierarquical method
comparison <- table(normed$clusternumber,normed$cluster)


#Boxplot charts
normed$cluster <- k_means_model$cluster

boxplot1<-boxplot(Quantity ~ cluster, data= normed,main= "Cluster", ylab= "Quantity")
boxplot2<-boxplot(UnitPriceM ~ cluster, data= normed,main= "Cluster", ylab= "UnitPriceM")
boxplot3<-boxplot(FreqCompra ~ cluster, data= normed,main= "Cluster", ylab= "FreqCompra")

#Cluster Plot
install.packages("factoextra")
library(factoextra)
k_meanss<-kmeans(normed[,-c(4,5)],centers=6,iter.max=1000)
Cluster_Plot<-fviz_cluster(k_meanss,data=normed[,-c(4,5)],geom="point")
Cluster_Plot

gcluster<-kmeans(normed[,c(1:3)], centers = 6, iter.max = 1000)
plot(normed[c("Quantity","UnitPriceM")], col = gcluster$cluster)
plot(normed[c("Quantity","FreqCompra")], col = gcluster$cluster)
plot(normed[c("FreqCompra","UnitPriceM")], col = gcluster$cluster)

