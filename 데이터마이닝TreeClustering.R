# 1

treetrue = read.csv('covtype.csv')
treetrue

tree답 <- treetrue[ , 'Cover_Type']
tree답
tree
tree1000답 <- tree답[7000:8000]
tree1000답
table(tree1000답)
library(tidyverse)  
library(cluster)    
library(dendextend) 
library(factoextra)
# install.packages('dendextend')
# install.packages('factoextra') 

df <- tree
str(df)

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

hc4 <- diana(df)
hc4$dc
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

#https://uc-r.github.io/hc_clustering
##################333
# Mini-batch K-means 클러스터링 수행



d <- dist(df, method = "euclidean")
hc <- hclust(d, method='single')
sub_grp <- cutree(hc, k = 3)
table(sub_grp)
rs = df %>%
  mutate(cluster = sub_grp) 
plot(hc, cex = 0.6)
rect.hclust(hc, k = 3 border = 2:5)
fviz_cluster(list(data = df, cluster = sub_grp))
plot(hc, cex = 0.6, labels = NULL)
rect.hclust(hc, k = 3, border = 2:5)

# 클러스터링 결과 시각화
fviz_cluster(list(data = df, cluster = sub_grp), geom = "point")

# 와드 5 
d1 <- dist(df, method = "euclidean")
hc1 <- hclust(d1, method='ward.D2')
sub_grp <- cutree(hc1, k = 5)
table(sub_grp)
rs = USArrests %>%
  mutate(cluster = sub_grp) 
plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 5, border = 2:5)
fviz_cluster(list(data = df, cluster = sub_grp))
plot(hc, cex = 0.6, labels = NULL)
# 클러스터링 결과 시각화
fviz_cluster(list(data = df, cluster = sub_grp), geom = "point")

# 와드2


tree3 <- tree[,1:3]
tree3

df <-tree3

d1 <- dist(df, method = "euclidean")
hc1 <- hclust(d1, method='ward.D2')
sub_grp <- cutree(hc1, k = 2)
table(sub_grp)
rs = USArrests %>%
  mutate(cluster = sub_grp) 
plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 2, border = 2:5)
fviz_cluster(list(data = df, cluster = sub_grp))
plot(hc, cex = 0.6, labels = NULL)
# 클러스터링 결과 시각화
fviz_cluster(list(data = df, cluster = sub_grp), geom = "point")

##형님 코드

selected_vars <- c("Elevation", "Aspect", "Slope")

df <- treetrue
# 최소 5개의 연속형 변수 선택
X_continuous <- df %>% select(all_of(selected_vars))
# 데이터 표준화
X_scaled <- scale(X_continuous)

X_scaled
##############################################
# PCA를 사용하여 변수를 축소
pca_result <- prcomp(X_scaled)
# 주성분 개수를 결정하기 위해 설명된 분산 비율을 확인
summary(pca_result)

# 주성분 개수 결정
plot(summary(pca_result)$importance[2,], type="b")

# 주성분 개수 선택
n_pca <- 2 # 예를 들어, 주성분 개수를 2개로 선택

# 주성분 개수에 따라 데이터 변환
X_pca <- as.data.frame(pca_result$x[, 1:n_pca])

# 수동으로 군집 수 설정 
optimal_clusters <- 4  # 예를 들어, 3개의 군집으로 설정
optimal_clusters
# K-means 군집 분석 수행
kmeans_result_pca <- kmeans(X_pca, centers = optimal_clusters, nstart = 25)
kmeans_result_pca 

# 군집 결과를 데이터 프레임에 추가
df$Cluster <- as.factor(kmeans_result_pca$cluster)

# 시각화
fviz_cluster(kmeans_result_pca, data = X_pca, geom = "point", ellipse.type = "convex")


##########
