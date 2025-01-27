# 1. Kh�m ph� d??? li???u
  data_customer=read.csv("Mall_Customers.csv")
  str(data_customer)
  
  names(data_customer)
  
    # head(): Hi???n th??? mu???i h�ng d???u ti�n c???a t???p d??? li???u
    head(data_customer, 10)
    
    
    # summary(): Xem t???ng quan t�m t???t v??? d??? li???u
    summary(data_customer)
    
    summary(data_customer$Tuoi)
    sd(data_customer$Tuoi)
    
    summary(data_customer$Thu.Nhap.Hang.Nam..k..)
    sd(data_customer$Thu.Nhap.Hang.Nam..k..)
    
    summary(data_customer$Diem.Chi.Tieu..1.100.)
    sd(data_customer$Diem.Chi.Tieu..1.100.)
  
# 2. Tr???c quan h�a d??? li???u

# 2.1. Tr???c quan h�a gi???i t�nh kh�ch h�ng 
    
    
    # barplot(): Bi???u d??? h�nh kh???i cho bi???n gi???i t�nh 
    
    gt = table(data_customer$Gioi.Tinh)
    barplot(gt, main="BI???U D??? SO S�NH GI???I T�NH - S??? D???NG BARPLOT",
            ylab = "SoLuong",
            xlab = "GioiTinh",
            col = rainbow(2),
            legend = rownames(gt))

    #pie(): Bi???u d??? h�nh tr�n m� t??? t??? l??? nam v� n???
    library(plotrix)
    ct = round(gt/sum(gt)*100)
    lbs = paste(c("Female", "Male")," ", ct, "%", sep="")
    pie3D(gt, 
          labels = lbs,
          main = "BI???U D??? M� T??? T??? L??? NAM N??? - PIECHART")

# 2.2. Tr???c quan h�a d??? li???u tu???i c???a kh�ch h�ng

    summary(data_customer$Tuoi)
  
    #hist(): V??? histogram cho bi???n tu???i
    hist(data_customer$Tuoi,
         xlab = "Age Class",
         ylab = "Frequency",
         labels = TRUE,
         col = "blue",
         main = "BI???U D??? TH??? HI???N PH�N B??? L???P TU???I KH�CH H�NG")

    #boxplot(): V??? d??? th??? theo d???ng h�nh h???p
    boxplot(data_customer$Tuoi,
              col = "red",
              main = "BOXPLOT - BI???U D??? PH�N T�CH V� M� T??? TU???I C???A KH�CH H�NG")

# 2.3. Ph�n t�ch thu nh???p h�ng nam c???a kh�ch h�ng

    summary(data_customer$Thu.Nhap.Hang.Nam..k..)

    #hist():
    hist(data_customer$Thu.Nhap.Hang.Nam..k..,
       col = "pink",
       main = "BI???U D??? THU NH???P H�NG NAM",
       xlab = "Annual Income Class",
       ylab = "Frequency",
       labels = TRUE)
  
    #plot():
    plot(density(data_customer$Thu.Nhap.Hang.Nam..k..),
       col = "yellow",
       main = "BI???U D??? M???T D??? - THU NH???P H�NG NAM",
       xlab = "Annual Income Class",
       ylab = "Density")
  
    polygon(density(data_customer$Thu.Nhap.Hang.Nam..k..),
          col ="yellow")  

# 2.4. Ph�n t�ch Spending Score c???a kh�ch h�ng
  
    summary(data_customer$Diem.Chi.Tieu..1.100.)
  
    #boxplot():
    boxplot(data_customer$Diem.Chi.Tieu..1.100.,
            horizontal = TRUE,
            col = "red",
            main = "BOXPLOT - PH�N T�CH SPENDING SCORE")
    
    #hist():
    hist(data_customer$Diem.Chi.Tieu..1.100.,
         main = "BI???U D??? CHO PENDING SCORE",
         xlab = "Spending Score Class",
         ylab = "frequency",
         col = "orchid",
         labels = TRUE)
# 3. Ph�n kh�c kh�ch h�ng 
    # Phuong ph�p Elbow
    library(purrr)
    set.seed(123)
    # h�m d??? t�nh to�n t???ng b�nh phuong t???ng trong c???m 
    iss <- function(k) {
      kmeans(data_customer[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" 
             )$tot.withinss
    }
    k.values <- 1:10
    iss_values <- map_dbl(k.values, iss)
    plot(k.values, iss_values,
         type="b", pch = 19, frame = FALSE, 
         xlab="Number of clusters K",
         ylab="Total intra-clusters sum of squares")
    # Phuong ph�p Silhouette trung b�nh
    library(cluster) 
    library(gridExtra)
    library(grid)
    k2<-kmeans(data_customer[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
    s2<-plot(silhouette(k2$cluster,dist(data_customer[,3:5],"euclidean")))

    
    #
    k4<-kmeans(data_customer[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
    s4<-plot(silhouette(k4$cluster,dist(data_customer[,3:5],"euclidean")))
    #
    k5<-kmeans(data_customer[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
    s5<-plot(silhouette(k5$cluster,dist(data_customer[,3:5],"euclidean")))
    #
    k6<-kmeans(data_customer[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
    s6<-plot(silhouette(k6$cluster,dist(data_customer[,3:5],"euclidean")))
    #
    k7<-kmeans(data_customer[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
    s7<-plot(silhouette(k7$cluster,dist(data_customer[,3:5],"euclidean")))
    #
    k8<-kmeans(data_customer[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
    s8<-plot(silhouette(k8$cluster,dist(data_customer[,3:5],"euclidean")))
    #
    k9<-kmeans(data_customer[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
    s9<-plot(silhouette(k9$cluster,dist(data_customer[,3:5],"euclidean")))
    
    #
    k10<-kmeans(data_customer[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
    s10<-plot(silhouette(k10$cluster,dist(data_customer[,3:5],"euclidean")))
    
    #
    library(NbClust)
    library(factoextra)
    fviz_nbclust(data_customer[,3:5], kmeans, method = "silhouette")
    
    # Phuong ph�p th???ng k� kho???ng c�ch 
    set.seed(125)
    stat_gap <- clusGap(data_customer[,3:5], FUN = kmeans, nstart = 25,
                        K.max = 10, B = 50)
    fviz_gap_stat(stat_gap)
    
    #
    k6<-kmeans(data_customer[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
    k6
    
#5. Ph�n c???m kh�ch h�ng
  pcclust=prcomp(data_customer[,3:5],scale=FALSE) #principal component analysis
  summary(pcclust)
  pcclust$rotation[,1:2]

#
  set.seed(1)
  ggplot(data_customer, aes(x = Thu.Nhap.Hang.Nam..k.., y = Diem.Chi.Tieu..1.100.)) + 
    geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
    scale_color_discrete(name=" ",
                         breaks=c("1", "2", "3", "4", "5","6"),
                         labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
    ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")
    
#

  ggplot(data_customer, aes(x = Diem.Chi.Tieu..1.100., y = Tuoi)) + 
    geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
    scale_color_discrete(name=" ",
                         breaks=c("1", "2", "3", "4", "5","6"),
                         labels=c("Cluster 1", "Cluster 2", "Cluster 3", 
                                  "Cluster 4", "Cluster 5","Cluster 6")) +
    ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")
#
  ggplot(data_customer, aes(x = Tuoi, y = Thu.Nhap.Hang.Nam..k..)) + 
    geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
    scale_color_discrete(name=" ",
                         breaks=c("1", "2", "3", "4", "5","6"),
                         labels=c("Cluster 1", "Cluster 2", "Cluster 3", 
                                  "Cluster 4", "Cluster 5","Cluster 6")) +
    ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")
#
  kCols=function(vec){cols=rainbow (length (unique (vec)))
  return (cols[as.numeric(as.factor(vec))])}
  
  digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters
  
  plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
  legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))
