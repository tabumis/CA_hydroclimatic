a<-as.data.frame(seq(1:100))
colnames(a)<-"m1"

b<-as.data.frame(seq(1:100))
b$n2<-seq(100, 1, length.out = 100)
b$n3<-floor(runif(100, min=0, max=101))
seq(100, 1, length.out = 100)

colnames(b)<-c("n1","n2","n3")


re<-cor(a,b,method = "spearman")
re


cor.test(a,b)

cor.test(a,b)$p.value