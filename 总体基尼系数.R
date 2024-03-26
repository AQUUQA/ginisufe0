data <- read.csv("C:/Users/10120/Desktop/科研/基尼系数/数据/已处理数据/test_20.csv")
data
###总体基尼系数：G=绝对平均差值/2mu
##处理初始数据 
no_zero_data <- data[data[,1]!=0,] 
fin_data <- no_zero_data[order(no_zero_data[,1]), ]
d0 <- fin_data
d0
nrow(d0)
##计算绝对平均差值
test <- d0[1:3,]
test
test_n <- 5
nsquare <- nrow(d0)*nrow(d0)
delta <- 0
for(i in 1:nrow(d0)){
  for(j in i+1:n){
    delta <- delta+2*abs(d0[i,1]-d0[j,1])/nsquare
  }
}
delta
##计算总体均值
mu <- sum(d0[,1])/nrow(d0)
mu
##计算基尼系数
G <- delta/2*mu
G



