data <- read.csv("C:/Users/10120/Desktop/科研/基尼系数/数据/已处理数据/test_20.csv")

zero_num <- sum(data[,1]==0)
zero_num

no_zero_data <- data[data[,1]!=0,] 

fin_data <- no_zero_data[order(no_zero_data[,1]), ]

print(fin_data)

zero_num2 <- sum(fin_data[,1]==0)
zero_num2
nrow <- nrow(fin_data)
nrow
n <- 10

#构造初始矩阵
df <- matrix(0,nrow = nrow(fin_data),ncol=3)
df[,2] <- fin_data[,1]
df[,3] <- fin_data[,2]
# 将数据等分成n组，并添加一列表示类别的属性
df[,1]<- cut(c(1:nrow(fin_data)), breaks = n, labels = c(1:n))
print(df[4000,])
df

##构造df2矩阵
R <- c(1:n)
df2 <- matrix(0,nrow=n,ncol=6)
df2[,1] <- R #组类别
#各组收入均值
df2[,2] <- sapply(R, function(x) {
  ans <- mean(df[df[,1] == x, 2])
  return(ans)
})
#各组总人数
df2[,3] <- sapply(R, function(x) {
  ans <- sum(df[df[,1] == x, 3])
  return(ans)
})
#各组样本数量
df2[,4] <- sapply(R, function(x) {
  ans <- sum(df[,1] == x)
  return(ans)
})
#各组起始序号
df2[,5] <- sapply(R, function(x) {
  num <- 1
  if (x == 1) {
    num <- 1
  } else {
    for (i in 1:(x - 1)) {
      num <- num + df2[i, 4]
    }
  }
  return(num)
})
#各组结束序号
df2[,6] <- sapply(R, function(x) {
  num <-df2[x,5]+df2[x,4]-1
  return(num)
})

df2


##计算组间基尼系数
G_jh <- matrix(0,ncol=n,nrow = n)
G_1 <- 0
G_2 <- 0

Delta <- matrix(0,ncol=n,nrow = n)
for(j in 1:n){
  for(h in 1:n){
    d <- 0
    for(i in df2[j,5]:df2[j,6]){
      for(r in df2[h,5]:df2[h,6]){
        d <- abs(df[i,2]-df[r,2])*df[i,2]*df[r,3]
        Delta[j,h] <-  Delta[j,h]+d
      }
    }
  }
}
Delta

# #各组间绝对差值Delta_jh
# for (j in 1:n){#第j组
#   for (h in 1:n){#第i组
#     if(j != h){
#       for (i in df2[j,5]:df2[j,6]){
#         for (r in df2[h,5]:df2[h,6]){
#           a <- 2*abs(df[i,2]-df[r,2])*df[i,3]*df[r,3]
#           Delta_jh[j,h] <- Delta_jh[j,h]+a}
#       }
#     }else
#     {
#       for (i in df2[j,5]:df2[h,6] ){
#         for (r in i:df2[h,6]){
#           if (i <= nrow(df) && r <= nrow(df)) {
#             a <- 2 * abs(df[i, 2] - df[r, 2]) * df[i, 3] * df[r, 3]
#             Delta_jh[j, h] <- Delta_jh[j, h] + a
#           }
#         }
#       }
#     }}}
# Delta_jh
# 
# # #各组间绝对差值Delta_jh
# # for (j in 1:n){#第j组
# #   for (h in 1:n){#第i组
# #     if(j != h){
# #       for (i in df2[j,5]:df2[j,5]+df2[j,4]-1 ){
# #         for (r in df2[h,5]:df2[h,5]+df2[h,4]-1){
# #           a <- 2*abs(df[i,2]-df[r,2])*df[i,3]*df[r,3]
# #           Delta_jh[j,h] <- Delta_jh[j,h]+a}
# #       }
# #       }else
# #       {
# #         for (i in df2[j,5]:df2[j,5]+df2[j,4]-1 ){
# #           for (r in i:df2[h,5]+df2[h,4]-1){
# #             if (i <= nrow(df) && r <= nrow(df)) {
# #               a <- 2 * abs(df[i, 2] - df[r, 2]) * df[i, 3] * df[r, 3]
# #               Delta_jh[j, h] <- Delta_jh[j, h] + a
# #       }
# #     }
# #   }
# # }}}

#组间基尼系数
G_jh <- matrix(0,ncol=n,nrow = n)
for (j in 1:n){#第j组
  for (h in 1:n){#第i组
      G_jh[j,h] <- Delta[j,h]/((df2[j,2]+df2[h,2])*df2[j,4]*df2[h,4])
  }
}
G_jh

P_i <- rep(0,n)#人口占比
s_i <- rep(0,n)#收入占比
mu <- mean(df[,2])
for( i in 1:n ){
  P_i[i] <- df2[i,3]/sum(df[,3]) 
}

for( i in 1:n ){
  s_i[i] <- df2[i,2]*df2[i,4]/sum(df[,2]) 
}

s_i
P_i
sum(s_i)
sum(P_i)
mu
max <- max(df[,2])
max

for (i in 1:n){
  G_1 <- G_1+P_i[i]*s_i[i]*G_jh[i,i]
}


for (j in 1:n ){
    for (h in j+1:n)
    {
      if(h<=10){ 
      a <- P_i[j]*P_i[h]*(df2[j,2]-df2[h,2])/mu
      G_2 <- G_2+ a}
      else {
      G_2 <- G_2  
      }
    }
}
s_i
P_i
G <- G_1 + G_2
G
G_hat <- G + 1/(2*n)
G_hat




