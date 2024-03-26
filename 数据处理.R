## 读取第一个数据集
df_12 <- read.csv("C:/Users/10120/Desktop/科研/基尼系数/数据/已处理数据/fin_12.csv")
df_14 <- read.csv("C:/Users/10120/Desktop/科研/基尼系数/数据/已处理数据/fin_14.csv")
df_16 <- read.csv("C:/Users/10120/Desktop/科研/基尼系数/数据/已处理数据/fin_16.csv")
df_18 <- read.csv("C:/Users/10120/Desktop/科研/基尼系数/数据/已处理数据/fin_18.csv")
df_20 <- read.csv("C:/Users/10120/Desktop/科研/基尼系数/数据/已处理数据/fin_20.csv")

# ## 对每个数据集做预处理
# fin_df_12 <- df_12[df_12[,8]!= 0 & !is.na(df_12[,8]),8]
# fin_data_12
# class(fin_data_12)
# fin_df_12 <- fin_df_12[order(fin_df_12), ]
# fin_df_12 <- df_12[df_12[, 8] != 0 & !is.na(df_12[, 8]), 8]
# 
# 
# fin_df_14 <- df_14[df_14[,8]!= 0 & !is.na(df_14[,8]),8]
# fin_df_14 <- t(as.matrix(fin_df_14))
# fin_df_14
# dim(fin_data_14)
# class(fin_data_14)
# 
# df_16
# fin_df_166 <- df_16[df_16[, 10] != 0 & !is.na(df_16[, 10]), 10]
# fin_df_16 <- df_16[df_16[,10]!= 0 & !is.na(df_16[,10]),10]
# fin_df_16-fin_df_166
# 
# df_18
# fin_df_18 <- df_18[df_18[, 11] != 0 & !is.na(df_18[, 11]), 11]
# fin_df_18
# 
# df_20
# fin_df_20 <- df_20[df_20[, 12] != 0 & !is.na(df_20[, 12]), 12]
# fin_df_20

df_0 <- matrix(0,nrow = 9000,ncol=5)
# 从每年中抽取 9000 个样本
sample_size <- 9000
sampled_indices <- sample(nrow(df_12), size = sample_size, replace = FALSE)
df_12 <- df_12[sampled_indices, ]

sampled_indices <- sample(nrow(df_14), size = sample_size, replace = FALSE)
df_14 <- df_14[sampled_indices, ]

sampled_indices <- sample(nrow(df_16), size = sample_size, replace = FALSE)
df_16 <- df_16[sampled_indices, ]

sampled_indices <- sample(nrow(df_18), size = sample_size, replace = FALSE)
df_18 <- df_18[sampled_indices, ]

sampled_indices <- sample(nrow(df_20), size = sample_size, replace = FALSE)
df_20 <- df_20[sampled_indices, ]

df_12 <- as.matrix(df_12)
df_12 <- df_12[order(df_12[,1]), ]

df_14 <- as.matrix(df_14)
df_14 <- df_14[order(df_14[,1]), ]

df_16 <- as.matrix(df_16)
df_16 <- df_16[order(df_16[,1]), ]

df_18 <- as.matrix(df_18)
df_18 <- df_18[order(df_18[,1]), ]

df_20 <- as.matrix(df_20)
df_20 <- df_20[order(df_20[,1]), ]

## 合并数据集
n <- 10
df_9000 <- matrix(0,nrow = 9000,ncol=6)
df_9000[,1]<- cut(c(1:9000), breaks = n, labels = c(1:n))
df_9000[,2] <- df_12
df_9000[,3] <- df_14
df_9000[,4] <- df_16
df_9000[,5] <- df_18
df_9000[,6] <- df_20
df_9000 
da<- df_9000
# 获取当前工作目录的路径
current_directory <- getwd()
print(current_directory)
# 设置新的工作目录
setwd("C:/Users/10120/Desktop/科研/基尼系数/数据/已处理数据")

write.csv(data, file = "df_9000.csv", row.names = FALSE)

