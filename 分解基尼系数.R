rm(list = ls())
#da<- read.csv('Dagum分解sample_data.csv',header=F)
da<- df_9000 
group <- da[,1] # 读取分组数据
R  <-  unique(group)# 计算分组标识
nR  <-  length(R)# 分组个数
N <- dim(da)[1]
Tt <- dim(da)[2]

data0 <- da
out <- matrix(0,nrow=Tt-1,ncol=4) # 输出分解结果，每年保存一行
out_Gw <- matrix(0,nrow=Tt-1,ncol=nR) # 输出组内差异
out_Gnb <- matrix(0,nrow=Tt-1,ncol=nR*(nR-1)/2) # 输出组间差异
gR <- matrix(0,nrow=Tt-1,ncol=nR)

Dagum_Gini <- function(Data,group){
  if(dim(as.matrix(Data))[2] != 1)
    Data  <-  t(Data)
  if(dim(as.matrix(group))[2] != 1)
    group  <-  t(group)
  if(dim(as.matrix(Data))[2] != 1){
    print("输入格式错误！")
  }else{
    R  <-  unique(group)
    nR  <-  length(R)
    cR  <-  sapply(R,function(x){
      ans  <-  sum(group == x)
      return(ans)
    })
    Rm  <-  matrix(0,nrow=nR,ncol = 5)
    Rm[,1]  <-  R;Rm[,2]  <-  cR
    mR  <-  max(cR) 
    Y <- matrix(0,nrow=nR,ncol=mR)
    for(i in 1:nR){
      yy <- Data[group == Rm[i,1]]
      Rm[i,3] <- mean(yy)
      Rm[i,4] <- i;
      Y[i,1:Rm[i,2]] <- t(yy)
    }
    # Rm矩阵说明：第一列为分组标识，第二列为每组观测个数，第三列为各组平均值，第4列辅助保存编号。
    
    # 下面按照各组平均值从小到大对Y的各行排序
    Y <- Y[order(Rm[,3]),]
    Rm <- Rm[order(Rm[,3]),]
    # 计算Gjh
    G_jh <- matrix(0,nrow=nR,ncol=nR)#
    Delta_jh <- G_jh 
    d_jh <- G_jh  
    p_jh <- G_jh   
    G <- 0   
    for(j in 1:nR){
      for(h in 1:nR){
        m <- Rm[h,2];n <- Rm[j,2] 
        for(r in 1:m){
          for(i in 1:n){
            G_jh[j,h] <- G_jh[j,h] + abs(Y[j,i]-Y[h,r]) 
            if(Y[j,i] > Y[h,r]){
              d_jh[j,h] <- d_jh[j,h] + Y[j,i] - Y[h,r] 
            }else{
              p_jh[j,h] <- p_jh[j,h] - Y[j,i] + Y[h,r] 
            }
          }
        }
        Delta_jh[j,h] <- G_jh[j,h]/(m*n) 
        G <- G+G_jh[j,h] 
        G_jh[j,h] <- Delta_jh[j,h]/((Rm[j,3]+Rm[h,3])) 
        d_jh[j,h] <- d_jh[j,h]/(m*n) 
        p_jh[j,h] <- p_jh[j,h]/(m*n) 
      }
    }
    G <- G/(2*length(Data)*length(Data)*mean(Data)); 
    p <- Rm[,2]/sum(Rm[,2]); 
    tem <- Rm[,3]/mean(Data); 
    s <- p*tem; 
    D_jh <- (d_jh-p_jh)/Delta_jh;
    gR <- Rm[,1]
    B <- sort(gR)
    ix <- order(gR)
    gR <- B
    
    Gw <- 0 
    out_Gw <- vector(mode = "double",length = nR)
    for(i in 1:nR){
      Gw <- Gw + G_jh[i,i]*p[i]*s[i]
      out_Gw[i] <- G_jh[ix[i],ix[i]]
    }
    Gnb <- 0
    Gt <- 0  
    out_Gnb <- vector(mode = "double",length = nR*(nR-1)/2) # 各组之间的差异，依次输出：2-1,3-1,3-2,4-1,4-2,4-3,5-1...
    m <- 0
    for(j in 2:nR){
      for(h in 1:(j-1)){  
        tw <- G_jh[j,h]*(p[j]*s[h]+p[h]*s[j])
        Gnb <- Gnb+ tw * D_jh[j,h]
        Gt <- Gt + tw * (1-D_jh[j,h])
        m <- m+1
        out_Gnb[m] <- G_jh[max(ix[j],ix[h]),min(ix[j],ix[h])]
      }
    }
    tem <- c(Gw,Gnb,Gt)
    out <- c(G,tem)
    output <- list(gini_dec=out,group_in_group=out_Gw,group_bet_group=out_Gnb,order=gR)
    return(output)
  }
}



for(i in 1:(Tt-1)){
  Data <- data0[,i+1] 
  print('正在对数据进行 Dagum 分解，请稍候...\n')
  rs <- Dagum_Gini(Data,group)
  out[i,] <- rs$gini_dec
  out_Gw[i,] <- rs$group_in_group
  out_Gnb[i,] <- rs$group_bet_group
  gR[i,] <- rs$order
  #rm(rs)
}
print('\n 分解完毕！')
print('\n 输出变量：out保存总分解结果；out_Gw保存组内差异；')
print('\n          out_Gnb保存组间差异；gR保存相应分组标识。\n')
View(out)
out_mean <- apply(out, 2, mean)
out_mean

col_1<-c("总差异G","组内差异Gw","组间差异Gnb",'超密度贡献Gt')
col_2<-c(1,2,3,4,5,6,7,8,9,10) 
col_3<-c(1,2,3,4,5,6,7,8,9,10)  

write_data<-function(data,file_name,col){
  new_data<-data.frame(data)
  colnames(new_data)<-col
  rownames(new_data)<-seq(2008,2020)
  write.csv(new_data,file_name)
}

write_data(out,'差异总表.csv',col_1)
write_data(out_Gw,'组内差异.csv',col_2)
write_data(out_Gnb,'组间差异.csv',col_3)
