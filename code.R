# 4.(a)
# Set working directory
setwd('C:\\Users\\FuuuFuuu\\iCloudDrive\\Work\\26R')
# Library package
library(readxl)
# Read data
df = read_xlsx('Chosen Price.xlsx')
# Build function to calculate YTM
# Create bond valuation function
bval <- function(i, cf,last, t=seq(along = cf)-last)
  sum(cf / (1 + i)^t)
# Create ytm() function using uniroot
ytm <- function(cf,last) {
  uniroot(bval, c(0, 1), cf = cf,last = last)$root
}
# Use ytm() function to find yield for 10*10 bonds
ytm.result=data.frame(Date = c(16:20,23:27))
for(bond_i in 1:10){
  bond_result = c()
  for(date_j in 1:10){
    cf = c(-1*as.numeric(df[bond_i,8+date_j]),
           rep(100*df$Coupon[bond_i]/2,floor(df$`Months until Maturity`[bond_i]/6)-1),
           100+100*df$Coupon[bond_i]/2)
    last = df$`Months since Last Coupon`[bond_i]
    bond_result = c(bond_result,(1+2*ytm(cf,last))^5-1)
  }
  ytm.result = data.frame(ytm.result,bond=bond_result)
}   
names(ytm.result)[-1] = df$`Bond Name`
# Wide to long
library(ggplot2)
library(tidyr)
ytm.result.long = gather(ytm.result, Bond, YTM, `CAN2.5 Dec 32`:`CAN3.5 Mar 28`, factor_key=TRUE)
ggplot(data=ytm.result.long,aes(x = Date,y=YTM,col=Bond))+
  ylab('5-Year YTM')+
  theme_bw()+
  geom_line()


# 4(b)
ytm.result=data.frame(Date = c(16:20,23:27))
for(bond_i in 1:10){
  bond_result = c()
  for(date_j in 1:10){
    cf = c(-1*as.numeric(df[bond_i,8+date_j]),
           rep(100*df$Coupon[bond_i]/2,floor(df$`Months until Maturity`[bond_i]/6)-1),
           100+100*df$Coupon[bond_i]/2)
    last = df$`Months since Last Coupon`[bond_i]
    bond_result = c(bond_result,2*ytm(cf,last))
  }
  ytm.result = data.frame(ytm.result,bond=bond_result)
}   
names(ytm.result)[-1] = df$`Bond Name`
# Use 2.5% bond as base bond
spot_rate=data.frame(Date = c(16:20,23:27))
# Use 1.5% bond to calculate 1 year
spot_rate$`1`=
  (as.numeric(df[3,9:18])*(1+ytm.result$`CAN1.5 Dec 31`)/as.numeric(df[1,9:18]))-1
# Use 0.5% bond to calculate 2 year spot rate
spot_rate$`2`=
  (as.numeric(df[5,9:18])*(1+ytm.result$`CAN0.5 Dec 30`)^2/as.numeric(df[1,9:18]))^0.5-1
# Use 2.25% bond to calculate 3 year spot rate
spot_rate$`3`=
  (as.numeric(df[7,9:18])*(1+ytm.result$`CAN2.25 Jun 29`)^3/as.numeric(df[1,9:18]))^(1/3)-1
# Use 2% bond to calculate 4 year spot rate
spot_rate$`4`=
  (as.numeric(df[9,9:18])*(1+ytm.result$`CAN2 Jun 28`)^4/as.numeric(df[1,9:18]))^(1/4)-1
# Use bootstrap
spot_rate$`5`=2*spot_rate$`4`-spot_rate$`3`
# Wide to long
spot_rate.long = gather(spot_rate, Day, Spot, `1`:`5`, factor_key=TRUE)
ggplot(data=spot_rate.long,aes(x = Date,y=Spot,col=Day))+
  ylab('Spot rates')+
  theme_bw()+
  geom_line()

# 4(c)
forward_rate=spot_rate
forward_rate$`2`=(1+spot_rate$`2`)/(spot_rate$`1`+1)-1
forward_rate$`3`=((1+spot_rate$`3`)/(spot_rate$`1`+1))^(1/2)-1
forward_rate$`4`=((1+spot_rate$`4`)/(spot_rate$`1`+1))^(1/3)-1
forward_rate$`5`=((1+spot_rate$`5`)/(spot_rate$`1`+1))^(1/4)-1
# Wide to long
forward_rate.long = gather(forward_rate, Day, Forward, `1`:`5`, factor_key=TRUE)
ggplot(data=forward_rate.long,aes(x = Date,y=Forward,col=Day))+
  ylab('Forward rates')+
  theme_bw()+
  geom_line()

# 5
# YTM
ytm.mat=ytm.result[,-1]
ytm.cov=cov(log(ytm.mat[2:10,]/ytm.mat[1:9,]))
options(digits=2)
print((ytm.cov))
# Forward
forward.mat=forward_rate[,-1]
forward.cov=cov(log(forward.mat[2:5,]/forward.mat[1:4,]))
options(digits=2)
print((forward.cov))

# 6
print(eigen(ytm.cov)) 
print(eigen(forward.cov)) 






