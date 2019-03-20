#Name: Chipo Banza, ID: 45575002
#ACST890 Take home quiz 1
#Question 1
bondfn <- function(C,F,n,deltas) {

  coupons <- seq(0.5, n/2, by= 0.5)# coupons = times when coupons are paid
  
  index <- 1:length(coupons)# index of coupon payment times
  cashflows <- rnorm(n)
  
  for (i in index){
    t <- coupons[i]
    d <- deltas[i]
    
    discount <- exp(-d*t)
    cf <- C*discount
    
    cashflows[i] <- cf
  }
  cf <- F*exp(deltas[n]*coupons[n])
  p <- cf + sum(cashflows)
  
  return(p)
}



#Question 3
#3.a)
dataset = read.csv("singapore.economy.csv")
#3.b)
ndataset = na.omit(dataset)
attach(ndataset)
#3.c)
plot(ndataset$time,ndataset$gdp, xlab = "Time", ylab = "GDP(%)",main = "Singapore GDP growth")
#3.d)
#mean for period 1
period1 = subset(ndataset, ndataset$period == 1)
 m1 <- mean(period1$gdp)
#mean period 1 = 1.702953
#standard deviation for period 1
sd1 <- sd(period1$gdp)
#sd = 1.685216

#mean for period 2
period2 = subset(ndataset, ndataset$period == 2)
m2 <- mean(period2$gdp)
#mean 2 = 1.571337
sd2 <- sd(period2$gdp)
#sd 2 = 1.787153

#mean for period 3 
period3 = subset(ndataset, ndataset$period == 3)
m3 <- mean(period3$gdp)
#mean 3 = 1.04528
sd3 <- sd(period3$gdp)
#sd 3 = 2.408231

means = c(m1,m2,m3)
stdDevs = c(sd1, sd2, sd3)
stat.table = data.frame(as.table(setNames(means, stdDevs)))

#3.e)
#scatter plot of the pairs of variables excluding time and period
pairs(~gdp+exp+epg+hpr+gdpus+oil+crd+bci, data = ndataset, main = "Scatter plot")

#3.f)
model <- lm(ndataset$gdp~ndataset$exp, data = ndataset)
summary(model)

#3.g)
model2 <- glm(formula = gdp~exp+epg+hpr+oil+gdpus+crd, 
             data = ndataset)
summary(model2)

#3.h)
qt = quantile(gdp, 0.05)
state = c(ndataset$gdp < qt)
newdataset = data.frame(ndataset, state)
model3 <- lm(newdataset$state~newdataset$bci, data = newdataset)
summary(model3)
