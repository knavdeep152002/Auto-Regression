library(ggplot2)
library(matlib)
library(Metrics)
#read data
dataset <- read.csv("E:/IDA/Proj/FINAL_FROM_DF.csv")
#plot time series
dataset$TIMESTAMP = as.Date(dataset$TIMESTAMP)
syms <- unique(dataset$SYMBOL)[0:1]
syms
data <- dataset[dataset$SYMBOL==syms, c("TIMESTAMP","OPEN")]
#dropna to remove NA values
data <- data[!is.na(data$OPEN),]
data <- data[order(data$TIMESTAMP),]

#create time series plot
gg <- ggplot(data, aes(x=TIMESTAMP, y=OPEN)) + geom_line()
gg
mean <- mean(data$OPEN)
var <- var(data$OPEN)
std <- sd(data$OPEN)
y <- c(data$OPEN)
n = length(y)-20
Cov <- c()
R <- c()
ARp <- function(p){
  for (k in 1:(p)){
    sk <- 0
    s0 <- 0
    for (i in (k+1):n){
      sk <- sk + (y[i]-mean)*(y[i-k]-mean)
    }
    for (i in (k+1):n){
      s0 <- s0 + (y[i]-mean)*(y[i]-mean)
    }
    sk <- sk / n
    s0 <- s0 / n
    R[k] <<- sk / s0
    Cov <<- append(Cov,sk)
  }
  R
  #print(Cov)
  # R <- c()
  # p = 5
  # for (k in 1:(p-1)){
  #   x <- y[(k+1):n]
  #   z <- y[1:(n-k)]
  #   cv <- cov(x,z)
  #   print(cv)
  # }
  # R
  
  Mat <- c()
  first <- c(1)
  for (i in 1:(p-1)){
    first <- append(first,R[i])
  }
  Mat <- rbind(Mat,first)
  for(i in 1:(p-1)){
    sec <- c(R[i])
    sec <- append(sec,first[1:p-1])
    first <- sec
    Mat <- rbind(Mat,first)
  }
  Mat
  Mat2 <- c()
  for (i in 1:p){
    Mat2 <- rbind(Mat2,R[i])
  }
  Mat2
  inverse <- inv(Mat)
  if(p==1){
    beta <<- R
  } 
  else {beta <<- inverse %*% Mat2}
}
beta <- c()
p = 1
ARp(p)
p
days <- 20
test_dec <- function(days){
  for (i in 1:days){
    x <- 0
    for( j in 1:length(beta)){
      x <- x + beta[j]*y[495+i-j-days] 
    }
    x <- x + rnorm(n = 1,mean = 0,sd = 1)
    dec[i] <<- x
  }
}
dec <- c()
test_dec(days)
dec2 <- c()
for (i in 1:495-days){
  dec2[i] <- y[i]
}
for (i in 1:days){
  dec2[495+i-days] <- dec[i] 
}
original_dec <- tail(data,n=days)
gg2 <- ggplot(original_dec, aes(x=TIMESTAMP, y=OPEN)) + geom_line()
gg2 <- gg2 + geom_line(aes(y=dec), colour="red")
gg2

# EVALUATING FORECASTING MODEL TO FIND ACCURATE P(ORDER) VALUE

y_actual <- y[(495-days+1):495]

RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

MAPE = function(actual,pred){
  mean(abs((actual-pred)/actual))*100
}

MAPE(y_actual,forecast)

p_arr <- c(9, 10, 11, 12, 13)
# p_arr <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
mape_arr <- c()
direction_score <- c()
for (i in p_arr){
  sum <- 0
  sum_dir <- 0
  for (j in 1:10){
    ARp(i)
    test_dec(days)
    sum <- sum + MAPE(y_actual,forecast)
    for(x in 2:days){
      if((y_actual[x]>y_actual[x-1]) & (forecast[x]>forecast[x-1])){
        sum_dir <- sum_dir + 1
      }
      if((y_actual[x]<=y_actual[x-1]) & (forecast[x]<=forecast[x-1])){
        sum_dir <- sum_dir + 1
      }
    }
  }
  mape_arr[i] <- sum/10
  direction_score[i] <- sum_dir/10
}
mape_arr <- mape_arr[!is.na(mape_arr)]
direction_score <- direction_score[!is.na(direction_score)]
mape_arr
direction_score

# Concluding that p=10 is the optimal order for AR model