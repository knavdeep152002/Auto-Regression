#IMPORTING LIBRARIES

library(ggplot2)
library(matlib)

#READ DATA AND PLOT TIME SERIES

dataset <- read.csv("E:/IDA/Proj/FINAL_FROM_DF.csv")
dataset$TIMESTAMP = as.Date(dataset$TIMESTAMP)
syms <- unique(dataset$SYMBOL)[0:1]
syms
data <- dataset[dataset$SYMBOL==syms, c("TIMESTAMP","OPEN")]
data <- data[!is.na(data$OPEN),]      #dropna to remove NA values
data <- data[order(data$TIMESTAMP),]

gg <- ggplot(data, aes(x=TIMESTAMP, y=OPEN)) + geom_line()  #create time series plot
gg

#AR MODEL

mean <- mean(data$OPEN)
var <- var(data$OPEN)
std <- sd(data$OPEN)
y <- c(data$OPEN)
days <- 20 #for predicting december days <- 20
n = length(y)-days
Cov <- c()
R <- c()
beta <- c()
ARp <- function(p){
  Cov <<- c()
  R <<- c()
  beta <<- c()
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

p_test = 11
ARp(p_test)
test_dec <- function(days){
  for (i in 1:days){
    x <- 0
    for( j in 1:length(beta)){
      x <- x + beta[j]*y[495+i-j-days] 
    }
    x <- x + rnorm(n = 1,mean = 0,sd = 1)
    forecast[i] <<- x
  }
}
forecast <- c()
test_dec(days)
dec2 <- c()
for (i in 1:495-days){
  dec2[i] <- y[i]
}
for (i in 1:days){
  dec2[495+i-days] <- forecast[i]
}
original_dec <- tail(data,n=days)

# GRAPHS AT MULTIPLE SCALES

gg2 <- ggplot(original_dec, aes(x=TIMESTAMP, y=OPEN)) + geom_line()
gg2 <- gg2 + geom_line(aes(y=forecast), colour="red")
gg2
# gg <- gg + geom_line(aes(y=dec2), colour="red")
# gg <- gg + geom_line(aes(y=OPEN), colour="blue")
# gg

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

