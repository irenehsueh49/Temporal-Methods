---
title: "Irene Hsueh's BS 728 Homework 5"
author: "Irene Hsueh"
date: "12/14/2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


### Question 1
```{r}
deaths <- read.csv("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 728 - Public Health Surveillance/Class 5 - Temporal Methods I/Homework 5/deaths.csv") 
head(deaths, 10)

deaths_decomposition <- ts(deaths$num, freq=12)
plot(decompose(deaths_decomposition))

n_months <- nrow(deaths)
```


### Question 2 
```{r}
plot(deaths$num,
     type="b", 
     pch=20, 
     xlab="Year", 
     ylab="Number of Deaths", 
     main="Number of Monthly Accidental Deaths from 1973-1978",
     xaxt="n")
axis(side=1, 
     at=c(0,12,24,36,48,60,72),
     labels=c("1972","1973","1974","1975","1976","1977","1978"))
```



### Question 3
```{r}
plot(deaths$num ~ as.factor(deaths$num.month), 
     col="hotpink", 
     xlab="Month", 
     ylab="Number of Deaths",
     main="Boxplots for Each Month")
```



### Basic Linear Model
```{r}
linear_model <- lm(num ~ num.month, data=deaths)
summary(linear_model)
linear_model_residuals <- linear_model$residuals

plot(linear_model$residuals, 
     pch=20,
     col="hotpink", 
     type="b", 
     xlab="Month",
     ylab="Linear Model Residuals")
abline(h=0)
```



### Modeling Seasonality
```{r}
#Periods 
annual_period <- 12
semiannual_period <- 6

#Sinusoid Terms
annual_sin <- sin(c(1:n_months)*2*pi/annual_period)
annual_cos <- cos(c(1:n_months)*2*pi/annual_period)
semiannual_sin <- sin(c(1:n_months)*2*pi/semiannual_period)
semiannual_cos <- cos(c(1:n_months)*2*pi/semiannual_period)
```



## Model of Annual Period 
```{r}
annual_model <- lm(deaths$num ~ annual_sin + annual_cos)
summary(annual_model)

#Fitted Values vs Data
plot(deaths$num, 
     type="l", 
     xlab="Month", 
     ylab="Linear Model Residuals", 
     main="Fitted Values of Annual Model vs Data")
lines(annual_model$fitted.values, col="hotpink", lwd=3)

#Residual Plot
plot(annual_model$residuals, 
     pch=20, 
     col="hotpink", 
     type="b", 
     xlab="Month", 
     ylab="Residuals", 
     main="Annual Model Residuals")
abline(h=0)

#ACF of Residuals
acf(annual_model$residuals, main="ACF of Annual Model Residuals")
```

## Model of Annual and Semiannual Periods
```{r}
semiannual_model <- lm(deaths$num ~ annual_sin + annual_cos + semiannual_sin + semiannual_cos)
summary(semiannual_model)


#Fitted Values vs Data
plot(deaths$num, 
     type="l", 
     xlab="Month", 
     ylab="Linear Model Residuals", 
     main="Fitted Values of Semiannual Model vs Data")
lines(semiannual_model$fitted.values, col="hotpink", lwd=3)

#Residual Plot
plot(semiannual_model$residuals, 
     pch=20, 
     col="hotpink", 
     type="b", 
     xlab="Month", 
     ylab="Residuals", 
     main="Semiannual Model Residuals")
abline(h=0)

#ACF of Residuals
acf(semiannual_model$residuals, main="ACF of Semiannual Model Residuals")
```



### Question 4 - Modeling Trend 
```{r}
deaths$time_squared <- (deaths$time)^2

#Linear Model
linear_trend <- lm(annual_model$residuals ~ deaths$time)
summary(linear_trend)

plot(linear_trend$residuals, 
     pch=20, 
     col="hotpink", 
     type="b", 
     xlab="Month", 
     ylab="Residuals", 
     main="Linear Trend Model Residuals")
abline(h=0)


#Quadratic Model
quadratic_trend <- lm(annual_model$residuals ~ deaths$time + deaths$time_squared)
summary(quadratic_trend)  

plot(quadratic_trend$residuals, 
     pch=20, 
     col="hotpink", 
     type="b", 
     xlab="Month", 
     ylab="Residuals", 
     main="Quadratic Trend Model Residuals")
abline(h=0)


#Annual Model Residuals vs Linear Model Fitted Values
plot(annual_model$residuals, 
     type="l", 
     xlab="Month", 
     ylab="Annual Model Residuals", 
     main="Annual Model Residuals vs Linear Model Fitted Values")
lines(quadratic_trend$fitted.values, col="hotpink", lwd=3)
```



### Question 5 - Investigating Autocorrelation 
```{r}
#ACF of Quadratic Trend Residuals 
acf(quadratic_trend$residuals)
```


### MA Model Order 6
```{r}
ma_order6 <- arima(deaths$num, order=c(0,0,6))


#Significance Tests
ma_order6_zscores <- ma_order6$coef / sqrt(diag(ma_order6$var.coef))
ma_order6_pvalues <- 2*(1-pnorm(abs(ma_order6_zscores)))


#Residual Plot
plot(ma_order6$residuals, 
     pch=20, 
     col="hotpink", 
     type="b", 
     xlab="Month", 
     ylab="Residuals", 
     main="Moving Average Order 6 Residuals")
abline(h=0)


ma_order6
ma_order6_pvalues
```

### MA Model Order 12
```{r}
ma_order12 <- arima(deaths$num, order=c(0,0,12), 
                   xreg=cbind(deaths$num.month, annual_sin, annual_cos))


#Significance Tests
ma_order12_zscores <- ma_order12$coef / sqrt(diag(ma_order12$var.coef))
ma_order12_pvalues <- 2*(1-pnorm(abs(ma_order12_zscores)))


#Residual Plot
plot(ma_order12$residuals, 
     pch=20, 
     col="hotpink", 
     type="b", 
     xlab="Month", 
     ylab="Residuals", 
     main="Moving Average Order 12 Residuals")
abline(h=0)


ma_order12
ma_order12_pvalues
```






