## Modulos
#install.packages("vars")
library("vars")
#install.packages("urca")
library("urca")
#install.packages("BETS")
library("BETS")
#install.packages("dplyr")
library(dplyr)

### Dados
data <- read.csv2("D:\\07. UFOP\\Análise de Dados\\Trabalho Final\\data.csv",sep=";", header=T)

### Describe
summary(data)
#     selic           cambio        industria         inflacao      
# Min.   : 1.90   Min.   :1.563   Min.   : 66.10   Min.   :-0.3800  
# 1st Qu.: 8.04   1st Qu.:2.031   1st Qu.: 82.00   1st Qu.: 0.2700  
# Median :11.18   Median :2.597   Median : 86.50   Median : 0.4600  
# Mean   :11.67   Mean   :2.898   Mean   : 88.34   Mean   : 0.5158  
# 3rd Qu.:14.15   3rd Qu.:3.480   3rd Qu.: 97.60   3rd Qu.: 0.7100  
# Max.   :26.32   Max.   :5.651   Max.   :106.60   Max.   : 3.0200 

### Plot
plot(data$Inflacao, nc = 2, xlab = "")
plot(data$Industria, nc = 2, xlab = "")
plot(data$Selic, nc = 2, xlab = "")
plot(data$Cambio, nc = 2, xlab = "")

### Time Series
data <- ts(data, start = c(2002,1), frequency = 12)
data <- data[,2:5]
data[1:5,]

data[,1] <- abs(data[,1])
data

### Plot Data
plot(data, nc=2, xlab="", main = "Séries Temporais        ")


### Teste Dickey-Fuller
require(urca)
#Em Nível
s1selic <- summary(urca::ur.df(data[, "Selic"], type = "trend", lags = 2))
s2inflacao <- summary(urca::ur.df(data[, "Inflacao"], type = "trend", lags = 2))
s3cambio <- summary(urca::ur.df(data[, "Cambio"], type = "trend", lags = 2))
s4industria <- summary(urca::ur.df(data[, "Industria"], type = "trend", lags = 2))

cbind(t(s1selic@teststat), s1selic@cval)
#       statistic  1pct  5pct 10pct
# tau3 -4.059975 -3.99 -3.43 -3.13
# phi2  5.629303  6.22  4.75  4.07
# phi3  8.441301  8.43  6.49  5.47

cbind(t(s2inflacao@teststat), s2inflacao@cval)
#       statistic  1pct  5pct 10pct
# tau3 -5.111385 -3.99 -3.43 -3.13
# phi2  8.810786  6.22  4.75  4.07
# phi3 13.181509  8.43  6.49  5.47

cbind(t(s3cambio@teststat), s3cambio@cval)
#       statistic  1pct  5pct 10pct
# tau3 -1.426812 -3.99 -3.43 -3.13
# phi2  1.338239  6.22  4.75  4.07
# phi3  1.499807  8.43  6.49  5.47

cbind(t(s4industria@teststat), s4industria@cval)
#       statistic  1pct  5pct 10pct
# tau3 -2.322179 -3.99 -3.43 -3.13
# phi2  1.986529  6.22  4.75  4.07
# phi3  2.955213  8.43  6.49  5.47

#First Diferrence
adf1 <-summary(ur.df(diff(data[, "Selic"]), type = "trend", lags = 2))
adf2 <-summary(ur.df(diff(data[, "Inflacao"]), type = "drift", lags = 2))
adf3 <-summary(ur.df(diff(data[, "Cambio"]), type = "trend", lags = 2))
adf4 <-summary(ur.df(diff(data[, "Industria"]), type = "trend", lags = 2))

adf1
adf2
adf3
adf4


### Metodo First Diferrence
dados <- diff(log(data))
dados <- diff(dados)
summary(dados)
# inflacao            selic               cambio            industria        
# Min.   :    -Inf   Min.   :-0.192787   Min.   :-0.100083   Min.   :-0.164095  
# 1stQu. :-0.35810   1stQu.:-0.025745    1stQu. :-0.024284   1stQu. :-0.015021  
# Median : 0.03381   Median : 0.000000   Median :-0.002784   Median : 0.001985  
# Mean   :     NaN   Mean   :-0.002213   Mean   : 0.003046   Mean   : 0.000523  
# 3rdQu. : 0.42488   3rdQu.: 0.015255    3rdQu. : 0.024897   3rQu.  : 0.016908  
# Max.   :     Inf   Max.   : 0.216328   Max.   : 0.188575   Max.   : 0.090085  
dados[,1][dados[,1] == "-Inf"] <- 0
dados[,1][dados[,1] == "Inf"] <- 0
summary(dados)

### Modelo Var
vars::VARselect(dados, lag.max = 15, type = "const")

model <- vars::VAR(dados, p = 10, type = "const")

### Raizes do Modelo
sum.model <- summary(model)
sum.model[["roots"]]

### Autocorrelação
#Serial
vars::serial.test(model, lags.pt=16, type="PT.asymptotic")$serial
# data:  Residuals of VAR object model
# Chi-squared = 171.33, df = 160, p-value = 0.2559
vars::serial.test(model, lags.pt=16, type="PT.adjusted")$serial
# data:  Residuals of VAR object model
# Chi-squared = 162.69, df = 160, p-value = 0.4259

#Normalidade
normality.test(model)$jb.mul$JB

#Hetorocedasticidade
arch.test(model, lags.multi = 5)$arch.mul

### Matriz de Variância
sum.model <- summary(model)
sum.model$covres
#               inflacao         selic        cambio     industria
# inflacao   0.5799326072 -5.416240e-04 -4.699262e-04 -0.0001566664
# selic     -0.0005416240  6.581320e-04 -5.208742e-05  0.0001800438
# cambio    -0.0004699262 -5.208742e-05  1.380538e-03 -0.0002856314
# industria -0.0001566664  1.800438e-04 -2.856314e-04  0.0010383598

# Elementos da diagonal = variância
# Elementos fora da diagonal = covariância
# Matriz simetrica 

# Elementos fora da diagonal podemos supor que há correlação comteporânea

### Matriz de Correlação
sum.model$corres
#            inflacao       selic      cambio    industria
# inflacao   1.000000000 -0.02772379 -0.01660798 -0.006384297
# selic     -0.027723794  1.00000000 -0.05464521  0.217794942
# cambio    -0.016607976 -0.05464521  1.00000000 -0.238565623
# industria -0.006384297  0.21779494 -0.23856562  1.000000000

### Decomposição Choleski - Choque 
t(chol(sum.model$covres))
#            inflacao        selic       cambio  industria
# inflacao   0.7615330638  0.000000000  0.000000000 0.00000000
# selic     -0.0007112285  0.025644223  0.000000000 0.00000000
# cambio    -0.0006170792 -0.002048270  0.037093959 0.00000000
# industria -0.0002057250  0.007015128 -0.007316272 0.03058721

# Inflacao = 0.8679 (inflacao) 
# Selic = -0.0012 (inflacao) + 0.0249 (Selic)
# Cambio = 0.0010 (inflacao) -0.0025 (Selic) +  0.0370 (Cambio)
# Industria = -0.0011 (inflacao) + 0.0046 (Selic) +  -0.0059 (Cambio) + 0.0321 (Industria  )

### Função Impulso-Resposta
fir.model <- vars::irf(model, n.ahead = 12, boot = TRUE, ortho = TRUE)
plot(fir.model)

### Estatísticas
sum.model 

### Impulso da Cambio e Resposta da Inflação
oir <- vars::irf(model, impulse = "Cambio", response = "Inflacao",
                 n.head = 12, ortho = TRUE, runs = 1000, seed = 12345)
plot(oir)

  