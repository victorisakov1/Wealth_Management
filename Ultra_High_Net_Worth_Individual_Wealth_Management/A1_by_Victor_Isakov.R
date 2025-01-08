#==============================================================================#
#==============================================================================#
#                                                                              #
#                       A1 Assignment by Victor Isakov                         #
#                                                                              #
#             Recommendations and Advice for Ultra High Net Worth Client       #
#                                                                              #
#==============================================================================#
#==============================================================================#




#Importing Libraries
library(readr)
library(quantmod)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(data.table)
library(quadprog)
library(tseries)
library(stats)




#Getting Data for required asset tickers
s1_returns <- monthlyReturn(getSymbols("IXN", auto.assign = F)) 
s2_returns <- monthlyReturn(getSymbols("QQQ", auto.assign = F))
s3_returns <- monthlyReturn(getSymbols("IEF", auto.assign = F))
s4_returns <- monthlyReturn(getSymbols("VNQ", auto.assign = F))
s5_returns <- monthlyReturn(getSymbols("GLD", auto.assign = F))




#Merging Returns
joined_monthly <- merge.xts(s1_returns,
                            s2_returns,
                            s3_returns,
                            s4_returns,
                            s5_returns)

#Benchmark
bnchmrk_returns <- monthlyReturn(getSymbols("VONE", auto.assign = F))

joined_monthly <- merge.xts(joined_monthly,
                            bnchmrk_returns)




################################################################################
####                  Question 1               #################################
################################################################################




#Calculating monthly returns

time_index <- nrow(joined_monthly) 

#Establishing % allocation of each asset (weight)

#Original Weights
#s1_w <- 0.175
#s2_w <- 0.221
#s3_w <- 0.285
#s4_w <- 0.089
#s5_w <- 0.23



#Suggested Weights for better performance
s1_w <- 0.03828932      
s2_w <- 0.50000000      
s3_w <- 0  
s4_w <- 0
s5_w <- 0.46171068  




################################################################################
#12 Months
s1_exp_12 <- mean(joined_monthly$monthly.returns[time_index:(time_index-11)])
s2_exp_12 <- mean(joined_monthly$monthly.returns.1[time_index:(time_index-11)])
s3_exp_12 <- mean(joined_monthly$monthly.returns.2[time_index:(time_index-11)])
s4_exp_12 <- mean(joined_monthly$monthly.returns.3[time_index:(time_index-11)])
s5_exp_12 <- mean(joined_monthly$monthly.returns.4[time_index:(time_index-11)])


#Multiplying returns with weights to get Portfolio returns
joined_monthly <- as.data.frame(joined_monthly) %>%
  mutate(portfolio_ret = s1_w * monthly.returns + 
                         s2_w * monthly.returns.1 +
                         s3_w * monthly.returns.2 + 
                         s4_w * monthly.returns.3 + 
                         s5_w * monthly.returns.4)


pf_exp <- mean(joined_monthly$portfolio_ret[time_index:(time_index-11)]) 


print(s1_exp_12*100) #Multiply by 100 to see actual percentage
print(s2_exp_12*100)
print(s3_exp_12*100)
print(s4_exp_12*100)
print(s5_exp_12*100)
print(pf_exp*100)

#> 12 Months Returns for assets are the following:
#> IXN = 2.76%
#> QQQ = 2.25%
#> IEF = -0.04%
#> VNQ = 0.39%
#> GLD = 1.76%
#> Portfolio = 1.41%




################################################################################
#18 Months
s1_exp_18 <- mean(joined_monthly$monthly.returns[time_index:(time_index-17)])
s2_exp_18 <- mean(joined_monthly$monthly.returns.1[time_index:(time_index-17)])
s3_exp_18 <- mean(joined_monthly$monthly.returns.2[time_index:(time_index-17)])
s4_exp_18 <- mean(joined_monthly$monthly.returns.3[time_index:(time_index-17)])
s5_exp_18 <- mean(joined_monthly$monthly.returns.4[time_index:(time_index-17)])


pf_exp_18 <- mean(joined_monthly$portfolio_ret[time_index:(time_index-17)]) 


print(s1_exp_18*100) #Multiply by 100 to see actual percentage
print(s2_exp_18*100)
print(s3_exp_18*100)
print(s4_exp_18*100)
print(s5_exp_18*100)
print(pf_exp_18*100)


#> 18 Months Returns for assets are the following:
#> IXN = 3.26%
#> QQQ = 3.02%
#> IEF = -0.22%
#> VNQ = -0.08%
#> GLD = 1.29%
#> Portfolio = 1.47%




################################################################################
#24 Months
s1_exp_24 <- mean(joined_monthly$monthly.returns[time_index:(time_index-23)])
s2_exp_24 <- mean(joined_monthly$monthly.returns.1[time_index:(time_index-23)])
s3_exp_24 <- mean(joined_monthly$monthly.returns.2[time_index:(time_index-23)])
s4_exp_24 <- mean(joined_monthly$monthly.returns.3[time_index:(time_index-23)])
s5_exp_24 <- mean(joined_monthly$monthly.returns.4[time_index:(time_index-23)])


pf_exp_24 <- mean(joined_monthly$portfolio_ret[time_index:(time_index-23)]) 


print(s1_exp_24*100) #Multiply by 100 to see actual percentage
print(s2_exp_24*100)
print(s3_exp_24*100)
print(s4_exp_24*100)
print(s5_exp_24*100)
print(pf_exp_24*100)


#> 24 Months Returns for assets are the following:
#> IXN = 2.39%
#> QQQ = 2.06%
#> IEF = -0.39%
#> VNQ = -0.32%
#> GLD = 1.37%
#> Portfolio = 1.05%
#> 
#> IXN and QQQ have showed consistently higher returns compared to other assets,
#> while IEF and VNQ have showed negative returns in most cases.
#> Overall, portfolio returns are low and further balancing could be beneficial.




################################################################################
####                  Question 2               #################################
################################################################################




#Calculating Correlations
cor_12 <- cor(joined_monthly[time_index:(time_index-11),])
cor_18 <- cor(joined_monthly[time_index:(time_index-17),])
cor_24 <- cor(joined_monthly[time_index:(time_index-23),])


print(cor_12)
print(cor_18)
print(cor_24)


#> GLD has the lowest correlations with all other assets, which means it could 
#> be helpful for the diversification of assets.
#> Moreover, all other assets have high correlations with each other which is 
#> interesting, because they are from different classes and industries.




################################################################################
####                  Question 3               #################################
################################################################################




#Calculating Sigmas for the past year
s1_sigma <- sd(joined_monthly$monthly.returns[time_index:(time_index-11)])*sqrt(12) 
s2_sigma <- sd(joined_monthly$monthly.returns.1[time_index:(time_index-11)])*sqrt(12)
s3_sigma <- sd(joined_monthly$monthly.returns.2[time_index:(time_index-11)])*sqrt(12)
s4_sigma <- sd(joined_monthly$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)
s5_sigma <- sd(joined_monthly$monthly.returns.4[time_index:(time_index-11)])*sqrt(12)


pf_sigma <- sd(joined_monthly$portfolio_ret[time_index:(time_index-11)])*sqrt(12) 


print(s1_sigma*100) #Multiplying by 100 to get percentage
print(s2_sigma*100)
print(s3_sigma*100)
print(s4_sigma*100)
print(s5_sigma*100)
print(pf_sigma*100)


#> Last year sigmas (risks) for assets are the following:
#> IXN = 20.35%
#> QQQ = 16.8%
#> IEF = 8.6%
#> VNQ = 21.8%
#> GLD = 12.91%
#> Portfolio = 12%
#> 
#> Most risky assets are VNQ, IXN and QQQ. They should have higher returns to
#> be worth of investments.
#> On the other hand, least risky assets are IEF and GLD, which makes sense 
#> because bonds and Gold should have lower risks.
#> Overall, Portfolio risk is around 12% which is a good risk overall.
#> In order to make any conclusions, Sharpe Ratio needs to be calculated and
#> further adjustments of weights should be done.




################################################################################
####                  Question 4               #################################
################################################################################




#Getting Sharpe Ratio for Assets and Portfolio
#Starting with Risk-free rate
risk_free <- 0.0001


s1_sharpe <- (((1+s1_exp_12)^12)-1 - risk_free)/s1_sigma
s2_sharpe <- (((1+s2_exp_12)^12)-1 - risk_free)/s2_sigma
s3_sharpe <- (((1+s3_exp_12)^12)-1 - risk_free)/s3_sigma
s4_sharpe <- (((1+s4_exp_12)^12)-1 - risk_free)/s4_sigma
s5_sharpe <- (((1+s5_exp_12)^12)-1 - risk_free)/s5_sigma


pf_sharpe <- (((1+pf_exp)^12)-1 - risk_free)/pf_sigma


print(s1_sharpe)
print(s2_sharpe)
print(s3_sharpe)
print(s4_sharpe)
print(s5_sharpe)
print(pf_sharpe)


#> Sharpe Ratios for assets and portfolio are the following:
#> IXN = 1.89
#> QQQ = 1.82
#> IEF = -0.06
#> VNQ = 0.22
#> GLD = 1.8
#> Portfolio = 1.52
#> 
#> It suggests, that assets IXN, QQQ, and GLD should be increased in amount
#> due to their highest Sharpe Ratios.
#> While IEF and VNQ should be minimized due to lowest Sharpe Ratios.
#> So Buying IXN, QQQ, and GLD looks like a good decision, 
#> and Selling IEF and VNQ is going to help too.
#> 
#> But before that, let's try re-balancing the portfolio:




################################################################################
####                  Question 5               #################################
################################################################################




enddate <- "2024-7-13"
t<-1985 
myvector <- c()
nstocks <- 3
pricinglist <- as.data.frame(matrix(ncol=nstocks, nrow=t))
colnames(pricinglist) <- c("IXN", "QQQ", "GLD")


#the pricinglist data starts form 2016-8-22 - this is the first row
for (i in 1:(ncol(pricinglist))){
  current_ticker <- colnames(pricinglist)[i]
  newtable <- getSymbols(current_ticker, src = "yahoo", from="2016-8-22", to=enddate, auto.assign=FALSE)
  pricinglist[,i] <- newtable[,6]
}


#forecasting the next price using a backpropagation training algorithm in a neural network. 
# a Autoregressive Model of fourth order AR4 was used.
#those weight will give you the highest sharpe ratio 
newpricingdataset <- pricinglist


#creating a dataset with monthly ROR for each day using continuous compounding
dailyROR <- as.data.frame(matrix(ncol=ncol(newpricingdataset), nrow=nrow(newpricingdataset)-25))
colnames(dailyROR) <- colnames(pricinglist)
for (c in 1:(ncol(newpricingdataset))){
  for (r in 1:(nrow(newpricingdataset)-25)){
    dailyROR[r,c] <- log(as.numeric(newpricingdataset[(r+25),c])/as.numeric(newpricingdataset[r,c]))
  }
}


#The most current expected return for n+25 (n is today) is in the last row of the above dataset
#calculating Expected(R) for all securities 
averet <- as.matrix(dailyROR[nrow(dailyROR),], nrow=1)


#calculating covariance matrix
rcov <- cov(dailyROR[(nrow(dailyROR)-125):(nrow(dailyROR)),]) #125 stands for 6 trading months
target.r <- 1/1000


#using solver to get to optimal weights
effFrontier = function(averet, rcov, nports, shorts, wmax, wmin)
{
  mxret <- max(averet)
  mnret <- -mxret
  n.assets <- ncol(averet)
  reshigh <- rep(wmax, n.assets)
  reslow <- rep(wmin, n.assets)
  min.rets <- seq(mnret, mxret, length.out=nports)
  vol <- rep(NA, nports)
  ret <- rep(NA, nports)
  pw <- data.frame(matrix(ncol=nports, nrow=n.assets))
  for (i in 1:nports)
  {
    port.sol <- NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[i], covmat=rcov,   reshigh = reshigh, reslow= reslow, shorts=F)
        , silent=T)
    if(!is.null(port.sol))
    {
      vol[i] <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[i] <- averet %*% port.sol$pw
      pw[,i] <- port.sol$pw
    }
  }
  return(list(vol=vol, ret = ret, weights = pw))
  
}

maxSharpe <- function(averet, rcov, shorts=F, wmax=0.2, min.weight=0.01)
{
  optim.callback=function(param, averet, rcov, reshigh, reslow, shorts)
  { 
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=param, covmat=rcov,
                                    reshigh=reshigh, reslow=reslow, shorts=shorts),silent=T)
    if(is.null(port.sol)) { ratio= 10^9} else 
    {
      m.return <- averet %*% port.sol$pw
      m.risk <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ratio <- m.return/m.risk
      assign("w", port.sol$pw, inherits=T)
    }
    return(ratio)
  }
  
  ef <- effFrontier(averet=averet, rcov=rcov, shorts=shorts, wmax=wmax, nports = 100, wmin=min.weight)
  n <- ncol(averet)
  reshigh <- rep(wmax, n)
  reslow <- rep(min.weight, n)
  
  max.sh <- which.max(ef$ret/ef$vol)
  
  
  if(is.na(ef$ret[max.sh-1])){lowerinterval<-ef$ret[max.sh]}else{lowerinterval <- ef$ret[max.sh-1]}
  if(is.na(ef$ret[max.sh+1])){upperinterval<-ef$ret[max.sh]}else{upperinterval <- ef$ret[max.sh+1]}
  
  w <- rep(0, ncol(averet))
  xmin <- optimize(f=optim.callback, interval = c(lowerinterval, upper=upperinterval), 
                   averet=averet, rcov=rcov, reshigh=reshigh, reslow=reslow, shorts=shorts)
  return(w)
  return(xmin)
}

z <- maxSharpe(averet, rcov, shorts=F, wmax=0.5)

print(z)


#> After Re-balancing the Portfolio, changing weights, and practically removing
#> assets IEF and VNQ, most parameters improved.
#> 
#> Expected Returns: From 1.39% to 2.04%. (Which is better by 47%)
#> Risks (sigma): From 12% to 11.37%. (Which is slightly better)
#> Sharpe Ratio: From 1.5 to 2.413 (Which is a significant boost by 61%)
#> 
#> Overall, these adjustments will be beneficial for our client.




################################################################################
####                  Question 6               #################################
################################################################################




#################################
#### Three risky assets portfolio
#################################
# load the data
ticker1_select <- "IXN" 
ticker2_select <- "QQQ"
ticker3_select <- "GLD"


mydf1 <- as.data.frame(monthlyReturn(getSymbols(ticker1_select, auto.assign=FALSE)))
mydf2 <- as.data.frame(monthlyReturn(getSymbols(ticker2_select, auto.assign=FALSE)))
mydf3 <- as.data.frame(monthlyReturn(getSymbols(ticker3_select, auto.assign=FALSE)))


combined_df <- cbind(mydf1[,1], mydf2[,1], mydf3[,1])
df <- as.data.frame(combined_df)
colnames(df) <- c("x","y", "z")
df$date = as.Date(rownames(mydf1))


# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)


# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)


# III) covariance
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)


# create portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)


# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))


three_assets[, wz := 1 - wx - wy]


# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]


# take out cases where we have negative weights (shortselling)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets


# lastly plot the values
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)



#> The output of the Efficient Frontier states the benefits of diversification,
#> which will be helpful for investors to have higher return per unit of risk.
#> We can see a "Christmas Effect" where two assets improve returns while one 
#> other one decreases the risks.
#> Moreover, investors should define their Risk Tolerance in order to define
#> the weight of investments to each asset. The will follow their goals and 
#> bring greater results.




################################################################################
####                 Further Analysis                    #######################
####                                                     #######################
####    Working Only with 3 Chosen Assets + Benchmark    #######################
################################################################################





####========================================================================####
####                  Tracking Error for the Last 12 months                 ####
####========================================================================####




IXN_te <- sd(joined_monthly$monthly.returns[time_index:(time_index-11)]-
               joined_monthly$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)
QQQ_te <- sd(joined_monthly$monthly.returns.1[time_index:(time_index-11)]-
               joined_monthly$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)
GLD_te <- sd(joined_monthly$monthly.returns.4[time_index:(time_index-11)]-
               joined_monthly$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)


print(IXN_te)
print(QQQ_te)
print(GLD_te)


#> The following Tracking Errors show how closely to VONE (benchmark here) they
#> perform.
#> 
#> QQQ has TE of 4.24% which is insignificant and means that this asset is
#> consistent with mimicking the market.
#> 
#> IXN has TE of 8.2% which is medium and means that assets mostly follows 
#> the market trend with slight deviations.
#> 
#> GLD has the highest TE of 18% which means it deviates the most,
#> probably because it has a direct focus on Gold only.
#> 
#> This code is flexible as a different Benchmark asset can be set based on the
#> investor's risk tolerance and investment objectives.




####========================================================================####
####                              CAPM Model                                ####
####========================================================================####




#Creating Quasi CAPM models
last_12_months <- joined_monthly[time_index:(time_index-11),]


#Building CAPM
IXN_reg  <- lm(monthly.returns~monthly.returns.5, data = last_12_months)
QQQ_reg <- lm(monthly.returns.1~monthly.returns.5, data = last_12_months)
GLD_reg  <- lm(monthly.returns.4~monthly.returns.5, data = last_12_months)


#Getting Details
summary(IXN_reg)
summary(QQQ_reg)
summary(GLD_reg)


#> IXN has R-squared of 0.86 which means that it's returns can be mostly
#> explained by market (VONE). Furthermore, it has a Beta of 1.27 and it means
#> that for every unit of VONE increase, IXN will increase by 1.27 units.
#> In other words, IXN is more risky than the market.
#> 
#> QQQ has similar results. R-Squared is 0.94 which states a statistical
#> significance and that returns can be mostly explained by market trends.
#> Moreover, Beta is 1.09 which means that for every unit increase in VONE,
#> QQQ will increase by 1.09. In other terms, this asset is slightly riskier
#> than the market.
#> 
#> GLD has a -0.06 R-Squared which states randomness of movements compared to 
#> the market. Moreover, Beta is 0.15 which means that for every 1 unit
#> increase in market value - GLD will increase by 0.15 units. Or that it is
#> much less risky compared to VONE.
#> 
#> All these details help to evaluate chosen assets compared to market.
#> In the future, it helps to understand assets sensitivity to the market.




####========================================================================####
####                             FAMA FRENCH Model                          ####
####========================================================================####




#Fama French Model
source("C://Users//Mi//Desktop//My Files//1-MBAN//Summer II//Wealth Management//Classes//8.2 Fama French predictions and residuals - source this file before running the next one.R")


#calling the Fama French 3F model UDF for IXN
IXN_FF3F <- fama_french_3F_pred_res(ticker="IXN", from_date='2020-01-02', to_date='2024-07-13')
IXN_FF3F$actuals
IXN_FF3F$model_pred


#####               More predictions are wrong than right                  #####




#calling the Fama French 3F model UDF for QQQ
QQQ_FF3F <- fama_french_3F_pred_res(ticker="QQQ", from_date='2020-01-02', to_date='2024-07-13')
QQQ_FF3F$actuals
QQQ_FF3F$model_pred


#####               More predictions are wrong than right                  #####




#calling the Fama French 3F model UDF for GLD
GLD_FF3F <- fama_french_3F_pred_res(ticker="GLD", from_date='2020-01-02', to_date='2024-07-13')
GLD_FF3F$actuals
GLD_FF3F$model_pred


#####               More predictions are wrong than right                  #####




#> As a result of running Fama French Model and Linear Regression - neither of 
#> the assets price direction was predicted correctly. Most of the predictions
#> were wrong, which means that we cannot use it for potential forecasting
#> or future assessments regarding our current assets.
