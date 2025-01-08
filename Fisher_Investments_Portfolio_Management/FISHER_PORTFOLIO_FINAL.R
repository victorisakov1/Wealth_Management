#let's call the tools for the analysis

library(quantmod) 
library(dplyr)

#Selecting the top 25 stock of the portfolio (weight 49% of the entire pf)

stock1_returns <- monthlyReturn(getSymbols("MSFT", auto.assign = F)) 
stock2_returns <- monthlyReturn(getSymbols("AAPL", auto.assign = F))
stock3_returns <- monthlyReturn(getSymbols("NVDA", auto.assign = F))
stock4_returns <- monthlyReturn(getSymbols("AMZN", auto.assign = F))
stock5_returns <- monthlyReturn(getSymbols("GOOGL", auto.assign = F))
stock6_returns <- monthlyReturn(getSymbols("VCIT", auto.assign = F))
stock7_returns <- monthlyReturn(getSymbols("AMD", auto.assign = F))
stock8_returns <- monthlyReturn(getSymbols("TSM", auto.assign = F))
stock9_returns <- monthlyReturn(getSymbols("LLY", auto.assign = F))
stock10_returns <- monthlyReturn(getSymbols("HD", auto.assign = F))
#stock11_returns <- monthlyReturn(getSymbols("CRM", auto.assign = F))
#stock12_returns <- monthlyReturn(getSymbols("XOM", auto.assign = F)) 
#stock13_returns <- monthlyReturn(getSymbols("CAT", auto.assign = F))
#stock14_returns <- monthlyReturn(getSymbols("ASML", auto.assign = F))
#stock15_returns <- monthlyReturn(getSymbols("AVGO", auto.assign = F))
#stock16_returns <- monthlyReturn(getSymbols("CVX", auto.assign = F))
#stock17_returns <- monthlyReturn(getSymbols("META", auto.assign = F))
#stock18_returns <- monthlyReturn(getSymbols("PG", auto.assign = F))
#stock19_returns <- monthlyReturn(getSymbols("FCX", auto.assign = F))
#stock20_returns <- monthlyReturn(getSymbols("WMT", auto.assign = F))
#stock21_returns <- monthlyReturn(getSymbols("V", auto.assign = F))
#stock22_returns <- monthlyReturn(getSymbols("JPM", auto.assign = F))
#stock23_returns <- monthlyReturn(getSymbols("GS", auto.assign = F))
#stock24_returns <- monthlyReturn(getSymbols("ADBE", auto.assign = F)) 
#stock25_returns <- monthlyReturn(getSymbols("ORCL", auto.assign = F))

#Let's  merge all these stocks return

joined_monthly_fisherpf <- merge.xts(stock1_returns,
                                     stock2_returns,
                                     stock3_returns,
                                     stock4_returns,
                                     stock5_returns,
                                     stock6_returns,
                                     stock7_returns,
                                     stock8_returns,
                                     stock9_returns,
                                     stock10_returns
)


#Let's calculate the sigma for the stocks!
#Setting the time index

time_index <- nrow(joined_monthly_fisherpf) #210 months so -12 months = 199


stock1_sigma <- sd(joined_monthly_fisherpf$monthly.returns[time_index:(time_index-11)])*sqrt(12) 
stock2_sigma <- sd(joined_monthly_fisherpf$monthly.returns.1[time_index:(time_index-11)])*sqrt(12)
stock3_sigma <- sd(joined_monthly_fisherpf$monthly.returns.2[time_index:(time_index-11)])*sqrt(12)
stock4_sigma <- sd(joined_monthly_fisherpf$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)
stock5_sigma <- sd(joined_monthly_fisherpf$monthly.returns.4[time_index:(time_index-11)])*sqrt(12)
stock6_sigma <- sd(joined_monthly_fisherpf$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)
stock7_sigma <- sd(joined_monthly_fisherpf$monthly.returns.6[time_index:(time_index-11)])*sqrt(12)
stock8_sigma <- sd(joined_monthly_fisherpf$monthly.returns.7[time_index:(time_index-11)])*sqrt(12)
stock9_sigma <- sd(joined_monthly_fisherpf$monthly.returns.8[time_index:(time_index-11)])*sqrt(12)
stock10_sigma <- sd(joined_monthly_fisherpf$monthly.returns.9[time_index:(time_index-11)])*sqrt(12)

#Let's calculate the sigma for the entire pf

pf_sigma <- sd(joined_monthly_fisherpf$portfolio_ret[time_index:(time_index-11)])*sqrt(12) 

#Now we set the risk free rate as discussed in class

Risk_Free <- 0.0001

#let's calulcate expected returns

s1_exp <- mean(joined_monthly_fisherpf$monthly.returns[time_index:(time_index-11)])
s2_exp <- mean(joined_monthly_fisherpf$monthly.returns.1[time_index:(time_index-11)])
s3_exp <- mean(joined_monthly_fisherpf$monthly.returns.2[time_index:(time_index-11)])
s4_exp <- mean(joined_monthly_fisherpf$monthly.returns.3[time_index:(time_index-11)])
s5_exp <- mean(joined_monthly_fisherpf$monthly.returns.4[time_index:(time_index-11)])
s6_exp <- mean(joined_monthly_fisherpf$monthly.returns.5[time_index:(time_index-11)])
s7_exp <- mean(joined_monthly_fisherpf$monthly.returns.6[time_index:(time_index-11)])
s8_exp <- mean(joined_monthly_fisherpf$monthly.returns.7[time_index:(time_index-11)])
s9_exp <- mean(joined_monthly_fisherpf$monthly.returns.8[time_index:(time_index-11)])
s10_exp <- mean(joined_monthly_fisherpf$monthly.returns.9[time_index:(time_index-11)])

#Let's calculate the return for the entire pf

stock1_w <- 0.16
stock2_w <- 0.14
stock3_w <- 0.12
stock4_w <- 0.12
stock5_w <- 0.11
stock6_w <- 0.10
stock7_w <- 0.08
stock8_w <- 0.06
stock9_w <- 0.06
stock10_w <- 0.05

joined_monthly_fisherpf <- as.data.frame(joined_monthly_fisherpf) %>%
  mutate(portfolio_ret = stock1_w * monthly.returns + stock2_w * monthly.returns.1 +
           stock3_w * monthly.returns.2 + stock4_w * monthly.returns.3 + stock5_w * monthly.returns.4 +
           stock6_w * monthly.returns.5 + stock7_w * monthly.returns.6 + stock8_w * monthly.returns.7 +
           stock9_w * monthly.returns.8 + stock10_w * monthly.returns.9)

Pf_exp <- mean(joined_monthly_fisherpf$portfolio_ret[time_index:(time_index-11)]) 
               
# Now let's calculate Sharpe Ratio 

s1_Sharpe <- (((1+s1_exp)^12)-1 - Risk_Free)/stock1_sigma
s2_Sharpe <- (((1+s2_exp)^12)-1 - Risk_Free)/stock2_sigma
s3_Sharpe <- (((1+s3_exp)^12)-1 - Risk_Free)/stock3_sigma
s4_Sharpe <- (((1+s4_exp)^12)-1 - Risk_Free)/stock4_sigma
s5_Sharpe <- (((1+s5_exp)^12)-1 - Risk_Free)/stock5_sigma
s6_Sharpe <- (((1+s6_exp)^12)-1 - Risk_Free)/stock6_sigma
s7_Sharpe <- (((1+s7_exp)^12)-1 - Risk_Free)/stock7_sigma
s8_Sharpe <- (((1+s8_exp)^12)-1 - Risk_Free)/stock8_sigma
s9_Sharpe <- (((1+s9_exp)^12)-1 - Risk_Free)/stock9_sigma
s10_Sharpe <- (((1+s10_exp)^12)-1 - Risk_Free)/stock10_sigma

#Let's calculate the sharpe for the entire pf

pf_sharpe <- (((1+Pf_exp)^12)-1 - Risk_Free)/pf_sigma

#let's find the tracking error for these stocks

# in order to do this we need a benchmark that would be the NASDAQ 

Benchmark_returns <- monthlyReturn(getSymbols("^IXIC", auto.assign = F))
Benchmark_sigma <- sd(monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)])*sqrt(12) 

Benchmark_Exp <- mean(monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)])
Benchmark_Sharpe <-(((1+Benchmark_Exp)^12)-1 - Risk_Free)/Benchmark_sigma
#te

s1_te <- sd(joined_monthly_fisherpf$monthly.returns[time_index:(time_index-11)]
             - monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)]) * sqrt(12)
s2_te <- sd(joined_monthly_fisherpf$monthly.returns.1[time_index:(time_index-11)]
            - monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)]) * sqrt(12)
s3_te <- sd(joined_monthly_fisherpf$monthly.returns.2[time_index:(time_index-11)]
            - monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)]) * sqrt(12)
s4_te <- sd(joined_monthly_fisherpf$monthly.returns.3[time_index:(time_index-11)]
            - monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)]) * sqrt(12)
s5_te <- sd(joined_monthly_fisherpf$monthly.returns.4[time_index:(time_index-11)]
            - monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)]) * sqrt(12)
s6_te <- sd(joined_monthly_fisherpf$monthly.returns.5[time_index:(time_index-11)]
               - monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)]) * sqrt(12)
s7_te <- sd(joined_monthly_fisherpf$monthly.returns.6[time_index:(time_index-11)]
           - monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)]) * sqrt(12)
s8_te <- sd(joined_monthly_fisherpf$monthly.returns.7[time_index:(time_index-11)]
            - monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)]) * sqrt(12)
s9_te <- sd(joined_monthly_fisherpf$monthly.returns.8[time_index:(time_index-11)]
            - monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)]) * sqrt(12)
s10_te <- sd(joined_monthly_fisherpf$monthly.returns.9[time_index:(time_index-11)]
             - monthlyReturn(getSymbols("^IXIC", auto.assign = F))[time_index:(time_index-11)]) * sqrt(12)

# Let's visualize everything in a table

Overview <- data.frame(
  Stock = c("MSFT", "AAPL", "NVDA", "AMZN", "GOOGL", "VCIT", "AMD", "TSM", "LLY", "HD", 
            "IXIC","PORT"),
  Expected_Return = c(s1_exp, s2_exp, s3_exp, s4_exp, s5_exp, s6_exp, s7_exp, s8_exp, s9_exp, s10_exp, 
                      Benchmark_Exp, Pf_exp),
  Sigma = c(stock1_sigma, stock2_sigma, stock3_sigma, stock4_sigma, stock5_sigma, stock6_sigma, stock7_sigma, 
            stock8_sigma, stock9_sigma, stock10_sigma, Benchmark_sigma, pf_sigma),
  Tracking_Error = c(s1_te, s2_te, s3_te, s4_te, s5_te, s6_te, s7_te, s8_te, s9_te, s10_te, 
                     "N/A", "N/A"),
  Sharpe_Ratio = c(s1_Sharpe, s2_Sharpe, s3_Sharpe, s4_Sharpe, s5_Sharpe, s6_Sharpe, s7_Sharpe, s8_Sharpe, s9_Sharpe, 
                   s10_Sharpe, Benchmark_Sharpe, pf_sharpe)
)

#let's scatter E(x) and Risk (sigma)

library(ggplot2)

ggplot(Overview, aes(x = Sigma, y = Expected_Return, label = Stock)) +
  geom_point(color = "azure4") +
  geom_text(size = 2.2, hjust = 0.5, vjust = -1) +  # Adjust text alignment for clarity
  labs(x = "Sigma (Standard Deviation)", y = "Expected Return", title = "           Expected Return vs Sigma for Stocks") +
  theme_minimal()+
  theme(  
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "aliceblue"),
    panel.grid.minor = element_line(color = "lightgrey"),
    text = element_text(color = "black"))

#let's scatter E(x) and Risk (TE)

Overview_te <- data.frame(
  Stock = c("MSFT", "AAPL", "NVDA", "AMZN", "GOOGL", "VCIT", "AMD", "TSM", "LLY", "HD"),
  Expected_Return = c(s1_exp, s2_exp, s3_exp, s4_exp, s5_exp, s6_exp, s7_exp, s8_exp, s9_exp, s10_exp),
  Tracking_Error = c(s1_te, s2_te, s3_te, s4_te, s5_te, s6_te, s7_te, s8_te, s9_te, s10_te))

ggplot(Overview_te, aes(x = Tracking_Error, y = Expected_Return, label = Stock)) +
  geom_point(color = "azure4") +
  geom_text(size = 2.2, hjust = 0.5, vjust = -1) +  # Adjust text alignment for clarity
  labs(x = "TE", y = "Expected Return", title = "           Expected Return vs Tracking Error") +
  theme_minimal()+
  theme(  
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "aliceblue"),
    panel.grid.minor = element_line(color = "lightgrey"),
    text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
  
#let's check for correlation among the pf assets

correlations_fisher <- as.data.frame(cor(joined_monthly_fisherpf[time_index:(time_index-11),]))

#let's build a CAPM model for the assets in Fisher's pf

last_12_months <- joined_monthly_fisherpf[time_index:(time_index-11),]

#MSFT

stock1_reg <- lm(joined_monthly_fisherpf$monthly.returns~Benchmark_returns$monthly.returns, data=last_12_months) 

summary(stock1_reg)

#AAPL

stock2_reg <- lm(joined_monthly_fisherpf$monthly.returns.1~Benchmark_returns$monthly.returns, data=last_12_months) 

summary(stock2_reg)

#NVDA

stock3_reg <- lm(joined_monthly_fisherpf$monthly.returns.2~Benchmark_returns$monthly.returns, data=last_12_months) 

summary(stock3_reg)

#AMZN

stock4_reg <- lm(joined_monthly_fisherpf$monthly.returns.3~Benchmark_returns$monthly.returns, data=last_12_months) 

summary(stock4_reg)

#GOOGL

stock5_reg <- lm(joined_monthly_fisherpf$monthly.returns.4~Benchmark_returns$monthly.returns, data=last_12_months) 

summary(stock5_reg)

#VCIT #Rsqr 26.. 

stock6_reg <- lm(joined_monthly_fisherpf$monthly.returns.5~Benchmark_returns$monthly.returns, data=last_12_months) 

summary(stock6_reg)

#AMD 

stock7_reg <- lm(joined_monthly_fisherpf$monthly.returns.6~Benchmark_returns$monthly.returns, data=last_12_months) 

summary(stock7_reg)

#TSM

stock8_reg <- lm(joined_monthly_fisherpf$monthly.returns.7~Benchmark_returns$monthly.returns, data=last_12_months) 

summary(stock8_reg)

#LLY #Rsquared 0.1 

stock9_reg <- lm(joined_monthly_fisherpf$monthly.returns.8~Benchmark_returns$monthly.returns, data=last_12_months) 

summary(stock9_reg)

#HD 

stock10_reg <- lm(joined_monthly_fisherpf$monthly.returns.9~Benchmark_returns$monthly.returns, data=last_12_months) 

summary(stock10_reg)

#Let's visualize the betas

Overview_betas <- data.frame(
  Stock = c("MSFT", "AAPL", "NVDA", "AMZN", "GOOGL", "AMD", "TSM", "HD"),
  Expected_Return = c(s1_exp, s2_exp, s3_exp, s4_exp, s5_exp, s7_exp, s8_exp, s10_exp),
  Betas = c(stock1_reg$coefficients[2], stock2_reg$coefficients[2], stock3_reg$coefficients[2], stock4_reg$coefficients[2],
            stock5_reg$coefficients[2], stock7_reg$coefficients[2], stock8_reg$coefficients[2],
            stock10_reg$coefficients[2]))

ggplot(Overview_betas, aes(x = Betas, y = Expected_Return, label = Stock)) +
  geom_point(color = "azure4") +
  geom_text(size = 2.2, hjust = 0.5, vjust = -1) +  # Adjust text alignment for clarity
  labs(x = "Beta", y = "Expected Return", title = "           Expected Return vs Beta") +
  theme_minimal()+
  theme(  
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "aliceblue"),
    panel.grid.minor = element_line(color = "lightgrey"),
    text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )

#Let's calculate Treynor Ratio

Stock1_Trey <- (((1+s1_exp)^12)-1 - Risk_Free)/stock1_reg$coefficients[2] 
Stock2_Trey <- (((1+s2_exp)^12)-1 - Risk_Free)/stock2_reg$coefficients[2]
Stock3_Trey <- (((1+s3_exp)^12)-1 - Risk_Free)/stock3_reg$coefficients[2]
Stock4_Trey <- (((1+s4_exp)^12)-1 - Risk_Free)/stock4_reg$coefficients[2]
Stock5_Trey <- (((1+s5_exp)^12)-1 - Risk_Free)/stock5_reg$coefficients[2]
# VCIT Garbage Model
Stock7_Trey <- (((1+s7_exp)^12)-1 - Risk_Free)/stock7_reg$coefficients[2]
Stock8_Trey <- (((1+s8_exp)^12)-1 - Risk_Free)/stock8_reg$coefficients[2]
# LLY Garbage Model
Stock10_Trey <- (((1+s10_exp)^12)-1 - Risk_Free)/stock10_reg$coefficients[2]




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
#Efficient Frontier
library(data.table)
library(scales)
library(ggplot2)
library(quantmod)
library(reshape2)
ticker1 <- "NVDA"
ticker2<- "TSM"
ticker3<- "LLY"
mydf1 <- as.data.frame(getSymbols(ticker1, auto.assign=FALSE))
mydf2 <- as.data.frame(getSymbols(ticker2, auto.assign=FALSE))
mydf3 <- as.data.frame(getSymbols(ticker3, auto.assign=FALSE))

combined_df <- cbind(mydf1[,4], mydf2[,4], mydf3[,4])

dt <- as.data.frame(combined_df)
colnames(dt) <- c(ticker1, ticker2, ticker3)
dt$date = as.Date(rownames(mydf1))
dt <- melt(dt, id="date")
colnames(dt) <- c("date", "ticker","price")
dt <- data.table(dt)
# create indexed values
dt[, idx_price := price/price[1], by = ticker]
# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Price Developments") +
  xlab("Date") + ylab("Pricen(Indexed 2000 = 1)") +
  scale_color_discrete(name = "Company")

# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]

# summary table
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]

# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 4),
               sd = round(sd(ret), 4)),
           by = "ticker"]

ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.002)) +
  scale_x_continuous(label = percent, limits = c(0, 0.05))

################################################
#### Three risky assets portfolio
################################################
# load the data
ticker1_select <- "NVDA" #which of the 3 do you want to use
ticker2_select <- "TSM"
ticker3_select <- "LLY"

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


#library(plotly)
#Fama French Model
source("C://Users//Mi//Desktop//My Files//1-MBAN//Summer II//Wealth Management//Classes//8.2 Fama French predictions and residuals - source this file before running the next one.R")
#calling the Fama French 3F model UDF for NVDA
NVDA_FF3F <- fama_french_3F_pred_res(ticker="NVDA", from_date='2020-01-02', to_date='2024-07-01')
NVDA_FF3F$actuals
NVDA_FF3F$model_pred

#More predictions are wrong than right


#calling the Fama French 3F model UDF for TSM
TSM_FF3F <- fama_french_3F_pred_res(ticker="TSM", from_date='2020-01-02', to_date='2024-07-01')
TSM_FF3F$actuals
TSM_FF3F$model_pred

#More predictions are wrong than right


#calling the Fama French 3F model UDF for LLY
LLY_FF3F <- fama_french_3F_pred_res(ticker="LLY", from_date='2020-01-02', to_date='2024-07-01')
LLY_FF3F$actuals
LLY_FF3F$model_pred


#More predictions are right than wrong





###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################



