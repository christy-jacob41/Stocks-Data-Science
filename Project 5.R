require(tidyquant)
require(quantmod)
require(tidyverse)

# graphing SPY to take a look
getSymbols("SPY")
chartSeries(SPY)

# getting monthly returns of SPY from January 1993 which is the farthest back it will go
Ra <- c("SPY") %>%
  tq_get(get  = "stock.prices",
         from = "1991-01-01",
         to   = "2021-05-07") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")
Ra

# creating a list to hold the accumulated totals after each month for passive investing
R <- c(0, 1)

# started with 0 and adding 1000 during the first month
R[1] <- 1000.0

# for all the month, getting the return at the end of the month assuming we add 1000 each month 
for(i in 2:341)
{
  # adding 1000 for this month to the previous months total times the monthly return
  R[i] <- 1000 + R[[i-1]] + (R[[i-1]] * Ra[i, 3])
}
R

# plotting the graph for passive investing
plot(x = Ra[,2], y = R, xlab = "Year", ylab="Accumulated Return of SPY in Dollars")

# getting the accumulated total for each year
R[[12]]
R[[24]]
R[[36]]
R[[48]]
R[[60]]
R[[72]]
R[[84]]
R[[96]]
R[[108]]
R[[120]]
R[[132]]
R[[144]]
R[[156]]
R[[168]]
R[[180]]
R[[192]]
R[[204]]
R[[216]]
R[[228]]
R[[240]]
R[[252]]
R[[264]]
R[[276]]
R[[288]]
R[[300]]
R[[312]]
R[[324]]
R[[336]]
R[[341]] # final accumulated total

tq_mutate_fun_options()

# getting the stock prices for SPY
SPY_prices <-  tq_get("SPY", get  = "stock.prices",
         from = "1991-01-01",
         to   = "2021-05-07") 

# adding a column with SMA
col_name <- "close"
mutate <- c("MACD", "SMA")
SPY_prices <- tq_mutate_xy_(SPY_prices, x = col_name, mutate_fun = mutate[[2]])
# mutating it to monthly
SPY_prices <- tq_transmute(SPY_prices, mutate_fun = to.monthly)

# creating a list to hold accumulated totals for active investing
A <- c(1000,0)
A[1] <- 1000 # start with 0 and add 1000 the first month
# we couldn't get the SMA at the end of January 1991 the way we implemented it, so we assumed to invest
A[[2]] <- 2000 + 1000 * (Ra[2,3])

# get whether or not the closing price is higher than the SMA for each month
x <- c(TRUE, FALSE)
for(i in 2:341)
{
 x[i] <- SPY_prices[i-1,5] > SPY_prices[i-1,8] 
}

x

# if the closing price is higher, stay invested, if not sell
for(i in 3:341)
{
  if (x[[i]]) # if closing price is higher stay invested, so you gain the previous months total times the monthly returns as well
  {
    A[i] <- 1000 + A[[i-1]] + A[[i-1]] * ((SPY_prices[i-1,5]/SPY_prices[i-2,5]) / 100)
  }
  else # if not, sell which mean you only gain 1000 dollars that month
  {
    A[i] <- 1000 + A[[i-1]]
  }
}


# get the accumulated total after each year
A[[12]]
A[[24]]
A[[36]]
A[[48]]
A[[60]]
A[[72]]
A[[84]]
A[[96]]
A[[108]]
A[[120]]
A[[132]]
A[[144]]
A[[156]]
A[[168]]
A[[180]]
A[[192]]
A[[204]]
A[[216]]
A[[228]]
A[[240]]
A[[252]]
A[[264]]
A[[276]]
A[[288]]
A[[300]]
A[[312]]
A[[324]]
A[[336]]
A[[341]] # final accumulated total


# plotting the active investing graph
plot(x = Ra[,2], y = A, xlab = "Year", ylab="Accumulated Return of SPY in Dollars")

