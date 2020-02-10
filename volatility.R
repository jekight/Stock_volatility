library(tidyverse)
library(tidyquant)
library(ggplot2)

#Load in NYSE companies
NYSE <- tq_exchange('NYSE')

#Create a list of symbols for companies in NYSE
ticker_list <- NYSE[1]

#Obtain stock price information for all companies in NYSE
stocks_df <- tq_get(ticker_list, from = "2019-02-07", to = "2020-02-07")

#Merge the two dataframes together
stocks_data <- stocks_df %>%
  left_join(NYSE, by = 'symbol')

#Place companies with missing sectors into Miscellaneous sector
stocks_data$sector <- fct_explicit_na(stocks_data$sector)
table(stocks_data$sector)
stocks_data$sector[stocks_data$sector == "(Missing)"] <- "Miscellaneous"
table(stocks_data$sector)

write_csv(stocks_data,"/Users/jeremykight/Desktop/R Directory/Stock_volatility/stocks_data.csv")

#######################################################################################################
#Convert each assets raw adjusted closing price to returns
daily_returns = stocks_data %>%
  group_by(company, symbol, sector) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily") %>%
  ungroup()  
table(daily_returns$sector)
#######################################################################################################

#Compute Average Returns Then Volatility from daily returns
sector_daily_returns <- daily_returns %>%
  group_by(date, sector) %>%
  summarise(avg_volatility = sd(daily.returns))

table(sector_daily_returns$sector)

#######################################################################################################

# Find which sector had the highest average volatility on specific dates
sector_daily_returns %>%
  arrange(desc(avg_volatility)) %>%
  head(15)
#######################################################################################################

#Convert each assets raw adjusted closing price to returns monthly
monthly_returns = stocks_data %>%
  group_by(company, symbol, sector) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly") %>%
  ungroup()

#Compute monthly Returns Then Volatility
sector_monthly_returns <- monthly_returns %>%
  group_by(date, sector) %>%
  summarise(avg_volatility = sd(monthly.returns))
head(sector_monthly_returns,15)
#######################################################################################################

# Find which sector had the highest average volatility during specific months
sector_monthly_returns %>%
  arrange(desc(avg_volatility)) %>%
  head(15)
#######################################################################################################

#Average Volatility Year
year_avg = daily_returns %>%
  group_by(sector) %>%
  summarise(avg_volatility = sd(daily.returns)) %>%
  arrange(desc(avg_volatility))

head(year_avg,13)
#######################################################################################################

#Plot returns vs. volatility to show companies having the largest impact.

daily_returns %>%
  group_by(symbol, company, sector) %>%
  summarise(avg_return = round(mean(daily.returns), 4),
            avg_volatility = sd(daily.returns), na.rm = TRUE) %>%
  ggplot(aes(avg_return, avg_volatility))+
  geom_text(aes(label = symbol), size = 3)+
  labs(title = "Average Return vs Volatility Over Last Year",
       x = "Average Return", y = "Average Volatility")+
  theme_minimal()
#######################################################################################################

# Find why FHI has such a high volatility. Check missing values
FHI_data <- stocks_data %>%
  filter(symbol == "FHI")
sum(is.na(FHI_data$adjusted))
sum(!is.na(FHI_data$adjusted))
#######################################################################################################

# Find why FINS has such a high volatility. Check missing values
FINS_data <- stocks_data %>%
  filter(symbol == "FINS")
sum(is.na(FINS_data$adjusted))
sum(!is.na(FINS_data$adjusted))
#######################################################################################################

# Find why NEX has such a high volatility. Check missing values

NEX_data <- stocks_data %>%
  filter(symbol == "NEX")
sum(is.na(NEX_data$adjusted))
sum(!is.na(NEX_data$adjusted))
#######################################################################################################

#Remove FHI, FINS, and NEX from dataset
daily_returns_adj <- daily_returns %>%
  filter(symbol != "FHI" & symbol != "FINS" & symbol != "NEX")

#Calculate volatility 
year_avg_adj = daily_returns_adj %>%
  group_by(sector) %>%
  summarise(avg_volatility = sd(daily.returns)) %>%
  arrange(desc(avg_volatility))

head(year_avg_adj,13)
#######################################################################################################

#Load in S&P 500 Companies
sp500 <- tq_index('SP500')

#Create a list of tickers for the companies in the S&P 500
tickers <- sp500[1]

#Obtain stock price information for all companies in S&P 500
sp500_df <- tq_get(tickers, from = "2019-02-07", to = "2020-02-07")

#Join the dataframes together
sp500_data <- sp500_df %>%
  left_join(sp500, by = 'symbol')

#Place companies with missing sectors into Miscellaneous sector
sp500_data$sector <- fct_explicit_na(sp500_data$sector)
sp500_data$sector[sp500_data$sector == "(Missing)"] <- "Miscellaneous"
#######################################################################################################

#Convert each assets raw adjusted closing price to returns
sp500_daily_returns = sp500_data %>%
  group_by(company, symbol, sector) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily") %>%
  ungroup()  
#######################################################################################################

#Compute Returns Then Volatility
sp500_sector_daily_returns <- sp500_daily_returns %>%
  group_by(date, sector) %>%
  summarise(avg_volatility = sd(daily.returns))
#######################################################################################################

#Convert each assets raw adjusted closing price to returns monthly
sp500_monthly_returns = stocks_data %>%
  group_by(company, symbol, sector) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly") %>%
  ungroup()

#Compute monthly Returns Then Volatility
sp500_sector_monthly_returns <- sp500_monthly_returns %>%
  group_by(date, sector, symbol) %>%
  summarise(avg_volatility = sd(monthly.returns))
#######################################################################################################

# Find which sector had the highest average volatility
sp500_sector_daily_returns %>%
  arrange(desc(avg_volatility)) %>%
  head(15)
#######################################################################################################

#Find which months had the highest volatility and which sector caused it
sp500_sector_monthly_returns %>%
  arrange(desc(avg_volatility)) %>%
  head(15)
#######################################################################################################

#Average Volatility Year
sp500_year_avg = sp500_daily_returns %>%
  group_by(sector) %>%
  summarise(avg_return = round(mean(daily.returns), 4),
            avg_volatility = sd(daily.returns)) %>%
  arrange(desc(avg_volatility))

head(sp500_year_avg,13)
#######################################################################################################
