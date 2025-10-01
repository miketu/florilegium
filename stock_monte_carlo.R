# Stock market simulation, using historical returns
# I need to better learn about long-tail distributions, risk management, etc. 
# Eventually I can try to integrate it into a "portfolio management" to see portfolio worth with "continual investment"
# Indebted to https://www.youtube.com/watch?v=wrWmko61WMg for much of this approach, although I modified to made it more "tidyverse"

library(tidyverse)
library(quantmod)



for(ticker in c("QQQ","SPY",'AAPL','GOOG','TSLA')){


getSymbols(ticker) 

 ticker_data <- as.data.frame(date=index(get(ticker)),coredata(get(ticker)))  # ChatGPT helped me generate this... I don't quite understand the datatype manipulations
 names(ticker_data) <- sub(ticker,'',names(ticker_data))
 ticker_data <-   mutate(ticker_data,daily_return=as.numeric(.Close/lag(.Close,n=1)))  %>%
     na.omit() %>%
     select(daily_return)
 
 
 
 
paths <- replicate(n=5,expr=sample(ticker_data$daily_return,251*5,replace=TRUE)) %>% # 251 Trading days a year times years, n=number_of_trials
    apply(MARGIN=2,FUN=cumprod) %>%    
    as.data.frame() %>%
    mutate(time=row_number()) %>% 
    mutate(ticker=ticker) %>%
    pivot_longer(cols=starts_with("V")) 
  

print(ggplot(paths,aes(x=time,y=value,color=name))+geom_line()+ggtitle(ticker))

filtered_paths <- filter(paths,time==1000)
print(ggplot(filtered_paths,aes(x=value))+geom_density()+ggtitle(ticker))
print(paste(ticker,mean(filtered_paths$value))) 
}
