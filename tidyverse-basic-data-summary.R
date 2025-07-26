
require(tidyverse)
require(nycflights13)

a <- flights |>
    group_by(month) |>
    summarize(arr_delay_mean=mean(arr_delay,na.rm=TRUE),arr_delay_sd=sd(arr_delay,na.rm=TRUE),arr_delay_median=median(arr_delay,na.rm=TRUE))
