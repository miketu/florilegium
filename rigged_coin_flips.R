require(tidyverse)
# Animation to Show Off Coin Flips and Dice Tosses! ------------------------

library(animation)
ani.options(interval = 0.05, nmax = 500)

flip.coin(faces = c("Head",  "Tail"), type = "n", prob = c(0.8, 0.2)) ## "Cheating coin"

flip.coin(faces = c(1,2,3,4,5,6), type = "n", prob = c(1/6, 1/6,1/6,1/6,1/6,1/6),col=c(1,2,3,4,5,6)) ## "Six Sided Dice"

# Individual Case of a coin flip ---------------------------

individual_demo <- sample(seq(1:2),size=10,replace=TRUE,prob=c(0.5,0.5)) |>
    as.data.frame() |>
    rename(rolls = 1)

ggplot(individual_demo)+
    theme_bw()+
    geom_histogram(aes(x=rolls))


# Trial of Trials (Discrete Case) ---------------------------------------

# Now let's try to flip 10 times and count a LOT 
fair_coin_values <- data.frame()
unfair_coin_values <- data.frame()

for(x in 1:500)
{
    fair_coin_values <- rbind(fair_coin_values,sum(sample(seq(1:2),size=10,replace=TRUE)==1)) #Fair Dice
    unfair_coin_values <- rbind(unfair_coin_values,sum(sample(seq(1:2),size=10,replace=TRUE,prob=c(0.75,0.25))==1))

}

collected_values <- data.frame(fair_coin_values,unfair_coin_values) |>
    rename(faircoin=1,unfaircoin=2)

ggplot(data=collected_values)+geom_histogram(aes(x=faircoin),binwidth=0.5,fill="green",alpha=0.3)+geom_histogram(aes(x=unfaircoin),binwidth=0.5,fill="red",alpha=0.5)

ggplot(data=collected_values)+geom_density(aes(x=faircoin),fill="green",alpha=0.1)+geom_density(aes(x=unfaircoin),fill="red",alpha=0.1)

# How do we detect a cheating coin? -----------------------------------
