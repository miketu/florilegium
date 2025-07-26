require(tidyverse)
require(scale)

# Binomial Distribution Example-----------------------------

n = 5 #rolls of dice
plot_data <- data.frame(k=seq(0,n,by=1)) %>%
    mutate(probability=dbinom(k,n,1/6))

## Bar Plot
ggplot(plot_data,aes(x=k,y=probability))+geom_col()+
    theme_bw()

## Line Plot
ggplot(plot_data,aes(x=k,y=probability))+geom_point()+geom_line()+
    #ylim(c(0,1))+
    #xlim(c(0,10))+
    scale_y_continuous(n.breaks=20)+
    xlab("x")+
    theme_bw()


  # Law of Large Numbers---------------------------------
  
  probability_demo <- sample(seq(1:6),6000,replace=TRUE) |>
          as.data.frame() |>
          rename(rolls = 1)

  ggplot(probability_demo)+
      theme_bw()+
      geom_histogram(aes(x=rolls))

  # Central Limit Theorem--------------------------------------


  dice_pulled <- 10
  pulling_samples <- replicate(10000,mean(sample(probability_demo$rolls,dice_pulled,replace=TRUE))) |>
      as.data.frame() |>
      rename(samples = 1)

  pulling_plot <- ggplot(pulling_samples,aes(x=samples))+
      theme_bw()+
      geom_histogram(bins=10,color="grey",fill="grey",aes(y=..density..))+geom_density(alpha=0.2,fill="red")+
      xlim(c(1,6))

  print(pulling_plot)





 # Normal distribution comparison --------------------------------'


mu = 0
spread_1=1
spread_2=3

vals <- seq(-10,10,length=500)

plot_value_1 <- data.frame(x=vals,sigma=spread_1) %>%
                                                      mutate(probabilities=dnorm(vals,mean=mu,sd=spread_1))
      

plot_value_2 <- data.frame(x=vals,sigma=spread_2) %>%
      mutate(probabilities=dnorm(vals,mean=mu,sd=spread_2)) 
# 
# plot_value_2 <- data.frame(x=vals,sigma=spread_2) %>%
# 	mutate(probabilities=dt(vals,df=1)) 


plot_value_combine <- rbind(plot_value_1,plot_value_2)

ggplot(data=plot_value_combine,aes(x=x,y=probabilities,color=as.factor(sigma)))+geom_point()

# Normal Distribution vs. T-Distribution --------------------------------------

mu = 0
spread_1=1
student_df=1

vals <- seq(-10,10,length=500)

plot_value_1 <- data.frame(x=vals,sigma="normal") %>%
							mutate(probabilities=dnorm(vals,mean=mu,sd=spread_1))
					
plot_value_2 <- data.frame(x=vals,sigma="student t") %>%
	mutate(probabilities=dt(vals,df=student_df))

plot_value_combine <- rbind(plot_value_1,plot_value_2)
		
ggplot(data=plot_value_combine,aes(x=x,y=probabilities,color=as.factor(sigma)))+geom_point()#+ylim(c(0.0,0.4))+xlim(c(-1,1))


# Normal Distribution and T-tests ---------------------------------------------------


  population_1 <- data.frame(vals=rnorm(n=100000,mean=0.0,sd=1))

  population_2 <- data.frame(vals=rnorm(n=100000,mean=1.0,sd=1))


  difference_test <- data.frame(vals=c())
  for(x in 1:1000)
  {
      sample_n <- 30
      sampler_1 <- data.frame(vals=sample(population_1$vals,size=sample_n,replace=FALSE))
      sampler_2 <- data.frame(vals=sample(population_2$vals,size=sample_n,replace=FALSE))
      sampler_2_better <- sum(sampler_2 > sampler_1)/sample_n
      difference_test <- rbind(difference_test,data.frame(vals=sampler_2_better))
  }

  ggplot()+
      geom_density()+geom_density(data=population_1,aes(x=vals),alpha=0.8,fill="red")+geom_density(data=population_2,aes(x=vals),alpha=0.8,fill="green")

  mean(difference_test$vals)
  ggplot()+geom_histogram(data=difference_test,aes(x=vals),fill="BLUE",alpha=0.8)

  t.test(sampler_1$vals,sampler_2$vals)

  ggplot()+
      geom_density()+geom_density(data=sampler_1,aes(x=vals),alpha=0.8,fill="red")+geom_density(data=sampler_2,aes(x=vals),alpha=0.8,fill="green")

