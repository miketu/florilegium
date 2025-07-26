
  require(tidyverse)
  # Setting up the Diamond Dataset--------------------------
  plot_data <- diamonds
  plot_graph <- ggplot(data=plot_data)
  
  # Plot Types-------------------------------------------
  ## Scatter Plot----------------------------------------
  plot_graph+geom_point(aes(x=carat,y=price))

   ## Bar Plot----------------------------------------
  plot_graph+geom_bar(aes(x=cut,y=price,fill=clarity),stat="identity")

   ## Histogram Plots----------------------------------------
  plot_graph+geom_histogram(aes(price))

   ## Box Plots----------------------------------------
   plot_graph+geom_boxplot(aes(x=cut,y=price),outlier.shape=NA)+geom_jitter(aes(x=cut,y=price,color=color),width=0.1)

   
