
  require(NHANES)
  require(tidyverse)

  a <- NHANES |>
      filter(!is.na(PhysActive))

  ggplot(a)+geom_boxplot(aes(x=PhysActive,y=Weight))

  exercises <- filter(a,PhysActive=="Yes")
  slackers <- filter(a,PhysActive=="No")

  t.test(exercises$Weight,slackers$Weight)
