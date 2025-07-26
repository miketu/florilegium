library(animation)
ani.options(interval = 0.05, nmax = 500)

flip.coin(faces = c("Head",  "Tail"), type = "n", prob = c(0.8, 0.2)) ## "Cheating coin"

flip.coin(faces = c(1,2,3,4,5,6), type = "n", prob = c(1/6, 1/6,1/6,1/6,1/6,1/6),col=c(1,2,3,4,5,6)) ## "Six Sided Dice"
