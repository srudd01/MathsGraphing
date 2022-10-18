#Assume x is in the interval [0,1], n is a whole number of iterations.
cantor <- function(x, n = 10){
  if(n <= 1){
    return(x)
  }
  
  if(x < 1/3){
    0.5*cantor(3*x, n-1)
  }
  else if(x > 2/3){
    n <- n-1
    0.5 +0.5*cantor(3*x-2, n-1)
  }
  else{
    #No need for further iterations after this point.
    return(0.5)
  }
}


x <- seq(0, 1, length.out=1000)

y <- lapply(x, cantor)
plot(x,y, main="Cantor Function")