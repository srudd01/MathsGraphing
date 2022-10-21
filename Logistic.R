#The parameter 'n' controls the number of times the logistic map
#is applied. 'ndef' is the default value of 'n'.
#We assume 0 <= x due to e.g. how population models work.
ndef <- 40

#Range over which to sample initial conditions from. In general if r <= 4 then
#x_n are all bounded for 0 <= x <= 1.
xmin <- 0
xmax <- 1

#The r value in the formula x_{n+1} = rx_n(1-x_n) is defaulted to 3.56995
rdef <- 3.56995
#The value 3.56995 is where chaotic behaviour occurs.
#Naturally n must be a natural number of iterations and be nonzero.
logistic <- function(x, r=rdef, n=ndef){
  for (i in 1:n){
    x = r*x*(1-x)
  }
  return(x)
}


#cairo_pdf required because other the less than or equal symbol will not display
cairo_pdf(file= "logistic.pdf", onefile=T)

 for(i in 1:60){
   ndef <- i
   x <- seq(xmin,xmax, length.out = 10000)
   y <- lapply(x, logistic)
   str <- paste("Logistic Map Iterated", toString(i), "times with r = ",
                toString(rdef), " and",  toString(xmin),
                expression("\U2264 x \U2264 "), toString(xmax))
   plot(x,y, main=str, xlab = bquote("x"[.(i)]), ylab = bquote("x"[.(i+1)]))
 }
