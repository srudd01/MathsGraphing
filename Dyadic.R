#The parameter 'n' controls the number of times the dyadic transformation
#is applied. 'ndef' is the default value of 'n'.
#We assume 0 <= x <= 1
ndef <- 41
dyadic <- function(x, n=ndef){
  if(n < 1){
    return(x)
  }
  
  if(x < 1/2){
    return(dyadic(2*x, n-1))
  }
  else{
    return(dyadic(2*x-1,n-1))
  }
}

pdf(file= "dyadic.pdf")

#We only require 60 iterations because when ndef = 60, then the graph is now
#the straight line at y=0. At this point the transformation will always assign
#y=0 to 2*y = 0, meaning no further changes will occur.
 for(i in 1:60){
   ndef <- i
   x <- seq(0,1, length.out = 10000)
   y <- lapply(x, dyadic)
   str <- paste("Dyadic Transformation Iterated", toString(i), "times")
   #Using bquote we label the axes with the number of times the dyadic
   #transformation has been iterated.
   plot(x,y, main=str, xlab = bquote(paste("f"^.(i), "(x)")),
        ylab = bquote(paste("f"^.(i+1), "(x)")))
}
