#' Repeatedly collect samples from a uniform distribution and create a histogram
#' of their sum distribution in order to demonstrate the
#' Central Limit Theorem (CLT).
#'
#' @param n Number of samples to collect each iteration
#' @param iter Number of iterations to perform and show in the histogram
#' @param a Lower limit of uniform distribution
#' @param b Upper limit of uniform distribution
#' @return Nothing
#' @examples
#' mycltu(n=20,iter=100000)
#'
#' @export
mycltu = function(n,iter,a=0,b=10) {
  # fix annoying check problem, hides note: "no visible binding for global variable 'x'"
  x <- NULL

  ## r-random sample from the uniform
  y=runif(n*iter,a,b)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax
  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  ## add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  ## Add the density from which the samples were taken
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
