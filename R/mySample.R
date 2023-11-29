#' Takes and graphs a sample of numbers from one to ten, repeating multiple times.
#'
#' @param n number of samples to take
#' @param iter number of iterations
#' @param time time for R to suspend after generating each graph to reduce strain on the system
#' @return NULL This function does not return anything
#' @examples
#'   mysample(n=1000, iter=30)
#'
#' @export
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
