#' R Package
#'
#' @param csv
#' @param x X variable as a vector
#' @param y Y variable as a vector
#' @param xlab x label
#' @param ylab y label
#' @param n1 size of population 1
#' @param sigma1 standard deviation of population 1
#' @param mean1 mean of population 1
#' @param iter the number of desired iterations
#' @param ymax max y value for fitting the graph
#'
#' @return Dataset (myread), A sophisticated plot (scatterhist), table(myplot), Simulated T-Test (myTsim)
#' @export
#'
#' @examples
#' dataset <- myread("filename.csv")
#' \dontrun{with(ddt, scatterhist(LENGTH,WEIGHT, xlab="LENGTH"))}
#' q <- myplot(dataset)
#' t <- myTsim(20, 5, 400, 10000, .01)
#'
myread=function(csv){
  return(read.csv(csv))
}

scatterhist = function(x, y, xlab="", ylab=""){
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,1,1))
  plot(x,y)
  par(mar=c(0,3,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
  par(mar=c(3,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0,
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0,
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}

mytab=function(x, y){
  return(table(x, y))
}

myTsim<-function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,...){    # adjust ymax to make graph fit
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1

  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1

  sd1=apply(data1.mat,2,sd) # sd
  ybar=apply(data1.mat,2,mean)  # mean

  w=(ybar-mean1)/(sd1/sqrt(n1))      #T stat

  hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
       main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",T," iterations= ",iter)),
       xlab=expression(paste(T, "Statistic",sep=" ")), las=1)
  lines(density(w),col="Blue",lwd=3) # add a density plot
  curve(dt(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  title=expression(T==frac((bar(y)-mu),s/sqrt(n1))) #mathematical annotation -see ?plotmath
  legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Legend #
  return(list(w=w,summary=summary(w),sd=sd(w),fun="T")) # some output to use if needed
}
