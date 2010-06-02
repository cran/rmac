`rmacBoot.table` <- function(x,cost, conf.level = 0.95, na.rm = FALSE, numr = 999, ...)
{
	y<-c()
	z<-c()

 	k<-dim(x)[[1]]
 	for (i in 1:k)
 	{
		for(j in 1:k)
		{
			y<-c(y,rep(i,x[i,j]))
			z<-c(z,rep(j,x[i,j]))
		}
	}

	xnew<- cbind(y,z)
	
	rmacBootCalc(x = xnew, cost = cost,dataname = deparse(substitute(x)),
		costname = deparse(substitute(cost)), conf.level = conf.level,
			na.rm = na.rm, numr = numr)

}