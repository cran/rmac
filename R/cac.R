`cac` <-function(x, type = c("absolute", "squared"), method = c("fmac", "rmac"), alternative=c("two.sided","less","greater"), conf.level=.95, na.rm=FALSE, numr = 999)
{
	type <- match.arg(type)
	method <- match.arg(method)	

	if (method == "rmac")
	{
		if (type == "squared")
		{
			rmac <- rmacSqdiff(x=x[,1], y=x[,2], alternative= alternative, conf.level=conf.level, na.rm = na.rm)
		}else if (type == "nominal")
		{
			rmac <- rmacBoot(x=x,cost = cost.nom, conf.level=.95)
		}else if (type == "absolute")
		{
			rmac <- rmacBoot(x=x, cost = cost.abs, conf.level=.95)
		}
		return(rmac)
	}else if (method == "fmac")
	{
		costfun <- function(p,q, fun = type)
		{
			cost(p,q, fun = fun)
		}
		fmac<-fmacBoot(x=x, cost = costfun,conf.level=.95)
		return(fmac)
	}
}
