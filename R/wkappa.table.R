`wkappa.table` <- function(x, Wcode = 0, method = c("fmac", "rmac"), conf.int = TRUE, conf.level = 0.95, s = 1:k, ...)
{
	method<- match.arg(method)

	k<-dim(x)[[1]]
	
	if (method == "rmac")
	{
		RMAC <- wkappa.rmac(data=x, dataname = deparse(substitute(x)), Wcode = Wcode, conf.int = conf.int, conf.level = conf.level, s = s, k)
		return(RMAC)
	}else if(method == "fmac")
	{
		FMAC <- wkappa.fmac(data= x, dataname = deparse(substitute(x)), Wcode = Wcode, conf.int = conf.int, conf.level = conf.level, s = s, k)
		return(FMAC)
	}	
}