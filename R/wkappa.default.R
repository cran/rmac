`wkappa.default` <- function(x, Wcode = 0, method = c("fmac", "rmac"), conf.int = TRUE, conf.level = 0.95, s = 1:k, ...)
{
	method <- match.arg(method)

	facts <- sort(unique(c(x[,1],x[,2])))
	p <- factor(x[,1], levels = facts)
	q <- factor(x[,2], levels = facts)
	m<-table(p,q)


	k<-dim(m)[[1]]
	
	if (method  == "rmac")
	{
		RMAC <- wkappa.rmac(data=m, dataname = deparse(substitute(x)), Wcode = Wcode, conf.int = conf.int, conf.level = conf.level, s = s, k)
		return(RMAC)
	}else if (method == "fmac")
	{
		FMAC <- wkappa.fmac(data= m, dataname = deparse(substitute(x)), Wcode = Wcode, conf.int = conf.int, conf.level = conf.level, s = s, k)
		return(FMAC)
	}
}
