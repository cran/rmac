`rmacSqdiff` <-function(x,y,alternative=c("two.sided","less","greater"), conf.level ,na.rm)
{
	alternative<-match.arg(alternative)

	if (abs(length(x)-length(y))>0){stop("x and y must be the same length") }

	#store the names of the data sets
	dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))


	if (na.rm)
	{
		ismiss<- is.na(x) | is.na(y) 
		x<-x[!ismiss]
		y<-y[!ismiss]
	}

	nx<-length(x)
	ny<-length(y)
	v1<-mean(x)
	v2<-mean(y)
	v3<-mean(x*x)
	v4<-mean(y*y)
	v5<-mean(x*y)
	n<-length(x)

    xitilde<- .5* log( (v3+v4+2*v5-(v1+v2)^2)/(v3+v4-2*v5) )
    xihat<-xitilde + .5*log( n/(n-1) )
    rhohat<-tanh(xihat)
    sxx<- (1/(n-1))* sum( (x-mean(x))^2 )
    syy<- (1/(n-1))* sum( (y-mean(y))^2 )
    sxy<- (1/(n-1))* sum( (x-mean(x))*(y-mean(y)))

    varxi<- (n/(n-2))*((v1-v2)^4*( sxx+syy+2*sxy ) + 2*(v1-v2)^2*(sxx^2 + syy^2 + 6*sxx*syy - 8*(sxy^2)) + 8*(sxx+syy-2*sxy)*(sxx*syy - sxy^2))/(2*n*(sxx+syy-2*sxy+(v1-v2)^2 )^2 * (sxx+syy+2*sxy ))

    sigma<-sqrt(varxi)
    z<-xihat

    cint <- switch(alternative, less = c(-Inf, z + sigma * qnorm(conf.level)), greater = c(z - sigma * qnorm(conf.level), Inf), two.sided = z + c(-1, 1) * sigma * qnorm((1 + conf.level)/2))

    cint <- tanh(cint)
    attr(cint,"conf.level")<-conf.level

    p<- pnorm(xihat/(sigma/sqrt(n)))
    PVAL <- switch(alternative, less = p, greater = 1 - p, two.sided = 2 * pnorm(-abs(zval)))

    method<-c("RMAC with Squared Difference Function")
    names(rhohat)<-"RMAC concordance"
    NVAL<-0
    names(NVAL)<-"RMAC concordance"
    zval<-xihat/sigma
    names(zval)<-"z"

    out<-list(method=method,
        statistic=zval,
        conf.int=cint,
        estimate=rhohat, 
        alternative=alternative,
        null.value=NVAL, 
        p.value=as.numeric(PVAL),
        data.name=dname)

    class(out)<-"htest"

    return(out)
}


