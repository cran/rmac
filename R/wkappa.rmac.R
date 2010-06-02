`wkappa.rmac` <- function(data,dataname, Wcode, conf.int, conf.level, s, k)
{
	costname <- c("nominal", "absolute", "squared difference")

	m<- data
	
	n<-sum(m)
	m<-m/n
	klist <- 1:k
	pio<-0
	piz<-0

	row.m<- apply(m,1,sum)
	col.m<- apply(m,2,sum)
	
	#calculate the weights matrix, based on the type of cost function used
	w<-matrix(0,k,k)
	if (Wcode==0)
	{
		w<-diag(k)

	} else if (Wcode==1)
	{
		for (i in 1:k)
		{
			for (j in 1:k)
			{
				w[i,j]<- 1- abs( s[i]-s[j] )/(s[k]-s[1])
			}
		}
	} else if (Wcode==2)
	{
		for (i in 1:k)
		{
			for (j in 1:k)
			{
				w[i,j]<- 1 - ( s[i]-s[j] )^2/(s[k]-s[1])^2
			}
		}
	}

	#computes Pio and Pie
	pizcalc<-function(j)
	{
		inpizcalc<- function(i)
		{
			temp1<-w[i,j]*(.5*row.m[i]+.5*col.m[i])*(.5*row.m[j]+.5*col.m[j])
		}
		temp2<-lapply(klist,inpizcalc)
	}

	piocalc <- function(j)
	{
		inpiocalc<- function(i)
		{
			temp1<-w[i,j]*m[i,j]
		}
		temp2<-lapply(klist,inpiocalc)
	}

	pio <- sum(unlist(lapply(klist,piocalc)))
	piz <- sum(unlist(lapply(klist,pizcalc)))

	#if the chance agreement is 1, return a warning
	if (piz==1)
	{
		rmac<-1
		warning("chance agreement is 1, kappa set to 1 but really undefined") 
	} else
	{
		rmac<- (pio - piz)/(1 - piz)
	}

	#calculate the confidence intervals
	if (conf.int)
	{

		if (rmac==1 | rmac==-1)
		{
			lower.rmac<- -1
			upper.rmac<-  1
			warning("confidence interval could not be calculated")
			zval <- NA
			pval<- NA
		} else
		{
			W<-rep(0,k)

			Dmat<-matrix(NA,k,k)

			for (g in 1:k)
			{
				W[g]<- .5* sum( w[g,]*(row.m + col.m) )
			}

			for (g in 1:k)
			{
  				for (h in 1:k)
				{
					Dmat[g,h]<- (w[g,h] - 2*(W[g]+W[h]))/(2*(1+pio-2*piz)) + w[g,h]/(2*(1-pio))
  				}
			}

			#computes sigma squared (variance)
			sig2.1<-function(j)
			{
				insig2.1<- function(i)
				{
					temp1<-m[j,i]*(Dmat[j,i])^2
				}
				temp2<-lapply(klist,insig2.1)
			}



			sig2.2<- function(j)
			{
				insig2.2<- function(i)
				{
					temp1<-m[j,i]*Dmat[j,i]
				}
				temp2<-lapply(klist,insig2.2)
			}
			
			sig21 <- sum(unlist(lapply(klist,sig2.1)))
			sig22 <- sum(unlist(lapply(klist,sig2.2)))
			sig2<- sig21 - (sig22^2)
			
			beta<-.5* log( (1+rmac)/(1-rmac) )

			lower.rmac<- tanh( beta - qnorm(1-(1-conf.level)/2)*sqrt(sig2/n) )
			upper.rmac<- tanh( beta + qnorm(1-(1-conf.level)/2)*sqrt(sig2/n) )

			zval<- beta/sqrt(sig2)

			pval<- 2 * pnorm(-abs(zval))

		} 
		
		ci<- c(lower.rmac,upper.rmac)
    	}else
	{

	  ci<- c(NA,NA)
	  zval <- NA
	  pval<- NA
	}

 	#format the output list
	attr(ci,"conf.level")<- conf.level
	
	method <- paste("RMAC with", costname[Wcode + 1], "cost function", sep = " ")
	alternative <-c("true Kappa is not equal to zero")
	names(zval) <- "z"
	names(rmac) <- "RMAC concordance"
	out <- list(method = method,
		statistic = zval,
		conf.int = ci,
		estimate = rmac,
		alternative = alternative,
		p.value = as.numeric(pval),
		data.name = dataname)

	class(out)<-"htest"
	return(out)
}