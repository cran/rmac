`wkappa.fmac` <- function(data, dataname, Wcode, conf.int, conf.level, s, k)
{
	costname <- c("nominal", "absolute", "squared difference")

	m<-data
	#checks to see if their are any rows or columns in m that
	#are completely zeros; if so, the confidence interval is not calculated
	#and a warning is returned
	test<- sum(ifelse(rowSums(m) == 0 | colSums(m) == 0, 1, 0))

	#find the table total and convert the counts to proportions
	n<-sum(m) 
	m<-m/n
	klist <- 1:k

	#find the marginal distributions
	row.m<- apply(m,1,sum)
	col.m<-apply(m,2,sum)

	#Constructs the weights matrix
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

	piecalc<-function(j)
	{
		inpiecalc<- function(i)
		{
			temp1<-w[i,j]*row.m[i]*col.m[j]
		}
		temp2<-lapply(klist,inpiecalc)
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
	pie <- sum(unlist(lapply(klist,piecalc)))


	#if the chance agreement is 1, return a warning
	if (pie==1)
	{
		fmac<- 1
		warning("chance agreement is 1, FMAC set to 1 but really undefined")
	}else{
		fmac <- (pio - pie)/(1 - pie)
	}

	est<- fmac


	#calculate the confidence intervals
	if (conf.int)
	{
		#if the weighted kappa is 1 or -1, return a warning and set the confience interval to (-1,1)
		#otherwise, calculate the confidence interval
		if (test != 0 | fmac==1 | fmac==-1)
		{
			lower.fmac<- -1
			upper.fmac<-  1
			warning("confidence interval not able to be calculated")
			zval<- NA
			pval<- NA
		} else
		{
			wbaridot <- rep(0,k)
			wbardotj <- rep(0,k)

			for (g in 1:k)
			{
         			wbaridot[g]<- sum( w[g,]*col.m )
				wbardotj[g]<- sum( w[g,]*row.m )
			}


			#calculate sigma squared (variance)
		      sig2calc<-function(j)
			{
				insig2calc<- function(i)
				{
					temp1<-m[i,j]*( w[i,j] -(wbaridot[i]+wbardotj[j])*(1-fmac))^2
				}
				temp2<-lapply(klist,insig2calc)
			}
			sig2 <- sum(unlist(lapply(klist,sig2calc)))
			sig2<- (1/(n*(1-pie)^2)) * ( sig2 - (fmac - pie*(1-fmac))^2 )

			lower.fmac<-  max(-1, est - qnorm(1-(1-conf.level)/2)*sqrt(sig2/n))
			upper.fmac<-  min(1, est + qnorm(1-(1-conf.level)/2)*sqrt(sig2/n))

			zval<- est/sqrt(sig2)
			pval<- 2 * pnorm(-abs(zval))
		}
	
		ci<- c(lower.fmac,upper.fmac)
		
    	}else
	{

	  ci<- c(NA,NA)
	}

 	#format the output list
	attr(ci,"conf.level")<- conf.level

	method <- paste("FMAC with", costname[Wcode + 1], "cost function", sep = " ")
	alternative <-c("true Kappa is not equal to zero")
	names(zval) <- "z"
	names(est) <- "FMAC concordance"
	names(ci) <- paste(conf.level,"% Confidence Interval")

	out <- list(method = method,
		statistic = zval,
		conf.int = ci,
		estimate = est,
		alternative = alternative,
		p.value = as.numeric(pval),
		data.name = dataname)

	class(out)<-"htest"
	return(out)
}


