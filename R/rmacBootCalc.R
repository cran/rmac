`rmacBootCalc` <- function(x, cost, dataname, costname, conf.level = 0.95, na.rm = FALSE, numr = 999)
{
	library(boot)
	
	y <- x[,1]
	z <- x[,2]

	n<- length(y)
	#check to see if the two input vectors are the same length

	if(abs(length(y) - length(z)) >0)
	{
		stop("input vectors must have the same length")
	}

	check<-sum(ifelse(y == z , 0, 1))

	if(check == 0)
	{
		warning("input vectors are identical data sets!")
		est <- 1
		ci <- c(1,1)
		pval <- NA
		zval <- NA
	}else
	{
		#remove missing data if specified
		if(na.rm)
		{
			ismiss <- is.na(y) | is.na(z)
			y <- y[!ismiss]
			z <- z[!ismiss]
		}

		#calculate the numerator
		bdata <- list(y,z)
		numerator <- (sum(do.call(cost,bdata)))/length(y)

		#calculate the denominator
		#find all combinations of the two data vectors
		allComb<- expand.grid(c(y,z),c(y,z))
		denominator<- sum(do.call(cost,list(allComb[,1], allComb[,2])))/nrow(allComb)
		coef<- 1 - (numerator/denominator)

		#use boot() to find the confidence interval
		agreeCalc<- function(data,indicies)
		{
			#calculate the numerator
			bdata <- list(data[indicies,1],data[indicies,2])
			numerator <- (sum(do.call(cost,bdata)))/length(data[indicies,1])

			#calculate the denominator
			#find all combinations of the two data vectors
			allComb<- expand.grid(c(data[indicies,1], data[indicies,2]), c(data[indicies,2],data[indicies,1]))
			denominator<- sum(do.call(cost,list(allComb[,1], allComb[,2])))/nrow(allComb)
			cf<- 1 - (numerator/denominator)
		}
		
		#use the boot()on the data
		data <- cbind(y,z)
		boot.out<- boot(data, agreeCalc, R = numr)
		confi<- try(boot.ci(boot.out, conf = conf.level, type = "bca"))

		#format the output list
		if (class(confi) ==  "bootci")
		{
			ci <- c(round(confi$bca[4],4), round(confi$bca[5],4))
		} else
		{
		  	warning("confidence interval not able to be calculated")
			ci <- c(-1, 1)
		}


		#perform a z-test on the RMAC estimate
		est <- coef
		sigma <- max((ci[2] - est),(est-ci[1])) /qnorm(1-(1-conf.level)/2)
		zval <-est/sigma
		pval<- 2 * pnorm(-abs(zval))

	}

	#format the output list
	attr(ci,"conf.level")<- conf.level
	method <- paste("RMAC with", costname, "cost function", sep = " ")
	alternative <-c("true RMAC is not equal to zero")

	names(zval) <- "z"
	names(est) <- "RMAC concordance"
	names(ci) <- paste(conf.level,"% Confidence Interval")

	out <- list(method = method,
			statistic = zval,
			conf.int = ci,
			estimate = est,
			alternative = alternative,
			p.value = as.numeric(pval),
			data.name = dataname)
	class(out)<- "htest"
	return(out)
}
