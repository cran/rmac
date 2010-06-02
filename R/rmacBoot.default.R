`rmacBoot.default` <- function(x, cost, conf.level = 0.95, na.rm = FALSE, numr = 999, ...)
{
	rmacBootCalc(x = x,cost = cost, dataname = deparse(substitute(x)),
		costname = deparse(substitute(cost)),conf.level = conf.level,
			na.rm = na.rm, numr = numr)
}
