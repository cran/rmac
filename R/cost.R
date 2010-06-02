`cost` <- function(p,q, fun = c("nominal", "absolute", "squared"))
{
	fun <- match.arg(fun)

	switch(fun, nominal = cost.nom(p,q),
		     absolute = cost.abs(p,q),
		     squared = cost.sqd(p,q))

}