`cost.nom` <- function(p,q)
{
	test <- abs(p-q)
	ifelse(test > 0, 1, 0)
}