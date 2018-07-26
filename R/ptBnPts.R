ptBnPts <- function(pt, p1, p2){
	
	## Tests if pt is between p1 and p2

	d12 <- sqrt(sum((p1 - p2)^2))
	
	d1 <- sqrt(sum((pt - p1)^2))
	d2 <- sqrt(sum((pt - p2)^2))

	if(d1 < d12 && d2 < d12) return(TRUE)
	return(FALSE)	
}