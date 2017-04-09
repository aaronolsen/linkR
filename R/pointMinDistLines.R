pointMinDistLines <- function(l1, l2){

	# Finds the position of a point at a minimum distance from several lines
	dist_point_to_line_error
	start <- colMeans(l1)
	lower <- apply(rbind(l1,l2), 2, 'min')
	upper <- apply(rbind(l1,l2), 2, 'max')

	min_dist_solve <- tryCatch(
		expr={
			nlminb(start=start, objective=dist_point_to_line_error, 
				lower=lower, upper=upper, l1=l1, l2=l2)
		},
		error=function(cond) {print(cond);return(NULL)},
		warning=function(cond) {print(cond);return(NULL)}
	)

	return(list('p'=min_dist_solve$par, 'error'=min_dist_solve$objective))
}