intersectLines <- function(p1, v1, p2, v2){

	## Find intersection of two lines
	# https://math.stackexchange.com/questions/270767/find-intersection-of-two-3d-lines

	# Find distance between lines
	dl <- distLines(p1, v1, p2, v2)
	
	# Check if points are the same
	if(sum(abs(p1-p2)) == 0) return(p1)

	# If distance is greater than 0 then return NULL
	if(dl > 1e-10) return(NULL)

	# If lines are parallel then lines are coincident
	if(avec(v1, v2) < 1e-10) return(p1)

	fe <- cprod(v2, v1)
	fg <- cprod(v2, p2-p1)
	
	#
	denom <- sqrt(sum(fe^2))
	
	# Return NULL if denominator is 0
	if(denom == 0) return(NULL)
	
	#
	co <- sqrt(sum(fg^2)) / denom

	if(co == 0){
		i_l <- p1
	}else{

		# If co is non-zero, check which direction
		#If f×g and f×e point in the same direction, then the sign is +; otherwise, the sign is −
		if(avec(fe, fg) < 0.1){
			i_l <- p1 + co*v1
		}else{
			i_l <- p1 - co*v1
		}
	}
	
	i_l
}