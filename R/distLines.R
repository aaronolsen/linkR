distLines <- function(p1, v1, p2, v2){

	## Finds shortest distance between two lines
	# https://math.stackexchange.com/questions/13734/how-to-find-shortest-distance-between-two-skew-lines-in-3d

	# Check if lines are parallel
	if(avec(v1, v2) < 1e-6){
		
		# Project point from one line onto other (doesn't matter which point since distance is same all along lines)
		pnl <- pointNormalOnLine(p1, p2, p2+v2)
		
		# Return distance
		return(distPointToPoint(p1, pnl))
	}

	# Get uvec of cprod
	cp12 <- uvector(cprod(v1, v2))
	
	abs(sum(cp12*(p2-p1)))
}