intersectPlaneLine <- function(p, n, l1, l2){

	# https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection	

	# Check that line and plane are not parallel
	if(avec(n, l2-l1) == pi/2) return(c(NA,NA,NA))

	# Get two points in the plane
	v <- vorthogonal(n)
	q <- p + v
	r <- p + cprod(n, q-p)
	
	# Create a and b matrices
	a <- cbind(l1-l2, q-p, r-p)
	b <- cbind(l1-p)
	
	# Solve for t,u,v
	tuv <- solve(a, b)

	# Find intersection point
	l1 + (l2 - l1)*tuv[1]
}