pointMinDistOnCircle <- function(circle, p){

	## Finds a point on a circle at a minimum distance from some point in space
	
	# Project point p onto circle plane
	ppp <- pointPlaneProj(p, circle$C, circle$N)
	
	# Re-scale vector to point as circle radius
	circle$R*uvector(ppp-circle$C) + circle$C
}