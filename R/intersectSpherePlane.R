intersectSpherePlane <- function(c, r, p, n){

	## Intersection of sphere and plane

	# Make sure normal vector is unit length
	#n <- uvector(n)

	# Find center of circle in plane (projection of sphere center into plane)
	center <- pointPlaneProj(c, p, n)

	# Find R and d
	r_2 <- r^2
	d_2 <- sqrt(sum((c - center)^2))
	
	# Check for tangency
	if(r_2 == d_2) return(list('center'=center, 'nvector'=n, 'radius'=0))

	# Check for no intersection
	if(r_2 < d_2) return(NULL)
	
	# Find radius
	radius <- sqrt(r^2 - d_2^2)

	list(
		'center'=center,
		'nvector'=n,
		'radius'=radius
	)
}