create_body_points <- function(center, shape=c('triangle'), nvector, size = 1){

	# Find second vector normal to normal vector
	v1 <- uvector(vorthogonal(nvector))

	# Length of side
	h <- 1

	# Find distance from triangle center to vertices
	a <- 2*((sqrt(h^2 - (h/2)^2)) / 3)
	
	# Define triangle vertices
	verts <- rbind(
		center + a*v1,
		center + a*(v1 %*% tMatrixEP(nvector, (2*pi)/3)),
		center + a*(v1 %*% tMatrixEP(nvector, (4*pi)/3))
	)

	verts
}
