define2DTriangle <- function(center, nvector, size=1, with.center=FALSE){

	# Find second vector normal to normal vector
	v1 <- uvector(vorthogonal(nvector))

	# Length of side
	h <- size

	# Find distance from triangle center to vertices
	a <- 2*((sqrt(h^2 - (h/2)^2)) / 3)

	# Define triangle vertices
	verts <- rbind(
		center + a*v1,
		center + a*(v1 %*% tMatrixEP(nvector, (2*pi)/3)),
		center + a*(v1 %*% tMatrixEP(nvector, (4*pi)/3))
	)

	if(with.center){
		verts <- rbind(verts, center)
		connect <- list(c(1,2), c(2,3), c(1,3))
	}else{
		connect <- list(c(1,2), c(2,3), c(1,3))
	}
	
	list(
		'points'=verts,
		'connect'=connect
	)
}
