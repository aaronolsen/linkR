create_body_points <- function(center, shape=c('triangle'), nvector, with.center = TRUE, size = 1){

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
	
	if(with.center) verts <- rbind(center, verts)
	
	# Create local coordinate system
	#lcs <- rbind(center, center+uvector(verts[4,]-verts[3,]), center-uvector(verts[2,]-center), center+nvector)

	#list('coor'=verts, 'lcs'=lcs)
	
	verts
}
