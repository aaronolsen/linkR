createPointShape <- function(shape = c('triangle', 'rectangle'), center = NULL, 
	ends = NULL, width = NULL, nvector = NULL, with.center = FALSE, size = 1){

	if(shape[1] == 'triangle'){

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
		
		connect <- list(c(1,2), c(2,3), c(1,3))

	}else if(shape[1] %in% c('rect', 'rectangle')){
		
		#
		wvec <- width*uvector(cprod(ends[2,]-ends[1,], nvector))
		
		verts <- rbind(
			ends[1, ] + wvec,
			ends[1, ] - wvec,
			ends[2, ] + wvec,
			ends[2, ] - wvec
		)
		
		connect <- list(c(1,2), c(2,4), c(4,3), c(3,1))
	}
	
	list(
		'points'=verts,
		'connect'=connect
	)
}
