associatePointShape <- function(mechanism, shape, body, center = NULL, 
	ends = NULL, width = NULL, nvector = NULL, with.center = FALSE, size = 1){

	point_shape <- createPointShape(shape=shape, center=center, ends=ends, width=width, 
		nvector=nvector, with.center=with.center, size=size)
	
	mechanism <- associatePoints(mechanism, points=point_shape$points, body=body, 
		points.connect=point_shape$connect)
	
	mechanism
}
