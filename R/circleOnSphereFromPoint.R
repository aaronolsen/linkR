circleOnSphereFromPoint <- function(sphere, d, p, point.compare=NULL){

	# GIVEN A SPHERE AND A POINT P, THIS FUNCTION FINDS THE CIRCLE ON THE SURFACE OF THE 
	#	SPHERE WITH EACH POINT ON THE CIRCLE BEING THE SPECIFIED DISTANCE FROM POINT P

	# DEFINE CIRCLE NORMAL VECTOR
	nvector <- uvector(p - sphere$C)

	# SINGLE INTERSECTION - POINT AT RADIUS DISTANCE ALONG VECTOR BETWEEN P AND SPHERE CENTER
	if(distPointToPoint(sphere$C, p) == d+sphere$R) return(defineCircle(center=sphere$C+sphere$R*nvector, nvector=nvector, radius=0))

	# FIND DISTANCE BETWEEN POINT AND SPHERE CENTER
	distCp <- distPointToPoint(sphere$C, p)
	
	# SOLVE FOR DISTANCE OF CIRCLE CENTER FROM SPHERE CENTER
	distCC <- (d^2 - distCp^2 - sphere$R^2) / (-2*distCp)

	# NO INTERSECTION - POINT IS TOO FAR FROM SPHERE
	if(abs(distCC) > sphere$R) return(NULL)

	# SOLVE FOR CIRCLE RADIUS
	radius <- sqrt(sphere$R^2 - distCC^2)
	
	# FIND CIRCLE CENTER
	center <- sphere$C + distCC*nvector
	
	# DEFINE CIRCLE
	defineCircle(center=center, nvector=nvector, radius=radius)
}