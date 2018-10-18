anglePlaneVector <- function(p, n, v){

	if(avec(n, v) == 0) return(pi/2)

	# Find projection of vector onto plane
	v1 <- pointPlaneProj(q=c(0,0,0), p=p, n=n)
	v2 <- pointPlaneProj(q=v, p=p, n=n)

	# 
	avec(v, v2-v1)
}