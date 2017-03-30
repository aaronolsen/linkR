vproj <- function(a, b){
	
	# Projection of vector a onto vector b
	(sum(a*b) / sqrt(sum(b^2)))*uvector(a)
}