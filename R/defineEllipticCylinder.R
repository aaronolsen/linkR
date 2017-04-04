defineEllipticCylinder <- function(center, nvector, a, r1, r2){

	# Define main elements
	ecylinder <- list('C'=center, 'N'=nvector, 'R1'=r1, 'R2'=r2)

	# Make sure is unit vector
	nvector <- nvector

	# Find U vector
	ecylinder$U <- vorthogonal(nvector)
	
	# Set orientation of U vector
	ecylinder$U <- ecylinder$U %*% tMatrixEP(nvector, a)

	# Find U vector
	ecylinder$V <- cprod(nvector, ecylinder$U)

	ecylinder
}