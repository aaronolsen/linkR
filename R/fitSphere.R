fitSphere <- function(mat){

	# Source (python): http://jekel.me/2015/Least-Squares-Sphere-Fit/

	# Make the A matrix
    A <- cbind(mat*2, rep(1, nrow(mat)))
    
    # Make the f matrix
    f <- matrix(0, nrow(mat), 1)
    f[, 1] = (mat[, 1]*mat[, 1]) + (mat[, 2]*mat[, 2]) + (mat[, 3]*mat[, 3])

	# Solve equations
	a_si <- tryCatch({
		solve(t(A) %*% A)
	}, warning = function(cond) {
		return(NULL)
	}, error = function(cond) {
		return(NULL)
	})

	if(is.null(a_si)) return(NULL)

	ab <- t(A) %*% f
	cen <- a_si %*% ab

	# Solve for radius
	rad2 = (cen[1]*cen[1])+(cen[2]*cen[2])+(cen[3]*cen[3])+cen[4]
    radius = sqrt(rad2)

	rlist <- list(
    	'C'=cen[1:3],
    	'R'=radius
    )

	class(rlist) <- 'sphere'
	
	rlist
}