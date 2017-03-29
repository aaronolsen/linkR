fitCircle2D <- function(mat){

	# MATH SOURCE: www.dtcenter.org/met/users/docs/write_ups/circle_fit.pdf

	# NUMBER OF POINTS
	N <- nrow(mat)

	# POINT MEAN
	xb <- mean(mat[, 1])
	yb <- mean(mat[, 2])

	# CIRCLE CENTER IN TRANSFORMED COORDINATE SYSTEM
	u <- mat[, 1] - xb
	v <- mat[, 2] - yb

	Suu <- sum(u^2)
	Suuu <- sum(u^3)
	Svv <- sum(v^2)
	Svvv <- sum(v^3)
	Suv <- sum(u*v)
	Suvv <- sum(u*v*v)
	Svuu <- sum(v*u*u)
	
	# SET UP LINEAR EQUATIONS TO SOLVE
	a <- matrix(c(Suu, Suv, Suv, Svv), nrow=2)
	b <- matrix(c((Suuu + Suvv)/2, (Svvv + Svuu)/2), nrow=2)

	# Solve equations
	a_si <- solve(t(a) %*% a)
	ab <- t(a) %*% b
	sol <- a_si %*% ab

	# CIRCLE CENTER IN ORIGINAL COORDINATE SYSTEM
	xy <- c(sol[1] + xb, sol[2] + yb)

	# CIRCLE RADIUS
	alpha <- sol[1]^2 + sol[2]^2 + (Suu + Svv)/N
	r <- sqrt(alpha)

	list('C' = c(sol[1] + xb, sol[2] + yb), 'R' = r)
}