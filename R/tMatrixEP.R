tMatrixEP <- function(v, a){
	# Top version from https://en.wikipedia.org/wiki/Rotation_matrix
	# Bottom from ?
	# Both give the same result

	# For some reason the resulting rotation matrix does not follow the right-hand rule... Flip angle so that it is right-hand
	a <- -a

	v <- uvector(v)

	r <- matrix(0, 3, 3)
	r[1, ] <- c(cos(a)+v[1]^2*(1-cos(a)), v[1]*v[2]*(1-cos(a))-v[3]*sin(a), v[1]*v[3]*(1-cos(a))+v[2]*sin(a))
	r[2, ] <- c(v[2]*v[1]*(1-cos(a))+v[3]*sin(a), cos(a) + v[2]^2*(1-cos(a)), v[2]*v[3]*(1-cos(a))-v[1]*sin(a))
	r[3, ] <- c(v[3]*v[1]*(1-cos(a))-v[2]*sin(a), v[3]*v[2]*(1-cos(a))+v[1]*sin(a), cos(a)+v[3]^2*(1-cos(a)))
	
	return(r)

	v <- uvector(v)

	t0 <- cos(a/2)
	t1 <- v[1]*sin(a/2)
	t2 <- v[2]*sin(a/2)
	t3 <- v[3]*sin(a/2)

	r <- matrix(0, 3, 3)
	r[1, ] <- c(2*(t0^2 + t1^2) - 1, 2*(t1*t2 - t0*t3), 2*(t1*t3 + t0*t2))
	r[2, ] <- c(2*(t1*t2 + t0*t3), 2*(t0^2 + t2^2) - 1, 2*(t2*t3 - t0*t1))
	r[3, ] <- c(2*(t1*t3 - t0*t2), 2*(t2*t3 + t0*t1), 2*(t0^2 + t3^2) - 1)

	return(r)
}
