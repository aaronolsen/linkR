rmToVA <- function(m){

	## Converts rotation matrix to axis and angle
	# From http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToAngle/

	epsilon <- 0.01
	epsilon2 <- 0.1

	if ((abs(m[1,2]-m[2,1]) < epsilon) && (abs(m[1,3]-m[3,1]) < epsilon) && (abs(m[2,3]-m[3,2]) < epsilon)) {

		# singularity found
		# first check for identity matrix which must have +1 for all terms
		#  in leading diagonaland zero in other terms
		if (((m[1,2]+m[2,1]) < epsilon2) && ((m[1,3]+m[3,1]) < epsilon2) && ((m[2,3]+m[3,2]) < epsilon2) && ((m[1,1]+m[2,2]+m[3,3]-3) < epsilon2)) {
			# this singularity is identity matrix so angle <- 0
		   return(list('angle'=0, 'vector'=c(1,0,0)))	# zero angle, arbitrary axis
		}
		# otherwise this singularity is angle = 180
		angle <- pi
		xx <- (m[1,1]+1)/2
		yy <- (m[2,2]+1)/2
		zz <- (m[3,3]+1)/2
		xy <- (m[1,2]+m[2,1])/4
		xz <- (m[1,3]+m[3,1])/4
		yz <- (m[2,3]+m[3,2])/4
		if ((xx > yy) && (xx > zz)) { # m[1,1] is the largest diagonal term
			if (xx< epsilon) {
				x <- 0
				y <- 0.7071
				z <- 0.7071
			} else {
				x <- sqrt(xx)
				y <- xy/x
				z <- xz/x
			}
		} else if (yy > zz) { # m[2,2] is the largest diagonal term
			if (yy< epsilon) {
				x <- 0.7071
				y <- 0
				z <- 0.7071
			} else {
				y <- sqrt(yy)
				x <- xy/y
				z <- yz/y
			}	
		} else { # m[3,3] is the largest diagonal term so base result on this
			if (zz< epsilon) {
				x <- 0.7071
				y <- 0.7071
				z <- 0
			} else {
				z <- sqrt(zz)
				x <- xz/z
				y <- yz/z
			}
		}
	   return(list('angle'=angle, 'vector'=c(x,y,z)))
	}

	# as we have reached here there are no singularities so we can handle normally
	s <- sqrt((m[3,2] - m[2,3])*(m[3,2] - m[2,3]) + (m[1,3] - m[3,1])*(m[1,3] - m[3,1]) + (m[2,1] - m[1,2])*(m[2,1] - m[1,2])) # used to normalise
	if ((s) < 0.001) s <- 1 
		# prevent divide by zero, should not happen if matrix is orthogonal and should be
		# caught by singularity test above, but I've left it in just in case
	angle <- acos(( m[1,1] + m[2,2] + m[3,3] - 1)/2)
	x <- (m[3,2] - m[2,3])/s
	y <- (m[1,3] - m[3,1])/s
	z <- (m[2,1] - m[1,2])/s
   return(list('angle'=angle, 'vector'=c(x,y,z)))
}