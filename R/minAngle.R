minAngle <- function(angle, units='rad'){

	if(is.matrix(angle)) return(apply(angle, 1:2, minAngle, units=units))
	if(is.vector(angle) && length(angle) > 1) return(sapply(angle, minAngle, units=units))

	rmdr <- sign(angle)*(abs(angle) %% (2*pi))

	if(units == 'rad'){full <- 2*pi}else{full <- 360}
	mins <- c(rmdr, -sign(rmdr)*abs((2*pi)-rmdr), -sign(rmdr)*abs((2*pi)+rmdr))

	min_angle <- mins[which.min(abs(mins))]
	
	if((min_angle + pi) < 1e-10) min_angle <- pi
	
	min_angle
}
