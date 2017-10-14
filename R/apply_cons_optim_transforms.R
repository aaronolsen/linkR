apply_cons_optim_transforms <- function(p, cons, type, dof){

	## Function that takes an input vector to an optimization routine and uses it to 
	## transform joint constraint parameters. The joint constraint parameters cannot be 
	## input into the optimization directly because they are not all independent. For 
	## example the second and third rotational and translational axes are orthogonal to the
	## first. This also reduces the number of optimization parameters needed.

	# Create constraint vector
	cons_out <- cons

	# Rotate constraint parameters using optimize parameters
	if(type == 'L') cons_out[1:3] <- cons[1:3] %*% rotationMatrixZYX(p[1:3])
	if(type == 'P'){
		cons_out[1:3] <- cons[1:3] %*% rotationMatrixZYX(p[1:3])
		cons_out[4:6] <- (cons[4:6] %*% rotationMatrixZYX(p[1:3])) %*% tMatrixEP(cons_out[1:3], p[4])
	}

	if(dof['R'] > 0) cons_out[1:3] <- p[1:3]
	if(type == 'R'){
		cons_out[4:6] <- cons[4:6] %*% rotationMatrixZYX(p[4:6])
	}
	if(type %in% c('U', 'V')){
		cons_out[4:6] <- cons[4:6] %*% rotationMatrixZYX(p[4:6])
		cons_out[7:9] <- (cons[7:9] %*% rotationMatrixZYX(p[4:6])) %*% tMatrixEP(cons_out[4:6], p[7])
	}

	cons_out
}
