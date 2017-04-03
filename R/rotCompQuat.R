rotCompQuat <- function(q, v1, v2 = NULL, mat = diag(3), v2.solve = FALSE, v2.ortho = FALSE){

	## Finds the rotational components about 1 or 2 vectors from a quaternion

	# Check whether to return single angle or two
	return_single <- FALSE
	if(is.null(v2)){
		v2 <- v1
		return_single <- TRUE
	}

	# Apply to set of non-collinear coordinates
	mat_r <- mat %*% quat2RM(q)

	# Swing-twist decomposition
	st1 <- swingTwistDecomp(q=q, v=v2)
	#print(st1)
	
	# Fix edge issues
	if(abs(st1$twist.angle) == pi/2) st1$twist.angle <- st1$twist.angle + 1e-10
	if(abs(st1$twist.angle) > pi) st1$twist.angle <- pi - abs(st1$twist.angle)

	if(!v2.solve && return_single){

		v_err <- c(
			sqrt(mean((mat %*% tMatrixEP(v2, -st1$twist.angle) - mat_r)^2)),
			sqrt(mean((mat %*% tMatrixEP(v2, st1$twist.angle) - mat_r)^2))
		)
		
		# Find minimum error
		which_min <- which.min(v_err)
		
		return(list(
			'angles'=-c(-1,1)[which_min]*st1$twist.angle,
			'error'=v_err[which_min]
		))
	}

	# Subtract AoR1 rotational component by reversing twist
	# Find remaining component
	#mat_s2 <- coor[, , iter] %*% tMatrixEP(v2, -st1$twist.angle)
	#inst_rotate <- instRotate(mat_s2, coor[, , iter-1], CoR=c(0,0,0))
	#st2 <- swingTwistDecomp(q=axisAngle2Quat(inst_rotate$AoR, inst_rotate$angle), v=v1)

	if(v2.solve){

		# Reverse twist on quaternion
		q_rn <- qprod(axisAngle2Quat(v2, -st1$twist.angle), q)
		q_rp <- qprod(axisAngle2Quat(v2, st1$twist.angle), q)

		# Solve for remaining rotation
		q_aa_n <- quat2AxisAngle(q_rn)
		q_aa_p <- quat2AxisAngle(q_rp)
		
		v1_mat <- rbind(q_aa_p$axis, q_aa_p$axis, q_aa_n$axis, q_aa_n$axis)
		v1_angles <- c(-q_aa_p$angle, q_aa_p$angle, -q_aa_n$angle, q_aa_n$angle)
		v2_angles <- c(-st1$twist.angle, st1$twist.angle, st1$twist.angle, st1$twist.angle)
		
		v_err <- rep(NA, nrow(v1_mat))
		for(i in 1:length(v_err)){
			v_err[i] <- sqrt(mean((mat %*% tMatrixEP(v1_mat[1, ], v1_angles[i]) %*% tMatrixEP(v2, -v2_angles[i]) - mat_r)^2))
		}
		
		#print(v_err)

		# Find minimum error
		which_min <- which.min(v_err)
		
		# Find vector that is closest to v1 and orthogonal to v2
		if(v2.ortho){

			# Set values
			v1 <- v1_mat[which_min,]
	
			# Make sure v1 is orthogonal to v2
			v1 <- cprod(v2, cprod(v1, v2))

		}else{

			return(list(
				'angles'=c(v1_angles[which_min], v2_angles[which_min]), 
				'v1'=uvector(v1_mat[which_min,]),
				'error'=v_err[which_min]
			))
		}
	}

	# Reverse twist on quaternion
	q_r <- qprod(axisAngle2Quat(v2, -st1$twist.angle), q)

	# Find rotational component about second axis
	st2 <- swingTwistDecomp(q=q_r, v=v1)

	# Fix edge issues
	if(abs(st2$twist.angle) == pi) st2$twist.angle <- st2$twist.angle + 1e-10
	if(abs(st2$twist.angle) > pi) st2$twist.angle <- pi - abs(st2$twist.angle)

	# Find combination of angles that gives the lowest error (which corresponds to original quaternion)
	v_err <- c(
		sqrt(mean((mat %*% tMatrixEP(v1, -st2$twist.angle) %*% tMatrixEP(v2, -st1$twist.angle) - mat_r)^2)),
		sqrt(mean((mat %*% tMatrixEP(v1, st2$twist.angle) %*% tMatrixEP(v2, -st1$twist.angle) - mat_r)^2)),
		sqrt(mean((mat %*% tMatrixEP(v1, -st2$twist.angle) %*% tMatrixEP(v2, st1$twist.angle) - mat_r)^2)),
		sqrt(mean((mat %*% tMatrixEP(v1, st2$twist.angle) %*% tMatrixEP(v2, st1$twist.angle) - mat_r)^2))
	)
	
	# Find minimum error
	which_min <- which.min(v_err)
	angles <- c(c(-1,1,-1,1)[which_min]*st2$twist.angle, c(-1,-1,1,1)[which_min]*st1$twist.angle)

#print(v_err)

	return(list(
		'angles'=angles,
		'v1'=uvector(v1), 
		'error'=v_err[which_min]
	))
}