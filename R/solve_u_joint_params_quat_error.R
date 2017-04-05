solve_u_joint_params_quat_error <- function(p, q, f.axis, r.axis, vo=NULL, mat=diag(3)){

	# Make axes into matrix
	aor_u <- rbind(f.axis, r.axis)

	# Rotate axes as matrix
	if(length(p) == 3){
		aor_ur <- aor_u %*% rotationMatrixZYX(p)
	}else{
		aor_ur <- aor_u %*% tMatrixEP(vo, p[1])
		aor_ur <- aor_ur %*% tMatrixEP(aor_ur[1, ], p[2])
	}

	# Get axes back
	f.axis <- aor_ur[1,]
	r.axis <- aor_ur[2,]

	n_q <- nrow(q)
	errors <- rep(NA, n_q)

	for(i in 1:n_q){
	
		# Get quaternions
		u_jt_params <- solveUJointParamsQuat(q[i, ], f.axis, r.axis, mat)
		
		# Re-orient r.axis
		r.axis <- u_jt_params$r.axis.r

		# Save error
		errors[i] <- u_jt_params$error
	}

	mean(errors)
}