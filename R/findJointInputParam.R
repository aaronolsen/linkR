findJointInputParam <- function(type, coor, cons, ref, poses, zero = 1e-10){
	
	## Finds input parameter to transform 'ref' to match 'poses'

	#
	if(!is.matrix(cons)) cons <- matrix(cons, nrow=1)
		
	# Get number of iterations
	n_iter <- 1
	if(class(poses) == 'array') n_iter <- dim(poses)[3]
	
	# 
	if(type == 'R') inputs <- matrix(NA, nrow=n_iter, ncol=1)
	if(type == 'U') inputs <- matrix(NA, nrow=n_iter, ncol=2)
	if(type == 'O') inputs <- matrix(NA, nrow=n_iter, ncol=3)

	if(n_iter > 1){
		for(iter in 1:n_iter){
			inputs[iter, ] <- findJointInputParam(type=type, coor=coor, cons=cons, ref=ref, poses=poses[, , iter])
		}
		return(inputs)
	}

	# Place center of rotation at 0
	ref <- ref - matrix(coor, nrow=nrow(ref), ncol=3, byrow=TRUE)
	poses <- poses - matrix(coor, nrow=nrow(poses), ncol=3, byrow=TRUE)
	coor <- c(0,0,0)

	if(type == 'R'){
		
		angles <- c(
			avec(poses[1,], ref[1,], axis=cons[1,], about.axis=TRUE),
			avec(poses[2,], ref[2,], axis=cons[1,], about.axis=TRUE),
			avec(poses[3,], ref[3,], axis=cons[1,], about.axis=TRUE)
			)
		
		if(any(abs(angles - pi) < zero)){
			inputs[1,1] <- pi
		}else{
			inputs[1,1] <- mean(angles)
		}
		#print(inputs[1,1])

	}else if(type == 'U'){

		# SVD of cross-covariance matrix
		svd <- svd(t(ref) %*% poses)

		# Correction to ensure a right-handed coordinate system
		S <- diag(3)
		S[3,3] <- sign(det(svd$v %*% t(svd$u)))

		# Get rotation matrix
		RM <- svd$v %*% S %*% t(svd$u)
		
		# Check for identity matrix
		if(sum(abs(RM - diag(3))) > zero){

			# Rotate configuration
			poses_rot <- poses %*% RM

			# Get axis and angle from rotation matrix RM <- tMatrixEP(iva$vector, -iva$angle)
			iva <- rmToVA(RM)
		
			# Find projection of inst rotation vector onto plane normal to AoR1
			irv_proj1 <- pointPlaneProj(iva$vector, c(0,0,0), cons[1,])
			irv_proj2 <- pointPlaneProj(iva$vector, c(0,0,0), cons[2,])

			if(sqrt(sum(irv_proj1^2)) < zero){

				# inst vector is parallel to AoR1, rotation only about first axis
				inputs[1,] <- c(findJointInputParam(type='R', coor=coor, cons=cons[1,], 
					ref=ref, poses=poses), 0)

			}else if(sqrt(sum(irv_proj2^2)) < zero){

				# inst vector is parallel to AoR2, rotation only about second axis
				inputs[1,] <- c(0, findJointInputParam(type='R', coor=coor, cons=cons[2,], 
					ref=ref, poses=poses))

			}else{

				# inst vector is parallel to AoR2, rotation only about second axis
				zero1 <- c(0, findJointInputParam(type='R', coor=coor, cons=cons[2,], 
					ref=ref, poses=poses))

				# inst vector is parallel to AoR1, rotation only about first axis
				zero2 <- c(findJointInputParam(type='R', coor=coor, cons=cons[1,], 
					ref=ref, poses=poses), 0)

				# Find angle between projection and AoR2
				irv_proj1_aor2 <- abs(avec(irv_proj1, cons[2,]))
				irv_proj2_aor1 <- abs(avec(irv_proj2, cons[1,]))

				# Find sign of each angle
				if(irv_proj2_aor1 > pi/2){ sign_angle1 <- -1 }else{ sign_angle1 <- 1 }
				if(irv_proj1_aor2 > pi/2){ sign_angle2 <- -1 }else{ sign_angle2 <- 1 }

				# Find possible rotations about each axis
				if(sign_angle1 == 1){
					aor1_angles <- c(c(2,-2)*irv_proj1_aor2, c(-1,1)*(irv_proj1_aor2*2-2*pi))
				}else{
					aor1_angles <- c(c(2,-2)*irv_proj1_aor2, c(-1,1)*(irv_proj1_aor2*2-2*pi))
				}
				if(sign_angle2 == 1){
					aor2_angles <- c(c(2,-2)*irv_proj2_aor1, c(-1,1)*(irv_proj2_aor1*2-2*pi))
				}else{
					aor2_angles <- c(c(2,-2)*irv_proj2_aor1, c(-1,1)*(irv_proj2_aor1*2-2*pi))
				}

				# Get all possible combinations
				inputs_possible <- matrix(NA, 10, 2)
				inputs_possible[1,] <- c(aor1_angles[1], aor2_angles[1])
				inputs_possible[2,] <- c(aor1_angles[1], aor2_angles[4])
				inputs_possible[3,] <- c(aor1_angles[2], aor2_angles[2])
				inputs_possible[4,] <- c(aor1_angles[2], aor2_angles[3])
				inputs_possible[5,] <- c(aor1_angles[3], aor2_angles[2])
				inputs_possible[6,] <- c(aor1_angles[3], aor2_angles[3])
				inputs_possible[7,] <- c(aor1_angles[4], aor2_angles[1])
				inputs_possible[8,] <- c(aor1_angles[4], aor2_angles[4])
				inputs_possible[9,] <- zero1
				inputs_possible[10,] <- zero2

				# Sort by increasing total rotations
				inputs_possible <- inputs_possible[order(rowSums(abs(inputs_possible))), ]
		
				# Find correct combination
				in_poss_errors <- rep(NA, nrow(inputs_possible))
				for(j in 1:nrow(inputs_possible)){

					# Try transformation
					ref_t <- rotateBody(ref, cons[1,], inputs_possible[j,1], coor)
					AoR2_t<- cons[2,] %*% tMatrixEP(cons[1,], inputs_possible[j,1])
					ref_t <- rotateBody(ref_t, AoR2_t, inputs_possible[j,2], coor)

					# Measure error			
					in_poss_errors[j] <- sum(abs(ref_t - poses))

					# If error is zero, break
					if(in_poss_errors[j] < zero) break
				}

				# Find combination with lowest error
				inputs[1,] <- inputs_possible[which.min(in_poss_errors), ]
			}
		}else{
			inputs[1,] <- c(0,0)
		}

		# Test by trying transformation
		ref_t <- rotateBody(ref, cons[1,], inputs[1,1], coor)
		AoR2_t<- cons[2,] %*% tMatrixEP(cons[1,], inputs[1,1])
		ref_t <- rotateBody(ref_t, AoR2_t, inputs[1,2], coor)

	}

	inputs
}