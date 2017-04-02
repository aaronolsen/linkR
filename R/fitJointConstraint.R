fitJointConstraint <- function(coor, type, print.progress = FALSE){

	# Set number of coordinates
	n_coor <- dim(coor)[1]

	# Set number of iterations
	n_iter <- dim(coor)[3]

	# Fit single rotational axis constraint
	if(type %in% c('L', 'P')){
	
		# Vectors
		vecs <- matrix(NA, n_coor, 3)

		# Fit a 3D line to the position of each body point over time
		for(i in 1:n_coor){

			if(type == 'L'){
				fit_vector <- fitShape(t(coor[i, , ]), 'vector')$V
			}else if(type == 'P'){
				fit_vector <- fitShape(t(coor[i, , ]), 'plane')$N
			}

			if(is.null(fit_vector)) next
			vecs[i, ] <- fit_vector
		}

		# Remove NA rows
		vecs <- vecs[!is.na(vecs[,1]), ]

		# Find single vector
		if(nrow(vecs) == 1){
			vec <- vecs
		}else{
			# Flip vectors that are in opposite direction from first
			for(i in 2:nrow(vecs)) if(avec(vecs[1, ], vecs[i, ]) > pi/2) vecs[i, ] <- -vecs[i, ]

			# Find average vector
			vec <- uvector(colMeans(vecs, na.rm=TRUE))
		}
		
		# Set joint constraint
		if(type == 'L'){
			joint_cons <- vec
		}else if(type == 'P'){
			vec_o <- vorthogonal(vec)
			joint_cons <- c(vec_o, cprod(vec_o, vec))
		}
		
	}else if(type %in% c('R', 'U')){

		# Find centroid size of each point over time (for weights)
		Csizes <- apply(coor, 1, 'centroidSize', transpose=TRUE)

		# Parameters
		CoRs <- matrix(NA, n_coor, 3)
		rads <- rep(NA, n_coor)
		if(type == 'R') AoRs <- matrix(NA, n_coor, 3)

		# Fit circle/sphere to the set of positions of each body point over time
		for(i in 1:n_coor){

			if(type == 'R'){

				# Align points by centroid
				centroid_align <- t(coor[i, , ]) - matrix(colMeans(t(coor[i, , ])), n_iter, dim(coor)[2], byrow=TRUE)

				# Fit circles to each point across iterations
				fit_shape <- fitShape(t(coor[i, , ]), 'circle', centroid.align=centroid_align)

			}else if (type == 'U'){

				# Fit spheres to each point across iterations
				fit_shape <- fitShape(t(coor[i, , ]), 'sphere')
			}

			# If NULL, skip
			if(is.null(fit_shape)) next

			# Save center, axis of rotation and radius
			CoRs[i, ] <- fit_shape$C
			rads[i] <- fit_shape$R
			if(type == 'R') AoRs[i, ] <- fit_shape$N
		}
		
		# Remove NA rows
		CoRs <- CoRs[!is.na(CoRs[,1]), ]
		rads <- rads[!is.na(rads)]
		if(type == 'R') AoRs <- AoRs[!is.na(AoRs[,1]), ]

		# Find single CoR and AoR
		if(nrow(CoRs) == 1){
			CoR <- CoRs
			if(type == 'R') AoR <- AoRs
		}else{

			# Flip AoRs that are in opposite direction from first
			if(type == 'R') for(i in 2:nrow(AoRs)) if(avec(AoRs[1, ], AoRs[i, ]) > pi/2) AoRs[i, ] <- -AoRs[i, ]

			# Find average CoR and AoR, weighting each iteration by centroid size of points
			# Using centroid size is better than radius because noisy points in straight line over small range can lead to false large radius
			CoR <- colSums(CoRs*Csizes, na.rm=TRUE) / sum(Csizes, na.rm=TRUE)
			if(type == 'R') AoR <- uvector(colSums(AoRs*Csizes, na.rm=TRUE) / sum(Csizes, na.rm=TRUE))
		}
		
		if(type == 'U'){

			if(print.progress) cat(paste0('\tEstimating U-joint axis orientations\n'))

			# Find instantaneous AoR and angle between each iteration
			inst_aor <- matrix(NA, nrow=n_iter-1, ncol=4)
			for(iter in 2:n_iter){
				inst_aor[iter-1, ] <- unlist(instRotate(coor[, , iter-1], coor[, , iter], CoR=CoR))[c(1:3,7)]
			}

			# Find cross product between consecutive aors
			c_prod <- matrix(NA, nrow(inst_aor)-1, 3)
			for(i in 2:(n_iter-1)) c_prod[i-1, ] <- cprod(inst_aor[i-1, 1:3], inst_aor[i, 1:3])

			# Flip axes when more than 90 degrees apart (opposite)
			for(i in 2:(n_iter-2)) if(abs(avec(c_prod[1,], c_prod[i,])) > pi/2) c_prod[i,] <- -c_prod[i,]

			# Get angle weights (for one of cross product pair)
			angle_wts <- 1/abs(inst_aor[2:nrow(inst_aor), 4])
			angle_wts <- (angle_wts - min(angle_wts)) / diff(range(angle_wts))

			# Find average cross product vector, inversely weighted by angle magnitude
			# Should be close to the plane orthogonal to the fixed (first) AoR
			fao_plane <- uvector(colSums(c_prod*angle_wts) / sum(angle_wts))
			#print((pi/2-avec(c(1,0,0), fao_plane))*(180/pi))
			
			# Starting guess for fixed AoR - perpendicular to fao plane
			aor_f <- vorthogonal(fao_plane)
			
			# Starting guess for mobile AoR - perpendicular to aor f
			aor_r <- cprod(aor_f, fao_plane)

			# Find quaternion equivalent for each instantaneous rotation
			qua <- matrix(NA, n_iter-1, 4)
			for(i in 1:nrow(inst_aor)) qua[i, ] <- axisAngle2Quat(inst_aor[i, 1:3], inst_aor[i, 4])

#return(1)
			if(TRUE){

				start_p <- c(0,0)

				# Find starting parameter
				ni <- nj <- 20
				error_mat <- matrix(NA, ni, nj)
				ij_seq <- seq(0,pi,length=ni)
				q_seq <- 1:min(nrow(qua), 25) # needs to be consecutive because rotating AoR is updated after each iteration
				for(i in 1:ni) for(j in 1:nj) error_mat[i,j] <- solve_u_joint_params_quat_error(c(ij_seq[i], ij_seq[j]), qua[q_seq, ], aor_f, aor_r, vo=fao_plane)

				# Create matrix to look up parameters corresponding to lowest error			
				error_at <- cbind(c(matrix(ij_seq,ni,nj,byrow=TRUE)), c(matrix(ij_seq,ni,nj,byrow=FALSE)), c(t(error_mat)))

				# Find minimum
				which_min <- which.min(error_at[, 3])

				# Set as starting parameters for optimization
				start_p <- error_at[which_min, 1:2]
			
				# Create diagnostic plot
				#error_mat_n <- (error_mat - min(error_mat)) / max(error_mat - min(error_mat))
				#error_vec_n <- c(t(error_mat_n))
				#plot(error_at[,1:2], col=gray(1-error_vec_n))
				#points(start_p[1], start_p[2], cex=2, col='blue')

				if(print.progress) cat(paste0('\t\tError after sampling 2-parameter space: ', round(error_at[which_min, 3], 7), '\n'))

				# Get axes orientations
				aor_u <- rbind(aor_f, aor_r)
				aor_ur <- aor_u %*% tMatrixEP(fao_plane, start_p[1])
				aor_ur <- aor_ur %*% tMatrixEP(aor_ur[1, ], start_p[2])
				aor_f <- aor_ur[1,]
				aor_r <- aor_ur[2,]
			}
			
			if(TRUE){

				q_seq <- 1:min(nrow(qua), 50) # needs to be consecutive within fitJointConstraint because rotating AoR is updated after each iteration

				u_joint_solve <- tryCatch(
					expr={
						nlminb(start=c(0,0,0), objective=solve_u_joint_params_quat_error, 
							lower=c(-pi,-pi,-pi), upper=c(pi,pi,pi), q=qua[q_seq, ], f.axis=aor_f,
							r.axis=aor_r)
					},
					error=function(cond) {print(cond);return(NULL)},
					warning=function(cond) {print(cond);return(NULL)}
				)

				if(print.progress) cat(paste0('\t\tError after optimizing 3-parameter space: ', round(u_joint_solve$objective, 7), '\n'))

				aor_fo <- aor_f %*% rotationMatrixZYX(u_joint_solve$par)
				aor_ro <- aor_r %*% rotationMatrixZYX(u_joint_solve$par)
			}

			if(FALSE){

				svg.new('Test.html')
				svg.frame(coor)
			#	for(i in 1:dim(coor)[3]) svg.pointsC(coor[, , i], col.stroke=c('red', 'green', 'blue'), close=TRUE, opacity.stroke.C=0.25)
			#	svg.pointsC(coor, col.stroke=c('red', 'green', 'blue'), close=TRUE, opacity.stroke.C=0.25)
				svg.arrows(rbind(c(0,0,0), 0.5*c(1,0,0)), col='orange')
				svg.arrows(rbind(c(0,0,0), aor_f), col='orange', lwd=2)
				svg.arrows(rbind(c(0,0,0), 1.5*aor_fo), col='orange', lwd=2)
				svg.arrows(rbind(c(0,0,0), 0.5*c(0,0,1)), col='brown')
				svg.arrows(rbind(c(0,0,0), aor_r), col='brown', lwd=2)
				svg.arrows(rbind(c(0,0,0), 1.5*aor_ro), col='brown', lwd=2)
				svg.arrows(rbind(CoR, CoR+0.5*fao_plane), col='red', lwd=2)

				for(i in 1:nrow(c_prod)) svg.arrows(rbind(c(0,0,0), 2*c_prod[i, 1:3]), col='purple', opacity=0.4)
		#		for(i in 1:nrow(c_prod)) svg.arrows(rbind(c(0,0,0), 2*inst_aor[i, 1:3]), col='purple', opacity=0.4)

				svg.close()
			}
		}

		# Set joint constraints
		if(type == 'R') joint_cons <- c(CoR, AoR)
		if(type == 'U') joint_cons <- c(CoR, uvector(aor_fo), uvector(aor_ro))
	}

	return(list(
		'cons'=joint_cons
	))
}