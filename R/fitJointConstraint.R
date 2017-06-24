fitJointConstraint <- function(coor, type, control = NULL, fixed = NULL, select.t.axis='min', 
	print.progress = FALSE){

	# Set default controls
	if(is.null(control)){
		control <- list(
			'subsample.method'='Evenly spaced',		# or 'Maximum dispersion'
			'max.disp.prop.R'=0.2,					# Proportion of time points used in max dispersion sampling
			'max.disp.prop.S'=0.2,
			'max.disp.prop.U'=0.2
		)
	}

	# If fixed coordinate names are provided, get mobile coordinates relative to fixed
	if(!is.null(fixed)){

		# 3 or more fixed coordinates
		if(length(fixed) >= 3){

			# Fix coordinates using best alignment to first iteration
			coor <- immobilize(coor, coor[fixed, , 1])

		# 2 fixed coordinates
		}else{

		}
	}

	# Set number of coordinates
	n_coor <- dim(coor)[1]

	# Set number of iterations
	n_iter <- dim(coor)[3]

	# Find centroid size of each point "position cloud" over time (for weights)
	if(type %in% c('R', 'U', 'S')) Csizes_time <- apply(coor, 1, 'centroidSize', transpose=TRUE)

	# Try optimizing translation vector such that CoRs are collinear and form a line parallel
	# to the translation vector (3 parameter optimization)
	if(type %in% c('RL')){

		# Get point ranges
		coor_range <- apply(coor, 2, 'range')

		# Find maximum distance for point range
		max_dist <- sqrt(sum((coor_range[2,]-coor_range[1,])^2))

		# Set starting vector orientations - may need to add more
		start <- list(c(1,0,0), c(0,1,0), c(0,0,1))
		
		#error <- linear_cor_trajectory_error(start, coor=coor, max.dist=max_dist*1)

		# Solve for CoRs that form a linear trajectory parallel to translation vector
		objectives <- rep(10, length(start))
		cor_solve <- list()
		for(i in 1:length(start)){
			cor_solve[[i]] <- tryCatch(
				expr={
					nlminb(start=start[[i]], objective=linear_cor_trajectory_error, 
						lower=c(-1,-1,-1), upper=c(1,1,1), coor=coor, max.dist=max_dist*1)
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)
			objectives[i] <- cor_solve[[i]]$objective
			if(cor_solve[[i]]$objective < 1e-4) break
		}
		
		# Save minimum solution
		cor_solve <- cor_solve[[which.min(objectives)]]
		#print(cor_solve)

		if(print.progress) cat(paste0('\t\tError after translation vector estimation: ', round(cor_solve$objective, 7), '\n'))

		# Set translation vector
		tvec <- uvector(cor_solve$par)
		
		# Create matrices for AoR, CoRs
		inst_aor <- matrix(NA, nrow=n_iter-1, ncol=3)
		inst_cor <- matrix(NA, nrow=n_iter-1, ncol=3)
		angles <- rep(NA, n_iter-1)

		# Find instantaneous AoRs and CoRs given translation vector
		for(iter in 2:n_iter){
			
			# Decompose rotation and translation given translation vector
			rtdr <- rtd(coor[, , iter-1], coor[, , iter], tvec=tvec)
			inst_aor[iter-1, ] <- rtdr$aor
			inst_cor[iter-1, ] <- rtdr$cor
			angles[iter-1] <- rtdr$angle
			
			# Flip axes when more than 90 degrees apart or exactly opposite
			if(iter > 2){
				if(abs(avec(inst_aor[1,], inst_aor[iter-1,])) > pi/2) inst_aor[iter-1,] <- -inst_aor[iter-1,]
				if(sum(inst_aor[1,]+inst_aor[iter-1,]) < 1e-8) inst_aor[iter-1,] <- -inst_aor[iter-1,]
			}
		}

		# Find major AoR by pca
		pca <- prcomp(angles*inst_aor)
		avg_aor <- pca$rotation[, 1]

		# Fit line to CoRs
		fit_line <- fitLine3D(inst_cor)

		# Find point on line closest to first CoR point
		cor_init <- pointNormalOnLine(inst_cor[1, ], fit_line$p1, fit_line$p2)
		
		# Set joint constraints
		joint_cons <- c(cor_init, avg_aor, tvec)
		
		#
		if(FALSE){
			svg.new('Test.html')
			svg.frame(coor)
			cols <- c('red', 'green', 'blue')
			for(i in 1:dim(coor)[1]) svg.pointsC(t(coor[i, , ]), col.stroke.C=cols[i])
			svg.lines(inst_cor, col='blue')
			#svg.lines(rbind(fit_line$p1, fit_line$p2), col='blue')
			svg.points(inst_cor, col='blue', cex=0.5)
		
			svg.lines(rbind(colMeans(coor_range), colMeans(coor_range)+tvec), col='purple')

			#for(i in 1:nrow(inst_aor)) svg.arrows(rbind(centroid[i, ], centroid[i, ] + 0.5*inst_aor[i, ]), col='purple', opacity=0.4)

			svg.close()
		}
	} else if(type %in% c('RL2')){
	
		# Find centroid over time
		centroid <- t(apply(coor, 3, 'colMeans'))

		# Fit cylinder to centroid of points across iterations
		fit_ecylinder <- fitEllipticCylinder(centroid, select.axis=select.t.axis)
		
		# Set central axis of cylinder as translation vector
		tvec <- uvector(fit_ecylinder$N)

		# Create matrices for AoR, CoRs
		inst_aor <- matrix(NA, nrow=n_iter-1, ncol=3)
		inst_cor <- matrix(NA, nrow=n_iter-1, ncol=3)
		angles <- rep(NA, n_iter-1)

		# Find instantaneous AoRs and CoRs given translation vector
		for(iter in 2:n_iter){
			
			# Decompose rotation and translation given translation vector
			rtdr <- rtd(coor[, , iter-1], coor[, , iter], tvec=tvec)
			inst_aor[iter-1, ] <- rtdr$aor
			inst_cor[iter-1, ] <- rtdr$cor
			angles[iter-1] <- rtdr$angle
			
			# Flip axes when more than 90 degrees apart or exactly opposite
			if(iter > 2){
				if(abs(avec(inst_aor[1,], inst_aor[iter-1,])) > pi/2) inst_aor[iter-1,] <- -inst_aor[iter-1,]
				if(abs(sum(inst_aor[1,]+inst_aor[iter-1,])) < 1e-8) inst_aor[iter-1,] <- -inst_aor[iter-1,]
			}
		}

		# Find major AoR by pca
		pca <- prcomp(angles*inst_aor)
		avg_aor <- pca$rotation[, 1]

		# Fit line to CoRs
		fit_line <- fitLine3D(inst_cor)
		
		# Find point on line closest to first CoR point
		cor_init <- pointNormalOnLine(inst_cor[1, ], fit_line$p1, fit_line$p2)

		# Set joint constraints
		joint_cons <- c(cor_init, avg_aor, tvec)

		if(FALSE){
			svg.new('Test.html')
			svg.frame(coor)

			cols <- c('red', 'green', 'blue')
			for(i in 1:dim(coor)[1]) svg.pointsC(t(coor[i, , ]), col.stroke.C=cols[i])
			svg.points(centroid)
			svg.lines(inst_cor, col='blue')
			#svg.lines(rbind(fit_line$p1, fit_line$p2), col='blue')
			svg.points(c(0.3,0.5,0.4), col='brown')

			svg.points(joint_cons[1:3], col='blue', cex=3)
			svg.arrows(rbind(joint_cons[1:3], joint_cons[1:3]+tvec), col='blue')

			for(i in 1:nrow(inst_aor)) svg.arrows(rbind(centroid[i, ], centroid[i, ] + 0.5*inst_aor[i, ]), col='purple', opacity=0.4)

			svg.close()
		}
	} else if(type %in% c('L', 'P')){
	
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
		
	}else if(type %in% c('R', 'U', 'S')){

		# Parameters
		CoRs <- matrix(NA, n_coor, 3)
		rads <- rep(NA, n_coor)
		AoRs <- matrix(NA, n_coor, 3)
		
		#
		if(type == 'S') max_disp_prop <- control$max.disp.prop.S
		if(type == 'R') max_disp_prop <- control$max.disp.prop.R
		if(type == 'U') max_disp_prop <- control$max.disp.prop.U

		# Use n% most dispersed points, in order of dispersion
		sub_max <- round(max_disp_prop*dim(coor)[3])
		max_disp <- whichMaxDisperse(t(coor[which.max(Csizes_time), , ]), n=sub_max)

		# Set as subset
		coor_s <- coor[, , max_disp]
		n_iter_s <- length(max_disp)

		# Find instantaneous AoRs and CoRs angle between each iteration
		inst_aor <- matrix(NA, nrow=n_iter_s-1, ncol=3)
		inst_cor <- matrix(NA, nrow=n_iter_s-1, ncol=3)
		angles <- rep(NA, n_iter_s-1)
		for(iter in 2:n_iter_s){

			inst_rotate <- instRotate(coor_s[, , iter-1], coor_s[, , iter])
			inst_aor[iter-1, ] <- inst_rotate$AoR
			inst_cor[iter-1, ] <- inst_rotate$CoR
			angles[iter-1] <- inst_rotate$angle

			# Flip axes when more than 90 degrees apart or exactly opposite
			if(iter > 2){
				#if(abs(avec(inst_aor[1,], inst_aor[iter-1,])) > pi/2) inst_aor[iter-1,] <- -inst_aor[iter-1,]
				#if(sum(inst_aor[1,]+inst_aor[iter-1,]) < 1e-8) inst_aor[iter-1,] <- -inst_aor[iter-1,]
			}
		}

		# Find major AoR by pca
		pca <- prcomp(angles*inst_aor)
		avg_aor <- pca$rotation[, 1]

		# Find CoR
		# CoR can be any point on AoR so find average CoR by finding a point in space at minimum distance from all AoRs
		# If all the AoRs intersect, this will be the intersection point of all the AoRs
		min_cor <- pointMinDistLines(inst_cor, inst_cor+inst_aor)$p
		
		CoR <- min_cor
		AoR <- avg_aor

		if(FALSE){
			svg.new('Test2.html')
			svg.frame(coor)
			cols <- c('red', 'green', 'blue')
			for(i in 1:dim(coor)[1]) svg.points(t(coor[i, , ]), col.stroke=cols[i], opacity.fill=0.2, opacity.stroke=0.2)
			for(i in 1:dim(coor_s)[1]) svg.pointsC(t(coor_s[i, , ]), col.stroke.C=cols[i])
			for(i in 1:nrow(inst_cor)) svg.arrows(rbind(inst_cor[i, ]-uvector(inst_aor[i, ]), inst_cor[i, ]+1*uvector(inst_aor[i, ])), col='purple')
			for(i in 1:nrow(inst_cor)) svg.arrows(rbind(CoR, CoR+100*angles[i]*uvector(inst_aor[i, ])), col='purple')
			svg.arrows(rbind(CoR, CoR+10*avg_aor), col='blue')
			svg.points(min_cor, col='blue', cex=5)
			#svg.points(c(0.3, 0.5, 0.4), col='green', cex=5)
			svg.close()
		}

		if(type == 'R') return(list('cons'=c(CoR, AoR)))

		if(type == 'S') return(list('cons'=c(CoR, c(1,0,0), c(0,1,0), c(0,0,1))))

		if(type == 'U'){

			if(print.progress) cat(paste0('\tEstimating U-joint axis orientations\n'))

			# Change iAoR matrix format
			inst_aor <- cbind(inst_aor, angles)

			# Find cross product between consecutive aors
			c_prod <- matrix(NA, nrow(inst_aor)-1, 3)
			for(i in 2:nrow(inst_aor)) c_prod[i-1, ] <- cprod(inst_aor[i-1, 1:3], inst_aor[i, 1:3])

			# Flip axes when more than 90 degrees apart (opposite)
			for(i in 2:nrow(c_prod)) if(abs(avec(c_prod[1,], c_prod[i,])) > pi/2) c_prod[i,] <- -c_prod[i,]

			# Get angle weights (for one of cross product pair)
			angle_wts <- 1/abs(inst_aor[2:nrow(inst_aor), 4])
			angle_wts <- (angle_wts - min(angle_wts)) / diff(range(angle_wts))

			# Find average cross product vector, inversely weighted by angle magnitude
			# Should be close to the plane orthogonal to the fixed (first) AoR
			fao_plane <- uvector(colSums(c_prod*angle_wts) / sum(angle_wts))
			#fao_plane <- uvector(colMeans(c_prod))
			
			#print((pi/2-avec(c(1,0,0), fao_plane))*(180/pi))
			
			# Starting guess for fixed AoR - perpendicular to fao plane
			aor_f <- vorthogonal(fao_plane)
			
			# Starting guess for mobile AoR - perpendicular to aor f
			aor_r <- cprod(aor_f, fao_plane)

			# Find quaternion equivalent for each instantaneous rotation
			qua <- matrix(NA, nrow(inst_aor), 4)
			for(i in 1:nrow(inst_aor)) qua[i, ] <- axisAngle2Quat(inst_aor[i, 1:3], inst_aor[i, 4])

			if(FALSE){

				svg.new('Test1.html')
				svg.frame(coor)
				for(i in 1:length(max_disp)) svg.pointsC(coor[, , max_disp[i]], col.stroke=c('red', 'green', 'blue'), close=TRUE, opacity.stroke.C=0.25)
				svg.arrows(rbind(CoR, CoR+aor_f), col='orange', lwd=2)
				svg.arrows(rbind(CoR, CoR+aor_r), col='brown', lwd=2)
				svg.arrows(rbind(CoR, CoR+fao_plane), col='red', lwd=2)
				svg.arrows(rbind(CoR, CoR+AoR), col='blue', lwd=2)

				for(i in 1:nrow(c_prod)) svg.arrows(rbind(CoR, CoR+c_prod[i, 1:3]), col='purple', opacity=0.4)
				#for(i in 1:nrow(inst_aor)) svg.arrows(rbind(CoR, CoR+inst_aor[i, 4]*1*inst_aor[i, 1:3]), col='purple', opacity=0.4)
				#for(i in 1:nrow(c_prod)) svg.arrows(rbind(CoR, CoR+10*c_prod[i, 1:3]), col='purple', opacity=0.4)

				svg.close()
			}

#return(1)
			## Simple search for best 2 starting parameters
			## Could rewrite as 2-parameter optimization
			# Create error matrix
			ni <- nj <- 20
			error_mat <- matrix(NA, ni, nj)
			ij_seq <- seq(0,pi,length=ni)
			q_seq <- 1:min(nrow(qua), 25)
			for(i in 1:ni) for(j in 1:nj) error_mat[i,j] <- solve_u_joint_params_quat_error(c(ij_seq[i], ij_seq[j]), 
				qua[q_seq, ], aor_f, aor_r, vo=fao_plane, mat=coor_s[1:3, , 1])

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
			
			# Save minimum 2-parameter error
			min_2p_error <- error_at[which_min, 3]

			if(print.progress) cat(paste0('\t\tError after sampling 2-parameter space: ', round(error_at[which_min, 3], 7), '\n'))

			# Get axes orientations
			aor_u <- rbind(aor_f, aor_r)
			aor_ur <- aor_u %*% tMatrixEP(fao_plane, start_p[1])
			aor_ur <- aor_ur %*% tMatrixEP(aor_ur[1, ], start_p[2])
			aor_f <- aor_ur[1,]
			aor_r <- aor_ur[2,]
			
			if(FALSE){

				svg.new('Test2.html')
				svg.frame(coor)
				for(i in 1:length(max_disp)) svg.pointsC(coor[, , max_disp[i]], col.stroke=c('red', 'green', 'blue'), close=TRUE, opacity.stroke.C=0.25)
			#	for(i in 1:dim(coor)[3]) svg.pointsC(coor[, , i], col.stroke=c('red', 'green', 'blue'), close=TRUE, opacity.stroke.C=0.25)
			#	svg.pointsC(coor, col.stroke=c('red', 'green', 'blue'), close=TRUE, opacity.stroke.C=0.25)
			#	svg.arrows(rbind(CoR, CoR+0.5*c(1,0,0)), col='orange')
				svg.arrows(rbind(CoR, CoR+20*aor_f), col='orange', lwd=2)
			#	svg.arrows(rbind(CoR, CoR+1.5*aor_fo), col='orange', lwd=2)
			#	svg.arrows(rbind(CoR, CoR+0.5*c(0,0,1)), col='brown')
				svg.arrows(rbind(CoR, CoR+20*aor_r), col='brown', lwd=2)
			#	svg.arrows(rbind(c(0,0,0), 1.5*aor_ro), col='brown', lwd=2)
				svg.arrows(rbind(CoR, CoR+20*fao_plane), col='red', lwd=2)

				for(i in 1:nrow(c_prod)) svg.arrows(rbind(CoR, CoR+10*c_prod[i, 1:3]), col='purple', opacity=0.4)
			#	for(i in 1:nrow(c_prod)) svg.arrows(rbind(c(0,0,0), 2*inst_aor[i, 1:3]), col='purple', opacity=0.4)

				svg.close()
			}

			if(FALSE){	# Increased final error in two cases using in vivo data

				q_seq <- 1:min(nrow(qua), 50)

				u_joint_solve <- tryCatch(
					expr={
						nlminb(start=c(0,0,0), objective=solve_u_joint_params_quat_error, 
							lower=c(-pi,-pi,-pi), upper=c(pi,pi,pi), q=qua[q_seq, ], f.axis=aor_f,
							r.axis=aor_r, mat=coor_s[1:3, , 1])
					},
					error=function(cond) {print(cond);return(NULL)},
					warning=function(cond) {print(cond);return(NULL)}
				)

				# If error is lower, save new axes
				message <- ' (2-parameter values used)'
				if(u_joint_solve$objective < min_2p_error){
					aor_f <- aor_f %*% rotationMatrixZYX(u_joint_solve$par)
					aor_r <- aor_r %*% rotationMatrixZYX(u_joint_solve$par)
				}

				if(print.progress) cat(paste0('\t\tError after optimizing 3-parameter space: ', round(u_joint_solve$objective, 7), '\n'))
			}

			return(list('cons'=c(CoR, uvector(aor_f), uvector(aor_r))))
		}

	}else if(type == 'T'){

		# Set to any set of three mutually orthogonal vectors
		return(list('cons'=c(c(1,0,0), c(0,1,0), c(0,0,1))))
	}

	return(list(
		'cons'=joint_cons
	))
}