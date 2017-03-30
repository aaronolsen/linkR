fitJointConstraint <- function(coor, type){

	# Fit single rotational axis constraint
	if(type %in% c('L', 'P')){
	
		# Vectors
		vecs <- matrix(NA, dim(coor)[1], 3)

		# Fit a 3D line to the position of each body point over time
		for(i in 1:dim(coor)[1]){

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
		CoRs <- matrix(NA, dim(coor)[1], 3)
		rads <- rep(NA, dim(coor)[1])
		if(type == 'R') AoRs <- matrix(NA, dim(coor)[1], 3)

		# Fit circle/sphere to the set of positions of each body point over time
		for(i in 1:dim(coor)[1]){

			if(type == 'R'){

				# Align points by centroid
				centroid_align <- t(coor[i, , ]) - matrix(colMeans(t(coor[i, , ])), dim(coor)[3], dim(coor)[2], byrow=TRUE)

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

			# 
		}

		# Set joint constraints
		if(type == 'R') joint_cons <- c(CoR, AoR)
		if(type == 'U') joint_cons <- c(CoR)

		svg.new(file='Test.html')

		svg.pointsC(t(coor[i, , ]), col.fill='none', cex=1)

		svg.points(fit_shape$C, cex=3, col='orange')
		
		for(x in -5:5){
			for(y in -5:5){
				for(z in -5:5){
					svg.points(fit_shape$C + fit_shape$R*uvector(c(x,y,z)), cex=1, col='orange')
				}
			}
		}

		svg.close()
	}

	return(list(
		'cons'=joint_cons
	))
}