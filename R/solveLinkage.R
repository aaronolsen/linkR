solveLinkage <- function(linkage, input.param, joint.conn, min.input.step=0.04, add.input.step=0.02){

	if(is.null(names(linkage))){
		for(ii in 1:length(linkage)){

			joint_coor <- solveLinkage(linkage=linkage[[ii]], input.param=input.param[[ii]], 
				joint.conn=joint.conn, min.input.step=min.input.step, add.input.step=add.input.step)

			if(ii == 1){
				joint_coor_arr <- array(NA, dim=c(dim(joint_coor), length(linkage)), 
					dimnames=list(dimnames(joint_coor)[[1]], dimnames(joint_coor)[[2]], 
					dimnames(joint_coor)[[3]], NULL))
			}
			joint_coor_arr[, , , ii] <- joint_coor
		}

		return(joint_coor_arr)
	}

	linkage[['RD']] <- input.param
	linkage[['joint.conn']] <- joint.conn

	# Threshold under which displacement vector is used to determine next point
	dv_thresh <- 0

	# Number of previous points to average for comparison point
	pc_nrow <- 5

	# Get number of iterations
	n_iter <- length(linkage$RD[[1]])

	# Convert all inputs to consistent dimensions
	expand_input <- c('FJ', 'AJ', 'RA', 'LL')
	for(input in expand_input){
		for(name_list in names(linkage[[input]])){
			if(is.vector(linkage[[input]][[name_list]])){
				if(length(linkage[[input]][[name_list]]) < n_iter) linkage[[input]][[name_list]] <- rep(linkage[[input]][[name_list]][1], n_iter)
			}else if(is.matrix(linkage[[input]][[name_list]])){
				if(nrow(linkage[[input]][[name_list]]) < n_iter){
					linkage[[input]][[name_list]] <- matrix(linkage[[input]][[name_list]][1, ], nrow=n_iter, 
						ncol=ncol(linkage[[input]][[name_list]]), byrow=TRUE)
				}
			}
		}
	}
	
	# Expand JO with NAs if only a singe row
	for(name_list in names(linkage[['JO']])){
		if(nrow(linkage[['JO']][[name_list]]) == 1){
			m <- matrix(NA, nrow=n_iter, ncol=ncol(linkage[['JO']][[name_list]]))
			m[1, ] <- linkage[['JO']][[name_list]]
			linkage[['JO']][[name_list]] <- m
		}
	}

	# Add JO to expand input for step adding
	expand_input <- c('FJ', 'AJ', 'RA', 'LL', 'JO')
	
	# Set steps to output at end
	steps_out <- rep(TRUE, n_iter)

	# Find intervals of input angles
	RD_diff <- c(diff(linkage$RD[[1]]))
	
	# Create new list with added steps
	linkage_add <- linkage
	
	# Find if any steps exceed the threshold
	if(sum(!is.na(linkage$JO[[1]][, 1])) <=1 && sum(abs(RD_diff) > min.input.step) > 0){

		# Find where steps exceed minimum
		exceed_min <- which(abs(RD_diff) > min.input.step)
		
		# Set where to add after
		add_steps <- list()
		
		for(i in 1:length(exceed_min)){

			# Create step sequence to bridge gap
			from <- linkage$RD[[1]][exceed_min[i]]
			to <- linkage$RD[[1]][exceed_min[i]+1]
			step_seq <- seq(from, to, by=sign(to-from)*add.input.step)

			# What steps to add
			add_steps[[length(add_steps)+1]] <- step_seq[2:(length(step_seq)-1)]
		}
		
		# Add last element in vector for consistent process per iteration
		exceed_min <- c(exceed_min, n_iter)
		
		# Start vector with first subset before exceeded minimum
		linkage_add$RD[[1]] <- linkage_add$RD[[1]][1:exceed_min[1]]
		steps_out <- rep(TRUE, exceed_min[1])

		for(i in 1:(length(exceed_min)-1)){

			# Add new steps and existing steps up to next exceed min point
			linkage_add$RD[[1]] <- c(linkage_add$RD[[1]], add_steps[[i]], linkage$RD[[1]][(exceed_min[i]+1):exceed_min[i+1]])
			steps_out <- c(steps_out, rep(FALSE, length(add_steps[[i]])), rep(TRUE, length(linkage$RD[[1]][(exceed_min[i]+1):exceed_min[i+1]])))
		}
		
		# Expand each list element
		for(input in expand_input){
			for(name_list in names(linkage[[input]])){
				if(is.vector(linkage[[input]][[name_list]])){

					# Create new vector
					linkage_add[[input]][[name_list]] <- rep(NA, length(steps_out))

					# Fill in output places with current values
					linkage_add[[input]][[name_list]][steps_out] <- linkage[[input]][[name_list]]
					
					# Find mean value
					fill <- mean(linkage[[input]][[name_list]], na.rm=TRUE)

					# Fill in output places with current values
					for(i in 1:length(linkage_add[[input]][[name_list]])){
						if(!is.na(linkage_add[[input]][[name_list]][i])) next
						linkage_add[[input]][[name_list]][i] <- fill
					}

				}else if(is.matrix(linkage[[input]][[name_list]])){

					# Create new matrix
					linkage_add[[input]][[name_list]] <- matrix(NA, nrow=length(steps_out), ncol=ncol(linkage_add[[input]][[name_list]]))

					# Fill in output places with current values
					linkage_add[[input]][[name_list]][steps_out, ] <- linkage[[input]][[name_list]]

					# Use input angular displacements to set weights
					#weights <- c(0, abs(diff(linkage$RD[[1]])))
				
					# Remove large skips
					#weights[weights > min.input.step] <- 0
			
					# Find mean vector across all steps
					#fill <- colMeans(linkage[[input]][[name_list]]*matrix(weights, 
					#	nrow=nrow(linkage[[input]][[name_list]]), ncol=3), na.rm=TRUE)

					# Fill in output places with current values
					copy <- linkage_add[[input]][[name_list]][1, ]
					for(i in 1:nrow(linkage_add[[input]][[name_list]])){
						if(!is.na(linkage_add[[input]][[name_list]][i, 1])){
							copy <- linkage_add[[input]][[name_list]][i, ]
							next
						}

						# Error is smallest with previous non-NA value rather than weighted mean
						linkage_add[[input]][[name_list]][i, ] <- copy
					}
				}
			}
		}
		
		#plot(linkage_add$RD[[1]], type='l')
		#cols <- rep('blue', length(linkage_add$RD[[1]]));cols[steps_out] <- 'orange'
		#points(linkage_add$RD[[1]], cex=0.5, col=cols)
	}

	# Assign all list elements to variables of same name
	for(input in names(linkage_add)) assign(input, linkage_add[[input]])
	
	# Update number of iterations
	n_iter <- length(steps_out)

	# Create array for joint coordinates
	joint.coor <- array(NA, dim=c(nrow(joint.conn), 3, n_iter), 
		dimnames=list(rownames(joint.conn), NULL, NULL))

	# Set fixed joint
	joint.coor[names(FJ)[1], , ] <- FJ[[1]]

	# Set joints adjacent to fixed joint
	for(adj_joint in names(AJ)){
	
		# Get label for link length (distance between joints)
		ll_label <- paste(sort(c(names(FJ)[1], adj_joint)), collapse='-')

		# Get scaled vector for joint position
		adj_vec <- t(AJ[[adj_joint]]*matrix(LL[[ll_label]], nrow=nrow(AJ[[adj_joint]]), 
			ncol=ncol(AJ[[adj_joint]])))
		
		# Add joint position relative to fixed joint
		joint.coor[adj_joint, , ] <- joint.coor[names(FJ)[1], , ] + adj_vec
	}

	# If NA, set axes of rotation assuming planar linkage
	if(is.na(RA[[1]][1,1])){

		RA2D <- uvector(cprod(joint.coor[names(AJ), , 1], RI[[1]]))
		RA[[1]] <- matrix(RA2D, nrow=nrow(RA[[1]]), ncol=3, byrow=TRUE)
		RA[[2]] <- RA[[1]]
	}

	# Matrix of points to use in determining best solution
	point_compare_mat <- matrix(NA, nrow=pc_nrow, ncol=3)

	# Get input link name
	input_link <- paste(sort(c(names(FJ), names(RI))), collapse='-')
	#print(joint.conn[names(FJ), joint.conn[names(FJ), ] %in% joint.conn[names(RI), ]])

	# Get output link name
	output_link <- paste(sort(c(names(RA)[2], names(JO))), collapse='-')
	
	# Get name of joint adjacent to output joint but not an R joint
	adj_JO <- rownames(joint.conn)[!rownames(joint.conn) %in% c(names(RA), names(JO))]

	# Get name of link between output joint and adjacent
	adj_JO_link <- paste(sort(c(names(JO), adj_JO)), collapse='-')

	# Set first R-joint
	for(itr in 1:n_iter){

		## Apply input rotation
		# Find rotation matrix
		RM <- tMatrixEP(v=RA[[names(RA)[1]]][itr, ], a=RD[[1]][itr])

		# Rotate first R-joint link (input link)
		joint.coor[names(RI), , itr] <- FJ[[1]][itr, ] + uvector(RI[[1]] %*% RM)*LL[[input_link]][itr]
		
		if(!is.na(JO[[1]][itr, 1])){

			# Set point for first iteration using vector
			joint.coor[names(JO), , itr] <- t(FJ[[1]][itr, ] + JO[[1]][itr, ])

			# Save compare point
			point_compare_mat <- point_compare_mat*NA
			point_compare_mat[1, ] <- joint.coor[names(JO), , itr]
		}

		## Solve for output rotation
		# Define circle for output link
		output_circle <- defineCircle(center=joint.coor[names(RA)[2], , itr], 
			nvector=RA[[2]][itr, ], radius=LL[[output_link]][itr])

		# Find angle on circle at distance from transmission link joint
		output_link_ts <- angleOnCircleFromPoint(circle=output_circle, dist=LL[[adj_JO_link]][itr], 
			P=joint.coor[adj_JO, , itr])

		# If no solution, set as NA
		if(is.na(output_link_ts[1])){joint.coor[names(JO), , itr] <- rep(NA, 3); next}
		
		# Find corresponding points on circle
		circle_points <- circlePoint(circle=output_circle, T=output_link_ts)

		# Find point closest to previous point
		dA <- distPointToPoint(circle_points, colMeans(point_compare_mat, na.rm=TRUE))

		# Find difference between point distances relative to mean distance
		dA_rdiff <- abs(diff(dA)) / mean(dA, na.rm=TRUE)

		if(dA_rdiff < dv_thresh){

			# Find average displacement vector from previous points
			dvec <- colMeans(point_compare_mat[2:nrow(point_compare_mat), ] - point_compare_mat[1:(nrow(point_compare_mat)-1), ], na.rm=TRUE)

			# Find expected next position based on displacement from average of some previous coordinates
			next_pos <- joint.coor[names(JO), , itr-1] + dvec
			
			# Find point closest to expected next position
			dA <- distPointToPoint(next_pos, circle_points)
		}

		# Save output
		output_link_t <- ifelse(dA[1] < dA[2], 1, 2)

		# Find corresponding point on circle
		joint.coor[names(JO), , itr] <- circle_points[output_link_t, ]

		# Point already added
		if(itr == 1) next
		
		# Add to point compare matrix
		if(is.na(point_compare_mat[pc_nrow, 1])){
			point_compare_mat[which(is.na(point_compare_mat[, 1]))[1], ] <- joint.coor[names(JO), , itr]
		}else{
			point_compare_mat[1:(pc_nrow-1), ] <- point_compare_mat[2:pc_nrow, ]
			point_compare_mat[pc_nrow, ] <- joint.coor[names(JO), , itr]
		}
	}

	return(joint.coor[, , steps_out])
}