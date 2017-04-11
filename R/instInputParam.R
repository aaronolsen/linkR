instInputParam <- function(input.param, disp_mag=NULL, small=0.0001, rot.small=0.00001){

	# If vector, make list
	if(!is.list(input.param)) input.param <- list(input.param)

	# Create list of input parameters plus small displacements centered on input value, with direction of small change the same as input
	state_plus_small <- as.list(rep(NA, length(input.param)))

	for(in_idx in 1:length(input.param)){

		if(is.vector(input.param[[in_idx]])){
		
			# Get number of iterations - add before and after small displacements (x3)
			num_iter <- length(input.param[[in_idx]])*3

			# Get sign of changes - assume change at first iteration is same as second to make sign same length as input.param
			sign <- sign(c(diff(input.param[[in_idx]])[1], diff(input.param[[in_idx]])))

			# Make new input parameter list with small displacements on either side of input
			#	(needs to be continuous to use previous position at toggle points)
			state_plus_small[[in_idx]] <- rep(NA, num_iter)

			# Add values into new input parameter list
			j <- 1
			for(i in seq(1, num_iter, by=3)){

				state_plus_small[[in_idx]][i:(i+2)] <- c(input.param[[in_idx]][j] - sign[j]*(rot.small/2), 
					input.param[[in_idx]][j], input.param[[in_idx]][j] + sign[j]*(rot.small/2))
		
				j <- j + 1
			}
		}

		if(is.matrix(input.param[[in_idx]])){

			# Get number of iterations - add before and after small displacements (x3)
			num_iter <- nrow(input.param[[in_idx]])*3

			# Get translation vector for each iteration (to add small displacements in correct orientation)
			disp_uvectors <- uvector(input.param[[in_idx]][2:nrow(input.param[[in_idx]]), ] - matrix(input.param[[in_idx]][1, ], 
				nrow=nrow(input.param[[in_idx]])-1, ncol=ncol(input.param[[in_idx]]), byrow=TRUE))

			# Assume direction at last iteration is same as second to last to make same length as input.param
			disp_uvectors <- rbind(disp_uvectors, disp_uvectors[nrow(disp_uvectors), ])

			# Make new input parameter list with small displacements on either side of input
			#	(needs to be continuous to use previous position at toggle points)
			state_plus_small[[in_idx]] <- matrix(NA, nrow=num_iter, ncol=ncol(input.param[[in_idx]]))

			# Find magnitude of rotations to apply
			total_disp <- distPointToPoint(input.param[[in_idx]][1, ], input.param[[in_idx]][nrow(input.param[[in_idx]]), ])

			# If displacement input is 0, set to disp_mag (testing rotation without displacement)
			if(!is.null(disp_mag)) total_disp <- disp_mag

			# Add values into new input parameter list
			j <- 1
			for(i in seq(1, num_iter, by=3)){

				state_plus_small[[in_idx]][i:(i+2), ] <- rbind(
					input.param[[in_idx]][j, ] - disp_uvectors[j, ]*(small/2), 
					input.param[[in_idx]][j, ], 
					input.param[[in_idx]][j, ] + disp_uvectors[j, ]*(small/2)
					)
		
				j <- j + 1
			}
		}
	}

	state_plus_small
}