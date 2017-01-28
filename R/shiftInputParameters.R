shiftInputParameters <- function(linkage, ref.coor){

	if('fit.linkage' %in% names(linkage)){

		if(is.null(names(linkage$fit.linkage))){

			shift_input <- list('linkage'=NULL, 'input.param'=NULL)
			for(ii in 1:length(linkage$fit.linkage)){

				#linkage_ii <- linkage
				linkage_ii <- linkage$fit.linkage[[ii]]
				linkage_ii$input.param <- linkage$input.param[[ii]]
				shift_input_params <- shiftInputParameters(linkage=linkage_ii, ref.coor=ref.coor[, , , ii])
				linkage$fit.linkage[[ii]] <- shift_input_params
				linkage$fit.linkage[[ii]]$input.param <- NULL
				linkage$input.param[[ii]] <- shift_input_params$input.param
			}

			return(linkage)
		}else{
			link <- linkage$fit.linkage
		}
	}else{

		if(is.null(names(linkage))){
			linkage_new <- list()
			for(ii in 1:length(linkage)) linkage_new[[ii]] <- shiftInputParameters(linkage=linkage[[ii]], ref.coor=ref.coor[, , , ii])
			return(linkage_new)
		}
		
	}
	
	if('RD' %in% names(linkage)) input_param_name <- 'RD'
	if('input.param' %in% names(linkage)) input_param_name <- 'input.param'

	# Skip if rotational axes are non-constant
	if(nrow(linkage$RA[[1]]) > 1) return(linkage)

	link <- linkage

	# Get input link name
	input_link <- paste(sort(c(names(link$FJ), names(link$RI))), collapse='-')

	# If NA, set axes of rotation assuming planar linkage
	if(is.na(link$RA[[1]][1,1])){
		RA2D <- uvector(cprod(link$FJ[[1]][1, ] + link$AJ[[1]], link$RI[[1]]))
		link$RA[[1]] <- matrix(RA2D, nrow=nrow(link$RA[[1]]), ncol=3, byrow=TRUE)
		link$RA[[2]] <- link$RA[[1]]
	}
	
	itr <- 1
	
	# Find rotation matrix
	RM <- tMatrixEP(v=link$RA[[names(link$RA)[1]]][1, ], a=linkage[[input_param_name]][[1]][itr])

	# Rotate first R-joint link (input link)
	j1 <- link$FJ[[1]][1, ] + uvector(link$RI[[1]] %*% RM)*link$LL[[input_link]][itr]

	# Find difference in angle between first point
	a_vec <- avec(j1-link$FJ[[1]][1, ], ref.coor[names(link$RI), , 1]-link$FJ[[1]][1, ], 
		axis=link$RA[[names(link$RA)[1]]][1, ], about.axis=TRUE)

	# Adjust input angle to minimize difference in position (0 angle difference)
	angle_add <- a_vec

	# Find rotation matrix
	RM <- tMatrixEP(v=link$RA[[names(link$RA)[1]]][1, ], a=linkage[[input_param_name]][[1]][1]+angle_add)

	# Rotate first R-joint link (input link)
	j1 <- link$FJ[[1]][1, ] + uvector(link$RI[[1]] %*% RM)*link$LL[[input_link]][1]

	# Find difference in angle between first point
	a_vec <- avec(j1-link$FJ[[1]][1, ], ref.coor[names(link$RI), , 1]-link$FJ[[1]][1, ], 
		axis=link$RA[[names(link$RA)[1]]][1, ], about.axis=TRUE)
	
	if(abs(a_vec) > 1e-3) angle_add <- -angle_add
	
	linkage[[input_param_name]][[1]] <- linkage[[input_param_name]][[1]] + angle_add

	linkage
}