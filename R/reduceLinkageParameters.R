reduceLinkageParameters <- function(linkage, min.input.step=0.04){

	if(is.null(names(linkage))){

		for(ii in 1:length(linkage)){

			red_params <- reduceLinkageParameters(linkage=linkage[[ii]], min.input.step=min.input.step)
			if(ii == 1) red_param_mat <- matrix(NA, nrow=length(linkage), ncol=length(red_params), 
				dimnames=list(NULL, names(red_params)))
			red_param_mat[ii, ] <- red_params
		}

		return(colMeans(red_param_mat))
	}

	params <- c()
	param_names <- c()

	# Reduce parameters by finding means or weighted means
	for(input in names(linkage)){
	
		if(input == 'RD') next
	
		i <- 1
	
		for(name_list in names(linkage[[input]])){

			if(is.vector(linkage[[input]][[name_list]])){

				# Find simple mean
				if(length(linkage[[input]][[name_list]]) > 1){
					red_param <- mean(linkage[[input]][[name_list]], na.rm=TRUE)
				}else{
					red_param <- linkage[[input]][[name_list]]
				}

				params <- c(params, red_param)
				param_names <- c(param_names, paste0(input, i))

			}else if(is.matrix(linkage[[input]][[name_list]])){
			
				if(nrow(linkage[[input]][[name_list]]) > 1){

					if(input == 'RA'){
						# Use input angular displacements to set weights
						weights <- c(0, abs(diff(linkage$RD[[1]])))
				
						# Remove large skips
						weights_skip <- weights
						weights_skip[weights_skip > min.input.step] <- 0
						
						# If no angles remain, retain all
						if(sum(weights_skip) < 1e-4) weights_skip <- weights
						weights <- weights_skip

					}else{

						weights <- rep(1, nrow(linkage[[input]][[name_list]]))
					}
			
					# Find mean vector across all steps
					w_mean <- colMeans(linkage[[input]][[name_list]]*matrix(weights, 
						nrow=nrow(linkage[[input]][[name_list]]), ncol=3), na.rm=TRUE)

					red_param <- uvector(w_mean)
				}else{
					red_param <- linkage[[input]][[name_list]]
				}

				params <- c(params, red_param)
				param_names <- c(param_names, paste0(input, i, letters[24:26]))
			}
			
			i <- i + 1
		}
	}

	setNames(params, param_names)
}

