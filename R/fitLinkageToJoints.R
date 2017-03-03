fitLinkageToJoints <- function(ref.coor, joint.types, fixed.link, input.joint, model, 
	shift.input = TRUE){

	# Re-parameterize marker coordinates as linkage
	linkage <- reparameterizeLinkage(joint.coor=ref.coor, joint.types=joint.types, 
		fixed.link=fixed.link, input.joint=input.joint)

	# Reduce linkage parameters (currently just means and weighted means)
	red_params <- reduceLinkageParameters(linkage)

	# Set mean value vectors
	mean_axes <- red_params[c(paste0('RA1', letters[24:26]), paste0('RA2', letters[24:26]))]
	mean_lengths <- red_params[paste0('LL', 1:4)]
	RI_vector <- red_params[paste0('RI1', letters[24:26])]
	AJ_vector <- red_params[paste0('AJ1', letters[24:26])]
	mean_vectors <- red_params[c(paste0('RI1', letters[24:26]), paste0('AJ1', letters[24:26]))]	# Others to potentially add: FJ1, JO, RI

	# Set model name to lowercase
	model_tl <- tolower(model)

	# Separate constant and parameters to be optimized
	if(model_tl %in% c('full parameter')){
		start <- NULL
		NA.penalty <- 0
	}

	if(model_tl %in% c('constant axes', 'constant axis')){
		start <- c(mean_axes, RI_vector)
		#start <- mean_axes
		NA.penalty <- 0.01
	}

	if(model_tl %in% c('constant lengths', 'constant length')){
		#start <- c(mean_lengths, RI_vector)
		start <- mean_lengths
		NA.penalty <- 0.01
	}

	if(model_tl %in% c('constant 3d')){
		start <- c(mean_axes, mean_lengths, RI_vector)
		NA.penalty <- 0.005
	}

	if(model_tl %in% c('constant parallel')){
		start <- c(mean_axes, mean_vectors)
		start[names(mean_axes)] <- NA
		NA.penalty <- 0
	}

	if(model_tl %in% c('constant 2d')){
		start <- c(mean_axes, mean_lengths, mean_vectors)
		start[names(mean_axes)] <- NA
		NA.penalty <- 0
	}
	
	# Get number of frames
	if(is.list(ref.coor)){
		nframes <- dim(ref.coor[[1]])[3]
	}else{
		nframes <- dim(ref.coor)[3]
	}

	# Measure error before optimization
	rmse_pre_optim <- solveLinkageError(p=start, linkage=linkage, marker_array=ref.coor, 
		NA.penalty=NA.penalty, shift.input=shift.input)

	#cat('\t\tPre-optim RMSE : ', round(rmse_pre_optim, 8), '\n', sep='')

	# Find optimal parameters
	if(!model_tl %in% c('full parameter')){

		link_fits <- list()
		if(nframes < 60){
			#set_seeds <- c(42,43)
			set_seeds <- c(42)
		}else{
			#set_seeds <- c(42,43,44)
			set_seeds <- c(42)
		}

		fit_errors <- rep(NA, length(set_seeds))
		
		for(i in 1:length(set_seeds)){
		
			# Vary the start parameters by multiplying small constant
			set.seed(set_seeds[i])
			start_sample <- start*sample(x=c(0.95, 0.92, 1.05, 0.97, 1.07), size=length(start), replace=TRUE)
		
			# Add small number to zeros
			start_sample[which(abs(start_sample) < 1e-8)] <- start_sample[which(abs(start_sample) < 1e-8)] + 0.001

			link_fits[[i]] <- tryCatch(
				expr={
					nlminb(start=start_sample, objective=solveLinkageError, 
						linkage=linkage, marker_array=ref.coor, NA.penalty=NA.penalty,
						shift.input=shift.input)
				},
				error=function(cond){return(NULL)},
				warning=function(cond) return(NULL)
			)

			if(is.null(link_fits[[i]])){
				print('Error');
				fit_errors[i] <- NA
			}else{
				fit_errors[i] <- link_fits[[i]]$objective
			}
		}
		
		link_fit <- link_fits[[which.min(fit_errors)]]
		
		# Save error
		rmse_post_optim <- link_fit$objective

		# Check whether optimized error is less or more
		if(rmse_post_optim < rmse_pre_optim){less_or_more <- 'less'}else{less_or_more <- 'more'}
		if(abs(rmse_post_optim - rmse_pre_optim) < 0.00001) less_or_more <- 'equal'

		#cat(paste0('\t\tPost-optim RMSE: ', round(rmse_post_optim, 8), ' (', less_or_more, ')\n'), sep='')
		rmse_final <- rmse_post_optim

		#print(link_fit$par)

		if(less_or_more == 'more'){
			link_fit <- list('par'=start)
			rmse_final <- rmse_pre_optim
		}

	}else{

		link_fit <- list('par'=c())
		rmse_post_optim <- rmse_pre_optim
		rmse_final <- rmse_pre_optim
	}

	# Replace linkage parameters with optimized parameters
	fit_linkage <- replaceParameters(p=link_fit$par, linkage=linkage)

	# Shift input parameters for constant axes to most closely match reference
	if(shift.input) fit_linkage <- shiftInputParameters(linkage=fit_linkage, ref.coor=ref.coor)

	if(is.null(names(fit_linkage))){
		input.param <- list()
		for(ii in 1:length(linkage)){
			if(ii == 1) joint.conn <- fit_linkage[[ii]]$joint.conn
			input.param[[ii]] <- fit_linkage[[ii]]$RD
			fit_linkage[[ii]]$joint.conn <- NULL
			fit_linkage[[ii]]$RD <- NULL
		}
	}else{
		joint.conn <- fit_linkage$joint.conn
		input.param <- fit_linkage$RD
		fit_linkage$joint.conn <- NULL
		fit_linkage$RD <- NULL
	}
	
	# Solve linkage
	solve_linkage <- solveLinkage(fit_linkage, input.param=input.param, joint.conn=joint.conn)
	dimnames(solve_linkage) <- dimnames(ref.coor)

	# Count numbers of parameters
	fit_linkage_count <- fit_linkage
	if(is.null(names(fit_linkage))) fit_linkage_count <- fit_linkage[[1]]

	par_count <- c()
	par_count_names <- c()
	for(input in names(fit_linkage_count)){
		i <- 1
		for(name_list in names(fit_linkage_count[[input]])){
			par_count <- c(par_count, sum(!is.na(fit_linkage_count[[input]][[name_list]])))
			par_count_names <- c(par_count_names, paste0(input, i))
			i <- i + 1
		}
	}
	par_count <- setNames(par_count, par_count_names)

	rlist <- list(
		'par'=link_fit$par,
		'fit.linkage'=fit_linkage,
		'rmse_pre_optim'=rmse_pre_optim,
		'rmse_post_optim'=rmse_post_optim,
		'rmse_final'=rmse_final,
		'par.count'=par_count,
		'fit.coor'=solve_linkage,
		'ref.coor'=ref.coor,
		'ref.linkage'=linkage,
		'fixed.link'=fixed.link,
		'joint.conn'=joint.conn,
		'input.param'=input.param,
		'input.joint'=input.joint,
		'model'=model
	)
	
	rlist
}