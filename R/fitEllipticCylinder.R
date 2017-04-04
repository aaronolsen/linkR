fitEllipticCylinder <- function(mat, select.axis=c('min', 'mid', 'max')){

	# Get point ranges
	mat_range <- apply(mat, 2, 'range')

	# Find midpoint of dimensions
	mid_range <- colMeans(mat_range)

	# Find PC axes
	pca <- prcomp(mat)
	pc_axes <- pca$rotation

	# Find ranges in dimensions in planes orthogonal to PC axes
	range_dist <- matrix(NA, 3, 2)
	for(i in 1:3){
		
		# Find maximum distance from line to points
		dpl <- distPointToLine(mat, mid_range, mid_range + pc_axes[, i])
		range_dist[i, ] <- range(dpl)
	}

	# Get point ranges
	pt_range <- apply(mat, 2, 'range')
	diff_range <- pt_range[2, ] - pt_range[1, ]

	# Try fitting cylinders using three central axis combination
	objectives <- rep(NA, 3)
	fit_ecylinder <- list()
	for(i in 1:3){

		# Set limits
		lower <- c(mid_range-2*range_dist[i, 2], rep(-1,2), 0, 0, rep(range_dist[i, 2]*0.01, 2))
		upper <- c(mid_range+2*range_dist[i, 2], rep(1,3), pi, rep(range_dist[i, 2]*2, 2))

		# Set starting parameters
		start_param <- colMeans(rbind(lower, upper))
		start_param[4:6] <- pc_axes[, i]
		start_param[8:9] <- range_dist[i, 2]*0.5

		# Fit
		fit_ecylinder[[i]] <- tryCatch(
			expr={
				nlminb(start=start_param, objective=fit_elliptic_cylinder_error, 
					lower=lower, upper=upper, mat=mat, control=list('eval.max'=50, 'iter.max'=50))
			},
			error=function(cond) {print(cond);return(NULL)},
			warning=function(cond) {print(cond);return(NULL)}
		)

		objectives[i] <- fit_ecylinder[[i]]$objective
		fit_ecylinder[[i]]$lower <- lower
		fit_ecylinder[[i]]$upper <- upper
	}

	# Choose fit to refine
	if(select.axis[1] == 'min'){
		refine_fit <- fit_ecylinder[[which.min(objectives)]]
	}else if(select.axis[1] == 'max'){
		refine_fit <- fit_ecylinder[[which.max(objectives)]]
	}else if(select.axis[1] == 'mid'){
		refine_fit <- fit_ecylinder[[which(!objectives %in% range(objectives))]]
	}	

	# Refine fit
	refined_fit <- tryCatch(
		expr={
			nlminb(start=refine_fit$par, objective=fit_elliptic_cylinder_error, 
				lower=refine_fit$lower, upper=refine_fit$upper, mat=mat, 
				control=list('eval.max'=300, 'iter.max'=300))
		},
		error=function(cond) {print(cond);return(NULL)},
		warning=function(cond) {print(cond);return(NULL)}
	)

	# Define cylinders
	refined_fit <- refined_fit

	# Return cylinder
	return(defineEllipticCylinder(center=refined_fit$par[1:3], nvector=refined_fit$par[4:6], 
		a=refined_fit$par[7], r1=refined_fit$par[8], r2=refined_fit$par[9]))
}