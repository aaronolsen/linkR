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
	if(FALSE){

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

		fit_ecylinder <<- fit_ecylinder
		objectives <<- objectives
	}
	
	#print(fit_ecylinder)

	# Choose fit to refine
	if(select.axis[1] == 'min'){
		refine_fit <- fit_ecylinder[[which.min(objectives)]]
	}else if(select.axis[1] == 'max'){
		refine_fit <- fit_ecylinder[[which.max(objectives)]]
	}else if(select.axis[1] == 'mid'){
		refine_fit <- fit_ecylinder[[which(!objectives %in% range(objectives))]]
	}	
	
	# Refine fit
	if(FALSE){

		refined_fit <<- tryCatch(
			expr={
				nlminb(start=refine_fit$par, objective=fit_elliptic_cylinder_error, 
					lower=refine_fit$lower, upper=refine_fit$upper, mat=mat, 
					control=list('eval.max'=300, 'iter.max'=300))
			},
			error=function(cond) {print(cond);return(NULL)},
			warning=function(cond) {print(cond);return(NULL)}
		)
	}

	# Define cylinders
	fit_shape <- list(refined_fit)
	cylinders <- list()
	for(i in 1:length(fit_shape)){
		cylinders[[i]] <- defineEllipticCylinder(center=fit_shape[[i]]$par[1:3], nvector=fit_shape[[i]]$par[4:6], 
			a=fit_shape[[i]]$par[7], r1=fit_shape[[i]]$par[8], r2=fit_shape[[i]]$par[9])
	}

	svg.new('Test.html')

	svg.points(mat)
#	svg.points(mid_range, col='blue')

#	for(i in 1:3) svg.arrows(rbind(mid_range, mid_range + range_dist[i, 2]*uvector(pc_axes[, i])), col='orange')

	# Draw cylinders
	cols <- c('red', 'green', 'blue')
	for(i in 1:length(cylinders)){

		axis_seq <- seq(-1,1,length=10)
		circ_seq <- seq(0,2*pi,length=30)
		cyl_pts <- matrix(NA, length(axis_seq)*length(circ_seq), 3)
		j <- 1
		for(ci in 1:length(axis_seq)){
			ec_pts <- matrix(NA, 0, 3)
			for(cj in 1:length(circ_seq)){
				ecpt <- ellipticCylinderPoint(cylinders[[i]], axis_seq[ci], circ_seq[cj])
				ec_pts <- rbind(ec_pts, ecpt)
				cyl_pts[j, ] <- ecpt
				j <- j + 1
			}
			svg.pointsC(ec_pts, col.stroke.C=cols[i], col.fill=cols[i], col.stroke=cols[i], opacity.stroke=0.5, opacity.stroke.C=0.5)
		}

		for(cj in 1:length(circ_seq)){
			ec_pts <- matrix(NA, 0, 3)
			for(ci in 1:length(axis_seq)){
				ec_pts <- rbind(ec_pts, ellipticCylinderPoint(cylinders[[i]], axis_seq[ci], circ_seq[cj]))
			}
			svg.pointsC(ec_pts, col.stroke.C=cols[i], col.fill=cols[i], col.stroke=cols[i], opacity.stroke=0.5, opacity.stroke.C=0.5)
		}
	
	#	svg.points(cyl_pts, col=cols[i], opacity.fill=0.5, opacity.stroke=0.5)
	}

	svg.frame(cyl_pts, z.index=-1)

	#svg.arrows(rbind(cylinders[[1]]$C, cylinders[[1]]$C + cylinders[[1]]$N), col='orange', lwd=1)
	#svg.arrows(rbind(cylinders[[1]]$C, cylinders[[1]]$C + cylinders[[1]]$R1*uvector(cylinders[[1]]$U)), col='brown', lwd=1)
	#svg.arrows(rbind(cylinders[[1]]$C, cylinders[[1]]$C + cylinders[[1]]$R2*uvector(cylinders[[1]]$V)), col='brown', lwd=1)

	svg.close()

	# Return cylinder
	return(defineEllipticCylinder(center=refined_fit$par[1:3], nvector=refined_fit$par[4:6], 
		a=refined_fit$par[7], r1=refined_fit$par[8], r2=refined_fit$par[9]))
}