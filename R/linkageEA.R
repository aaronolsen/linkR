linkageEA <- function(linkage, input.param, input.joint, input.points, output.points, 
	disp_mag=NULL, small=0.001){

	# If vector, make list
	if(!is.list(input.param)) input.param <- list(input.param)

	# Get number of iterations - add before and after small displacements (x3)
	if(is.vector(input.param[[1]])) num_iter <- length(input.param[[1]])*3
	if(is.matrix(input.param[[1]])) num_iter <- nrow(input.param[[1]])*3

	# Create list of input parameters plus small displacements centered on input value, with direction of small change the same as input
	state_plus_small <- instInputParam(input.param, disp_mag=disp_mag, small=small)

	# Animate linkage over small displacement including middle point
	link_anim <- animateLinkage(linkage=linkage, input.param=state_plus_small, 
		input.joint=input.joint)

	# Create single array for joint coordinates and points
	num_points <- dim(link_anim$joint.coor)[1]
	pt_names <- dimnames(link_anim$joint.coor)[[1]]
	if(!is.null(linkage$link.points)){
		num_points <- num_points + dim(link_anim$link.points)[1]
		pt_names <- c(pt_names, dimnames(link_anim$link.points)[[1]])
	}
	pt_arr <- array(NA, dim=c(num_points, 3, num_iter), dimnames=list(pt_names, NULL, NULL))

	# Fill array
	pt_arr[1:dim(link_anim$joint.coor)[1], , ] <- link_anim$joint.coor
	if(!is.null(link_anim$link.points)) pt_arr[(dim(link_anim$joint.coor)[1]+1):dim(pt_arr)[1], , ] <- link_anim$link.points

	# Put input and output points into list for easy looping
	io_points <- list(input.points, output.points)

	# Check that input and output points are found in array
	for(i in 1:length(io_points)){
		if(sum(!io_points[[i]] %in% dimnames(pt_arr)[[1]]) > 0){
			stop(paste0("The following points were not found in the linkage joint and/or point coordinates: ", 
				paste0(io_points[[i]][!io_points[[i]] %in% dimnames(pt_arr)[[1]]], collapse=', ')))
		}
	}

	# Find instantaneous measures of motion (displacement or centroid size) for input
	inst_motion <- list()
	for(i in 1:length(io_points)){
		if(length(io_points[[i]]) == 1){

			# Find instantaneous displacement around each central iteration
			inst_motion[[i]] <- sqrt(colSums((pt_arr[io_points[[i]], , seq(3, num_iter, by=3)] - pt_arr[io_points[[i]], , seq(1, num_iter, by=3)])^2))
			
		}else{

			# Find centroid size at each iteration
			centroid_sizes <- centroidSize(pt_arr[io_points[[i]], , ])

			# Measure change in centroid size around each central iteration
			inst_motion[[i]] <- centroid_sizes[seq(3, num_iter, by=3)] - centroid_sizes[seq(1, num_iter, by=3)]
		}
	}
	
	# Calculate expansion advantage (output displacement / input displacement)
	ea <- inst_motion[[2]] / inst_motion[[1]]
	
	# Create return list
	rlist <- list(
		'ea'=ea,
		'in.d'=inst_motion[[1]],
		'out.d'=inst_motion[[2]]
	)

	rlist
}