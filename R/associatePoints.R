associatePoints <- function(mechanism, points, body, points.connect = NULL){

	# Input needs to be matrix so that row names are maintained

	if(nrow(points) == 0) return(mechanism)

	# Get current number of points
	if(is.null(mechanism$body.points)){
		num_points <- 0
	}else{
		num_points <- nrow(mechanism$body.points)
	}

	## Add points to matrix
	if(is.null(mechanism$body.points)){
		mechanism$body.points <- points
	}else{
		mechanism$body.points <- rbind(mechanism$body.points, points)
	}

	# Create list for points associated with each body
	if(is.null(mechanism$points.assoc)) mechanism$points.assoc <- as.list(rep(NA, mechanism$num.bodies))

	if(length(body) == 1){

		# Find number corresponding to body name
		body_num <- which(body == mechanism$body.names)
		
		# Check that body is in body names
		if(length(body_num) == 0) stop(paste0("Body '", body, "' not found in mechanism$body.names"))

		if(length(mechanism$points.assoc[[body_num]]) == 1 && is.na(mechanism$points.assoc[[body_num]][1])){

			mechanism$points.assoc[[body_num]] <- (1:nrow(points)) + num_points

		}else{

			# Add points associated with body
			mechanism$points.assoc[[body_num]] <- c(mechanism$points.assoc[[body_num]], (1:nrow(points)) + num_points)
		}
	}
	#print(mechanism$points.assoc)
	
	## Add body associations
	# Add body associations to vector
	if(is.null(mechanism$body.assoc)){
		mechanism$body.assoc <- rep(body_num, nrow(points))
	}else{
		mechanism$body.assoc <- c(mechanism$body.assoc, rep(body_num, nrow(points)))
	}

	## Add points to connect when drawing
	if(!is.null(points.connect)){

		if(is.list(points.connect)){

			add_point_connect <- points.connect

		}else{

			read_lines <- readLines(points.connect)
			plist_names <- list()
			for(line_num in 1:length(read_lines)) plist_names[[length(plist_names)+1]] <- strsplit(read_lines[line_num], ',[ ]?')[[1]]

			# Save CT marker names for reference
			xr_marker_names <- rownames(points)

			# Create path connect list
			plist_xr <- list()
			for(i in 1:length(plist_names)){
				v_xr <- c()
				for(j in c(1:length(plist_names[[i]]))){
					v_xr <- c(v_xr, which(xr_marker_names == plist_names[[i]][j]))
				}
				plist_xr[[i]] <- v_xr
			}
		
			add_point_connect <- plist_xr
		}

		if(is.null(mechanism$points.connect)){
			mechanism$points.connect <- add_point_connect
		}else{
			num_points_connect <- length(mechanism$points.connect)
			for(i in 1:length(add_point_connect)){
				mechanism$points.connect[[i + num_points_connect]] <- add_point_connect[[i]] + num_points
			}
		}

	}else{

		if(is.null(mechanism$points.connect)){
			mechanism$points.connect <- points.connect
		}else{
			num_points_connect <- length(mechanism$points.connect)
			for(i in 1:length(points.connect)){
				mechanism$points.connect[[i + num_points_connect]] <- points.connect[[i]] + num_points
			}
		}
	}

	mechanism
}
