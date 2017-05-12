associatePoints <- function(mechanism, points, body.name, points.connect = NULL){

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

	if(length(body.name) == 1){

		# Find number corresponding to body name
		body_num <- which(body.name == mechanism$body.names)

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
	if(is.null(mechanism$points.connect)){
		mechanism$points.connect <- points.connect
	}else{
		num_points_connect <- length(mechanism$points.connect)
		for(i in 1:length(points.connect)){
			mechanism$points.connect[[i + num_points_connect]] <- points.connect[[i]] + num_points
		}
	}

	mechanism
}
