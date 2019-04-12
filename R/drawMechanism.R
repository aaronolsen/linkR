drawMechanism <- function(mechanism, method = "svgViewR", file = NULL, animate = TRUE, 
	animate.duration = 1, animate.reverse = FALSE, animate.repeat = -1, 
	connect.joints=TRUE, window.title='svgViewR', joint.col.fill="white", 
	joint.col.stroke="black", joint.cex=1.5, joint.lwd=2,
	point.col.fill="black", point.col.stroke="black", point.cex=1, point.lwd=2,
	path.col.fill=NA, path.opacity.fill=1, path.opacity.stroke=1, path.col.stroke="black", 
	path.lwd = 1, draw.trajectory = FALSE, add = FALSE, debug = FALSE, ...){

	#if(is.null(file) && method == "svgViewR") stop("To plot a mechanism using the svgViewR method, 'file' must be non-NULL.")
	if(is.null(file)) method <- "plot"

	# FIND FILE TYPE
	if(!is.null(file)){
		extension_found <- FALSE
		if(grepl('[.]html$', file, ignore.case=TRUE)){
			method <- "svgViewR"
			extension_found <- TRUE
		}
		if(grepl('[.][A-Za-z0-9]+$', file, ignore.case=TRUE) && !extension_found) stop("File extension not recognized.")
		if(!extension_found){
			method <- "svgViewR"
			file <- paste0(file, '.html')
		}
	}

	# Get animated joints
	if(is.null(mechanism[['joint.coor.anim']])){
		joints <- array(mechanism[['joint.coor']], dim=c(dim(mechanism[['joint.coor']])[1:2], 1, 2))
	}else{
		joints <- mechanism[['joint.coor.anim']]
	}
	
	# Get animated joint constraints
	if(is.null(mechanism[['joint.cons.anim']])){
		joint_cons <- list()
		for(i in 1:length(mechanism[['joint.cons']])){
			joint_cons[[i]] <- array(mechanism[['joint.cons']][[i]], dim=c(dim(mechanism[['joint.cons']][[i]])[1:2], 1, 2))
		}
	}else{
		joint_cons <- mechanism[['joint.cons.anim']]
	}
	
	# Get body points	
	if(is.null(mechanism[['body.points.anim']])){
		if(is.null(mechanism[['body.points']])){
			body_points <- NULL
		}else{
			body_points <- array(mechanism[['body.points']], dim=c(dim(mechanism[['body.points']])[1], 
				dim(mechanism[['body.points']])[2], 1))
		}
	}else{
		body_points <- mechanism[['body.points.anim']]
	}

	# GET NUMBER OF ITERATIONS
	num_iter <- dim(joints)[3]

	# GET XYZ OF ALL JOINTS
	xyz1 <- apply(array(joints[, , , 1], dim=c(dim(joints)[1:3])), 2, matrix, nrow=dim(joints)[1]*num_iter, ncol=dim(joints)[2])
	xyz2 <- apply(array(joints[, , , 2], dim=c(dim(joints)[1:3])), 2, matrix, nrow=dim(joints)[1]*num_iter, ncol=dim(joints)[2])

	# GET XYZ OF ALL POINTS
	if(!is.null(body_points)){
		xyz3 <- apply(body_points, 2, matrix, nrow=dim(body_points)[1]*num_iter, ncol=dim(body_points)[2])
		#xyz <- rbind(xyz, cbind(c(body_points[, 1, ]), c(body_points[, 2, ]), c(body_points[, 3, ])))
		#if(is.array(body.points) && length(dim(body.points)) > 2) xyz <- rbind(xyz, apply(body.points, 2, matrix, nrow=dim(body.points)[1]*dim(body.points)[3], ncol=dim(body.points)[2]))
	}else{
		xyz3 <- matrix(NA, nrow=0, ncol=3)
	}
	
	xyz <- matrix(NA, nrow=nrow(xyz1)+nrow(xyz2)+nrow(xyz3), ncol=3)
	xyz[1:nrow(xyz1), ] <- xyz1
	xyz[(nrow(xyz1)+1):(nrow(xyz1)+nrow(xyz2)), ] <- xyz2
	if(nrow(xyz3) > 0) xyz[(nrow(xyz1)+nrow(xyz2)+1):nrow(xyz), ] <- xyz3

	# GET XYZ CENTROID SIZE
	cs_xyz <- mean(distPointToPoint(xyz, colMeans(xyz, na.rm=TRUE)), na.rm=TRUE)

	# SET JOINT CONSTRAINT ARROW PROPERTIES
	arrow_len <- cs_xyz*0.3
	arrowhead_len <- cs_xyz*0.03

	# Set paths connecting points
	points_connect <- mechanism[['points.connect']]
	path_list <- NULL
	if(!is.null(body_points) && !is.null(points_connect)){

		# CREATE PATH LIST TO CONNECT JOINTS
		path_list <- vector("list", length(points_connect))

		if(!is.numeric(points_connect[[1]]) && !is.null(dimnames(body_points)[[1]])){
			for(i in 1:length(points_connect)){
				for(j in 1:length(points_connect[[i]])){

					# PATH PATH LABELS TO ROWNAMES OF POINT ARRAY
					grepl_match <- grepl(paste0('^', points_connect[[i]][j], '$'), dimnames(body_points)[[1]])
		
					# ADD TO PATH LIST
					if(sum(grepl_match) > 0) path_list[[i]] <- c(path_list[[i]], which(grepl_match))
				}
			}
		}else{
			for(i in 1:length(points_connect)) path_list[[i]] <- points_connect[[i]]
		}
	}

	if(method == "svgViewR"){
		
		# SET NA TO NONE
		path.col.fill[is.na(path.col.fill)] <- 'none'

		# VECTOR OF DRAWN LINKS
		drawn_links <- c()

		# Open new file connection
		svg.new(file=file, window.title=window.title, animate.duration=animate.duration, 
			animate.reverse=animate.reverse, animate.repeat, layers=writeLinkageLayers())

		# INITIAL PATH INDEX
		index_add <- 0

		# Animated
		animate <- TRUE
		#if(animate || (length(dim(joints)) > 2 && num_iter == 1)){

		## Plot joints
		# Get joint sets as array
		joints_n1 <- joints[, , , 1]
		if(dim(joints)[1] == 1) joints_n1 <- array(joints_n1, dim=dim(joints)[1:3])

		if(debug){

			# Get second joint set
			joints_n2 <- joints[, , , 2]
			if(dim(joints)[1] == 1) joints_n2 <- array(joints_n2, dim=dim(joints)[1:3])

			if(animate){
				svg.points(joints_n1, col.fill='red', col.stroke='red', 
					cex=joint.cex, lwd=joint.lwd, layer='Joints')
				svg.points(joints_n2, col.fill='none', 
					col.stroke='green', cex=joint.cex+2, lwd=joint.lwd, layer='Joints')
			}else{
				for(i in 1:num_iter){
					svg.points(joints_n1[, , i], col.fill='red', col.stroke='red', 
						cex=joint.cex, lwd=joint.lwd, layer='Joints')
					svg.points(joints_n2[, , i], col.fill='none', 
						col.stroke='green', cex=joint.cex+2, lwd=joint.lwd, layer='Joints')
				}
			}

		}else{

			if(animate){
				svg.points(joints_n1, col.fill=joint.col.fill, col.stroke=joint.col.stroke, 
					cex=joint.cex, lwd=joint.lwd, layer='Joints')
			}else{
				for(i in 1:num_iter){
					svg.points(joints_n1[, , i, 1], col.fill=joint.col.fill, 
						col.stroke=joint.col.stroke, cex=joint.cex, lwd=joint.lwd, layer='Joints')
				}
			}
		}

		## Create joint constraints
		# Create joint constraint lists
		cons_vec1 <- list()
		cons_vec2 <- list()

		# For each joint constraint
		for(i in 1:length(mechanism[['joint.cons']])){
		
			# Skip if NULL
			if(is.null(mechanism[['joint.cons']][[i]])) next

			# Skip if NA
			if(is.vector(mechanism[['joint.cons']][[i]]) && is.na(mechanism[['joint.cons']][[i]][1])){
				cons_vec1[[i]] <- NULL
				if(debug) cons_vec2[[i]] <- NULL
				next
			}
			
			# Set number of constraint vectors
			n_vectors <- dim(mechanism[['joint.cons']][[i]])[1]
			
			# Create empty arrays for vector origins and tips
			cons_vec1[[i]] <- array(NA, dim=c(n_vectors*2, 3, num_iter))
			if(debug) cons_vec2[[i]] <- array(NA, dim=c(n_vectors*2, 3, num_iter))

			if(n_vectors == 1){
				arrow_len_part <- arrow_len
			}else{
				arrow_len_part <- 0.8*arrow_len
			}

			# Fill arrays
			for(j in 1:num_iter){
				for(k in seq(1, n_vectors*2, by=2)){
					cons_vec1[[i]][k, , j] <- joints[i, , j, 1]
					cons_vec1[[i]][k+1, 1:3, j] <- joints[i, , j, 1] + 
						arrow_len_part*uvector(joint_cons[[i]][(k+1)/2, , j, 1])
					if(debug){
						cons_vec2[[i]][k, , j] <- joints[i, , j, 2]
						cons_vec2[[i]][k+1, 1:3, j] <- joints[i, , j, 2] + 
							0.8*arrow_len_part*uvector(joint_cons[[i]][(k+1)/2, , j, 2])
					}
				}
			}
		}

		## Draw joint constraint vectors
		for(i in 1:length(cons_vec1)){

			if(is.null(cons_vec1[[i]])) next
			
			# GET NUMBER OF VECTORS TO DRAW
			n_vectors <- dim(cons_vec1[[i]])[1]

			if(debug){
				for(k in seq(1, n_vectors, by=2)){

					if(k == 1){ col1 <- rgb(1,0,0); lwd1 <- 2; col2 <- rgb(0,1,0); lwd2 <- 2 }
					if(k == 3){ col1 <- rgb(0.75,0,0); lwd1 <- 3; col2 <- rgb(0,0.75,0); lwd2 <- 3 }
					if(k == 5){ col1 <- rgb(0.5,0,0); lwd1 <- 4; col2 <- rgb(0,0.5,0); lwd2 <- 4 }

					svg.arrows(x=cons_vec1[[i]][k:(k+1), 1:3, ], len=2*arrowhead_len, col=col1, 
						lwd=lwd1, layer='Joint constraints', z.index=1)
					svg.arrows(x=cons_vec2[[i]][k:(k+1), 1:3, ], len=2*arrowhead_len, col=col2, 
						lwd=lwd2, layer='Joint constraints', z.index=1)
				}
			}else{

				for(k in seq(1, n_vectors, by=2)){
					svg.arrows(x=cons_vec1[[i]][k:(k+1), 1:3, ], len=2*arrowhead_len, col='purple', 
						lwd=2, layer='Joint constraints', z.index=1)
				}
			}
		}

		# Advance point count for path index
		if(animate){
			if(debug){ index_add <- index_add + 2*dim(joints)[1] }else{ index_add <- index_add + dim(joints)[1] }
		}else{
			if(debug){ index_add <- index_add + (2*dim(joints)[1]*num_iter) }else{ index_add <- index_add + (dim(joints)[1]*num_iter) }
		}

		## Draw points and connecting paths
		if(!is.null(body_points)){
		
			# DRAW POINTS
			#svgviewr.points(body_points, file=file, col.fill=point.col.fill, col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd)

			if(animate){
				svg.points(body_points, col.fill=point.col.fill, 
					col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd, layer='Points')
			}else{
				for(i in 1:dim(body_points)[3]){
					svg.points(body_points[, , i], col.fill=point.col.fill, 
						col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd, layer='Points')
				}
			}
			
			# DRAW PATHS CONNECTING JOINTS
			if(!is.null(points_connect)){

				# CONNECT POINTS WITH PATHS
				path_list_add <- vector("list", length(points_connect))
				if(animate){
					for(j in 1:length(path_list)){
						if(is.null(path_list[[j]])) next
						path_list_add[[j]] <- path_list[[j]] + index_add
					}
					svg.pathsC(path_list_add, col.fill=path.col.fill, 
						opacity.fill=path.opacity.fill, col.stroke=path.col.stroke, 
						opacity.stroke=path.opacity.stroke, lwd=path.lwd, layer='Point wire frame')
				}else{
					for(i in 1:num_iter){
						for(j in 1:length(path_list)){
							if(is.null(path_list[[j]])) next
							path_list_add[[j]] <- path_list[[j]] + index_add + dim(link.points)[1]*(i-1)
						}
						svg.pathsC(path_list_add, col.fill=path.col.fill, 
							opacity.fill=path.opacity.fill, col.stroke=path.col.stroke, 
							opacity.stroke=path.opacity.stroke, lwd=path.lwd, layer='Point wire frame')
					}
				}
			}
			
			# DRAW LINES TRACING POINT TRAJECTORIES
			if(draw.trajectory){
				for(i in 1:dim(body_points)[2]){
					svg.lines(t(body_points[i, , ]), layer='Points', col=path.col.stroke)
				}
			}
		}

		# DRAW FRAME
		#print(xyz)
		svg.frame(x=xyz)

		svg.close()
		return(1)


		# ANIMATED
		if(animate || (length(dim(joints)) > 2 && num_iter == 1)){


		}else{

			# DRAW CONSTRAINT VECTOR
			for(i in 1:length(cons_vec)){

				if(is.null(cons_vec[[i]])) next
				
				# CHECK THAT JOINT CONSTRAINT VECTOR CHANGES OVER TIME
				j_max <- dim(cons_vec[[i]])[3]
				if(sum(apply(cons_vec[[i]], 1:2, sd)) < 1e-10) j_max <- 1

				for(j in 1:j_max){

					svg.arrows(x=cons_vec[[i]][, 1:3, j], len=arrowhead_len, col='purple', 
						lwd=2, layer='Joint constraints', z.index=1)
				}
			}
		}


		# ADVANCE POINT COUNT FOR PATH INDEX
		if(animate){
			if(debug){ index_add <- index_add + 2*dim(joints)[1] }else{ index_add <- index_add + dim(joints)[1] }
		}else{
			if(debug){ index_add <- index_add + (2*dim(joints)[1]*num_iter) }else{ index_add <- index_add + (dim(joints)[1]*num_iter) }
		}

		# DRAW POINTS AND CONNECTING PATHS
		if(!is.null(body_points)){

			# DRAW POINTS
			#svgviewr.points(body_points, file=file, col.fill=point.col.fill, col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd)

			if(animate || (length(dim(body_points)) > 2 && dim(body_points)[3] == 1)){
				svg.points(body_points, col.fill=point.col.fill, 
					col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd, layer='Points')
			}
			if(!animate && (length(dim(body_points)) > 2 && dim(body_points)[3] > 1)){
				for(i in 1:dim(body_points)[3]){
					svg.points(body_points[, , i], col.fill=point.col.fill, 
						col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd, layer='Points')
				}
			}
			
			# DRAW PATHS CONNECTING JOINTS
			if(!is.null(points_connect)){

				# CONNECT POINTS WITH PATHS
				path_list_add <- vector("list", length(points_connect))
				if(animate){
					for(j in 1:length(path_list)){
						if(is.null(path_list[[j]])) next
						path_list_add[[j]] <- path_list[[j]] + index_add
					}
					svg.pathsC(path_list_add, col.fill=path.col.fill, 
						opacity.fill=path.opacity.fill, col.stroke=path.col.stroke, 
						opacity.stroke=path.opacity.stroke, lwd=path.lwd, layer='Point wire frame')
				}else{
					for(i in 1:num_iter){
						for(j in 1:length(path_list)){
							if(is.null(path_list[[j]])) next
							path_list_add[[j]] <- path_list[[j]] + index_add + dim(link.points)[1]*(i-1)
						}
						svg.pathsC(path_list_add, col.fill=path.col.fill, 
							opacity.fill=path.opacity.fill, col.stroke=path.col.stroke, 
							opacity.stroke=path.opacity.stroke, lwd=path.lwd, layer='Point wire frame')
					}
				}
			}
			
			# DRAW LINES TRACING POINT TRAJECTORIES
			if(draw.trajectory){
				for(i in 1:dim(body_points)[2]){
					svg.lines(t(body_points[i, , ]), layer='Points', col=path.col.stroke)
				}
			}
		}
		
		# DRAW FRAME
		svg.frame(x=xyz)

		svg.close()
	}
	
	NULL
}
