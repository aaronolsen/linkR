drawMechanism <- function(linkage, method = "svgViewR", file = NULL, animate = TRUE, 
	animate.duration = 1, animate.reverse = FALSE, animate.repeat = -1, 
	connect.joints=TRUE, window.title='Linkage Viewer', joint.col.fill="white", 
	joint.col.stroke="black", joint.cex=1.5, joint.lwd=2,
	point.col.fill="black", point.col.stroke="black", point.cex=1, point.lwd=2,
	path.col.fill=NA, path.opacity.fill=1, path.opacity.stroke=1, path.col.stroke="black", 
	path.lwd = 1, draw.trajectory = FALSE, add = FALSE, debug = FALSE, ...){

	#if(is.null(file) && method == "svgViewR") stop("To plot a linkage using the svgViewR method, 'file' must be non-NULL.")
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

	# COPY JOINTS
	joints <- linkage$joint.coor
	body.points <- linkage$body.points

	# IF MATRIX, CONVERT TO ARRAY
	if(length(dim(joints)) == 2) joints <- array(joints, dim=c(dim(joints)[1], dim(joints)[2], 1))
	if(length(dim(points)) == 2) points <- array(points, dim=c(dim(points)[1], dim(points)[2], 1))

	# GET NUMBER OF ITERATIONS
	num_iter <- dim(joints)[3]

	# GET XYZ OF ALL JOINTS
	if(is.matrix(joints)) xyz <- joints
	if(is.array(joints) && length(dim(joints)) > 2) xyz <- apply(joints, 2, matrix, nrow=dim(joints)[1]*num_iter, ncol=dim(joints)[2])

	# GET XYZ OF ALL POINTS
	if(!is.null(body.points)){
		if(is.matrix(body.points)) xyz <- rbind(xyz, body.points)
		if(is.array(body.points) && length(dim(body.points)) == 3) xyz <- rbind(xyz, cbind(c(body.points[, 1, ]), c(body.points[, 2, ]), c(body.points[, 3, ])))
		#if(is.array(body.points) && length(dim(body.points)) > 2) xyz <- rbind(xyz, apply(body.points, 2, matrix, nrow=dim(body.points)[1]*dim(body.points)[3], ncol=dim(body.points)[2]))
	}

	# GET XYZ CENTROID SIZE
	cs_xyz <- mean(distPointToPoint(xyz, colMeans(xyz, na.rm=TRUE)), na.rm=TRUE)

	# SET JOINT CONSTRAINT ARROW PROPERTIES
	arrow_len <- cs_xyz*0.5
	arrowhead_len <- cs_xyz*0.03

	col_cons <- c('purple', 'hotpink')
	
	# SET PATHS CONNECTING POINTS
	points.connect <- linkage$points.connect
	path_list <- NULL
	if(!is.null(body.points) && !is.null(points.connect)){

		# CREATE PATH LIST TO CONNECT JOINTS
		path_list <- vector("list", length(points.connect))

		if(!is.numeric(points.connect[[1]]) && !is.null(dimnames(body.points)[[1]])){
			for(i in 1:length(points.connect)){
				for(j in 1:length(points.connect[[i]])){

					# PATH PATH LABELS TO ROWNAMES OF POINT ARRAY
					grepl_match <- grepl(paste0('^', points.connect[[i]][j], '$'), dimnames(body.points)[[1]])
		
					# ADD TO PATH LIST
					if(sum(grepl_match) > 0) path_list[[i]] <- c(path_list[[i]], which(grepl_match))
				}
			}
		}else{
			for(i in 1:length(points.connect)) path_list[[i]] <- points.connect[[i]]
		}
	}
	
	if(method == "svgViewR"){
		
		# SET NA TO NONE
		path.col.fill[is.na(path.col.fill)] <- 'none'

		# VECTOR OF DRAWN LINKS
		drawn_links <- c()

		# CREATE NEW SVG FILE
		if(!file.exists(file) || (file.exists(file) && !add)){

			con <- svg.new(file=file, window.title=window.title, animate.duration=animate.duration, 
				animate.reverse=animate.reverse, animate.repeat, layers=writeLinkageLayers())
		}else{

			# FIX : ADD CAPABILITY TO UPDATE/ADD TO ALREADY EXISTING FILE VIA CONNECTION
			con <- file
		}

		# IF ADD, COUNT NUMBER OF EXISTING POINT TAGS
		num_points <- 0
		if(add){

			# READ FILE
			if(is.list(con)){
				# Read lines - readChar requires number of characters to read and I could not figure out how to do that with a connection
				file_read <- paste(readLines(con$con, n=-1), collapse='')
			
				# readLines leaves internal current position of connection at the end - reset to beginning
				seek(con$con, 0, rw = "r")

			}else{
				file_read <- readChar(con, file.info(con)$size)
			}
		
			# COUNT THE NUMBER OF POINT TAGS ALREADY IN THE SVG FILE
			str_split <- strsplit(file_read, "<point ")[[1]]
			num_points <- length(str_split) - 1
		}
		
		# INITIAL PATH INDEX
		index.add <- num_points

		# CREATE JOINT CONSTRAINTS
		cons_vec <- list()
		if(debug){
			cons_vec1 <- cons_vec
			cons_vec2 <- cons_vec
		}
		for(i in 1:length(linkage$joint.cons)){
		
			if(is.null(linkage$joint.cons[[i]])) next

			if(is.vector(linkage$joint.cons[[i]]) && is.na(linkage$joint.cons[[i]][1])){
				cons_vec[[i]] <- NULL
				next
			}
			
			# SET NUMBER OF CONSTRAINT VECTORS
			n_vectors <- dim(linkage$joint.cons[[i]])[1]
			
			# CREATE EMPTY ARRAY
			cons_vec[[i]] <- array(NA, dim=c(n_vectors*2, 3, num_iter))
			
			# FILL ARRAY
			for(j in 1:num_iter){
				for(k in seq(1, n_vectors*2, by=2)){
					cons_vec[[i]][k, , j] <- joints[i, , j]
					cons_vec[[i]][k+1, 1:3, j] <- joints[i, , j] + arrow_len*uvector(linkage$joint.cons[[i]][(k+1)/2, , j])
				}
			}

			if(n_vectors == 1){
				arrow_len_part <- arrow_len
			}else{
				arrow_len_part <- arrow_len / 2
			}

			if(debug){

				# CREATE EMPTY ARRAY
				cons_vec1[[i]] <- cons_vec2[[i]] <- array(NA, dim=c(n_vectors*2, 3, num_iter))

				# FILL ARRAY
				for(j in 1:num_iter){
					for(k in seq(1, n_vectors*2, by=2)){
						cons_vec1[[i]][k, , j] <- linkage$joint.coorn[i, , j, 1]
						cons_vec1[[i]][k+1, 1:3, j] <- linkage$joint.coorn[i, , j, 1] + arrow_len_part*uvector(linkage$joint.consn[[i]][(k+1)/2, , j, 1])
						cons_vec2[[i]][k, , j] <- linkage$joint.coorn[i, , j, 2]
						cons_vec2[[i]][k+1, 1:3, j] <- linkage$joint.coorn[i, , j, 2] + arrow_len_part*uvector(linkage$joint.consn[[i]][(k+1)/2, , j, 2])
					}
				}
			}
		}

		# ANIMATED
		if(animate || (length(dim(joints)) > 2 && num_iter == 1)){

			# DRAW ANIMATED JOINTS
			if(debug){
			
				joints_n1 <- linkage$joint.coorn[, , , 1]
				joints_n2 <- linkage$joint.coorn[, , , 2]
				if(dim(linkage$joint.coorn)[1] == 1){
					joints_n1 <- array(joints_n1, dim=dim(linkage$joint.coorn)[1:3])
					joints_n2 <- array(joints_n2, dim=dim(linkage$joint.coorn)[1:3])
				}
			
				svg.points(joints_n1, col.fill='red', 
					col.stroke='red', cex=joint.cex, lwd=joint.lwd, layer='Joints')
				svg.points(joints_n2, col.fill='none', 
					col.stroke='green', cex=joint.cex+2, lwd=joint.lwd, layer='Joints')
			}else{

				joints_plot <- joints
				if(dim(joints)[1] == 1) joints_plot <- array(joints_plot, dim=dim(joints)[1:3])

				svg.points(joints_plot, col.fill=joint.col.fill, 
					col.stroke=joint.col.stroke, cex=joint.cex, lwd=joint.lwd, layer='Joints')
			}

			# DRAW CONSTRAINT VECTOR
			for(i in 1:length(cons_vec)){

				if(is.null(cons_vec[[i]])) next
				
				# GET NUMBER OF VECTORS TO DRAW
				n_vectors <- dim(cons_vec[[i]])[1]

				if(debug){
					for(k in seq(1, n_vectors, by=2)){
						if(k == 1){ col1 <- 'pink'; lwd1 <- 2; col2 <- 'springgreen'; lwd2 <- 2 }
						if(k == 3){ col1 <- 'red'; lwd1 <- 3; col2 <- 'green'; lwd2 <- 3 }
						if(k == 5){ col1 <- 'darkred'; lwd1 <- 4; col2 <- 'darkgreen'; lwd2 <- 4 }

						svg.arrows(x=cons_vec1[[i]][k:(k+1), 1:3, ], len=arrowhead_len, col=col1, 
							lwd=lwd1, layer='Joint constraints', z.index=1)
						svg.arrows(x=cons_vec2[[i]][k:(k+1), 1:3, ], len=2*arrowhead_len, col=col2, 
							lwd=lwd2, layer='Joint constraints', z.index=1)
					}
				}else{

					for(k in seq(1, n_vectors, by=2)){
						svg.arrows(x=cons_vec[[i]][k:(k+1), 1:3, ], len=arrowhead_len, col=col_cons[1], 
							lwd=2, layer='Joint constraints', z.index=1)
					}
				}
			}
		}
		
		# STATIC
		if(!animate && (length(dim(joints)) > 2 && num_iter > 1)){
			
			# DRAW JOINTS
			for(i in 1:num_iter){
				svg.points(joints[, , i], col.fill=joint.col.fill, 
					col.stroke=joint.col.stroke, cex=joint.cex, lwd=joint.lwd, layer='Joints')
			}

			# DRAW CONSTRAINT VECTOR
			for(i in 1:length(cons_vec)){

				if(is.null(cons_vec[[i]])) next
				
				# CHECK THAT JOINT CONSTRAINT VECTOR CHANGES OVER TIME
				j_max <- dim(cons_vec[[i]])[3]
				if(sum(apply(cons_vec[[i]], 1:2, sd)) < 1e-10) j_max <- 1

				for(j in 1:j_max){

					svg.arrows(x=cons_vec[[i]][, 1:3, j], len=arrowhead_len, col=col_cons[1], 
						lwd=2, layer='Joint constraints', z.index=1)
				}
			}
		}

		# CONNECT JOINTS WITH PATHS
		if(connect.joints && !is.null(linkage$joint.conn)){
			if(animate){
				for(i in 1:nrow(linkage$joint.conn)){

					# SKIP LINES AMONG JOINTS CONNECTED TO GROUND
					#if(linkage$joint.conn[i, 'body.idx'] == 1) next
					if(linkage$joint.conn[i, 'joint1'] == 0 || linkage$joint.conn[i, 'joint2'] == 0) next
					#if(linkage$joint.conn[i, 'joint1'] == 1 || linkage$joint.conn[i, 'joint2'] == 1) next

					# ADD PATHS
					svg.pathsC(c(linkage$joint.conn[i, 'joint1'], linkage$joint.conn[i, 'joint2']), 
						index.add=index.add, lwd=path.lwd, layer='Joint wire frame')
				}
			}else{

				for(itr in 1:num_iter){
					for(i in 1:nrow(linkage$joint.conn)){

						# SKIP LINES AMONG JOINTS CONNECTED TO GROUND
						if(linkage$joint.conn[i, 'body.idx'] == 1) next
						#if(linkage$joint.conn[i, 'joint1'] == 1 || linkage$joint.conn[i, 'joint2'] == 1) next

						# ADD PATHS
						svg.pathsC(c(linkage$joint.conn[i, 'joint1'], linkage$joint.conn[i, 'joint2']), 
							index.add=(itr-1)*(nrow(joints)), lwd=path.lwd, layer='Joint wire frame')
					}
				}
			}
		}

		# ADVANCE POINT COUNT FOR PATH INDEX
		if(animate){
			if(debug){ index.add <- index.add + 2*dim(joints)[1] }else{ index.add <- index.add + dim(joints)[1] }
		}else{
			if(debug){ index.add <- index.add + (2*dim(joints)[1]*num_iter) }else{ index.add <- index.add + (dim(joints)[1]*num_iter) }
		}

		# DRAW POINTS AND CONNECTING PATHS
		if(!is.null(body.points)){

			# DRAW POINTS
			#svgviewr.points(body.points, file=file, col.fill=point.col.fill, col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd)

			if(animate || (length(dim(body.points)) > 2 && dim(body.points)[3] == 1)){
				svg.points(body.points, col.fill=point.col.fill, 
					col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd, layer='Points')
			}
			if(!animate && (length(dim(body.points)) > 2 && dim(body.points)[3] > 1)){
				for(i in 1:dim(body.points)[3]){
					svg.points(body.points[, , i], col.fill=point.col.fill, 
						col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd, layer='Points')
				}
			}
			
			# DRAW PATHS CONNECTING JOINTS
			if(!is.null(points.connect)){

				# CONNECT POINTS WITH PATHS
				path_list_add <- vector("list", length(points.connect))
				if(animate){
					for(j in 1:length(path_list)){
						if(is.null(path_list[[j]])) next
						path_list_add[[j]] <- path_list[[j]] + index.add
					}
					svg.pathsC(path_list_add, col.fill=path.col.fill, 
						opacity.fill=path.opacity.fill, col.stroke=path.col.stroke, 
						opacity.stroke=path.opacity.stroke, lwd=path.lwd, layer='Point wire frame')
				}else{
					for(i in 1:num_iter){
						for(j in 1:length(path_list)){
							if(is.null(path_list[[j]])) next
							path_list_add[[j]] <- path_list[[j]] + index.add + dim(link.points)[1]*(i-1)
						}
						svg.pathsC(path_list_add, col.fill=path.col.fill, 
							opacity.fill=path.opacity.fill, col.stroke=path.col.stroke, 
							opacity.stroke=path.opacity.stroke, lwd=path.lwd, layer='Point wire frame')
					}
				}
			}
			
			# DRAW LINES TRACING POINT TRAJECTORIES
			if(draw.trajectory){
				for(i in 1:dim(body.points)[2]){
					svg.lines(t(body.points[i, , ]), layer='Points', col=path.col.stroke)
				}
			}
		}
		
		# DRAW FRAME
		svg.frame(x=xyz)

		svg.close()
	}
	
	NULL
}
