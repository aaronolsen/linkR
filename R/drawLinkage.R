drawLinkage <- function(linkage, method = "svgViewR", file = NULL, animate = TRUE, 
	animate.duration = 1, animate.reverse = FALSE, animate.repeat = -1, path.connect=NULL, 
	connect.joints=TRUE, window.title='Linkage Viewer', joint.col.fill="white", 
	joint.col.stroke="black", joint.cex=1.5, joint.lwd=2,
	point.col.fill="black", point.col.stroke="black", point.cex=1, point.lwd=2,
	path.col.fill=NA, path.opacity.fill=1, path.opacity.stroke=1, path.col.stroke="black", 
	path.lwd = 1, add = FALSE, ...){

	#if(is.null(file) && method == "svgViewR") stop("To plot a linkage using the svgViewR method, 'file' must be non-NULL.")
	if(is.null(file)) method <- "plot"

	# FIND FILE TYPE
	if(!is.null(file)){
		extension_found <- FALSE
		if(grepl('[.](jpg$|jpeg$|bmp$|png$|tiff$|eps$)', file, ignore.case=TRUE)){
			method <- "plot"
			extension_found <- TRUE
		}
		if(grepl('[.]html$', file, ignore.case=TRUE)){
			method <- "svgViewR"
			extension_found <- TRUE
		}
		if(grepl('[.][A-Za-z0-9]+$', file, ignore.case=TRUE) && !extension_found) stop("File extension not recognized.")
		if(!extension_found){
			method <- "svgViewR"
			file <- paste0(file, '.html')
		}

		# CHECK FOR FILE WRITING PERMISSION
		# THIS CREATES AN EMPTY FILE - COULD CAUSE PROBLEMS WITH 'ADD' AND OVERWRITING AN EXISTING FILE
		#tt <- tryCatch(write("", file),error=function(e) e, warning=function(w) w)
		#if(!is.null(tt)){
		#	if(grepl('permission denied', tt, ignore.case=TRUE)){
		#		stop("It looks like your current working directory is not granting R permission to write files. Please change your current working directory to one with file writing permissions (e.g. Desktop).")
		#	}
		#}
	}

	# COPY POINTS
	link.points <- linkage$link.points

	# If matrix, convert to array
	if(length(dim(link.points)) == 2) link.points <- array(link.points, dim=c(dim(link.points), 1), dimnames=list(rownames(link.points), colnames(link.points), NULL))

	# COPY JOINTS
	joints <- linkage$joint.coor

	# IF MATRIX, CONVERT TO ARRAY
	if(length(dim(joints)) == 2) joints <- array(joints, dim=c(dim(joints)[1], dim(joints)[2], 1))
	
	# GET XYZ OF ALL JOINTS
	if(is.matrix(joints)) xyz <- joints
	if(is.array(joints) && length(dim(joints)) > 2) xyz <- apply(joints, 2, matrix, nrow=dim(joints)[1]*dim(joints)[3], ncol=dim(joints)[2])
	
	# GET XYZ OF ALL POINTS
	if(!is.null(link.points)){
		if(is.matrix(link.points)) xyz <- rbind(xyz, link.points)
		if(is.array(link.points) && length(dim(link.points)) > 2) xyz <- rbind(xyz, apply(link.points, 2, matrix, nrow=dim(link.points)[1]*dim(link.points)[3], ncol=dim(link.points)[2]))
	}

	# GET XYZ CENTROID SIZE
	cs_xyz <- mean(distPointToPoint(xyz, colMeans(xyz)))

	# GET XYZ RANGE
	#range_xyz <- apply(xyz, 2, range)
	
	# GET MAXIMUM DIMENSION
	#max_dim_xyz <- max(range_xyz[2, ] - range_xyz[1, ])
	
	# SET JOINT CONSTRAINT ARROW PROPERTIES
	arrow_len <- cs_xyz*0.25
	arrowhead_len <- cs_xyz*0.03

	col_lcs <- c('red', 'green', 'blue')
	col_cons <- 'purple'
	
	# If input path.connect is NULL and path connect found with linkage, overwrite
	if(is.null(path.connect) && !is.null(linkage$path.connect)) path.connect <- linkage$path.connect

	# SET PATHS CONNECTING POINTS
	path_list <- NULL
	if(!is.null(link.points) && !is.null(path.connect)){

		# CREATE PATH LIST TO CONNECT JOINTS
		path_list <- vector("list", length(path.connect))

		if(!is.numeric(path.connect[[1]]) && !is.null(dimnames(link.points)[[1]])){
			for(i in 1:length(path.connect)){
				for(j in 1:length(path.connect[[i]])){

					# PATH PATH LABELS TO ROWNAMES OF POINT ARRAY
					grepl_match <- grepl(paste0('^', path.connect[[i]][j], '$'), dimnames(link.points)[[1]])
		
					# ADD TO PATH LIST
					if(sum(grepl_match) > 0) path_list[[i]] <- c(path_list[[i]], which(grepl_match))
				}
			}
		}else{
			for(i in 1:length(path.connect)) path_list[[i]] <- path.connect[[i]]
		}
	}

	if(method == "plot"){

		# GET MATRIX OF ALL POINTS
		all_points <- apply(joints, 2, rbind)
		if(!is.null(link.points)) all_points <- rbind(all_points, apply(link.points, 2, rbind))

		# FIND CENTER OF ALL POINTS
		vcenter <- colMeans(all_points, na.rm=TRUE)
		
		# CENTER ALL COORDINATES ABOUT THE ORIGIN
		joints <- joints - array(matrix(vcenter, nrow=dim(joints)[1], ncol=dim(joints)[2], byrow=TRUE), dim=dim(joints))
		if(!is.null(link.points)) link.points <- link.points - array(matrix(vcenter, nrow=dim(link.points)[1], ncol=dim(link.points)[2], byrow=TRUE), dim=dim(link.points))
		all_points <- all_points - matrix(vcenter, nrow=nrow(all_points), ncol=ncol(all_points), byrow=TRUE)
		
		# PROJECT 3D TO 2D
		joints2d <- proj3DTo2D(joints)
		if(!is.null(link.points)) points2d <- proj3DTo2D(link.points)
		all_points2d <- proj3DTo2D(all_points)

		# SET GRAPHICAL PARAMETERS
		gp_joints <- c("joint.col.fill", "joint.col.stroke", "joint.cex", "joint.lwd")
		gp_points <- c("point.col.fill", "point.col.stroke", "point.cex", "point.lwd")
		gp_paths <- c("path.col.fill", "path.opacity.fill", "path.opacity.stroke", "path.col.stroke", "path.lwd")

		# CONVERT GRAPHICAL PARAMETERS TO VECTORS WITH SAME NUMBER OF ELEMENTS OF FIRST X DIMENSION
		for(gpar in gp_joints) if(length(get(gpar)) == 1) assign(gpar, rep(get(gpar), dim(joints)[1]))
		if(!is.null(link.points)) for(gpar in gp_points) if(length(get(gpar)) == 1) assign(gpar, rep(get(gpar), dim(link.points)[1]))
		if(!is.null(path.connect)) for(gpar in gp_paths) if(length(get(gpar)) == 1) assign(gpar, rep(get(gpar), length(path.connect)))

		# INITIATE IMAGE FILE
		if(!is.null(file)){
			if(grepl('.bmp$', file, ignore.case=TRUE)) bmp(file, ...)
			if(grepl('.png$', file, ignore.case=TRUE)) png(file, ...)
			if(grepl('.jpg$|.jpeg$', file, ignore.case=TRUE)) jpeg(file, ...)
			if(grepl('.tiff$', file, ignore.case=TRUE)) tiff(file, ...)
			if(grepl('.eps$', file, ignore.case=TRUE)) cairo_ps(file, ...)
		}

		# CREATE PLOT DEVICE THAT FITS ALL JOINT COORDINATES
		par(mar=c(0,0,0,0))
		plot(all_points2d, asp=1, type='n', bty='n', yaxt='n', xaxt='n', xlab='', ylab='')

		for(itr in 1:dim(joints)[3]){

			# PLOT JOINTS
			points(joints2d[, , itr], pch=20, col=joint.col.stroke, cex=joint.cex)

			# CONNECT JOINTS WITH LINES
			if(connect.joints){
				for(i in 1:nrow(linkage$joint.links)){

					# SKIP LINES AMONG JOINTS CONNECTED TO GROUND
					if(linkage$joint.links[i, 'Link.idx'] == 0) next

					# ADD SEGMENTS
					lines(joints2d[c(linkage$joint.links[i, 'Joint1'], linkage$joint.links[i, 'Joint2']), 1, itr], 
						joints2d[c(linkage$joint.links[i, 'Joint1'], linkage$joint.links[i, 'Joint2']), 2, itr])
				}
			}

			# PLOT POINTS
			if(!is.null(link.points)) points(points2d[, , itr], pch=20, 
				col=point.col.stroke, cex=point.cex)

			# CONNECT POINTS WITH PATHS
			if(!is.null(path_list)){
				for(i in 1:length(path_list)){
				
					path <- path_list[[i]]

					if(is.null(path)) next

					polygon(points2d[path, 1, itr], points2d[path, 2, itr], border=NA, col=path.col.fill[i])

					# DRAW LINES
					#if(fade.with.iteration){
					#	lines(points2d[path, 1, itr], points2d[path, 2, itr], col=rgb(0,0,0, 1 - (itr / (dim(joints)[3]+1))), lwd=path.lwd[i])
					#}else{
					#	lines(points2d[path, 1, itr], points2d[path, 2, itr], col=path.col.stroke[i], lwd=path.lwd[i])
					#}
				}
			}
		}
		
		if(!is.null(file)) dev.off()
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
		for(i in 1:dim(joints)[1]){
		
			if(is.na(linkage$joint.cons[[i]][1])){
				cons_vec[[i]] <- NULL
				next
			}
			
			# CREATE EMPTY ARRAY
			cons_vec[[i]] <- array(NA, dim=c(2, dim(joints)[2:3]))

			# FILL ARRAY
			for(j in 1:dim(joints)[3]){
				cons_vec[[i]][1, , j] <- joints[i, , j]
				if(is.null(linkage$joint.cons.dyn)){
					if(is.vector(linkage$joint.cons[[i]])) cons_vec[[i]][2, , j] <- joints[i, , j] + arrow_len*uvector(linkage$joint.cons[[i]])
				}else{
					cons_vec[[i]][2, , j] <- joints[i, , j] + arrow_len*uvector(linkage$joint.cons.dyn[[i]][j, ])
				}
			}
		}

		lcs_vec <- list()
		for(i in 1:length(linkage$link.lcs)){

			lcs_vec[[i]] <- array(NA, dim=c(4, dim(joints)[2:3]))

			# If matrix, convert to array
			if(length(dim(linkage$link.lcs[[i]])) == 2) linkage$link.lcs[[i]] <- array(linkage$link.lcs[[i]], dim=c(dim(linkage$link.lcs[[i]]), 1))

			lcs_vec[[i]][1,,] <- linkage$link.lcs[[i]][1,,]

			for(j in 1:dim(joints)[3]){
				for(k in 2:4){
					lcs_vec[[i]][k, , j] <- linkage$link.lcs[[i]][1, , j] + arrow_len*0.5*(linkage$link.lcs[[i]][k, , j]-linkage$link.lcs[[i]][1, , j])
				}
			}
		}
		
		# DRAW FRAME
		svg.frame(x=xyz)

		# ANIMATED
		if(animate || (length(dim(joints)) > 2 && dim(joints)[3] == 1)){

			# DRAW ANIMATED JOINTS
			svg.points(joints, col.fill=joint.col.fill, 
				col.stroke=joint.col.stroke, cex=joint.cex, lwd=joint.lwd, layer='Joints')

			# DRAW CONSTRAINT VECTOR
			for(i in 1:length(cons_vec)){
				if(is.null(cons_vec[[i]])) next
				svg.arrows(x=cons_vec[[i]], len=arrowhead_len, col=col_cons, 
					lwd=2, layer='Joint constraints', z.index=1)
			}

			# DRAW LOCAL COORDINATE SYSTEMS ASSOCIATED WITH EACH LINK
			for(i in 1:length(lcs_vec)){
				for(j in 2:4){
					svg.arrows(x=lcs_vec[[i]][c(1,j), ,], len=arrowhead_len, 
						col=col_lcs[j-1], layer='Link coordinate systems', z.index=1)
				}
			}
		}
		
		# STATIC
		if(!animate && (length(dim(joints)) > 2 && dim(joints)[3] > 1)){
			
			# DRAW JOINTS
			for(i in 1:dim(joints)[3]){
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
					svg.arrows(x=cons_vec[[i]][, , j], len=arrowhead_len, col=col_cons, 
						lwd=2, layer='Joint constraints', z.index=1)
				}
			}

			# DRAW LOCAL COORDINATE SYSTEMS ASSOCIATED WITH EACH LINK
			for(i in 1:length(lcs_vec)){

				# CHECK THAT VECTORS CHANGE OVER TIME
				k_max <- dim(lcs_vec[[i]])[3]
				if(sum(apply(lcs_vec[[i]], 1:2, sd)) < 1e-10) k_max <- 1

				for(k in 1:k_max){
					for(j in 2:4){
						svg.arrows(x=lcs_vec[[i]][c(1,j), , k], len=arrowhead_len, 
							col=col_lcs[j-1], layer='Link coordinate systems', z.index=1)
					}
				}
			}
		}
		
		# CONNECT JOINTS WITH PATHS
		if(connect.joints){
			if(animate){
				for(i in 1:nrow(linkage$joint.links)){

					# SKIP LINES AMONG JOINTS CONNECTED TO GROUND
					if(linkage$joint.links[i, 'Link.idx'] == 0) next

					# ADD PATHS
					svg.pathsC(c(linkage$joint.links[i, 'Joint1'], linkage$joint.links[i, 'Joint2']), 
						index.add=index.add, lwd=path.lwd, layer='Joint wire frame')
				}
			}else{

				for(itr in 1:dim(joints)[3]){
					
					for(i in 1:nrow(linkage$joint.links)){

						# SKIP LINES AMONG JOINTS CONNECTED TO GROUND
						if(linkage$joint.links[i, 'Link.idx'] == 0) next

						# ADD PATHS
						svg.pathsC(c(linkage$joint.links[i, 'Joint1'], linkage$joint.links[i, 'Joint2']), 
							index.add=(itr-1)*(nrow(joints)), lwd=path.lwd, layer='Joint wire frame')
					}
				}
			}
		}

		# ADVANCE POINT COUNT FOR PATH INDEX
		if(animate){
			index.add <- index.add + dim(joints)[1]
		}else{
			index.add <- index.add + (dim(joints)[1]*dim(joints)[3])
		}
		
		# DRAW POINTS AND CONNECTING PATHS
		if(!is.null(link.points)){

			# DRAW POINTS
			#svgviewr.points(link.points, file=file, col.fill=point.col.fill, col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd)

			if(animate || (length(dim(link.points)) > 2 && dim(link.points)[3] == 1)){
				svg.points(link.points, col.fill=point.col.fill, 
					col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd, layer='Points')
			}
			if(!animate && (length(dim(link.points)) > 2 && dim(link.points)[3] > 1)){
				for(i in 1:dim(link.points)[3]){
					svg.points(link.points[, , i], col.fill=point.col.fill, 
						col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd, layer='Points')
				}
			}

			# DRAW PATHS CONNECTING JOINTS
			if(!is.null(path.connect)){

				# CONNECT POINTS WITH PATHS
				path_list_add <- vector("list", length(path.connect))
				if(animate){
					for(j in 1:length(path_list)){
						if(is.null(path_list[[j]])) next
						path_list_add[[j]] <- path_list[[j]] + index.add
					}
					svg.pathsC(path_list_add, col.fill=path.col.fill, 
						opacity.fill=path.opacity.fill, col.stroke=path.col.stroke, 
						opacity.stroke=path.opacity.stroke, lwd=path.lwd, layer='Point wire frame')
				}else{
					for(i in 1:dim(joints)[3]){
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
		}
		
		svg.close()
	}
	
	NULL
}
