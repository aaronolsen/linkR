defineMechanism <- function(joint.coor, joint.types, joint.cons, body.conn, fixed.body = 'Fixed', 
	find.paths = TRUE, print.progress = FALSE){

	if(print.progress) cat('defineMechanism()\n')
	indent <- '  '

	# MAKE SURE JOINT COORDINATES ARE MATRIX
	if(is.vector(joint.coor)) joint.coor <- matrix(joint.coor, nrow=1)

	# MAKE SURE BODY.CONN IS MATRIX
	if(is.vector(body.conn)) body.conn <- matrix(body.conn, nrow=1, ncol=2)

	# VALIDATE INPUTS
	if(!is.null(body.conn) && nrow(body.conn) != length(joint.types)) stop(paste0("The number of rows in 'body.conn' (", nrow(body.conn), ") must be equal to the number of joints specified in 'joint.types' (", length(joint.types), ")."))
	if(length(joint.types) != nrow(joint.coor)) stop(paste0("The number of rows in 'joint.coor' (", nrow(joint.coor), ") must be equal to the number of joints specified in 'joint.types' (", length(joint.types), ")."))

	# MAKE SURE JOINT TYPE LETTERS ARE UPPERCASE FOR STRING MATCHING
	if(!is.null(joint.types)) joint.types <- toupper(joint.types)

	# CHECK THAT JOINTS ARE 3D
	if(ncol(joint.coor) == 2) stop("Joint coordinates must be three-dimensional.")
	
	# GET NUMBER OF JOINTS
	n_joints <- nrow(joint.coor)

	# MAKE SURE JOINT CONSTRAINTS ARE LIST
	if(!is.list(joint.cons)) joint.cons <- list(joint.cons)

	for(i in 1:length(joint.cons)){

		# IF JOINT CONSTRAINT IS N (NO CONSTRAINT) MAKE SURE IT IS NA
		if(joint.types[i] == 'N'){ joint.cons[[i]] <- NA; next }

		# IF JOINT CONSTRAINT IS ALREADY AN ARRAY, MAKE SURE VECTORS ARE UNIT VECTORS
		if(is.array(joint.cons[[i]]) && length(dim(joint.cons[[i]])) > 2) for(j in 1:dim(joint.cons[[i]])) joint.cons[[i]][, , j] <- uvector(joint.cons[[i]][, , j])

		# MAKE SURE JOINT CONSTRAINTS ARE ARRAY AND MAKE SURE VECTORS ARE UNIT LENGTH
		if(is.vector(joint.cons[[i]])) joint.cons[[i]] <- array(uvector(joint.cons[[i]]), dim=c(1, length(joint.cons[[i]]), 1))
		if(is.matrix(joint.cons[[i]])) joint.cons[[i]] <- array(uvector(joint.cons[[i]]), dim=c(dim(joint.cons[[i]]), 1))
	}

	# ADD ROWNAMES TO JOINTS (NAME BASED ON JOINT TYPE AND ORDER IN INPUT SEQUENCE)
	if(is.null(rownames(joint.coor))){
		joint_names <- rep(NA, n_joints)
		for(i in 1:n_joints){
			if(i == 1){
				joint_names[i] <- paste0(joint.types[i], i)
			}else{
				joint_names[i] <- paste0(joint.types[i], 1 + sum(joint.types[1:(i-1)] == joint.types[i]))
			}
		}
		rownames(joint.coor) <- joint_names
	}

	# ADD ROWNAMES TO CONSTRAINT LIST
	if(is.null(names(joint.cons))) names(joint.cons) <- rownames(joint.coor)

	# GET UNIQUE INDICES OF BODIES
	body_num_unique <- sort(unique(c(body.conn)))

	# GET NUMBER OF BODIES
	num_bodies <- length(body_num_unique)

	# IF BODY.CONN IS NUMERIC
	if(is.numeric(body_num_unique[1])){
	
		# CHECK THAT THERE IS A ZERO BODY
		if(!1 %in% body_num_unique) stop("If body.conn is numeric, there must be a body '1' to indicate the fixed.body.")
	
		# CHECK THAT BODY NUMBERS ARE CONSECUTIVE
		if(max(body_num_unique) != num_bodies) stop("If body.conn is numeric, body numbers must be consecutive (a number cannot be skipped).")

		# CHECK THAT fixed.body IS NOT BODY#
		if(grepl('Body[0-9]+', fixed.body)) stop("If body.conn is numeric, fixed.body must have a value other than the form 'Body#'.")
	
		# SET BODY NAMES
		if(num_bodies-1 < 10){
			body.names <- c(fixed.body, paste0("B", formatC(1:(num_bodies-1), width=1, flag="0")))
		}else{
			body.names <- c(fixed.body, paste0("B", formatC(1:(num_bodies-1), width=2, flag="0")))
		}
		
		# SET CORRESPONDENCES BETWEEN BODY NAME AND NUMBER
		body.num <- body_num_unique
		names(body.num)[body.num == 1] <- fixed.body
		names(body.num)[body.num != 1] <- body.names[2:length(body.names)]
		
		# CREATE NUMERIC BODY.CONN
		body_conn_num <- body.conn

	}else{
		
		# CHECK THAT THERE IS A FIXED BODY
		if(!fixed.body %in% body_num_unique) stop(paste0("If body.conn contains the body names, there must be a body with the same name as the specified fixed body (\"", fixed.body, "\")."))

		# SET BODY NAMES
		body.names <- body_num_unique

		# SET CORRESPONDENCES BETWEEN BODY NAME AND NUMBER
		body.num <- setNames(rep(NA, num_bodies), body.names)
		body.num[fixed.body] <- 1
		body.num[names(body.num) != fixed.body] <- 2:num_bodies

		# CREATE NUMERIC BODY.CONN
		body_conn_num <- matrix(NA, nrow(body.conn), ncol(body.conn))
		for(i in 1:nrow(body_conn_num)) body_conn_num[i, ] <- body.num[body.conn[i, ]]
	}
	
	# SORT BODY NAMES BY NUMBER
	body.names <- body.names[order(body.num)]
	
	# SET DIMNAMES FOR BODY CONNECTIONS
	rownames(body.conn) <- rownames(joint.coor)
	rownames(body_conn_num) <- rownames(joint.coor)
	colnames(body.conn) <- colnames(body_conn_num) <- paste0('body', 1:2)

	# CREATE LIST OF JOINTS ASSOCIATED WITH EACH BODY
	body_joints <- as.list(rep(NA, num_bodies))

	for(i in 1:num_bodies){
		body_joints[[i]] <- sort(unique(c(which(body_conn_num[, 1] == i), which(body_conn_num[, 2] == i))))
	}

	if(print.progress){
		cat(paste0(paste0(rep(indent, 1), collapse=''), 'body.joints\n'))
		for(i in 1:length(body_joints)){
			body_joints_names <- c()
			for(j in 1:length(body_joints[[i]])) body_joints_names <- c(body_joints_names, paste0(rownames(joint.coor)[body_joints[[i]]][j], '(', body_joints[[i]][j], ')'))
			cat(paste0(paste0(rep(indent, 2), collapse=''), i, ' (', body.names[i], '): ', paste(body_joints_names, collapse=', '), '\n'))
		}
	}

	# IF PATH FINDING IS ON
	if(find.paths){

		# SET SOLVABLE FRAGMENTS
		solvable_paths <- c('R-S-S', 'R-R-L', 'R-R-R', 'S-R-S', 'S-S-L', 'S-S-S') #, 'S-S-S', 'S-S-S-S'

		# GET LIST OF ALL CLOSED LOOPS
		find_joint_paths <- findJointPaths(body_conn_num, joint.types, solvable_paths)

		# FIND DISTANCES BETWEEN JOINTS IN PATHS
		if(is.null(find_joint_paths$paths.open)){
			paths_open_dist <- NULL
		}else{
			paths_open_dist <- as.list(rep(NA, length(find_joint_paths$paths.open)))
			for(i in 1:length(paths_open_dist)){
				joints_path <- find_joint_paths$paths.open[[i]]
				if(length(joints_path) == 0){
					paths_open_dist[[i]] <- NA
				}else{
					paths_open_dist[[i]] <- distPointToPoint(joint.coor[joints_path, ])
				}
			}
		}

		if(is.null(find_joint_paths$paths.closed)){
			paths_closed_dist <- NULL
		}else{

			paths_closed_dist <- as.list(rep(NA, length(find_joint_paths$paths.closed)))
			paths_closed_bodies <- as.list(rep(NA, length(find_joint_paths$paths.closed)))

			for(i in 1:length(paths_closed_dist)){

				joints_path <- find_joint_paths$paths.closed[[i]]

				if(length(joints_path) == 0){
					paths_closed_dist[[i]] <- NA
				}else{

					# SET BODIES BETWEEN CONSECUTIVE JOINTS IN PATHS
					body_vec <- c()
					for(j in 2:length(joints_path)){
						row_match <- (find_joint_paths$joint.conn[, 'joint1'] == min(joints_path[(j-1):j])) + (find_joint_paths$joint.conn[, 'joint2'] == max(joints_path[(j-1):j]))
						body_vec <- c(body_vec, find_joint_paths$joint.conn[row_match == 2, 'body.idx'])
					}
					paths_closed_bodies[[i]] <- setNames(body_vec, NULL)
			
					# FIND DISTANCES BETWEEN JOINTS IN PATHS
					paths_closed_dist[[i]] <- distPointToPoint(joint.coor[joints_path, ])
				}
			}
		}

		if(print.progress){
			cat(paste0(paste0(rep(indent, 1), collapse=''), 'paths.closed\n'))
			for(i in 1:length(find_joint_paths$paths.closed)){
				joints_names <- c()
				for(j in 1:length(find_joint_paths$paths.closed[[i]])) joints_names <- c(joints_names, paste0(rownames(joint.coor)[find_joint_paths$paths.closed[[i]]][j], '(', find_joint_paths$paths.closed[[i]][j], ')'))
				cat(paste0(paste0(rep(indent, 2), collapse=''), paste(joints_names, collapse='-'), ' '))
				cat(paste0(' (', paste0(joint.types[find_joint_paths$paths.closed[[i]]], collapse='-'), ')\n'))
			}

			cat(paste0(paste0(rep(indent, 1), collapse=''), 'paths.open'))
			if(is.null(find_joint_paths$paths.open)){
				cat(': none\n')
			}else{
				cat('\n')
				for(i in 1:length(find_joint_paths$paths.open)){
					joints_names <- c()
					for(j in 1:length(find_joint_paths$paths.open[[i]])) joints_names <- c(joints_names, paste0(rownames(joint.coor)[find_joint_paths$paths.open[[i]]][j], '(', find_joint_paths$paths.open[[i]][j], ')'))
					cat(paste0(paste0(rep(indent, 2), collapse=''), paste(joints_names, collapse='-'), ' '))
					cat(paste0(' (', paste0(joint.types[find_joint_paths$paths.open[[i]]], collapse='-'), ')\n'))
				}
			}

			cat(paste0(paste0(rep(indent, 1), collapse=''), 'fixed.joints: ', paste0(find_joint_paths$fixed.joints, collapse=','), '\n'))
		}

		# FIND "OPEN DESCENDANTS" FOR EACH JOINT
		paths_open <- find_joint_paths$paths.open
		joints_open <- NULL
		if(!is.null(paths_open)){

			# CREATE EMPTY LIST
			joint_desc_open <- as.list(rep(NA, n_joints))
		
			# GET ALL OPEN JOINTS
			joints_open <- unique(unlist(paths_open))

			# FILL LIST
			for(i in 1:length(joint_desc_open)){
		
				# IF JOINT IS NOT IN OPEN CHAIN, SAVE AS NA
				if(!i %in% joints_open){ joint_desc_open[[i]] <- NA; next }
		
				# FIND ALL OPEN PATHS WITH JOINT
				path_joints <- i
				for(j in 1:length(paths_open)){

					# CHECK IF JOINT IS IN PATH
					if(!i %in% paths_open[[j]]) next
				
					# FIND JOINT POSITION IN PATH
					path_match <- which(paths_open[[j]] == i)

					# SKIP IF AT END OF PATH
					if(path_match == length(paths_open[[j]])) next

					# ADD ALL JOINTS DISTAL FROM FIXED LINK, NOT INCLUDING JOINT
					path_joints <- c(path_joints, paths_open[[j]][(path_match+1):length(paths_open[[j]])])
				}
			
				joint_desc_open[[i]] <- path_joints
			}

			if(length(joint_desc_open) == 0) joint_desc_open <- NULL

		}else{
			joint_desc_open <- NULL
		}

		# FIND JOINT ASSOCIATED WITH THE SAME MOBILE BODY - MAY BE ABLE TO COMBINE WITH JOINT DESC OPEN
		joints_cobody <- as.list(rep(NA, n_joints))

		# REMOVE FIXED BODY ROWS
		joint_conn_m <- find_joint_paths$joint.conn[find_joint_paths$joint.conn[, 1] > 1, ]

		# CREATE LIST OF JOINTS THAT SHARE A BODY WITH EACH JOINT
		for(i in 1:n_joints){

			# FIND ALL CONNECTED JOINTS
			joints_conn_i <- c(i, joint_conn_m[rowSums(i == joint_conn_m[, 2:3]) > 0, 2:3])

			# ADD TO LIST
			joints_cobody[[i]] <- sort(unique(joints_conn_i))
		}

		#print(joints_cobody)
		#cat('--------\n')
		#print(joint_desc_open)

		# COMBINE COBODY JOINTS AND OPEN DESCENDANT JOINTS
		if(FALSE){
			joints_tform <- list()
			if(!is.null(joint_desc_open)){
		
				for(i in 1:n_joints){

					all_joints <- c(i, joint_desc_open[[i]])

					# IF FIXED JOINT, ADD ALL JOINTS IN SAME BODY
					if(i %in% find_joint_paths$fixed.joints) all_joints <- c(all_joints, joints_cobody[[i]])

					all_joints <- all_joints[!is.na(all_joints)]
					if(length(all_joints) == 0){
						all_joints <- NA
					}else{
						all_joints <- sort(unique(all_joints))
					}
					joints_tform[[i]] <- all_joints
				}
			}else{
				joints_tform <- joints_cobody
			}

			if(print.progress){
				cat(paste0(paste0(rep(indent, 1), collapse=''), 'joints.tform\n'))
				for(i in 1:length(joints_tform)){
					if(is.na(joints_tform[[i]][1])){ cat(paste0(paste0(rep(indent, 2), collapse=''), i, ' (', rownames(joint.coor)[i], '): none\n')) ; next }
					joints_tform_names <- c()
					for(j in 1:length(joints_tform[[i]])) joints_tform_names <- c(joints_tform_names, paste0(rownames(joint.coor)[joints_tform[[i]]][j], '(', joints_tform[[i]][j], ')'))
					cat(paste0(paste0(rep(indent, 2), collapse=''), i, ' (', rownames(joint.coor)[i], '): ', paste(joints_tform_names, collapse=', '), '\n'))
				}
			}
		}

		# SET BODIES TRANSFORMED BY EACH INPUT PARAMETER
		body_transform <- NULL
		if(!is.null(joint_desc_open)){
	
			# CREATE LIST
			body_transform <- as.list(rep(NA, n_joints))

			for(i in 1:length(body_transform)){
		
				if(i > length(joint_desc_open)){ body_transform[[i]] <- NA; next}
			
				if(is.na(joint_desc_open[[i]][1])){ body_transform[[i]] <- NA; next}
			
				# GET JOINTS EXCEPT INPUT JOINT
				joint_desc <- joint_desc_open[[i]][joint_desc_open[[i]] != i]
			
				# OPEN JOINT BUT NO DESCENDANT JOINTS (FIND LAST BODY IN OPEN CHAIN)
				if(length(joint_desc) == 0){
			
					# GET BODIES CONNECTED TO LAST JOINT
					body_conn_joint <- body_conn_num[joint_desc_open[[i]], ]

					# FIND WHICH BODY IS ONLY CONNECTED TO LAST JOINT
					body_transform[[i]] <- body_conn_joint[!body_conn_joint %in% c(body_conn_num[-joint_desc_open[[i]], ])]

				}else{

					body_transform[[i]] <- sort(unique(c(body_conn_num[joint_desc, ])))
				}
			}	
		}

		if(print.progress){
			cat(paste0(paste0(rep(indent, 1), collapse=''), 'body.transform'))
			if(is.null(body_transform)){
				cat(': NULL\n')
			}else{
				cat('\n')
				for(i in 1:length(body_transform)){
					if(is.na(body_transform[[i]][1])){ cat(paste0(paste0(rep(indent, 2), collapse=''), i, ' (', rownames(joint.coor)[i], '): none\n')) ; next}
					body_transform_names <- c()
					for(j in 1:length(body_transform[[i]])) body_transform_names <- c(body_transform_names, paste0(body.names[body_transform[[i]]][j], '(', body_transform[[i]][j], ')'))
					cat(paste0(paste0(rep(indent, 2), collapse=''), i, ' (', rownames(joint.coor)[i], '): ', paste(body_transform_names, collapse=', '), '\n'))
				}
			}
		}

	}else{
		
		find_joint_paths <- NULL
		paths_open_dist <- NULL
		paths_closed_dist <- NULL
		paths_closed_bodies <- NULL
		joint_desc_open <- NULL
		joints_open <- NULL
		body_transform <- NULL
	}

	mechanism <- list(
		'joint.coor' = joint.coor,
		'joint.cons' = joint.cons,
		'joint.types' = joint.types,
		'paths.open' = find_joint_paths$paths.open,
		'paths.closed' = find_joint_paths$paths.closed,
		'paths.open.dist' = paths_open_dist,
		'paths.closed.dist' = paths_closed_dist,
		'paths.closed.bodies' = paths_closed_bodies,
		'joint.desc.open'=joint_desc_open,
		'joints.open' = joints_open,
		'joint.names' = rownames(joint.coor),
		'joint.conn' = find_joint_paths$joint.conn,
#		'joints.tform' = joints_tform,
		'body.conn' = body.conn,
		'body.conn.num' = body_conn_num,
		'body.joints'=body_joints,
		'body.names' = body.names,
		'body.transform'=body_transform,
		'fixed.joints' = find_joint_paths$fixed.joints,
		'num.joints' = n_joints,
		'num.bodies' = num_bodies,
		'body.points' = NULL,
		'points.assoc' = NULL,
		'body.assoc' = NULL,
		'points.connect' = NULL
	)

	class(mechanism) <- 'mechanism'

	mechanism
}