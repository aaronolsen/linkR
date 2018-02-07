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
		if(is.array(joint.cons[[i]]) && length(dim(joint.cons[[i]])) > 2){
			#if(dim(joint.cons[[i]][, , j])[3] == 1) joint.cons[[i]] <- array(joint.cons[[i]][, , j], dim=c(dim(joint.cons[[i]])[1:2], 2))
			for(j in 1:dim(joint.cons[[i]])[3]) joint.cons[[i]][, , j] <- uvector(joint.cons[[i]][, , j])
		}

		# MAKE SURE JOINT CONSTRAINTS ARE ARRAY AND MAKE SURE VECTORS ARE UNIT LENGTH
		#if(is.vector(joint.cons[[i]])) joint.cons[[i]] <- array(uvector(joint.cons[[i]]), dim=c(1, length(joint.cons[[i]]), 2))
		#if(is.matrix(joint.cons[[i]])) joint.cons[[i]] <- array(uvector(joint.cons[[i]]), dim=c(dim(joint.cons[[i]]), 2))
		if(is.vector(joint.cons[[i]])) joint.cons[[i]] <- matrix(uvector(joint.cons[[i]]), nrow=1, ncol=3)
		if(is.matrix(joint.cons[[i]])) joint.cons[[i]] <- matrix(uvector(joint.cons[[i]]), nrow=dim(joint.cons[[i]]), ncol=3)
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
	for(i in 1:num_bodies) body_joints[[i]] <- sort(unique(c(which(body_conn_num[, 1] == i), which(body_conn_num[, 2] == i))))
	names(body_joints) <- body.names

	if(print.progress){
		cat(paste0(paste0(rep(indent, 1), collapse=''), 'body.joints\n'))
		for(i in 1:length(body_joints)){
			body_joints_names <- c()
			for(j in 1:length(body_joints[[i]])) body_joints_names <- c(body_joints_names, paste0(rownames(joint.coor)[body_joints[[i]]][j], '(', body_joints[[i]][j], ')'))
			cat(paste0(paste0(rep(indent, 2), collapse=''), i, ' (', body.names[i], '): ', paste(body_joints_names, collapse=', '), '\n'))
		}
	}

	## Find "sole joints" (joints that are the only joint connected to a particular body)
	sole_joints <- c()

	# Get bodies that are in body connection matrix just once
	not_in_joint_conn <- which(table(body_conn_num) == 1)

	# Remove fixed body
	not_in_joint_conn <- not_in_joint_conn[!not_in_joint_conn == 1]

	# Get joints connected to body
	if(length(not_in_joint_conn) > 0){
		for(i in 1:nrow(body_conn_num)){
			if(body_conn_num[i,1] %in% not_in_joint_conn || body_conn_num[i,2] %in% not_in_joint_conn) sole_joints <- c(sole_joints, i)
		}
	}

	if(print.progress){
		cat(paste0(paste0(rep(indent, 1), collapse=''), 'Sole joints (joints that are the only joint connected to a particular body):'))
		if(length(sole_joints) == 0){
			cat(' none\n')
		}else{
			sole_joints_names <- c()
			for(j in 1:length(sole_joints)) sole_joints_names <- c(sole_joints_names, paste0(rownames(joint.coor)[sole_joints[j]], '(', sole_joints[j], ')'))
			cat(paste0(' ', paste0(sole_joints_names, collapse=', '), '\n'))
		}
	}


	# Set joints transformed by each body transformation
	joint_transform <- setNames(as.list(rep(NA, num_bodies)), body.names)
	joint_set_transform <- setNames(as.list(rep(NA, num_bodies)), body.names)

	# Fill list
	for(body_num in 1:num_bodies){
		bc_rows <- which(rowSums(body_conn_num == body_num) > 0)
		jt_t_v <- c()
		jt_s_t_v <- c()
		for(i in 1:length(bc_rows)){
			set_match <- which(body_conn_num[bc_rows[i], ] == body_num)
			jt_t_v <- c(jt_t_v, bc_rows[i])
			jt_s_t_v <- c(jt_s_t_v, set_match)
		}
		names(jt_t_v) <- NULL
		names(jt_s_t_v) <- NULL
		joint_transform[[body_num]] <- jt_t_v
		joint_set_transform[[body_num]] <- jt_s_t_v
	}

	# IF PATH FINDING IS ON
	if(find.paths){

		# SET SOLVABLE FRAGMENTS
		solvable_paths <- c('R-S-S', 'R-R-L', 'R-R-R', 'S-R-S', 'S-S-L', 'S-S-S') #, 'S-S-S', 'S-S-S-S'

		# GET LIST OF ALL CLOSED LOOPS
		find_joint_paths <- findJointPaths(body_conn_num, joint.types, solvable_paths, sole_joints)

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
			paths_closed_bodies <- NULL
			paths_closed_set <- NULL
		}else{

			paths_closed_dist <- as.list(rep(NA, length(find_joint_paths$paths.closed)))
			paths_closed_bodies <- as.list(rep(NA, length(find_joint_paths$paths.closed)))
			paths_closed_set <- as.list(rep(NA, length(find_joint_paths$paths.closed)))

			for(i in 1:length(paths_closed_dist)){

				joints_path <- find_joint_paths$paths.closed[[i]]

				if(length(joints_path) == 0){
					paths_closed_dist[[i]] <- NA
					paths_closed_set[[i]] <- NA
				}else{

					# SET BODIES BETWEEN CONSECUTIVE JOINTS IN PATHS
					body_vec <- c()
					for(j in 2:length(joints_path)){
						row_match <- (find_joint_paths$joint.conn[, 'joint1'] == min(joints_path[(j-1):j])) + (find_joint_paths$joint.conn[, 'joint2'] == max(joints_path[(j-1):j]))
						body_vec <- c(body_vec, find_joint_paths$joint.conn[row_match == 2, 'body.idx'])
					}
					paths_closed_bodies[[i]] <- setNames(body_vec, NULL)

					# Create object
					paths_closed_set[[i]] <- matrix(NA, length(find_joint_paths$paths.closed[[i]]), ncol=2)

					# Find joint set order for path
					for(j in 1:length(joints_path)){
						
						if(j == 1){
							if(body_conn_num[joints_path[j], 1] == paths_closed_bodies[[i]][j]){
								paths_closed_set[[i]][j,] <- c(2,1)
							}else{
								paths_closed_set[[i]][j,] <- c(1,2)
							}
						}else if(j < length(joints_path)){
							paths_closed_set[[i]][j,1] <- which(body_conn_num[joints_path[j], ] == paths_closed_bodies[[i]][j-1])
							paths_closed_set[[i]][j,2] <- which(body_conn_num[joints_path[j], ] == paths_closed_bodies[[i]][j])
						}else{
							if(body_conn_num[joints_path[j], 1] == paths_closed_bodies[[i]][j-1]){
								paths_closed_set[[i]][j,] <- c(1,2)
							}else{
								paths_closed_set[[i]][j,] <- c(2,1)
							}
						}
					}
			
					# FIND DISTANCES BETWEEN JOINTS IN PATHS
					paths_closed_dist[[i]] <- distPointToPoint(joint.coor[joints_path, ])
				}
			}
		}

		if(print.progress){
			cat(paste0(paste0(rep(indent, 1), collapse=''), 'paths.closed'))
			if(!is.null(find_joint_paths$paths.closed)){

				cat('\n', sep='')

				for(i in 1:length(find_joint_paths$paths.closed)){
					joints_names <- c()
					for(j in 1:length(find_joint_paths$paths.closed[[i]])) joints_names <- c(joints_names, paste0(rownames(joint.coor)[find_joint_paths$paths.closed[[i]]][j], '(', find_joint_paths$paths.closed[[i]][j], ')'))
					cat(paste0(paste0(rep(indent, 2), collapse=''), paste(joints_names, collapse='-'), ' '))
					cat(paste0(' (', paste0(joint.types[find_joint_paths$paths.closed[[i]]], collapse='-'), ')\n'))
				}
			}else{
				cat(' : none\n', sep='')
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

		# Find descendant bodies for each body
		body_open_desc <- NULL
		paths_open <- find_joint_paths$paths.open
		joints_open <- NULL
		if(!is.null(paths_open)){
			
			# CREATE EMPTY LIST
			body_open_desc <- as.list(rep(NA, num_bodies))

			# GET ALL OPEN JOINTS
			joints_open <- unique(unlist(paths_open))

			# Get bodies in closed chains
			bodies_in_closed <- unique(unlist(paths_closed_bodies))

			# 
			for(body_num in 1:num_bodies){
			
				# Get joints associated with body
				joints_assoc <- body_joints[[body_num]]
				
				#cat(paste0('Joints associated with body ', body.names[body_num], ' (', body_num, '): ', paste0(joints_assoc, collapse=','), '\n'))
				
				path_joints <- c()
				for(joint_assoc in joints_assoc){

					# IF JOINT IS NOT IN OPEN CHAIN, SAVE AS NA
					if(!joint_assoc %in% joints_open) next

					# FIND ALL OPEN PATHS WITH JOINT
					for(j in 1:length(paths_open)){

						# CHECK IF JOINT IS IN PATH
						if(!joint_assoc %in% paths_open[[j]]) next
				
						# FIND JOINT POSITION IN PATH
						path_match <- which(paths_open[[j]] == joint_assoc)

						# SKIP IF AT END OF PATH
						if(path_match == length(paths_open[[j]])) next

						# ADD ALL JOINTS DISTAL FROM FIXED LINK, NOT INCLUDING JOINT
						path_joints <- c(path_joints, paths_open[[j]][(path_match+1):length(paths_open[[j]])])
					}
					
					#cat(paste0('\tAll open descendant joints of joint ', joint_assoc, ': ', paste0(path_joints, collapse=','), '\n'))
				}

				# Find bodies associated with joints
				bodies_assoc <- NA
				if(length(path_joints) > 0){
				
					# Associated bodies
					bodies_assoc <- sort(unique(c(body_conn_num[path_joints, ])))

					# Remove current body
					bodies_assoc <- bodies_assoc[bodies_assoc != body_num]
					
					#
					if(is.na(bodies_assoc[1])) bodies_assoc <- NA
				
					#cat(paste0('\tAll associated descendant bodies: ', paste0(body.names[bodies_assoc], collapse=','), '\n'))
				}
				
				if(!is.na(bodies_assoc[1])){

					# Check if each body is in a closed chain
					in_bodies_in_closed <- apply(matrix(bodies_assoc, nrow=1, ncol=length(bodies_assoc)), 2, '%in%', bodies_in_closed)

					# Set to NA if any of the bodies are also in a closed chain
					if(any(in_bodies_in_closed) && body_num != 1) bodies_assoc <- NA
				}

				body_open_desc[[body_num]] <- bodies_assoc
			}

			# Set names
			names(body_open_desc) <- body.names
		}

		joint_desc_open <- NULL
		body_transform <- NULL
		if(FALSE){

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
		
			#print(joints_cobody)
			#cat('--------\n')
			#print(joint_desc_open)

			## COMBINE COBODY JOINTS AND OPEN DESCENDANT JOINTS
			# REMOVE FIXED BODY ROWS
			joint_conn_m <- find_joint_paths$joint.conn[find_joint_paths$joint.conn[, 1] > 1, ]

			# FIND JOINT ASSOCIATED WITH THE SAME MOBILE BODY - MAY BE ABLE TO COMBINE WITH JOINT DESC OPEN
			joints_cobody <- as.list(rep(NA, n_joints))

			# CREATE LIST OF JOINTS THAT SHARE A BODY WITH EACH JOINT
			for(i in 1:n_joints){

				# FIND ALL CONNECTED JOINTS
				joints_conn_i <- c(i, joint_conn_m[rowSums(i == joint_conn_m[, 2:3]) > 0, 2:3])

				# ADD TO LIST
				joints_cobody[[i]] <- sort(unique(joints_conn_i))
			}

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
						body_conn_joint_only <- body_conn_joint[!body_conn_joint %in% c(body_conn_num[-joint_desc_open[[i]], ])]
					
						# REMOVE FIXED BODY
						body_transform[[i]] <- body_conn_joint_only[body_conn_joint_only != 1]

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
		'paths.closed.set'=paths_closed_set,
		'joint.desc.open'=joint_desc_open,
		'joints.open' = joints_open,
		'joint.transform' = joint_transform,
		'joint.set.transform' = joint_set_transform,
		'joint.names' = rownames(joint.coor),
		'joint.conn' = find_joint_paths$joint.conn,
#		'joints.tform' = joints_tform,
		'body.conn' = body.conn,
		'body.conn.num' = body_conn_num,
		'body.joints'=body_joints,
		'body.names' = body.names,
		'body.transform'=body_transform,
		'body.open.desc'=body_open_desc,
		'fixed.joints' = find_joint_paths$fixed.joints,
		'num.paths.closed' = length(find_joint_paths$paths.closed),
		'num.paths.open' = length(find_joint_paths$paths.open),
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

print.mechanism <- function(x, ...){

	#print(names(x))
	
	#return(1)
	#print(x$body.joints)

	# Start return string
	rc <- ''

	rc <- c(rc, paste0('Mechanism\n'))
	rc <- c(rc, paste0('\tNumber of joints (num.joints): ', x$num.joints, '\n'))
	rc <- c(rc, paste0('\tNumber of bodies (num.bodies): ', x$num.bodies, '\n'))

	## Joints
	# Create dataframe for joints
	joint_df_colnames <- c('Name', 'Type')
	joint_df <- data.frame('Name'=x$joint.names, 'Type'=paste0('     ', x$joint.types), 
		'Body.1'=paste0('  ', x$body.conn[,1], ' (', x$body.conn.num[,1], ')'),
		'Body.2'=paste0('  ', x$body.conn[,2], ' (', x$body.conn.num[,2], ')'),
		'Position'=paste0('  {', apply(signif(x$joint.coor, 3), 1, 'paste0', collapse=','), '}'))
	#print(joint_df)
	rc <- c(rc, paste0('\tMechanism joints:\n\t\t', paste0(capture.output(print(joint_df)), collapse='\n\t\t'), '\n'))

	## Fixed joints
	rc <- c(rc, paste0('\tFixed joints (fixed.joints):\n'))
	for(i in 1:length(x[['fixed.joints']])){
		rc <- c(rc, paste0('\t\t ', x$joint.names[x[['fixed.joints']][i]], ' (', x[['fixed.joints']][i], ')\n'))
	}

	## Closed paths
	if(!is.null(x$paths.closed)){
		# Set path closed joint name strings
		pc_names <- rep(NA, length(x$paths.closed))
		for(path_num in 1:length(x$paths.closed)) pc_names[path_num] <- paste0(x$joint.names[x$paths.closed[[path_num]]], collapse=',')

		# Set path closed joint type strings
		pc_types <- rep(NA, length(x$paths.closed))
		for(path_num in 1:length(x$paths.closed)) pc_types[path_num] <- paste0(x$joint.types[x$paths.closed[[path_num]]], collapse='-')

		# Set path closed body names
		pc_body_names <- rep(NA, length(x$paths.closed))
		for(path_num in 1:length(x$paths.closed)) pc_body_names[path_num] <- paste0(x$body.names[x$paths.closed.bodies[[path_num]]], collapse=',')

		# Set path closed body names
		pc_body_num <- rep(NA, length(x$paths.closed))
		for(path_num in 1:length(x$paths.closed)) pc_body_num[path_num] <- paste0(x$paths.closed.bodies[[path_num]], collapse=',')

		# Set closed path joint sets (order in which joint sets proceed through path)
		pc_path_set <- rep(NA, length(x$paths.closed))
		for(path_num in 1:length(x$paths.closed)) pc_path_set[path_num] <- paste0(paste0(x$paths.closed.set[[path_num]][,1], x$paths.closed.set[[path_num]][,2]), collapse='-')

		path_df <- data.frame('Indices'=paste0('    ', unlist(lapply(x$paths.closed, 'paste0', collapse=','))),
			'Names'=paste0('   ', pc_names), 'Types'=paste0('   ', pc_types),
			'Body.names'=paste0('       ', pc_body_names), 'Body.indices'=paste0('           ', pc_body_num),
			'Path.set'=paste0('   ', pc_path_set)
			)
		rc <- c(rc, paste0('\tClosed joint paths (paths.closed)\n\t\t', paste0(capture.output(print(path_df)), collapse='\n\t\t'), '\n'))
	}else{
		rc <- c(rc, paste0('\tClosed joint paths (paths.closed): none\n'))
	}
	
	## Open paths
	if(!is.null(x$paths.open)){
		#rc <- c(rc, paste0('\tOpen joint paths!!!\n'))
	}
	
	if(!is.null(x$body.open.desc)){

		rc <- c(rc, paste0('\tDescendant bodies in open chains\n'))

		for(i in 1:length(x$body.open.desc)){

			if(is.na(x$body.open.desc[[i]][1])) next
				# cat(paste0(paste0(rep('\t', 2), collapse=''), i, ' (', rownames(x$joint.coor)[i], '): none\n')) ; next}
			
			body_transform_names <- c()
			for(j in 1:length(x$body.open.desc[[i]])){
				body_transform_names <- c(body_transform_names, paste0(x$body.names[x$body.open.desc[[i]]][j], '(', x$body.open.desc[[i]][j], ')'))
			}

			rc <- c(rc, paste0(paste0(rep('\t', 2), collapse=''), i, ' (', x$body.names[i], '): ', paste(body_transform_names, collapse=', '), '\n'))
		}
	}else{
		rc <- c(rc, paste0('\tOpen chains: none\n'))
	}

	rc <- c(rc, paste0('\tNumber of points associated with bodies: '))
	if(!is.null(x[['body.points']])){

		rc <- c(rc, paste0(nrow(x[['body.points']]), '\n'))

		# For each body
		for(body_num in 1:length(x[['points.assoc']])){

			if(is.na(x[['points.assoc']][[body_num]][1])) next
			
			# Apply transformation
			rc <- c(rc, paste0('\t\t', x[['body.names']][body_num], ' (', body_num, '): ', length(x[['points.assoc']][[body_num]]), '\n'))
		}

	}else{
		rc <- c(rc, paste0('0\n'))
	}

	cat(rc, sep='')
}