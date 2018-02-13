defineMechanism <- function(joint.coor, joint.types, joint.cons, body.conn, input.body, 
	input.joint = NULL, fixed.body = 'Fixed', find.paths = TRUE, print.progress = FALSE){

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
	
	# Set joint names
	joint_names <- rownames(joint.coor)

	# ADD ROWNAMES TO CONSTRAINT LIST
	if(is.null(names(joint.cons))) names(joint.cons) <- joint_names

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
	rownames(body.conn) <- joint_names
	rownames(body_conn_num) <- joint_names
	colnames(body.conn) <- colnames(body_conn_num) <- paste0('body', 1:2)

	# CREATE LIST OF JOINTS ASSOCIATED WITH EACH BODY
	body_joints <- as.list(rep(NA, num_bodies))
	for(i in 1:num_bodies) body_joints[[i]] <- sort(unique(c(which(body_conn_num[, 1] == i), which(body_conn_num[, 2] == i))))
	names(body_joints) <- body.names

	if(print.progress){
		cat(paste0(paste0(rep(indent, 1), collapse=''), 'body.joints\n'))
		for(i in 1:length(body_joints)){
			body_joints_names <- c()
			for(j in 1:length(body_joints[[i]])) body_joints_names <- c(body_joints_names, paste0(joint_names[body_joints[[i]]][j], '(', body_joints[[i]][j], ')'))
			cat(paste0(paste0(rep(indent, 2), collapse=''), i, ' (', body.names[i], '): ', paste(body_joints_names, collapse=', '), '\n'))
		}
	}

	# Set input.joint if NULL and a single joint
	if(is.null(input.joint)){
		if(n_joints > 1) stop("If the mechanism has more than one joint 'input.joint' must be specified.")
		input.joint <- 1
	}

	# If input.joint is non-numeric, convert to numeric
	if(!is.numeric(input.joint[1])){
		if(sum(!input.joint %in% joint_names) > 0) stop("'input.joint' names do not match joint names.")
		input_joint_num <- rep(NA, length(input.joint))
		for(i in 1:length(input.joint)) input_joint_num[i] <- which(joint_names == input.joint[i])
		input.joint <- input_joint_num
	}

	# Get joints connected to each joint
	joint_conn <- list()
	for(i in 1:nrow(body_conn_num)){

		# Find bodies connected by joint
		body1 <- body_conn_num[i, 1]
		body2 <- body_conn_num[i, 2]
		
		# Get all joints connected to bodies
		joint_conn_all <- sort(unique(c(which(rowSums(body_conn_num == body1) > 0), which(rowSums(body_conn_num == body2) > 0))))
		
		# Save joints except current joint
		joint_conn[[i]] <- joint_conn_all[joint_conn_all != i]
	}
	
	# Find fixed joints
	fixed_joints <- which(rowSums(body_conn_num == 1) > 0)
	
	# Find open joints (joints that are connected to fixed link through a single joint)
	open_joints <- findOpenJoints(joint_conn, body_conn_num, fixed_joints, 
		body.names=body.names, indent=indent, indent.level=1, print.progress=FALSE)

	# Print open joints
	if(print.progress){
		cat(paste0(paste0(rep(indent, 1), collapse=''), 'open.joints: '))
		joints_names <- c()
		for(j in 1:length(open_joints)) joints_names <- c(joints_names, paste0(joint_names[open_joints[j]], '(', open_joints[j], ')'))
		cat(paste0(paste0(joints_names, collapse=', '), '\n'))
	}
	
	# Find closed paths
	solve_paths <- findSolvablePaths(joint_conn, body_conn_num, input.joint, fixed_joints, open_joints, 
		joint.types=joint.types, body.names=body.names, indent=indent, indent.level=1, print.progress=FALSE)

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

	if(is.null(solve_paths[['paths.joints']])){
		paths_closed_dist <- NULL
		paths_closed_set <- NULL
	}else{

		paths_closed_dist <- as.list(rep(NA, length(solve_paths[['paths.joints']])))
		paths_closed_set <- as.list(rep(NA, length(solve_paths[['paths.joints']])))

		for(i in 1:length(paths_closed_dist)){

			joints_path <- solve_paths[['paths.joints']][[i]]

			if(length(joints_path) == 0){
				paths_closed_dist[[i]] <- NA
				paths_closed_set[[i]] <- NA
			}else{

				# Create object
				paths_closed_set[[i]] <- matrix(NA, length(solve_paths[['paths.joints']][[i]]), ncol=2)

				# Find joint set order for path
				for(j in 1:length(joints_path)){
					
					if(j == 1){
						if(body_conn_num[joints_path[j], 1] == solve_paths[['paths.bodies']][[i]][j]){
							paths_closed_set[[i]][j,] <- c(2,1)
						}else{
							paths_closed_set[[i]][j,] <- c(1,2)
						}
					}else if(j < length(joints_path)){

						# Joint does not connect directly to body (happens when skipping over input joints)
						if(!solve_paths[['paths.bodies']][[i]][j-1] %in% body_conn_num[joints_path[j], ]){
							paths_closed_set[[i]][j,2] <- which(body_conn_num[joints_path[j], ] == solve_paths[['paths.bodies']][[i]][j])
							if(paths_closed_set[[i]][j,2] == 2){
								paths_closed_set[[i]][j,1] <- 1
							}else{
								paths_closed_set[[i]][j,1] <- 2
							}
						}else{
							paths_closed_set[[i]][j,1] <- which(body_conn_num[joints_path[j], ] == solve_paths[['paths.bodies']][[i]][j-1])
						}

						if(!solve_paths[['paths.bodies']][[i]][j] %in% body_conn_num[joints_path[j], ]){
							paths_closed_set[[i]][j,1] <- which(body_conn_num[joints_path[j], ] == solve_paths[['paths.bodies']][[i]][j-1])
							if(paths_closed_set[[i]][j,1] == 2){
								paths_closed_set[[i]][j,2] <- 1
							}else{
								paths_closed_set[[i]][j,2] <- 2
							}
						}else{
							paths_closed_set[[i]][j,2] <- which(body_conn_num[joints_path[j], ] == solve_paths[['paths.bodies']][[i]][j])
						}

					}else{
						if(body_conn_num[joints_path[j], 1] == solve_paths[['paths.bodies']][[i]][j-1]){
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

	mechanism <- list(
		'joint.coor' = joint.coor,
		'joint.cons' = joint.cons,
		'joint.types' = joint.types,
		'input.joint' = input.joint,
		'input.body' = input.body,
		'paths.closed' = solve_paths[['paths.joints']],
		'paths.closed.dist' = paths_closed_dist,
		'paths.closed.bodies' = solve_paths[['paths.bodies']],
		'paths.closed.set'=paths_closed_set,
		'joint.transform' = joint_transform,
		'joint.set.transform' = joint_set_transform,
		'joint.names' = joint_names,
		'joint.conn' = joint_conn,
		'body.conn' = body.conn,
		'body.conn.num' = body_conn_num,
		'body.joints'=body_joints,
		'body.names' = body.names,
		'fixed.joints' = fixed_joints,
		'open.joints' = open_joints,
		'num.paths.closed' = length(solve_paths[['paths.joints']]),
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
	rc <- c(rc, paste0('\tFixed joints (fixed.joints): '))
	joints_names <- c()
	for(i in 1:length(x[['fixed.joints']])) joints_names <- c(joints_names, paste0(x$joint.names[x[['fixed.joints']][i]], '(', x[['fixed.joints']][i], ')'))
	rc <- c(rc, paste0(paste0(joints_names, collapse=', '), '\n'))

	## Open joints
	rc <- c(rc, paste0('\tOpen joints (open.joints): '))
	if(!is.null(x[['open.joints']])){
		joints_names <- c()
		for(i in 1:length(x[['open.joints']])) joints_names <- c(joints_names, paste0(x$joint.names[x[['open.joints']][i]], '(', x[['open.joints']][i], ')'))
		rc <- c(rc, paste0(paste0(joints_names, collapse=', '), '\n'))
	}else{
		rc <- c(rc, 'none\n')
	}

	## Solvable paths
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
		rc <- c(rc, paste0('\tSolvable joint paths (paths.closed)\n\t\t', paste0(capture.output(print(path_df)), collapse='\n\t\t'), '\n'))
	}else{
		rc <- c(rc, paste0('\tSolvable joint paths (paths.closed): none\n'))
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