defineMechanism <- function(joint.coor, joint.types, joint.cons, body.conn, fixed.body = 'Fixed'){

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

	# MAKE SURE JOINT CONSTRAINTS ARE LIST
	if(!is.list(joint.cons)) joint.cons <- list(joint.cons)

	for(i in 1:length(joint.cons)){

		# IF JOINT CONSTRAINT IS N (NO CONSTRAINT) MAKE SURE IT IS NA
		if(joint.types[i] == 'N'){ joint.cons[[i]] <- NA; break }

		# MAKE SURE JOINT CONSTRAINTS ARE ARRAY
		if(is.vector(joint.cons[[i]])) joint.cons[[i]] <- array(joint.cons[[i]], dim=c(1, length(joint.cons[[i]]), 1))
		if(is.matrix(joint.cons[[i]])) joint.cons[[i]] <- array(joint.cons[[i]], dim=c(dim(joint.cons[[i]]), 1))
	}

	# ADD ROWNAMES TO JOINTS (NAME BASED ON JOINT TYPE AND ORDER IN INPUT SEQUENCE)
	if(is.null(rownames(joint.coor))){
		joint_names <- rep(NA, nrow(joint.coor))
		for(i in 1:nrow(joint.coor)){
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
	body_num_unique <- unique(c(body.conn))

	# GET NUMBER OF BODIES
	num_bodies <- length(body_num_unique)

	# IF BODY.CONN IS NUMERIC
	if(is.numeric(body_num_unique[1])){
	
		# CHECK THAT THERE IS A ZERO BODY
		if(!0 %in% body_num_unique) stop("If body.conn is numeric, there must be a body '0' to indicate the fixed.body.")
	
		# CHECK THAT fixed.body IS NOT BODY#
		if(grepl('Body[0-9]+', fixed.body)) stop("If body.conn is numeric, fixed.body must have a value other than the form 'Body#'.")
	
		# SET BODY NAMES
		body.names <- c(fixed.body, paste0("Body", formatC(1:(num_bodies-1), width=2, flag="0")))
		
		# SET CORRESPONDENCES BETWEEN BODY NAME AND NUMBER
		body.num <- body_num_unique
		names(body.num)[body.num == 0] <- fixed.body
		names(body.num)[body.num != 0] <- body.names[2:length(body.names)]
		
		# CREATE NUMERIC BODY.CONN
		body_conn_num <- body.conn

	}else{
		
		# CHECK THAT THERE IS A FIXED BODY
		if(!fixed.body %in% body_num_unique) stop(paste0("If body.conn contains the body names, there must be a body with the same name as the specified fixed body (\"", fixed.body, "\")."))

		# SET BODY NAMES
		body.names <- body_num_unique

		# SET CORRESPONDENCES BETWEEN BODY NAME AND NUMBER
		body.num <- 0:(num_bodies-1)
		names(body.num)[body.num == 0] <- fixed.body
		names(body.num)[body.num != 0] <- body_num_unique[2:length(body_num_unique)]

		# CREATE NUMERIC BODY.CONN
		body_conn_num <- matrix(NA, nrow(body.conn), ncol(body.conn))
		for(i in 1:nrow(body_conn_num)) body_conn_num[i, ] <- body.num[body.conn[i, ]]
	}

	# GET LIST OF ALL CLOSED LOOPS
	find_joint_paths <- findJointPaths(body_conn_num)

	#print(find_joint_paths)
	
	# FIND "OPEN DESCENDANTS" FOR EACH JOINT
	paths_open <- find_joint_paths$paths.open
	joints_open <- NULL
	if(!is.null(paths_open)){

		# CREATE EMPTY LIST
		joint_desc_open <- as.list(rep(NA, nrow(joint.coor)))
		
		# GET ALL OPEN JOINTS
		joints_open <- unique(unlist(paths_open))
		joints_open <- joints_open[joints_open != 0]

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
	
	# SET BODIES TRANSFORMED BY EACH INPUT PARAMETER
	body_transform <- NULL
	if(!is.null(joint_desc_open)){
	
		# CREATE LIST
		body_transform <- as.list(rep(NA, nrow(joint.coor)))

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

	#print(rownames(joint.coor)[joints_open])

	mechanism <- list(
		'joint.coor' = joint.coor,
		'joint.cons' = joint.cons,
		'joint.types' = joint.types,
		'paths.open' = find_joint_paths$paths.open,
		'paths.closed' = find_joint_paths$paths.closed,
		'joint.desc.open'=joint_desc_open,
		'joints.open' = joints_open,
		'body.conn' = body.conn,
		'body.conn.num' = body_conn_num,
		'joint.init' = joint.coor,
		'body.names' = body.names,
		'body.transform'=body_transform,
		'fixed.joints' = find_joint_paths$fixed.joints,
		'num.bodies' = num_bodies
	)

	class(mechanism) <- 'mechanism'

	mechanism
}
