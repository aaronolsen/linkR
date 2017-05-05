defineMechanism <- function(joint.coor, joint.types, joint.cons, joint.conn, fixed.body = 'Fixed'){

	# MAKE SURE JOINT COORDINATES ARE MATRIX
	if(is.vector(joint.coor)) joint.coor <- matrix(joint.coor, nrow=1)

	# VALIDATE INPUTS
	if(!is.null(joint.conn) && nrow(joint.conn) != length(joint.types)) stop(paste0("The number of rows in 'joint.conn' (", nrow(joint.conn), ") must be equal to the number of joints specified in 'joint.types' (", length(joint.types), ")."))
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
	body_num_unique <- unique(c(joint.conn))

	# GET NUMBER OF BODIES
	num_bodies <- length(body_num_unique)

	# IF JOINT.CONN IS NUMERIC
	if(is.numeric(body_num_unique[1])){
	
		# CHECK THAT THERE IS A ZERO BODY
		if(!0 %in% body_num_unique) stop("If joint.conn is numeric, there must be a body '0' to indicate the fixed.body.")
	
		# CHECK THAT fixed.body IS NOT BODY#
		if(grepl('Body[0-9]+', fixed.body)) stop("If joint.conn is numeric, fixed.body must have a value other than the form 'Body#'.")
	
		# SET BODY NAMES
		body.names <- c(fixed.body, paste0("Body", formatC(1:(num_bodies-1), width=2, flag="0")))
		
		# SET CORRESPONDENCES BETWEEN BODY NAME AND NUMBER
		body.num <- body_num_unique
		names(body.num)[body.num == 0] <- fixed.body
		names(body.num)[body.num != 0] <- body.names[2:length(body.names)]

	}else{
		
		# CHECK THAT THERE IS A FIXED BODY
		if(!fixed.body %in% body_num_unique) stop(paste0("If joint.conn contains the body names, there must be a body with the same name as the specified fixed body (\"", fixed.body, "\")."))

		# SET BODY NAMES
		body.names <- body_num_unique

		# SET CORRESPONDENCES BETWEEN BODY NAME AND NUMBER
		body.num <- 0:(num_bodies-1)
		names(body.num)[body.num == 0] <- fixed.body
		names(body.num)[body.num != 0] <- body_num_unique[2:length(body_num_unique)]
	}

	# FIND JOINTS CONNECTED TO GROUND

	# GET LIST OF ALL CLOSED LOOPS

	# CREATE CONNECTED JOINT SEQUENCES
	if(nrow(joint.coor) > 1){

		#joint_paths <- connJointSeq(joint.bodies, joint.types, joint.conn, ground_joints)
		joint_paths <- NULL

	}else{

		joint_paths <- NULL
	}

	linkage <- list(
		'joint.coor' = joint.coor,
		'joint.cons' = joint.cons,
		'joint.types' = joint.types,
		'joint.paths' = joint_paths,
		'joint.conn' = joint.conn,
		'joint.init' = joint.coor,
		'body.names' = body.names,
		'num.bodies' = num_bodies
	)

	class(linkage) <- 'linkage'

	linkage
}
