reparameterizeLinkage <- function(joint.coor, joint.types, fixed.link, input.joint,
	joint.conn = NULL, zero = 1e-3, jo.at=NULL){

	if(length(dim(joint.coor)) == 4){
		linkage <- list()
		for(ii in 1:dim(joint.coor)[4]){
			linkage[[ii]] <- reparameterizeLinkage(joint.coor=joint.coor[, , , ii], 
				joint.types=joint.types, joint.conn=joint.conn, fixed.link=fixed.link, 
				input.joint=input.joint, zero = zero, jo.at=jo.at)
		}
		return(linkage)
	}

	# Set which links are connected by each joint
	if(is.null(joint.conn)) joint.conn <- matrix(unlist(strsplit(dimnames(joint.coor)[[1]], '[.]')), ncol=2, byrow=TRUE)

	# This function takes animated linkage coordinates and reparameterizes the linkage into
	# a minimum parameter set consisting of joint positions relative to a fixed joint 
	# coordinate, linkage orientation, and link lengths

	# Check that input is 4-bar linkage
	if(nrow(joint.coor) > 4) stop("parameterizeLinkage() is currently only compatible only with 2D and 3D four-bar linkages.")
	if(sum('R' == joint.types) != 2 || sum('S' == joint.types) != 2) stop("parameterizeLinkage() is currently only compatible only with 2D and 3D four-bar linkages (2 S-joints, 2 R-joints).")

	# 4-bar minimum parameters:								2D	3D
	#	FJ	Ri-joint position								3	3	constant	(linkage position)
	#	AJ	Joint adj to fixed joint (not via input link)	3	3	variable 	(linkage orientation)
	# 	LL	Link lengths									4	4	variable
	#	RA	Ri-joint AOR (unit vec)							NA	3	variable
	#	RA	Ro-joint AOR (unit vec)							NA	3	variable
	#	RI	Ri-joint initial vector							3	3	constant	(resolve toggle at start)
	#	JO	Ro-joint toggle initial							3	3	constant	(resolve toggle at start)
	#	Total											
	#	Total (-position,orientation)					6	8
	#
	# Input parameter
	#	RD	Ri-joint angle									1	1	variable
	#
	# Solve for...
	#	So-joint position
	#
	# Note, unit vector cannot be of length 2 with third determined by unit length because 
	#	the third value could be either positive or negative and still give unit length. So
	#	the third value is undetermined.
	# Inputs should be separate lists because then it will be possible in the future to 
	#	to allow for functions in addition to vectors/matrices of values
		
	# Set rownames of joint.conn
	rownames(joint.conn) <- dimnames(joint.coor)[[1]]
	
	# Set names for joint types
	if(is.null(names(joint.types))) names(joint.types) <- dimnames(joint.coor)[[1]]
	
	# Find adjacent joints
	adj_joints <- list()
	for(i in 1:nrow(joint.conn)){

		# Find joints connected to same link
		adj <- unique(c(rownames(joint.conn)[rowSums(joint.conn == joint.conn[i, 1]) == 1], 
			rownames(joint.conn)[rowSums(joint.conn == joint.conn[i, 2]) == 1]))

		# Remove current joint and add to list
		adj_joints[[rownames(joint.conn)[i]]] <- adj[adj != rownames(joint.conn)[i]]
	}
	
	# Identify joint whose position will be solved for (S-joint not directly connected to input R-joint)
	joint_solve <- rownames(joint.conn)[!rownames(joint.conn) %in% c(input.joint, adj_joints[[input.joint]])]
	
	# Find joints associated with fixed link
	fixed_joints <- dimnames(joint.coor)[[1]][which(rowSums(joint.conn == fixed.link) > 0)]

	# Find joint attached to fixed link, if includes input joint, take that one
	fixed.joint <- names(joint.types[fixed_joints])[which(joint.types[fixed_joints] == 'R')]
	if(input.joint %in% fixed.joint) fixed.joint <- input.joint
	
	# Vector(s) from origin to fixed joint position vector
	FJ <- list()
	if(mean(apply(joint.coor[input.joint, , ], 1, 'sd')) > zero){
		FJ[[fixed.joint]] <- t(joint.coor[fixed.joint, , ])
	}else{
		FJ[[fixed.joint]] <- matrix(joint.coor[fixed.joint, , 1], nrow=1, ncol=3)
	}

	# Vector(s) fixed joint position vector to adjacent joints
	AJ <- list()
	for(joint_name in adj_joints[[fixed.joint]]){
	
		# Skip if joint is connected via input link (not fixed)
		if(joint.conn[joint_name, joint.conn[joint_name, ] %in% joint.conn[fixed.joint, ]] != fixed.link) next
		
		# Find vector from fixed joint position to adjacent joints
		vec <- uvector(t(joint.coor[joint_name, , ] - joint.coor[fixed.joint, , ]))

		# Check if vector changes with time
		if(mean(apply(vec, 2, 'sd')) > zero){
			AJ[[joint_name]] <- vec
		}else{
			AJ[[joint_name]] <- matrix(vec[1, ], nrow=1, ncol=3)
		}
	}
	
	# Get link names
	link_names <- unique(c(joint.conn))
	
	# Measure link lengths
	LL <- list()
	for(link_name in link_names){
		
		# Joints defining each link
		joints <- rownames(joint.conn)[rowSums(joint.conn == link_name) == 1]
		
		# Distance between joints
		len <- distPointToPoint(t(joint.coor[joints[1], , ]), t(joint.coor[joints[2], , ]))

		if(sd(len) > zero){
			LL[[paste(sort(joints), collapse='-')]] <- len
		}else{
			LL[[paste(sort(joints), collapse='-')]] <- len[1]
		}
	}
	
	# Find R joints
	R_joints <- names(joint.types)[which(joint.types == 'R')]
	
	# Make sure input joint is first
	if(R_joints[1] != input.joint) R_joints <- R_joints[2:1]

	# Set axes of rotation
	RA <- list()
	RD <- list()
	RI <- list()
	for(R_joint in R_joints){
	
		RA[[R_joint]] <- matrix(NA, nrow=dim(joint.coor)[3], ncol=3)
		if(R_joint %in% input.joint) RD[[R_joint]] <- rep(NA, dim(joint.coor)[3])

		# Find adjacent joint to use in determining the axis of rotation
		adj <- adj_joints[[R_joint]]

		# Exclude joint associated with fixed link
		adj <- adj[!adj %in% fixed_joints]

		# Exclude input joint associated with fixed link
		adj <- adj[!adj %in% input.joint]

		# Set initial vector for finding axis of rotation
		vi <- matrix(joint.coor[adj, , 1] - joint.coor[R_joint, , 1], ncol=3)
		
		if(R_joint %in% input.joint) RI[[adj]] <- matrix(vi, nrow=1, ncol=3)

		for(j in 1:dim(joint.coor)[3]){

			# Get vector at each time point
			vj <- joint.coor[adj, , j] - joint.coor[R_joint, , j]
			
			# Skip if NA
			if(is.na(vj[1])) next

			# Find instantaneous axes of rotation
			RA[[R_joint]][j, ] <- uvector(cprod(vi, vj))
			
			# Find angular displacement for input joint(s)
			if(R_joint %in% input.joint) RD[[R_joint]][j] <- avec(vi, vj)
		}
		
		# Make sure vector position can be recovered from rotation matrix
		#j <- 2
		#print(FJ[[1]][1, ] + uvector(vi %*% tMatrixEP(v=RA[[R_joint]][j, ], a=RD[[1]][j]))*LL[['ljaw.susp-susp.oper']][j])
		#print(joint.coor['ljaw.susp', , j])
		#print(RA[[R_joint]][j, ])

		# If axes of rotation constant, set to single row matrix
		if(mean(apply(RA[[R_joint]], 2, 'sd')) < zero) RA[[R_joint]] <- matrix(RA[[R_joint]][1, ], nrow=1, ncol=3)

		# If zero vector, set to any value
		if(sum(abs(RA[[R_joint]][1, ])) == 0) RA[[R_joint]][1, ] <- RA[[R_joint]][2, ]
	}
	
	# Set JO to determine correct toggle position of unknown joint (joint out)
	#	Could theoretically do this with a single value/angle but then it would depend on 
	#	the orientation of the axis of rotation. Probably best for now to use a 3D point 
	#	so that it works even when a different axis of rotation (e.g. mean, optimal) is used
	JO <- list()
	if(is.null(jo.at)){

		JO[[joint_solve]] <- matrix(joint.coor[joint_solve, , 1] - joint.coor[fixed.joint, , 1], nrow=1, ncol=3)

	}else{

		# Create NA matrix	
		JO[[joint_solve]] <- matrix(NA, nrow=dim(joint.coor)[3], ncol=3)

		# Set joint out to resolve toggle at time points indicated in input
		JO[[joint_solve]][jo.at, ] <- t(joint.coor[joint_solve, , jo.at] - joint.coor[fixed.joint, , jo.at])
	}

	# Create linkage
	linkage <- list(
		'FJ' = FJ,
		'AJ' = AJ,
		'LL' = LL,
		'RA' = RA,
		'RD' = RD,
		'RI' = RI,
		'JO' = JO,
		'joint.conn' = joint.conn
	)

	return(linkage)
}

