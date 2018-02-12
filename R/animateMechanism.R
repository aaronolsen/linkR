animateMechanism <- function(mechanism, input.param, input.joint = NULL, input.body = NULL, 
	joint.compare = NULL, use.ref.as.prev = FALSE, check.inter.joint.dist = TRUE, check.joint.cons = TRUE, 
	check.inter.point.dist = TRUE, print.progress = FALSE, print.progress.iter = 2){

	if(print.progress) cat(paste0('animateMechanism()\n'))

	# Convert input.param into list of matrices for consistency across mechanisms with differing degrees of freedom
	if(class(input.param) == 'numeric') input.param <- list(matrix(input.param, nrow=length(input.param), ncol=1))
	if(class(input.param) == 'matrix') input.param <- list(input.param)
	if(class(input.param) == 'list'){
		for(i in 1:length(input.param)) if(is.vector(input.param[[i]])) input.param[[i]] <- matrix(input.param[[i]], nrow=length(input.param[[i]]), ncol=1)
	}
	
	# Set constants
	n_inputs <- length(input.param)
	n_iter <- nrow(input.param[[1]])
	indent <- '  '
	
	# Set original coordinates and constraints as reference
	mechanism[['joint.coor.ref']] <- mechanism[['joint.coor']]
	mechanism[['joint.cons.ref']] <- mechanism[['joint.cons']]

	# If joint coordinates are array, only keep last iteration
	if(length(dim(mechanism[['joint.coor']])) == 3) mechanism[['joint.coor']] <- mechanism[['joint.coor']][, , dim(mechanism[['joint.coor']])[3]]
	# ** Should do same for joint.cons


	## Convert joint constraints and coordinates into lists/arrays with two sets
	# Convert joint constraints
	mechanism[['joint.cons.anim']] <- list()
	for(i in 1:length(mechanism$joint.cons)){

		if(is.na(mechanism$joint.cons[[i]][1])){joint_cons[[i]] <- NULL;next}
		mechanism[['joint.cons.anim']][[i]] <- array(mechanism$joint.cons[[i]], dim=c(dim(mechanism$joint.cons[[i]])[1:2], n_iter, 2))
		
		# If U-joint, make sure only corresponding axis is in each set
		if(mechanism$joint.types[i] %in% c('U', 'O')){
			mechanism[['joint.cons.anim']][[i]][1, , , 2] <- NA
			mechanism[['joint.cons.anim']][[i]][2, , , 1] <- NA
		}

		# Keep 3rd axis only with second body/joint set
		if(mechanism$joint.types[i] %in% c('O')) mechanism[['joint.cons.anim']][[i]][3, , , 1] <- NA
	}
	
	# Convert joint coordinates
	mechanism[['joint.coor.anim']] <- array(mechanism$joint.coor, dim=c(dim(mechanism$joint.coor), n_iter, 2), 
		dimnames=list(mechanism$joint.names, colnames(mechanism$joint.coor), NULL, NULL))

	# Make joint coordinate compare a single iteration array if single time point and add to mechanism
	if(!is.null(joint.compare)){
		if(length(dim(joint.compare)) == 2) mechanism[['joint.compare']] <- array(joint.compare, dim=c(dim(joint.compare), 1), 
			dimnames=list(dimnames(joint.compare)[[1]], dimnames(joint.compare)[[2]], NULL))
	}

	# Set input.joint if NULL and a single joint
	if(is.null(input.joint)){
		if(mechanism[['num.joints']] > 1) stop("If the mechanism has more than one joint 'input.joint' must be specified.")
		input.joint <- 1
	}

	# If input.joint is non-numeric, convert to numeric
	if(!is.numeric(input.joint[1])){
		if(sum(!input.joint %in% mechanism$joint.names) > 0) stop("'input.joint' names do not match joint names.")
		input_joint_num <- rep(NA, length(input.joint))
		for(i in 1:length(input.joint)) input_joint_num[i] <- which(mechanism$joint.names == input.joint[i])
		input.joint <- input_joint_num
	}

	# Check that number of input parameters matches input.joint length
	if(n_inputs != length(input.joint)) stop(paste0("The length of input.param (", n_inputs, ") must match the number of input.joint (", length(input.joint), ")."))


	## FIND DEFAULT INPUT BODIES
	# CHECK WHETHER THERE ARE OPEN CHAIN JOINTS
	if(!is.null(mechanism$joints.open)){
	
		# FIND JOINTS IN OPEN CHAIN
		joints_open_in <- input.joint[input.joint %in% mechanism$joints.open]

		# REMAINING JOINTS
		joints_remaining <- input.joint[!input.joint %in% joints_open_in]
	}

	# Make sure input bodies are numeric
	if(!is.numeric(input.body[1])){

		for(i in 1:length(input.body)){

 
			# MAKE SURE THAT ALL BODY NAMES ARE FOUND
			if(!input.body[i] %in% mechanism[['body.names']]) stop(paste0("Name '", input.body[i], "' in 'input.body' not found in body names."))

			# FIND NUMBER CORRESPONDING TO BODY
			input.body[i] <- which(input.body[i] == mechanism$body.names)
		}

		# MAKE NUMERIC
		input.body <- as.numeric(input.body)
	}

	# IF INPUT BODY IS NULL CREATE LIST
	if(FALSE){

		# **** Fix this so that it is simple vector - only one input body needed per input joint

		if(is.null(input.body)){
			input_body <- as.list(rep(NA, length(input.joint)))
		}else{

			# CHECK THAT THE LENGTH OF INPUT.BODY MATCHES THE LENGTH OF INPUT.JOINT
			if(length(input.body) != length(input.joint)) stop(paste0("The length of input.body (", length(input.body), ") should match the length of input.joint (", length(input.joint), ")"))

			input_body <- input.body
		}

		# FILL IN UNKNOWNS IN INPUT BODY LIST
		for(i in 1:length(input_body)){
	
			if(!is.na(input_body[[i]][1])){

				for(j in 1:length(input_body[[i]])){

					if(is.numeric(input_body[[i]][j])) next

					# GET NON-NA VALUES
					input_body_nna <- input_body[!is.na(input_body)]
			
					# MAKE SURE THAT ALL BODY NAMES ARE FOUND
					if(!input_body[[i]][j] %in% mechanism$body.names) stop(paste0("Name '", input_body[[i]][j], "' in 'input.body' not found in body names."))
			
					# FIND NUMBER CORRESPONDING TO BODY
					input_body[[i]][j] <- which(input_body[[i]][j] == mechanism$body.names)
				}

				# MAKE NUMERIC
				input_body[[i]] <- as.numeric(input_body[[i]])

				next
			}

			if(1 %in% mechanism$body.conn.num[input.joint[i], ]){
		
				# LINK OTHER THAN 1 (FIXED)
				input_body[[i]] <- max(mechanism$body.conn.num[input.joint[i], ])
			}else{

				# CHOOSE MAX FOR NOW - PERHAPS NOT NECESSARY IN MOST CASES SINCE INPUT AT JOINT 
				# WITHIN CLOSED LOOP LIKELY USED FOR SPECIFIC PATH FRAGMENTS IN SOLVING PATH
				input_body[[i]] <- max(mechanism$body.conn.num[input.joint[i], ])
			}

			if(is.null(mechanism$body.transform)) next

			# ADD OPEN CHAIN BODIES TO TRANSFORM FOR EACH INPUT PARAMETER
			body_transform <- sort(unique(c(input_body[[i]], mechanism$body.transform[[input.joint[i]]])))
			input_body[[i]] <- body_transform[!is.na(body_transform)]
		}
	}

	## Other
	# Get linkage size (if more than one joint and joints are not the same)
	linkage_size <- 1
	if(mechanism[['num.joints']] > 1 && sum(apply(mechanism$joint.coor, 2, 'sd', na.rm=TRUE)) > 1e-5){
		linkage_size <- mean(sqrt(rowSums((mechanism$joint.coor - matrix(colMeans(mechanism$joint.coor), nrow=mechanism[['num.joints']], ncol=3, byrow=TRUE))^2)))
	}

	# Create array of transformation matrices for each body and iteration
	mechanism[['tmat']] <- array(diag(4), dim=c(4,4,mechanism[['num.bodies']], n_iter), 
		dimnames=list(NULL, NULL, mechanism[['body.names']], NULL))

	# Create initial joint status
	status_init <- list(
		'jointed'=rep(TRUE, mechanism[['num.joints']]),
		'solved'=matrix(0, nrow=mechanism[['num.joints']], ncol=2),
		'transformed'=matrix(FALSE, nrow=mechanism[['num.joints']], ncol=2)
	)
	
	# Set fixed joints halves as fixed and solved
	status_init[['solved']][mechanism[['body.conn.num']] == 1] <- 2

	## Animation
	# Loop through each iteration
	for(iter in 1:n_iter){

		# Set whether to print progress for current iteration
		print_progress_iter <- ifelse(print.progress && iter %in% print.progress.iter, TRUE, FALSE)

		# Set known joint motions
		if(print_progress_iter) cat(paste0(paste0(rep(indent, 1), collapse=''), 'Iteration: ', iter, '\n'))

		# Reset joint status to initial state
		mechanism[['status']] <- status_init

		if(print_progress_iter){
			cat(paste0(paste0(rep(indent, 2), collapse=''), 'Apply known transformations\n'))
		}

		for(kn_idx in 1:length(input.joint)){
		
			kn_jt_idx <- input.joint[kn_idx]

			# Print known transformation header (transformation number, body, joint)
			if(print_progress_iter){
				cat(paste0(paste0(rep(indent, 3), collapse=''), kn_idx, ') Apply transformation to body \'', 
					mechanism$body.names[input.body[kn_idx]], '\' (', input.body[kn_idx], ') at joint \'', 
					dimnames(mechanism$joint.coor)[[1]][kn_jt_idx], '\' (', kn_jt_idx, 
					') of type \'', mechanism$joint.types[kn_jt_idx], '\'\n'))
			}

			# Resolve disjoint at input joint if disjointed
			# Extend transformation across input joint and then subtracting that transformation 
			# from any subsequently disjointed joints so that any previous transformations are kept
			mechanism <- extendTransformation(mechanism, joint=kn_jt_idx, body=input.body[kn_idx],
				body.excl=input.body[kn_idx], iter=iter, recursive=TRUE, print.progress=print_progress_iter, indent=indent, 
				indent.level=4)

			# Get transformation to apply to input body at input joint
			kn_tmat <- getKnownTransformation(mechanism=mechanism, input.param=input.param[[kn_idx]], 
				joint=kn_jt_idx, body=input.body[kn_idx], iter=iter, print.progress=print_progress_iter, 
				indent=indent, indent.level=4)
			
			# **** U-joint and other joints where different axes are associated with different bodies
			# should be split into two transformations. Same input joint but different bodies. 
			# This way first transformation transforms the second axis, which is then ready 
			# for the second transformation.			

			# Transform body and extend transformation
			mechanism <- extendTransformation(mechanism, body=input.body[kn_idx], tmat=kn_tmat, 
				iter=iter, recursive=TRUE, joint=kn_jt_idx, status.solved.to=1, body.excl=input.body[kn_idx], 
				print.progress=print_progress_iter, indent=indent, indent.level=4)
				
			# Extend solved status through mechanism
			mechanism <- extendSolvedStatus(mechanism, print.progress=print_progress_iter, 
				indent=indent, indent.level=4)

			#if(print_progress_iter) print_joint_status(mechanism, indent)

			#break

			# Print statuses
			#if(print_progress_iter) print_joint_status(mechanism, indent)
		}
		
		#next

		# Print statuses
		#if(print_progress_iter) print_joint_status(mechanism, indent, indent.level=2)

		# Try solving joint paths
		mechanism <- resolveJointPaths(mechanism, iter=iter, print.progress=print_progress_iter, 
			indent=indent, indent.level=2)

		# Print statuses
		if(print_progress_iter) print_joint_status(mechanism, indent, indent.level=2)
	}

	## Apply body transformations to body points
	if(!is.null(mechanism[['body.points']])){

		# Create array from body point matrix
		mechanism[['body.points.anim']] <- array(mechanism[['body.points']], dim=c(dim(mechanism[['body.points']]), n_iter), 
			dimnames=list(rownames(mechanism[['body.points']]), NULL, NULL))

		# For each body
		for(body_num in 1:length(mechanism[['points.assoc']])){

			if(is.na(mechanism[['points.assoc']][[body_num]][1])) next
			
			# Apply transformation
			mechanism[['body.points.anim']][mechanism[['points.assoc']][[body_num]], , ] <- 
				applyTransform(mechanism[['body.points']][mechanism[['points.assoc']][[body_num]], ], 
					mechanism[['tmat']][, , body_num, ])
		}
	}

	return(mechanism)
}