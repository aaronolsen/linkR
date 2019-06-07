animateMechanism <- function(mechanism, input.param, joint.compare = NULL, 
	use.ref.as.prev = FALSE, check.inter.joint.dist = FALSE, check.joint.cons = FALSE, check.inter.point.dist = FALSE, 
	print.progress = FALSE, print.progress.iter = 1){

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
	
	# Save number of iterations
	mechanism[['num.iter']] <- n_iter

	# If joint coordinates are array, only keep last iteration
	if(length(dim(mechanism[['joint.coor']])) == 3) mechanism[['joint.coor']] <- mechanism[['joint.coor']][, , dim(mechanism[['joint.coor']])[3]]
	# ** Should do same for joint.cons


	## Convert joint constraints and coordinates into lists/arrays with two sets
	# Convert joint constraints
	mechanism[['joint.cons.anim']] <- list()
	for(i in 1:length(mechanism$joint.cons)){

		# Set default constraint?? - should do this in defineMechanism
		if(is.na(mechanism$joint.cons[[i]][1])){
			next
		}

		# Create array
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

	# Create identity matrix associated with each body
	#mechanism[['body.diag']] <- array(diag(3), dim=c(3,3, mechanism$num.bodies, n_iter, 2), dimnames=list(NULL, NULL, mechanism[['body.names']], NULL, NULL))
	
	# Make joint coordinate compare an array matching number of iterations and add to mechanism
	if(!is.null(joint.compare)){
		if(length(dim(joint.compare)) == 2){
			mechanism[['joint.compare']] <- array(joint.compare, dim=c(dim(joint.compare), n_iter), 
				dimnames=list(dimnames(joint.compare)[[1]], dimnames(joint.compare)[[2]], NULL))
		}else{
			mechanism[['joint.compare']] <- joint.compare
		}
	}

	# Get input joint(s) and body/bodies from mechanism
	input.dof <- mechanism[['input.dof']]
	input.joint <- mechanism[['input.joint']]
	input.body <- mechanism[['input.body']]

	# Check that number of input parameters matches input_joint_full length
	if(n_inputs != length(input.joint)) stop(paste0("The length of input.param (", n_inputs, ") must match the number of input.joint (", length(input.joint), ")."))

	## Separate full and partial inputs *** Eventually these will be combined, user will 
	# specify all known and necessary DoFs at each joint (either full or partial) and program will solve for 
	# transformations. But currently chain solving only treats joints as solved or not solved, 
	# not partially solved. Solved status 1 is a solved joint that is not yet fixed.
	# Which joints are fully input (all DoFs specified)
	input_joint_is_full <- rep(TRUE, length(input.joint))
	for(i in 1:length(input.joint)) if(any(is.na(input.dof[[i]]))) input_joint_is_full[i] <- FALSE

	# Vector of just those input joints that have all DoFs specified
	input_joint_full <- input.joint[input_joint_is_full]
	input_body_full <- input.body[input_joint_is_full]
	input_param_full <- input.param[input_joint_is_full]
	
	# Get partial inputs
	input_joint_part <- input.joint[!input_joint_is_full]
	input_body_part <- input.body[!input_joint_is_full]
	input_dof_part <- input.dof[!input_joint_is_full]
	input_param_part <- input.param[!input_joint_is_full]

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

		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Apply known transformations\n'))

		for(kn_idx in 1:length(input_joint_full)){
		
			kn_jt_idx <- input_joint_full[kn_idx]

			# Print known transformation header (transformation number, body, joint)
			if(print_progress_iter){
				cat(paste0(paste0(rep(indent, 3), collapse=''), kn_idx, ') Apply transformation to body \'', 
					mechanism[['body.names']][input_body_full[kn_idx]], '\' (', input_body_full[kn_idx], ') at joint \'', 
					dimnames(mechanism[['joint.coor']])[[1]][kn_jt_idx], '\' (', kn_jt_idx, 
					') of type \'', mechanism[['joint.types']][kn_jt_idx], '\'\n'))
			}

			# Resolve disjoint at input joint if disjointed
			# Extend transformation across input joint and then subtracting that transformation 
			# from any subsequently disjointed joints so that any previous transformations are kept
			mechanism <- extendTransformation(mechanism, joint=kn_jt_idx, body=input_body_full[kn_idx],
				body.excl=input_body_full[kn_idx], iter=iter, recursive=TRUE, print.progress=print_progress_iter, indent=indent, 
				indent.level=4)

			# Get transformation to apply to input body at input joint
			kn_tmat <- getKnownTransformation(mechanism=mechanism, input.param=input_param_full[[kn_idx]], 
				joint=kn_jt_idx, body=input_body_full[kn_idx], iter=iter, print.progress=print_progress_iter, 
				indent=indent, indent.level=4)
			
			# **** U-joint and other joints where different axes are associated with different bodies
			# should be split into two transformations. Same input joint but different bodies. 
			# This way first transformation transforms the second axis, which is then ready 
			# for the second transformation.			

			# Transform body and extend transformation
			mechanism <- extendTransformation(mechanism, body=input_body_full[kn_idx], tmat=kn_tmat, 
				iter=iter, recursive=TRUE, joint=kn_jt_idx, status.solved.to=1, body.excl=input_body_full[kn_idx], 
				print.progress=print_progress_iter, indent=indent, indent.level=4)
				
			# Extend solved status through mechanism
			mechanism <- extendSolvedStatus(mechanism, print.progress=print_progress_iter, 
				indent=indent, indent.level=4)

			#if(print_progress_iter) print_joint_status(mechanism, indent)

			#break

			# Print statuses
			if(print_progress_iter) print_joint_status(mechanism, indent)
		}
		
		#next

		# Print statuses
		#if(print_progress_iter) print_joint_status(mechanism, indent, indent.level=2)

		# Try solving joint paths
		mechanism <- resolveJointPaths(mechanism, iter=iter, print.progress=print_progress_iter, 
			indent=indent, indent.level=2)

		# Apply partial knowns (at this point this is just used to rotate S-S links about specified axis
		if(length(input_joint_part) > 0){

			if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Apply known partial transformations\n'))

			for(kn_idx in 1:length(input_joint_part)){

				kn_jt_idx <- input_joint_part[kn_idx]

				# Print known transformation header (transformation number, body, joint)
				if(print_progress_iter){
					cat(paste0(paste0(rep(indent, 3), collapse=''), kn_idx, ') Apply partial transformation to body \'', 
						mechanism[['body.names']][input_body_part[kn_idx]], '\' (', input_body_part[kn_idx], ') at joint \'', 
						dimnames(mechanism[['joint.coor']])[[1]][kn_jt_idx], '\' (', kn_jt_idx, 
						') of type \'', mechanism[['joint.types']][kn_jt_idx], '\'\n'))
				}

				# Get transformation to apply to input body at input joint
				kn_tmat <- getKnownTransformation(mechanism=mechanism, input.param=input_param_part[[kn_idx]], 
					joint=kn_jt_idx, dof=input_dof_part[[kn_idx]], body=input_body_part[kn_idx], iter=iter, print.progress=print_progress_iter, 
					indent=indent, indent.level=4)
				
				# Transformation body
				mechanism <- transformBody(mechanism, status.solved.to=NULL, body=input_body_part[kn_idx], 
					tmat=kn_tmat, iter=iter, print.progress=print_progress_iter, 
					indent=indent, indent.level=4)
			}
		}

		# Print statuses
		if(print_progress_iter) print_joint_status(mechanism, indent, indent.level=2)
	}

	## Apply body transformations to joints and constraints
	mechanism <- applyJointTransform(mechanism)

	## Perform error checks
	checkMechanism(mechanism, check.inter.joint.dist=check.inter.joint.dist, check.joint.cons=check.joint.cons, 
		check.inter.point.dist=check.inter.point.dist, print.progress=print.progress, print.progress.iter=print.progress.iter)

	## Apply body transformations to body points
	if(!is.null(mechanism[['body.points']])){

		# Create array from body point matrix
		mechanism[['body.points.anim']] <- array(mechanism[['body.points']], dim=c(dim(mechanism[['body.points']]), n_iter), 
			dimnames=list(rownames(mechanism[['body.points']]), NULL, NULL))

		# For each body
		for(body_num in 1:length(mechanism[['points.assoc']])){

			if(is.na(mechanism[['points.assoc']][[body_num]][1])) next
			
			# Apply transformation
			transformed <- applyTransform(mechanism[['body.points']][mechanism[['points.assoc']][[body_num]], ], 
				mechanism[['tmat']][, , body_num, ])

			# Account for single point at multiple time points getting transposed when coercing into an array
			if(length(mechanism[['points.assoc']][[body_num]]) == 1 && length(dim(mechanism[['tmat']][, , body_num, ])) == 3){
				transformed <- t(transformed)
			}
			
			mechanism[['body.points.anim']][mechanism[['points.assoc']][[body_num]], , ] <- transformed
		}
	}

	return(mechanism)
}