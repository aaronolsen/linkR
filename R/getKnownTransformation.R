getKnownTransformation <- function(mechanism, input.param, joint, body, iter, 
	dof = NULL, print.progress = FALSE, indent = '\t', indent.level=3){

	#if(print.progress) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Apply known transformation', ))

	# Get joint type
	kn_jt_type <- mechanism$joint.types[joint]

	# Apply current transformation to get current joint coordinate and constraint vectors
	joint_current <- applyJointTransform(mechanism, joint=joint, iter=iter)
	kn_jt_center <- joint_current$coor[,,1]
	kn_jt_axes <- joint_current$cons[[1]]

	#kn_jt_center <- mechanism[['joint.coor.anim']][joint, , iter, 1]
	#kn_jt_axes <- array(mechanism[['joint.cons.anim']][[joint]][, , iter, ], dim=c(dim(mechanism[['joint.cons.anim']][[joint]])[1:2], 2))

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'getKnownTransformation()\n'))

	# Print known parameters
	if(FALSE){

		# For each joint set
		axes_c <- c()
		for(i in 1:2){

			# Create string of axis vectors
			if(dim(kn_jt_axes)[1] == 1){
				axes_c <- c(axes_c, paste0('Axis-', i, ': ', paste0(signif(kn_jt_axes[, , i], 3), collapse=',')))
			}else{
				axes_str <- paste0('Axes-', i, ': ')
				for(j in 1:dim(kn_jt_axes)[1]){
					axes_str <- paste0(axes_str, ' ', paste0(signif(kn_jt_axes[j, , i], 3), collapse=','))
				}
				axes_c <- c(axes_c, axes_str)
			}
		}
		
		if(kn_jt_type %in% c('R', 'U', 'S', 'O', 'X')){
			#cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Center: ', paste0(signif(kn_jt_center, 3), collapse=','), '; '))
			cat(paste0('Center: ', paste0(signif(kn_jt_center, 3), collapse=','), '; '))
		}
		cat(paste0(paste0(axes_c, collapse='; '), '; '))
		if(length(input.param[iter, ]) == 1){
			cat(paste0('Magnitude: ', paste0(signif(input.param[iter, ], 3), collapse=',')))
		}else{
			cat(paste0('Magnitudes: ', paste0(signif(input.param[iter, ], 3), collapse=',')))
		}
		cat('\n')
	}

	#
	rtmat <- ttmat <- diag(4)

	# Get joint set index of current/transformed (first) and neighboring body (second)
	jt_set <- which(body == mechanism[['body.conn.num']][joint, ])
	jt_set_adj <- which(body != mechanism[['body.conn.num']][joint, ])
	
	# Set initial value of whether transformation was found
	tform_set <- FALSE

	# Create transformation matrix
	if(kn_jt_type %in% c('S', 'X', 'XO', 'R', 'U', 'PR', 'N', 'RP', 'O')){

		# Set initial transformation matrices
		tmat1 <- tmat2 <- tmat3 <- diag(4)

		# Set rotation magnitudes
		if(kn_jt_type %in% c('PR', 'RP')){
			mags <- input.param[iter, 3]
		}else if(kn_jt_type %in% c('N')){
			mags <- input.param[iter, 4:6]
		}else{
			mags <- input.param[iter, ]
		}

		# SKIP IF INPUTS ARE NA (ALLOWS INPUT RESOLVE PARAMETERS TO BE ADDED IN ADDITION TO INPUT PARAMETERS)
		if(sum(is.na(mags)) == length(mags)) return(NULL)

		# TRANSLATE FROM CENTER OF ROTATION (JOINT)
		tmat1[1:3, 4] <- kn_jt_center

		# Set axes of rotation
		if(kn_jt_type %in% c('PR', 'RP')){
			AOR <- matrix(cprod(kn_jt_axes[1, , 1], kn_jt_axes[2, , 1]), nrow=1, ncol=3)
		}else if(kn_jt_type == 'U'){
			AOR <- matrix(NA, 2, 3)
			AOR[1,] <- matrix(kn_jt_axes[1, , 1], ncol=3)
			AOR[2,] <- matrix(kn_jt_axes[2, , 2], ncol=3)
		}else{
			AOR <- matrix(kn_jt_axes[, , 1], ncol=3)
		}
		
		# Limit AOR to specified degrees of freedom
		if(!is.null(dof)) AOR <- matrix(AOR[na.omit(dof),], nrow=sum(!is.na(dof)), ncol=3)
		
		#
		#if(print.progress) rotations_print <- c()
		
		# LOOP THROUGH EACH COLUMNN OF INPUT PARAMETERS
		for(i in 1:length(mags)){

			# SKIP IF NA
			if(is.na(mags[i]) || is.na(AOR[i, 1])) next
			
			#
			RM <- tMatrixEP(AOR[i, ], -mags[i])
			
			# APPLY ROTATION ABOUT SINGLE JOINT CONSTRAINT VECTOR
			tmat2[1:3, 1:3] <- RM %*% tmat2[1:3, 1:3]

			if(print.progress){
				#rotations_print <- c(rotations_print, 
				cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Apply rotation of magnitude ', 
					signif(mags[i], 3), ' radians about center {', paste0(signif(kn_jt_center, 3), collapse=','), 
					'} and axis {', paste0(signif(AOR[i, ], 3), collapse=','), '}\n'))
			}

			#if(print.progress){ # First two and last two axes should stay orthogonal, first and third not necessarily
			#	print(avec(AOR[1,],AOR[2,]))
			#	print(avec(AOR[2,],AOR[3,]))
			#	print(avec(AOR[1,],AOR[3,]))
			#	cat('\n')
			#}

			#
			if(kn_jt_type == 'U'){
				if(i == 1) AOR[2, ] <- RM %*% AOR[2, ]
			}else if(kn_jt_type == 'O'){
				if(i == 1){
					AOR[2, ] <- RM %*% AOR[2, ]
					AOR[3, ] <- RM %*% AOR[3, ]
				}else if(i == 2){
					AOR[3, ] <- RM %*% AOR[3, ]
				}
			}
		}

		#if(print.progress) cat(paste0(rev()), sep='')
		
		#if(print.progress){ # First two and last two axes should stay orthogonal, first and third not necessarily
		#	print(avec(AOR[1,],AOR[2,]))
		#	print(avec(AOR[2,],AOR[3,]))
		#	print(avec(AOR[1,],AOR[3,]))
		#	cat('\n')
		#}

		# TRANSLATE TO CENTER OF ROTATION
		tmat3[1:3, 4] <- -kn_jt_center
		
		# Get rotation transformations
		rtmat <- tmat1 %*% tmat2 %*% tmat3
		
		# Transform found
		tform_set <- TRUE
	}

	if(kn_jt_type %in% c('L', 'P', 'T', 'PR', 'N', 'RP')){
	
		# Set translation magnitudes
		if(kn_jt_type %in% c('PR', 'RP')){
			mags <- input.param[iter, 1:2]
		}else if(kn_jt_type %in% c('N')){
			mags <- input.param[iter, 1:3]
		}else{
			mags <- input.param[iter, ]
		}
		
		# Get axis/axes along which to translate
		if(kn_jt_type %in% c('P')){
			T_axes <- matrix(kn_jt_axes[1:2, , jt_set], ncol=3)
		}else{
			T_axes <- matrix(kn_jt_axes[, , jt_set], ncol=3)
		}

		# Limit AOR to specified degrees of freedom
		if(!is.null(dof)) T_axes <- matrix(AOR[na.omit(dof),], nrow=sum(!is.na(dof)), ncol=3)

		# TRANSLATE
		ttmat[1:3, 4] <- colSums(mags*T_axes)

		if(print.progress){
			for(i in 1:length(mags)){
				cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Apply translation of magnitude ', 
					paste0(signif(mags[i], 3), collapse=','), ' along axis {', paste0(signif(uvector(T_axes[i,]), 3), collapse=','), '}\n'))
			}
		}

		# Transform found
		tform_set <- TRUE
	}

	# Error if joint not found
	if(!tform_set){cat('\n');stop(paste0("Unrecognized joint type '", kn_jt_type, "'"))}

	# COMBINE TRANSFORMATION MATRICES
	if(kn_jt_type == 'RP'){
		tmat <- ttmat %*% rtmat
	}else{
		tmat <- rtmat %*% ttmat
	}
	
	tmat
}