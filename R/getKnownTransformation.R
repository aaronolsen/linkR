getKnownTransformation <- function(mechanism, input.param, joint, body, iter, 
	print.progress = FALSE, indent = '\t', indent.level=3){

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
	if(kn_jt_type %in% c('S', 'X', 'XO', 'R', 'U', 'PR', 'RP', 'O')){

		# Set initial transformation matrices
		tmat1 <- tmat2 <- tmat3 <- diag(4)

		# Set magnitudes
		if(kn_jt_type %in% c('PR', 'RP')){
			mags <- input.param[iter, 3]
		}else{
			mags <- input.param[iter, ]
		}

		# SKIP IF INPUTS ARE NA (ALLOWS INPUT RESOLVE PARAMETERS TO BE ADDED IN ADDITION TO INPUT PARAMETERS)
		if(sum(is.na(mags)) == length(mags)) return(NULL)

		# TRANSLATE TO CENTER OF ROTATION (JOINT)
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

		# LOOP THROUGH EACH COLUMNN OF INPUT PARAMETERS
		for(i in length(mags):1){

			# SKIP IF NA
			if(is.na(mags[i]) || is.na(AOR[i, 1])) next
			
			#
			RM <- tMatrixEP(AOR[i, ], -mags[i])
			
			# APPLY ROTATION ABOUT SINGLE JOINT CONSTRAINT VECTOR
			tmat2[1:3, 1:3] <- tmat2[1:3, 1:3] %*% RM

			if(print.progress){
				cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Apply rotation of magnitude ', 
					signif(mags[i], 3), ' about center {', paste0(signif(kn_jt_center, 3), collapse=','), 
					'} and axis {', paste0(signif(AOR[i, ], 3), collapse=','), '}\n'))
			}

			#
			if(kn_jt_type == 'U' && i == 2) AOR[1, ] <- AOR[1, ] %*% RM
		}

		# TRANSLATE BACK FROM CENTER OF ROTATION
		tmat3[1:3, 4] <- -kn_jt_center
		
		# Get rotation transformations
		rtmat <- tmat1 %*% tmat2 %*% tmat3
		
		# Transform found
		tform_set <- TRUE
	}

	if(kn_jt_type %in% c('L', 'P', 'T', 'PR', 'RP')){
	
		# Set translation magnitudes
		if(kn_jt_type %in% c('PR', 'RP')){
			mags <- input.param[iter, 1:2]
		}else{
			mags <- input.param[iter, ]
		}

		# TRANSLATE
		ttmat[1:3, 4] <- colSums(mags*matrix(kn_jt_axes[, , jt_set], ncol=3))

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