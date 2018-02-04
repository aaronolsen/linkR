getKnownTransformation <- function(mechanism, input.param, joint, body, iter, print.progress, indent){

	#if(print.progress) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Apply known transformation', ))

	# Get joint parameters
	kn_jt_center <- mechanism[['joint.coor.anim']][joint, , iter, 1]
	kn_jt_type <- mechanism$joint.types[joint]
	kn_jt_axes <- array(mechanism[['joint.cons.anim']][[joint]][, , iter, ], dim=c(dim(mechanism[['joint.cons.anim']][[joint]])[1:2], 2))

	# Print known parameters
	if(print.progress){

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
		
		cat(paste0(paste0(rep(indent, 3), collapse=''), 'getKnownTransformation()\n'))
		if(kn_jt_type %in% c('R', 'U', 'S', 'O', 'X')){
			cat(paste0(paste0(rep(indent, 4), collapse=''), 'Center: ', paste0(signif(kn_jt_center, 3), collapse=','), '; '))
		}
		cat(paste0(paste0(axes_c, collapse='; '), '; '))
		if(length(input.param[iter, ]) == 1){
			cat(paste0('Magnitude: ', paste0(signif(input.param[iter, ], 3), collapse=',')))
		}else{
			cat(paste0('Magnitudes: ', paste0(signif(input.param[iter, ], 3), collapse=',')))
		}
		cat('\n')
	}

	# Set initial transformation matrices
	tmat1 <- tmat2 <- tmat3 <- tmat4 <- diag(4)

	# Get joint set index of current/transformed (first) and neighboring body (second)
	jt_set <- which(body == mechanism[['body.conn.num']][joint, ])
	
	#
	joint_cons <- mechanism[['joint.cons.anim']][[joint]]

	# Set initial value of whether transformation was found
	tform_set <- FALSE

	# Create transformation matrix
	if(kn_jt_type %in% c('S', 'X', 'XO', 'R', 'U', 'PR', 'O')){

	# Set magnitudes
	if(kn_jt_type == 'PR'){
		mags <- input.param[iter, 3]
	}else{
		mags <- input.param[iter, ]
	}

	# SKIP IF INPUTS ARE NA (ALLOWS INPUT RESOLVE PARAMETERS TO BE ADDED IN ADDITION TO INPUT PARAMETERS)
	if(sum(is.na(mags)) == length(mags)) return(NULL)

		# Set axes of rotation
		if(kn_jt_type == 'PR'){
			AOR <- matrix(cprod(kn_jt_axes[1, , jt_set], kn_jt_axes[2, , jt_set]), nrow=1)
		}else{
			AOR <- matrix(kn_jt_axes[, , jt_set], nrow=1)
		}
		
		# TRANSLATE TO CENTER OF ROTATION (JOINT)
		tmat1[1:3, 4] <- kn_jt_center

		# LOOP THROUGH EACH COLUMNN OF INPUT PARAMETERS
		for(i in dim(AOR)[1]:1){
		
			# SKIP IF NA
			if(is.na(mags[i])) next
			
			# APPLY ROTATION ABOUT SINGLE JOINT CONSTRAINT VECTOR
			tmat2[1:3, 1:3] <- tmat2[1:3, 1:3] %*% tMatrixEP(AOR[i, ], -mags[i])
		}

		# TRANSLATE BACK FROM CENTER OF ROTATION
		tmat3[1:3, 4] <- -kn_jt_center
		
		# Transform found
		tform_set <- TRUE
	}

	if(kn_jt_type %in% c('L', 'P', 'T', 'PR')){
	
		# Set translation magnitudes
		if(kn_jt_type == 'PR'){
			mags <- input.param[iter, 1:2]
		}else{
			mags <- input.param[iter, ]
		}

		# TRANSLATE
		tmat4[1:3, 4] <- colSums(mags*matrix(kn_jt_axes[, , jt_set], ncol=3))

		# Transform found
		tform_set <- TRUE
	}

	# Error if joint not found
	if(!tform_set){cat('\n');stop(paste0("Unrecognized joint type '", kn_jt_type, "'"))}

	# COMBINE TRANSFORMATION MATRICES
	tmat <- tmat1 %*% tmat2 %*% tmat3 %*% tmat4
	
	tmat
}