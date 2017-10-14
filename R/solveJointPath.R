solveJointPath <- function(joint.types, joint.status, joint.coor, joint.cons,  body.num, 
	input, body.conn, joint.names, joint.prev, joint.ref, iter = 1, print.progress = FALSE, 
	indent = ''){

	# INPUT
	if(length(joint.types) == 1){

		# SET NULL TRANSFORMATION MATRICES
		tmat1 <- tmat2 <- tmat3 <- tmat4 <- diag(4)

		# GET JOINT SET TO TRANSFORM
		jt_set <- which(body.num == body.conn)
		
		# Logical if input transformation set
		tform_set <- FALSE

		if(print.progress) joint_props <- c()

		# CREATE TRANSFORMATION MATRIX
		if(joint.types %in% c('S', 'X', 'XO', 'R', 'U', 'PR', 'V')){

			# Set rotation magnitudes
			if(joint.types == 'PR'){
				mags <- input[iter, 3]
			}else{
				mags <- input[iter, ]
			}

			# SKIP IF INPUTS ARE NA (ALLOWS INPUT RESOLVE PARAMETERS TO BE ADDED IN ADDITION TO INPUT PARAMETERS)
			if(sum(is.na(mags)) == length(mags)) return(list('body.tmat'=NULL,'joint.status'=NULL,'solution'=TRUE))
			
			# Set axes of rotation
			if(joint.types == 'PR'){
				AOR <- array(cprod(joint.cons[[1]][1, , iter, jt_set], joint.cons[[1]][2, , iter, jt_set]), dim=c(1,3,1))
			}else{
				AOR <- array(joint.cons[[1]][, , iter, ], dim=dim(joint.cons[[1]])[c(1,2,4)])
			}

			#
			if(joint.types %in% c('U', 'V')){

				jt_set <- 1:2

				# Apply first axis rotation to second axis of U-joint and second and third axis of V-joint
				AOR[2:dim(AOR)[1], , jt_set[2]] <- rotateBody(m=AOR[2:dim(AOR)[1], , jt_set[2]], v=AOR[1, , jt_set[1]], a=mags[1])

				# Apply second axis rotation to third axis of V-joint
				if(joint.types == 'V') AOR[3, , jt_set[2]] <- rotateBody(m=AOR[3, , jt_set[2]], v=AOR[2, , jt_set[2]], a=mags[2])

			}else{
				jt_set <- rep(jt_set, dim(AOR)[1])
			}

			if(print.progress){

				AORs_print <- c()
				for(i in 1:dim(AOR)[1]){
					if(i == 1){ jt_set_i <- 1 }else{ jt_set_i <- 2 }
					AORs_print <- c(AORs_print, paste0(round(AOR[i, , jt_set_i], 3), collapse=','))
				}
				AOR_print <- paste0(paste0('AoR', 1:length(AORs_print), ':', AORs_print), collapse='; ')
				
				joint_props <- c(joint_props, paste0('CoR:', paste0(round(joint.coor[, jt_set[1]], 3), collapse=',')))
				joint_props <- c(joint_props, AOR_print)
				joint_props <- c(joint_props, paste0('Angle:', paste0(round(mags, 3), collapse=',')))
			}

			# TRANSLATE TO CENTER OF ROTATION (JOINT)
			tmat1[1:3, 4] <- joint.coor[, jt_set[1]]

			# LOOP THROUGH EACH COLUMNN OF INPUT PARAMETERS
			for(i in dim(AOR)[1]:1){
			
				# SKIP IF NA
				if(is.na(mags[i])) next
				
				# Set joint set
				if(i == 1){ jt_set_i <- 1 }else{ jt_set_i <- 2 }

				# APPLY ROTATION ABOUT SINGLE JOINT CONSTRAINT VECTOR
				tmat2[1:3, 1:3] <- tmat2[1:3, 1:3] %*% tMatrixEP(AOR[i, , jt_set_i], -mags[i])
			}

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat3[1:3, 4] <- -joint.coor[, jt_set[1]]
			
			# Transform found
			tform_set <- TRUE
		}

		if(joint.types %in% c('L', 'P', 'T', 'PR')){
		
			# Set translation magnitudes
			if(joint.types == 'PR'){
				mags <- input[iter, 1:2]
			}else{
				mags <- input[iter, ]
			}

			if(print.progress){

				tvecs_print <- c()
				for(i in 1:dim(joint.cons[[1]])[1]) tvecs_print <- c(tvecs_print, paste0(round(joint.cons[[1]][i, , iter, jt_set], 3), collapse=','))
				tvec_print <- paste0(paste0('Tvec', 1:length(tvecs_print), ':', tvecs_print), collapse='; ')

				joint_props <- c(joint_props, tvec_print)
				joint_props <- c(joint_props, paste0('Mag:', paste0(round(mags, 3), collapse=',')))
			}

			# TRANSLATE
			tmat4[1:3, 4] <- colSums(mags*matrix(joint.cons[[1]][, , iter, jt_set], ncol=3))

			# Transform found
			tform_set <- TRUE
		}

		# Error if joint not found
		if(!tform_set){cat('\n');stop(paste0("Unrecognized joint type '", joint.types, "'"))}
		
		if(print.progress) cat(paste0('{', paste0(joint_props, collapse='; '), '}\n'))

		# CHANGE JOINT STATUS
		joint.status[unique(jt_set)] <- 'i'
		
		# COMBINE TRANSFORMATION MATRICES
		tmat <- tmat1 %*% tmat2 %*% tmat3 %*% tmat4
		
		return(list('body.tmat'=list(tmat), 'joint.status'=joint.status))

	# SOLVE
	}else{

		# SET TRANSFORM INDICATOR
		trfm_vec <- rep('', length(joint.types))
		#trfm_vec[rowSums(joint.status == '') > 0] <- '^'
		trfm_vec[rowSums(joint.status == 'c') == 2] <- '*'
		trfm_vec[rowSums((joint.status == 'f')+(joint.status == 'c')) == 2] <- '*'
		trfm_vec[rowSums((joint.status == '')+(joint.status == 'c')) == 2] <- '*'
#		trfm_vec[rowSums((joint.status == '')+(joint.status == 'c')) == 2] <- '*'

		# CREATE STRING WITH JOINT TYPES AND TRANSFORM INDICATOR
		type_str <- paste0(paste0(joint.types, trfm_vec), collapse='-')

		# REVERSE INPUTS
		if(type_str %in% c('S-S*-L', 'S*-S-S', 'S*-S-S-S', 'R-S*-S')){

			solve_joint_path <- solveJointPath(
				joint.types=joint.types[length(joint.types):1], 
				joint.status=joint.status[nrow(joint.status):1, ], 
				joint.coor=joint.coor[nrow(joint.coor):1, , ], 
				joint.cons=joint.cons[length(joint.cons):1], 
				body.num=body.num[length(body.num):1], 
				input=input[length(input):1], 
				body.conn=body.conn, 
				joint.names=joint.names[length(joint.cons):1], 
				joint.prev=joint.prev[nrow(joint.prev):1, ], 
				joint.ref=joint.ref[nrow(joint.ref):1, ], 
				iter=iter, print.progress=print.progress, indent=indent)
			
			if(is.null(solve_joint_path$body.tmat)) return(list('body.tmat'=NULL,'joint.status'=NULL,'solution'=solve_joint_path$solution))
			
			return(list(
				'body.tmat'=solve_joint_path$body.tmat[length(solve_joint_path$body.tmat):1],
				'joint.status'=solve_joint_path$joint.status[nrow(solve_joint_path$joint.status):1, ],
				'solution'=solve_joint_path$solution
				))
		}

		# PRINT PATH DETAILS
		if(print.progress){
			name_vec <- joint.names
			name_wt <- paste0(paste0(name_vec, trfm_vec), collapse='-')
			#cat(paste0(type_str, ' '))
#			cat(paste0('Solve path (', name_wt, ')'))
			cat(paste0(paste0(rep(indent, 4), collapse=''), 'Solve path ', name_wt, ''))
			cat('\n')
		}

		if(type_str == 'S-R*-S'){

			# NEED INPUT RESOLVE
		}

		# IDENTIFY J2 SET TRANSFORMED WITH JOINT 1 AND 3
		J2_J1_set <- which(body.conn[2, ] != body.num[2])
		J2_J3_set <- which(body.conn[2, ] == body.num[2])

		if(type_str %in% c('L-S*-S', 'S-S*-S')){

			# GET SPHERE RADIUS BY MEASURING DISTANCE BETWEEN JOINTS
			J3_sphere_r <- distPointToPoint(joint.coor[3, , 1], joint.coor[2, , J2_J3_set])
		}

		if(type_str %in% c('R-R*-R', 'S-S*-R', 'S-S*-S')){

			# GET POINT DISTANCE
			dist_J1_J2 <- distPointToPoint(joint.coor[2, , J2_J1_set], joint.coor[1, , 1])
		}

		if(type_str == 'S-S*-S'){

			#if(print.progress) print(input)

			# SET NULL TRANSFORMATION MATRICES
			tmat4 <- tmat5 <- tmat6 <- tmat7 <- diag(4)

			# DEFINE SPHERE OF POTENTIAL SOLUTIONS FROM J1
			sphere <- list('C'=joint.coor[3, , 1], 'R'=J3_sphere_r)
	
			# FIND CIRCLE ON SPHERE
			circle <- circleOnSphereFromPoint(sphere, d=dist_J1_J2, p=joint.coor[1, , 1])
			
			# IF NO CIRCLE
			if(is.null(circle)) return(list('body.tmat'=NULL,'joint.status'=NULL,'solution'=TRUE))

			# CHOOSE POINT ON CIRCLE
			J2_npos <- circlePoint(circle, T=input[[2]][iter, 1])

			## FIND ROTATION OF SECOND BODY
			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			v_pre <- joint.coor[2, , J2_J3_set] - joint.coor[3, , 1]
			v_new <- J2_npos - joint.coor[3, , 1]

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat4[1:3, 4] <- joint.coor[3, , 1]

			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			tmat6[1:3, 1:3] <- tMatrixEP(cprod(v_pre, v_new), -avec(v_pre, v_new))

			# CHECK FOR ROTATION BETWEEN S-JOINTS
			#if(!is.na(input[[2]][1]) || !is.na(input[[3]][1])){
			
			#	if(!is.na(input[[2]][1])) angle <- input[[2]][iter, 1]
			#	if(!is.na(input[[3]][1])) angle <- input[[3]][iter, 1]
				
			#	tmat5[1:3, 1:3] <- tMatrixEP(J2_npos - joint.coor[3, , 1], angle)
			#}
			if(!is.na(input[[3]][1])) tmat5[1:3, 1:3] <- tMatrixEP(J2_npos - joint.coor[3, , 1], angle <- input[[3]][iter, 1])

			# TRANSLATE TO CENTER OF ROTATION
			tmat7[1:3, 4] <- -joint.coor[3, , 1]

			# COMBINE TRANSFORMATIONS
			tmat_body2 <- tmat4 %*% tmat5 %*% tmat6 %*% tmat7
		}


		if(type_str %in% c('S-S*-R', 'R-R*-R')){

			## FIND JOINT POSITION THAT SOLVES CONSTRAINT
			# DEFINE CIRCLE FOR OUTPUT LINK (NEED TO REDEFINE CENTER BECAUSE CIRCLE CENTER MAY NOT BE SAME AS JOINT)
			output_circle <- defineCircle(center=joint.coor[3, , 1], nvector=joint.cons[[3]][, , iter, 1], 
				point_on_radius=joint.coor[2, , J2_J3_set], redefine_center=TRUE)

			# FIND ANGLE ON CIRCLE AT DISTANCE FROM TRANSMISSION LINK JOINT
			output_link_t <- angleOnCircleFromPoint(circle=output_circle, dist=dist_J1_J2, 
				P=joint.coor[1, , 1], point_compare=joint.prev[2, ])

			# FIND CORRESPONDING POINT ON CIRCLE
			J2_npos <- circlePoint(circle=output_circle, T=output_link_t)
			
			# If no solution, return NULL
			if(is.vector(J2_npos)){
				if(is.na(J2_npos[1])) return(list('body.tmat'=NULL,'joint.status'=NULL,'solution'=FALSE))
			}else{
				if(is.na(J2_npos[1,1])) return(list('body.tmat'=NULL,'joint.status'=NULL,'solution'=FALSE))
			}
		}

		if(type_str %in% c('S-S*-R', 'S-S*-S')){
		
			# SET NULL TRANSFORMATION MATRICES
			tmat1 <- tmat2 <- tmat3 <- tmat4 <- diag(4)
		
			## FIND TRANSFORMATION OF FIRST BODY
			# ROTATE COUPLER LINK TO MATCH VECTOR BETWEEN UNTRANSFORMED AND TRANSFORMED JOINT
			v_pre <- joint.coor[2, , J2_J1_set] - joint.coor[1, , 1]
			v_new <- J2_npos - joint.coor[1, , 1]

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat1[1:3, 4] <- joint.coor[1, , 1]

			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			tmat3[1:3, 1:3] <- tMatrixEP(cprod(v_pre, v_new), -avec(v_pre, v_new))

			# CHECK FOR ROTATION BETWEEN S-JOINTS
			#if(!is.na(input[[1]][1]) || !is.na(input[[2]][1])){
			
			#	if(!is.na(input[[1]][1])) angle <- input[[1]][iter, 1]
			#	if(!is.na(input[[2]][1])) angle <- input[[2]][iter, 1]
				
			#	tmat2[1:3, 1:3] <- tMatrixEP(J2_npos - joint.coor[1, , 1], angle)
			#}
			if(!is.na(input[[1]][1])) tmat2[1:3, 1:3] <- tMatrixEP(J2_npos - joint.coor[1, , 1], input[[1]][iter, 1])

			# TRANSLATE TO CENTER OF ROTATION
			tmat4[1:3, 4] <- -joint.coor[1, , 1]

			# COMBINE TRANSFORMATIONS
			tmat_body1 <- tmat1 %*% tmat2 %*% tmat3 %*% tmat4
		}

		if(type_str %in% c('S-S*-R', 'R-R*-R')){

			# SET NULL TRANSFORMATION MATRICES
			tmat4 <- tmat5 <- tmat6 <- diag(4)

			## FIND ROTATION OF SECOND BODY
			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			v_pre <- joint.coor[2, , J2_J3_set] - joint.coor[3, , 1]
			v_new <- J2_npos - joint.coor[3, , 1]

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat4[1:3, 4] <- joint.coor[3, , 1]

			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			tmat5[1:3, 1:3] <- tMatrixEP(joint.cons[[3]][, , iter, 1], avec(v_pre, v_new, axis=joint.cons[[3]][, , iter, 1], about.axis=TRUE))

			# TRANSLATE TO CENTER OF ROTATION
			tmat6[1:3, 4] <- -joint.coor[3, , 1]

			# COMBINE TRANSFORMATIONS
			tmat_body2 <- tmat4 %*% tmat5 %*% tmat6
		}

		if(type_str == 'R-R*-R'){
			
			# SET NULL TRANSFORMATION MATRICES
			tmat1 <- tmat2 <- tmat3 <- diag(4)
			
			## FIND ROTATION OF FIRST BODY
			v_pre <- joint.coor[2, , J2_J1_set] - joint.coor[1, , 1]
			v_new <- J2_npos - joint.coor[1, , 1]

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat1[1:3, 4] <- joint.coor[1, , 1]

			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			tmat2[1:3, 1:3] <- tMatrixEP(joint.cons[[1]][, , iter, 1], avec(v_pre, v_new, axis=joint.cons[[1]][, , iter, 1], about.axis=TRUE))

			# TRANSLATE TO CENTER OF ROTATION
			tmat3[1:3, 4] <- -joint.coor[1, , 1]

			# COMBINE TRANSFORMATIONS
			tmat_body1 <- tmat1 %*% tmat2 %*% tmat3
		}

		if(type_str == 'L-S*-S'){
		
			# SET NULL TRANSFORMATION MATRICES
			tmat1 <- tmat2 <- tmat3 <- tmat4 <- diag(4)
			
			#if(print.progress) print(joint.status)
		
			## SLIDE S TO ALIGN L- AND S-JOINTS: INTERSECTION OF SPHERE AND LINE

			# FIND INTERSECTION OF SPHERE AND LINE
			J2_new <- intersectSphereLine(c=joint.coor[3, , 1], 
				r=J3_sphere_r, x=joint.coor[2, , J2_J1_set], l=joint.cons[[1]][, , iter, 1], 
				point.compare=joint.prev[2, ])

			# FIND TRANSLATION ALONG L-JOINT CONSTRAINT VECTOR
			tmat1[1:3, 4] <- J2_new - joint.coor[2, , J2_J1_set]

			# BODY1 TRANSFORMATION
			tmat_body1 <- tmat1
			
			# TRANSLATE J3 TO ORIGIN
			tmat2[1:3, 4] <- -joint.coor[3, , 1]

			# FIND ROTATION
			v_pre <- joint.coor[2, , 1] - joint.coor[3, , 1]
			v_new <- J2_new - joint.coor[3, , 1]
			rm_aor <- cprod(v_pre, v_new)
			rm_mag <- avec(v_pre, v_new)

			# FIND ROTATION ABOUT J3
			tmat3[1:3, 1:3] <- tMatrixEP(rm_aor, -rm_mag)

			# TRANSLATE J2 TO NEW POSITION
			tmat4[1:3, 4] <- joint.coor[3, , 1]

			# FIND TRANSFORMATION OF BODY2
			tmat_body2 <- tmat4 %*% tmat3 %*% tmat2
		}

		if(type_str %in% c('L-S*-S', 'S-S*-R', 'S-S*-S', 'R-R*-R')){
			
			# UPDATE JOINT STATUS
			joint.status[2, ] <- 's'

			return(list('body.tmat'=list(tmat_body1, tmat_body2), 'joint.status'=joint.status))
		}

		if(type_str == 'S-S-S-S*'){
			
			# IDENFITY S-JOINT ATTACHED TO SECOND BODY (BRIDGE JOINT)
			# TRANSFORM FIRST BODY ABOUT NON-BRIDGE JOINTS SO THAT BRIDGE JOINT IS AT 
			# DISTANCE EQUAL TO LENGTH OF SECOND BODY
			# ALSO ROTATE SECOND BODY ABOUT BRIDGE JOINT TOO
		}
	}

	return(list('body.tmat'=NULL,'joint.status'=NULL,'solution'=FALSE))
}