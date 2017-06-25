solveJointPath <- function(joint.types, joint.status, joint.coor, joint.cons, 
	body.num, input, body.conn, 
	joint.names, joint.prev, joint.ref, iter = 1, 
	print.progress = FALSE, indent = ''){

	# INPUT
	if(length(joint.types) == 1){

		# SET NULL TRANSFORMATION MATRICES
		tmat1 <- tmat2 <- tmat3 <- diag(4)

		# GET JOINT SET TO TRANSFORM
		jt_set <- which(body.num == body.conn)
		
		# CREATE TRANSFORMATION MATRIX
		if(joint.types %in% c('S')){

			# SKIP IF INPUTS ARE NA (ALLOWS INPUT RESOLVE PARAMETERS TO BE ADDED IN ADDITION TO INPUT PARAMETERS)
			if(sum(is.na(input[iter, 1:3])) == 3) return(NULL)

			if(print.progress){
				AOR <- paste0(paste0(round(joint.cons[[1]][1, , iter, jt_set], 3), collapse=','), ' ', paste0(round(joint.cons[[1]][2, , iter, jt_set], 3), collapse=','), ' ', paste0(round(joint.cons[[1]][3, , iter, jt_set], 3), collapse=','))
				cat(paste0('{CoR:', paste0(round(joint.coor[, jt_set], 3), collapse=','), '; AoR:', AOR, '; Angle:', round(input[iter, 1], 3),'}\n'))
			}

			# TRANSLATE TO CENTER OF ROTATION (JOINT)
			tmat1[1:3, 4] <- joint.coor[, jt_set]

			# LOOP THROUGH EACH COLUMNN OF INPUT PARAMETERS
			for(i in 1:nrow(joint.cons[[1]][, , iter, jt_set])){
			
				# SKIP IF NA
				if(is.na(input[iter, i])) next

				# APPLY ROTATION ABOUT SINGLE JOINT CONSTRAINT VECTOR
				tmat2[1:3, 1:3] <- tmat2[1:3, 1:3] %*% tMatrixEP(joint.cons[[1]][i, , iter, jt_set], -input[iter, i])
			}

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat3[1:3, 4] <- -joint.coor[, jt_set]
			
		}else if(joint.types == 'R'){
			
			if(print.progress) cat(paste0('{CoR:', paste0(round(joint.coor[, jt_set], 3), collapse=','), '; AoR:', paste0(round(joint.cons[[1]][1, , iter, jt_set], 3), collapse=','), '; Angle:', round(input[iter, 1], 3),'}\n'))

			# TRANSLATE TO CENTER OF ROTATION (JOINT)
			tmat1[1:3, 4] <- joint.coor[, jt_set]

			# APPLY ROTATION TO TRANSFORMATION MATRIX
			tmat2[1:3, 1:3] <- tMatrixEP(joint.cons[[1]][, , iter, jt_set], -input[iter, 1])

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat3[1:3, 4] <- -joint.coor[, jt_set]
			
		}else if(joint.types == 'P'){

			if(print.progress) cat('\n')

			# TRANSLATE TO CENTER OF ROTATION (JOINT)
			tmat1[1:3, 4] <- colSums(input[iter, ] * joint.cons[[1]][, , iter, jt_set])

		}else if(joint.types == 'L'){

			if(print.progress) cat('\n')

			# TRANSLATE TO CENTER OF ROTATION (JOINT)
			tmat1[1:3, 4] <- input[iter, 1]*joint.cons[[1]][, , iter, jt_set]
		}
		
		# CHANGE JOINT STATUS
		joint.status[jt_set] <- 'i'

		# COMBINE TRANSFORMATION MATRICES
		tmat <- tmat1 %*% tmat2 %*% tmat3
		
		return(list('body.tmat'=list(tmat), 'joint.status'=joint.status))

	# SOLVE
	}else{

		# SET NULL TRANSFORMATION MATRICES
		tmat1 <- tmat2 <- tmat3 <- tmat4 <- tmat5 <- tmat6 <- diag(4)

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
			
			if(is.null(solve_joint_path)) return(NULL)

			return(list(
				'body.tmat'=solve_joint_path$body.tmat[length(solve_joint_path$body.tmat):1],
				'joint.status'=solve_joint_path$joint.status[nrow(solve_joint_path$joint.status):1, ]
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

			# DEFINE SPHERE OF POTENTIAL SOLUTIONS FROM J1
			sphere <- list('C'=joint.coor[3, , 1], 'R'=J3_sphere_r)
	
			# FIND CIRCLE ON SPHERE
			circle <- circleOnSphereFromPoint(sphere, d=dist_J1_J2, p=joint.coor[1, , 1])
			
			# CHOOSE POINT ON CIRCLE
			J2_npos <- circlePoint(circle, T=input[[2]][iter, 4])


			## FIND ROTATION OF SECOND BODY
			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			v_pre <- joint.coor[2, , J2_J3_set] - joint.coor[3, , 1]
			v_new <- J2_npos - joint.coor[3, , 1]

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat4[1:3, 4] <- joint.coor[3, , 1]

			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			tmat5[1:3, 1:3] <- tMatrixEP(cprod(v_pre, v_new), -avec(v_pre, v_new))

			# TRANSLATE TO CENTER OF ROTATION
			tmat6[1:3, 4] <- -joint.coor[3, , 1]

			# COMBINE TRANSFORMATIONS
			tmat_body2 <- tmat4 %*% tmat5 %*% tmat6
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
				if(is.na(J2_npos[1])) return(NULL)
			}else{
				if(is.na(J2_npos[1,1])) return(NULL)
			}
		}

		if(type_str %in% c('S-S*-R', 'S-S*-S')){

			## FIND TRANSFORMATION OF FIRST BODY
			# ROTATE COUPLER LINK TO MATCH VECTOR BETWEEN UNTRANSFORMED AND TRANSFORMED JOINT
			v_pre <- joint.coor[2, , J2_J1_set] - joint.coor[1, , 1]
			v_new <- J2_npos - joint.coor[1, , 1]

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat1[1:3, 4] <- joint.coor[1, , 1]

			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			tmat2[1:3, 1:3] <- tMatrixEP(cprod(v_pre, v_new), -avec(v_pre, v_new))

			# TRANSLATE TO CENTER OF ROTATION
			tmat3[1:3, 4] <- -joint.coor[1, , 1]

			# COMBINE TRANSFORMATIONS
			tmat_body1 <- tmat1 %*% tmat2 %*% tmat3
		}

		if(type_str %in% c('S-S*-R', 'R-R*-R')){

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

	return(NULL)
}