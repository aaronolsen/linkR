solveJointPath <- function(joint.types, joint.change, joint.coor, joint.cons, joint.names, 
	joint.dist, joint.prev, joint.init, input.resolve = NULL, iter = 1, print.progress = FALSE, 
	indent = ''){

	# CREATE STRING WITH JOINT TYPES AND TRANSFORM INDICATOR
	type_vec <- joint.types
	trfm_vec <- rep('*', length(type_vec))
	trfm_vec[!joint.change] <- ''
	type_str <- paste0(paste0(type_vec, trfm_vec), collapse='-')

	# REVERSE INPUTS
	if(type_str %in% c('R*-R-L', 'S*-S-R', 'R*-R-R', 'S*-S-S')){

		solve_joint_path <- solveJointPath(
			joint.types=joint.types[length(joint.types):1], 
			joint.change=joint.change[length(joint.change):1], 
			joint.coor=joint.coor[nrow(joint.coor):1, ], 
			joint.cons=joint.cons[length(joint.cons):1], 
			joint.names=joint.names[length(joint.cons):1], 
			joint.dist=joint.dist[length(joint.dist):1], 
			joint.prev=joint.prev[nrow(joint.coor):1, ], 
			joint.init=joint.init[nrow(joint.init):1, ], 
			input.resolve=input.resolve[length(input.resolve):1], 
			iter=iter, print.progress=print.progress, indent=indent)

		return(list(
			'joint.tmat'=solve_joint_path$joint.tmat[length(solve_joint_path$joint.tmat):1], 
			'body.tmat'=solve_joint_path$body.tmat[length(solve_joint_path$body.tmat):1]
			))
	}

	# PRINT PATH DETAILS
	if(print.progress){
		name_vec <- joint.names
		name_wt <- paste0(paste0(name_vec, trfm_vec), collapse='-')
		cat(paste0(type_str, ' '))
		cat(paste0('(', name_wt, ')'))
		cat('\n')
	}

	# SET NULL TRANSFORMATION MATRICES
	tmat1 <- tmat2 <- tmat3 <- tmat4 <- diag(4)

	# SET PREVIOUS ITERATION
	if(iter == 1){prev_iter <- 1}else{prev_iter <- iter - 1}

	# 2D COUPLED SLIDERS
	if(type_str == 'L-R-R*'){
	
		## FIND INTERSECTION OF CIRCLE AND LINE
		# DEFINE CIRCLE FOR R*-JOINT
		R_circle <- defineCircle(center=joint.coor[3, ], nvector=joint.cons[[3]][, , iter], 
			radius=joint.dist[2])

		# FIND INTERSECTION OF CIRCLE AND LINE
		int_cir_line <- intersectCircleLine(R_circle, joint.coor[2, ], joint.coor[2, ]+joint.cons[[1]][, , iter])

		if(is.null(int_cir_line)){
			warning(paste0("No solution for ", type_str, " path at iteration ", iter, ""))
			return(NULL)
		}

		# SELECT BEST SOLUTION
		if(length(int_cir_line) == 1){
			R_joint_n <- int_cir_line$p1
		}else{

			# FIND DISTANCE FROM PREVIOUS POINT			
			d <- c(distPointToPoint(int_cir_line$p1, joint.coor[2, ]), distPointToPoint(int_cir_line$p2, joint.coor[2, ]))
			
			# FIND POINT CLOSEST TO PREVIOUS POINT
			R_joint_n <- int_cir_line[[which.min(d)]]
		}
		
		# SET TRANSLATION TRANSFORMATION MATRIX
		tmat1[1:3, 4] <- R_joint_n - joint.coor[2, ]
		tmat_L <- tmat1
		
		# FIND ROTATION
		r_transform <- avec(joint.coor[3, ] - R_joint_n, joint.init[3, ] - joint.coor[2, ])

		# FIND TRANSFORMATION MATRIX
		tmat1[1:3, 4] <- R_joint_n
		tmat2[1:3, 1:3] <- tMatrixEP(joint.cons[[2]][, , iter], r_transform)
		tmat3[1:3, 4] <- -R_joint_n
		tmat4[1:3, 4] <- R_joint_n - joint.coor[2, ]
		tmat_R <- tmat1 %*% tmat2 %*% tmat3 %*% tmat4

		# ROTATE TRANSMISSION LINK-OUTPUT JOINT
		joint_npos <- applyTransform(joint.init[3, ], tmat_R)
		
		# CHECK THAT ROTATION WAS IN THE RIGHT DIRECTION
		if(sum(abs(joint_npos - joint.coor[3, ])) > 1e-10){
			tmat2[1:3, 1:3] <- tMatrixEP(joint.cons[[2]][, , iter], -r_transform)
			tmat_R <- tmat1 %*% tmat2 %*% tmat3 %*% tmat4
		}
		
		# FIND TRANSFORMATION MATRIX FOR LINK ROTATION
		return(list('joint.tmat'=list(tmat_L, tmat_L, NA), 'body.tmat'=list(tmat_L, tmat_R)))

	# 3-DoF 4-BAR
	}else if(type_str %in% c('S-S-S*', 'R-R-R*', 'R-S-S*')){

		# FIND 3D CIRCLE OF POTENTIAL SOLUTIONS
		if(type_str == 'S-S-S*'){

			# USE JOINT CONSTRAINT VECTOR TO PROJECT POINT INTO CIRCLE PLANE, THEN 
			# SCALE TO RADIUS MAGNITUDE TO IDENTIFY SOLUTION
			# IF JOINT CONSTRAINT VECTOR IS PARALLEL TO CIRCLE NORMAL THEN USE THE SECOND JOINT 
			# CONSTRAINT VECTOR

			## FIND NEW JOINT2 POSITION
			# DEFINE SPHERE OF POTENTIAL SOLUTIONS FROM S*
			sphere <- list('C'=joint.coor[3, ], 'R'=joint.dist[2])
		
			# FIND CIRCLE ON SPHERE
			circle <- circleOnSphereFromPoint(sphere, d=joint.dist[1], p=joint.coor[1, ])

			# CHOOSE POINT ON CIRCLE
			joint2_npos <- circlePoint(circle, T=input.resolve[[1]][iter, 1])


			## FIND TRANSFORMATION OF FIRST BODY
			# ROTATE COUPLER LINK TO MATCH VECTOR BETWEEN UNTRANSFORMED AND TRANSFORMED JOINT
			vi <- joint.coor[2, ]-joint.coor[1, ]
			vf <- joint2_npos-joint.coor[1, ]
		
			# GET ROTATIONAL AXIS BETWEEN TWO VECTORS
			rot_axis <- cprod(vi, vf)

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat1[1:3, 4] <- joint.coor[1, ]

			# ROTATION ABOUT AXIS BETWEEN S-JOINTS
			tmat2[1:3, 1:3] <- tMatrixEP(vf, input.resolve[[1]][iter, 2])

			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			tmat3[1:3, 1:3] <- tMatrixEP(rot_axis, avec(vi, vf, axis=rot_axis, about.axis=TRUE))

			# TRANSLATE TO CENTER OF ROTATION
			tmat4[1:3, 4] <- -joint.coor[1, ]

			# COMBINE TRANSFORMATIONS
			tmat_A <- tmat1 %*% tmat2 %*% tmat3 %*% tmat4

		}else if(type_str %in% c('R-R-R*', 'R-S-S*')){

			## FIND ROTATION OF FIRST LINK
			# DEFINE CIRCLE FOR OUTPUT LINK
			output_circle <- defineCircle(center=joint.coor[1, ], nvector=joint.cons[[1]][, , iter], 
				point_on_radius=joint.coor[2, ])

			# FIND ANGLE ON CIRCLE AT DISTANCE FROM TRANSMISSION LINK JOINT
			output_link_t <- angleOnCircleFromPoint(circle=output_circle, dist=joint.dist[2], 
				P=joint.coor[3, ], point_compare=joint.prev[2, ])

			# FIND CORRESPONDING POINT ON CIRCLE
			output_joint_r <- circlePoint(circle=output_circle, T=output_link_t)

			# FIND ROTATION ANGLE FOR OUTLINK
			r_transform <- avec(joint.coor[2, ] - output_circle$C, output_joint_r - output_circle$C)

			# FIND TRANSFORMATION MATRIX
			tmat1[1:3, 4] <- output_circle$C
			tmat2[1:3, 1:3] <- tMatrixEP(joint.cons[[1]][, , iter], -r_transform)
			tmat3[1:3, 4] <- -output_circle$C
			tmat_A <- tmat1 %*% tmat2 %*% tmat3

			# ROTATE TRANSMISSION LINK-OUTPUT JOINT
			joint2_npos <- applyTransform(joint.coor[2, ], tmat_A)
		
			# CHECK THAT ROTATION WAS IN THE RIGHT DIRECTION
			if(abs(distPointToPoint(joint.coor[3, ], joint2_npos) - joint.dist[2]) > 1e-4){
				tmat2[1:3, 1:3] <- tMatrixEP(joint.cons[[1]][, , iter], r_transform)
				tmat_A <- tmat1 %*% tmat2 %*% tmat3
				joint2_npos <- applyTransform(joint.coor[2, ], tmat_A)
			}

		}


		## FIND TRANSFORMATION OF COUPLER LINK
		# CLEAR TRANSFORMATION MATRICES
		tmat1 <- tmat2 <- tmat3 <- tmat4 <- diag(4)

		## FIND TRANSFORMATION OF SECOND BODY
		vi <- joint.init[3, ]-joint.init[2, ]
		vf <- joint.coor[3, ]-joint2_npos

		# CHECK FOR ZERO DIFFERENCE IN ROTATION
		if(sum(abs(vi-vf)) < 1e-7){
			tmat_B <- diag(4)
		}else{

			# GET ROTATIONAL AXIS BETWEEN TWO VECTORS
			rot_axis <- cprod(vi, vf)

			# TRANSLATE BACK FROM CENTER OF ROTATION
			tmat1[1:3, 4] <- joint2_npos

			# ROTATION ABOUT AXIS BETWEEN S-JOINTS
			tmat2[1:3, 1:3] <- diag(3)
			if(length(input.resolve[[2]]) > 1 && !is.null(input.resolve[[2]])) tmat2[1:3, 1:3] <- tMatrixEP(vf, input.resolve[[2]][iter, 1])
			if(length(input.resolve[[3]]) > 1 && !is.null(input.resolve[[3]])) tmat2[1:3, 1:3] <- tMatrixEP(vf, input.resolve[[3]][iter, 1])

			# ROTATION TO ALIGN WITH NEW JOINT POSITION
			tmat3[1:3, 1:3] <- tMatrixEP(rot_axis, avec(vi, vf, axis=rot_axis, about.axis=TRUE))

			# TRANSLATE TO CENTER OF ROTATION
			tmat4[1:3, 4] <- -joint.coor[2, ]

			# COMBINE TRANSFORMATIONS
			tmat_B <- tmat1 %*% tmat2 %*% tmat3 %*% tmat4
		}

		return(list('joint.tmat'=list(tmat_A, tmat_A, NA), 'body.tmat'=list(tmat_A, tmat_B)))

	# 3D 4-BAR
	}else if(type_str == 'R-U-U-U-U*-R*'){
		
		## I DONT THINK THIS WILL WORK IN MOST CASES -- OUTSIDE U-JOINT AXES MUST STAY PERPENDICULAR TO COUPLER LINK
		## AND THIS CANT HAPPEN UNLESS THE OUTSIDE U-JOINTS ARE PARALLEL TO THE R-JOINTS
		## SEEMS LIKE IT MIGHT ONLY WORK WITH PLANAR 4-BAR

		# OUTSIDE U-JOINT AXES MUST BE PARALLEL TO ONE ANOTHER AND PERPENDICULAR TO VECTOR BETWEEN U-JOINTS
		# INSIDE U-JOINT AXES MUST BE PERPENDICULAR TO OUTSIDE U-JOINT AXES (AND THEREFORE ALSO PARALLEL TO ONE ANOTHER)
		
		## FIND ROTATION OF R-LINK
		# DEFINE CIRCLE FOR OUTPUT LINK
		output_circle <- defineCircle(center=joint.coor[1, ], nvector=joint.cons[[1]][, , iter], 
			point_on_radius=joint.coor[2, ])

		# FIND ANGLE ON CIRCLE AT DISTANCE FROM TRANSMISSION LINK JOINT
		output_link_t <- angleOnCircleFromPoint(circle=output_circle, dist=joint.dist[3], 
			P=joint.coor[5, ], point_compare=joint.prev[2, ])

		# FIND CORRESPONDING POINT ON CIRCLE
		output_joint_r <- circlePoint(circle=output_circle, T=output_link_t)

		# FIND ROTATION ANGLE FOR OUTLINK
		r_transform <- avec(joint.coor[2, ] - output_circle$C, output_joint_r - output_circle$C)

		# FIND TRANSFORMATION MATRIX
		tmat1[1:3, 4] <- output_circle$C
		tmat2[1:3, 1:3] <- tMatrixEP(joint.cons[[1]][, , iter], -r_transform)
		tmat3[1:3, 4] <- -output_circle$C
		tmat_A <- tmat1 %*% tmat2 %*% tmat3

		# ROTATE TRANSMISSION LINK-OUTPUT JOINT
		joint_npos <- applyTransform(joint.coor[2, ], tmat_A)
		
		# CHECK THAT ROTATION WAS IN THE RIGHT DIRECTION
		if(abs(distPointToPoint(joint.coor[5, ], joint_npos) - joint.dist[3]) > 1e-4){
			tmat2[1:3, 1:3] <- tMatrixEP(joint.cons[[1]][, , iter], r_transform)
			tmat_A <- tmat1 %*% tmat2 %*% tmat3
			joint_npos <- applyTransform(joint.coor[2, ], tmat_A)
		}
		
		###### COULD POTENTIALLY RE-WRITE THIS SO THAT TRANSFORMATIONS ARE DESCRIBED BY 
		###### ROTATIONS ABOUT THE TWO U-AXES. MIGHT BE APPLICABLE TO BROADER JOINT PATHS

		## FIND TRANSFORMATION OF COUPLER LINK
		# CLEAR TRANSFORMATION MATRICES
		tmat1 <- tmat2 <- tmat3 <- diag(4)
		
		# CREATE NEW TMATS
		tmat5 <- tmat6a <- tmat6b <- tmat7 <- diag(4)

		# TRANSLATE LINK SO THAT ONE INSIDE U-JOINT ALIGNS WITH NEW JOINT POSITION (CENTER OF ROTATION)
		tmat4[1:3, 4] <- joint_npos - joint.coor[3, ]

		# FIND ROTATION ABOUT OUTSIDE U-JOINT AXIS TO

		# See http://inside.mines.edu/fs_home/gmurray/ArbitraryAxisRotation/ "The matrix for rotation about an arbitrary line"
		# for transformation matrix given CoR, AoR, and angle

		# TRANSLATE COUPLER U-JOINTS
		joint34_npos <- joint.coor[c(3,4), ] - rbind(joint.coor[3, ]-joint_npos, joint.coor[3, ]-joint_npos)
		
		# ROTATE COUPLER LINK TO MATCH VECTOR BETWEEN UNTRANSFORMED AND TRANSFORMED JOINT
		v1 <- joint34_npos[2, ]-joint_npos
		v2 <- joint.coor[5, ]-joint_npos
		tmat1[1:3, 4] <- joint_npos
		tmat2[1:3, 1:3] <- tMatrixEP(cprod(v1, v2), -avec(v1, v2))
		tmat3[1:3, 4] <- -joint_npos

		# FIND TRANSFORMATION MATRIX
		tmat_B <- tmat1 %*% tmat2 %*% tmat3 %*% tmat4


		## ROTATE ABOUT LINE BETWEEN U-JOINTS TO KEEP U-JOINT AXES PERPENDICULAR
		# TRANSFORM INSIDE U-JOINT CONSTRAINT
		joint_cons_point <- rbind(joint.coor[3, ], joint.coor[3, ]+joint.cons[[3]][, , iter])
		joint_cons_point <- applyTransform(joint_cons_point, tmat_B)
		joint_cons_t <- joint_cons_point[2, ]-joint_cons_point[1, ]

		# SET ROTATIONAL AXIS AS VECTOR BETWEEN U-JOINTS (LONG AXIS OF BODY)
		r_axis <- joint.coor[5, ]-joint_npos

		# SET VECTOR TO ALIGN INSIDE U-JOINT AXES WITH - PERPENDICULAR TO R AXIS AND OUTSIDE U-JOINT AXES
		vec_align <- cprod(r_axis, joint.cons[[2]][, , iter])

		# APPLY ROTATION ABOUT NEW OUTSIDE U-JOINT POSITION - TRY BOTH DIRECTIONS
		tmat5[1:3, 4] <- joint_npos
		tmat6a[1:3, 1:3] <- tMatrixEP(r_axis, avec(joint_cons_t, vec_align, axis=r_axis, about.axis=TRUE))
		tmat6b[1:3, 1:3] <- tMatrixEP(r_axis, avec(joint_cons_t, -vec_align, axis=r_axis, about.axis=TRUE))
		tmat7[1:3, 4] <- -joint_npos

		# GET TRANSFORMATION MATRIX
		tmat_Ca <- tmat5 %*% tmat6a %*% tmat7 %*% tmat_B
		tmat_Cb <- tmat5 %*% tmat6b %*% tmat7 %*% tmat_B

		# APPLY TRANSFORMATION TO CONSTRAINT VECTOR
		joint_cons_point <- rbind(joint.coor[3, ], joint.coor[3, ]+joint.cons[[3]][, , iter])
		joint_cons_pointa <- applyTransform(joint_cons_point, tmat_Ca)
		joint_cons_ta <- joint_cons_pointa[2, ]-joint_cons_pointa[1, ]
		joint_cons_pointb <- applyTransform(joint_cons_point, tmat_Cb)
		joint_cons_tb <- joint_cons_pointb[2, ]-joint_cons_pointb[1, ]

		# FIND TRANSFORMATION CLOSEST TO PREVIOUS CONTRAINT VECTOR
		da <- sum(abs(joint_cons_ta - joint.cons[[3]][, , prev_iter]))
		db <- sum(abs(joint_cons_tb - joint.cons[[3]][, , prev_iter]))
		
		# SET AS TRANSFORMATION
		if(da < db){ tmat_C <- tmat_Ca }else{ tmat_C <- tmat_Cb }

		# FIND TRANSFORMATION MATRIX FOR LINK ROTATION
		return(list('joint.tmat'=list(tmat_A, tmat_A, tmat_C, tmat_C, NA, NA), 'body.tmat'=list(tmat_A, NA, tmat_C, NA, NA)))
	}

	return(NULL)
}