solveKinematicChain <- function(joint.types = NULL, joints.unknown = NULL, joint.coor = NULL, 
	joint.cons = NULL, joints.dist = NULL, joints.prev = NULL, joint.init = NULL, 
	query = NULL, reverse = FALSE, print.progress = FALSE){

	if(!is.null(joint.coor) && is.null(joints.unknown)) stop("If 'joint.coor' is non-NULL, 'joints.unknown' must also be non-NULL.")

	# ALLOWED CHAINS
	type_str_ok <- type_str_rev_ok <- c('RrSpS', 'LtSpS', 'SSpPpSpS', 'SRrPpSpS')

	# ADD REVERSE ORDER CHAINS
	for(i in 1:length(type_str_ok)) type_str_rev_ok <- c(type_str_rev_ok, paste(rev(strsplit(type_str_ok[i], split="")[[1]]), collapse=''))
	type_str_rev_ok <- unique(type_str_rev_ok)

	# REMOVE KNOWN/UNKNOWN NUMERIC DESIGNATION
	type_str_rev_upper_ok <- unique(gsub('[a-z]', '', type_str_rev_ok))
	
	# RETURN QUERY
	if(!is.null(query)){
		if(query == 'max path length') return(max(nchar(gsub('[a-z]','',type_str_ok))))
	}

	if(reverse){
		joint.types <- joint.types[length(joint.types):1]
		joints.unknown <- joints.unknown[length(joints.unknown):1]
		joint.coor <- joint.coor[nrow(joint.coor):1, ]
		joint.cons <- joint.cons[length(joint.cons):1]
		joints.dist <- joints.dist[length(joints.dist):1]
		joints.prev <- joints.prev[nrow(joint.coor):1, ]
		joint.init <- joint.init[nrow(joint.init):1, ]
	}

	if(!is.null(joint.types)){

		# CONVERT TYPE SEQUENCE TO STRING IF VECTOR
		type_str <- paste(joint.types, collapse="")

		# IF NO COORDINATES PROVIDED, RETURN WHETHER CHAIN IS ALLOWED
		if(is.null(joint.coor) && is.null(joints.unknown)) if(type_str %in% type_str_rev_upper_ok){return(1)}else{return(0)}

		if(!is.null(joint.coor)){
			
			# ADD KNOWN/UKNOWN TO TYPE STRING
			type_str <- paste(joint.types, joints.unknown, collapse='', sep='')

			# EMPTY RETURN LIST
			r_list <- list()
			
			# REVERSE INPUTS
			if(type_str %in% c('SSpRr', 'SSpLt', 'SSpPtRrpS')){
				return(solveKinematicChain(joint.types=joint.types, joints.unknown=joints.unknown, joint.coor=joint.coor, 
					joint.cons=joint.cons, joints.dist=joints.dist, joints.prev=joints.prev, joint.init=joint.init, reverse=TRUE, 
					print.progress=print.progress))
			}

			if(type_str == 'SRrpPtSpS'){

				if(print.progress) cat(paste0('\t\t\tSolve kinematic chain for ', type_str, '\n'))
				
				## MAKE SURE R-JOINT VECTOR IS ORTHOGONAL TO ADJOINING LINKS

				# DEFINE PLANE
				plane <- list('n'=joint.cons[[3]],'p'=joint.init[3, ])
	
				# SET PROJECTED POINTS
				j_prev_proj <- rbind(joints.prev[c(1,2), ], pointPlaneProj(q=joints.prev[2, ], p=plane$p, n=plane$n), 
					pointPlaneProj(q=joints.prev[4, ], p=plane$p, n=plane$n), joints.prev[c(4,5), ])
				j_init_proj <- rbind(joint.init[c(1,2), ], pointPlaneProj(q=joint.init[2, ], p=plane$p, n=plane$n), 
					pointPlaneProj(q=joint.init[4, ], p=plane$p, n=plane$n), joint.init[c(4,5), ])
	
				# INTERJOINT DISTANCE
				j_init_proj_dist <- distPointToPoint(j_init_proj)

				# CREATE MATRIX FOR NEW POINTS
				j_proj <- j_init_proj*NA
				j_proj[c(1,6),] <- joint.coor[c(1,5),]

				# DEFINE CIRCLE TO SOLVE FOR ROTATION OF RPPS LINK
				j1_proj <- pointPlaneProj(q=joint.coor[1,], p=plane$p, n=plane$n)
				j1_prev_proj <- pointPlaneProj(q=joint.init[1,], p=plane$p, n=plane$n)
				rotation_circle <- defineCircle(center=j1_proj, nvector=plane$n, radius=distPointToPoint(j1_prev_proj, j_init_proj[4,]))

				# FIND P2 CIRCLE PROJECTED INTO PLANE
				j5_proj <- pointPlaneProj(q=joint.coor[5,], p=plane$p, n=plane$n)
				tri_a <- abs(distPointToPoint(j5_proj, joint.coor[5,])-j_init_proj_dist[4])
				j4_circle_r <- sqrt(j_init_proj_dist[5]^2 - tri_a^2)
				j4_circle_plane <- defineCircle(center=j5_proj, nvector=plane$n, radius=j4_circle_r)

				# FIND INTERSECTION CIRCLES
				intersect_circles <- intersectCircles(j4_circle_plane, rotation_circle)
	
				# FIND POINT CLOSEST TO PREVIOUS POINT
				if(length(intersect_circles) == 3){
					dist_pp <- c(distPointToPoint(intersect_circles[[1]], j_prev_proj[4,]), distPointToPoint(intersect_circles[[2]], j_prev_proj[4,]))
					j_proj[4,] <- intersect_circles[[which.min(dist_pp)]]
				}else if(length(intersect_circles) == 1){
					j_proj[4,] <- intersect_circles[[1]]
				}

				# FIND RADIUS OF P1 CIRCLE PROJECTED INTO PLANE
				tri_a <- abs(distPointToPoint(j1_proj, joint.coor[1, ])-j_init_proj_dist[2])
				j1_circle_r <- sqrt(j_init_proj_dist[1]^2 - tri_a^2)
	
				# FIND PROJECTED CIRCLE OF POSSIBLE P1 POSITIONS IN PLANE
				j1_circle_plane <- defineCircle(center=j1_proj, nvector=plane$n, radius=j1_circle_r)

				# P3 POSITION ON CIRCLE AT GIVEN DISTANCE FROM P4
				P3 <- circlePoint(j1_circle_plane, angleOnCircleFromPoint(circle=j1_circle_plane, 
					dist=j_init_proj_dist[3], P=j_proj[4,]))

				# CHOOSE POINT CLOSEST TO PREVIOUS POINT
				if(is.matrix(P3)){
					j_proj[3,] <- P3[which.min(distPointToPoint(P3,j_prev_proj[3,])), ]
				}else{
					j_proj[3,] <- P3
				}

				# FIND J2 AND J5
				j_proj[2,] <- j_proj[3,] + j_init_proj_dist[2]*uvector(joint.coor[1,]-j1_proj)
				j_proj[5,] <- j_proj[4,] + j_init_proj_dist[4]*uvector(joint.coor[5,]-j5_proj)
				
				# FIND TRANSFORMATIONS
				translate <- j_proj[2,] - joint.coor[2, ]
				t_points <- joint.coor[2:4, ] + matrix(translate, nrow=3, ncol=3, byrow=TRUE)

				# FIND ROTATION ANGLE OF RPPS LINK
				pnl <- pointNormalOnLine(t_points[3, ], t_points[1, ], j_proj[3,])
				r_angle <- avec(j_proj[5,] - pnl, t_points[3, ] - pnl)

				# ROTATE RPPS LINK
				r_points <- rotateBody(m=t_points, p=pnl, v=plane$n, a=r_angle)
				if(sum(abs(r_points[3, ] - j_proj[5,])) > 1e-5){
					r_points <- rotateBody(m=t_points, p=pnl, v=plane$n, a=-r_angle)
					r_angle <- -r_angle
				}

				#print(joint.coor)
				#print(r_points)
				#print(j_proj)

				r_list <- list(
					list('r'=0, 'p'=0),
					list('t'=translate, 'r'=r_angle, 'p'=pnl),
					list('p'=0)
				)
			}
			
			if(type_str == 'RrSpS'){

				if(print.progress) cat(paste0('\t\t\tSolve kinematic chain for ', type_str, '\n'))

				# DEFINE CIRCLE FOR OUTPUT LINK
				output_circle <- defineCircle(center=joint.coor[1, ], nvector=joint.cons[[1]], 
					point_on_radius=joint.coor[2, ])

				# FIND ANGLE ON CIRCLE AT DISTANCE FROM TRANSMISSION LINK JOINT
				output_link_t <- angleOnCircleFromPoint(circle=output_circle, dist=joints.dist[2], 
					P=joint.coor[3, ], point_compare=joints.prev[2, ])

				# FIND CORRESPONDING POINT ON CIRCLE
				output_joint_r <- circlePoint(circle=output_circle, T=output_link_t)

				# FIND ROTATION ANGLE FOR OUTLINK
				r_transform <- avec(joint.coor[2, ] - output_circle$C, output_joint_r - output_circle$C)

				# ROTATE TRANSMISSION LINK-OUTPUT JOINT
				joint_npos <- rotateBody(m=joint.coor[2, ], p=output_circle$C, v=joint.cons[[1]], 
					a=r_transform)

				# CHECK THAT ROTATION WAS IN THE RIGHT DIRECTION
				if(abs(distPointToPoint(joint.coor[3, ], joint_npos) - joints.dist[2]) > 1e-4){
					r_transform <- -r_transform
					joint_npos <- rotateBody(m=joint.coor[2, ], p=output_circle$C, v=joint.cons[[1]], 
						a=r_transform)
				}

				r_list <- list(list('r'=r_transform), list('p'=joint_npos-joint.coor[2, ]))
			}
			
			if(type_str == 'LtSpS'){

				if(print.progress) cat(paste0('\t\t\tSolve kinematic chain for ', type_str, '\n'))

				# FIND POSITION OF SLIDING JOINT
				joint_npos <- intersectSphereLine(c=joint.coor[3, ], r=joints.dist[2], 
					x=joint.coor[2, ], l=joint.cons[[1]], point.compare=joints.prev[2, ])

				r_list <- list(list('t'=joint_npos-joint.coor[2, ]), list('p'=joint_npos-joint.coor[2, ]))
			}

			if(type_str == 'SSpPtSpS'){
				
				if(print.progress) cat(paste0('\t\t\tSolve kinematic chain for ', type_str, '\n'))

				# TEST IF ALL POINTS ARE COINCIDENT
				#centroid_size <- sum(abs(joint.coor[2:4, ] - matrix(colMeans(joint.coor[2:4, ]), nrow=3, ncol=3, byrow=TRUE)))

				# MAKE SURE THAT MIDDLE POINTS ARE COLINEAR
				if(sum(abs(joint.coor[3, ]-joint.coor[4, ])) > 0 && distPointToLine(pt=joint.coor[2, ], l1=joint.coor[3, ], l2=joint.coor[4, ]) > 1e-10){
					stop("Currently linkR only determines the joint positions of an S-P-S chain if all three joints are collinear. One or more of the joints in an S-P-S are not collinear.")
					return(NULL)
				}

				# MAKE SURE THAT MIDDLE POINTS ARE COLINEAR
				if(sum(abs(joint.coor[2, ]-joint.coor[4, ])) > 0 && distPointToLine(pt=joint.coor[3, ], l1=joint.coor[2, ], l2=joint.coor[4, ]) > 1e-10){
					stop("Currently linkR only determines the joint positions of an S-P-S chain if all three joints are collinear. One or more of the joints in an S-P-S are not collinear.")
					return(NULL)
				}

				# MAKE SURE THAT LINE IS PARALLEL TO THE NORMAL VECTOR OF THE PLANE
				if(sum(abs(joint.coor[3, ]-joint.coor[2, ])) > 0 && avec(joint.coor[3, ]-joint.coor[2, ], joint.cons[[3]]) > 1e-10){
					stop("Currently linkR only determines the joint positions of an S-P-S chain if the line described by the three joints is parallel to the normal vector of the plane. The line is not parallel to the plane's normal vector.")
					return(NULL)
				}

				# MAKE SURE THAT LINE IS PARALLEL TO THE NORMAL VECTOR OF THE PLANE
				if(sum(abs(joint.coor[3, ]-joint.coor[4, ])) > 0 && avec(joint.coor[3, ]-joint.coor[4, ], joint.cons[[3]]) > 1e-10){
					stop("Currently linkR only determines the joint positions of an S-P-S chain if the line described by the three joints is parallel to the normal vector of the plane. The line is not parallel to the plane's normal vector.")
					return(NULL)
				}

				# MAKE SURE THAT LINE IS PARALLEL TO THE NORMAL VECTOR OF THE PLANE
				if(sum(abs(joint.coor[2, ]-joint.coor[4, ])) > 0 && avec(joint.coor[2, ]-joint.coor[4, ], joint.cons[[3]]) > 1e-10){
					stop("Currently linkR only determines the joint positions of an S-P-S chain if the line described by the three joints is parallel to the normal vector of the plane. The line is not parallel to the plane's normal vector.")
					return(NULL)
				}
				
				# SOLVE FOR POINT POSITION IN PLANE
				joint_npos <- pointsInPlaneFromPoints(p=joint.coor, d=joints.dist, n=joint.cons[[3]],
					compare=joints.prev)

				if(is.null(joint_npos)) stop("Circles in plane are non-coincident")
				r_list <- list(
					list('p'=joint_npos[1, ] - joint.coor[2, ]),
					list('t'=joint_npos[2, ] - joint.coor[3, ]), 
					list('p'=joint_npos[3, ] - joint.coor[4, ]))
			}

			if(length(r_list) > 0 && reverse){
				return(r_list[length(r_list):1])
			}
			if(length(r_list) > 0){
				return(r_list)
			}
		}
		
		return(NULL)
	}
}