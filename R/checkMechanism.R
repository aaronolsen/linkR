checkMechanism <- function(mechanism, check.inter.joint.dist, check.joint.cons,  
	check.inter.point.dist, print.progress, print.progress.iter){

	#print(names(mechanism))

	# Set tolerance for equivalencies based on machine precision
	tolerance = .Machine$double.eps^0.5

	# Set number of iterations
	n_iter <- dim(mechanism$joint.coor.anim)[3]

	## For each body check distances among joints
	if(check.inter.joint.dist){

		#print(mechanism$joint.coor.anim)

		# For each body
		for(i in 1:length(mechanism$body.joints)){

			# Get joints associated with body
			body_joints <- mechanism$body.joints[[i]]
			
			# Get joint types
			joint_types <- mechanism$joint.types[body_joints]
			
			# Exclude translational joints
			body_joints <- body_joints[!joint_types %in% c('L', 'P')]
			
			# Skip if less than 2 joints
			if(length(body_joints) < 2) next

			# Get all pairs of joints
			pair_idx <- generatePairs(1:length(body_joints))
			
			# Create matrix for distances
			dist_mat <- matrix(NA, nrow=n_iter, ncol=nrow(pair_idx), dimnames=list(NULL, 1:nrow(pair_idx)))

			for(prow in 1:nrow(pair_idx)){
				
				#colnames(dist_mat)[prow] <- paste0(mechanism$joint.names[pair_idx[prow,]], collapse='-')
				colnames(dist_mat)[prow] <- paste0(pair_idx[prow,], collapse='-')

				# For each iteration
				for(iter in 1:n_iter){

					# Get distance
					dist_mat[iter, prow] <- distPointToPoint(mechanism$joint.coor.anim[body_joints[pair_idx[prow,]],, iter, 1])
				}
			}
			
			# Get SD of each column
			col_sds <- apply(dist_mat, 2, 'sd')
			print(dist_mat)
			print(col_sds)
			
			# Check whether any SDs are above zero
			cat('\n')
			
			
			#break
		}
	}

#	 [1] "joint.coor"          "joint.cons"          "joint.types"         "input.joint"         
#	"input.dof"           "input.body"          "paths.closed"        "paths.closed.dist"  
#	 [9] "paths.closed.bodies" "paths.closed.set"    "joint.transform"     "joint.set.transform" 
#	"joint.names"         "joint.conn"          "body.conn"           "body.conn.num"      
#	[17] "body.joints"         "body.names"          "fixed.body"          "fixed.joints"        
#	"open.joints"         "num.paths.closed"    "num.joints"          "num.bodies"         
#	[25] "body.points"         "points.assoc"        "body.assoc"          "points.connect"      
#	"num.iter"            "joint.cons.anim"     "joint.coor.anim"     "tmat"               
#	[33] "status"              "paths.solved"

}