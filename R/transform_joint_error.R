transform_joint_error <- function(p, joint.idx, fit.points, mechanism, input.param, 
	fit.wts, joint.compare = NULL, print.progress = FALSE){

	# Create test transformation matrix from p
	tmat <- diag(4)
	tmat[1:3, 1:3] <- rotationMatrixZYX(p[1:3])
	tmat[1:3, 4] <- p[4:6]

	# Apply transform to joint coordinate
	mechanism$joint.coor[joint.idx, ] <- applyTransform(mechanism$joint.coor[joint.idx, ], tmat)
	
	# Apply transform to joint constraint
	mechanism$joint.cons[[joint.idx]] <- applyTransform(mechanism$joint.cons[[joint.idx]], tmat)
	
	# Run mechanism model
	anim_mech <- suppressWarnings(animateMechanism(mechanism, input.param=input.param, 
		joint.compare=joint.compare, use.ref.as.prev=use.ref.as.prev, 
		print.progress=print.progress, check.inter.joint.dist=FALSE, check.joint.cons=FALSE))

	# Compare simulated coordinates to reference
	if(dim(anim_mech$body.points.anim)[3] == 1){
		return_error <- sqrt(mean(fit.wts*(anim_mech$body.points.anim[, , 1] - fit.points)^2))
	}else{
		return_error <- sqrt(mean(fit.wts*(anim_mech$body.points.anim - fit.points)^2))
	}

	# Add NA penalty
	#if(sum(is.na(anim_mech$joint.coor)) > 0){cat(TRUE, '')}else{cat(FALSE, '')}
	return_error <- return_error + 0.2*(sum(is.na(anim_mech$joint.coor)) / 3)

	return(return_error)
}