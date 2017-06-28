mechanism_transform_error <- function(p, fit.points, mechanism, 
	input.param, input.joint, input.body, fit.wts, center = NULL){

	# Create test transformation matrix from p
	if(is.null(center)){
		tmat <- diag(4)
		tmat[1:3, 1:3] <- rotationMatrixZYX(p[1:3])
		tmat[1:3, 4] <- p[4:6]
	}else{
		if(nrow(center) == 1){
			tmat1 <- tmat2 <- tmat3 <- diag(4)
			tmat1[1:3, 4] <- center
			tmat2[1:3, 1:3] <- rotationMatrixZYX(p)
			tmat3[1:3, 4] <- -center
			tmat <- tmat1 %*% tmat2 %*% tmat3
		}else{

			tmat1 <- tmat2 <- tmat3 <- diag(4)
			tmat1[1:3, 4] <- center[1, ]
			tmat2[1:3, 1:3] <- tMatrixEP(center[4, ]-center[1, ], p)
			tmat3[1:3, 4] <- -center[1, ]
			tmat <- tmat1 %*% tmat2 %*% tmat3
		}
	}

	# Apply transformation to joint constraints
	for(i in 1:mechanism$num.joints){
		if(mechanism$joint.types[i] == 'R'){
			cons_vec <- rbind(mechanism$joint.coor[i,], mechanism$joint.coor[i,]+mechanism$joint.cons[[i]][, , 1])
			cons_vec_t <- applyTransform(cons_vec, tmat)
			mechanism$joint.cons[[i]][, , 1] <- cons_vec_t[2, ] - cons_vec_t[1, ]
		}
	}

	# Apply transformation to joint coordinates
	mechanism$joint.coor <- applyTransform(mechanism$joint.coor, tmat)	

	# Run mechanism model
	anim_mech <- suppressWarnings(animateMechanism(mechanism, input.param=input.param, 
		input.joint=input.joint, input.body=input.body, print.progress=FALSE, 
		check.inter.joint.dist=FALSE, check.joint.cons=FALSE))
	
	# Compare simulated coordinates to ideal
	if(dim(anim_mech$body.points)[3] == 1) return(sqrt(mean(fit.wts*(anim_mech$body.points[, , 1] - fit.points)^2)))
	return(sqrt(mean(fit.wts*(anim_mech$body.points - fit.points)^2)))
}