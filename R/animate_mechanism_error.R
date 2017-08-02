animate_mechanism_error <- function(p, fit.points, mechanism, input.param, input.joint, 
	input.body, fit.wts, replace, planar = FALSE, n.input = NULL, cons.fill = NULL, 
	coor.vectors = NULL, joint.optim = NULL, use.ref.as.prev = FALSE){

	# Replace parameter with optimize parameters
	if(replace == 'input.param'){
		n <- 1
		for(i in 1:length(input.param)){
			input.param[[i]] <- matrix(p[n:(n + n.input[i] - 1)], nrow=1, ncol=n.input[i])
			n <- n + n.input[i]
		}
	}else if(replace == 'joint.coor'){

		#mechanism$joint.coor <- mechanism$joint.coor + matrix(p, mechanism$num.joints, 3, byrow=TRUE)

		j <- 1
		for(i in joint.optim){
			if(length(coor.vectors[[i]]) == 1){
				mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + p[j:(j+2)]
				j <- j + 3
			}else{
				if(nrow(coor.vectors[[i]]) == 1){
					mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + colSums(p[j]*coor.vectors[[i]])
					j <- j + 1
				}else if(nrow(coor.vectors[[i]]) == 2){
					mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + colSums(p[j:(j+1)]*coor.vectors[[i]])
					j <- j + 2
				}
			}

			if(planar){
				mechanism$joint.coor[i, ] <- pointPlaneProj(mechanism$joint.coor[i, ], 
					p=mechanism$joint.coor[1, ], n=mechanism$joint.cons[[i]][, , 1])
			}
		}
		

	}else if(replace == 'joint.cons'){

		k <- 1
		n <- 1
		for(i in 1:length(mechanism$joint.cons)){
			for(j in 1:nrow(mechanism$joint.cons[[i]])){
				if(cons.fill[k] == 'v'){
					mechanism$joint.cons[[i]][j, , 1] <- p[n:(n+2)]
					n <- n + 3
				}else if(cons.fill[k] == 'vo'){
					mechanism$joint.cons[[i]][j, , 1] <- rotateBody(m=mechanism$joint.cons[[i]][j, , 1], v=mechanism$joint.cons[[i]][j-1, , 1], a=p[n])
					n <- n + 1
				}else if(cons.fill[k] == 'cprod'){
					mechanism$joint.cons[[i]][j, , 1] <- cprod(mechanism$joint.cons[[i]][j-2, , 1], mechanism$joint.cons[[i]][j-1, , 1])
				}
				k <- k + 1
			}
		}
	}
	
	# Run mechanism model
	anim_mech <- suppressWarnings(animateMechanism(mechanism, input.param=input.param, input.joint=input.joint, 
		input.body=input.body, use.ref.as.prev=use.ref.as.prev, print.progress=FALSE, check.inter.joint.dist=FALSE, 
		check.joint.cons=FALSE))
	
	# Compare simulated coordinates to ideal
	if(dim(anim_mech$body.points)[3] == 1) return(sqrt(mean(fit.wts*(anim_mech$body.points[, , 1] - fit.points)^2)))
	return(sqrt(mean(fit.wts*(anim_mech$body.points - fit.points)^2)))
}