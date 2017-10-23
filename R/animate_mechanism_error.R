animate_mechanism_error <- function(p, fit.points, mechanism, input.param, input.joint, 
	input.body, fit.wts, replace, direct.input = NULL, joint.compare = NULL, planar = FALSE, 
	cons.fill = NULL, coor.vectors = NULL, joint.optim = NULL, 
	use.ref.as.prev = FALSE, input.param.fill = NULL, iter = NULL, print.progress = FALSE){

	# Replace parameter with optimize parameters
	if(replace == 'input.param'){

		input_param <- list()
		for(i in 1:length(input.param.fill)){
			input_param[[i]] <- matrix(input.param[[i]][iter, ], nrow=1)
			input_param[[i]][, input.param.fill[[i]][['list.idx']]] <- matrix(p[input.param.fill[[i]][['col.idx']]], nrow=1)
		}
		input.param <- input_param

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
				}else if(nrow(coor.vectors[[i]]) == 3){
					mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + colSums(p[j:(j+2)]*coor.vectors[[i]])
					j <- j + 3
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
			tform1 <- diag(3)
			tform2 <- diag(3)
			for(j in 1:nrow(mechanism$joint.cons[[i]])){
				if(cons.fill[k] == 'v'){
					tform1 <- tMatrixEP(v=cprod(mechanism$joint.cons[[i]][j, , 1], p[n:(n+2)]), a=avec(mechanism$joint.cons[[i]][j, , 1], p[n:(n+2)]))
					mechanism$joint.cons[[i]][j, , 1] <- uvector(p[n:(n+2)])
					n <- n + 3
				}else if(cons.fill[k] == 'vo'){
					if(j == 2){
						after_tform <- mechanism$joint.cons[[i]][j, , 1] %*% tform1
						after_tform <- after_tform %*% tMatrixEP(v=mechanism$joint.cons[[i]][j-1, , 1], a=p[n])
						tform2 <- tMatrixEP(v=cprod(mechanism$joint.cons[[i]][j, , 1], after_tform), a=avec(mechanism$joint.cons[[i]][j, , 1], after_tform))
					}else if(j == 3){
						after_tform <- mechanism$joint.cons[[i]][j, , 1] %*% tform2
						after_tform <- after_tform %*% tMatrixEP(v=mechanism$joint.cons[[i]][j-1, , 1], a=p[n])
						#tform2 <- tMatrixEP(v=cprod(mechanism$joint.cons[[i]][j, , 1], after_tform), a=avec(mechanism$joint.cons[[i]][j, , 1], after_tform))
					}
					mechanism$joint.cons[[i]][j, , 1] <- after_tform

					#mechanism$joint.cons[[i]][j, , 1] <- mechanism$joint.cons[[i]][j, , 1] %*% tform1
					#mechanism$joint.cons[[i]][j, , 1] <- mechanism$joint.cons[[i]][j, , 1] %*% tMatrixEP(v=mechanism$joint.cons[[i]][j-1, , 1], a=p[n])
					n <- n + 1
				}else if(cons.fill[k] == 'cprod'){
					mechanism$joint.cons[[i]][j, , 1] <- cprod(mechanism$joint.cons[[i]][j-2, , 1], mechanism$joint.cons[[i]][j-1, , 1])
				}
				k <- k + 1
			}
		}
	}
	
	# Replace any input parameters with direct calculation
	if(!is.null(direct.input) && any(direct.input)){

		# Get input parameters
		for(i in which(direct.input)){
			input.param[[i]] <- findJointInputParam(type=mechanism$joint.type[i], 
				coor=mechanism$joint.coor[i,], cons=mechanism$joint.cons[[i]][, , 1], ref=mechanism$body.points, 
				poses=fit.points)
		}
	}
	
	if(print.progress){
		#cat('\n')
		#print(mechanism$joint.cons[[1]])
	}

	# Run mechanism model
	anim_mech <- suppressWarnings(animateMechanism(mechanism, input.param=input.param, input.joint=input.joint, 
		input.body=input.body, joint.compare=joint.compare, use.ref.as.prev=use.ref.as.prev, 
		print.progress=print.progress, check.inter.joint.dist=FALSE, check.joint.cons=FALSE))

	# Compare simulated coordinates to ideal
	if(dim(anim_mech$body.points)[3] == 1){
		return_error <- sqrt(mean(fit.wts*(anim_mech$body.points[, , 1] - fit.points)^2))
	}else{
		return_error <- sqrt(mean(fit.wts*(anim_mech$body.points - fit.points)^2))
	}
	
	# Add NA penalty
	#if(sum(is.na(anim_mech$joint.coor)) > 0){cat(TRUE, '')}else{cat(FALSE, '')}
	return_error <- return_error + 0.2*(sum(is.na(anim_mech$joint.coor)) / 3)

	return(return_error)
}