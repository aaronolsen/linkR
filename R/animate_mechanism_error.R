animate_mechanism_error <- function(p, fit.points, mechanism, input.param, input.joint, 
	input.body, fit.wts, replace, n.input = NULL, n.cons = NULL, 
	coor.vectors = NULL, joint.optim = NULL){

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
			if(mechanism$joint.types[i] == 'R'){
				mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + colSums(p[j:(j+1)]*coor.vectors[[i]])
				j <- j + 2
			}else{
				mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + p[j:(j+2)]
				j <- j + 3
			}
		}

	}else if(replace == 'joint.cons'){
		i_params <- 1
		for(i in 1:mechanism$num.joints){
			if(n.cons[i] == 0) next
			mechanism$joint.cons[[i]][, , 1] <- p[i_params:(i_params+n.cons[i]-1)]
			i_params <- i_params + n.cons[i]
		}
	}
	
	# Run mechanism model
	anim_mech <- suppressWarnings(animateMechanism(mechanism, input.param=input.param, input.joint=input.joint, 
		input.body=input.body, print.progress=FALSE, check.inter.joint.dist=FALSE, 
		check.joint.cons=FALSE))
	
	# Compare simulated coordinates to ideal
	if(dim(anim_mech$body.points)[3] == 1) return(sqrt(mean(fit.wts*(anim_mech$body.points[, , 1] - fit.points)^2)))
	return(sqrt(mean(fit.wts*(anim_mech$body.points - fit.points)^2)))
}