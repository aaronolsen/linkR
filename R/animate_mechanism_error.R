animate_mechanism_error <- function(p, fit.points, mechanism, 
	input.param, input.joint, input.body, replace, iter){

	# Replace parameter with optimize parameters
	if(replace == 'input.param'){
		n <- 1
		for(i in 1:length(input.param)){
			n_inputs <- ncol(input.param[[i]])
			input.param[[i]] <- matrix(p[n:(n + n_inputs - 1)], nrow=1, ncol=n_inputs)
			n <- n + n_inputs
		}
	}
	
	# Run mechanism model
	anim_mech <- animateMechanism(mechanism, input.param=input.param, input.joint=input.joint, 
		input.body=input.body, print.progress=FALSE, check.inter.joint.dist=FALSE, 
		check.joint.cons=FALSE)

	# Compare simulated coordinates to ideal
	sqrt(mean((anim_mech$body.points[, , 1] - fit.points)^2))
}