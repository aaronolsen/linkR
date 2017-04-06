constraint_error <- function(p, type, param, coor, coor.compare, lcs = NULL, 
	check.joint.cons = FALSE, print.progress = FALSE){

	# Run joint model
	anim <- animate_joint(type=type, cons=p, param=param, coor=coor, lcs=lcs, 
		check.joint.cons=check.joint.cons, print.progress=print.progress)

	# Compare simulated coordinates to ideal
	sqrt(mean((anim$coor - coor.compare)^2))
}