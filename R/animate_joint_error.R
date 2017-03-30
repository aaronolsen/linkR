animate_joint_error <- function(p, type, cons, coor, coor.compare, lcs = NULL, 
	check.joint.cons = FALSE, print.progress = FALSE){

	# Run joint model
	anim <- animate_joint(type=type, cons=cons, param=rbind(rep(0, length(p)), p), coor=coor, lcs=lcs, 
		check.joint.cons=check.joint.cons, print.progress=print.progress)

	# Compare simulated coordinates to ideal
	sqrt(mean((anim$coor[, , 2] - coor.compare)^2))
}
