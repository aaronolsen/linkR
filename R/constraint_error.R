constraint_error <- function(p, type, param, coor, coor.compare, joint.cons.init, dof, lcs = NULL, 
	check.joint.cons = FALSE, print.progress = FALSE){

	# Apply transformations to constraint parameters
	cons <- apply_cons_optim_transforms(p, joint.cons.init, type, dof)

	# Run joint model
	anim <- animate_joint(type=type, cons=cons, param=param, coor=coor, lcs=lcs, 
		check.joint.cons=check.joint.cons, print.progress=print.progress)

	# Compare simulated coordinates to ideal
	sqrt(mean((anim$coor - coor.compare)^2))
}