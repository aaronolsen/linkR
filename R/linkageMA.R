linkageMA <- function(linkage, input.param, input.joint, input.point, output.point, 
	disp_mag=NULL, small=0.001){

	# Make sure only a single input and output point are specified
	if(length(input.point) > 1) stop("Only a single input point is allowed for calculating MA.")
	if(length(output.point) > 1) stop("Only a single output point is allowed for calculating MA.")

	# Use linkageEA to find instantaneous displacements at input and output
	linkage_ea <- linkageEA(linkage=linkage, input.param=input.param, input.joint=input.joint, 
		input.points=input.point, output.points=output.point, disp_mag=disp_mag, small=small)
	
	# Create return list
	rlist <- list(
		'ma'=1/linkage_ea$ea,
		'in.d'=linkage_ea$in.d,
		'out.d'=linkage_ea$out.d
	)

	rlist
}