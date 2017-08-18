drawMechanismFit <- function(fit.mechanism, method = "svgViewR", file = NULL, animate = TRUE, 
	animate.duration = 1, animate.reverse = FALSE, animate.repeat = -1, 
	connect.joints=TRUE, window.title='Mechanism Fit', joint.col.fill="white", 
	joint.col.stroke="black", joint.cex=1.5, joint.lwd=2,
	point.col.fill="black", point.col.stroke="black", point.cex=1, point.lwd=2,
	path.col.fill=NA, path.opacity.fill=1, path.opacity.stroke=1, path.col.stroke="black", 
	path.lwd = 1, add = FALSE, debug = FALSE, ...){

	# Animate mechanism
	anim_mech <- animateMechanism(fit.mechanism$mechanism, input.param=fit.mechanism$input.param, 
		use.ref.as.prev=fit.mechanism$use.ref.as.prev, input.joint=fit.mechanism$input.joint,
		input.body=fit.mechanism$input.body, joint.compare=fit.mechanism$joint.compare)

#	print(fit.mechanism$mechanism$body.points)
#	print(fit.mechanism$mechanism$body.assoc)
#	print(fit.mechanism$mechanism$points.assoc)
#	print(dim(anim_mech$tmarr))
#	print(anim_mech$tmarr[, , , 1])
	#print(dimnames(anim_mech$body.points))
	#for(i in 1:30) print(distPointToPoint(anim_mech$body.points[c('SuspensoriumL_oper_susp_jt_dor', 'SuspensoriumL_nc_susp_jt_cra'), , i]))

	# Draw mechanism
	drawMechanism(linkage=anim_mech, method=method, file=file, animate=animate,
		animate.duration=animate.duration, animate.reverse=animate.reverse, animate.repeat=animate.repeat, 
		connect.joints=connect.joints, window.title=window.title, joint.col.fill=joint.col.fill, 
		joint.col.stroke=joint.col.stroke, joint.cex=joint.cex, joint.lwd=joint.lwd,
		point.col.fill=point.col.fill, point.col.stroke=point.col.stroke, point.cex=point.cex, point.lwd=point.lwd,
		path.col.fill=path.col.fill, path.opacity.fill=path.opacity.fill, path.opacity.stroke=path.opacity.stroke, 
		path.col.stroke=path.col.stroke, path.lwd=path.lwd, add=add, debug=debug)

	#
	svg.points(fit.mechanism$fit.points, col='purple', cex=2, file=file)

	list(
		'animate'=anim_mech
	)
}
