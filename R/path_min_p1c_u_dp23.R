path_min_p1c_u_dp23 <- function(p, params){

	# Select new p1 as point on circle
	p1_new <- circlePoint(circle=params[['circle']], T=p)

	# Find transformation to move p1 to p1_new given U-joint parameters
	solve_ujt <- solveUJtFromPts(center=params[['u.center']], axes=params[['u.axes']], 
		p1=params[['p1']], p2=p1_new, tp1=params[['p2']], tp2=params[['tp2']])
	
	# Check if just returning transformation
	if(params[['return.tmat']]) return(solve_ujt$tmat)

	# Copy that transformation to p2
	p2_new <- applyTransform(to=params[['p2']], tmat=solve_ujt$tmat)
	
	# Find difference in the distance between the new p2 and p3 versus objective
	abs(distPointToPoint(p2_new, params[['p3']]) - params[['d23']])
}
