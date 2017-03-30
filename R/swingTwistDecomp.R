swingTwistDecomp <- function(q, v){

	# Make sure v is unit vector
	v <- uvector(v)

	# Rotation axis
	ra <- q[2:4]
	
	# Projection of ra onto v
	p <- vproj(ra, v)

	# Normalized twist quaternion - twist axis determined by input quaternion
	twist <- uvector(c(q[1], p[1:3]))

	# Swing quaternion
	swing <- qprod(q, c(twist[1], -twist[2:4]))

	# Get axes and angle
	t_q2aa <- quat2AxisAngle(twist)
	s_q2aa <- quat2AxisAngle(swing)

	list(
		'twist'=twist, 
#		'twist.axis'=v, 
		'twist.axis'=t_q2aa$axis, 
		'twist.angle'=t_q2aa$angle, 
		'swing'=swing, 
		'swing.axis'=s_q2aa$axis, 
		'swing.angle'=s_q2aa$angle
	)
}