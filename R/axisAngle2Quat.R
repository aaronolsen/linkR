axisAngle2Quat <- function(aor, angle){

	## Converts rotation of angle about aor to a quaternion
	angle2 <- angle / 2
	
	# Make sure aor is normalized
	aor <- uvector(aor)

	c(cos(angle2), aor*sin(angle2))
}