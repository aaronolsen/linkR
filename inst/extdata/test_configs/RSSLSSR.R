# Define joint coordinates
joint.coor <- rbind(c(0,0,0), c(0,1,0), c(0.9,1,0.9), c(1,1,1), 
	c(1.1,1,0.9), c(2,2,0), c(2,0,0))

# Define joint types
joint.types <- c("R", "S", "S", "L", "S", "S", "R")

# Define joint constraints
joint.cons <- list(c(-1,0,1), NA, NA, c(1,0,0), NA, NA, c(0,0,1))

# Define two links connected by each joint
joint.conn <- rbind(c(0,1), c(1,2), c(2,3), c(3,0), c(3,4), c(4,5), c(5,0))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(0,pi/6,length=50), input.joint=1)

# Draw linkage
drawLinkage(anim, file='RSSLSSR.html', animate.reverse=TRUE)