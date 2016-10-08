# Define joint coordinates
joint.coor <- rbind(c(-0.5,0.5,0), c(-0.5,0.6,0), c(0.3,0.5,0), c(0.5,0,0), 
	c(0.7,0.5,0), c(1.5,0.9,0), c(1.5,1,0))

# Define joint types
joint.types <- c("L", "S", "S", "R", "S", "S", "L")

# Define joint constraints
joint.cons <- list(c(1,0,0), NA, NA, c(0,0,-1), NA, NA, c(0,0,1))

# Define two links connected by each joint
joint.conn <- rbind(c(0,1), c(1,2), c(2,3), c(3,0), c(3,4), c(4,5), c(5,0))

# Define points associated with links
link.points <- rbind(c(-0.6,0.5,0), c(-0.6,0.6,0), c(-0.4,0.6,0), c(-0.4,0.5,0),
		c(1.5,0.9,-0.1), c(1.5,1,-0.1), c(1.5,1,0.1), c(1.5,0.9,0.1), c(-1,0.5,0), 
		c(0,0.5,0), c(1.5,1,-0.5), c(1.5,1,0.5), c(-0.1,0.55,0), c(1.1,0.7,0))

# Define links with which points are associated
link.assoc <- c(rep(1,4), rep(5,4), rep(0,4), 2, 4)

# Define paths to connect points
path.connect <- list(c(1:4,1), c(5:8,5), c(9,10), c(11,12))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, 
	link.assoc=link.assoc, path.connect=path.connect)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(0,pi/4,length=50), input.joint=4)

# Draw linkage
drawLinkage(anim, file='LSSRSSL.html', animate.reverse=TRUE)