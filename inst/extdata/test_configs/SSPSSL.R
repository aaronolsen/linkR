# Define joint coordinates
joint.coor <- rbind(c(-0.6,0,0), c(-0.2,1,0), c(0,1,0), c(0.2,1,0), c(1.5,0.5,0), c(1.5,0.4,0))

# Define joint types
joint.types <- c("S", "S", "P", "S", "S", "L")

# Define joint constraints
joint.cons <- list(NA, NA, c(1,0,0), NA, NA, c(1,0,0))

# Define two links connected by each joint
joint.conn <- rbind(c(0,1), c(1,2), c(2,0), c(2,3), c(3,4), c(4,0))

# Define points associated with links
link.points <- rbind(c(-0.4,0.5,0), c(1.4,0.4,0), c(1.4,0.5,0), c(1.6,0.5,0), 
	c(1.6,0.4,0), c(0.55,0.4,0), c(1.75,0.4,0), c(0.85,0.75,0),
	c(-0.2,0.9,0.1), c(0.2,0.9,0.1), c(0.2,0.9,-0.1), c(-0.2,0.9,-0.1),
	c(-0.2,1.1,0.1), c(0.2,1.1,0.1), c(0.2,1.1,-0.1), c(-0.2,1.1,-0.1))

# Define links with which points are associated
link.assoc <- c(1,4,4,4,4,0,0,3,rep(2,8))

# Define paths to connect points
path.connect <- list(c(2:5,2), c(6:7), c(9:12,9), c(13:16,13), c(9,13), 
	c(10,14), c(11,15), c(12,16))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, 
	link.assoc=link.assoc, path.connect=path.connect)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(0,-pi/4,length=50), input.joint=6)

# Draw linkage
drawLinkage(anim, file='SSPSSL.html', animate=TRUE, animate.reverse=TRUE)