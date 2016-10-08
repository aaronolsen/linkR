# Define joint coordinates
joint.coor <- rbind(c(0,0,0), c(0,1,0), c(0.9,0.6,0), c(1,0.5,0))

# Define joint types
joint.types <- c("R", "S", "S", "L")

# Define joint constraints
joint.cons <- list(c(1,0,1), NA, NA, c(1,1,0))

# Define two links connected by each joint
joint.conn <- rbind(c(0,1), c(1,2), c(2,3), c(3,0))

# Define points associated with links
link.points <- rbind(c(0.75,0.25,0), c(1.75,1.25,0), c(0.9,0.4,0), c(0.8,0.5,0), 
	c(1,0.7,0), c(1.1,0.6,0), c(0.1,0,0.1), c(-0.1,0,-0.1), c(-0.05,0,0.05), 
	c(-0.05,1,0.05), c(0.05,1,-0.05), c(0.05,0,-0.05), c(0.45,0.8,0))

# Define links with which points are associated
link.assoc <- c(0,0,3,3,3,3,0,0,1,1,1,1,2)

# Define paths to connect points
path.connect <- list(c(1,2), c(3:6,3), c(7,8), c(9:12,9))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, 
	link.assoc=link.assoc, path.connect=path.connect)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(0,-pi/2,length=60), input.joint=1)

# DRAW LINKAGE
drawLinkage(anim, file='RSSL.html', animate.reverse=TRUE)
