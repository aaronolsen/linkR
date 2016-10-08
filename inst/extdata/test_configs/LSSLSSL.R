# Define joint coordinates
joint.coor <- rbind(c(0,0,0),c(0,0.1,0),c(0.9,1,0),c(1,1,0),c(0.9,1,0),c(0,1.9,0),c(0,2,0))

# Define joint types
joint.types <- c("L", "S", "S", "L", "S", "S", "L")

# Define joint constraints
joint.cons <- list(c(1,0,0), NA, NA, c(0,1,0), NA, NA, c(1,0,0))

# Define two links connected by each joint
joint.conn <- rbind(c(0,1), c(1,2), c(2,3), c(3,0), c(3,4), c(4,5), c(5,0))

# Define points associated with links
link.points <- rbind(c(-0.1,0,0), c(-0.1,0.1,0), c(0.1,0.1,0), c(0.1,0,0),
	c(1,0.9,0), c(0.9,0.9,0), c(0.9,1.1,0), c(1,1.1,0), c(-0.1,2,0), c(-0.1,1.9,0), 
	c(0.1,1.9,0), c(0.1,2,0), c(-0.5,0,0), c(1,0,0), c(1,0.5,0), c(1,1.5,0),
	c(-1,2,0), c(0.5,2,0), c(0.45,0.55,0), c(0.45,1.45,0))

# Define links with which points are associated
link.assoc <- c(rep(1,4), rep(3,4), rep(5,4), rep(0,6), 2, 4)

# Define paths to connect points
path.connect <- list(c(1:4,1), c(5:8,5), c(9:12,9), c(13,14), c(15,16), c(17,18))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, 
	link.assoc=link.assoc, path.connect=path.connect)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(-0.2, 0.5, length=50), input.joint=1)

# Draw linkage
drawLinkage(anim, file='LSSLSSL.html', animate.reverse=TRUE)