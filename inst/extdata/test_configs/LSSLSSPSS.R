# Define joint coordinates
joint.coor <- rbind(c(0,0.5,0), c(0,0.6,0), c(2,1,0), c(2,1,0), c(0,0.5,0), 
	c(0.9,0.25,0), c(1,0.25,0), c(1.1,0.25,0), c(1.9,1,0))

# Define joint types
joint.types <- c("L", "S", "S", "L", "S", "S", "P", "S", "S")

# Define joint constraints
joint.cons <- list(c(1,0,0), NA, NA, c(0,1,0), NA, NA, c(1,0,0), NA, NA)

# Define two links connected by each joint
joint.conn <- rbind(c(0,1),c(1,2),c(2,3),c(3,0),c(1,4),c(4,5),c(5,0),c(5,6),c(6,3))

# Define points associated with links
link.points <- rbind(c(-0.25,0.5,0), c(0.5,0.5,0), c(2,0.75,0), c(2,1.75,0), 
	c(1,0.8,0), c(0.45,0.375,0), c(1.5,0.625,0))

# Define links with which points are associated
link.assoc <- c(rep(0,4),2,4,6)

# Define paths to connect points
path.connect <- list(c(1,2), c(3,4))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, 
	link.assoc=link.assoc, path.connect=path.connect)

# Animate linkage
anim <- animateLinkage(linkage,input.param=seq(0,0.3,length=50),input.joint=1)

# Draw linkage
drawLinkage(anim, file='LSSLSSPSS.html', animate.reverse=TRUE)