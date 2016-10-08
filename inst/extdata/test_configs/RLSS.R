# Define joint coordinates
joint.coor <- rbind(c(0,0,0), c(0,0.75,0), c(0.1,0.75,0), c(0.8,0.4,0))

# Define joint types
joint.types <- c("R", "L", "S", "S")

# Define joint constraints
joint.cons <- list(c(0,0,1), c(0,1,0), NA, NA)

# Define link names
link.names <- c('Ground', 'L1', 'L2', 'L3')

# Define two links connected by each joint
joint.conn <- rbind(c('Ground','L1'), c('L1','L2'), c('L2','L3'), c('L3','Ground'))

# Define points associated with links
link.points <- rbind(c(0,0.65,0), c(0.1,0.65,0), c(0.1,0.85,0), c(0,0.85,0),
	c(0,0,0), c(-0.1,0,0), c(-0.1,1.5,0), c(0,1.5,0), c(0.45, 0.575, 0))

# Define links with which points are associated
link.assoc <- c(2,2,2,2,1,1,1,1,3)

# Define lines connecting associated points
path.connect <- list(c(1:4,1), c(5:8,5))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.names=link.names, 
	link.points=link.points, link.assoc=link.assoc, path.connect=path.connect)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(0,-0.4,length=50), input.joint=1)

# Draw linkage
drawLinkage(anim, file='RLSS.html', animate.reverse=TRUE)