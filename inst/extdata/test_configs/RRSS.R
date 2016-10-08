# Define joint coordinates
joint.coor <- rbind(c(0,0,0), c(0,1,0), c(1,2,0), c(2,1,0))

# Define joint types
joint.types <- c("R", "R", "S", "S")

# Define joint constraints
joint.cons <- list(c(1,0,0), c(0,0,1), NA, NA)

# Define link names
link.names <- c('Ground', 'L1', 'L2', 'L3')

# Define two links connected by each joint
joint.conn <- rbind(c('Ground','L1'), c('L1','L2'), c('L2','L3'), c('L3','Ground'))

# Define points associated with links
link.points <- rbind(c(0,0,0.1), c(0,1,0.1), c(0,1,-0.1), c(0,0,-0.1),
	c(0,1,0.1), c(0,1,-0.1), c(1,2,-0.1), c(1,2,0.1), c(1.5,1.5,0))

# Define links with which points are associated
link.assoc <- c(1,1,1,1,2,2,2,2,3)

# Define lines connecting associated points
path.connect <- list(c(1:4,1), c(5:8,5))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, link.names=link.names, joint.conn=joint.conn, 
	link.points=link.points, link.assoc=link.assoc, path.connect=path.connect)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(0,-pi/2,length=50), input.joint=1)

# Draw linkage
drawLinkage(anim, file='RRSS.html', animate.reverse=TRUE)