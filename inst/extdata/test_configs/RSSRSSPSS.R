# Define joint coordinates
joint.coor <- rbind(c(0,0,0), c(0,1,0), c(2,1,0), c(2,0,0),
	c(0,1,0.5), c(1,0.5,0.5), c(1,0.5,0.5), c(1,0.5,0.5), c(2,1,0.5))

# Define joint types
joint.types <- c("R", "S", "S", "R", "S", "S", "P", "S", "S")

# Define joint constraints
joint.cons <- list(c(0,0,1), NA, NA, c(0,1,1), NA, NA, c(0,1,1), NA, NA)

# Define two links connected by each joint
joint.conn <- rbind(c(0,1),c(1,2),c(2,3),c(3,0),c(1,4),c(4,5),c(5,0),c(5,6),c(6,3))

# Define points associated with links
link.points <- rbind(c(1,0.5,0.5), c(1,0.6,0.6))

# Define links with which points are associated
link.assoc <- rep(5,2)

# Define lines connecting associated points
path.connect <- list(c(1,2))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, 
	link.assoc=link.assoc, path.connect=path.connect)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(-0.05,-pi/4,length=50), input.joint=1)

# Draw linkage
drawLinkage(anim, file='RSSRSSPSS.html', animate.reverse=TRUE)