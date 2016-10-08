# Define joint coordinates
joint.coor <- rbind(c(0,0,0), c(0,1,0),c(1,1,0), c(1,0,0), 
	c(1,0.75,0), c(2,1.4,0), c(2,1.5,0))

# Define joint types
joint.types <- c("R", "S", "S", "R", "S", "S", "L")

# Define joint constraints
joint.cons <- list(c(0,0,1), NA, NA, c(0,0,1), NA, NA, c(1,0,0))

# Define two links connected by each joint
joint.conn <- rbind(c(0,1), c(1,2), c(2,3), c(3,0), c(3,4), c(4,5), c(5,0))

# Define points associated with links
link.points <- rbind(c(0,0.5,0), c(0.5,1,0), c(1,0.5,0), c(1.5,1.075,0))

# Define links with which points are associated
link.assoc <- c(1,2,3,4)

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, 
	link.assoc=link.assoc)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(0, pi/3, length=50), input.joint=1)

# Draw linkage
drawLinkage(anim, file='RSSR(SSL).html', animate.reverse=TRUE)