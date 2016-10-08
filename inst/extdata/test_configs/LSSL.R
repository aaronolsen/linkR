# Define joint coordinates
joint.coor <- rbind(c(0,0,0), c(0,0.1,0), c(0.9,0.5,0), c(1,0.5,0))

# Define joint types
joint.types <- c("L", "S", "S", "L")

# Define joint constraints
joint.cons <- list(c(1,0,0), NA, NA, c(0,1,0))

# Define points associated with links
link.points <- rbind(c(-0.3,0,0), c(1,0,0), c(-0.1,0,0), c(-0.1,0.1,0), c(0.1,0.1,0), 
	c(0.1,0,0), c(1,0.4,0), c(0.9,0.4,0), c(0.9,0.6,0), c(1,0.6,0), c(1,0,0), c(1,1.2,0),
	c(0.45, 0.3, 0))

# Define links with which points are associated
link.assoc <- c(0,0,1,1,1,1,3,3,3,3,0,0,2)

# Define lines connecting associated points
path.connect <- list(c(1,2), c(3:6,3), c(7:10,7), c(11,12))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, link.points=link.points, link.assoc=link.assoc, 
	path.connect=path.connect)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(0,0.5,length=50), input.joint=1)

# Draw linkage
drawLinkage(anim, file='LSSL.html', animate.reverse=TRUE)