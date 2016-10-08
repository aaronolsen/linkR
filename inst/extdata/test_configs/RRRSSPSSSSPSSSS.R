# Define joint coordinates
joint.coor <- rbind(c(-1,1,0), c(0,1,1), c(0,1,-1), c(0,0,1.5), c(0,-1,0.1), 
	c(0,-1,0), c(0,-1,-0.1), c(0,0,-1.5), c(0.5,0,1.5), c(1,-0.5,0.1), c(1,-0.5,0), 
	c(1,-0.5,-0.1), c(0.5,0,-1.5), c(0,-1,0), c(1,-0.5,0))

# Define joint types
joint.types <- c("R","R","R","S","S","P","S","S","S","S","P","S","S","S","S")

# Define joint constraints
joint.cons <- list(c(0,0,1), c(1,0,0), c(1,0,0), NA, NA, c(0,0,1), NA, NA, NA, NA, 
	c(0,0,1), NA, NA, NA, NA)

# Define two links connected by each joint
joint.conn <- rbind(c(0,1), c(1,2), c(1,3), c(2,4), c(4,5), c(5,0), c(5,6), 
	c(6,3), c(2,7), c(7,9), c(9,0), c(9,8), c(8,3), c(5,10), c(10,9))

# Define points associated with links
link.points <- rbind(c(-0.5,1,0.5), c(0,1,0), c(-0.5,1,-0.5),
	c(0,0.5,1.25), c(0.25,0.5,1.25),c(0,0.5,-1.25), c(0.25,0.5,-1.25))

# Define links with which points are associated
link.assoc <- c(rep(1,3),2,2,3,3)

# Define input parameters
n_iter <- 50
input.param <- list(seq(0*(pi/180), 10*(pi/180), length=n_iter),
	matrix(c(-1,0,0), nrow=n_iter, ncol=3, byrow=TRUE)*matrix(seq(0, 1, length=n_iter), nrow=n_iter, ncol=3))

# Define input joint(s)
input.joint <- c(1,6)

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, 
	link.assoc=link.assoc)

# Animate linkage
anim <- animateLinkage(linkage, input.param=input.param, input.joint=input.joint)

# Draw linkage
drawLinkage(anim, file='RRRSSPSSSSPSSSS.html', animate.reverse=TRUE)