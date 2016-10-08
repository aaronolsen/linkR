# Define the joint coordinates
joint.coor <- rbind(c(0,0,0), c(0,0.5,0), c(1,1,0), c(1,0,0))

# Define the joint types
joint.types <- c('R', 'S', 'S', 'R')

# Define the joint constraint vectors
joint.cons <- list(c(0,0,1), NA, NA, c(0,0,1))

# Define the connections among joints
joint.conn <- rbind(c(0,1), c(1,2), c(2,3), c(3,0))

# Define first rectangle
rec1 <- rbind(c(-0.1,-0.1,0), c(-0.1,0.6,0), c(0.1,0.6,0), c(0.1,-0.1,0))

# Define second rectangle
rec2<- rbind(c(0.9,-0.1,0), c(0.9,1.1,0), c(1.1,1.1,0), c(1.1,-0.1,0))

# Define the point coordinates
link.points <- rbind(rec1, c(0.5,0.75,0), rec2)

# Set the link associations for each point
link.assoc <- c(1,1,1,1, 2, 3,3,3,3)

# Define points to connect into paths
path.connect <- list(c(1:4,1), c(6:9,6))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, 
	link.assoc=link.assoc, path.connect=path.connect)

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(0,2*pi,length=60), input.joint=1)

# Draw linkage
drawLinkage(anim, file='RSSR.html')