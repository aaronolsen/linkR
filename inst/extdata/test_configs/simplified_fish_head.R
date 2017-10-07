# Set landmark coordinates
lms <- rbind(
	c(-21, 42, 0),		# nc_vc
	c(-5, 34, -17),		# nc_su_a_L
	c(-27, 2, -32),		# pc_su_L
	c(6, -19, -7),		# ac_hy_L
	c(6, -19, 0),		# hy_mid
	c(6, -19, 7),		# ac_hy_R
	c(-27, 2, 32),		# pc_su_R
	c(-5, 34, 17),		# nc_su_a_R
	c(6, -19, 0),		# hy_mid
	c(40, 2, 0),		# lj_sy_inf
	c(-11, -8, -26),	# lj_qd_L
	c(-11, -8, 26)		# lj_qd_R
	)
           
# Set rownames for landmarks
rownames(lms) <- c('nc_vc', 'nc_su_a_L', 'pc_su_L', 'ac_hy_L', 'hy_mid', 'ac_hy_R', 
	'pc_su_R', 'nc_su_a_R', 'hy_mid', 'lj_sy_inf', 'lj_qd_L', 'lj_qd_R')

# Assign joint coordinates from landmark matrix (some landmarks need to be repeated in joint assignment)
joint.coor <- lms[c('nc_vc', 'nc_su_a_L', 'pc_su_L', 'ac_hy_L', 'hy_mid', 'ac_hy_R', 
	'pc_su_R', 'nc_su_a_R', 'hy_mid', 'lj_sy_inf', 'lj_sy_inf', 'lj_qd_L', 'lj_sy_inf', 
	'lj_sy_inf', 'lj_qd_R'), ]

# Define joint types
joint.types <- c('R','R','S','S','P','S','S','R','S','S','P','S','S','S','S')

# Define joint constraints
joint.cons <- list(
	c(0,0,-1), 		# nc_vc
	c(1,0,0), 		# nc_su_a_L
	NA, NA, 
	c(0,0,-1), 		# hy_mid
	NA, NA, 
	c(1,0,0), 		# nc_su_a_R
	NA, NA, 
	c(0,0,-1), 		# lj_sy_inf
	NA, NA, NA, NA)

# Define two links connected by each joint
joint.conn <- rbind(c('vert_column', 'neurocranium'), c('neurocranium', 'suspensorium_L'), 
	c('suspensorium_L', 'hyoid_L'), c('hyoid_L', 'hypohyal'), c('hypohyal', 'vert_column'), 
	c('hypohyal', 'hyoid_R'), c('hyoid_R', 'suspensorium_R'), c('suspensorium_R', 'neurocranium'), 
	c('hypohyal', 'hyoid_lowerjaw'), c('hyoid_lowerjaw', 'lowerjaw_symph'), 
	c('lowerjaw_symph', 'vert_column'), c('suspensorium_L', 'lowerjaw_L'), 
	c('lowerjaw_L', 'lowerjaw_symph'), c('lowerjaw_symph', 'lowerjaw_R'), 
	c('lowerjaw_R', 'suspensorium_R'))

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types,
	joint.cons=joint.cons, joint.conn=joint.conn, ground.link='vert_column')

# Set number of animation iterations
anim_len <- 50

# Set input parameters
input.param <- list(seq(0,-0.2,length=anim_len), cbind(seq(0.001,-3.001,length=anim_len), rep(0, anim_len), rep(0, anim_len)))

# Animate linkage (this can take some time to run)
anim <- animateLinkage(linkage, input.param=input.param, input.joint=c(1,5))

# Draw linkage
drawLinkage(anim, file='simplified_fish_head.html', animate.reverse=TRUE)
