# Get specimen data
salmon <- linkR_data('salmo_salar_wlc2016001')

# Copy landmarks for easy reference
lms <- salmon$landmarks

# Define joint coordinates
joint.coor <- lms[c('nc_vc', 'nc_su_a_L', 'pc_su_L', 'ac_hy_L', 'hy_mid', 
	'ac_hy_R', 'pc_su_R', 'nc_su_a_R', 'hy_mid', 'lj_sy_inf', 'lj_sy_inf', 
	'lj_qd_L', 'lj_sy_inf', 'lj_sy_inf', 'lj_qd_R'), ]

# Define joint types
joint.types <- c("R","R","S","S","P","S","S","R","S","S","P","S","S","S","S")

# Define joint constraints
joint.cons <- list(
	lms['nc_su_p_L', ] - lms['nc_su_p_R', ], lms['nc_su_a_L', ] - lms['nc_su_p_L', ], 
	NA, NA,	lms['nc_su_a_L', ] - lms['nc_su_a_R', ], NA, NA, 
	lms['nc_su_a_R', ] - lms['nc_su_p_R', ], NA, NA, 
	lms['nc_su_a_L', ] - lms['nc_su_a_R', ], NA, NA, NA, NA)

# Define two links connected by each joint
joint.conn <- rbind(
	c('vert_column', 'neurocranium'), c('neurocranium', 'suspensorium_L'), 
	c('suspensorium_L', 'hyoid_L'), c('hyoid_L', 'hypohyal'), 
	c('hypohyal', 'vert_column'), c('hypohyal', 'hyoid_R'), 
	c('hyoid_R', 'suspensorium_R'), c('suspensorium_R', 'neurocranium'),
	c('hypohyal', 'hyoid_lowerjaw'), c('hyoid_lowerjaw', 'lowerjaw_symph'), 
	c('lowerjaw_symph', 'vert_column'), c('suspensorium_L', 'lowerjaw_L'),
	c('lowerjaw_L', 'lowerjaw_symph'), c('lowerjaw_symph', 'lowerjaw_R'), 
	c('lowerjaw_R', 'suspensorium_R'))

# Set long axis rotation constraints
lar.cons <- list(
	list('link'='lowerjaw_L', 'type'='P', 'point'=lms['lj_sy_sup', ], 
		'vec'=lms['nc_su_p_L', ] - lms['nc_su_p_R', ]),
	list('link'='lowerjaw_R', 'type'='P', 'point'=lms['lj_sy_sup', ], 
		'vec'=lms['nc_su_p_L', ] - lms['nc_su_p_R', ]),
	list('link'='hyoid_R', 'type'='P', 'point'=lms['ac_as_R', ], 
		'vec'=lms['nc_su_p_L', ] - lms['nc_su_p_R', ]),
	list('link'='hyoid_L', 'type'='P', 'point'=lms['ac_as_L', ], 
		'vec'=lms['nc_su_p_L', ] - lms['nc_su_p_R', ]))

# Define points associated with links
link.points <- salmon$landmarks

# Define links with which points are associated
link.assoc <- salmon$lm.assoc

# Define lines connecting associated points
path.connect <- salmon$path.connect

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, link.points=link.points, link.assoc=link.assoc, 
	joint.conn=joint.conn, path.connect=path.connect, ground.link='vert_column',
	lar.cons=lar.cons)

# Set number of animation iterations
anim_len <- 30

# Set input parameters
input.param <- list(seq(0,-0.1,length=anim_len),
	cbind(seq(0.001,-3.001,length=anim_len), rep(0, anim_len), rep(0, anim_len)))

# Animate linkage (this can take some time to run)
anim <- animateLinkage(linkage, input.param=input.param, input.joint=c(1,5))

# Draw linkage
drawLinkage(anim, file='salmo_salar_wlc2016001.html', animate.reverse=TRUE)