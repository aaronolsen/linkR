# Get specimen data
owl <- linkR_data('owl')

# Copy landmarks for easy reference
lms <- owl$landmarks

# Define joint coordinates
joint.coor <- lms[c('nc_qd_l_R', 'ju_qd_R', 'ju_ub_R', 'nc_ub_R', 'pt_qd_R', 
	'pa_pt_R', 'pa_pt_R', 'pa_pt_R', 'pa_ub_R', 'nc_qd_l_L', 'ju_qd_L', 'ju_ub_L', 
	'nc_ub_L', 'pt_qd_L', 'pa_pt_L', 'pa_pt_L', 'pa_pt_L', 'pa_ub_L'), ]

# Define joint types
joint.types <- c("R", "S", "S", "R", "S", "S", "P", "S", "S",
	"R", "S", "S", "R", "S", "S", "P", "S", "S")

# Define joint constraints
joint.cons <- list(
	lms['nc_qd_l_R', ]-lms['nc_qd_m_R', ], NA, NA, lms['nc_ub_L', ]-lms['nc_ub_R', ],
	NA, NA, lms['nc_ub_L', ]-lms['nc_ub_R', ], NA, NA,
	lms['nc_qd_l_L', ]-lms['nc_qd_m_L', ], NA, NA, lms['nc_ub_R', ]-lms['nc_ub_L', ],
	NA, NA, lms['nc_ub_R', ]-lms['nc_ub_L', ], NA, NA)

# Define two links connected by each joint
joint.conn <- rbind(
	c('neurocranium', 'quadrate_R'), c('quadrate_R', 'jugal_R'), 
	c('jugal_R', 'upperbeak'), c('upperbeak', 'neurocranium'),
	c('quadrate_R', 'pterygoid_R'), c('pterygoid_R', 'pp-slide_R'), 
	c('pp-slide_R', 'neurocranium'), c('pp-slide_R', 'palatine_R'), 
	c('palatine_R', 'upperbeak'), c('neurocranium', 'quadrate_L'), 
	c('quadrate_L', 'jugal_L'), c('jugal_L', 'upperbeak'), 
	c('upperbeak', 'neurocranium'), c('quadrate_L', 'pterygoid_L'), 
	c('pterygoid_L', 'pp-slide_L'), c('pp-slide_L', 'neurocranium'), 
	c('pp-slide_L', 'palatine_L'), c('palatine_L', 'upperbeak')
)

# Define points associated with links
link.points <- owl$landmarks

# Define links with which points are associated
link.assoc <- owl$lm.assoc

# Define lines connecting associated points
path.connect <- owl$path.connect

# Define linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, link.points=link.points, link.assoc=link.assoc, 
	joint.conn=joint.conn, path.connect=path.connect, ground.link='neurocranium')

# Animate linkage
anim <- animateLinkage(linkage, input.param=seq(-0.07,0.17,length=30), input.joint=1)

# Draw linkage
drawLinkage(anim, file='owl.html', animate.reverse=TRUE)