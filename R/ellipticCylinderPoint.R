ellipticCylinderPoint <- function(ecylinder, A, T){
	ecylinder$C + A*ecylinder$N + ecylinder$R1*cos(T)*ecylinder$U + ecylinder$R2*sin(T)*ecylinder$V
}