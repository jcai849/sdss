S <- function(alpha) pmax(alpha-kappa, 0) - pmax(-alpha-kappa |	S <- function(kappa) function(alpha) pmax(alpha-kappa, 0) - p
p_norm <- function(p) function(x) sum(abs(x)^p)^(1/p)		p_norm <- function(p) function(x) sum(abs(x)^p)^(1/p)
l1_norm <- p_norm(1)						l1_norm <- p_norm(1)
l2_norm <- p_norm(2)						l2_norm <- p_norm(2)

A <- matrix(runif(m*n), n, m)				      |	dA <- read.dmatrix("filepath-A")
chunks_i <- tapply(seq(n), cut(seq(n), N), identity, simplify |	db <- read.dmatrix("filepath-b")
A <- lapply(chunks_i, function(i, x) x[i,], A)		      <
b <- lapply(chunks_i, function(i, x) x[i], b)		      <

lasso <- function(A, b, tolerance=1, rho=3, lambda=rho) {     |
    M_N <- dim(A)					      |
							      >	dlasso <- function(A, b, tolerance=1, rho=3, lambda=rho) {
							      >	    M_N <- dim(Ref(A))
    m <- ncol(A)						    m <- ncol(A)
    S_z <- S(lambda/(rho*M_N[2]))				    S_z <- S(lambda/(rho*M_N[2]))

    z_prev <- rep(Inf, m)				      |	    z_prev <- x_prev <- u_prev <- rep(Inf, m)
    x_prev <- u_prev <- rep(list(z_prev), N)		      |
    z_curr <- rep(1, m)						    z_curr <- rep(1, m)
    x_curr <- u_curr <- rep(list(z_curr), N)		      |	    x_curr <- distribute(z_curr, where=A); u_curr <- distribu

    while (l1_norm(z_curr - z_prev) > tolerance) {		    while (l1_norm(z_curr - z_prev) > tolerance) {
        x_prev <- x_curr; z_prev <- z_curr; u_prev <- u_curr	        x_prev <- x_curr; z_prev <- z_curr; u_prev <- u_curr
        x_curr <- mapply(x.update, x_prev, A, b, u_prev, More |	        x_curr <- d.x_update(x_prev, A, b, u_prev, rho, z_pre
        x_local <- unlist(x_curr); u_local <- unlist(u_curr); |	        dim(Ref(x_curr)) <- dim(Ref(u_curr)) <- M_N
        z_curr <- S_z(rowMeans(x_local) + rowMeans(u_local))  |	        z_curr <- S_z(rowMeans(emerge(x_curr)) + rowMeans(eme
        u_curr <- mapply(function(u_prev, x_curr, z_curr) u_p |	        u_curr <- u_prev + x_curr - z_curr
    }								    }
    z_curr							    z_curr
}								}
x_update <- function(x_prev, A, b, u_prev, rho, z_prev) {     |	d.x_update <- d(function(x_prev, A, b, u_prev, rho, z_prev) {
                    optim(x_prev,				                    optim(x_prev,
                        function(x_prev) (1/2)*l2_norm(A %*% 	                        function(x_prev) (1/2)*l2_norm(A %*% 
                                        (rho/2)*l2_norm(x_pre	                                        (rho/2)*l2_norm(x_pre
                }					      |	                })