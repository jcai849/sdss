S <- function(kappa) function(alpha) pmax(alpha-kappa, 0) - pmax(-alpha-kappa, 0)
p_norm <- function(p) function(x) sum(abs(x)^p)^(1/p)
l1_norm <- p_norm(1)
l2_norm <- p_norm(2)

dA <- read.dmatrix("filepath-A")
db <- read.dmatrix("filepath-b")



dlasso <- function(A, b, tolerance=1, rho=3, lambda=rho) {
    M_N <- dim(Ref(A))
    m <- ncol(A)
    S_z <- S(lambda/(rho*M_N[2]))

    z_prev <- x_prev <- u_prev <- rep(Inf, m)

    z_curr <- rep(1, m)
    x_curr <- distribute(z_curr, where=A); u_curr <- distribute(z_curr, where=A)

    while (l1_norm(z_curr - z_prev) > tolerance) {
        x_prev <- x_curr; z_prev <- z_curr; u_prev <- u_curr
        x_curr <- d.x_update(x_prev, A, b, u_prev, rho, z_prev)
        dim(Ref(x_curr)) <- dim(Ref(u_curr)) <- M_N
        z_curr <- S_z(rowMeans(emerge(x_curr)) + rowMeans(emerge(u_curr)))
        u_curr <- u_prev + x_curr - z_curr
    }
    z_curr
}
d.x_update <- d(function(x_prev, A, b, u_prev, rho, z_prev) {
                    optim(x_prev,
                        function(x_prev) (1/2)*l2_norm(A %*% x_prev - b)^2 +
                                        (rho/2)*l2_norm(x_prev - z_prev + u_prev)^2)$par
                })