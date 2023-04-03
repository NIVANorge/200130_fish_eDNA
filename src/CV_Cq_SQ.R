# ?sd
# > sd(35.2, 34.75, 35.6)
# Error in sd(35.2, 34.75, 35.6) : unused argument (35.6)
# > sd(c(35.2, 34.75, 35.6))
# [1] 0.425245
# > (2)^(0.425^2)*log(2)
# [1] 0.7855955
# > (2)^(0.425^2)*log(2) -1
# [1] -0.2144045
# > (2)^((0.425^2)*log(2)) -1
# [1] 0.0906587
# > sqrt(0.0905)
# [1] 0.3008322
# > sqrt(0.0907)
# [1] 0.3011644
# > (1.95)^((0.425^2)*log(1.95)) -1
# [1] 0.08389175
# > sqrt(0.08389)
# [1] 0.2896377


# Equation pink salmon assay:
#Cq = -3.3*log(SQ) + 13.05 (Efficiency: 100%) log(SQ) = (Cq-13.05)/-3.3 SQ = exp((Cq-13.05)/-3.3)
# Cq = 23.013-3.530*log(c

#SQ = exp((Cq-23.013)/-3.530)

cqvec <- c(36.81, 36.54, 37.27)
sqfunc <- function(x) {10^((x-23.013)/-3.53)}
sdlin <- sd(sqfunc(cqvec))
sdlin/mean(sqfunc(cqvec))

cqvec2 <- c(35.21, 34.75, 35.6)
sqfunc2 <- function(x) {10^((x-19.031)/-3.224)}
sdlin2 <- sd(sqfunc2(cqvec2))
sdlin2/mean(sqfunc2(cqvec2))

cqvec_salp <- c(38.65, 38.01, 37.17)
sqfunc_salp <- function(x) {10^((x-21.461)/-3.308)}
sdlin_salp <- sd(sqfunc_salp(cqvec_salp))
sdlin_salp/mean(sqfunc_salp(cqvec_salp))
cqvec_salp

cqvec_danskpukkel <- c(33.21, 31.54, 32.44)

sqfunc_danskp <- function(x) {10^((x-18.539)/-3.44)}

sqfunc_danskp(cqvec_danskpukkel)
sd(sqfunc_danskp(cqvec_danskpukkel))/mean(sqfunc_danskp(cqvec_danskpukkel))

norsk <- c(8.65e-5, 1.01e-4, 4.79e-5)
sd(norsk)/mean(norsk)
