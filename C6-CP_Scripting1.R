sim.runs <- 1000
lambda<- 0.2
cfunc <- function(x, n) sqrt(n) * (mean(x) - 1 / lambda) / (1 / lambda)
randvars <- matrix(rexp(sim.runs*40, lambda), sim.runs)
dat <- data.frame(x = c(apply(randvars, 1, cfunc, 40)),
                  samplemeans = apply(randvars, 1, mean),
                  samplevar = apply(randvars, 1, var),
                  size = factor(rep(c(40), rep(sim.runs, 1))))

if (!suppressMessages(require("ggplot2"))){
    install.packages("ggplot2")
    suppressMessages(require("ggplot2"))
}
g1 <- ggplot(dat, aes(x = samplemeans)) +
        geom_histogram(binwidth = 0.1) +
        ggtitle("Graph A:Distribution of sample means") +
        geom_vline(xintercept = 1 / lambda, colour = "red", size = 2)
print(g1)

g2 <- ggplot(dat,aes(x=samplevar)) +
        geom_histogram(binwidth = 0.5) +
        ggtitle("Graph B:Distribution of sample variances") +
        geom_vline(xintercept = (1 / lambda)^2, color = "red", size = 2)
print(g2)

g3 <- ggplot(dat, aes(x = x, fill = size)) +
        geom_histogram(binwidth = .3, colour = "black", aes(y = ..density..)) +
        stat_function(fun = dnorm, size = 2) +
        facet_grid(. ~ size) +
        ggtitle("Graph C:Distribution of normalized sample means")
print(g3)
