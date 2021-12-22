library(rethinking)
d <- read.csv("https://raw.githubusercontent.com/torkar/dat321/master/data_autumn2020.csv", sep = ";") # nolint

d$technique[d$technique == "0T"] <- "OT" # Fix the error

dat_list <- list(
    tp = standardize(d$tp),
    T = ifelse(d$technique == "NT", 1L, 2L),
    C = ifelse(d$category == "LE", 1L, 2L)
)

dat_list2 <- list(
    tp = d$tp,
    T = ifelse(d$technique == "NT", 1L, 2L),
    C = ifelse(d$category == "LE", 1L, 2L)
)




m1 <- ulam(
    alist(
        tp ~ dnorm(mu, sigma),
        mu <- a + bT * T + bC * C,
        a ~ dnorm(0, 0.2),
        c(bT, bC) ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ),
    data = dat_list, chains = 4, cores = 4, log_lik = TRUE
)

m2 <- ulam(
    alist(
        tp ~ dpois(lambda),
        log(lambda) <- a,
        a ~ dnorm(3, 0.5)
    ),
    data = dat_list2, chains = 4, log_lik = TRUE
)


m3 <- ulam(
    alist(
        tp ~ dnorm(mu, sigma),
        mu <- w[T] + j[C],
        w[T] ~ dnorm(0, 0.05),
        j[C] ~ dnorm(0, 0.05),
        sigma ~ dexp(1)
    ),
    data = dat_list, chains = 4, cores = 4, log_lik = TRUE
)



set.seed(77)
compare(m1, m2, m3, func = WAIC)




# _____________Dag_________
# T <U+22A5> C
library(dagitty)
dag <- dagitty("dag{ T -> TP; C -> TP; }")
coordinates(dag) <- list(x = c(T = 0, TP = 1, C = 2), y = c(T = 0, TP = 0, C = 0)) # nolint
drawdag(dag)


library(ggdag)
dag <- dagitty::dagitty("dag {
    T -> TP <- C
    C [exposure]
    T [exposure]
    TP [outcome]
  }")

tidy_dag <- tidy_dagitty(dag)

tidy_dag

ggdag(tidy_dag) +
    theme_dag()



# #_____________traceplot_________


precis_plot(precis(m1, 2))
traceplot(m1)
# You’ll see that the traceplots pass the hairy-caterpillar-ocular-inspection-test:
# all the chains mix in the same region, and they move
# quickly through it, not getting stuck anyplace.

trankplot(m1)


# #_____________Descriptive statistics_________
# head(d)
# str(d)

# #First check tp
# min(d$tp)
# max(d$tp)
# plot(d$tp)
# plot(density(d$tp))

# #Second Check category
# library(summarytools)
# freq(d$category)

# #third check technique
# #______________Find Something________________
# min(d$technique) #  "0T" !!
# library(summarytools)
# freq(d$technique) #find three categury !!

# library(plyr)
# count(d, "subject")