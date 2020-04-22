# 99_estimate_sens_spec.R
# estimate the sensitivity and specificity of the algorithm using a gold standard check by hand (see 99_random_check.R)
# February 2020

# prior (flat prior with 1 success and 1 failure)
p = seq(0, 1, length.out = 100) # probabilities to examine
prior = dbeta(p, shape1=1, shape2=1) # prior
prior = prior / sum(prior) # scale to p 
plot(p, prior)

# posterior assuming no failures
n.check = 300
n.fail = 0
post = dbeta(p, shape1=(n.check-n.fail)+1, shape2=1+n.fail) # posterior
post = post / sum(post) # scale to p 
plot(p, post, xlim=c(0.9,1), type='b')
cat('The 95% confidence interval is ', round(1000*qbeta(0.025, shape1=(n.check-n.fail)+1, shape2=1+n.fail))/1000, 
    ' to ', round(1000*qbeta(0.975, shape1=(n.check-n.fail)+1, shape2=1+n.fail))/1000, '.\n', sep='')
cat('There is a 95% probability that the sensitivity/specificity is at least ', round(1000*qbeta(0.05, shape1=(n.check-n.fail)+1, shape2=1+n.fail))/1000, '.\n', sep='')
cat('There is a 95% probability that the error is at most ', 1- round(1000*qbeta(0.05, shape1=(n.check-n.fail)+1, shape2=1+n.fail))/1000, '.\n', sep='')

# posterior with one failure
n.fail = 1
post = dbeta(p, shape1=(n.check-n.fail)+1, shape2=1+n.fail) # posterior
post = post / sum(post) # scale to p 
plot(p, post, xlim=c(0.9,1), type='b')
cat('The 95% confidence interval is ', round(1000*qbeta(0.025, shape1=(n.check-n.fail)+1, shape2=1+n.fail))/1000, 
    ' to ', round(1000*qbeta(0.975, shape1=(n.check-n.fail)+1, shape2=1+n.fail))/1000, '.\n', sep='')
cat('There is a 95% probability that the sensitivity/specificity is at least ', round(1000*qbeta(0.05, shape1=(n.check-n.fail)+1, shape2=1+n.fail))/1000, '.\n', sep='')

# posterior with two failures
n.fail = 2
post = dbeta(p, shape1=(n.check-n.fail)+1, shape2=1+n.fail) # posterior
post = post / sum(post) # scale to p 
plot(p, post, xlim=c(0.9,1), type='b')
cat('The 95% confidence interval is ', round(1000*qbeta(0.025, shape1=(n.check-n.fail)+1, shape2=1+n.fail))/1000, 
    ' to ', round(1000*qbeta(0.975, shape1=(n.check-n.fail)+1, shape2=1+n.fail))/1000, '.\n', sep='')
cat('There is a 95% probability that the sensitivity/specificity is at least ', round(1000*qbeta(0.05, shape1=(n.check-n.fail)+1, shape2=1+n.fail))/1000, '.\n', sep='')
