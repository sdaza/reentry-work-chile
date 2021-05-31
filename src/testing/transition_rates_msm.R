jobcrime_msm = tranSequence(seq_data_anyjobcrime, cal_covs)
jobcrime_msm[state == 11, state := 3]
jobcrime_msm[state == 12, state := 4]
table(jobcrime_msm$state)

q = matrix(rep(0.1, 4*4), 4, 4, byrow = TRUE)
colnames(q) = labs
rownames(q) = labs

m0 =  msm(state ~ time, subject = id, data = jobcrime_msm, gen.inits = TRUE, qmatrix = q, 
    method = "BFGS", control = list(fnscale = 4000, maxit = 10000))

hazard.msm(m0)
pmatrix.msm(m0, t = 1, ci = "normal")
pmatrix.msm(m0, t = 50, ci = "normal")
