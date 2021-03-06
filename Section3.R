# Section 3 Code

# For evidence of true effect

p.vals=c(0.01,0.02,0.03,0.04)
pp.vals=p.vals/0.05
t.stat=sum(-2*log(pp.vals))
pchisq(t.stat,df=6,lower.tail=FALSE)

p.vals=c(0.001,0.01,0.013,0.02)
pp.vals=p.vals/0.05
t.stat=sum(-2*log(pp.vals))
pchisq(t.stat,df=6,lower.tail=FALSE)

# For evidence of p-hacking

p.vals=c(0.047,0.048,0.049)
p.vals=c(0.05-0.047,0.05-0.048,0.05-0.049)
pp.vals=p.vals/0.05
t.stat=sum(-2*log(pp.vals))
pchisq(t.stat,df=6,lower.tail=FALSE)

# Re-estimating the treatment effect using the P-curve

loss=function(t_obs,df_obs,d_est) {
ncp_est=sqrt((df_obs+2)/4)*d_est
tc=qt(.975,df_obs)
power_est=1-pt(tc,df_obs,ncp_est)
p_larger=pt(t_obs,df=df_obs,ncp=ncp_est)
ppr=(p_larger-(1-power_est))/power_est
KSD=ks.test(ppr,punif)$statistic
return(KSD)
}

# Say we have 10 tests, with t-values 2.4, 5.7, 2.1, 3.3, 2.5, 3.1, 4.1, 3.7, 2.9, 4.2, and degrees fo freedom 
# 101, 250, 57, 77, 100, 140, 500, 234, 321, 405.

t_obs=c(2.4, 5.7, 2.1, 3.3, 2.5, 3.1, 4.1, 3.7, 2.9, 4.2)
df_obs=c(101, 250, 57, 77, 100, 140, 500, 234, 321, 405
)

optimize(loss,c(-2,2),df_obs=df_obs,t_obs=t_obs)
