#######Program for impact of prices on sardine equilibrium
###### Aneesh Hariharan - PhD Chapter 1
##########################################################
#####Assumptions of sardine population:
#####(A) Ages are discrete classes: 0,1, 2,3, .....10+
#####(B) Age 0 are recruits and are not fished. An initial value of 1 is assumed for age 0 recruits.
#####(C) All parameter values below are cited with reference in the write-up

##########################################################Fixed parameters

S_a<-2 #Selectivity at age - Knife edged
Linf<-23.4 # Asymptotic length
vbK<- 0.4238 #von Bertalolanfy k parameter
aw<- 7.52E-06 #Allometric param 1
bw<-3.2332 # Allometric param2
matslope<- -0.89252 #Maturity slope
matinflect<-15.44 #Maturity inflection
Lzero<- 11.38 #Initial length
#Fmort <- 0.693147181
Rzero<- 100
h<- 0.95 
#U<-0.5 #Fishing mortality
M_a<-c(0.71,0.46,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4)
Age<-seq(1,11)
optimal_age_harvest_profit<-rep(NA,length(seq(0.001,1,0.001)))
optimal_age_harvest_val<-rep(NA,length(seq(0.001,1,0.001)))
optimal_age_harvest_levels<-rep(NA,length(seq(0.001,1,0.001)))
price_a<-matrix(NA,nrow=length(seq(0.001,1,0.001)), ncol=length(seq(1,11)))
len<-length(seq(0.025,1,0.025))
len1<-length(seq(1,10))
t_seq<-seq(1,21)
price_a2<-matrix(NA,nrow=length(t_seq),ncol=length(seq(1,11)))
rev_a2<-matrix(NA,nrow=length(t_seq),ncol=length(seq(1,11)))
harv_a2<-matrix(NA,nrow=length(t_seq),ncol=length(seq(1,11)))
age_a2<-matrix(NA,nrow=length(t_seq),ncol=length(seq(1,11)))
catch_val<-matrix(NA,nrow = len1,ncol=len)
maxval_catch_age<-rep(NA,nrow(catch_val))
harvesting_level_maxval_catch_age<-rep(NA,nrow(catch_val))
price_a1<-c(0,0,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001)
for(t in seq(1,20))
{
for(a in seq(11,1,-1))
{
for(p1 in 1:length(seq(0.001,1,0.001)))
{
p<-seq(0.001,1,0.001)
#price_a[p1,]<-c(0.02545455,0.02545455,0.02545455,0.02545455,0.09580068,p[p1],0.17170977,0.17727273,0.20227273,0.22727273,0.22727273)
#price_a[p1,]<-c(0.02545455,0.02545455,0.02545455,0.02545455,0.127,0.169,0.235,0.333,0.480,0.713,0.349)
#price_a[p1,]<-c(0.02545455,0.02545455,0.02545455,0.02545455,0.127,0.169,0.235,0.333,0.480,0.713,p[p1])
#price_a[p1,]<-c(0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001)
price_a[p1,]<-price_a1
price_a[p1,a]<-p[p1]
price_a[p1,1]<-0
price_a[p1,2]<-0
#price_a[p1,a-1]<-p[p1]

##########################################################0.16614681

#Function 1: Optimal age compute matrix computes the catch values for variable fishing mortalities (0.025-1) and various selectivities
##IMPORTANT: Selectivity is asssumed to be knife edged i.e. a policy measure that implies if S_a=a; fish <=age a are let go 

for(j in 1:len1)
{
  for(i in 1:len)
  { 
    U=seq(0.025,1,0.025)
    catch_val[j,i]<-optimal_age_compute_matrix(U=U[i],S_a=j,Age = Age,
                                               price_a = price_a,
                                               M_a=M_a,h=h,
                                               Linf = Linf,vbK = vbK,aw=aw,bw=bw,
                                               LZero = LZero,Rzero = Rzero,
                                               matslope = matslope,matinflect = matinflect)
  }
}
############################################################################
#optimal_harvest_catch<-matrix(NA,nrow=nrow(catch_val),ncol=2)
for(i in seq(1,nrow(catch_val)))
{
  maxval_catch_age[i]<-max(catch_val[i,])
  harvesting_level_maxval_catch_age[i]<-0.025*which(catch_val[i,]==max(catch_val[i,]))
}
optimal_harvest_catch<-cbind(maxval_catch_age,harvesting_level_maxval_catch_age)
#plot(harvesting_level_maxval_catch_age, type="l",col="blue",ylim=c(0,2))
#lines(maxval_catch_age,type="l")
maxval_catch_age_val<-which(optimal_harvest_catch[,1]==max(optimal_harvest_catch[,1]))
optimal_age_harvest_val[p1]<-maxval_catch_age_val
maxval_catch_age<-max(maxval_catch_age)
  #optimal_age_harvest[p1]<-which(catch_val[i,]==max(catch_val[i,]))
optimal_age_harvest_profit[p1]<-maxval_catch_age
optimal_age_harvest_levels[p1]<-optimal_harvest_catch[maxval_catch_age_val,2]
final_choice<-cbind(optimal_age_harvest_val,optimal_age_harvest_levels,optimal_age_harvest_profit)
}
  for(i in seq(1,length(final_choice[,1])-1))
  {
    if (as.numeric(final_choice[(i+1),1]-final_choice[i,1])!=0)
    {
      price_a2[t,a]=0.001*(i+1)
      rev_a2[t,a]=final_choice[i+1,3]
      harv_a2[t,a]=final_choice[i+1,2]
      age_a2[t,a]=final_choice[i+1,1]
    }
  }
  price_a2[t,1]<-0
  price_a2[t,2]<-0
  price_a1[a]<-price_a2[t,a]
 }
}

#Round 1
#c(0,0.003,0.003,0.003,0.003,0.003,0.004,0.005,0.007,0.009,0.004)
#Round 2
#c(0,0.006,0.005,0.005,0.006,0.007,0.009,0.012,0.017,0.025,0.012)
#Round 3
#c(0,0.009,0.008,0.008,0.009,0.012,0.016,0.022,0.031,0.045,0.022)
#Round 4
#c(0,0.011,0.01,0.011, 0.013,0.017,0.023,0.032,0.046,0.068,0.033)
#Round 5
#c(0,0.013,0.012,0.013,0.016,0.021,0.029,0.04,0.057,0.084,0.041)
#Round 6
#c(0,0.016,0.014,0.015,0.018,0.024,0.033,0.046,0.066,0.098,0.048)
#Round 7
#c(0,0.019,0.017,0.018,0.022,0.029,0.04,0.056,0.08,0.119,0.058)
#Round 8
#c(0,0.021,0.019,0.021,0.026,0.034,0.047,0.067,0.097,0.143,0.07)
#Round 9
#c(0,0.024,0.022,0.024,0.029,0.038,0.052,0.074,0.106,0.157,0.077)
#Round 10
#c(0,0.027,0.025,0.027,0.033,0.044,0.06,0.085,0.123,0.182,0.089)
#Round 12
#c(0,0.031,0.029,0.032,0.04,0.053,0.073,0.103,0.149,0.221,0.108)
write.csv(rev_a2,'rev_sel_2.csv')
write.csv(price_a2,'price_sel_2.csv')
write.csv(harv_a2,'harv_sel_2.csv')
