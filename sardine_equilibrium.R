#######Program for impact of prices on sardine equilibrium
###### Aneesh Hariharan - PhD Chapter 1
##########################################################
#####Assumptions of sardine population:
#####(A) Ages are discrete classes: 0,1, 2,3, .....10+
#####(B) Age 0 are recruits and are not fished. An initial value of 1 is assumed for age 0 recruits.
#####(C) All parameter values below are cited with reference in the write-up

#######################################################################################
S_a<-1 #Selectivity at age - Knife edged
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
optimal_age_harvest_profit<-rep(NA,length(seq(0.001,2,0.001)))
optimal_age_harvest_val<-rep(NA,length(seq(0.001,2,0.001)))
optimal_age_harvest_levels<-rep(NA,length(seq(0.001,2,0.001)))
price_a<-matrix(NA,nrow=length(seq(0.001,2,0.001)), ncol=length(seq(1,11)))
len<-length(seq(0.025,1,0.025))
len1<-length(seq(1,10))
catch_val<-matrix(NA,nrow = len1,ncol=len)
maxval_catch_age<-rep(NA,nrow(catch_val))
harvesting_level_maxval_catch_age<-rep(NA,nrow(catch_val))


for(p1 in 1:length(seq(0.001,2,0.001)))
{
p<-seq(0.001,2,0.001)
#price_a[p1,]<-c(0.02545455,0.02545455,0.02545455,0.02545455,0.09580068,p[p1],0.17170977,0.17727273,0.20227273,0.22727273,0.22727273)
price_a[p1,]<-c(0.02545455,0.02545455,0.02545455,0.02545455,0.09580068,p[p1],0.17170977,0.17727273,0.20227273,0.22727273,0.22727273)
##########################################################
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
plot(harvesting_level_maxval_catch_age, type="l",col="blue",ylim=c(0,2))
lines(maxval_catch_age,type="l")
maxval_catch_age_val<-which(optimal_harvest_catch[,1]==max(optimal_harvest_catch[,1]))
optimal_age_harvest_val[p1]<-maxval_catch_age_val
maxval_catch_age<-max(maxval_catch_age)
  #optimal_age_harvest[p1]<-which(catch_val[i,]==max(catch_val[i,]))
optimal_age_harvest_profit[p1]<-maxval_catch_age
optimal_age_harvest_levels[p1]<-optimal_harvest_catch[maxval_catch_age_val,2]
}
final_choice<-cbind(optimal_age_harvest_val,optimal_age_harvest_levels,optimal_age_harvest_profit)
