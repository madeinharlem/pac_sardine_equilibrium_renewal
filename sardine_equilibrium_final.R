#######Program for impact of prices on sardine equilibrium
##########################################################
#####Assumptions of sardine population:
#####(A) Ages are discrete classes: 0,1, 2,3, .....10+
#####(B) Age 0 are recruits and are not fished. An initial value of 1 is assumed for age 0 recruits.
#####(C) All parameter values below are cited with reference in the write-up
##### No fishing of age 0 fish - Fishing starts at age 1

######################################################################
#Variable parameters: Change only required here
h<- 0.95 #########Steepness; choices seen in paper are h=0.95 an h=0.8
######################################################################################


###############Min S_a==2; Max S_a=11 #### Results are varied from 2 to 11 (see Lemma 1 and Lemma 2 in the paper.)
###############(S_a=2 will produce results to validate Lemma 1)

S_a<-3 #Price selectivity parameter; see paper

#####Note indexing: S_a=5 theoretically matches with S_a=4, since ages go from 0 to 10+
#####and in R the indices run from 1 to 11.. In general S_a=k specified below will produce theoretical results 
#####for S_a=k-1
#####E.g. S_a=5 specified below implies 4 age classes are let go i.e. 0,1,2,3, which theoretically amounts to S_a=4.
#####Thus, ensure that 1 is added to the theoretical price selectivity chosen.

######################################################################################
########Initializing prices Uniform (0.001)
price_a1<-c(0,0.001,0.001,	0.001,	0.001,	0.001,	0.001,	0.001,	0.001,	0.001,	0.001)

#########Match prices E.g. S_a=2 specified above
#########implies theoretically S_a=1 implies 
########price_a1<-c(0,0.001,0.001,	0.001,	0.001,	0.001,	0.001,	0.001,	0.001,	0.001,	0.001)


############# Fixed Parameters ()
Linf<-23.4 # Asymptotic length
vbK<- 0.4238 #von Bertalolanfy k parameter
aw<- 7.52E-06 #Allometric param 1
bw<-3.2332 # Allometric param2
matslope<- -0.89252 #Maturity slope
matinflect<-15.44 #Maturity inflection
Lzero<- 11.38 #Initial length
M_a<-c(0.71,0.46,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4)####Natural mortality
Rzero<- 4.828*10^9 ##Equilibrium recruitment



#########################################################################
#Simulation details
Age<-seq(1,11)#(TUNABLE)
price_steps<-seq(0.001,1,0.001) #(TUNABLE)
harvest_rates<-seq(0.025,1,0.025) #(TUNABLE)
t_seq<-seq(1,30)####Number of steps (#TUNABLE)
#########################################################################

#########################################################################
#Initilizing vectors and matrices
optimal_age_harvest_profit<-rep(NA,length(price_steps))
optimal_age_harvest_val<-rep(NA,length(price_steps))
optimal_age_harvest_levels<-rep(NA,length(price_steps))
price_a<-matrix(NA,nrow=length(price_steps), ncol=length(Age))
len<-length(harvest_rates)
len1<-length(Age-1)
price_a2<-matrix(NA,nrow=length(t_seq),ncol=length(Age))
rev_a2<-matrix(NA,nrow=length(t_seq),ncol=length(Age))
harv_a2<-matrix(NA,nrow=length(t_seq),ncol=length(Age))
age_a2<-matrix(NA,nrow=length(t_seq),ncol=length(Age))
catch_val<-matrix(NA,nrow = len1,ncol=len)
maxval_catch_age<-rep(NA,nrow(catch_val))
harvesting_level_maxval_catch_age<-rep(NA,nrow(catch_val))
#########################################################################


#########################################################################
#Main code
for(t in t_seq)
{
  for(a in rev(Age))
  {
    for(p1 in 1:length(seq(0.001,1,0.001)))
    {
      for(sel in seq(1,S_a-1))
      {
      price_a2[t,sel]<-0
      }
      p<-seq(0.001,1,0.001)
      price_a[p1,]<-price_a1
      price_a[p1,a]<-p[p1]
      for(sel in seq(1,S_a-1))
      {
      price_a[p1,sel]<-0
      }
      
      #Function 1: Optimal age compute matrix computes the catch values for variable fishing mortalities (0.025-1) and various selectivities
      ##IMPORTANT: Selectivity is asssumed to be knife edged i.e. a policy measure that implies if S_a=a; fish <age a are let go 
      
      for(j in 1:len1)
      {
        for(i in 1:len)
        { 
          U=harvest_rates
          ###################################################################################################
          #Call to inner function optimal_age_compute_matrix
          catch_val[j,i]<-optimal_age_compute_matrix(U=U[i],S_a=j,Age = Age,
                                                     price_a = price_a,
                                                     M_a=M_a,h=h,
                                                     Linf = Linf,vbK = vbK,aw=aw,bw=bw,
                                                     LZero = LZero,Rzero = Rzero,
                                                     matslope = matslope,matinflect = matinflect)
        }
      }
      #################################################################################################
      for(i in seq(1,nrow(catch_val)))
      {
        maxval_catch_age[i]<-max(catch_val[i,])
        harvesting_level_maxval_catch_age[i]<-(harvest_rates[2]-harvest_rates[1])*which(catch_val[i,]==max(catch_val[i,]))
      }
      optimal_harvest_catch<-cbind(maxval_catch_age,harvesting_level_maxval_catch_age)
      maxval_catch_age_val<-which(optimal_harvest_catch[,1]==max(optimal_harvest_catch[,1]))
      optimal_age_harvest_val[p1]<-maxval_catch_age_val
      maxval_catch_age1<-max(maxval_catch_age)
      optimal_age_harvest_profit[p1]<-maxval_catch_age1
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
    if(S_a>=3)
    {
    for(r in seq(2,S_a-1))
    {
      rev_a2[t,S_a-r+1]=as.numeric(optimal_harvest_catch[S_a-r,1])
      harv_a2[t,S_a-r+1]=as.numeric(optimal_harvest_catch[S_a-r,2])
    }
    }
    print(rev_a2[t,a])
    price_a1[a]<-price_a2[t,a]
  }
}

#write.csv(rev_a2,'rev_sel_1_results.csv')
#write.csv(price_a2,'price_sel_1_results.csv')
#write.csv(harv_a2,'harv_sel_1_results.csv')
