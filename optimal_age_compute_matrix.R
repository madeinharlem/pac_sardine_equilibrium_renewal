optimal_age_compute_matrix<-function(U,Age=Age,price_a=price_a[p1,],M_a=M_a,h=h,Linf=Linf,vbK=vbK,S_a=S_a,aw=aw,bw=bw,matslope=matslope,matinflect=matinflect,LZero=Lzero,Rzero=Rzero)
{
  Fmort<- -log(U) #ln fishing mortality
  L<-rep(NA,11)
  L[1]<-Lzero
  W<-rep(NA,11)
  W[1]<-aw*(L[1]^bw)
  fmort<-rep(NA,10)
  N_nofish<-rep(NA,11)
  N_nofish1<-rep(NA,11)
  maturity<-rep(NA,11)
  N<-rep(NA,11)
  N1<-rep(NA,11)
  G<-rep(NA,11)
  FMG<-rep(NA,11)
  YPR<-rep(NA,11)
  for(i in 2:11)
  {L[i]=Linf-(Linf-L[i-1])*exp(-vbK)
  W[i]=aw*(L[i]^bw)}
  cond<-(Age-1)>=S_a
  for (i in Age)
  {
    if(cond[i]==TRUE)
    {
      fmort[i]= Fmort
    }
    if(cond[i]==FALSE)
    {
      fmort[i]= 0
    }
  }
  N_nofish=exp(-cumsum(M_a))
  N_nofish[10]<-N_nofish[10]/(1-exp(-M_a[10]))
  for(i in 2:11)
  {
    N_nofish1[i]<-N_nofish[i-1]
  }
  N_nofish1[1]<-1
  N_nofish<-N_nofish1
  for(i in 1:11)
  {
    maturity[i]<-1/(1+exp(matslope*(L[i]-matinflect)))
  }
  N=exp(-cumsum(M_a)-cumsum(fmort))
  N[10]<-N[10]/(1-exp(-M_a[10]-fmort[10]))
  for(i in 2:11)
  {
    N1[i]<-N[i-1]
  }
  N1[1]<-1
  N<-N1
  for(i in 1:10)
  {
    G[i]<-log(W[i+1])-log(W[i])
  }
  G[11]<-0
  FMG<-fmort+M_a-G
  YPR<-N*fmort*W*(1-exp(-FMG))/FMG
  #}
  SSBNofish<-sum(W*N_nofish*maturity)
  SPR<-sum(W*N*maturity)
  beta<-(h-0.2)/((1-h)*(0.2*SSBNofish*Rzero))
  alpha<-(1+beta*SSBNofish*Rzero)/(SSBNofish)
  Rinf<-(alpha*SPR-1)/(beta*SPR)
  YPRcum<-sum(YPR)
  YVPR<-sum(YPR*price_a[p1,])
  
  Catch<-YPRcum*Rinf
  Catch_value<-YVPR*Rinf
  return(Catch_value)
}
