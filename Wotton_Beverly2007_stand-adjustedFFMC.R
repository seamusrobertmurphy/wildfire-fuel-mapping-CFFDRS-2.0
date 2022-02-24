

# Cite as: Wotton, B. M., & Beverly, J. L. (2007). Stand-specific litter moisture content calibrations 
# for the Canadian Fine Fuel Moisture Code. International Journal of Wildland Fire, 16(4), 463-472.

#base R - no packages required

#define mcF, mcD, ex.mod intermediate functions
mcF<-function(ffmc){
  147.2*(101-ffmc)/(59.5+ffmc)
}
mcD<-function(dmc) {
  20+exp(-(dmc-244.72)/43.43)
}
ex.mod<-function(s1, s2, s3, ffmc, dmc) {
  exp(s1+s2*log(mcF(ffmc))+s3*mcD(dmc))
}


#define stand-adjusted mc function
#FFMC, DMC, stand, density, season
samc<-function(ffmc, dmc, stand, density, season) {
  #Get coefficients
  CoTr1 <-c(
    0.7299,0.0202,0.7977,0.8517,0.7391,
    0.4387,-0.271,0.5065,0.5605,0.4479,
    -0.2449,-0.9546,-0.1771,-0.1231,-0.2357,
    0.1348,-0.5749,0.2026,0.2566,0.144,
    -0.1564,-0.8661,-0.0886,-0.0346,-0.1472,
    -0.84,-1.5497,-0.7722,-0.7182,-0.8308,
    0.1601,-0.55,0.2279,0.2819,0.1693,
    -0.1311,-0.8408,-0.0633,-0.0093,-0.1219,
    -0.8147,-1.5244,-0.7469,-0.6929,-0.8055)
  CoTr2 <- c(
    0.5221,0.6264,0.5042,0.3709,0.4285,
    0.7133,0.8176,0.6954,0.5621,0.6197,
    1.0462,1.1505,1.0283,0.895,0.9526,
    0.8691,0.9734,0.8512,0.7179,0.7755,
    1.0603,1.1646,1.0424,0.9091,0.9667,
    1.3932,1.4975,1.3753,1.242,1.2996,
    0.9495,1.0538,0.9316,0.7983,0.8559,
    1.1407,1.245,1.1228,0.9895,1.0471,
    1.4736,1.5779,1.4557,1.3224,1.38)
  co3 <- 0.002232
  
  #Create data frame for pulling coefs
  AllCo <-data.frame("co1"=CoTr1, "co2"=CoTr2)
  
  #Spring and Summer coefs for 'sprummer' model
  co_sp <- AllCo[1:15,]
  co_su <- AllCo[16:30,]
  
  
  if(season==1.5) {
    #spring
    c1.sprD=co_sp[(density*5-4):(density*5), 1]
    c2.sprD=co_sp[(density*5-4):(density*5), 2]
    c1=c1.sprD[stand]
    c2=c2.sprD[stand]
    mc.spr=ex.mod(c1, c2, co3, ffmc, dmc)   
    #summer 
    c1.sumD=co_su[(density*5-4):(density*5), 1]
    c2.sumD=co_su[(density*5-4):(density*5), 2]
    c3=c1.sumD[stand]
    c4=c2.sumD[stand]
    mc.sum=ex.mod(c3, c4, co3, ffmc, dmc)  
    
    #final 'sprummer' mc calc
    return(mean(c(mc.spr, mc.sum)))
    
    #for all others - spring, summer or fall
  } else {
    c1a=AllCo$co1[(season*15-14):(season*15)]
    c2a=AllCo$co2[(season*15-14):(season*15)]
    c1b=c1a[(density*5-4):(density*5)]
    c2b=c2a[(density*5-4):(density*5)]
    c1c=c1b[stand]
    c2c=c2b[stand]
    return(ex.mod(c1c, c2c, co3, ffmc, dmc))
  }
}

#[End]
#
