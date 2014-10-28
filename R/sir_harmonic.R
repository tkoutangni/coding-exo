##################################################################################
##################################################################################
# An R script to solve ODE's of an SIR model with a harmonic transmission rate
#
# Author: Sherry Towers
#         smtowers@asu.edu
# Created: Dec 1st, 2012
#
# Copyright Sherry Towers, 2012
#
# This script is not guaranteed to be free of bugs and/or errors.
#
# This script can be freely used and shared, as long as the author information
# and copyright in the header remains intact.
##################################################################################
require("deSolve")

##################################################################################
##################################################################################
# this is a function which, given a value of S,I and R at time t
# calculates the time derivatives of S I and R
# vparameters contains the parameters of the model, like the
# recovery period, gamma, and the baseline transmission rate, beta0,
# the day of the year when transmission is maximal, phi, and the relative
# degree of seasonal forcing, epsilon
#
# Note that unlike a simple SIR model, the harmonic forcing of the transmission
# rate makes this set of ODE's non-autonomous.  This means that the results
# of the model will depend on the time of introduction of the virus, relative
# to the day of the year when transmission is maximal!
#
# this function gets passed to the ODEsolve package
##################################################################################
SIRfunc_harmonic=function(t
                         ,x
                         ,vparameters
                         ){
   S = x[1]  # the value of S at time t
   I = x[2]  # the value of I at time t
   R = x[3]  # the value of R at time t
   if (I<0) I=0 # this is a cross check to ensure that we always have sensical values of I

   with(as.list(vparameters),{
      npop=S+I+R
      beta = beta0*(1+epsilon*cos(2*pi*(t-phi)/365.25))
      dS = -beta*S*I/npop
      dI = +beta*S*I/npop - gamma*I
      dR = +gamma*I
      out = c(dS,dI,dR)
      list(out)
   })
}

##################################################################################
##################################################################################
# Let's set up some initial conditions at time t=0
##################################################################################
npop = 10000000  # the population size
I_0 = 1          # put one infected person in the population
S_0 = npop-I_0
R_0 = 0          # assume that initially no one has recovered yet

gamma = 1/3        # recovery period of influenza in days^{-1}
R0    = 1.5        # reproduction number of a hypothetical strain of pandemic influenza
beta0 = R0*gamma   # the baseline transmission rate
phi   = 0          # assume Jan 1st is day 0, and this is when flu is most transmissible
epsilon = 0.30     # assume 30% seasonal forcing of the transmission rate



par(mfrow=c(1,1))     # Divide the plotting area to show just one plot per page
ymax = 0.12           # The maximum prevalence, used when plotting... change this if the epidemic
                      # curves don't fit in the plotting area when you change the model parameters
legend = character(0) # We are going to fill this with labels for the plot legend

vt0 = c(10,60,90)     # try three different times-of-introduction

for (it0 in 1:length(vt0)){
   vt = seq(vt0[it0],400,1) # the epidemic begins at time t0
   t0 = vt0[it0]

   legend = append(legend,paste("Day epidemic begins, relative to Jan 1st=",vt0[it0],sep=""))  # we are going to use this in the plot legend

   vparameters = c(gamma=gamma,beta0=beta0,phi=phi,epsilon=epsilon) # parameters to be passed to SIRfunc_harmonic
   inits       = c(S=S_0,I=I_0,R=R_0)                               # initial values to be passed to SIRfunc_harmonic
   
   sirharm = as.data.frame(lsoda(inits, vt, SIRfunc_harmonic, vparameters))
   
   ########################################################################
   # now plot the results of the simulation
   ########################################################################
   if (it0==1){
      plot(sirharm$time,sirharm$I/npop,type="l",xlab="time, in days relative to Jan 1st",ylab="fraction infected (prevalence)",lwd=5,col=it0,ylim=c(0,ymax),main="Pandemic influenza simulation, with harmonic transmission rate")
   }else{
      lines(sirharm$time,sirharm$I/npop,lwd=5,col=it0)
   }

   ########################################################################
   # print out the final size (total fraction of the population infected)
   ########################################################################
   cat("The final size when t0 is ",t0," is ",max(sirharm$R)/npop,"\n")

   ########################################################################
   # this is a cute little trick in R to find the indices of the
   # local minima and maxima
   ########################################################################
   max_indices = which(diff(sign(diff(sirharm$I)))==-2)+1
   min_indices = which(diff(sign(diff(sirharm$I)))==+2)+1
   cat("The time(s) of the local maxima are:",sirharm$time[max_indices],"\n")
   cat("The time(s) of the local minima are:",sirharm$time[min_indices],"\n")
} # end loop over different times-of-introduction
legend("topright",legend=legend,bty="n",lwd=5,col=seq(1,length(vt0)),cex=0.7)





