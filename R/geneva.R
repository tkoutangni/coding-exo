##################################################################################
# Authors: Ilyssa Summer, Na Zhang, Anarina Murillo, S Towers
# 
# Created: March 20, 2013
#
# This script is not guaranteed to be free of bugs and/or errors.
#
# This script can be freely used and shared as long as the author and
# copyright information in this header remain intact.
##################################################################################

##################################################################################
# first load the required libraries
##################################################################################
require("deSolve")
require("sfsmisc")
require("chron")

##################################################################################
##################################################################################
# First define the function that will be used by the lsoda() function in the
# deSolve library to solve the SIR compartmental model
# This function doesn't need to be named fun... you can name it anything you want.
##################################################################################
fun=function(t
            ,x
            ,parameters
            ){
     S=x[1]
     I=x[2]
     R=x[3]
     S[S<0] = 0 # check to ensure that S, I and R are never less than zero
     I[I<0] = 0
     R[R<0] = 0
     with(as.list(parameters),{
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
# geneva data starts Jul 1, 1918
# population in 1918 was around 120,000
# http://en.wikipedia.org/wiki/Geneva#Historic_population
##################################################################################
gdat=read.table("data/geneva.dat",header=T)

##################################################################################
# calculate the time, relative to Jan 1st, 1918
##################################################################################
gdat$julian = julian(gdat$month
                    ,gdat$day
                    ,gdat$year) # days, relative to Jan 1st, 1970

gdat$time = gdat$julian-julian(1,1,1918) # days relative Jan 1st 1918

##################################################################################
# convert the julian day vector to a Date class vector, and then use this
# to convert to the week of the year, and weekday (0 is Sunday and 6 is Saturday)
# For more examples of formatting dates, look at 
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
##################################################################################
gdat$date=as.Date(gdat$julian
                 ,origin="1970-01-01")
gdat$weekday=as.numeric(format(gdat$date,"%w"))

##################################################################################
# these model parameters were estimated by Zhang, Summer, and Murillo
# Carrat et al "Timelines of Infection and Disease in Human Influenza"
# http://aje.oxfordjournals.org/content/167/7/775
##################################################################################
gamma      = 1/4.8    # infectious period of flu, estimated by Carrat et al.
population = 120000   # population of Geneva in 1918
t0         = 166      # time-of-introduction of the virus, relative to Jan 1st, 1918
epsilon    = 0.8165   # relative seasonal forcing of transmission rate
R0         = 5.367495 # basic reproduction number
phi        = 44       # day, relative to Jan 1st, that virus is most transmissible
beta0 = R0*gamma

fimmune = 0 # assume that the pre-immune fraction in the population is zero
I_0     = 1 # start with one infected person in the population
S_0     = population*(1-fimmune)-I_0
R_0     = fimmune*population

#########################################################################
# Solve the model between the time-of-introduction to the maximum
# value of the time in the data 
# The results are put in the flumodel object
#########################################################################
parameters = c(gamma,beta0,phi,epsilon) # vector of parameters
initial    = c(S=S_0,I=I_0,R=R_0)       # vector of initial values                         
vt         = seq(t0,max(gdat$time),1)   # times at which we want the model estimates
flumodel   = as.data.frame(lsoda(initial, vt, fun, parameters))

#########################################################################
# Calculate the daily incidence (which is the change in S each day)
# and then normalize such that the model prediction for
# the incidence sums to the data
# We only show the model fit up to day 314, which was Armistice Day.
#########################################################################
time = flumodel$time
newI = -diff(flumodel$S)
newI = append(0,newI)
newI = newI*sum(gdat$num[gdat$time<314])/sum(newI[time>=min(gdat$time)&time<314])

newI = newI[flumodel$time>=min(gdat$time)
           &flumodel$time<=max(gdat$time)]
time = time[flumodel$time>=min(gdat$time)
           &flumodel$time<=max(gdat$time)]

#########################################################################
# now let's calculate the residuals (data minus the model)
#########################################################################
vresiduals = gdat$num-newI
vresiduals = vresiduals[gdat$time<314]
vweekday   = gdat$weekday[gdat$time<314]

gmean = aggregate(vresiduals
                 ,by=list(vweekday)
                 ,FUN="mean")

#########################################################################
# now plot the results
# The mult.fig function is from the sfsmisc library, and provides a 
# nice means of dividing up the plotting area, and putting a title
# at the top of the page
#########################################################################
mult.fig(2  # number of plotting areas per page
        ,main="Geneva 1918 data") 

plot(gdat$time
    ,gdat$num
    ,pch=20     # type of point: solid dot
    ,xlab="Days, relative to Jan 1st 1918"
    ,ylab="Incidence"
    ,ylim=c(0,1.2*max(gdat$num))) # leave 20% free space at the top

arrows(314    # beginning x
      ,200    # beginning y
      ,314    # end x
      ,120    # end y
      ,col=4
      ,lwd=4
      ,length=0.10) # length of head of arrow

text(317  # x
    ,235  # y
    ,"Armistice"
    ,col=4
    ,cex=0.8)  # relative size of text (1 is default)

lines(time
     ,newI
     ,col=2
     ,lwd=4)

legend("topleft"
      ,legend=c("Geneva data","Fitted model (up to Armistice)")
      ,col=c(1,2)
      ,lwd=4
      ,bty="n") # don't plot a box around the legend

#########################################################################
# also plot the residuals by weekday.
#########################################################################
plot(gmean[[1]]
    ,gmean[[2]]
    ,ylab="Avg of residuals within weekday"
    ,xlab=""
    ,xaxt="n"  # dont plot the x-axis ticks
    ,xaxs="i"  # make the axes meet at the min value of x and y
    ,yaxs="i"
    ,type="l"
    ,lwd=5
    ,col="darkgreen"
    ,ylim=c(-8,8)
    ,xlim=c(-0.5,6.5))
vname=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
axis(side=1  # side 1 is the bottom, side 2 is the LHS, side 3 is the top, and side 4 is the RHS
    ,las=1   # plot the labels parallel to the axis (2 is perpendicular)
    ,labels=vname
    ,at=seq(0,6)
    ,cex.axis=0.8)


   
