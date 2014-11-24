#==============extracting dates into a vector=====
vtime=time(SEGUENEGA.incid) # extracting time vector (as date)
vtime[2]-vtime[1]

start(HOUNDA.incid)

end(HOUNDA.incid)

max(as.data.frame(HOUNDA.incid[as.Date(c("2006/11/06", "2006/05/01"))]))


dry_hounde<-window(HOUNDA.incid, end=as.Date("2006-11-27"), start=as.Date("2006/05/01")) 

sapply(HOUNDA.incid[!is.na(HOUNDA.incid)][,10],function(x) max(x))


plot(test[, 1:3]*1e+05, plot.type = "single", col = c("red", "blue"), lwd = 1)

plot(test[, 1:10]*1e+05, type="b",lwd = 1,pch=16, ylim=c(0,350))


sapply(test*1e+05,function(x) max(x))

#=====================================================
"Year 2006 district of Hounde"

dry_hounde=window(HOUNDA.incid, start = as.Date("2005-11-28"), end = as.Date("2006-05-29"))
harmattan_hounde<-window(HOUNDA.incid, start = as.Date("2005-11-28"), end = as.Date("2006-02-27"))
wet_hounde<-window(HOUNDA.incid, start = as.Date("2006-06-05"), end = as.Date("2006-10-30"))



wet_hounde_2006<-cbind(min=sapply(wet_hounde*1e+05,function(x) min(x,na.rm=FALSE)),
                  mean=sapply(wet_hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
                  max=sapply(wet_hounde*1e+05,function(x) max(x,na.rm=FALSE)))

mean(wet_hounde_2006[,1], na.rm=TRUE)
mean(wet_hounde_2006[,2], na.rm=TRUE)
mean(wet_hounde_2006[,3], na.rm=TRUE)




dry_hounde_2006<-cbind(min=sapply(dry_hounde*1e+05,function(x) min(x,na.rm=FALSE)),
      mean=sapply(dry_hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
      max=sapply(dry_hounde*1e+05,function(x) max(x,na.rm=FALSE)))

mean(dry_hounde_2006[,1], na.rm=TRUE)
mean(dry_hounde_2006[,2], na.rm=TRUE)
mean(dry_hounde_2006[,3], na.rm=TRUE)


harmattan_hounde_2006<-cbind(min=sapply(harmattan_hounde*1e+05,function(x) min(x,na.rm=FALSE)),
                       mean=sapply(harmattan_hounde*1e+05,function(x) mean(x,na.rm=TRUE)),
                       max=sapply(harmattan_hounde*1e+05,function(x) max(x,na.rm=FALSE)))


mean(harmattan_hounde_2006[,1], na.rm=TRUE)
mean(harmattan_hounde_2006[,2], na.rm=TRUE)
mean(harmattan_hounde_2006[,3], na.rm=TRUE)
#======================


# Hounde district 2004-2012

dry<-HOUNDA.incid[month(time(HOUNDA.incid))==11:12
                  |month(time(HOUNDA.incid))==1:5][,1:27]

dry_hounde_all<-cbind(min=sapply(dry*1e+05,function(x) min(x,na.rm=TRUE)),
        mean=sapply(dry*1e+05,function(x) mean(x,na.rm=TRUE)),
        max=sapply(dry*1e+05,function(x) max(x,na.rm=TRUE)))

mean(dry_hounde_all[,1], na.rm=TRUE)
mean(dry_hounde_all[,2], na.rm=TRUE)
mean(dry_hounde_all[,3], na.rm=TRUE)


wet<-HOUNDA.incid[month(time(HOUNDA.incid))==6:10]
wet_hounde_all<-cbind(min=sapply(wet*1e+05,function(x) min(x,na.rm=TRUE)),
                     mean=sapply(wet*1e+05,function(x) mean(x,na.rm=TRUE)),
                     max=sapply(wet*1e+05,function(x) max(x,na.rm=TRUE)))


mean(wet_hounde_all[,1], na.rm=TRUE)
mean(wet_hounde_all[,2], na.rm=TRUE)
mean(wet_hounde_all[,3], na.rm=TRUE)

# period harmattan
harmattan<-HOUNDA.incid[month(time(HOUNDA.incid))==c(11,12,1,2)]
harmattan_hounde_all<-cbind(min=sapply(harmattan*1e+05,function(x) min(x,na.rm=TRUE)),
                      mean=sapply(harmattan*1e+05,function(x) mean(x,na.rm=TRUE)),
                      max=sapply(harmattan*1e+05,function(x) max(x,na.rm=TRUE)))


mean(harmattan_hounde_all[,1], na.rm=TRUE)
mean(harmattan_hounde_all[,2], na.rm=TRUE)
mean(harmattan_hounde_all[,3], na.rm=TRUE)

## ORODARA DISTRICT 2004-2012

dry_orodara<-ORODARA.incid[month(time(ORODARA.incid))==11:12
                  |month(time(ORODARA.incid))==1:5]

dry_orodara_all<-cbind(min=sapply(dry_orodara*1e+05,function(x) min(x,na.rm=TRUE)),
                      mean=sapply(dry_orodara*1e+05,function(x) mean(x,na.rm=TRUE)),
                      max=sapply(dry_orodara*1e+05,function(x) max(x,na.rm=TRUE)))

mean(dry_orodara_all[,1], na.rm=TRUE)
mean(dry_orodara_all[,2], na.rm=TRUE)
mean(dry_orodara_all[,3], na.rm=TRUE)

#wet orodara

wet_orodara<-ORODARA.incid[month(time(ORODARA.incid))==6:10]

wet_orodara_all<-cbind(min=sapply(wet_orodara*1e+05,function(x) min(x,na.rm=TRUE)),
                       mean=sapply(wet_orodara*1e+05,function(x) mean(x,na.rm=TRUE)),
                       max=sapply(wet_orodara*1e+05,function(x) max(x,na.rm=TRUE)))

mean(wet_orodara_all[,1], na.rm=TRUE)
mean(wet_orodara_all[,2], na.rm=TRUE)
mean(wet_orodara_all[,3], na.rm=TRUE)

# harmattan orodara
harmattan_orodara<-ORODARA.incid[month(time(ORODARA.incid))==c(11,12,1,2)]

harmattan_orodara_all<-cbind(min=sapply(harmattan_orodara*1e+05,function(x) min(x,na.rm=TRUE)),
                       mean=sapply(harmattan_orodara*1e+05,function(x) mean(x,na.rm=TRUE)),
                       max=sapply(harmattan_orodara*1e+05,function(x) max(x,na.rm=TRUE)))

mean(harmattan_orodara_all[,1], na.rm=TRUE)
mean(harmattan_orodara_all[,2], na.rm=TRUE)
mean(harmattan_orodara_all[,3], na.rm=TRUE)
