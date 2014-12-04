

##===================================================
# Dry harmattan lena

## fs avec foyer epidemique
## Harmattan
dry.harmattan.lena_fe<-dry.harmattan.lena[,fe_fs_lena.index]# health centers with outbreak
dry.harmattan.lena_fe_2006<-extract(dry.harmattan.lena_fe,2006)# removing epidemic year 2006

dry.harmattan.lena_fe.incid<-cbind(min=sapply(dry.harmattan.lena_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                     mean=sapply(dry.harmattan.lena_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                     max=sapply(dry.harmattan.lena_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.lena_fe.incid
mean(dry.harmattan.lena_fe.incid[,1], na.rm=TRUE)
mean(dry.harmattan.lena_fe.incid[,2], na.rm=TRUE)
mean(dry.harmattan.lena_fe.incid[,3], na.rm=TRUE)

dry.harmattan.lena_fe_2006.incid<-cbind(min=sapply(dry.harmattan.lena_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                          mean=sapply(dry.harmattan.lena_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                          max=sapply(dry.harmattan.lena_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.lena_fe_2006.incid
mean(dry.harmattan.lena_fe_2006.incid[,1], na.rm=TRUE)
mean(dry.harmattan.lena_fe_2006.incid[,2], na.rm=TRUE)
mean(dry.harmattan.lena_fe_2006.incid[,3], na.rm=TRUE)

## Dry hot

dry.hot.lena_fe<-dry.hot.lena[,fe_fs_lena.index]# health centers with outbreak
dry.hot.lena_fe_2006<-extract(dry.hot.lena_fe,2006)# removing epidemic year 2006

dry.hot.lena_fe.incid<-cbind(min=sapply(dry.hot.lena_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                               mean=sapply(dry.hot.lena_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                               max=sapply(dry.hot.lena_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.hot.lena_fe.incid
mean(dry.hot.lena_fe.incid[,1], na.rm=TRUE)
mean(dry.hot.lena_fe.incid[,2], na.rm=TRUE)
mean(dry.hot.lena_fe.incid[,3], na.rm=TRUE)

dry.hot.lena_fe_2006.incid<-cbind(min=sapply(dry.hot.lena_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                    mean=sapply(dry.hot.lena_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                    max=sapply(dry.hot.lena_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
dry.hot.lena_fe_2006.incid
mean(dry.hot.lena_fe_2006.incid[,1], na.rm=TRUE)
mean(dry.hot.lena_fe_2006.incid[,2], na.rm=TRUE)
mean(dry.hot.lena_fe_2006.incid[,3], na.rm=TRUE)


## Wet (lena)

wet.lena_fe<-wet.lena[,fe_fs_lena.index]# health centers with outbreak
wet.lena_fe_2006<-extract(wet.lena_fe,2006)# removing epidemic year 2006

wet.lena_fe.incid<-cbind(min=sapply(wet.lena_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                           mean=sapply(wet.lena_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                           max=sapply(wet.lena_fe*1e+05,function(x) max(x,na.rm=TRUE)))

wet.lena_fe.incid
mean(wet.lena_fe.incid[,1], na.rm=TRUE)
mean(wet.lena_fe.incid[,2], na.rm=TRUE)
mean(wet.lena_fe.incid[,3], na.rm=TRUE)

wet.lena_fe_2006.incid<-cbind(min=sapply(wet.lena_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                mean=sapply(wet.lena_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                max=sapply(wet.lena_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
wet.lena_fe_2006.incid
mean(wet.lena_fe_2006.incid[,1], na.rm=TRUE)
mean(wet.lena_fe_2006.incid[,2], na.rm=TRUE)
mean(wet.lena_fe_2006.incid[,3], na.rm=TRUE)



## fs sans foyer epidemique (lena)

## Harmattan
dry.harmattan.lena_non_fe<-dry.harmattan.lena[,non_fe_fs_lena.index]# health centers with outbreak
dry.harmattan.lena_non_fe_2006<-extract(dry.harmattan.lena_non_fe,2006)# removing epidemic year 2006

dry.harmattan.lena_non_fe.incid<-cbind(min=sapply(dry.harmattan.lena_non_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                         mean=sapply(dry.harmattan.lena_non_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                         max=sapply(dry.harmattan.lena_non_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.lena_non_fe.incid
mean(dry.harmattan.lena_non_fe.incid[,1], na.rm=TRUE)
mean(dry.harmattan.lena_non_fe.incid[,2], na.rm=TRUE)
mean(dry.harmattan.lena_non_fe.incid[,3], na.rm=TRUE)

dry.harmattan.lena_non_fe_2006.incid<-cbind(min=sapply(dry.harmattan.lena_non_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                              mean=sapply(dry.harmattan.lena_non_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                              max=sapply(dry.harmattan.lena_non_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.lena_non_fe_2006.incid[1:6,]
mean(dry.harmattan.lena_non_fe_2006.incid[1:6,1], na.rm=TRUE)
mean(dry.harmattan.lena_non_fe_2006.incid[1:6,2], na.rm=TRUE)
mean(dry.harmattan.lena_non_fe_2006.incid[1:6,3], na.rm=TRUE)

## Dry hot

dry.hot.lena_non_fe<-dry.hot.lena[,non_fe_fs_lena.index]# health centers with outbreak
dry.hot.lena_non_fe_2006<-extract(dry.hot.lena_non_fe,2006)# removing epidemic year 2006

dry.hot.lena_non_fe.incid<-cbind(min=sapply(dry.hot.lena_non_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                   mean=sapply(dry.hot.lena_non_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                   max=sapply(dry.hot.lena_non_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.hot.lena_non_fe.incid
mean(dry.hot.lena_non_fe.incid[,1], na.rm=TRUE)
mean(dry.hot.lena_non_fe.incid[,2], na.rm=TRUE)
mean(dry.hot.lena_non_fe.incid[,3], na.rm=TRUE)

dry.hot.lena_non_fe_2006.incid<-cbind(min=sapply(dry.hot.lena_non_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                        mean=sapply(dry.hot.lena_non_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                        max=sapply(dry.hot.lena_non_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
dry.hot.lena_non_fe_2006.incid[1:6,]# removed hs with no or very little data 
mean(dry.hot.lena_non_fe_2006.incid[1:6,1], na.rm=TRUE)
mean(dry.hot.lena_non_fe_2006.incid[1:6,2], na.rm=TRUE)
mean(dry.hot.lena_non_fe_2006.incid[1:6,3], na.rm=TRUE)


## Wet (lena)

wet.lena_non_fe<-wet.lena[,non_fe_fs_lena.index]# health centers with outbreak
wet.lena_non_fe_2006<-extract(wet.lena_non_fe,2006)# removing epidemic year 2006

wet.lena_non_fe.incid<-cbind(min=sapply(wet.lena_non_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                               mean=sapply(wet.lena_non_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                               max=sapply(wet.lena_non_fe*1e+05,function(x) max(x,na.rm=TRUE)))

wet.lena_non_fe.incid
mean(wet.lena_non_fe.incid[,1], na.rm=TRUE)
mean(wet.lena_non_fe.incid[,2], na.rm=TRUE)
mean(wet.lena_non_fe.incid[,3], na.rm=TRUE)

wet.lena_non_fe_2006.incid<-cbind(min=sapply(wet.lena_non_fe_2006*1e+05,function(x) min(x,na.rm=FALSE)),
                                    mean=sapply(wet.lena_non_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                    max=sapply(wet.lena_non_fe_2006*1e+05,function(x) max(x,na.rm=FALSE)))
wet.lena_non_fe_2006.incid
mean(wet.lena_non_fe_2006.incid[,1], na.rm=TRUE)
mean(wet.lena_non_fe_2006.incid[,2], na.rm=TRUE)
mean(wet.lena_non_fe_2006.incid[,3], na.rm=TRUE)

## end of lena district estimations  # lena
##=======================================================================


# Dry harmattan K.VIGUE

## fs avec foyer epidemique
## Harmattan
dry.harmattan.k.vigue_fe<-dry.harmattan.k.vigue[,fe_fs_k.vigue.index]# health centers with outbreak
dry.harmattan.k.vigue_fe_2006<-extract(dry.harmattan.k.vigue_fe,2006)# removing epidemic year 2006

dry.harmattan.k.vigue_fe.incid<-cbind(min=sapply(dry.harmattan.k.vigue_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                   mean=sapply(dry.harmattan.k.vigue_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                   max=sapply(dry.harmattan.k.vigue_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.k.vigue_fe.incid
mean(dry.harmattan.k.vigue_fe.incid[,1], na.rm=TRUE)
mean(dry.harmattan.k.vigue_fe.incid[,2], na.rm=TRUE)
mean(dry.harmattan.k.vigue_fe.incid[,3], na.rm=TRUE)

dry.harmattan.k.vigue_fe_2006.incid<-cbind(min=sapply(dry.harmattan.k.vigue_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                        mean=sapply(dry.harmattan.k.vigue_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                        max=sapply(dry.harmattan.k.vigue_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.k.vigue_fe_2006.incid
mean(dry.harmattan.k.vigue_fe_2006.incid[,1], na.rm=TRUE)
mean(dry.harmattan.k.vigue_fe_2006.incid[,2], na.rm=TRUE)
mean(dry.harmattan.k.vigue_fe_2006.incid[,3], na.rm=TRUE)

## Dry hot

dry.hot.k.vigue_fe<-dry.hot.k.vigue[,fe_fs_k.vigue.index]# health centers with outbreak
dry.hot.k.vigue_fe_2006<-extract(dry.hot.k.vigue_fe,2006)# removing epidemic year 2006

dry.hot.k.vigue_fe.incid<-cbind(min=sapply(dry.hot.k.vigue_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                             mean=sapply(dry.hot.k.vigue_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                             max=sapply(dry.hot.k.vigue_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.hot.k.vigue_fe.incid
mean(dry.hot.k.vigue_fe.incid[,1], na.rm=TRUE)
mean(dry.hot.k.vigue_fe.incid[,2], na.rm=TRUE)
mean(dry.hot.k.vigue_fe.incid[,3], na.rm=TRUE)

dry.hot.k.vigue_fe_2006.incid<-cbind(min=sapply(dry.hot.k.vigue_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                  mean=sapply(dry.hot.k.vigue_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                  max=sapply(dry.hot.k.vigue_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
dry.hot.k.vigue_fe_2006.incid
mean(dry.hot.k.vigue_fe_2006.incid[,1], na.rm=TRUE)
mean(dry.hot.k.vigue_fe_2006.incid[,2], na.rm=TRUE)
mean(dry.hot.k.vigue_fe_2006.incid[,3], na.rm=TRUE)


## Wet (k.vigue)

wet.k.vigue_fe<-wet.k.vigue[,fe_fs_k.vigue.index]# health centers with outbreak
wet.k.vigue_fe_2006<-extract(wet.k.vigue_fe,2006)# removing epidemic year 2006

wet.k.vigue_fe.incid<-cbind(min=sapply(wet.k.vigue_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                         mean=sapply(wet.k.vigue_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                         max=sapply(wet.k.vigue_fe*1e+05,function(x) max(x,na.rm=TRUE)))

wet.k.vigue_fe.incid
mean(wet.k.vigue_fe.incid[,1], na.rm=TRUE)
mean(wet.k.vigue_fe.incid[,2], na.rm=TRUE)
mean(wet.k.vigue_fe.incid[,3], na.rm=TRUE)

wet.k.vigue_fe_2006.incid<-cbind(min=sapply(wet.k.vigue_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                              mean=sapply(wet.k.vigue_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                              max=sapply(wet.k.vigue_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
wet.k.vigue_fe_2006.incid
mean(wet.k.vigue_fe_2006.incid[,1], na.rm=TRUE)
mean(wet.k.vigue_fe_2006.incid[,2], na.rm=TRUE)
mean(wet.k.vigue_fe_2006.incid[,3], na.rm=TRUE)



## fs sans foyer epidemique (k.vigue)

## Harmattan
dry.harmattan.k.vigue_non_fe<-dry.harmattan.k.vigue[,non_fe_fs_k.vigue.index]# health centers with outbreak
dry.harmattan.k.vigue_non_fe_2006<-extract(dry.harmattan.k.vigue_non_fe,2006)# removing epidemic year 2006

dry.harmattan.k.vigue_non_fe.incid<-cbind(min=sapply(dry.harmattan.k.vigue_non_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                       mean=sapply(dry.harmattan.k.vigue_non_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                       max=sapply(dry.harmattan.k.vigue_non_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.k.vigue_non_fe.incid
mean(dry.harmattan.k.vigue_non_fe.incid[,1], na.rm=TRUE)
mean(dry.harmattan.k.vigue_non_fe.incid[,2], na.rm=TRUE)
mean(dry.harmattan.k.vigue_non_fe.incid[,3], na.rm=TRUE)

dry.harmattan.k.vigue_non_fe_2006.incid<-cbind(min=sapply(dry.harmattan.k.vigue_non_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                            mean=sapply(dry.harmattan.k.vigue_non_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                            max=sapply(dry.harmattan.k.vigue_non_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.k.vigue_non_fe_2006.incid[1:4,]
mean(dry.harmattan.k.vigue_non_fe_2006.incid[1:4,1], na.rm=TRUE)
mean(dry.harmattan.k.vigue_non_fe_2006.incid[1:4,2], na.rm=TRUE)
mean(dry.harmattan.k.vigue_non_fe_2006.incid[1:4,3], na.rm=TRUE)

## Dry hot

dry.hot.k.vigue_non_fe<-dry.hot.k.vigue[,non_fe_fs_k.vigue.index]# health centers with outbreak
dry.hot.k.vigue_non_fe_2006<-extract(dry.hot.k.vigue_non_fe,2006)# removing epidemic year 2006

dry.hot.k.vigue_non_fe.incid<-cbind(min=sapply(dry.hot.k.vigue_non_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                 mean=sapply(dry.hot.k.vigue_non_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                 max=sapply(dry.hot.k.vigue_non_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.hot.k.vigue_non_fe.incid
mean(dry.hot.k.vigue_non_fe.incid[,1], na.rm=TRUE)
mean(dry.hot.k.vigue_non_fe.incid[,2], na.rm=TRUE)
mean(dry.hot.k.vigue_non_fe.incid[,3], na.rm=TRUE)

dry.hot.k.vigue_non_fe_2006.incid<-cbind(min=sapply(dry.hot.k.vigue_non_fe_2006*1e+05,function(x) min(x,na.rm=FALSE)),
                                      mean=sapply(dry.hot.k.vigue_non_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                      max=sapply(dry.hot.k.vigue_non_fe_2006*1e+05,function(x) max(x,na.rm=FALSE)))
dry.hot.k.vigue_non_fe_2006.incid
mean(dry.hot.k.vigue_non_fe_2006.incid[,1], na.rm=TRUE)
mean(dry.hot.k.vigue_non_fe_2006.incid[,2], na.rm=TRUE)
mean(dry.hot.k.vigue_non_fe_2006.incid[,3], na.rm=TRUE)


## Wet (k.vigue)

wet.k.vigue_non_fe<-wet.k.vigue[,non_fe_fs_k.vigue.index]# health centers with outbreak
wet.k.vigue_non_fe_2006<-extract(wet.k.vigue_non_fe,2006)# removing epidemic year 2006

wet.k.vigue_non_fe.incid<-cbind(min=sapply(wet.k.vigue_non_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                             mean=sapply(wet.k.vigue_non_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                             max=sapply(wet.k.vigue_non_fe*1e+05,function(x) max(x,na.rm=TRUE)))

wet.k.vigue_non_fe.incid
mean(wet.k.vigue_non_fe.incid[,1], na.rm=TRUE)
mean(wet.k.vigue_non_fe.incid[,2], na.rm=TRUE)
mean(wet.k.vigue_non_fe.incid[,3], na.rm=TRUE)

wet.k.vigue_non_fe_2006.incid<-cbind(min=sapply(wet.k.vigue_non_fe_2006*1e+05,function(x) min(x,na.rm=FALSE)),
                                  mean=sapply(wet.k.vigue_non_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                  max=sapply(wet.k.vigue_non_fe_2006*1e+05,function(x) max(x,na.rm=FALSE)))
wet.k.vigue_non_fe_2006.incid
mean(wet.k.vigue_non_fe_2006.incid[,1], na.rm=TRUE)
mean(wet.k.vigue_non_fe_2006.incid[,2], na.rm=TRUE)
mean(wet.k.vigue_non_fe_2006.incid[,3], na.rm=TRUE)

## end of k.vigue district estimations 
##=======================================================================

# Dry harmattan SEGUENEGA

## fs avec foyer epidemique
## Harmattan
dry.harmattan.seguenega_fe<-dry.harmattan.seguenega[,fe_fs_seguenega.index]# health centers with outbreak
dry.harmattan.seguenega_fe_2006<-extract(dry.harmattan.seguenega_fe,2006)# removing epidemic year 2006

dry.harmattan.seguenega_fe.incid<-cbind(min=sapply(dry.harmattan.seguenega_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                      mean=sapply(dry.harmattan.seguenega_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                      max=sapply(dry.harmattan.seguenega_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.seguenega_fe.incid
mean(dry.harmattan.seguenega_fe.incid[,1], na.rm=TRUE)
mean(dry.harmattan.seguenega_fe.incid[,2], na.rm=TRUE)
mean(dry.harmattan.seguenega_fe.incid[,3], na.rm=TRUE)

dry.harmattan.seguenega_fe_2006.incid<-cbind(min=sapply(dry.harmattan.seguenega_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                           mean=sapply(dry.harmattan.seguenega_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                           max=sapply(dry.harmattan.seguenega_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.seguenega_fe_2006.incid
mean(dry.harmattan.seguenega_fe_2006.incid[,1], na.rm=TRUE)
mean(dry.harmattan.seguenega_fe_2006.incid[,2], na.rm=TRUE)
mean(dry.harmattan.seguenega_fe_2006.incid[,3], na.rm=TRUE)

## Dry hot

dry.hot.seguenega_fe<-dry.hot.seguenega[,fe_fs_seguenega.index]# health centers with outbreak
dry.hot.seguenega_fe_2006<-extract(dry.hot.seguenega_fe,2006)# removing epidemic year 2006

dry.hot.seguenega_fe.incid<-cbind(min=sapply(dry.hot.seguenega_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                mean=sapply(dry.hot.seguenega_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                max=sapply(dry.hot.seguenega_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.hot.seguenega_fe.incid
mean(dry.hot.seguenega_fe.incid[,1], na.rm=TRUE)
mean(dry.hot.seguenega_fe.incid[,2], na.rm=TRUE)
mean(dry.hot.seguenega_fe.incid[,3], na.rm=TRUE)

dry.hot.seguenega_fe_2006.incid<-cbind(min=sapply(dry.hot.seguenega_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                     mean=sapply(dry.hot.seguenega_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                     max=sapply(dry.hot.seguenega_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
dry.hot.seguenega_fe_2006.incid
mean(dry.hot.seguenega_fe_2006.incid[,1], na.rm=TRUE)
mean(dry.hot.seguenega_fe_2006.incid[,2], na.rm=TRUE)
mean(dry.hot.seguenega_fe_2006.incid[,3], na.rm=TRUE)


## Wet (seguenega)

wet.seguenega_fe<-wet.seguenega[,fe_fs_seguenega.index]# health centers with outbreak
wet.seguenega_fe_2006<-extract(wet.seguenega_fe,2006)# removing epidemic year 2006

wet.seguenega_fe.incid<-cbind(min=sapply(wet.seguenega_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                            mean=sapply(wet.seguenega_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                            max=sapply(wet.seguenega_fe*1e+05,function(x) max(x,na.rm=TRUE)))

wet.seguenega_fe.incid
mean(wet.seguenega_fe.incid[,1], na.rm=TRUE)
mean(wet.seguenega_fe.incid[,2], na.rm=TRUE)
mean(wet.seguenega_fe.incid[,3], na.rm=TRUE)

wet.seguenega_fe_2006.incid<-cbind(min=sapply(wet.seguenega_fe_2006*1e+05,function(x) min(x,na.rm=TRUE)),
                                 mean=sapply(wet.seguenega_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                 max=sapply(wet.seguenega_fe_2006*1e+05,function(x) max(x,na.rm=TRUE)))
wet.seguenega_fe_2006.incid
mean(wet.seguenega_fe_2006.incid[,1], na.rm=TRUE)
mean(wet.seguenega_fe_2006.incid[,2], na.rm=TRUE)
mean(wet.seguenega_fe_2006.incid[,3], na.rm=TRUE)



## fs sans foyer epidemique (seguenega)

## Harmattan
dry.harmattan.seguenega_non_fe<-dry.harmattan.seguenega[,non_fe_fs_seguenega.index]# health centers with outbreak
dry.harmattan.seguenega_non_fe_2006<-extract(dry.harmattan.seguenega_non_fe,2006)# removing epidemic year 2006

dry.harmattan.seguenega_non_fe.incid<-cbind(min=sapply(dry.harmattan.seguenega_non_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                          mean=sapply(dry.harmattan.seguenega_non_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                          max=sapply(dry.harmattan.seguenega_non_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.harmattan.seguenega_non_fe.incid
mean(dry.harmattan.seguenega_non_fe.incid[,1], na.rm=TRUE)
mean(dry.harmattan.seguenega_non_fe.incid[,2], na.rm=TRUE)
mean(dry.harmattan.seguenega_non_fe.incid[,3], na.rm=TRUE)

dry.harmattan.seguenega_non_fe_2006.incid<-cbind(min=sapply(dry.harmattan.seguenega_non_fe_2006*1e+05,function(x) min(x,na.rm=FALSE)),
                                               mean=sapply(dry.harmattan.seguenega_non_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                               max=sapply(dry.harmattan.seguenega_non_fe_2006*1e+05,function(x) max(x,na.rm=FALSE)))
dry.harmattan.seguenega_non_fe_2006.incid
mean(dry.harmattan.seguenega_non_fe_2006.incid[,1], na.rm=TRUE)
mean(dry.harmattan.seguenega_non_fe_2006.incid[,2], na.rm=TRUE)
mean(dry.harmattan.seguenega_non_fe_2006.incid[,3], na.rm=TRUE)

## Dry hot

dry.hot.seguenega_non_fe<-dry.hot.seguenega[,non_fe_fs_seguenega.index]# health centers with outbreak
dry.hot.seguenega_non_fe_2006<-extract(dry.hot.seguenega_non_fe,2006)# removing epidemic year 2006

dry.hot.seguenega_non_fe.incid<-cbind(min=sapply(dry.hot.seguenega_non_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                    mean=sapply(dry.hot.seguenega_non_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                    max=sapply(dry.hot.seguenega_non_fe*1e+05,function(x) max(x,na.rm=TRUE)))
dry.hot.seguenega_non_fe.incid
mean(dry.hot.seguenega_non_fe.incid[,1], na.rm=TRUE)
mean(dry.hot.seguenega_non_fe.incid[,2], na.rm=TRUE)
mean(dry.hot.seguenega_non_fe.incid[,3], na.rm=TRUE)

dry.hot.seguenega_non_fe_2006.incid<-cbind(min=sapply(dry.hot.seguenega_non_fe_2006*1e+05,function(x) min(x,na.rm=FALSE)),
                                         mean=sapply(dry.hot.seguenega_non_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                         max=sapply(dry.hot.seguenega_non_fe_2006*1e+05,function(x) max(x,na.rm=FALSE)))
dry.hot.seguenega_non_fe_2006.incid
mean(dry.hot.seguenega_non_fe_2006.incid[,1], na.rm=TRUE)
mean(dry.hot.seguenega_non_fe_2006.incid[,2], na.rm=TRUE)
mean(dry.hot.seguenega_non_fe_2006.incid[,3], na.rm=TRUE)


## Wet (seguenega)

wet.seguenega_non_fe<-wet.seguenega[,non_fe_fs_seguenega.index]# health centers with outbreak
wet.seguenega_non_fe_2006<-extract(wet.seguenega_non_fe,2006)# removing epidemic year 2006

wet.seguenega_non_fe.incid<-cbind(min=sapply(wet.seguenega_non_fe*1e+05,function(x) min(x,na.rm=TRUE)),
                                mean=sapply(wet.seguenega_non_fe*1e+05,function(x) mean(x,na.rm=TRUE)),
                                max=sapply(wet.seguenega_non_fe*1e+05,function(x) max(x,na.rm=TRUE)))

wet.seguenega_non_fe.incid
mean(wet.seguenega_non_fe.incid[,1], na.rm=TRUE)
mean(wet.seguenega_non_fe.incid[,2], na.rm=TRUE)
mean(wet.seguenega_non_fe.incid[,3], na.rm=TRUE)

wet.seguenega_non_fe_2006.incid<-cbind(min=sapply(wet.seguenega_non_fe_2006*1e+05,function(x) min(x,na.rm=FALSE)),
                                     mean=sapply(wet.seguenega_non_fe_2006*1e+05,function(x) mean(x,na.rm=TRUE)),
                                     max=sapply(wet.seguenega_non_fe_2006*1e+05,function(x) max(x,na.rm=FALSE)))
wet.seguenega_non_fe_2006.incid
mean(wet.seguenega_non_fe_2006.incid[,1], na.rm=TRUE)
mean(wet.seguenega_non_fe_2006.incid[,2], na.rm=TRUE)
mean(wet.seguenega_non_fe_2006.incid[,3], na.rm=TRUE)

## end of k.vigue district estimations 

#######################

### calcul duree moyenne du dépacement du seuil d'alerte des foyers epidemiques

#HOUNDE

## HOUNDE

index.semaine.foyer.hounde.basse<-which(HOUNDA.incid[,fe_fs_houde.index][,1]>=0.00075)
index.semaine.foyer.hounde.basse
diff(index.semaine.foyer.hounde.basse)

index.semaine.foyer.hounde.bohokari<-which(HOUNDA.incid[,fe_fs_houde.index][,2]>=0.00075)
index.semaine.foyer.hounde.bohokari
diff(index.semaine.foyer.hounde.bohokari)

index.semaine.foyer.hounde.bouahoun<-which(HOUNDA.incid[,fe_fs_houde.index][,3]>=0.00075)
index.semaine.foyer.hounde.bouahoun
diff(index.semaine.foyer.hounde.bouahoun)


index.semaine.foyer.hounde.bouere<-which(HOUNDA.incid[,fe_fs_houde.index][,4]>=0.00075)
index.semaine.foyer.hounde.bouere
diff(index.semaine.foyer.hounde.bouere)

index.semaine.foyer.hounde.dougoumanto<-which(HOUNDA.incid[,fe_fs_houde.index][,5]>=0.00075)
index.semaine.foyer.hounde.dougoumanto
diff(index.semaine.foyer.hounde.dougoumanto)

index.semaine.foyer.hounde.fafo<-which(HOUNDA.incid[,fe_fs_houde.index][,6]>=0.00075)
index.semaine.foyer.hounde.fafo
diff(index.semaine.foyer.hounde.fafo)

index.semaine.foyer.hounde.koumbia<-which(HOUNDA.incid[,fe_fs_houde.index][,7]>=0.00075)
index.semaine.foyer.hounde.koumbia
diff(index.semaine.foyer.hounde.koumbia)

index.semaine.foyer.hounde.sara<-which(HOUNDA.incid[,fe_fs_houde.index][,8]>=0.00075)
index.semaine.foyer.hounde.sara
diff(index.semaine.foyer.hounde.sara)


## LENA

index.semaine.foyer.lena.dorossiamasso<-which(ts.Lena.complete[,fe_fs_lena.index][,1]>=0.00075)
cat("index of weeks when incidence>=75 per 100000 pop \n at dorossiamasso health center are:",index.semaine.foyer.lena.dorossiamasso)
index.semaine.foyer.lena.fina<-which(ts.Lena.complete[,fe_fs_lena.index][,2]>=0.00075)
cat("number of weeks when incidence>=75 per 100000 pop\n at fina health center are:",index.semaine.foyer.lena.fina)
index.semaine.foyer.lena.kofila<-which(ts.Lena.complete[,fe_fs_lena.index][,3]>=0.00075)
cat("number of weeks when incidence>=75 per 100000 pop\n at fina health center are:",index.semaine.foyer.lena.kofila)
index.semaine.foyer.lena.lena-which(ts.Lena.complete[,fe_fs_lena.index][,4]>=0.00075)
cat("number of weeks when incidence>=75 per 100000 pop\n at fina health center are:",index.semaine.foyer.lena.lena)
index.semaine.foyer.lena.satiri<-which(ts.Lena.complete[,fe_fs_lena.index][,5]>=0.00075)
cat("number of weeks when incidence>=75 per 100000 pop\n at fina health center are:",index.semaine.foyer.lena.satiri)

# calcul du nombre de semaine pendant le depassement du seuil
duree.foyer.lena.dorossiamasso<-diff(index.semaine.foyer.lena.dorossiamasso)
duree.foyer.lena.dorossiamasso
duree.foyer.lena.fina<-diff(index.semaine.foyer.lena.fina)
duree.foyer.lena.fina
duree.foyer.lena.kofila<-diff(index.semaine.foyer.lena.kofila)
duree.foyer.lena.kofila
duree.foyer.lena.lena<-diff(index.semaine.foyer.lena.lena)
duree.foyer.lena.lena
duree.foyer.lena.satiri<-diff(index.semaine.foyer.lena.satiri)
duree.foyer.lena.satiri

## K.VIGUE
index.semaine.foyer.k.vigue.k.vigue<-which(ts.k.vigue.complete[,fe_fs_k.vigue.index][,1]>=0.00075)
index.semaine.foyer.k.vigue.soumousso<-which(ts.k.vigue.complete[,fe_fs_k.vigue.index][,2]>=0.00075)
duree.foyer.k.vigue.k.vigue<-diff(index.semaine.foyer.k.vigue.k.vigue)
duree.foyer.k.vigue.k.vigue
sum(duree.foyer.k.vigue.k.vigue)
duree.foyer.k.vigue.soumousso<-diff(index.semaine.foyer.k.vigue.soumousso)
duree.foyer.k.vigue.soumousso
sum(duree.foyer.k.vigue.soumousso)
#duree.foyer.lena.dorossiamasso.recode<-ifelse(duree.foyer.lena.dorossiamasso!=1,0,1)

## SEGUENEGA

index.semaine.foyer.seguenega.goubre<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,1]>=0.00075)
index.semaine.foyer.seguenega.gounbre<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,2]>=0.00075)
index.semaine.foyer.seguenega.irim<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,3]>=0.00075)
index.semaine.foyer.seguenega.kalsaka<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,4]>=0.00075)
index.semaine.foyer.seguenega.tangaye<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,5]>=0.00075)
index.semaine.foyer.seguenega.pourra<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,6]>=0.00075)
index.semaine.foyer.seguenega.rasma<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,8]>=0.00075)

# calcul des incidences min max et moyenne correspondant au depassement du seuil epidemic des foyers


### exemple pour k.vigue formation sanitaire de soumouso
coredata(ts.k.vigue.complete[,fe_fs_k.vigue.index])[index.semaine.foyer.k.vigue.soumousso,2] # obtenir les données 
### qui correspondent au semaine pendant lesquelles le seuil epidemique est dépassé au niveau formation sanitaire.
mean(coredata(ts.k.vigue.complete[,fe_fs_k.vigue.index])[index.semaine.foyer.k.vigue.soumousso,2],na.rm=T)#moyenne
min(coredata(ts.k.vigue.complete[,fe_fs_k.vigue.index])[index.semaine.foyer.k.vigue.soumousso,2],na.rm=T)#min
max(coredata(ts.k.vigue.complete[,fe_fs_k.vigue.index])[index.semaine.foyer.k.vigue.soumousso,2],na.rm=T)#max

index.semaine.foyer.k.vigue.k.vigue
mean(coredata(ts.k.vigue.complete[,fe_fs_k.vigue.index])[index.semaine.foyer.k.vigue.k.vigue,1],na.rm=T)*1e+05#moyenne
index.semaine.foyer.k.vigue.soumousso
mean(coredata(ts.k.vigue.complete[,fe_fs_k.vigue.index])[index.semaine.foyer.k.vigue.soumousso,2],na.rm=T)*1e+05#moyenne


## HOUNDE:
index.semaine.foyer.hounde.basse
mean(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.basse,1])*1e+05 # obtenir les données 
min(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.basse,1])*1e+05 # obtenir les données 
max(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.basse,1])*1e+05 # obtenir les données 

index.semaine.foyer.hounde.bohokari
coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.bohokari,2]*1e+05 # obtenir les données 
mean(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.bohokari,2][3:5])*1e+05
min(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.bohokari,2][3:5])*1e+05
max(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.bohokari,2][3:5])*1e+05


index.semaine.foyer.hounde.bouahoun
mean(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.bouere,3])*1e+05 # obtenir les données 
min(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.bouere,3])*1e+05 # obtenir les données 
max(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.bouere,3])*1e+05 # obtenir les données 


index.semaine.foyer.hounde.dougoumanto
mean(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.dougoumanto,4])*1e+05 # obtenir les données 
min(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.dougoumanto,4])*1e+05 # obtenir les données 
max(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.dougoumanto,4])*1e+05 # obtenir les données 


index.semaine.foyer.hounde.fafo
# can not compute because no consecutive week after treashold was crossed
index.semaine.foyer.hounde.koumbia
mean(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.koumbia,7][1:5])*1e+05
min(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.koumbia,7][1:5])*1e+05
max(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.koumbia,7][1:5])*1e+05

index.semaine.foyer.hounde.sara
mean(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.sara,8])*1e+05 # obtenir les données 
min(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.sara,8])*1e+05 # obtenir les données 
max(coredata(HOUNDA.incid[,fe_fs_houde.index])[index.semaine.foyer.hounde.sara,8])*1e+05 # obtenir les données 

(146.17+300.93+150.16+176.32+98.75+161.84)/6 # moyenne des moyennes
(97.44+225.70+118.54+40.69+75.96+119.88)/6 # moyenne des minima
(194.89+338.55+189.67+325.52+132.93+239.77)/6 # moyenne des maxima


## LENA

index.semaine.foyer.lena.dorossiamasso
mean(coredata(ts.Lena.complete[,fe_fs_lena.index])[index.semaine.foyer.lena.dorossiamasso,1][1:8])*1e+05 # obtenir les données 

index.semaine.foyer.lena.fina
mean(coredata(ts.Lena.complete[,fe_fs_lena.index])[index.semaine.foyer.lena.fina,2])*1e+05 # obtenir les données 

index.semaine.foyer.lena.kofila
mean(coredata(ts.Lena.complete[,fe_fs_lena.index])[index.semaine.foyer.lena.kofila,3][1:8])*1e+05 # obtenir les données 

index.semaine.foyer.lena.lena
mean(coredata(ts.Lena.complete[,fe_fs_lena.index])[index.semaine.foyer.lena.lena,4])*1e+05 # obtenir les données 

index.semaine.foyer.lena.satiri
mean(coredata(ts.Lena.complete[,fe_fs_lena.index])[index.semaine.foyer.lena.satiri,5])*1e+05 # obtenir les données 

(877.84+194.98+688.34+283.79+231.12)/5
## SEGUENEGA

index.semaine.foyer.seguenega.goubre<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,1]>=0.00075)
index.semaine.foyer.seguenega.gounbre<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,2]>=0.00075)
index.semaine.foyer.seguenega.irim<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,3]>=0.00075)
index.semaine.foyer.seguenega.kalsaka<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,4]>=0.00075)
index.semaine.foyer.seguenega.tangaye<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,5]>=0.00075)
index.semaine.foyer.seguenega.pourra<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,6]>=0.00075)
index.semaine.foyer.seguenega.rasma<-which(SEGUENEGA.incid[,fe_fs_seguenega.index][,8]>=0.00075)


index.semaine.foyer.seguenega.goubre
mean(coredata(SEGUENEGA.incid[,fe_fs_seguenega.index])[index.semaine.foyer.seguenega.goubre,1])*1e+05 # obtenir les données 

index.semaine.foyer.seguenega.gounbre
mean(coredata(SEGUENEGA.incid[,fe_fs_seguenega.index])[index.semaine.foyer.seguenega.goubre,2][1:2])*1e+05 # obtenir les données 

index.semaine.foyer.seguenega.irim[3:5]
mean(coredata(SEGUENEGA.incid[,fe_fs_seguenega.index])[index.semaine.foyer.seguenega.irim,3][3:5])*1e+05 # obtenir les données 

index.semaine.foyer.seguenega.kalsaka
mean(coredata(SEGUENEGA.incid[,fe_fs_seguenega.index])[index.semaine.foyer.seguenega.kalsaka,4][1:2])*1e+05 # obtenir les données 


index.semaine.foyer.seguenega.tangaye
mean(coredata(SEGUENEGA.incid[,fe_fs_seguenega.index])[index.semaine.foyer.seguenega.tangaye,5][2:4])*1e+05 # obtenir les données 
mean(coredata(SEGUENEGA.incid[,fe_fs_seguenega.index])[index.semaine.foyer.seguenega.tangaye,5][6:9])*1e+05 # obtenir les données 
mean(coredata(SEGUENEGA.incid[,fe_fs_seguenega.index])[index.semaine.foyer.seguenega.tangaye,5][11:13])*1e+05 # obtenir les données 

index.semaine.foyer.seguenega.pourra
mean(coredata(SEGUENEGA.incid[,fe_fs_seguenega.index])[index.semaine.foyer.seguenega.pourra,6])*1e+05 # obtenir les données 

(124.13+124.47+194.62+133.38+118.70)/5

