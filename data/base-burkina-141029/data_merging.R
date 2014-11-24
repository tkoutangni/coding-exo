#data merging/appending operations
# finding the health center DAFRA and LENA Districts have in common
names(LENA.incid)
names(DAFRA.incid)
tsLena=LENA.incid[,intersect(names(LENA.incid), names(DAFRA.incid))]
tsDaffra=DAFRA.incid[,intersect(names(LENA.incid), names(DAFRA.incid))]


tsLena=tsLena[year(tsLena)>=2008]
tsDaffra=tsDaffra[year(tsDaffra)<=2007]
ts.Lena.complete=rbind(tsDaffra,tsLena)
xyplot(ts.Lena.complete*1e+05, layout=c(2,4))

# merging ts.Lena.complete with the remaing health centers 
# in the Lena "pseudo-district"

ts.Lena.complete<-merge.zoo(ts.Lena.complete,LENA.incid[,c(12,13)])
plot(ts.Lena.complete*1e+05, plot.type="single", col=rainbow(13),lwd=1, main="Lena pseudo district")
xyplot(ts.Lena.complete*1e+05, layout=c(2,4))


# finding the health center DAFRA and K.VIGUE Districts have in common
names(K.VIGUE.incid)
names(DAFRA.incid)
tskvigue=K.VIGUE.incid[,intersect(names(K.VIGUE.incid), names(DAFRA.incid))]
tsDaffra_kv=DAFRA.incid[,intersect(names(K.VIGUE.incid), names(DAFRA.incid))]

tskvigue=tskvigue[year(tskvigue)>=2008]
tsDaffra_kv=tsDaffra_kv[year(tsDaffra_kv)<=2007]
ts.k.vigue.complete=rbind(tsDaffra_kv,tskvigue)
xyplot(ts.k.vigue.complete)

# merging ts.k.vigue.complete with the remaing health centers 
# in the k.vigue "pseudo-district"

ts.k.vigue.complete<-merge.zoo(ts.k.vigue.complete,K.VIGUE.incid[,c(7,8)])

plot(ts.k.vigue.complete*1e+05, plot.type="single", col=rainbow(8),lwd=1, main="k.vigue pseudo district")
xyplot(ts.k.vigue.complete*1e+05, layout=c(3,3))

# removing the health centers in lena and k.vigue districts from the daffra district.

only.dafra.hs=setdiff(names(DAFRA.incid),names(tsLena))
only.dafra.hs=setdiff(only.dafra.hs,names(tskvigue))
dim(DAFRA.incid[,only.dafra.hs])
DAFRA.incid[,only.dafra.hs]

plot(DAFRA.incid[,only.dafra.hs]*1e+05,
     plot.type="single",
     main="Dafra district weekly cases")





