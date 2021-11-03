library(extRemes)
library(in2extRemes)
in2extRemes()
winter = c(1:60, 305:365)
wooster.w = wooster
wooster.w$data =   wooster$data[c(winter, winter + 365,
                                  winter + 365 * 2, winter + 365 *3, winter+365*4),]
wooster.w$data[,2]= - wooster.w$data[,2]
wooster.w$data <-
  cbind(wooster.w$data,year.number=c(rep(1:5,rep(121,5))))

wooster.w.u.minus10.r.2<-
  decluster(wooster.w$data[,"value"],threshold=-10,r=2,
            groups=wooster.w$data[,"year.number"])
attributes(wooster.w.u.minus10.r.2) <- NULL
wooster.w.u.minus10.r.2  <-
  as.in2extRemesDataObject(wooster.w.u.minus10.r.2)

wooster.w.u.minus20.r.2<-
  decluster(wooster.w$data[,"value"],threshold=-20,r=2,
            groups=wooster.w$data[,"year.number"])
attributes(wooster.w.u.minus20.r.2) <- NULL
wooster.w.u.minus20.r.2  <-
  as.in2extRemesDataObject(wooster.w.u.minus20.r.2)

wooster.w.u.minus10.r.4<-
  decluster(wooster.w$data[,"value"],threshold=-10,r=4,
            groups=wooster.w$data[,"year.number"])
attributes(wooster.w.u.minus10.r.4) <- NULL
wooster.w.u.minus10.r.4 <-
  as.in2extRemesDataObject(wooster.w.u.minus10.r.4)

wooster.w.u.minus20.r.4<-
  decluster(wooster.w$data[,"value"],threshold=-20,r=4,
            groups=wooster.w$data[,"year.number"])
attributes(wooster.w.u.minus20.r.4) <- NULL
wooster.w.u.minus20.r.4 <-
  as.in2extRemesDataObject(wooster.w.u.minus20.r.4)

sum(wooster.w.u.minus10.r.2$data[,2]>-10)
sum(wooster.w$data[,2]>-10)
sum(wooster.w.u.minus10.r.4$data[,2]>-10)

sum(wooster.w.u.minus20.r.2$data[,2]>-20)
sum(wooster.w$data[,2]>-20)
sum(wooster.w.u.minus20.r.4$data[,2]>-20)

