
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(spdep)
library(INLA)


# Load the data after imputing the missing counts
load("../Imputed_data/IHD_imputed_data.Rdata")


S <- nrow(counts)
T <- ncol(counts)-1


# Prepare the data
Year <- unlist(lapply(seq(1999,2021, 1), function(x) rep(x, S)))

Data <- data.frame(GEOID=rep(carto$GEOID, T), ID.year=as.integer(Year), 
                   ID.area=rep(seq(1,S,1), T), ID.area.year=seq(1,S*T,1),
                   O=unlist(c(counts[, 2:(T+1)])), E=unlist(c(exp[, 2:(T+1)])))
rownames(Data) <- seq(1, S*T)



################################################
# Define the priors and neighbourhood matrices #
################################################

# Define the Uniform(0,inf) hyperprior
sdunif="expression:
  logdens=-log_precision/2;
  return(logdens)"


# Spatial neighborhood matrix: Rs
nb.list <- poly2nb(carto)
nb.mat <- nb2mat(poly2nb(carto), style ="B")
nbs <- unlist(lapply(poly2nb(carto), function(x) length(x)))

Rs <- diag(nbs)-nb.mat


# RW1 precision matrix
Dm <- diff(diag(T),differences=1)
Rt <- t(Dm)%*%Dm



#################################################
# Global spatio-temporal models with ICAR prior #
#################################################

##########
# Type I #
##########
f.icar.typeI <- O ~ 1 + f(ID.area, model="besag", graph=Rs, scale.model=FALSE, 
                          constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                        f(ID.year, model="rw1", constr=TRUE, 
                          hyper=list(prec=list(prior=sdunif))) +
                        f(ID.area.year, model="iid", constr=TRUE, 
                          hyper=list(prec=list(prior=sdunif)))


TypeI.ICAR <- inla(f.icar.typeI, family="poisson", data=Data, E=E,
              control.predictor=list(compute=TRUE, cdf=c(log(1))),
              control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE,
                                   return.marginals.predictor=TRUE))


save(TypeI.ICAR, file = "ST_TypeI_ICAR_global.Rdata")



###########
# Type II #
###########
f.icar.typeII <- O ~ 1 + f(ID.area, model="besag", graph=Rs, scale.model=FALSE, 
                           constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                         f(ID.year, model="rw1", constr=TRUE,
                           hyper=list(prec=list(prior=sdunif))) +
                         f(ID.area.year, model="generic0", Cmatrix=R, 
                           rankdef=rank.def, constr=TRUE,
                           extraconstr=list(A=A.constr, e=e), 
                           hyper=list(prec=list(prior=sdunif)))

R <- kronecker(Rt, diag(S))
rank.def <- S
A.constr <- kronecker(matrix(1,1,T), diag(S))
A.constr <- A.constr[-1,]
e <- rep(0,S-1)


TypeII.ICAR <- inla(f.icar.typeII, family="poisson", data=Data, E=E,
                    control.predictor=list(compute=TRUE, cdf=c(log(1))),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE,
                                         return.marginals.predictor=TRUE))


save(TypeII.ICAR, file = "ST_TypeII_ICAR_global.Rdata")



############
# Type III #
############
f.icar.typeIII <- O ~ 1 + f(ID.area, model="besag", graph=Rs, scale.model=FALSE, 
                            constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                          f(ID.year, model="rw1", constr=TRUE,
                            hyper=list(prec=list(prior=sdunif))) +
                          f(ID.area.year, model="generic0", Cmatrix=R, 
                            rankdef=rank.def, constr=TRUE,
                            extraconstr=list(A=A.constr, e=e), 
                            hyper=list(prec=list(prior=sdunif)))


R <- kronecker(diag(T), Rs)
rank.def <- T
A.constr <- kronecker(diag(T),matrix(1,1,S))
A.constr <- A.constr[-1,]
e <- rep(0,T-1)


TypeIII.ICAR <- inla(f.icar.typeIII, family="poisson", data=Data, E=E,
                     control.predictor=list(compute=TRUE, cdf=c(log(1))),
                     control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE,
                                          return.marginals.predictor=TRUE))


save(TypeIII.ICAR, file = "ST_TypeIII_ICAR_global.Rdata")



###########
# Type IV #
###########
f.icar.typeIV <- O ~ 1 + f(ID.area, model="besag", graph=Rs, scale.model=FALSE, 
                           constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                         f(ID.year, model="rw1", constr=TRUE,
                           hyper=list(prec=list(prior=sdunif))) +
                         f(ID.area.year, model="generic0", Cmatrix=R, 
                           rankdef=rank.def, constr=TRUE,
                           extraconstr=list(A=A.constr, e=e), 
                           hyper=list(prec=list(prior=sdunif)))


R <- kronecker(Rt, Rs)
rank.def <- S+T-1
A1 <- kronecker(matrix(1,1,T),diag(S))
A2 <- kronecker(diag(T),matrix(1,1,S))
A.constr <- rbind(A1[-1,], A2[-1,])
e <- rep(0, S+T-2)


TypeIV.ICAR <- inla(f.icar.typeIV, family="poisson", data=Data, E=E,
                    control.predictor=list(compute=TRUE, cdf=c(log(1))),
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE,
                                         return.marginals.predictor=TRUE))


save(TypeIV.ICAR, file = "ST_TypeIV_ICAR_global.Rdata")





