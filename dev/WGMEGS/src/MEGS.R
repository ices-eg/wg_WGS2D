library(TMB)
library(INLA)
library(fields)
library(rgeos)
library(mgcv)

# #Compile and load c++ code-------
# #code for linear+splines
# compile("spde_mixture.cpp")
# dyn.load(dynlib("spde_mixture"))
# 
# #code for linear only
# compile("spde_linear.cpp")
# dyn.load(dynlib("spde_linear"))
# 
# #code for splines only
# compile("spde_splines.cpp")
# dyn.load(dynlib("spde_splines"))
# 
# #--------------------------------

#--------------------------------
#Define mesh and components representing the  precision matrix----

#loc = cbind(df$utm_x, df$utm_y)
#loc = cbind(d2_spdf_utm$long, d2_spdf_utm$lat)
loc = cbind(df@coords[,1],df@coords[,2])

#Create boundaries (an internal and an external)
#boundary = INLA::inla.nonconvex.hull(loc)
#boundary2 = INLA::inla.nonconvex.hull(loc,convex = -0.35)
inner_boundary <- INLA::inla.nonconvex.hull(loc)
outer_boundary <- INLA::inla.nonconvex.hull(loc,convex = -0.35)

max_edge = max_edge #in kilometers

mesh = INLA::inla.mesh.2d(
  #loc=loc,
  #boundary = list(boundary,boundary2),
  boundary = list(inner_boundary,outer_boundary),
  max.edge=c(1,4)*max_edge,
  #cutoff=ifelse(max_edge<100,0.25,250)
  #cutoff=250
)
# mesh$crs <- crs32_km
# plot(mesh, asp=1)
# points(loc, pch=16)
# mesh$n
A = inla.spde.make.A(mesh,loc)
spde = inla.spde2.matern(mesh, alpha=2)


#-------------------------------------------------------------------
spdeMatrices = spde$param.inla[c("M0","M1","M2")]
#-------------------------------------------------------------------
#Define the data and parameters to be passed to TMB----------

data <- list(EP      = df$EP, #Response variable
             meshidxloc   = mesh$idx$loc - 1, #mesh nodes locations
             A            = A, #weight matrix
             spdeMatrices = spdeMatrices #spde matrices
             )

parameters <- list(log_tau       = 0,
                   log_kappa     = -1, #the model is sensitive to kappa check various starting values before drawing results
                   x             = rep(0.0, nrow(data$spdeMatrices$M0)),
                   log_phi       = 0,
                   log_p         = log(1.5)
                   )

if (models[[j]]$gam_setup$m==0) { #this means the model is linear (no splines are included)
  data$Xlinear      = models[[j]]$gam_setup$X[,c(1:models[[j]]$gam_setup$nsdf)] # Design matrix for linear terms including intercept
  data$Xlinear_pred      = models[[j]]$gam_setup$X[,c(1:models[[j]]$gam_setup$nsdf)] # Design matrix for linear terms including intercept
  parameters$betas_linear = c(rep(0,models[[j]]$gam_setup$nsdf))
  selected_dll <- "spde_linear"
  } else { #if the model contains splines, the intercept and linear covariates
    #Extrtact penelization matrices for the P-splines
    S_list <- list()
    for(i in 1:models[[j]]$gam_setup$m){
      S_list[[paste0('S_',models[[j]]$gam_setup$smooth[[i]]$term)]] <- models[[j]]$gam_setup$smooth[[i]]$S[[1]]
    }
    S_combined               = .bdiag(S_list)         # join S's in sparse matrix
    Sdims                    = unlist(lapply(S_list,nrow)) # Find dimension of each S
    data$S                   = S_combined
    data$Sdims               = Sdims
    data$Xsplines            = models[[j]]$gam_setup$X[,-c(1:models[[j]]$gam_setup$nsdf)]  # Design matrix, without intercept for splines
    data$Xlinear             = models[[j]]$gam_setup$X[,c(1:models[[j]]$gam_setup$nsdf)] # Design matrix for linear terms including intercept
    parameters$betas_linear  = c(rep(0,models[[j]]$gam_setup$nsdf))
    parameters$betas_splines = rep(0, sum(Sdims)) # Spline coefficients
    parameters$log_lambda    = rep(rep(0,length(Sdims))) #Log spline penalization coefficients
    selected_dll <- "spde_mixture"
    if (models[[j]]$gam_setup$nsdf==1) {  #if the model contains only splines and the intercept
      parameters$b0 = 0 # it doesn't like a single column matrix for some reason
      parameters$betas_linear = NULL # no more linear coefficients other than b0
      data$Xlinear      = NULL # no more linear covariates
      selected_dll <- "spde_splines"
    }
  }



#For report, used for constructing plots--------------
#Important! Keep the same names here as the ones in the model terms
if (models[[j]]$gam_setup$m!=0) { #this means the model is contains at least one spline
designMatrixForReport = list()
pred_df = list()
for(i in 1:models[[j]]$gam_setup$m){
spline <- paste0(models[[j]]$gam_setup$smooth[[i]]$term)
pred_df[[i]] <- data.frame(seq(floor(min(df[[spline]])), ceiling(max(df[[spline]])), length.out = 21))
colnames(pred_df[[i]]) <- spline
designMatrixForReport[[paste0(spline,'Report')]] <- PredictMat(models[[j]]$gam_setup$smooth[[i]], data = pred_df[[i]])  
}

data$designMatrixForReport = .bdiag(designMatrixForReport)
}
#Estimating the model and extract results-------------
data$flag = 1
startTime <- Sys.time()
#map=list(log_tau=factor(NA),log_kappa=factor(NA),x=factor(rep(NA, nrow(data$spdeMatrices$M0)))) #shut down spatial field
# #map=list() #re-include spatial field

if(grepl('noGMRF', names(models)[[j]])){
  map=list(log_tau=factor(NA),log_kappa=factor(NA),x=factor(rep(NA, nrow(data$spdeMatrices$M0)))) #shut down spatial field
} else {map <- list()}

if (models[[j]]$gam_setup$m==0) { #this means the model is linear (no splines are included)
  obj <- MakeADFun(data, parameters, random=c("x"), DLL=selected_dll, map=map, silent = TRUE)  
} else { #if the model contains splines, the intercept and linear covariates
  obj <- MakeADFun(data, parameters, random=c("betas_splines","x"), DLL=selected_dll, map=map, silent = TRUE) # the MakeADFun is the same for splines models, as the DLL is defined further up
} 
  
obj <- normalize(obj, flag="flag")
models[[j]]$results$opt <- nlminb(obj$par, obj$fn, obj$gr)
models[[j]]$results$rep <- sdreport(obj)
models[[j]]$results$obj <- obj
endTime <- Sys.time()
models[[j]]$results$timeUsed = endTime - startTime
#j=5
if(grepl('splines|mixture', names(models)[[j]])){
  models[[j]]$results$muSpline = models[[j]]$results$rep$value[names(models[[j]]$results$rep$value)=="splineForReport"]
  models[[j]]$results$sdSpline = models[[j]]$results$rep$sd[names(models[[j]]$results$rep$value)=="splineForReport"]
} else {
  models[[j]]$results$muSpline = list()
  models[[j]]$results$sdSpline = list()
}

if(grepl('noGMRF', names(models)[[j]])){
  models[[j]]$results$rangeIndex = numeric()
  #models[[j]]$results$fieldIndex = numeric()
} else {
  models[[j]]$results$rangeIndex = which(row.names(summary(models[[j]]$results$rep,"report"))=="range")
  #models[[j]]$results$fieldIndex = which(row.names(summary(models[[3]]$results$rep,"report"))=="x")
}
