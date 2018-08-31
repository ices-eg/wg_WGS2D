#include <TMB.hpp>   //Links in the TMB libraries

// The following should be used for the generalized gamma distribution
// template<class Type>
//  Type dgen_gamma(Type y, Type shape, Type scale, Type p, int give_log=0)
//  {
//    Type logres=-log(p)-lgamma(shape/p)+(shape-Type(1.0))*log(y)-pow(y/scale,p)-shape*log(scale);
//    //Type logres=-lgamma(shape)+(shape-Type(1.0))*log(y)-y/scale-shape*log(scale);
//    if(give_log)return logres; else return exp(logres);
//  }


template<class Type>
  Type objective_function<Type>::operator() ()
{
  using namespace R_inla; //includes SPDE-spesific functions, e.g. Q_spde()
  using namespace density; 
  using namespace Eigen; //Needed for utilisation of sparse structures
  
  //Load data and parameters----------------
  DATA_VECTOR(EP);                        //The response
  DATA_MATRIX(Xsplines);                       //Design matrix for fixed effects - splines
  DATA_MATRIX(Xlinear);                        //Design matrix for linear fixed effects including the intercept
  DATA_STRUCT(spdeMatrices,spde_t);            //Three matrices needed for representing the GMRF, see p. 8 in Lindgren et al. (2011)
  DATA_SPARSE_MATRIX(A);                       //Matrix for interpolating points witin triangles 
  DATA_INTEGER(flag);                          //if flag=0 the prior for x is calculated
  DATA_SPARSE_MATRIX(S);                       //diag(S1,S2,S3,S4,S5), but not storing off-diagonal zeros
  DATA_VECTOR(Sdims);                          //Dimensions of S1,S2,S3,S4,S5
  DATA_SPARSE_MATRIX(designMatrixForReport);
  
  //Splines part
  PARAMETER_VECTOR(betas_linear);                //these betas are including intercept!!!
  //PARAMETER_VECTOR(b0);                        //Intercept only!!!
  PARAMETER_VECTOR(betas_splines);             //for the splines part only!!!
  PARAMETER_VECTOR(log_lambda);
  
  //GMRF-related parameters
  PARAMETER(log_tau);
  PARAMETER(log_kappa);
  PARAMETER_VECTOR(x);  
  
  //Distribution-related parameters
  PARAMETER(log_phi);//This is for the tweedie distribution
  PARAMETER(log_p);  //This is for the tweedie distribution
  
  //---------------------------------------
  
  //Transform parameters-------------------
  Type tau = exp(log_tau);
  Type kappa = exp(log_kappa);
  Type phi = exp(log_phi);  
  Type p = exp(log_p); 
  vector<Type> lambda = exp(log_lambda);
  //------------------------------------------

  // Spatial interpolation
  vector<Type> delta = (A*x)/tau;
  
  //Construct sparce precision matrix for latent field---
  SparseMatrix<Type> Q = Q_spde(spdeMatrices,kappa);
  //---------------------------------------------
  
  //Calculates nll-------------------------------
  Type nll = 0.0;
  //nll = GMRF(Q)(x);                              
  
  nll = GMRF(Q, false)(x);  
  
  
  vector<Type> S_beta = S*betas_splines;
  nll -= 0.5*(log_lambda*Sdims).sum();  
  int counter = 0;
  for(int i=0;i<Sdims.size(); i++){
    for(int j=0;j<Sdims(i); j++){
      nll -= -0.5*lambda(i)*betas_splines(counter)*S_beta(counter);
      counter++;
    }
  }
  
  // Return un-normalized density on request
  if (flag == 0) return nll;
  
  vector<Type> mu(EP.size());
  mu = exp(Xlinear*betas_linear + Xsplines*betas_splines + delta); //includes intercept!!!
  //mu = exp(Xsplines*betas_splines + delta); //includes intercept!!!
  REPORT(mu);
  //Tweedie likelihood
  for(int i=0; i<EP.size(); i++){ 
    nll -= dtweedie(EP(i), mu(i), phi, p, true);
  }
  
  //---------------------------------------------
  
  //Report what we want to report----------------
  Type range = sqrt(8)/kappa;   //Distance at which correlation has dropped to 0.1, see p. 4 in Lindgren et al. (2011)
  ADREPORT(range);
  
  vector<Type> splineForReport = designMatrixForReport*betas_splines;
  ADREPORT(splineForReport);
  ADREPORT(betas_splines);
  ADREPORT(betas_linear);
  //ADREPORT(b0);
  //ADREPORT(mu);
  
  //---------------------------------------------
  
  return nll;
}
