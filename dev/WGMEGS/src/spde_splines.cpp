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
  DATA_STRUCT(spdeMatrices,spde_t);            //Three matrices needed for representing the GMRF, see p. 8 in Lindgren et al. (2011)
  DATA_SPARSE_MATRIX(A);                       //Matrix for interpolating points within triangles 
  DATA_INTEGER(flag);                          //Used for calculating the normalizing constant of the prior for beta on R side (if flag=0 the prior for x is calculated)
  DATA_SPARSE_MATRIX(S);                       //Penalization matrix diag(S1,S2,S3,S4,S5) without storing off-diagonal zeros.
  DATA_IVECTOR(Sdims);                         //Dimensions of splines
  DATA_SPARSE_MATRIX(designMatrixForReport);   //Design matrix for report of splines
  
  //Splines part
  PARAMETER(b0);                               //Intercept only!!!
  PARAMETER_VECTOR(betas_splines);             //Spline regression parameters (for the splines part only!!!)
  PARAMETER_VECTOR(log_lambda);                //Penalization parameters
  
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
  
  int k=0;  // Counter
  for(int i=0;i<Sdims.size();i++){
    int m_i = Sdims(i);
    vector<Type> betas_splines_i = betas_splines.segment(k,m_i);       // Recover betai
    SparseMatrix<Type> S_i = S.block(k,k,m_i,m_i);  // Recover Si
    nll -= Type(0.5)*m_i*log_lambda(i) - 0.5*lambda(i)*GMRF(S_i).Quadform(betas_splines_i);
    k += m_i;
  }
  
  
  
  
  
  // vector<Type> S_beta = S*betas_splines;
  // nll -= 0.5*(log_lambda*Sdims).sum();
  // int counter = 0;
  // for(int i=0;i<Sdims.size(); i++){
  //  for(int j=0;j<Sdims(i); j++){
  //    nll -= -0.5*lambda(i)*betas_splines(counter)*S_beta(counter);
  //    counter++;
  //  }
  // }
  
  //Construct the penelization precision matrix----------------
  // matrix<Type> lambdaDiag(CppAD::Integer(sum(Sdims)),CppAD::Integer(sum(Sdims)));
  // lambdaDiag.setZero();
  // int counter = 0;
  // for(int i =0; i<Sdims.size(); i++){
  //   for(int j=0; j<Sdims(i); j++){
  //     lambdaDiag(counter,counter) = lambda(i);
  //     counter++;
  //   }
  // }
  //SparseMatrix<Type> sparse = lambdaDiag.sparseView();
  //SparseMatrix<Type> lambdaS = S*sparse;
  //Add prior of spline regression prameters-----------------
  //nll = GMRF(lambdaS,false)(betas_splines);
  //---------------------------------------------------------
  
  
  // Return un-normalized density on request
  if (flag == 0) return nll;
  
  vector<Type> mu(EP.size());
  mu = exp(b0 + Xsplines*betas_splines + delta); //includes intercept!!!
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
  //ADREPORT(betas_linear);
  ADREPORT(b0);
  //ADREPORT(mu);
  
  //---------------------------------------------
  
  return nll;
}
