#include <Rcpp.h>
#include <Rmath.h>
#include <math.h>
#define ROOT5 2.23606797749978969640
#define FIVE_THIRDS 1.6666666666666666667

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix M52Sigma_2D(NumericMatrix x1, NumericMatrix x2, NumericVector kappa) {
  	double arg_x1, arg_x2, scaled_distance2, scaled_distance;
    int n1 = x1.nrow();
    int n2 = x2.nrow();
    NumericMatrix C(n1,n2);
    for(int i = 0; i < n1; i++) {
    	for(int j=0; j < n2; j++) {
    		arg_x1 = (x1(i,0) - x2(j,0))/kappa[0];
    		arg_x2 = (x1(i,1) - x2(j,1))/kappa[1];
    		scaled_distance2 = arg_x1*arg_x1 + arg_x2*arg_x2;
    		scaled_distance = sqrt(scaled_distance2);
    		C(i,j) = kappa[2]*(1.0 + ROOT5*scaled_distance + FIVE_THIRDS*scaled_distance2)*exp(-ROOT5*scaled_distance);
    		if(scaled_distance2 == 0.0) C(i,j) += kappa[3];
    	}
    }
    return C;
}
