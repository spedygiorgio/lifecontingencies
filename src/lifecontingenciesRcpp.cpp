#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(.mult3sum)]]
double mult3sum(NumericVector x, NumericVector y, NumericVector z)
{
  double total=0;
  // assuming x y z have the same length
  int n = x.size();
  
  for(int i = 0; i < n; ++i) {
    total += x[i]*y[i]*z[i];
  }
  return total;
}

// [[Rcpp::export(.mult2sum)]]
double mult2sum(NumericVector x, NumericVector y)
{
  double total=0;
  // assuming x y z have the same length
  int n = x.size();
  
  for(int i = 0; i < n; ++i) {
    total += x[i]*y[i];
  }
  return total;
}
