#include <Rcpp.h>
#include <math.h> 
using namespace Rcpp;


// [[Rcpp::export(name=".mult3sum")]]
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

// [[Rcpp::export(name=".mult2sum")]]
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

// [[Rcpp::export(name=".presentValueC")]]
double presentValueC(NumericVector cashFlows,
                     NumericVector timeIds,
                     NumericVector interestRates,
                     NumericVector probabilities,
                     double power = 1.0) {
  int n = cashFlows.size();
  double total = 0.0;

  for (int i = 0; i < n; ++i) {
    double discountFactor = pow(1.0 + interestRates[i], -timeIds[i]);
    double term = pow(cashFlows[i], power) * pow(discountFactor, power) * probabilities[i];
    total += term;
  }

  return total;
}


// [[Rcpp::export(name=".fExnCpp")]]
double fExnCpp(double T, double y, double n, double i)
{
  double out;
  if(T<y+n)
    out=0;
  else
    out=pow(1+i,-n);
  return out;
}



// [[Rcpp::export(name=".fAxnCpp")]]
double fAxnCpp(double T, double y, double n, double i, double m, double k=1)
{
  double out=0;
  if ((T>=y+m) && (T<=y+m+n-1/k))
    out=pow(1+i,-(T-y+1/k));
  else
    out=0;
  return out;
}



// [[Rcpp::export(name=".fIAxnCpp")]]
double fIAxnCpp(double T, double y, double n, double i, double m, double k=1) {
  double out;
  if ((T>=y+m) && (T<=y+m+n-1/k))
    out=(T-(y+m)+1/k)*pow(1+i,-(T-y+1/k));
  else out=0;
  return out;
}



// [[Rcpp::export(name=".fDAxnCpp")]]
double fDAxnCpp(double T, double y, double n, double i, double m, double k=1) {
  double out;
  if ((T>=y+m) && (T<=y+m+n-1/k))
    out=(n-(T-(y+m)+1/k))*pow(1+i,-(T-y+1/k));
  else
    out=0;
  return out;
}



// TODO: move faxn

// [[Rcpp::export(name=".fAExnCpp")]]
double fAExnCpp(double T, double y, double n, double i, double k=1)
{
  double out;
  if ((T>=y) && (T<=y+n-1/k))
    out=pow(1+i,-(T-y+1/k));
  else
    out=pow(1+i,-n);
  return out;
}
