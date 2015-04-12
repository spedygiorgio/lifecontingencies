#include <Rcpp.h>
#include <math.h> 
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


// [[Rcpp::export(.fExnCpp)]]
double fExnCpp(double T, double y, double n, double i)
{
  double out;
  if(T<y+n)
    out=0;
  else
    out=pow(1+i,-n);
  return out;
}


// [[Rcpp::export(.fAxnCpp)]]
double fAxnCpp(double T, double y, double n, double i, double m, int k)
{
  double out;
  if ((T>=y+m) && (T<=y+m+n-1/k))
    out=pow(1+i,-(T-y+1/k));
  else
    out=0;
  return out;
}



// [[Rcpp::export(.fIAxnCpp)]]
double fIAxnCpp(double T, double y, double n, double i, double m, int k=1) {
  double out;
  if ((T>=y+m) && (T<=y+m+n-1/k))
    out=(T-(y+m)+1/k)*pow(1+i,-(T-y+1/k));
  else out=0;
  return out;
}



// [[Rcpp::export(.fDAxnCpp)]]
double fDAxnCpp(double T, double y, double n, double i, double m, int k=1) {
  double out;
  if ((T>=y+m) && (T<=y+m+n-1/k))
    out=(n-(T-(y+m)+1/k))*pow(1+i,-(T-y+1/k));
  else
    out=0;
  return out;
}



// TODO: move faxn

// [[Rcpp::export(.fAExnCpp)]]

double fAExnCpp(double T, double y, double n, double i, int k=1)
{
  double out;
  if ((T>=y) && (T<=y+n-1/k))
    out=pow(1+i,-(T-y+1/k));
  else
    out=pow(1+i,-n);
  return out;
}