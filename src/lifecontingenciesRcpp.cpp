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


//pure endowment function
//.fExn<-function(T,y,n, i)
//{
//  out=ifelse(T<y+n,0,(1+i)^(-n))
//	return(out)
//}


// [[Rcpp::export(.fExnCpp)]]
double fExn(double T, double y, double n, double i)
{
  double out;
  if(T<y+n)
    out=0;
  else
    out=pow(1+i,-n);
  return out;
}


//.fAxn<-function(T,y,n, i, m, k)
//{
//  out=numeric(1)
//	out=ifelse(((T>=y+m) && (T<=y+m+n-1/k)),(1+i)^-(T-y+1/k),0)
//	return(out)
//}


// [[Rcpp::export(.fAxnCpp)]]
double fExn(double T, double y, double n, double i, double m, double k)
{
  double out;
  if ((T>=y+m) && (T<=y+m+n-1/k))
    out=pow(1+i,-(T-y+1/k));
  else
    out=0;
  return out;
}

//.fAxyzn<-function(T,y,n, i, m, k, status)
//{
//  out=numeric(1)
//	temp=T-y
//	T=ifelse(status=="joint",min(temp),max(temp)) #redefines T
//	out=ifelse(((T>=m) && (T<=m+n-1/k)),(1+i)^-(T+1/k),0)
//	return(out)
//}


// [[Rcpp::export(.fAxyznCpp)]]


