#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool scanEnter(int i, int rg = 0) {
  if (rg == 0) {
    return FALSE;
  }
  return i % (2 * rg - 2) == 0;
}

// [[Rcpp::export]]
bool caughtCpp(IntegerVector depth, IntegerVector range, int delay = 0) {
  int n = depth.size();
  int i = 0;
  while(i < n && !scanEnter(depth(i) + delay, range(i))) i++;
  return i != n;
}

// [[Rcpp::export]]
int minDelay(IntegerVector depth, IntegerVector range) {
  int delay = 0;
  while(caughtCpp(depth, range, delay)) delay++;
  return delay;
}
