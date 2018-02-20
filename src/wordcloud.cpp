#include "Rcpp.h"
using namespace Rcpp;

/*
 * Check if a box is overlapped with existing words
 * @param x x cordinate of left-bottom corner of a box 
 * @param y y cordinate of left-bottom corner of a box 
 * @param w width of a box
 * @param h height of a box
 * @param words_ list of numeric vectors recording existing words
 */

// [[Rcpp::export]]
bool qatd_cpp_is_overlap(double x, double y, double w, double h, DataFrame words_) {
    
    NumericVector x_ = words_["x"];
    NumericVector y_ = words_["y"];
    NumericVector w_ = words_["w"];
    NumericVector h_ = words_["h"];
    
	bool overlap = false;
	for (size_t i = 0; i < x_.size(); i++) {
	    if (R_IsNA(x_[i])) continue;
		if (x < x_[i]) {
			overlap = (x + w) > x_[i];
		} else {
			overlap = (x_[i] + w_[i]) > x;
		}
		if (y < y_[i]) {
			overlap = (overlap && ((y + h) > y_[i]));
		} else {
			overlap = (overlap && ((y_[i] + h_[i]) > y));
		}
		if (overlap)
			return true;
	}
	return false;
}
