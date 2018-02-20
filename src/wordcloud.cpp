#include "Rcpp.h"
using namespace Rcpp;

/*
 * Check if a box is overlapped with existing boxes
 * @param x1_ x cordinate of left-bottom corner of a box 
 * @param y1_ y cordinate of left-bottom corner of a box 
 * @param w1_ width of a box
 * @param h1_ height of a box
 * @param boxes_ list of numeric vectors recording existing boxes  
 * @author Ian Fellows
 */

// [[Rcpp::export]]
bool qatd_cpp_is_overlap(SEXP x1_, SEXP y1_, SEXP w1_, SEXP h1_, SEXP boxes_) {
    
	double x1 = as<double>(x1_);
	double y1 =as<double>(y1_);
	double w1 = as<double>(w1_);
	double h1 = as<double>(h1_);
	Rcpp::List boxes(boxes_);
	Rcpp::NumericVector box_;
	double x2, y2, w2, h2;
	bool overlap = false;
	for (size_t i = 0; i < boxes.size(); i++) {
		box_ = boxes[i];
		x2 = box_[0];
		y2 = box_[1];
		w2 = box_[2];
		h2 = box_[3];
		if (x1 < x2) {
			overlap = (x1 + w1) > x2;
		} else {
			overlap = (x2 + w2) > x1;
		}
		if (y1 < y2) {
			overlap = (overlap && ((y1 + h1) > y2));
		} else {
			overlap = (overlap && ((y2 + h2) > y1));
		}
		if (overlap)
			return true;
	}
	return false;
}
