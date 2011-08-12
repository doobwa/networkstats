const char* hello( std::string who ){
  std::string result( "hello " ) ;
  result += who ;
  return result.c_str() ;
}

double norm( double x, double y ) {
  return sqrt( x*x + y*y );
}
RCPP_MODULE(yada){
  using namespace Rcpp ;
  function( "hello", &hello ) ;
  function("norm",&norm);
}

using namespace Rcpp;
class Uniform {
public:
  Uniform(double min_, double max_) : min(min_), max(max_) {}
  NumericVector draw(int n) const {
    RNGScope scope;
    return runif( n, min, max );
  }
  double min, max;
};
double range( Uniform* w) {
  return w->max - w->min;
}
RCPP_MODULE(unif_module) {
  class_<Uniform>( "Uniform" )
    .constructor<double,double>()
    .field( "min", &Uniform::min)
    .field( "max", &Uniform::max)
    .method( "draw", &Uniform::draw)
    .method( "range", &range)
    ;
}
