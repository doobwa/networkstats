class Example {
public:
  Example(double min_, double max_) : min(min_), max(max_) {}
  NumericVector draw(int n) const {
    RNGScope scope;
    return runif( n, min, max );
  }
  double min, max;
  Network nw;
};
double range( Example* w) {
  return w->max - w->min;
}
RCPP_MODULE(network_module) {
  class_<Example>( "Example" )
    .constructor<double,double>()
    .field( "min", &Example::min)
    .field( "max", &Example::max)
    .method( "draw", &Example::draw)
    .method( "range", &range)
    ;
}
