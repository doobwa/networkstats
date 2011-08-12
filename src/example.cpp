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
