#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
#include <cstring>


char *convert(const std::string & s)
  {
    char *pc = new char[s.size()+1];
    std::strcpy(pc, s.c_str());
    return pc; 
  }

 
class Foo {
public:
  Foo(IntegerVector tail) {
    this->tail = tail;
  }

  int convertExample() {
       std::vector<std::string>  vs;
       vs.push_back("std::string");
       vs.push_back("std::vector<std::string>");
       vs.push_back("char*");
       vs.push_back("std::vector<char*>");
       std::vector<char*>  vc;

       std::transform(vs.begin(), vs.end(), std::back_inserter(vc), convert);	

       for ( size_t i = 0 ; i < vc.size() ; i++ )
            std::cout << vc[i] << std::endl;

       for ( size_t i = 0 ; i < vc.size() ; i++ )
            delete [] vc[i];
       return 0;
  }
private:
  IntegerVector tail;
};

RCPP_MODULE(foo_mod){
	using namespace Rcpp ;
	class_<Foo>( "Foo" )
          .constructor<IntegerVector>()    
          .method( "convertExample", &Foo::convertExample ,"")
	;
}                     
