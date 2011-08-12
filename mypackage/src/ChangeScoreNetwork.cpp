#include <Rcpp.h>
#include <edgetree.h>

class ChangeScoreNetwork {
public:
    ChangeScoreNetwork() : msg("hello"){}
    void set(std::string msg) { this->msg = msg; }
    std::string greet() { return msg; }
private:
    std::string msg;
};

RCPP_MODULE(change_score_network){
	using namespace Rcpp ;
	class_<ChangeScoreNetwork>( "ChangeScoreNetwork" )
	    .constructor()    
		.method( "greet", &ChangeScoreNetwork::greet , "get the message" )
		.method( "set", &ChangeScoreNetwork::set     , "set the message" )
	;
}                     

