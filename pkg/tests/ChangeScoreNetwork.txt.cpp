/*  
Author: Chris DuBois Module for computing changes in network
statistics after single edge insertions/deletions.  The Rcpp package is used to interface with the C/C++ datastructures and methods.  */

class ChangeScoreNetwork2 {
public:
  ChangeScoreNetwork2(IntegerVector tail,IntegerVector head) {
    this->tail = tail;
    this->head = head;
  }
  void set(std::string msg) { 
    this->msg = msg; 
  }
  List currentEdgelist() {
    List ret; 
    ret["tail"] = tail; 
    ret["head"] = head;
    return ret;
  }
private:
  std::string msg;
  IntegerVector tail, head;
  Network nw;
};

RCPP_MODULE(change_score_network2){
	using namespace Rcpp ;
	class_<ChangeScoreNetwork2>( "ChangeScoreNetwork2" )
          .constructor<IntegerVector,IntegerVector>()    
          .method( "currentEdgelist", &ChangeScoreNetwork2::currentEdgelist , "get the message" )
          .method( "set", &ChangeScoreNetwork2::set     , "set the message" )
	;
}                     


