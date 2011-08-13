#include "/home/chris/Documents/networkstats/pkg/src/edgetree.h"
#include "/home/chris/Documents/networkstats/pkg/src/wtedgetree.h"
#include "/home/chris/Documents/networkstats/pkg/src/model.h"
#include "/home/chris/Documents/networkstats/pkg/src/changestat.h"
#include "/home/chris/Documents/networkstats/pkg/src/changestats.h"
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
#include <cstring>
 
/*  
Author: Chris DuBois Module for computing changes in network
statistics after single edge insertions/deletions.  The Rcpp package is used to interface with the C/C++ datastructures and methods.  */


class ChangeScoreNetwork2 {
public:
  ChangeScoreNetwork2(IntegerVector tail,IntegerVector head) {
    this->hasNetwork = FALSE;
    this->hasModel = FALSE;
    this->tail = tail;
    this->head = head;
  }

  void initializeNetwork(IntegerVector tailsv, IntegerVector headsv, int nedges, 
                         int nnodes, int directed_flag, int bipartite) {
    // IntegerVector required for passing int arrays with Rcpp
    this->hasNetwork = TRUE;
    int* tails = tailsv.begin();
    int* heads = headsv.begin();
    int lasttoggle_flag = 0;

    this->nw = NetworkInitialize(tails, heads, nedges, nnodes, 
                                 directed_flag, bipartite, lasttoggle_flag);
  }
  void initializeModel(CharacterVector funnamesv, CharacterVector sonamesv, 
                       NumericVector inputsv, IntegerVector ntermsv) {
    //    char* funnames = funnamesv.begin();//
    //    char funnames[] = { 'H', 'e', 'l', 'l', 'o', '\0' }; 
    char funnames[]= {'e','d','g','e','s',' ','t','r','i','a','n','g','l','e','\0'}; 
    char sonames[] = {'e','r','g','m',' ','e','r','g','m','\0;'};
    double inputs[] = {0,1,0,0,1,0};
    int nterms = 1;
    this->m = ModelInitialize(funnames, funnames, inputs, nterms);
  }
  void toggleEdge(int tail, int head) {
    if (this->hasNetwork) {
      ToggleEdge(tail,head,&this->nw);
    }
  }
  void toggleEdgelist(IntegerVector tails, IntegerVector heads) {
    for (int i=0;i++;i<tails.size()) {
      toggleEdge(tails[i],heads[i]);
    }
  }
  
  int changescoreEdge(int tail, int head) {
  }
  int getNumEdges() {
    return this->nw.nedges;
  }
  double getStats() {
    return 5.0;//this->m->n_stats;
  }
  List getEdgelist() {
    List ret; 
    ret["tail"] = tail; 
    ret["head"] = head;
    return ret;
  }
  // char* convertString(std::string str) {
  //   std::vector<char> buffer(str.length() + 1, '\0');
  //   std::copy(str.begin(), str.end(), buffer.begin());
  //   char* cstr = &buffer[0];
  //   // char * writable = new char[str.size() + 1];
  //   // std::copy(str.begin(), str.end(), writable);
  //   // writable[str.size()] = '\0'; // don't forget the terminating 0
  //   return(cstr);
  // }
  // char** convertStringVector(std::vector<std::string> origVector) {
  //   std::vector<const char *> cStrArray;
  //   cStrArray.reserve(origVector.size());
  //   for(int index = 0; index < origVector.size(); ++index)  {
  //       cStrArray.push_back(origVector[index].c_str());
  //   }
  //   return &cStrArray[0];
  // }
private:
  IntegerVector tail, head;
  bool hasNetwork;
  bool hasModel;
  Network nw;
  Model* m;
};

RCPP_MODULE(change_score_network2){
	using namespace Rcpp ;
	class_<ChangeScoreNetwork2>( "ChangeScoreNetwork2" )
          .constructor<IntegerVector,IntegerVector>()    
          .method( "getEdgelist", &ChangeScoreNetwork2::getEdgelist , "get the message" )
          .method("initializeNetwork", &ChangeScoreNetwork2::initializeNetwork, 
                  "set the message" )
          .method("initializeModel", &ChangeScoreNetwork2::initializeModel,"")
          .method("getNumEdges", &ChangeScoreNetwork2::getNumEdges,  "set the message" )
          .method("getStats", &ChangeScoreNetwork2::getStats, "set the message" )
          .method("toggleEdge", &ChangeScoreNetwork2::toggleEdge, "set the message" )
          .method("toggleEdgelist", &ChangeScoreNetwork2::toggleEdgelist, "set the message" )
          // .method("initializeModel", &ChangeScoreNetwork2::initializeModel, 
          //         "set the message" )
	;
}                     
