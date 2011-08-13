#include <Rcpp.h>
// #include <iostream>
// #include <string>
// #include <vector>
// #include <algorithm>
// #include <iterator>
// #include <cstring>
// #include "edgetree.h"
// #include "wtedgetree.h"
// #include "model.h"
// #include "changestat.h"
// #include "changestats.h"

class ChangeScoreNetwork {
public:

  ChangeScoreNetwork(int tail) {
    // this->hasNetwork = FALSE;
    // this->hasModel = FALSE;
    this->tail = tail;
    // this->head = head;
  }

  // void initializeNetwork(IntegerVector tailsv, IntegerVector headsv, int nedges, 
  //                        int nnodes, int directed_flag, int bipartite) {
  //   // IntegerVector required for passing int arrays with Rcpp
  //   this->hasNetwork = TRUE;
  //   int* tails = tailsv.begin();
  //   int* heads = headsv.begin();
  //   int lasttoggle_flag = 0;

  //   this->nw = NetworkInitialize(tails, heads, nedges, nnodes, 
  //                                directed_flag, bipartite, lasttoggle_flag);
  // }
  // void initializeModel(CharacterVector funnamesv, CharacterVector sonamesv, 
  //                      NumericVector inputsv, IntegerVector ntermsv) {
  //   //    char* funnames = funnamesv.begin();//
  //   //    char funnames[] = { 'H', 'e', 'l', 'l', 'o', '\0' }; 
  //   char funnames[]= {'e','d','g','e','s',' ','t','r','i','a','n','g','l','e','\0'}; 
  //   char sonames[] = {'e','r','g','m',' ','e','r','g','m','\0;'};
  //   double inputs[] = {0,1,0,0,1,0};
  //   int nterms = 1;
  //   //    this->m = ModelInitialize(funnames, funnames, inputs, nterms);
  // }
  // void toggleEdge(int tail, int head) {
  //   if (this->hasNetwork) {
  //     ToggleEdge(tail,head,&this->nw);
  //   }
  //   // TODO: Update statistics
  // }
  // void toggleEdgelist(IntegerVector tails, IntegerVector heads) {
  //   for (int i=0;i++;i<tails.size()) {
  //     toggleEdge(tails[i],heads[i]);
  //   }
  //   // TODO: Update statistics
  // }
  
  // int getNumEdges() {
  //   return this->nw.nedges;
  // }
  // double getStats() {
  //   return 5.0;//this->m->n_stats;
  // }
  // List getEdgelist() {
  //   List ret; 
  //   ret["tail"] = tail; 
  //   ret["head"] = head;
  //   return ret;
  // }
private:
  int tail;
  bool hasNetwork;
  bool hasModel;
  // Network nw;
  // Model* m;
};

RCPP_MODULE(change_score_network){
	using namespace Rcpp ;
	class_<ChangeScoreNetwork>( "ChangeScoreNetwork" )
          .constructor<int>()    
          // .method( "getEdgelist", &ChangeScoreNetwork::getEdgelist , "get the message" )
          // .method("initializeNetwork", &ChangeScoreNetwork::initializeNetwork, 
          //         "set the message" )
          // .method("initializeModel", &ChangeScoreNetwork::initializeModel,"")
          // .method("getNumEdges", &ChangeScoreNetwork::getNumEdges,  "set the message" )
          // .method("getStats", &ChangeScoreNetwork::getStats, "set the message" )
          // .method("toggleEdge", &ChangeScoreNetwork::toggleEdge, "set the message" )
          // .method("toggleEdgelist", &ChangeScoreNetwork::toggleEdgelist, "set the message" )
	;
}                     

