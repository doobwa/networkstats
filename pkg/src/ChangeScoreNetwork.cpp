#include <Rcpp.h>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
#include <cstring>
#include "edgetree.h"
#include "wtedgetree.h"
#include "model.h"
#include "changestat.h"
#include "changestats.h"
// TODO: Try and not use the .c files.  Hitting dyn.load errors without them.
#include "edgetree.c"
#include "model.c"
#include "changestat.c"
using namespace Rcpp;

class ChangeScoreNetwork {
public:

  ChangeScoreNetwork(int tail) {
    //    IntegerVector a;
    // this->hasNetwork = FALSE;
    // this->hasModel = FALSE;
    this->tail = tail;
    // this->head = head;
  }

  void initializeNetwork(IntegerVector tailsv, IntegerVector headsv, int nedges, 
                         int nnodes, int directed_flag, int bipartite) {
    // IntegerVector required for passing int arrays with Rcpp
    this->hasNetwork = TRUE;
    int* tails = tailsv.begin();
    int* heads = headsv.begin();
    int lasttoggle_flag = 0;
    //    printf("hello\n");
    this->nw = NetworkInitialize(tails, heads, nedges, nnodes, 
                                 directed_flag, bipartite, lasttoggle_flag);
  }
  void initializeModel(CharacterVector funnamesv, CharacterVector sonamesv, 
                       NumericVector inputsv, IntegerVector ntermsv) {
    //    char* funnames = funnamesv.begin();//
    //    char funnames[] = { 'H', 'e', 'l', 'l', 'o', '\0' }; 
    this->hasModel = TRUE;
    char funnames[]= "edges triangle";
    char sonames[] = "pkg pkg";//"pkg pkg";
    double inputs[] = {0,1,0,0,1,0};
    int nterms = 10;
    char *fnames = funnames;
    char *snames = sonames;
    double *inp = inputs;
    this->m = ModelInitialize(fnames, snames, inp, nterms);
    //    double stats[nterms] = {0};
  }
  void toggleEdge(int tail, int head) {
    if (this->hasNetwork) {
      ToggleEdge(tail,head,&this->nw);
    }
    // TODO: Update statistics
  }
  void toggleEdgelist(IntegerVector toggletails, IntegerVector toggleheads) {
    if (!this->hasNetwork) { 
      printf("Need to initialize network\n");
      return;
    }
    if (!this->hasModel) { 
      printf("Need to initialize model\n");
      return;
    }
    Model *m = this->m;
    Network nw = this->nw;
    int ntoggles = toggletails.size();
    
    for (unsigned int termi=0; termi < m->n_terms; termi++)
      m->termarray[termi].dstats = m->workspace;
  
    for(Edge e=0; e < ntoggles; e++){
      // ModelTerm *mtp = m->termarray;
      // double *statspos=stats;
    
      // for (unsigned int termi=0; termi < m->n_terms; termi++, mtp++){
      //   (*(mtp->d_func))(1, toggletails+e, toggleheads+e, 
      //                    mtp, nw);  /* Call d_??? function */
      //   for (unsigned int i=0; i < mtp->nstats; i++,statspos++)
      //     *statspos += mtp->dstats[i];
      // }
    //      ToggleEdge(toggletails[e],toggleheads[e],&this->nw);
    }
  }
  
  int getNumEdges() {
    return this->nw.nedges;
  }
  int getNumTerms() {
    return this->m->n_terms;
  }
  double getStats() {
    double **dstatarray = this->m->dstatarray;
    //    double *firstterm = *(dstatarray[0]);
    double firststat = dstatarray[0][0];
    //    double *stats[] 
    return firststat;//this->m->n_stats;
  }
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
  Network nw;
  Model* m;
};

RCPP_MODULE(change_score_network){
  using namespace Rcpp ;
  class_<ChangeScoreNetwork>( "ChangeScoreNetwork" )
    .constructor<int>()    
    //          .method( "getEdgelist", &ChangeScoreNetwork::getEdgelist , "get the message" )
    .method("initializeNetwork", &ChangeScoreNetwork::initializeNetwork, 
            "set the message" )
    .method("initializeModel", &ChangeScoreNetwork::initializeModel,"")
    .method("getNumEdges", &ChangeScoreNetwork::getNumEdges,  "set the message" )
    .method("getNumTerms", &ChangeScoreNetwork::getNumTerms, "" )
    .method("getStats", &ChangeScoreNetwork::getStats, "set the message" )
    .method("toggleEdge", &ChangeScoreNetwork::toggleEdge, "set the message" )
    .method("toggleEdgelist", &ChangeScoreNetwork::toggleEdgelist, "set the message" )
	;
}                     

