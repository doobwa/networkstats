require(pkg)
library(inline)
library(network)
require(sna)
require(ergm)
source('R/utils.R')

library(Runit)
inc  <- '
	std::string hello(){
		return "hello" ;
	}
	int bar( int x){
		return x*2 ;
	}
	double foo( int x, double y){
		return x * y ;
	}
	void bla( ){
		Rprintf( "hello\\n" ) ;
	}
	void bla1( int x){
		Rprintf( "hello (x = %d)\\n", x ) ;
	}
	void bla2( int x, double y){
		Rprintf( "hello (x = %d, y = %5.2f)\\n", x, y ) ;
	}
        int bla3( IntegerVector x ) {
              return sum(x);
        }
        int bla4( IntegerVector x ) {
          int *y = x.begin();
          return y[0]+y[1];
        }
	class World {
	public:
	    World() : msg("hello"){}
	    void set(std::string msg_) { this->msg = msg_; }
	    std::string greet() { return msg; }
	private:
	    std::string msg;
	};
	void clearWorld( World* w ){
		w->set( "" );
	}
	RCPP_MODULE(yada){
		using namespace Rcpp ;
		function( "hello" , &hello ) ;
		function( "bar"   , &bar   ) ;
		function( "foo"   , &foo   ) ;
		function( "bla"   , &bla   ) ;
		function( "bla1"  , &bla1   ) ;
		function( "bla2"  , &bla2   ) ;
		function( "bla3"  , &bla3   ) ;
		function( "bla4"  , &bla4   ) ;
		class_<World>( "World" )
		    .constructor()
			.method( "greet", &World::greet )
			.method( "set", &World::set )
			.method( "clear", &clearWorld )
		;
	}
	'
fx <- cxxfunction( signature(), "" , include = inc, plugin = "Rcpp" )

mod <- Module( "yada", getDynLib(fx) )
checkEquals( mod$bar( 2L ), 4L )
checkEquals( mod$foo( 2L, 10.0 ), 20.0 )
checkEquals( mod$bla3( 1:3 ), 6L )
checkEquals( mod$bla4( c(2,10,1) ), 12L )
checkEquals( mod$hello(), "hello" )
checkEquals( capture.output( mod$bla() ), "hello" )
checkEquals( capture.output( mod$bla1(2L) ), "hello (x = 2)" )
checkEquals( capture.output( mod$bla2(2L, 5.0) ), "hello (x = 2, y =  5.00)" )

World <- mod$World
w <- new( World )
checkEquals( w$greet(), "hello" )
w$set( "hello world" )
checkEquals( w$greet(), "hello world" )
w$clear( )
checkEquals( w$greet(), "" )

inc <- '
#include "/home/chris/Documents/networkstats/pkg/src/edgetree.h"
#include "/home/chris/Documents/networkstats/pkg/src/wtedgetree.h"
#include "/home/chris/Documents/networkstats/pkg/src/model.h"
#include "/home/chris/Documents/networkstats/pkg/src/changestat.h"
#include "/home/chris/Documents/networkstats/pkg/src/changestats.h"
'
a <- modfunction('change_score_network2', 'tests/ChangeScoreNetwork.txt.cpp',
                 includes=inc, plugin='Rcpp', verbose=TRUE)

inc <- paste(readLines('tests/ChangeScoreNetwork.txt.cpp'),collapse="\n")
fx <- cxxfunction( signature(), "" , include = inc, plugin = "Rcpp" )
a <- Module( "change_score_network2", getDynLib(fx) )

csn <- new(a$ChangeScoreNetwork2,1:5,10:20)
csn$getEdgelist()

#nw <- network.initialize(5,directed=FALSE)
source("R/networkcs.R")
edges <- cbind(c(1,2,3,4,2),c(1,1,1,1,3))
nw <- as.network(edges)
csn.stuff <- network.for.changescores(nw ~ edges + triangles)

edgelist <- as.edgelist(csn.stuff$nw)
tails <- edgelist[,2]
heads <- edgelist[,1]
nedges <- nrow(edgelist)

csn$initializeNetwork(as.integer(tails), as.integer(heads),as.integer(nedges),
                      as.integer(csn.stuff$Clist$n),as.integer(csn.stuff$Clist$dir),
                      as.integer(csn.stuff$Clist$bipartite))
csn$getNumEdges()

nw <- new(a$ChangeScoreNetwork2,5,c(5,5))
nw$currentEdgelist()
nw$greet()

nw$set("hello everybody")
nw$greet()
