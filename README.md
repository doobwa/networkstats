An R package implementing online methods for computing sufficient
statistics of networks.

Online a few "local" statistics are implemented. Note that a small
subset of network statistics are really feasible to do in an online
fashion.  For example, betweenness would be impossible.

The majority of this code came from the internals of ergm, written by
the author of the statnet family of packages.  Having these methods in
a standalone package offers several benefits, though: 1) other network
applications, 2) more easily extensible to new statistics, 3) possibly
make ergm cleaner and easier for a wider community to understand and
maintain.

NOTE: Update code only working for directed networks.
