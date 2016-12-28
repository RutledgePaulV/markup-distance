### markup-distance

An exploration of invariants that describe the structure of xml
documents in order to support similarity searches (such as k-means) of
xml documents and/or fragments. The long term goal is that an indexed
store of these invariants mapped to real html pages with styling provides
a mechanism for auto-styling new pages based simply on their markup.

Very much a work in progress at the moment :)


Similarity between trees is an implementation of Tekli's edit distance algorithm
as described in [Efficient XML Structural Similarity Detection using Sub-tree
Commonalities](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.714.7129&rep=rep1&type=pdf).