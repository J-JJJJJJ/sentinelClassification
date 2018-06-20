# sentinelClassification
This repository is used for running continuous network SIR infections on a set of training graphs. The results of these infections are used to label those vertices that tend to get infected at an early stage in the infection process, they are called sentinels. 

The goal is to use machine learning techniques to classify vertices as sentinels. To do that, vertex properties are computed to use as features in the machine learning. *smallTable.csv* can be used directly as data for machine learning, the *sentClass* column indicates if a vertex was classified as sentinel or not. The *sentinel* and *sentinelVar* columns represent the expected value and variance of the time until the vertex is infected or the infection dies out, respectively.

Some of the training graphs are taken from this [Github repository accompanying a paper on community detection](https://github.com/altsoph/community_loglike).

## Rapid Miner directory
The RapidMiner directory can be read using [RapidMiner Studio](https://rapidminer.com/products/studio/). To do so the directory should be placed in RapidMiner's *local repository*. The *.rmp* files (RapidMiner Process files) can then be executed from within Rapid Minder Studio. 

## The R files
The R files are used to create some of the files in the other directories and the smallTable.csv. They should be executed in this order:
1. *artificialgraph.r* creates artificial training and test graphs. The training graphs are written as an edge list to the *graphs/artificial/* directory and the test graphs are written as [graphML](http://graphml.graphdrawing.org/) files to the *test graphs/* directory.

2. *graphs.r* reads the *graphs/* directory and creates graphs based on the files in that directory and the [Erdős–Rényi](https://en.wikipedia.org/wiki/Erd%C5%91s%E2%80%93R%C3%A9nyi_model) and [hierarchical configuration models (HCM)](https://www.nature.com/articles/srep29748). The HCM use the *graphFunctions.r* to rewire a set of edges. The resulting graphs are stored as *graphML* files in the *graphs without prop/* directory. 

3. The *runinfection.r* file is used to run infection simulations in parallel. For that purpose, the *infection.r* file is used to run a single infection simulation. The infection results are stored in the *infectionresults/* and *fullinfection/* directories. The first contains summarised results used for the machine learning. The second is not included in this Github repository (due to its size), however, the *runinfection.r* does produce these results (need to create that directly yourself), which consist of the full infection results for every simulation (the infection and recovery times, called *Sduration* and *SIduration*, respectively).

4. Finally, *smallTable.r* combines the results in the *infectionresult/* directory into a single table: *smallTable.csv*. This table is already included in the *RapidMiner directory* (named *smallTable.ioo*).
