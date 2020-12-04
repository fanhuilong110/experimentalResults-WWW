# experimentalResults-WWW
## A Novel Method for Solving the Multi-Commodity Flow Problem in Spatio-Temporal Dynamic Networks. 
The project contains experimental data, parameter settings and comparative experimental results.
data sources:  http://www.di.unipi.it/di/groups/optimize/Data/MMCF.html.
## 6.1 Datesets Details
  The specialized algorithm of the section 4 was tested using two sets of quadratic multicommodity instances. As far as we know, there is no standard set of MMCF problems in the dynamic networks. We generate dynamic network multi-commodity network instances obtained with the well-known PDS [48] generators¬ . We dynamically change the number of edges and the number of commodity demand in each group of instances, and each change represents a dynamic update of the network. The detailed information of the Initial dataset is shown in Table 6.
<div align=center>Table 6:  Initial Data Set State Parameters</div>
<div align=center><img src="https://github.com/fanhuilong110/experimentalResults-WWW/blob/main/dataset.png" width="500" height="434" /></div>
  As shown in Table 6, m represents the number of vertices in the network, n represents the number of arcs, and k represents the type of commodities.
## 6.2 Parameter Settings
  Each data instance is updated every 2 seconds, and the updated content is the addition and deletion of edges. The total number of updates is 100. The number of source nodes 0<s<10, the number of sink points 0<d<20, the number of each commodity demanded by the source node n>1.
## 6.3 Experimental Results
  We compared the time consumption of the CPLEX and IPM solvers on the minimum cost multi-object network flow problem. In this experiment, we mainly compare the solution time of the algorithm on the CPU. As shown in Table 7, we mainly focused on the experiments on the PDS data set. We divided the PDS into 10 data sets, of which 10 data sets contained a total of 100 network updates. It can be seen from Table 7 that the CPLEX solver has the largest time consumption on different data sets, followed by IPM, and PAFUDN has the smallest time consumption. With the growth of the data set size and the dynamic changes of the network. The time consumption of the three algorithms on the data set is gradually increasing.
<div align=center>Table 7:  Comparison of the time consumed by different algorithms for PDS data update</div>
<div align=center><img src="https://github.com/fanhuilong110/experimentalResults-WWW/blob/main/result.png" width="500" height="246" /></div>
  As shown in Figure 7, the figure shows the time comparison of the three solving methods of CPLEX, IPM and PAFUDN on the data sets PDS1, PDS10, PDS30 and PDS40. It can be seen from the figure that as the number of data sets increases, the solving rate of CPLEX increases significantly, especially after PDS20, the time consumption of CPLEX increases sharply. The rate of increase of IPM and PAFUDN is basically the same before PDS20. After PDS20, the rate of increase of IPM time consumption is greater than that of PAFUDN.

<div align=center><img src="https://github.com/fanhuilong110/experimentalResults-WWW/blob/main/RS1.png" width="500" height="382" /></div>
<div align=center>Figure 7: Performance comparison of three algorithms on datasets PDS1-40</div>

<div align=center><img src="https://github.com/fanhuilong110/experimentalResults-WWW/blob/main/RS2.png" width="500" height="392" /></div>
<div align=center>Figure 8: Performance comparison of three algorithms on data set PDS50-90</div>
  As shown in Figure 7, the figure shows the time comparison of the three solving methods of CPLEX, IPM and PAFUDN on the data sets PDS50, PDS60, PDS70, PDS80 and PDS90. It can be seen from the figure that with the increase of the number of data sets, the solving rate of CPLEX shows a rapid upward trend, and the time consumption value is much larger than the two algorithms of IPM and PAFUDN. The rate of increase of IPM and PAFUDN is basically the same, and the rate of increase of IPM time consumption is greater than that of PAFUDN.
  In summary, on multiple data sets, the computational time complexity of CPLEX and IPM solvers is greater than PAFUDN. Therefore, the algorithm PAFUDN proposed in this paper has certain advantages in computing performance.
