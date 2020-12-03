/* Translate NETGEN format to BERTSEKAS' format */
/* oct 24 1988 - Jorg Peters */

#include <stdio.h>

#define ARCS 70000
#define CHARLEN	 100
#define PERLINE	 10

int	    cost[ARCS],
	    cap[ARCS],
	    from[ARCS],
	    into[ARCS];

main( argc,argv)
int 	argc;
char 	*argv[];
{
    int 	i,j,
		divg,
		pbm,
		arcs,
		nodes,
		sources,
		sinks,
		seed,
		totsupply,
		art_source,
		art_sink;
    char	junk[CHARLEN];
    FILE	*fpi;

      /* Read the NETGEN output */
    if ((fpi = fopen(argv[1],"r")) == NULL) {
	printf("NETJOR: can't open file '%s'.\n",argv[1]); exit(); }
    fscanf(fpi,"%[BEGIN\n NETGEN PROBLEM]s",junk);
    fscanf(fpi,"%d",&pbm);
    fscanf(fpi,"%d",&nodes);
    fscanf(fpi,"%[NODES AND]s",junk);
    fscanf(fpi,"%d",&arcs);
    fscanf(fpi,"%[ARCS\n USER:]s",junk);
    fscanf(fpi,"%d",&seed);
    fscanf(fpi,"%d",&sources);
    fscanf(fpi,"%d",&sinks);
    fscanf(fpi,"%d",&i); 	/*mincost*/
    fscanf(fpi,"%d",&i);		/*maxcost*/
    fscanf(fpi,"%d",&totsupply);
    fscanf(fpi,"%[\n DATA:]s",junk);
    fscanf(fpi,"%d",&i);
    fscanf(fpi,"%d",&i);
    fscanf(fpi,"%d",&i);
    fscanf(fpi,"%d",&i);
    fscanf(fpi,"%d",&i);
    fscanf(fpi,"%d",&i);
    fscanf(fpi,"%[\n SUPPLY]s",junk);
    art_source = nodes+1;
    art_sink = nodes+2;
    for (i=0; i<sources; i++) {
	fscanf(fpi,"%d \n",&j);
	fscanf(fpi,"%d \n",&divg);
	from[i] = art_source;
	into[i] = j; 
	cost[i] = 0;
	cap[i]  = divg;
    }
    fscanf(fpi,"%[\n ARCS]s",junk);
    for (; fscanf(fpi,"%d %d %d %d\n",&from[i],&into[i],&cost[i],&cap[i]) == 4; i++);
    fscanf(fpi,"%[\n DEMAND]s",junk);
    for (; fscanf(fpi,"%d %d\n",&j,&divg) == 2; i++) {
	from[i] = j;
	into[i] = art_sink; 
	cost[i] = 0;
	cap[i]  = divg;
    }
      /* Print original arcs augmented by arcs 
      that move divergences to the artificial
      sink and source. */
    printf("%d %d\n", nodes,i);
    Out(from,i);
    Out(into,i);
    Out(cost,i);
    Out(cap,i);
}

Out(a,true_arcs)
int	a[],true_arcs;
{
    int		i,j,base,big,rest;

    big = true_arcs/PERLINE;
    rest= true_arcs%PERLINE;
    for (i=0; i<big; i++) {
	base = i*PERLINE;
	for (j=0; j<PERLINE; j++)
	    printf("%6d ",a[base+j]);
	printf("\n");
    }
    base = i*PERLINE;
    for (j=0; j<rest; j++)
	printf("%6d ",a[base+j]);
    if (rest != 0) printf("\n");
}


