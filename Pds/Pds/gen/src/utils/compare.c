/* read in 2 files, one in NETGEN format and the other ion PARNET format */
/* print out error messages if the 2 networks described are not identical*/

#include <stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
	FILE *fopen(), *parnfile, *parafile, *netfile;

	int pdiv, ndiv;
	int pfrom, pto, pcost, pcap;
	int nfrom, nto, ncost, ncap;
	int narcs, parcs, nnodes, pnodes;
	int done=0;
	int i;

	char * s;

	s = (char*) calloc(120, sizeof(char));

	/* open files */
	if (NULL == (netfile = fopen ("output", "r")))
		printf("can't open netgen file\n");

	if (NULL == (parnfile = fopen ("n1", "r")))
		printf("can't open parnet node file\n");

	if (NULL == (parafile = fopen ("a1", "r")))
		printf("can't open parnet arc file\n");

	fscanf(netfile, "%*s %*s %*d %*d %*s %*s %*d %*s");
	fscanf(netfile, "%*s %*d %*d %*d %*d %*d %*d");
	fscanf(netfile, "%*s %*d %*d %*d %*d %*d %*d");
	fscanf(netfile, "%*s");

	/* check supply node data */
	fscanf(netfile, "%*d %d", &ndiv);
	fread (&pdiv, 4, 1, parnfile);
	if (pdiv != ndiv) printf("Div  netgen %d parnet %d\n", ndiv, pdiv);

	fscanf(netfile, "%*s");
	/*check arc data */
	for(i=0; done!=1; i++)		/* read arc data */
		{
		fscanf(netfile, "%s", s);
		if (strcmp(s, "DEMAND") != 0)
			{
			nfrom = atoi(s);
			fscanf(netfile, "%d %d %d", &nto, &ncost, &ncap);
			fread(&pfrom, 4, 1, parafile);
			fread(&pto, 4, 1, parafile);
			fread(&pcost, 4, 1, parafile);
			fread(&pcap, 4, 1, parafile);
			if (nfrom != pfrom) printf("From netgen %d parnet %d\n", nfrom, pfrom);
			if (nto != pto) printf("to netgen %d parnet %d\n", nto, pto);
			if (ncost != pcost) printf("cost netgen %d parnet %d\n", ncost, pcost);
			if (ncap != pcap) printf("cap netgen %d parnet %d\n", ncap, pcap);
			}
		else done =1;
		}

	/* check demand node data */
	fscanf(netfile, "%*d %d", &ndiv);
	fread (&pdiv, 4, 1, parnfile);
	if (pdiv != ndiv) printf("Div  netgen %d parnet %d\n", ndiv, pdiv);

}
