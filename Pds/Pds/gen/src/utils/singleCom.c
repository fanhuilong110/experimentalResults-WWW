/* Gary Schultz
 *
 * singleCom.c
 *
 * Given an input file for nodes and one for arcs, an output file and a
 * commodity number, extract the single commodity from the data.
 *
 * In keeping with the assumptions of the main program, output should be
 * sent to stdout only if verbose.
 *
 *Modified 6/22/89 to create output in NETGEN format Jonathan Yackel
 *
 */

#include <stdio.h>

/* Open the node file. */
#define NOD_OP \
	nFil = fopen( nFilName, "r" ); \
	if ( NULL == nFil ) { \
	    if ( verbose ) \
		printf( "Error: could not open <%s>.\n", nFilName ); \
		return; \
	}
/* Close the node file. */
#define NOD_CL \
	fclose( nFil );

#define MAXLEN 64

char nFilName[MAXLEN], aFilName[MAXLEN], mFilName[MAXLEN], outName[MAXLEN];

singleCom( nFil, aFil, sFil, out, com, verbose )
FILE *nFil, *aFil, *sFil, *out;
int com, verbose;
{
    int num, c, ac, from, too, mut;
    int num_nodes;
    int maxcap;
    int narcs;
    int i, intdiv;
    float div, cost, cap;
    int done;	/* flag for end of supply nodes */
    FILE *a_out, *n_out;
    char s[25];
    int arcid;

    /* Know this much a priori. */
    if ( ( 0 >= com ) || ( 11 < com ) ) {
	if ( verbose ) printf( "Commodity %d does not exist.\n", com );
    	return;
    }

/* open a_out and n_out for output in Jorg's format */
sprintf(s, "a1");
a_out = fopen(s, "w");
if ((a_out == NULL)&&(verbose))
	{
	printf("can't open a1 for output\n");
	return;
	}
sprintf(s, "n1");
n_out = fopen(s, "w");
if ((n_out == NULL)&&(verbose))
	{
	printf("can't open n1 for output\n");
	return;
	}

/* 
 * Read the summary file data
 */

/* Where the equals signs are in the sumary file. */
#define EQUAL_COM 24
#define EQUAL_NOD 17
#define EQUAL_ARC 16
#define EQUAL_JNT 39
#define MAXLINE 256

    {
    int ok;
    char line[MAXLINE];
    char *res;

    /* Read through file until the number of commodities is found. */
    while (1) {
	res = fgets( line, MAXLINE, sFil );
	if ( NULL == res ) return( 0 );
	if ( '=' == line[EQUAL_COM] ) break;
    }

    /* Read the number of arcs. */
    res = fgets( line, MAXLINE, sFil );
    if ( NULL == res ) return( 0 );
    if ( '=' != line[EQUAL_ARC] ) return(0);
    sscanf( &line[EQUAL_ARC+1], "%d", &narcs );

    /* Read the number of nodes. */
    res = fgets( line, MAXLINE, sFil );
    if ( NULL == res ) return( 0 );
    if ( '=' != line[EQUAL_NOD] ) return(0);
    sscanf( &line[EQUAL_NOD+1], "%d", &num_nodes);

    /* Read the number of joint constraints. */
    res = fgets( line, MAXLINE, sFil );
    if ( NULL == res ) return( 0 );
    if ( '=' != line[EQUAL_JNT] ) return(0);
    /*sscanf( &line[EQUAL_JNT+1], "%d", &state->joint );*/
    }

    /* print the header data */
    fprintf(out, "BEGN\n");
    fprintf(out, "ROBLEM %8d %19d NODES AND %9d ARCS\n", 101, num_nodes, narcs);
    fprintf(out, "USER: %10d %10d %10d %10d %10d %10d\n", 1, 1, 1, 1, 1, 1);
    fprintf(out, "DATA: %10d %10d %10d %10d %10d %10d\n", 1, 1, 1, 1, 1, 1);

    /* Process supply nodes. */
    fprintf(out, "SUPPLY\n");
    done = 0;
    while (done == 0)
    {
	getNodes( nFil, &num, &c, &div );
	if (div <= 0) done = 1;
	else
	if ( c == com ) 
		{
		int intdiv;

		intdiv = (int)div;
		maxcap = (int) div;
		fprintf( out, "      %6d %20d\n", num, intdiv );
		fwrite(&intdiv, sizeof(int), 1, n_out);
		}
    }

    /*		     */
    /* Process arcs. */
    /*		     */
    arcid = 1;
    fprintf(out, "ARCS\n");
    while ( 0 != getArcs( aFil, &ac, &from, &too, &cost, &cap, &mut ) ) {

      int intcost, intcap;

	intcost = (int)cost;
	intcap = (int)cap;

      if ( ac == com )
	if (cap == -1)
	  {
	  fprintf( out, "      %6d %6d %20d %20d\n", from, too, intcost, maxcap);
	  fwrite(&from, sizeof(int), 1, a_out);
	  fwrite(&too, sizeof(int), 1, a_out);
	  fwrite(&intcost, sizeof(int), 1, a_out);
	  fwrite(&maxcap, sizeof(int), 1, a_out);
	  }
	else
	{
	fprintf( out, "      %6d %6d %20d %20d\n", from, too, (int)cost, (int)cap);
	  fwrite(&from, sizeof(int), 1, a_out);
	  fwrite(&too, sizeof(int), 1, a_out);
	  fwrite(&intcost, sizeof(int), 1, a_out);
	  fwrite(&intcap, sizeof(int), 1, a_out);
	}
	arcid++;
   }

    /* Process demand nodes. */
    fprintf(out, "DEMAND\n");
    if ((div < 0)&&( c == com )) 
	{
	int intdiv;
	intdiv = (int)div;

   	fprintf( out, "      %6d %20d\n", num, -(int)div );
	fwrite(&intdiv, sizeof(int), 1, n_out);
	}

    while ( 0 != getNodes( nFil, &num, &c, &div ) ) {
	if (div > 0)
		{
		if (verbose) printf("Error: supply nodes must precede demand nodes\n");
		return;
		}
	else 
	if ((div < 0)&&( c == com )) 
		{
		int intdiv;

		intdiv = (int)div;
		fprintf( out, "      %6d %20d\n", num, -intdiv );
		fwrite(&intdiv, sizeof(int), 1, n_out);
		}

    }
	/* output for trans-shipment points */
	intdiv = 0;
	for (i = 2; i< num_nodes; i++)
		fwrite(&intdiv, sizeof(int), 1, n_out);

    fprintf(out, "END\n");
} /* singleCom */

