/* Gary Schultz
 *
 * arcCheck.c
 *
 * Given an input file for arcs and an output file
 * compute the max and min capacities in each commodity and the number of
 * uncapacitated arcs in each commodity.
 *
 * In keeping with the assumptions of the main program, output should be
 * sent to stdout only if verbose.
 */

#define NCOM 11

/* The virtual minimum and maximum.  Numbers < 0 represents infinity. */
#define VMIN(a,b) ( a < 0 ? b : ( b < 0 ? a : ( a < b ? a : b ) ) )
#define VMAX(a,b) ( a < 0 ? a : ( b < 0 ? b : ( a < b ? b : a ) ) )
#define VADD(a,b) ( a < 0 ? a : ( b < 0 ? b : ( a+b ) ) )

#include <stdio.h>

arcCheck( aFil, mFil, out, verbose, numMutCon )
FILE *aFil, *mFil, *out;
int verbose;
int numMutCon;
{
    int c, from, too, mut;
    float cost, cap;
    float minB[NCOM+1], maxB[NCOM+1];
    float minC[NCOM+1], maxC[NCOM+1];
    int total[NCOM+1], uncap[NCOM+1], infin[NCOM+1];
    int *mutArray, totMut = 0;
    float *mutCap, totCap = 0.0;
    float *actCap, totActCap = 0.0;
    float *minCap, totMinCap = -1.0;
    int superflous = (1 == 1);
    int need, noneed = 0;
    char *malloc();

    /* Set up for the joint constraints if needed. */
    if ( 0 <= numMutCon ) {
	mutArray = (int *) malloc( (unsigned) ( sizeof(int)*(1+numMutCon) ) );
	mutCap = (float *) malloc( (unsigned) ( sizeof(float)*(1+numMutCon) ) );
	actCap = (float *) malloc( (unsigned) ( sizeof(float)*(1+numMutCon) ) );
	minCap = (float *) malloc( (unsigned) ( sizeof(float)*(1+numMutCon) ) );
    }
    else {
    	mutArray = NULL;
	mutCap = NULL;
	actCap = NULL;
	minCap = NULL;
    }

    for ( c = 0 ; c <= numMutCon ; c++ ) {
    	mutArray[c] = 0;
    	mutCap[c] = 0.0;
	actCap[c] = 0.0;
	minCap[c] = -1.0;
    }

    /* If infin[c] is 0 both minB[c] and maxB[c] are finite.
     * If infin[c] is not, both minB[c] and maxB[c] are infinite.
     */
    for ( c = 0 ; c <= NCOM ; c++ ) {
    	total[c] = uncap[c] = 0;
	infin[c] = 1;
    }

    /* Process arcs. */
    while ( 0 != getArcs( aFil, &c, &from, &too, &cost, &cap, &mut ) ) {
	c--;

	if ( ( numMutCon >= 0 ) && ( mut <= numMutCon ) ) {
	    mutArray[mut]++;
	    mutCap[mut] = VADD( cap, mutCap[mut] );
	    minCap[mut] = VMIN( minCap[mut], cap );
	}

	/* Check capacities. */
	if ( 0.0 > cap ) uncap[c]++; /* If uncapacitated. */
	else {
	    if ( 0 != infin[c] ) { /* This is the first capacitated found. */
		minB[c] = maxB[c] = cap;
		infin[c] = 0;
	    }
	    else {
		minB[c] = ( minB[c] < cap ? minB[c] : cap );
		maxB[c] = ( maxB[c] > cap ? maxB[c] : cap );
	    }
	}

	/* Check cost. */
	if ( 0 == total[c] ) {
	    minC[c] = maxC[c] = cost;
	}
	else {
	    minC[c] = ( minC[c] < cost ? minC[c] : cost );
	    maxC[c] = ( maxC[c] > cost ? maxC[c] : cost );
	}

	if ( ( cap >= 0.0 ) && ( mut > 0 ) ) noneed++;
	total[c]++; /* Count this arc. */

    } /* Process arcs. */

    if ( 0 <= numMutCon ) {
	while ( 0 != getMutual( mFil, &mut, &cap ) ) {
	    if ( mut <= numMutCon ) {
		actCap[mut] = cap;
		if ( actCap[mut] > minCap[mut] ) superflous = (0 == 1);
	    }
	}
    }

    /* Tally totals. */
    for ( c = 0 ; c < NCOM ; c++ ) {

	if ( 0 == infin[c] ) { /* This comm has some capacities. */
	    if ( 0 == infin[NCOM] ) { /* Have found capacities before. */
	    	minB[NCOM] = ( minB[c] < minB[NCOM] ? minB[c] : minB[NCOM] );
	    	maxB[NCOM] = ( maxB[c] > maxB[NCOM] ? maxB[c] : maxB[NCOM] );
	    }
	    else { /* These are the first capacities found. */
	    	infin[NCOM] = 0;
	    	minB[NCOM] = minB[c];
	    	maxB[NCOM] = maxB[c];
	    }
	}

	if ( 0 != total[c] ) { /* There were arcs in this commodity. */
	    if ( 0 != total[NCOM] ) { /* There were already arcs found. */
	    	minC[NCOM] = ( minC[c] < minC[NCOM] ? minC[c] : minC[NCOM] );
	    	maxC[NCOM] = ( maxC[c] > maxC[NCOM] ? maxC[c] : maxC[NCOM] );
	    }
	    else { /* These are the first arcs found. */
	    	minC[NCOM] = minC[c];
	    	maxC[NCOM] = maxC[c];
	    }
	}

	total[NCOM] += total[c];
	uncap[NCOM] += uncap[c];
    }
    for ( c = 1 ; c <= numMutCon ; c++ ) {
	totMut += mutArray[c];
	totCap = VADD( mutCap[c], totCap );
	totActCap = VADD( actCap[c], totActCap );
	totMinCap = VMIN( totMinCap, minCap[c] );
    }

    fprintf( out, "Arc capacities and costs by commodity.\n" );
    fprintf( out, " %4s%7s%7s", "comm", "total", "uncap" );
    fprintf( out, "%15s%15s", "min cap", "max cap" );
    fprintf( out, "%15s%15s\n", "min cost", "max cost" );

    for ( c = 0 ; c <= NCOM ; c++ ) {

	if ( NCOM == c ) fprintf( out, " %4s", "all" );
	else fprintf( out, " %4d", c+1 );

	fprintf( out, "%7d%7d", total[c], uncap[c] );

	if ( 0 != infin[c] ) {
	    fprintf( out, "%15s%15s", "uncapacitated", "uncapacitated" );
	}
	else {
	    fprintf( out, "%15f%15f", minB[c], maxB[c] );
	}

	if ( 0 != total[c] ) {
	    fprintf( out, "%15f%15f", minC[c], maxC[c] );
	}

	fprintf( out, "\n" );
    }

    if ( 0 < numMutCon ) {

	fprintf( out,
	    "Arc involvement in joint capacity constraints.\n" );
	fprintf( out, " " );
	fprintf( out, "%6s", "constr" );
	fprintf( out, "%6s", "# arc" );
	fprintf( out, "%16s", "implicit bnd" );
	fprintf( out, "%16s", "min ind bnd" );
	fprintf( out, "%16s", "explicit bnd" );
	fprintf( out, "%16s", "ratio" );
	fprintf( out, "\n" );

	for ( c = 1 ; c <= numMutCon ; c++ ) {
	    fprintf( out, " " );
	    fprintf( out, "%6d", c );
	    fprintf( out, "%6d", mutArray[c] );
	    if ( 0 <= mutCap[c] ) fprintf( out, "%16f", mutCap[c] );
	    else fprintf( out, "%16s", "infinity" );
	    if ( 0 <= minCap[c] ) fprintf( out, "%16f", minCap[c] );
	    else fprintf( out, "%16s", "infinity" );
	    if ( 0 <= actCap[c] ) fprintf( out, "%16f", actCap[c] );
	    else fprintf( out, "%16s", "infinity" );
	    if ( ( 0 <= mutCap[c] ) && ( 0 < actCap[c] ) )
		fprintf( out, "%16f", mutCap[c]/actCap[c] );
	    fprintf( out, "\n" );
	}

	fprintf( out, " " );
	fprintf( out, "%6s", "all" );
	fprintf( out, "%6d", totMut );
	if ( 0 <= totCap ) fprintf( out, "%16f", totCap );
	else fprintf( out, "%16s", "infinity" );
	if ( 0 <= totMinCap ) fprintf( out, "%16f", totMinCap );
	else fprintf( out, "%16s", "infinity" );
	if ( 0 <= totActCap ) fprintf( out, "%16f", totActCap );
	else fprintf( out, "%16s", "infinity" );
	if ( ( 0 <= totCap ) && ( 0 < totActCap ) )
	    fprintf( out, "%16f", totCap/totActCap );
	fprintf( out, "\n" );

	fprintf( out, " " );
	fprintf( out, "%6s", "unused" );
	fprintf( out, "%6d", mutArray[0] );
	if ( 0 <= mutCap[0] ) fprintf( out, "%16f", mutCap[0] );
	else fprintf( out, "%16s", "infinity" );
	if ( 0 <= minCap[0] ) fprintf( out, "%16f", minCap[0] );
	else fprintf( out, "%16s", "infinity" );
	fprintf( out, "\n" );

	if ( superflous ) {
	    fprintf( out,
		"All individual bounds on arcs participating in a joint\n" );
	    fprintf( out,
	    	" capacity constraint are redundant.\n" );
	    fprintf( out,
		" Using this fact we need to handle explicit bounds for\n" );
	    need = total[NCOM] - uncap[NCOM] - noneed;
	    fprintf( out,
		" only %d arcs (%g percent of the arcs) explicitly.\n", 
		need, (float)(100.0 * need / total[NCOM]) );
	}
	else {
	    fprintf( out,
	    	"Some of the individual bounds on jointly capacitated\n" );
	    fprintf( out,
	    	" arcs are necessary.\n" );
	}

	free( (char *) mutArray );
	free( (char *) mutCap );
	free( (char *) actCap );
	free( (char *) minCap );
    }

} /* arcCheck */

