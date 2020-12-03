/* Gary Schultz
 *
 * nodeCheck.c
 *
 * Given an input file for nodes and an output file
 * compute the max and min indices of nonzero divergences
 * in each commodity and the number of nodes.
 * Also sum the divergences; sum = 0 is a necessary condition for feasibility.
 *
 * In keeping with the assumptions of the main program, output should be
 * sent to stdout only if verbose.
 */

#define NCOM 11

#include <stdio.h>

nodeCheck( nFil, out, verbose )
FILE *nFil, *out;
int verbose;
{
    int num, c;
    float div;
    int minI[NCOM+1], maxI[NCOM+1]; /* Min and max indicies of nonzero div. */
    int linearly[NCOM+1]; /* Counts nodes; >=0 only if still in linear order. */
    float sum[NCOM+1];

    for ( c = 0 ; c <= NCOM ; c++ ) {
	linearly[c] = minI[c] = maxI[c] = 0;
	sum[c] = 0.0;
    }

    /* Process nodes. */
    while ( 0 != getNodes( nFil, &num, &c, &div ) ) {
	c--;

	if ( 0.0 != div ) {
	    sum[c] += div;
	    if ( 0 == minI[c] ) minI[c] = num;
	    maxI[c] = num;
	}

	if ( 0 <= linearly[c] ) {
	    linearly[c]++;
	    if ( num != linearly[c] ) linearly[c] *= -1; /* Negate. */
	}
	else {
	    linearly[c]--;
	}
    }

    /* Tally totals. */
    for ( c = 0 ; c < NCOM ; c++ ) {
	sum[NCOM] += sum[c];

	if ( 0 != minI[c] ) {
	    if ( 0 == minI[NCOM] ) minI[NCOM] = minI[c];
	    else minI[NCOM] = ( minI[c] < minI[NCOM] ? minI[c] : minI[NCOM] );

	    maxI[NCOM] = ( maxI[c] > maxI[NCOM] ? maxI[c] : maxI[NCOM] );
	}

	if ( 0 <= linearly[NCOM] ) {
	    if ( 0 <= linearly[c] )
		linearly[NCOM] = linearly[NCOM] + linearly[c];
	    else
		linearly[NCOM] = -linearly[NCOM] + linearly[c];
	}
	else {
	    linearly[NCOM] -= ( 0 <= linearly[c] ? linearly[c] : -linearly[c] );
	}
    }

    fprintf( out, "Node divergences by commodity.\n" );
    fprintf( out, " %4s%10s%10s", "comm", "nodes", "order" );
    fprintf( out, "%10s%10s", "min", "max" );
    fprintf( out, "%15s\n", "sum" );

    for ( c = 0 ; c <= NCOM ; c++ ) {

	if ( NCOM == c ) fprintf( out, " %4s", "all" );
	else fprintf( out, " %4d", c+1 );

	if ( 0 <= linearly[c] ) {
	    fprintf( out, "%10d%10s", linearly[c], "yes" );
	}
	else {
	    fprintf( out, "%10d%10s", -linearly[c], "no" );
	}

	if ( 0 == minI[c] ) {
	    fprintf( out, "%20s", "no such indices" );
	}
	else {
	    fprintf( out, "%10d%10d", minI[c], maxI[c] );
	}

	fprintf( out, "%15f\n", sum[c] );
    }

} /* nodeCheck */

