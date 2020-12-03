/* Gary Schultz
 *
 * mutlCheck.c
 *
 * Given an input file for nodes and an output file
 * compute the max and min joint arc capacities
 * and the number of joint arc capacity constraints.
 *
 * In keeping with the assumptions of the main program, output should be
 * sent to stdout only if verbose.
 */

#define NCOM 11

#include <stdio.h>

int mutlCheck( mFil, out, verbose )
FILE *mFil, *out;
int verbose;
{
    int num;
    float cap;
    float minC, maxC; /* Min and max joint capacities. */
    int linearly; /* Counts nodes; >=0 only if still in linear order. */

    minC = 1.0;
    maxC = -1.0;
    linearly = 0;

    /* Process constraints. */
    while ( 0 != getMutual( mFil, &num, &cap ) ) {

	if ( 0 == linearly ) minC = maxC = cap;

	minC = ( cap < minC ? cap : minC );
	maxC = ( cap > maxC ? cap : maxC );

	if ( 0 <= linearly ) {
	    linearly++;
	    if ( num != linearly ) linearly *= -1; /* Negate. */
	}
	else {
	    linearly--;
	}
    }

    fprintf( out, "Mutual capacities.\n" );
    fprintf( out, " %10s%10s", "number", "order" );
    fprintf( out, "%15s%15s\n", "min", "max" );

    if ( 0 <= linearly ) fprintf( out, " %10d%10s", linearly, "yes" );
    else fprintf( out, " %10d%10s", -linearly, "no" );

    if ( minC <= maxC ) fprintf( out, "%15f%15f\n", minC, maxC );
    else fprintf( out, "\n" );

    /* Return number of mutual constraints. */
    return( ( linearly >= 0 ? linearly : -linearly ) );

} /* mutlCheck */

