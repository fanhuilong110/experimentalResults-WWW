/* Gary Schultz
 *
 * cordForm.c
 *
 * Put in format of my coordinator program.
 */

#define NCOM 11
#define STACKSIZE 9000

#define NOT_PART -2.0

#define WRITE_ARC(a) \
	fprintf( out, "\t%d\t%d\t%f", arc[a].from, arc[a].too, arc[a].cost ); \
	for ( j = 0 ; j < NCOM ; j++ ) { \
	    fprintf( out, "\t%f", \
		( NOT_PART == arc[a].upper[j] ? (float)0.0 : arc[a].upper[j] ) \
	    ); \
	} \
	fprintf( out, "\n" );

#include <stdio.h>

typedef struct {

    int from, too;

    float cost, upper[NCOM];

} ARCTYPE;

cordForm( which, aFil, out, verbose )
char which;
FILE *aFil, *out;
int verbose;
{
    int comm, from, too, mut;
    float cost, cap;
    int arcNum;
    ARCTYPE arc[STACKSIZE];	int topOfStack = 0;
    int existArcs;
    int this;
    int participating;
    int numArcs[NCOM];
    int perm[NCOM];		int ordered = 0;
    int temp;
    int i, j;

    for ( i = 0 ; i < NCOM ; i++ ) {
	numArcs[i] = 0;
	perm[i] = i;
    }

    if ( 0 == getArcs( aFil, &comm, &from, &too, &cost, &cap, &mut ) ) {
	if (verbose) printf( "No arcs in file.\n" );
	fflush( out );
	return;
    }

    for ( arcNum = 0, existArcs = (1==1) ; existArcs ; arcNum++ ) {

	/* Read in the data for another arc. */

	arc[topOfStack].from = from;
	arc[topOfStack].too = too;
	arc[topOfStack].cost = cost;
	for ( i = 0 ; i < NCOM ; i++ ) {
	    arc[topOfStack].upper[i] = NOT_PART;
	}

	for ( participating = -1 , this = (1==1)
	    ; this
	    ; participating++
	) { /* Read other lines in arc file. */

	    /* If arc is same. */
	    if ( ( from == arc[topOfStack].from )
		&& ( too == arc[topOfStack].too )
	    ) {

		if (existArcs) {

		    /* Set upper bound. */
	    	    arc[topOfStack].upper[comm-1] = cap;

		    numArcs[comm-1]++;

	    	    /* Check to make sure cost is same. */
	    	    if ( cost != arc[topOfStack].cost ) {
		    	if (verbose) printf(
			"cordForm: cost is not same in arc from %d, to %d.\n"
			    , from, too );
		    	fflush( out );
		    	return;
		    }

		    /* Read another one. */
		    if ( 0 ==
		    	getArcs( aFil, &comm, &from, &too, &cost, &cap, &mut )
	    	    ) {
		    	existArcs = (0==1);
	    	    }

		} else this = (0==1);

		if ( NOT_PART == cap ) {
		    if (verbose) printf( "cordForm: NOT_PART is in file.\n" );
		    fflush( out );
		    return;
		}

	    } else {
		this = (0==1);
	    }

	} /* Read other lines in arc file. */

	/* If all commodities participate. */
	if ( NCOM == participating ) {

	    if ( 'G' == which ) { WRITE_ARC(topOfStack) }

	} else { /* Not all commodities participate. */

	    /* Check to make sure the commodities that participated in
	     * the previously found ``not all'' arcs are participating in 
	     * this arc.  This means that, by ordering these arcs via a
	     * stack, we can use the same from and too lists, only with
	     * a different size for each commodity.
	     */
	    if ( ordered <= participating ) {

		/* Bring the newly participating commodities into the perm. */
		for ( i = ordered ; i < NCOM ; i++ ) {
		    if ( NOT_PART != arc[topOfStack].upper[perm[i]] ) {
			/* switch */
			temp = perm[ordered];
			perm[ordered] = perm[i];
			perm[i] = temp;
			ordered++;
		    }
		}

		if ( ordered != participating ) {
		    if (verbose) printf(
			"cordForm: internal error counting ordered.\n" );
		    fflush( out );
		    return;
		}

		/* Check participation. */
		for ( i = 0 ; i < ordered ; i++ ) {
		    if ( NOT_PART == arc[topOfStack].upper[perm[i]] ) {
			if (verbose) printf(
			    "cordForm: order does not work.\n" );
			fflush( out );
			return;
		    }
		}

	    } else {

		if ( verbose ) {
		    printf( "cordForm: number of partial participants" );
		    printf( " must be nondecreasing.\n" );
		}
		fflush( out );
		return;
	    }

	    topOfStack++; /* Push on stack. */
	    if ( STACKSIZE == topOfStack ) {
		if (verbose) printf( "cordForm: out of stack space.\n" );
		fflush( out );
		return;
	    }

	} /* Not all commodities participate. */

    } /* for all arcs... */

    /* Write out the stack. */
    if ( 'G' == which ) {
    	for ( topOfStack-- ; topOfStack >= 0 ; topOfStack-- ) {
	    WRITE_ARC(topOfStack)
    	}
    }

    fprintf( out, "\n\n" );
    for ( i = 0 ; i < NCOM ; i++ ) {

    	fprintf( out, "\t%d", numArcs[i] );

    }
    fprintf( out, "\n" );

} /* cordForm */

