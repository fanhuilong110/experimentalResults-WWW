/* Gary Schultz
 *
 * readSerial.u
 *
 * This reads in the stuff which must be done in serial.
 */

ENV

#define DATAFILE "/usr1/rrm/schultz/trappist/MACTAPE/sumary"

/* Max size of lines to read in. */
#define MAXLINE 256

/* Where the equals signs are in the sumary file. */
#define EQUAL_COM 24
#define EQUAL_NOD 17
#define EQUAL_ARC 16
#define EQUAL_JNT 39

#include <stdio.h>
#include <math.h>
#include "data.h"
#include "global.h"

include(`include/write.inc')

readSerial( state )
PR_STATE * state;
{
    FILE * fil, * fopen();
    int ok;
    char line[MAXLINE];
    char *res;

    fil = fopen( DATAFILE, "r");

    if ( NULL == fil ) return(0);

    /* Read through file until the number of commodities is found. */
    while (1) {
	res = fgets( line, MAXLINE, fil );
	if ( NULL == res ) return( 0 );
	if ( '=' == line[EQUAL_COM] ) break;
    }
    sscanf( &line[EQUAL_COM+1], "%d", &state->ncom );
    printf( "%d commodities.\n", state->ncom );

    /* Read the number of arcs. */
    res = fgets( line, MAXLINE, fil );
    if ( NULL == res ) return( 0 );
    if ( '=' != line[EQUAL_ARC] ) return(0);
    sscanf( &line[EQUAL_ARC+1], "%d", &state->narc );
    printf( "%d arcs.\n", state->narc );

    /* Read the number of nodes. */
    res = fgets( line, MAXLINE, fil );
    if ( NULL == res ) return( 0 );
    if ( '=' != line[EQUAL_NOD] ) return(0);
    sscanf( &line[EQUAL_NOD+1], "%d", &state->node );
    printf( "%d nodes.\n", state->node );

    /* Read the number of joint constraints. */
    res = fgets( line, MAXLINE, fil );
    if ( NULL == res ) return( 0 );
    if ( '=' != line[EQUAL_JNT] ) return(0);
    sscanf( &line[EQUAL_JNT+1], "%d", &state->joint );
    printf( "%d joint constraints.\n", state->joint );

    fclose( fil );

    /* Return the number of things to push on the pool. */
    return( 4 );

} /* readSerial */


