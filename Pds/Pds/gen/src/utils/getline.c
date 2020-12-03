/* Gary Schultz
 *
 * getline.c
 *
 * These are routines to get the data in one line of each of the
 * pfgen output files.
 */

#define VERBOSE 0

/* Max size of lines to read in. */
#define MAXLINE 256

#include <stdio.h>

int getNodes( fp, num, comm, div )
FILE	*fp;
int	*num;
int	*comm;
float	*div;
{
    char line[MAXLINE];
    char *res;
    int ok;

    while (1) {

	res = fgets( line, MAXLINE, fp );
	if ( NULL == res ) return( 0 );

	/* Set 20th space equal to end of line. */
	line[20] = '\0';

#if VERBOSE
	printf( "%s\n", line );
#endif

	ok = sscanf( line, "%d %d %f", num, comm, div );
	if ( 3 == ok ) break;

    }

    return( 1 );

} /* getNodes */

int getArcs( fp, comm, from, too, cost, cap, mut )
FILE	*fp;
int	*comm;
int	*from;
int	*too;
float	*cost;
float	*cap;
int	*mut;
{
    char line[MAXLINE];
    char *res;
    int ok, i;

    while (1) {

	res = fgets( line, MAXLINE, fp );
	if ( NULL == res ) return( 0 );

	/* Set first 10 characters to blank. */
	for ( i = 0 ; i < 10 ; i++ ) {
	    line[i] = ' ';
	}
	/* Set 50th character to be end of line. */
	line[50] = '\0';

#if VERBOSE
	printf( "%s\n", line );
#endif

	ok = sscanf( line, "%d %d %d %f %f %d"
	    , from, too, comm, cost, cap, mut );
	if ( 6 == ok ) break;

    }

    return( 1 );

} /* getArcs */

int getMutual( fp, num, cap )
FILE	*fp;
int	*num;
float	*cap;
{
    char line[MAXLINE];
    char *res;
    int ok;

    while (1) {

	res = fgets( line, MAXLINE, fp );
	if ( NULL == res ) return( 0 );

	/* Set 15th space equal to end of line. */
	line[15] = '\0';

#if VERBOSE
	printf( "%s\n", line );
#endif

	ok = sscanf( line, "%d %f", num, cap );
	if ( 2 == ok ) break;

    }

    return( 1 );

} /* getMutual */

