/* Gary Schultz
 *
 * setDay.c
 *
 * Given a number of days on the command line, write the corresponding
 * <patdatx> file.
 *
 * Any error should cause a default # of days.
 */

#define DEFAULT 1
#define MAX 100

#define PAT_FILE "patdatx"

#include <stdio.h>

main( argc, argv )
int argc;
char *argv[];
{
    int days;
    FILE *fp, *fopen();
    char command[64];

    if ( 2 != argc ) {
	sprintf( command, "%s %s", "more", PAT_FILE );
	system( command );
    }

    if ( 1 != sscanf( argv[1], "%d", &days ) ) days = DEFAULT;

    if ( MAX < days ) days = DEFAULT;

    fp = fopen( PAT_FILE, "w" );

    fprintf( fp, "%5d\n", days );

    fprintf( fp, "%s\n",
	"1629245036401227335438                                                                                                              "
    );
    fprintf( fp, "%s\n",
	" 0 0 0 1 1 0 1 0 1 1 1                                                                                                              "
    );
    fprintf( fp, "%s\n",
	"0.1170.0320.1710.3680.1240.0380.0130.0190.0300.0810.007                                                                             "
    );
    fprintf( fp, "%s\n",
	"0.1040.0420.1720.3680.1250.0390.0130.0190.0300.0810.007                                                                             "
    );
    fprintf( fp, "%s\n",
	"0.0790.0280.1790.3860.1300.0410.0130.0200.0320.0850.007                                                                             "
    );
    fprintf( fp, "%s\n",
	"030000030000030000030000030000030000030000030000030000                                                                              "
    );
    fprintf( fp, "%s\n",
	"RUN 1                                                                                                                               "
    );

} /* main */


