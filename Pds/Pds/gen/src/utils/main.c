/* Gary Schultz
 *
 * main.c
 *
 * This is the driver for the utilities I wrote.
 * It opens the files and allows you to use any of the routines.
 */

#define MAXLEN 64

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

/* Open the arc file. */
#define ARC_OP \
	aFil = fopen( aFilName, "r" ); \
	if ( NULL == aFil ) { \
	    if ( verbose ) \
		printf( "Error: could not open <%s>.\n", aFilName ); \
		return; \
	}
/* Close the arc file. */
#define ARC_CL \
	    fclose( aFil );

/* Open the mutual file. */
#define MUT_OP \
	mFil = fopen( mFilName, "r" ); \
	if ( NULL == mFil ) { \
	    if ( verbose ) \
		printf( "Error: could not open <%s>.\n", mFilName ); \
		return; \
	}
/* Close the mutual file. */
#define MUT_CL \
	    fclose( mFil );

/* Open the summary file. */
#define SUM_OP \
	sFil = fopen( sFilName, "r" ); \
	if ( NULL == sFil ) { \
	    if ( verbose ) \
		printf( "Error: could not open <%s>.\n", sFilName ); \
		return; \
	}
/* Close the summary file. */
#define SUM_CL \
	    fclose( sFil );

/* Open the output file. */
#define OUT_OP \
	out = fopen( outName, "w" ); \
	if ( NULL == out ) { \
	    if ( verbose ) printf( "Error: could not open <%s>.\n", outName ); \
	    return; \
	}
/* Close the output file. */
#define OUT_CL \
	    fclose( out );

#include <stdio.h>

main( argc, argv )
int argc;
char *argv[];
{
    /* File variables. */
    char nFilName[MAXLEN], aFilName[MAXLEN], mFilName[MAXLEN], outName[MAXLEN];
    char sFilName[MAXLEN];
    char sumaryCall[MAXLEN];
    FILE *fopen(), *nFil, *aFil, *mFil, *sFil, *out;

    /* Other variables. */
    int numMutCon = -1; /* Number of mutual constraints.  Gets changed
	* after a call to the mutual constraint checker.  After that the
	* -c flag will also check the mutual capacity constraints themselves.
	*/
    int verbose = (1==1); /* Print to stdout only if verbose. */
    int i;

    if ( 1 == argc ) { /* Print the options. */
	printf( "This is a utility to be used on the ``pfgen'' output.\n" );
	printf( "The program processes its command line left to right.\n" );
	printf( "The following commands are valid...\n" );
	printf( " %-17s%-55s\n"
	    , "-a <arcfile>"
	    , "Changes arc file to <arcfile>.  Default is ``arcs''."
	    );
	printf( " %-17s%-55s\n"
	    , "-C"
	    , "Examine only arc capacities and costs."
	    );
	printf( " %-17s%-55s\n"
	    , "-c"
	    , "All -C stuff; view joint constraints if proceeded by -j."
	    );
	printf( " %-17s%-55s\n"
	    , "-d"
	    , "Examine demand ranges (node data)."
	    );
	printf( " %-17s%-55s\n"
	    , "-G"
	    , "Put data in Gary's format."
	    );
	printf( " %-17s%-55s\n"
	    , "-g"
	    , "Go through Gary's format but print out only some statistics."
	    );
	printf( " %-17s%-55s\n"
	    , "-j"
	    , "Examine joint capacity constraints."
	    );
	printf( " %-17s%-55s\n"
	    , "-m <mfile>"
	    , "Changes mutual file to <mfile>.  Default is ``mutual''."
	    );
	printf( " %-17s%-55s\n"
	    , "-n <nfile>"
	    , "Changes nodes file to <nfile>.  Default is ``nodes''."
	    );
	printf( " %-17s%-55s\n"
	    , "-o <outfile>"
	    , "Changes output file to <outfile>.  Default is ``output''."
	    );
	printf( " %-17s%-55s\n"
	    , "-s <n>"
	    , "Extracts the single commodity <n> from the problem."
	    );
	printf( " %-17s%-55s\n"
	    , "-V"
	    , "Verbose on; print messages to standard output (default -V)."
	    );
	printf( " %-17s%-55s\n"
	    , "-v"
	    , "Verbose off; print nothing to standard output (default -V)."
	    );
	printf( " %-17s%-55s\n"
	    , "-y <summaryfile>"
	    , "Changes summary file to <summaryfile>.  Default is ``sumary''."
	    );
	printf( " %-17s%-55s\n"
	    , "-z"
	    , "Prints problem sizes to standard output from summary file."
	    );
	return;
    }

    /* Set default file names. */
    sprintf( nFilName, "nodes" );
    sprintf( aFilName, "arcs" );
    sprintf( mFilName, "mutual" );
    sprintf( sFilName, "sumary" );
    sprintf( outName, "output" );
    sprintf( sumaryCall, "tail -4 %s", "sumary" );

    OUT_OP

    /* Process flags. */
    while ( --argc > 0 ) {
	++argv; /* Next input string. */

	if ( '-' == argv[0][0] ) { /* Remember the flag to be set. */

	    switch ( argv[0][1] ) { /* One character flags. */

	    case 'a':
		if ( ( 1 < argc ) && ( '-' != argv[1][0] ) ) {
		    --argc;
		    ++argv;
		    sprintf( aFilName, argv[0] );
		}
		else {
		    if (verbose) printf( "No argument given to -a option.\n" );
		}
		if ( verbose ) printf( "Arc file is %s.\n", aFilName );
		break;

	    case 'C':
		if ( verbose )
		    printf( "Examine only cost and capacity ranges...\n" );
		ARC_OP
		/* Second parameter will never be used. */
		arcCheck( aFil, aFil, out, verbose, -1 );
		ARC_CL
		break;

	    case 'c':
		if ( verbose ) printf( "Examine arc data...\n" );
		ARC_OP
		MUT_OP
		arcCheck( aFil, mFil, out, verbose, numMutCon );
		MUT_CL
		ARC_CL
		break;

	    case 'd':
		if ( verbose ) printf( "Examine demand ranges...\n" );
		NOD_OP
		nodeCheck( nFil, out, verbose );
		NOD_CL
		break;

	    case 'g':
	    case 'G':
		if ( verbose ) printf( "Put in Gary's format...\n" );
		ARC_OP
		cordForm( argv[0][1], aFil, out, verbose );
		ARC_CL
		break;

	    case 'j':
		if ( verbose ) printf( "Examine joint arc capacities...\n" );
		MUT_OP
		numMutCon = mutlCheck( mFil, out, verbose );
		MUT_CL
		break;

	    case 'm':
		if ( ( 1 < argc ) && ( '-' != argv[1][0] ) ) {
		    --argc;
		    ++argv;
		    sprintf( mFilName, argv[0] );
		}
		else {
		    if (verbose) printf( "No argument given to -m option.\n" );
		}
		if ( verbose ) printf( "Mutual file is %s.\n", mFilName );
		break;

	    case 'n':
		if ( ( 1 < argc ) && ( '-' != argv[1][0] ) ) {
		    --argc;
		    ++argv;
		    sprintf( nFilName, argv[0] );
		}
		else {
		    if (verbose) printf( "No argument given to -n option.\n" );
		}
		if ( verbose ) printf( "Node file is %s.\n", nFilName );
		break;

	    case 'o':
		if ( ( 1 < argc ) && ( '-' != argv[1][0] ) ) {
		    --argc;
		    ++argv;
		    sprintf( outName, argv[0] );
		    OUT_CL
		    OUT_OP
		}
		else {
		    if (verbose) printf( "No argument given to -o option.\n" );
		}
		if ( verbose ) printf( "Output file is %s.\n", outName );
		break;

	    case 's':
		if ( verbose ) printf( "Extract single commodity...\n" );
		if ( ( 1 < argc ) && ( '-' != argv[1][0] ) ) {
		    --argc;
		    ++argv;
		    if ( 1 == sscanf( argv[0], "%d", &i ) ) {
			NOD_OP
			ARC_OP
			SUM_OP
			singleCom( nFil, aFil, sFil, out, i, verbose );
			NOD_CL
			ARC_CL
			SUM_CL
		    }
		    else {
			if ( verbose )
			    printf( "Aborted: string <%s> %s.\n"
			    	, argv[0], "should be commodity number" );
		    }
		}
		else {
		}
		break;

	    case 'V':
		verbose = (1==1);
		break;

	    case 'v':
		verbose = (0==1);
		break;

	    case 'y':
		if ( ( 1 < argc ) && ( '-' != argv[1][0] ) ) {
		    --argc;
		    ++argv;
    		    sprintf( sumaryCall, "tail -4 %s", argv[0] );
		}
		else {
		    if (verbose) printf( "No argument given to -y option.\n" );
		}
		if ( verbose ) printf( "Summary file is %s.\n", argv[0] );
		break;

	    case 'z':
		system( sumaryCall );
		break;

	    default:
		if ( verbose ) printf( "Unknown option -%c.\n", argv[0][1] );

	    } /* switch */

	}
	else { /* This is the flag value. */
	    if ( verbose ) printf( "Unknown option %s.\n", argv[0] );
	}
    }

} /* main */

