      FUNCTION GCD(LATD1,LATM1,NS1,LNGD1,LNGM1,EW1,                      GCD    
     *             LATD2,LATM2,NS2,LNGD2,LNGM2,EW2)                      GCD    
C                                                                        GCD    
C ********************************************************************** GCD    
C *                                                                    * GCD    
C *********************  D E S C R I P T I O N  ************************ GCD    
C *                                                                    * GCD    
C ********************************************************************** GCD    
C                                                                        GCD    
C     FUNCTION GCD COMPUTES THE GREAT CIRCLE DISTANCE BETWEEN TWO        GCD    
C     POINTS, GIVEN THEIR LATITUDES AND LONGITUDES.                      GCD    
C                                                                        GCD    
C ********************************************************************** GCD    
C *                                                                    * GCD    
C ***************************  S T A T U S  **************************** GCD    
C *                                                                    * GCD    
C ********************************************************************** GCD    
C                                                                        GCD    
C      ORIGINAL AUTHOR        : MCLAIN                                   GCD    
C      ORIGINAL VERSION DATE  : 12/28/83                                 GCD    
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           GCD    
C                                2/15/85  FORMAL PARAMETERS MOVED TO     GCD    
C                                         COMMON BLOCKS.                 GCD    
C                                                                        GCD    
C ********************************************************************** GCD    
C *                                                                    * GCD    
C ********************  D E C L A R A T I O N S  *********************** GCD    
C *                                                                    * GCD    
C ********************************************************************** GCD    
C                                                                        GCD    
C  ----------------------------------------                              GCD    
C  ---I N T R I N S I C   R O U T I N E S .                              GCD    
C  ----------------------------------------                              GCD    
C                                                                        GCD    
      INTRINSIC         ACOS  ,  SIN   ,  COS   ,  FLOAT                 GCD    
C                                                                        GCD    
C  ------------------------                                              GCD    
C  ---P A R A M E T E R S .                                              GCD    
C  ------------------------                                              GCD    
C                                                                        GCD    
      PARAMETER        (MHOS  =  94  ,  MCAT  =  11  ,  MDOS  =  110 ,   MODMCN 
     *                  MAPOD =   2  ,  MDAYS =  90  ,  MIAPS =   13,    PARAMS 
     *                  MC9BAS=  72  ,  MASF  =  10  ,  NTYPEN=17,       PARAMS 
     *                  NTYPEA=  29  ,  NTYPEM=  12  ,  MAXAC =    2,    MODMCN 
     *                  MBASES= 272  ,  MFILES=  13  ,  MTOT=MDOS+MASF,  PARAMS 
     *                  NSIZE = NTYPEA*4 ,  NHOSDAY = MHOS*MDAYS,        PARAMS 
     *                  GNDLMT=  75.0,  AVGGS =  45.0,  MEND=MCAT+24,    PARAMS 
     *                  MBASARY = MBASES*20 ,  MFMTA = NTYPEA*27,        PARAMS 
     *                  MFMTN   = NTYPEN*27 ,  MFMTM = NTYPEM*27,        PARAMS 
     *                  MC9MSN  = MAPOD + MDOS + MASF)                   PARAMS 
C  ----------------------------------                                    GCD    
C  ---L O C A L   V A R I A B L E S .                                    GCD    
C  ----------------------------------                                    GCD    
C                                                                        GCD    
      DOUBLE PRECISION XLAT1 ,  XLAT2 ,  XLNG1 ,  XLNG2 ,  PI ,  XARG    GCD    
C                                                                        GCD    
C  ------------------------------------                                  GCD    
C  ---G L O B A L   V A R I A B L E S .                                  GCD    
C  ------------------------------------                                  GCD    
      INTEGER           LATD1 ,  LATM1 ,  LNGD1 ,  LNGM1 ,               GCD    
     *                  LATD2 ,  LATM2 ,  LNGD2 ,  LNGM2                 GCD    
C                                                                        GCD    
      REAL              GCD                                              GCD    
C                                                                        GCD    
      CHARACTER*1       NS1   ,  NS2   ,  EW1   ,  EW2                   GCD    
C                                                                        GCD    
C  ----------------------------------                                    GCD    
C  ---D A T A   S T A T E M E N T S .                                    GCD    
C  ----------------------------------                                    GCD    
C                                                                        GCD    
      DATA PI/3.141592654/                                               GCD    
C                                                                        GCD    
C ********************************************************************** GCD    
C *                                                                    * GCD    
C ************************  E N T R A N C E  *************************** GCD    
C *                                                                    * GCD    
C ********************************************************************** GCD    
C                                                                        GCD    
C ********************************************************************** GCD    
C ***                                                                *** GCD    
C *** STEP 1.                                                        *** GCD    
C ***        CONVERT INTEGER DEGREES AND MINUTES TO REAL RADIANS.    *** GCD    
C ***        CONVERT EAST AND SOUTH COORDINATES TO NEGATIVE VALUES.  *** GCD    
C ***                                                                *** GCD    
C ********************************************************************** GCD    
C                                                                        GCD    
      XLAT1  = (FLOAT(LATD1)+(FLOAT(LATM1)/60.))*(2.*(PI/360.))          GCD    
      XLAT2  = (FLOAT(LATD2)+(FLOAT(LATM2)/60.))*(2.*(PI/360.))          GCD    
      XLNG1  = (FLOAT(LNGD1)+(FLOAT(LNGM1)/60.))*(2.*(PI/360.))          GCD    
      XLNG2  = (FLOAT(LNGD2)+(FLOAT(LNGM2)/60.))*(2.*(PI/360.))          GCD    
      IF(EW1.EQ.'E')XLNG1=-1.0*XLNG1                                     GCD    
      IF(EW2.EQ.'E')XLNG2=-1.0*XLNG2                                     GCD    
      IF(NS1.EQ.'S')XLAT1=-1.0*XLAT1                                     GCD    
      IF(NS2.EQ.'S')XLAT2=-1.0*XLAT2                                     GCD    
C                                                                        GCD    
C ********************************************************************** GCD    
C ***                                                                *** GCD    
C *** STEP 2.                                                        *** GCD    
C ***        COMPUTE GREAT CIRCLE DISTANCE.                          *** GCD    
C ***                                                                *** GCD    
C ********************************************************************** GCD    
C                                                                        GCD    
      XARG = DSIN(XLAT1) * DSIN(XLAT2) +                                 GCD    
     *       DCOS(XLAT1) * DCOS(XLAT2) * DCOS(XLNG1-XLNG2)               GCD    
      IF (DABS(XARG) .LE. 1.0) THEN                                      GCD    
            GCD = 60. * (180./PI) * DACOS(XARG)                          GCD    
         ELSE                                                            GCD    
            GCD = -999.99                                                GCD    
         ENDIF                                                           GCD    
C                                                                        GCD    
C ********************************************************************** GCD    
C *                                                                    * GCD    
C ****************************  E X I T  ******************************* GCD    
C *                                                                    * GCD    
C ********************************************************************** GCD    
C                                                                        GCD    
      RETURN                                                             GCD    
      END                                                                GCD    
