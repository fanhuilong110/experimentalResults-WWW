      SUBROUTINE BLDLST(IDAY)                                            BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C *                                                                    * BLDLST 
C *********************  D E S C R I P T I O N  ************************ BLDLST 
C *                                                                    * BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
C     SUBROUTINE BLDLST USES THE ITINERARY FILE TO CREATE A              BLDLST 
C     LIST OF MISSIONS ON A GIVEN DAY (IF THERE ARE ANY).                BLDLST 
C     THE LIST IS ORDERED ON INTERTHEATER ARRIVAL TIME.                  BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C *                                                                    * BLDLST 
C ***************************  S T A T U S  **************************** BLDLST 
C *                                                                    * BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
C      ORIGINAL AUTHOR        : MCLAIN                                   BLDLST 
C      ORIGINAL VERSION DATE  : 12/28/83                                 BLDLST 
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           BLDLST 
C                                2/15/85  FORMAL PARAMETERS MOVED TO     BLDLST 
C                                         COMMON BLOCKS.                 BLDLST 
C                                2/26/85  CODE FROM DISTRIB PROGRAM      BLDLST 
C                                         ADDED TO BUILD TIME-ORDERED    BLDLST 
C                                         MISSION LIST                   BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C *                                                                    * BLDLST 
C ********************  D E C L A R A T I O N S  *********************** BLDLST 
C *                                                                    * BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
C  ------------------------                                              BLDLST 
C  ---P A R A M E T E R S .                                              BLDLST 
C  ------------------------                                              BLDLST 
C                                                                        BLDLST 
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
C                                                                        BLDLST 
C  ----------------------------------                                    BLDLST 
C  ---L O C A L   V A R I A B L E S .                                    BLDLST 
C  ----------------------------------                                    BLDLST 
C                                                                        BLDLST 
      INTEGER           I     ,  IWORK ,  IFORMAT, IERROR,  IAPOD ,      BLDLST 
     *                  IARRVT,  IDOS  ,  KFORMAT                        BLDLST 
C                                                                        BLDLST 
      DIMENSION         IWORK(20)                                        BLDLST 
C                                                                        BLDLST 
      CHARACTER*6       ACTYPE                                           BLDLST 
C                                                                        BLDLST 
C                                                                        BLDLST 
C  ------------------------------------                                  BLDLST 
C  ---G L O B A L   V A R I A B L E S .                                  BLDLST 
C  ------------------------------------                                  BLDLST 
C                                                                        BLDLST 
      INTEGER           IUNITS,  NODES ,  NBRNOD,  NODFMT,  IARCS ,      GLBINT 
     *                  NBREAT,  NBRBAT,  NODEFR,  NODETO,  NBRARC,      GLBINT 
     *                  IACFMT,  MCCDPF,  NBRMCC,  MCCFMT,  MCCC9 ,      GLBINT 
     *                  ICASLT,  IDMAND,  LOS   ,  LITAMB,  ITCBP ,      GLBINT 
     *                  IASN  ,  IDSN  ,  ICSN  ,  IHSPDTL, IPRHSP,      GLBINT 
     *                  IC9BAS,  IAPDTL,  LNKIAP,  IAPSMX,  IAPTMX,      GLBINT 
     *                  IAPPNN,  IAPPDY,  IPODDTL, LNKPOD,  IPODIND,     GLBINT 
     *                  IPODASF, IPODC9B, IPODDOS, IC9DTL,  IC9IND,      GLBINT 
     *                  LNKC9B,  IC9POD,  IC9DOS,  IDOSC9B, IDOSPOD,     GLBINT 
     *                  IC9ASF,  IDOSDTL, LNKDOS,  IDOSIND, IDOSASF,     GLBINT 
     *                  IASFDTL, LNKASF,  IASFMX,  IASFPOD, IASFDOS,     GLBINT 
     *                  IASFC9B, IASFPNN, IASFPDY, IBASES,  INDIAP,      GLBINT 
     *                  INDPOD,  INDASF,  INDC9B,  INDDOS,  LATDEG,      GLBINT 
     *                  LATMIN,  LNGDEG,  LNGMIN,                        GLBINT 
     *                  MAXSTG,  MAXTRX,  MAXASF,  NDXIAP,               GLBINT 
     *                  NDXPOD,  NDXASF,  NDXC9B,  NDXDOS,  IACRAFT,     GLBINT 
     *                  IBDAVL,  IBDRES,                                 GLBINT 
     *                  IFWD  ,  IBCK  ,  ISUC  ,  IPRD  ,  NBRMSN,      GLBINT 
     *                  HEADG ,  IQUEUE,  ICOUNT,  IDETAIL, NOMORE,      GLBINT 
     *                  MSNNBR,  MAXC9 ,  MAXDAYS                        MDN1208
      INTEGER           IDMAT,   IASFPTM,                                GLBINT 
     *                  MSNITA,  MSNAC ,  MSNITD,  MSNDDY,               GLBINT 
     *                  MSNDTM,  MSNADY,  MSNATM,  MSNFT1,               GLBINT 
     *                  MSNSDY,  MSNSTM,  MSNDOS,  MSNODY,               GLBINT 
     *                  MSNOTM,  MSNTYP,  MSNGTM,  MSNFT2,               GLBINT 
     *                  MSNSEQ,  MSNTOT,  MSNIAP,  MSNPOD,               GLBINT 
     *                  LMTLIT,  LMTAMB,  LMTTOT,  MSNONL,               GLBINT 
     *                  MAXFTT,  MAXGTT,  ILEG1 ,  ILEG2 ,               GLBINT 
     *                  NDOSDAY, NDOSCUM, NOLCL , NOC9B                  GLBINT 
C                                                                        BLDLST 
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        BLDLST 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        BLDLST 
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        BLDLST 
      DIMENSION         FYLNAM (         MFILES),  IAPDTL (MIAPS ,  5),  GLBDIM 
     *                  IUNITS (         MFILES),  IPODDTL(MAPOD  , 6),  GLBDIM 
     *                  HSPCAP (MDAYS,MCAT,MHOS),  IASFDTL(MASF  ,  9),  GLBDIM 
     *                  HSPTBD (MDAYS,     MHOS),  IDOSDTL(MDOS  ,  6),  GLBDIM 
     *                  ICASLT (MDAYS,MCAT     ),  IC9DTL (MC9BAS,  5),  GLBDIM 
     *                  IDMAND (      MCAT     ),  IHSPDTL(MHOS  ,  3),  GLBDIM 
     *                  LOS    (      MCAT     ),  NODES  (NTYPEN    ),  GLBDIM 
     *                  LITAMB (      MCAT     ),  NODFMT (NTYPEN, 27),  GLBDIM 
     *                  FACTOR (    3,MCAT     ),  NODTXT (NTYPEN    ),  GLBDIM 
     *                  ITCBP  (              9),  IARCS  (NTYPEA,  4),  GLBDIM 
     *                  IBDAVL (    2,MCAT,MHOS),  IACFMT (NTYPEA, 27),  GLBDIM 
     *                  IBDRES (      MCAT,MHOS),  ARCTXT (NTYPEA    ),  GLBDIM 
     *                  IDMAT  (  2,MDOS,MC9BAS),  ARCCIJ (NTYPEA    ),  GLBDIM 
     *                  AIRCRFT(          MAXAC),  ARCUIJ(NTYPEA,MCAT),  GLBDIM 
     *                  IACRAFT(MAXAC,       10),  RITE   (  3,NTYPEA),  GLBDIM 
     *                  IBASES (MBASES,      20),  MCCDPF (MCAT      ),  GLBDIM 
     *                  LINES  (              7),  MCCTXT (NTYPEM    ),  GLBDIM 
     *                  ICATS  (      MCAT     ),  MCCFMT (NTYPEM, 27),  GLBDIM 
     *                  NSEW   (MBASES,       2),  HOSPTL (MHOS      ),  GLBDIM 
     *                  IDETAIL(   300,    MEND),  MAXC9  (MDAYS     )   GLBDIM 
      DIMENSION         IQUEUE (   300,       3),  IC9MSN (MC9MSN,  5),  GLBDIM 
     *                  ILEG1  (3,MIAPS , MAPOD),  PODXIST(MAPOD     ),  GLBDIM 
     *                  ILEG2  (3, MDOS , MAPOD),  ASFXIST(MASF      ),  GLBDIM 
     *                  DOSXIST(MDOS           ),                        GLBDIM 
     *                  NDOSDAY(MDOS           ),  NDOSCUM(MDOS      ),  GLBDIM 
     *                  NOLCL  (MDOS           ),  NOC9B  (MDOS      )   GLBDIM 
C                                                                        BLDLST 
C  ------------------------------                                        BLDLST 
C  ---C O M M O N   B L O C K S .                                        BLDLST 
C  ------------------------------                                        BLDLST 
C                                                                        BLDLST 
      COMMON /CHRCTR/ ICATS ,  HOSPTL,  AIRCRFT, RUNLBL,  FYLNAM,        GLBCOM 
     *                LINES ,  ARCTXT,  NODTXT,  MCCTXT,  NSEW           GLBCOM 
      COMMON /FILES / IUNITS                                             GLBCOM 
      COMMON /NDAYS / MAXDAYS                                            MDN1208
      COMMON /NODE  / NODES ,  NBRNOD,  NODFMT                           GLBCOM 
      COMMON /ARCS  / IARCS ,  NBREAT,  NBRBAT,  NODEFR,  NODETO,        GLBCOM 
     *                NBRARC,  IACFMT,  ARCCIJ,  ARCUIJ                  GLBCOM 
      COMMON /MUTUAL/ MCCDPF,  NBRMCC,  MCCFMT,  MCCC9                   GLBCOM 
      COMMON /CASLTY/ ICASLT,  IDMAND,  LOS   ,  LITAMB,  FACTOR,  ITCBP GLBCOM 
      COMMON /SEQNBR/ IASN  ,  IDSN  ,  ICSN                             GLBCOM 
      COMMON /HSPDAT/ IHSPDTL, IPRHSP,  IC9BAS,  HSPCAP,  HSPTBD,        GLBCOM 
     *                NOLCL  , NOC9B                                     GLBCOM 
      COMMON /IAPDAT/ IAPDTL,  LNKIAP,  IAPSMX,  IAPTMX,  IAPPNN, IAPPDY GLBCOM 
      COMMON /PODDAT/ IPODDTL, LNKPOD,  IPODIND, IPODASF, IPODC9B,       GLBCOM 
     *                IPODDOS, IPODCHK, PODXIST                          GLBCOM 
      COMMON /C9BDAT/ IC9DTL,  LNKC9B,  IC9IND,  IC9POD,  IC9DOS,        GLBCOM 
     *                IC9ASF,  IDMAT ,  IC9MSN,  IC9PTR,  LNKC9M,        GLBCOM 
     *                IC9ONT,  IC9NDX,  IC9AMT,  IC9NOD                  GLBCOM 
      COMMON /DOSDAT/ IDOSDTL, LNKDOS,  IDOSIND, IDOSASF, IDOSC9B,       GLBCOM 
     *                IDOSPOD, IDOSCHK, DOSXIST, NDOSDAY, NDOSCUM        GLBCOM 
      COMMON /ASFDAT/ IASFDTL, LNKASF,  IASFMX,  IASFPOD, IASFDOS,       GLBCOM 
     *                IASFC9B, IASFPNN, IASFPDY, IASFPTM,IASFCHK,ASFXIST GLBCOM 
      COMMON /BASES / IBASES,  INDIAP,  INDPOD,  INDASF, INDC9B, INDDOS, GLBCOM 
     *                LATDEG,  LATMIN,  LNGDEG,  LNGMIN, MAXSTG, MAXTRX, GLBCOM 
     *                MAXASF,  NDXIAP,  NDXPOD,  NDXASF, NDXC9B, NDXDOS  GLBCOM 
      COMMON /PLANES/ IACRAFT, ITAS  ,  IALTIM,  ITAXI , MAXRNG, IPODGT, GLBCOM 
     *                IDOSGT,  IC9GT ,  MAXLIT,  MAXAMB,  MAXTOT         GLBCOM 
      COMMON /BEDUSE/ IBDAVL,  IBDRES                                    GLBCOM 
      COMMON /SWITCH/ RITE                                               GLBCOM 
      COMMON /FXDPTR/ IVAL ,  IFWD  ,  IBCK                              GLBCOM 
      COMMON /LNKLST/ ISUC  ,  IPRD  ,  NBRMSN,  HEADG ,  IQUEUE         GLBCOM 
      COMMON /MSNDTL/ ICOUNT,  IDETAIL, NOMORE,  MSNNBR,                 GLBCOM 
     *                MSNITA,  MSNAC ,  MSNITD,  MSNDDY,                 GLBCOM 
     *                MSNDTM,  MSNADY,  MSNATM,  MSNFT1,                 GLBCOM 
     *                MSNSDY,  MSNSTM,  MSNDOS,  MSNODY,                 GLBCOM 
     *                MSNOTM,  MSNTYP,  MSNGTM,  MSNFT2,                 GLBCOM 
     *                MSNSEQ,  MSNTOT,  MSNIAP,  MSNPOD,                 GLBCOM 
     *                LMTLIT,  LMTAMB,  LMTTOT,  MSNONL , ILEG1, ILEG2   GLBCOM 
      COMMON /C9CAP / MAXC9 ,  MAXFTT,  MAXGTT                           GLBCOM 
C                                                                        BLDLST 
C  --------------------------------------                                BLDLST 
C  ---F O R M A T   S T A T E M E N T S .                                BLDLST 
C  --------------------------------------                                BLDLST 
C                                                                        BLDLST 
  900 FORMAT(I5,1X,A6,6X,1X,I4,1X,I2,1X,I6,1X,I3,1X,I2,1X,I6,1X,         BLDLST 
     *       I2,1X,I6,1X,I4,1X,I2,1X,I6,1X,I1)                           BLDLST 
 1900 FORMAT(1X, 'IDOS=0,TYPE1,RCRD NUM',I3)                             BLDLST 
 9110 FORMAT(1X, 'ERROR NUMBER=',I6,' READ FORMAT=',I4,' IOSTAT=',I4,    BLDLST 
     *       ' IN BLDLST')                                               BLDLST 
 9120 FORMAT(1X, 'ERROR NUMBER=',I6,' AIRCRAFT TYPE = ',A6,              BLDLST 
     *       ' MISSION NUMBER',I6)                                       BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C *                                                                    * BLDLST 
C ************************  E N T R A N C E  *************************** BLDLST 
C *                                                                    * BLDLST 
C ********************************************************************** BLDLST 
C ***                                                                *** BLDLST 
C *** STEP 2.                   ***DO-UNTIL***                       *** BLDLST 
C ***                                                                *** BLDLST 
C ***         READ THE ITINERARY FILE UNTIL NEW MISSION DATE FOUND.  *** BLDLST 
C ***         IF AN ERROR OCCURS BRANCH TO ABNORMAL TERMINATION.     *** BLDLST 
C ***         IF AN END-OF-FILE OCCURS BRANCH AND RESET NOMORE TO 1. *** BLDLST 
C ***                                                                *** BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
      ASSIGN 900 TO IFORMAT                                              BLDLST 
      KFORMAT = 900                                                      BLDLST 
      ASSIGN 9110 TO IERROR                                              BLDLST 
      IERRNBR = 9912                                                     BLDLST 
C                                                                        BLDLST 
  200 READ(7,IFORMAT,END=300,ERR=9000,IOSTAT=IOS)IWORK(1),ACTYPE,        BLDLST 
     *                                          (IWORK(J),J=2,13)        BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C ***                                                                *** BLDLST 
C *** STEP 2B.                                                       *** BLDLST 
C ***          IF THE DAY IN THE NEW RECORD IS NOT IDAY, THEN        *** BLDLST 
C ***          EXIT THE ROUTINE.                                     *** BLDLST 
C ***                                                                *** BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
      NEWDAY = IWORK(MSNSDY)                                             BLDLST 
      IF(.NOT.(NEWDAY.EQ.IDAY))                                          BLDLST 
     *   THEN                                                            BLDLST 
             BACKSPACE 7                                                 BLDLST 
             GO TO 9999                                                  BLDLST 
      ENDIF                                                              BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C ***                                                                *** BLDLST 
C *** STEP 2D.                                                       *** BLDLST 
C ***          SAVE THE MISSION INFORMATION.                         *** BLDLST 
C ***          MISSION VARIABLES ARE DEFINED AS FOLLOWS:             *** BLDLST 
C ***                                                                *** BLDLST 
C ***               MSNAC  = MISSION AIRCRAFT TYPE FROM CHAR "ACTYPE"*** BLDLST 
C ***               MSNITD = INTERTHEATER DEPARTURE STATION          *** BLDLST 
C ***               MSNDDY = INTERTHEATER DEPARTURE DATE             *** BLDLST 
C ***               MSNDTM = INTERTHEATER DEPARTURE TIME             *** BLDLST 
C ***               MSNITA = INTERTHEATER ARRIVAL STATION            *** BLDLST 
C ***               MSNADY = INTERTHEATER ARRIVAL DATE                   BLDLST 
C ***               MSNATM = INTERTHEATER ARRIVAL TIME               *** BLDLST 
C ***               MSNFT1 = INTERTHEATER FLYING TIME                *** BLDLST 
C ***               MSNSDY = INTRACONUS DEPARTURE DATE (NEWDAY)          BLDLST 
C ***               MSNSTM = INTRACONUS DEPARTURE TIME               *** BLDLST 
C ***               MSNONL = DESTINATION ONLOAD STATION (DOS)        *** BLDLST 
C ***               MSNODY = DOS ARRIVAL DATE                        *** BLDLST 
C ***               MSNOTM = DOS ARRIVAL TIME                        *** BLDLST 
C ***               MSNTYP = MISSION TYPE                            *** BLDLST 
C ***               MSNAPOD= AERIAL PORT OF DEBARKATION INDEX        *** BLDLST 
C ***               MSNDOS = DESTINATION ONLOAD STATION INDEX        *** BLDLST 
C ***               MSNIAP = INTERTHEATER AIREVAC POINT INDEX        *** BLDLST 
C ***               MSNFT2 = INTRACONUS FLYING TIME                  *** BLDLST 
C ***               MSNGTM = INTERTHEATER ARRIVAL GROUND TIME        *** BLDLST 
C ***               MSNSEQ = DAILY MISSION SEQUENCE NUMBER           *** BLDLST 
C ***               MSNTOT = CUMULATIVE MISSION SEQUENCE NUMBER      *** BLDLST 
C ***               LMTLIT = LITTER CAPACITY                         *** BLDLST 
C ***               LMTAMB = AMBULATORY CAPACITY                     *** BLDLST 
C ***               LMTTOT = TOTAL PATIENT CAPACITY                  *** BLDLST 
C ***                                                                *** BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
      IAP      = 0                                                       BLDLST 
      IAPOD    = 0                                                       BLDLST 
      IDOS     = 0                                                       BLDLST 
      IF(IWORK(MSNITD).LT.MBASES.AND.IWORK(MSNITD).GT.0)                 BLDLST 
     *    THEN                                                           BLDLST 
              IAPPTR   = IWORK(MSNITD)                                   BLDLST 
              IAP      = IBASES(IAPPTR,NDXIAP)                           BLDLST 
      ENDIF                                                              BLDLST 
      IF(IWORK(MSNONL).LT.MBASES.AND.IWORK(MSNONL).GT.0)                 BLDLST 
     *    THEN                                                           BLDLST 
              IDOSPTR  = IWORK(MSNONL)                                   BLDLST 
              IDOS  = IBASES(IDOSPTR,NDXDOS)                             BLDLST 
      ENDIF                                                              BLDLST 
      IF(IWORK(MSNITA).LT.MBASES.AND.IWORK(MSNITA).GT.0)                 BLDLST 
     *    THEN                                                           BLDLST 
              IAPODPTR = IWORK(MSNITA)                                   BLDLST 
              IAPOD = IBASES(IAPODPTR,NDXPOD)                            BLDLST 
      ENDIF                                                              BLDLST 
      DO 210 IAC=1,MAXAC                                                 BLDLST 
             IF(AIRCRFT(IAC).EQ.ACTYPE)                                  BLDLST 
     *           THEN                                                    BLDLST 
                     GO TO 220                                           BLDLST 
             ENDIF                                                       BLDLST 
  210 CONTINUE                                                           BLDLST 
      ASSIGN 9120 TO IERROR                                              BLDLST 
      IERRNBR = 9910                                                     BLDLST 
      GO TO 9010                                                         BLDLST 
  220 CONTINUE                                                           BLDLST 
      IF(IWORK(MSNTYP).EQ.2)IWORK(MSNSTM)=IWORK(MSNATM)+                 BLDLST 
     *                      IACRAFT(IAC,IPODGT)                          BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C ***                                                                *** BLDLST 
C *** STEP 2E.                                                       *** BLDLST 
C ***         IF THE DOS EQUALS ZERO FOR A TYPE 1, 3, OR 4 MISSION,  *** BLDLST 
C ***         PRINT OUT AN ADVISORY AND SKIP THIS EVENT.             *** BLDLST 
C ***                                                                *** BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
      IF(IDOS.LE.0.AND.(IWORK(MSNTYP).EQ.1.OR.IWORK(MSNTYP).EQ.3.OR.     BLDLST 
     *  IWORK(MSNTYP).EQ.4))                                             BLDLST 
     *    THEN                                                           BLDLST 
              PRINT 1900,NBRMSN                                          BLDLST 
              GO TO 200                                                  BLDLST 
      ENDIF                                                              BLDLST 
C                                                                        BLDLST 
      IF (IDOS .NE. 0)                                                   BLDLST 
     *   THEN                                                            BLDLST 
            NDOSDAY(IDOS) = NDOSDAY(IDOS) + 1                            BLDLST 
            NDOSCUM(IDOS) = NDOSCUM(IDOS) + 1                            BLDLST 
      ENDIF                                                              BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C ***                                                                *** BLDLST 
C *** STEP 2F.                                                       *** BLDLST 
C ***         UPDATE THE CUMULATIVE EVENT COUNTER                    *** BLDLST 
C ***         INSERT THE MISSION IN THE LINKED LIST IN INTERTHEATER  *** BLDLST 
C ***         ARRIVAL TIME ORDER TO COORDINATE PATIENT DELIVERY      *** BLDLST 
C ***         STORE MISSION DETAIL IN IDETAIL                        *** BLDLST 
C ***                                                                *** BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
      NBRMSN=NBRMSN+1                                                    BLDLST 
      LINK = IWORK(MSNATM)                                               BLDLST 
      CALL INSERT(LINK)                                                  BLDLST 
      IDETAIL(ICOUNT, MSNITA) = IWORK(MSNITA)                            BLDLST 
      IDETAIL(ICOUNT, MSNITD) = IWORK(MSNITD)                            BLDLST 
      IDETAIL(ICOUNT, MSNDDY) = IWORK(MSNDDY)                            BLDLST 
      IDETAIL(ICOUNT, MSNDTM) = IWORK(MSNDTM)                            BLDLST 
      IDETAIL(ICOUNT, MSNFT1) = IWORK(MSNFT1)                            BLDLST 
      IDETAIL(ICOUNT, MSNADY) = IWORK(MSNADY)                            BLDLST 
      IDETAIL(ICOUNT, MSNATM) = IWORK(MSNATM)                            BLDLST 
      IDETAIL(ICOUNT, MSNSDY) = IWORK(MSNSDY)                            BLDLST 
      IDETAIL(ICOUNT, MSNSTM) = IWORK(MSNSTM)                            BLDLST 
      IDETAIL(ICOUNT, MSNONL) = IWORK(MSNONL)                            BLDLST 
      IDETAIL(ICOUNT, MSNODY) = IWORK(MSNODY)                            BLDLST 
      IDETAIL(ICOUNT, MSNOTM) = IWORK(MSNOTM)                            BLDLST 
      IDETAIL(ICOUNT, MSNTYP) = IWORK(MSNTYP)                            BLDLST 
      IDETAIL(ICOUNT, MSNSEQ) = MSNNBR                                   BLDLST 
      IDETAIL(ICOUNT, MSNTOT) = NBRMSN                                   BLDLST 
      IDETAIL(ICOUNT, MSNIAP) = IAP                                      BLDLST 
      IDETAIL(ICOUNT, MSNPOD) = IAPOD                                    BLDLST 
      IDETAIL(ICOUNT, MSNDOS) = IDOS                                     BLDLST 
      IDETAIL(ICOUNT, MSNFT2) = IWORK(MSNOTM) - IWORK(MSNSTM)            BLDLST 
      IDETAIL(ICOUNT, MSNGTM) = IACRAFT(IAC,IPODGT)                      BLDLST 
      IDETAIL(ICOUNT, LMTLIT) = IACRAFT(IAC,MAXLIT)                      BLDLST 
      IDETAIL(ICOUNT, LMTAMB) = IACRAFT(IAC,MAXAMB)                      BLDLST 
      IDETAIL(ICOUNT, LMTTOT) = IACRAFT(IAC,MAXTOT)                      BLDLST 
      IDETAIL(ICOUNT, MSNAC)  = IAC                                      BLDLST 
      GO TO 200                                                          BLDLST 
C     END DO-UNTIL                                                       BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C ***                                                                *** BLDLST 
C *** STEP 3.                                                        *** BLDLST 
C ***         IF END-OF-FILE ENCOUNTERED, MAKE NOMORE TRUE           *** BLDLST 
C ***                                                                *** BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
  300 NOMORE=1                                                           BLDLST 
      NEWDAY=MDAYS+1                                                     BLDLST 
      GO TO 9999                                                         BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C *                                                                    * BLDLST 
C ********************* A B N O R M A L   E X I T  ********************* BLDLST 
C *                                                                    * BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
 9000 CONTINUE                                                           BLDLST 
      WRITE(99,IERROR)IERRNBR,KFORMAT,IOS                                BLDLST 
      STOP 'I/O ERROR'                                                   BLDLST 
 9010 CONTINUE                                                           BLDLST 
      WRITE(99,IERROR) IERRNBR, ACTYPE, NBRMSN                           BLDLST 
      STOP 'AIRCRAFT TYPE ERROR'                                         BLDLST 
C                                                                        BLDLST 
C ********************************************************************** BLDLST 
C *                                                                    * BLDLST 
C **********************  N O R M A L   E X I T  *********************** BLDLST 
C *                                                                    * BLDLST 
C ********************************************************************** BLDLST 
C                                                                        BLDLST 
 9999 RETURN                                                             BLDLST 
      END                                                                BLDLST 
