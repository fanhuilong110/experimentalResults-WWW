      SUBROUTINE SORTC9                                                  SORTC9 
C                                                                        SORTC9 
C ********************************************************************** SORTC9 
C *                                                                    * SORTC9 
C *********************  D E S C R I P T I O N  ************************ SORTC9 
C *                                                                    * SORTC9 
C ********************************************************************** SORTC9 
C                                                                        SORTC9 
C     SUBROUTINE SORTC9 CREATES THE TRAVEL TIME MATRIX.  EACH ROW        SORTC9 
C     REPRESENTS A DOS AND EACH COLUMN A C9 BASE.  FOR DISTANCES UP TO   SORTC9 
C     A SPECIFIED MAXIMUM, SUBROUTINE GNDTYM                             SORTC9 
C                                                                        SORTC9 
C ********************************************************************** SORTC9 
C *                                                                    * SORTC9 
C ***************************  S T A T U S  **************************** SORTC9 
C *                                                                    * SORTC9 
C ********************************************************************** SORTC9 
C                                                                        SORTC9 
C      ORIGINAL AUTHOR        : MCLAIN                                   SORTC9 
C      ORIGINAL VERSION DATE  : 12/28/83                                 SORTC9 
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           SORTC9 
C                                2/15/85  FORMAL PARAMETERS MOVED TO     SORTC9 
C                                         COMMON BLOCKS.                 SORTC9 
C                                2/27/85                                 SORTC9 
C                                                                        SORTC9 
C ********************************************************************** SORTC9 
C *                                                                    * SORTC9 
C ********************  D E C L A R A T I O N S  *********************** SORTC9 
C *                                                                    * SORTC9 
C ********************************************************************** SORTC9 
C                                                                        SORTC9 
C  ------------------------                                              SORTC9 
C  ---P A R A M E T E R S .                                              SORTC9 
C  ------------------------                                              SORTC9 
C                                                                        SORTC9 
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
      PARAMETER        (IIW   = 3*MC9BAS,               IIR  = MC9BAS+1) SORTC9 
C  ----------------------------------                                    SORTC9 
C  ---L O C A L   V A R I A B L E S .                                    SORTC9 
C  ----------------------------------                                    SORTC9 
C                                                                        SORTC9 
      DIMENSION         IOP    (             4), KPOS   (         1),    SORTC9 
     *                  INTA   (        MC9BAS), KHOS   (MC9BAS,  4),    SORTC9 
     *                  A      (    1,  MC9BAS), WK     (       IIW),    SORTC9 
     *                  IR     (           IIR),                         SORTC9 
     *                  NSRVGND(MDOS       ), NSRVFLY(MDOS       ),      SORTC9 
     *                  ISRVG  (MDOS,MC9BAS), ISRVF  (MDOS,MC9BAS)       SORTC9 
C                                                                        SORTC9 
      REAL                   A,      WK                                  SORTC9 
C                                                                        SORTC9 
      INTEGER              IOP,    KPOS,      IR,    INTA,   KHOS,       SORTC9 
     *                      NR,      NC,      NK,     IER,               SORTC9 
     *                 NSRVGND, NSRVFLY,   ISRVG,   ISRVF,               SORTC9 
     *                 IFLAG1 ,  IFLAG2,  IFLAG3                         MDN1208
C                                                                        SORTC9 
      CHARACTER*2           C9                                           SORTC9 
C                                                                        SORTC9 
C  ------------------------------------                                  SORTC9 
C  ---G L O B A L   V A R I A B L E S .                                  SORTC9 
C  ------------------------------------                                  SORTC9 
C                                                                        SORTC9 
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
C                                                                        SORTC9 
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        SORTC9 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        SORTC9 
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        SORTC9 
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
C                                                                        SORTC9 
C  ------------------------------                                        SORTC9 
C  ---C O M M O N   B L O C K S .                                        SORTC9 
C  ------------------------------                                        SORTC9 
C                                                                        SORTC9 
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
C                                                                        SORTC9 
C  ----------------------------------                                    SORTC9 
C  ---D A T A   S T A T E M E N T S .                                    SORTC9 
C  ----------------------------------                                    SORTC9 
C                                                                        SORTC9 
      DATA  IOP/0,0,0,1/,                                                SORTC9 
     *      IA, NR, NC, NK, KPOS/1,1,MC9BAS,1,1/                         SORTC9 
      DATA  C9/'C9'/                                                     SORTC9 
      DATA  IROW, ICOL  /0,0/                                            SORTC9 
C                                                                        SORTC9 
C ********************************************************************** SORTC9 
C *                                                                    * SORTC9 
C ************************  E N T R A N C E  *************************** SORTC9 
C *                                                                    * SORTC9 
C ********************************************************************** SORTC9 
C                                                                        SORTC9 
C                                                                        SORTC9 
C             ***** FIND THE GREAT CIRCLE DISTANCES FOR EACH ROW OF THE  SORTC9 
C             ***** IDMAT ARRAY (THIS REPRESENTS THE DISTANCES FROM THE  SORTC9 
C             ***** DOS LOCATION TO THE C9BASE LOCATIONS.  THE DISTANCES SORTC9 
C             ***** ARE SORTED IN ASCENDING ORDER ON THE FIRST PLANE OF  SORTC9 
C             ***** THE ARRAY.  THE SECOND PLANE REPRESENTS THE C9BAS    SORTC9 
C             ***** ASSOCIATED WITH THAT DISTANCE AND DOS                SORTC9 
C             ***** FLYTYM - COMPUTES FLIGHT TRANSPORTATION TIME         SORTC9 
C             ***** TRVLT  - COMPUTES GROUND TRANSPORTATION TIME         SORTC9 
C                                                                        SORTC9 
C ********************************************************************** SORTC9 
C ***                                                                *** SORTC9 
C *** STEP 1.                                                        *** SORTC9 
C ***         FIND INDEX FOR C9 IN AIRCRAFT DATA TABLE.              *** SORTC9 
C ***                                                                *** SORTC9 
C ********************************************************************** SORTC9 
C                                                                        SORTC9 
      IPLANE= 0                                                          SORTC9 
      DO 100 ITEST = 1, MAXAC                                            SORTC9 
         IF(AIRCRFT(ITEST)(1:2) .EQ. C9)                                 SORTC9 
     *      THEN                                                         SORTC9 
               IPLANE = ITEST                                            SORTC9 
         ENDIF                                                           SORTC9 
  100 CONTINUE                                                           SORTC9 
      IF(IPLANE .LE. 0)                                                  SORTC9 
     *   THEN                                                            SORTC9 
            WRITE(99,*)                                                  SORTC9 
            STOP 'NO C9 RECORD FOUND'                                    SORTC9 
      ENDIF                                                              SORTC9 
C                                                                        SORTC9 
C ********************************************************************** SORTC9 
C ***                                                                *** SORTC9 
C *** STEP 2.                                                        *** SORTC9 
C ***         COMPUTE MAXIMUM GROUND AND FLIGHT TRAVEL TIMES         *** SORTC9 
C ***                                                                *** SORTC9 
C ********************************************************************** SORTC9 
C                                                                        SORTC9 
      APRLDG = FLOAT(IACRAFT(IPLANE,IALTIM))                             SORTC9 
      TAXIIN = FLOAT(IACRAFT(IPLANE,ITAXI))                              SORTC9 
      TAS    = FLOAT(IACRAFT(IPLANE,   ITAS))                            SORTC9 
      DIST   = FLOAT(IACRAFT(IPLANE, MAXRNG))                            SORTC9 
      MAXFTT = IFIX (((DIST/TAS) * 60.) + APRLDG + TAXIIN)               SORTC9 
      MAXGTT = IFIX ((GNDLMT / AVGGS) * 60.)                             SORTC9 
C                                                                        SORTC9 
C ********************************************************************** SORTC9 
C ***                                                                *** SORTC9 
C *** STEP 3.                                                        *** SORTC9 
C ***         CALCULATE TRAVEL TIMES BETWEEN DOS AND C9 BASES        *** SORTC9 
C ***                                                                *** SORTC9 
C ********************************************************************** SORTC9 
C                                                                        SORTC9 
      DO 300 I = 1, MBASES                                               SORTC9 
         IF(IBASES(I,INDDOS) .GE. 1)                                     SORTC9 
     *      THEN                                                         SORTC9 
               IFROM = I                                                 SORTC9 
               IROW  = IROW + 1                                          SORTC9 
                ICOL = 0                                                 SORTC9 
               DO 310 J = 1, MBASES                                      SORTC9 
                  IF(IBASES(J,INDC9B) .GE. 1)                            SORTC9 
     *               THEN                                                SORTC9 
                        ITO = J                                          SORTC9 
                        ICOL= ICOL + 1                                   SORTC9 
                        A(1,ICOL) = TRVLTYM(IFROM,ITO,IPLANE)            SORTC9 
                  ENDIF                                                  SORTC9 
  310          CONTINUE                                                  SORTC9 
               IF(ICOL .NE. MC9BAS)                                      SORTC9 
     *            THEN                                                   SORTC9 
                     WRITE(99,*)ICOL,MC9BAS                              SORTC9 
                     STOP 'MC9BAS PARAMETER ERROR'                       SORTC9 
               ENDIF                                                     SORTC9 
             IFLAG = 1                                                   SORTC9 
             IF (IFLAG .EQ. 1)                                           SORTC9 
     *          THEN                                                     SORTC9 
                   DO 312 IJ=1,IIR                                       SORTC9 
                      IR(IJ) = IJ                                        SORTC9 
  312              CONTINUE                                              SORTC9 
                   GO TO 335                                             SORTC9 
             ENDIF                                                       SORTC9 
C *** FOR NOW SKIP THE SORTING ROUTINE SINCE THE CODE DOES               SORTC9 
C *** NOT CURRENTLY USE IT.                                              SORTC9 
               DO 320 IJ = 1, IIR                                        SORTC9 
                  IR(IJ) = 0                                             SORTC9 
  320          CONTINUE                                                  SORTC9 
               DO 330 IJ = 1, IIW                                        SORTC9 
                  WK(IJ) = 0                                             SORTC9 
  330          CONTINUE                                                  SORTC9 
CDEBUG       CALL VSAR(A,IA,NR,NC,IOP,KPOS,NK,IR,WK,IER)                 SORTC9 
  335 CONTINUE                                                           SORTC9 
C *****************************************************************      SORTC9 
C *** SORTING CURRENTLY BYPASSED.  IF SORTING IS USED, CHECK ALL***      SORTC9 
C *** REFERENCES TO IDMAT TO ENSURE YOU ARE ASSOCIATING THE     ***      SORTC9 
C *** RIGHT DISTANCE WITH THE DOS BASE(IFROM) AND THE C-9 BASE(IT0)      SORTC9 
C *****************************************************************      SORTC9 
C                                                                        SORTC9 
               IFLAG1 = 0                                                SORTC9 
               IFLAG2 = 0                                                SORTC9 
C                                                                        SORTC9 
               DO 340 J = 1, MC9BAS                                      SORTC9 
                  IDMAT(1,IROW,J) = IFIX(A(1,J))                         SORTC9 
                  IDMAT(2,IROW,J) = IR(J)                                SORTC9 
                  IF (IDMAT(1,IROW,J) . LE. 0)                           SORTC9 
     *               THEN                                                SORTC9 
                        IFLAG1 = 1                                       SORTC9 
                        NSRVGND(IROW) = NSRVGND(IROW) + 1                SORTC9 
                        J2SUB = NSRVGND(IROW)                            SORTC9 
                        ISRVG(IROW,J2SUB) = J                            SORTC9 
                     ELSE                                                SORTC9 
                        IF (IDMAT(1,IROW,J) .LE. MAXFTT)                 SORTC9 
     *                     THEN                                          SORTC9 
                              IFLAG2 = 1                                 SORTC9 
                              NSRVFLY(IROW) = NSRVFLY(IROW) + 1          SORTC9 
                              J2SUB = NSRVFLY(IROW)                      SORTC9 
                              ISRVF(IROW,J2SUB) = J                      SORTC9 
                        ENDIF                                            SORTC9 
                  ENDIF                                                  SORTC9 
  340          CONTINUE                                                  SORTC9 
               IF(IFLAG1 .LE. 0) NOLCL(IROW) = 1                         SORTC9 
               IF(IFLAG2 .LE. 0) NOC9B(IROW) = 1                         SORTC9 
         ENDIF                                                           SORTC9 
  300 CONTINUE                                                           SORTC9 
      IF(IROW .NE. MDOS)                                                 SORTC9 
     *   THEN                                                            SORTC9 
            WRITE(99,*)IROW,MDOS                                         SORTC9 
            STOP 'MDOS PARAMETER ERROR'                                  SORTC9 
      ENDIF                                                              SORTC9 
C                                                                        SORTC9 
C ********************************************************************** SORTC9 
C ***                                                                *** SORTC9 
C ***    THIS SECTION WRITES THE VALUES OF IDMAT TO TAPE99 (ERRER)   *** SORTC9 
C ***                                                                *** SORTC9 
C ********************************************************************** SORTC9 
C                                                                        SORTC9 
      IFLAG3 = 0                                                         MDN1208
      IF(IFLAG3 .EQ. 0) GO TO 999                                        MDN1208
C                                                                        MDN1208
  900 FORMAT('1',8X,15(5X,I2,1X),/)                                      SORTC9 
  910 FORMAT('0',3X,I3,2X,15(I8))                                        SORTC9 
C                                                                        SORTC9 
      DO 850 K=1,5                                                       SORTC9 
         J1 = 15 * K - 14                                                SORTC9 
         J2 = MIN0(15*K,MC9BAS)                                          SORTC9 
                                                                         SORTC9 
         WRITE(99,900) (JHDR,JHDR=J1,J2)                                 SORTC9 
         DO 810 I=1,28                                                   SORTC9 
            WRITE(99,910) I,(IDMAT(1,I,J),J=J1,J2)                       SORTC9 
  810    CONTINUE                                                        SORTC9 
         WRITE(99,900) (JHDR,JHDR=J1,J2)                                 SORTC9 
         DO 820 I=29,56                                                  SORTC9 
            WRITE(99,910) I,(IDMAT(1,I,J),J=J1,J2)                       SORTC9 
  820    CONTINUE                                                        SORTC9 
         WRITE(99,900) (JHDR,JHDR=J1,J2)                                 SORTC9 
         DO 830 I=57,84                                                  SORTC9 
            WRITE(99,910) I,(IDMAT(1,I,J),J=J1,J2)                       SORTC9 
  830    CONTINUE                                                        SORTC9 
         WRITE(99,900) (JHDR,JHDR=J1,J2)                                 SORTC9 
         DO 840 I=85,110                                                 SORTC9 
            WRITE(99,910) I,(IDMAT(1,I,J),J=J1,J2)                       SORTC9 
  840    CONTINUE                                                        SORTC9 
  850 CONTINUE                                                           SORTC9 
C                                                                        SORTC9 
C ********************************************************************** SORTC9 
C ***                                                                *** SORTC9 
C ***    THIS SECTION WRITES STATISTICS ON THE NUMBER OF SERVICING   *** SORTC9 
C ***    LOCATIONS WITHIN MAXGTT AND MAXFTT OF EACH DOS.             *** SORTC9 
C ***                                                                *** SORTC9 
C ********************************************************************** SORTC9 
C                                                                        SORTC9 
  920 FORMAT('1',1X,'DOS    #    SERV LOC')                              SORTC9 
  930 FORMAT('0',1X,I3,3X,I3,1X,30(2X,I2))                               SORTC9 
  940 FORMAT(' ',7X,I3,1X,30(2X,I2))                                     SORTC9 
C                                                                        SORTC9 
      WRITE(99,920)                                                      SORTC9 
      DO 860 I=1,MDOS                                                    SORTC9 
         WRITE(99,930) I,NSRVGND(I),(ISRVG(I,J),J=1,NSRVGND(I))          SORTC9 
         WRITE(99,940) NSRVFLY(I),(ISRVF(I,J),J=1,NSRVFLY(I))            SORTC9 
  860 CONTINUE                                                           SORTC9 
C                                                                        SORTC9 
C ********************************************************************** SORTC9 
C *                                                                    * SORTC9 
C ****************************  E X I T  ******************************* SORTC9 
C *                                                                    * SORTC9 
C ********************************************************************** SORTC9 
C                                                                        SORTC9 
  999 CONTINUE                                                           MDN1208
      RETURN                                                             SORTC9 
      END                                                                SORTC9 
