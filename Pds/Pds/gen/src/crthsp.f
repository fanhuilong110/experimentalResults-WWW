      SUBROUTINE CRTHSP(IDAY)                                            CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C *                                                                    * CRTHSP 
C *********************  D E S C R I P T I O N  ************************ CRTHSP 
C *                                                                    * CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
C     SUBROUTINE CRTHSP (CREATE HOSPITAL) CREATES A NEW HOSPITAL         CRTHSP 
C     NODE FOR EACH HOSPITAL, AN ARC FROM THE CORRESPONDING NODE         CRTHSP 
C     ON THE PREVIOUS DAY, AND A DISCHARGE ARC FROM THE NEW NODE         CRTHSP 
C     TO THE SINK.  A MUTUTAL CAPACITY CONSTRAINT FOR EACH CATE-         CRTHSP 
C     GORY CONSTRAINS DISCHARGES ON DAY IDAY INTO THE SINK.              CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C *                                                                    * CRTHSP 
C ***************************  S T A T U S  **************************** CRTHSP 
C *                                                                    * CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
C      ORIGINAL AUTHOR        : MCLAIN                                   CRTHSP 
C      ORIGINAL VERSION DATE  : 12/28/83                                 CRTHSP 
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           CRTHSP 
C                                2/15/85  FORMAL PARAMETERS MOVED TO     CRTHSP 
C                                         COMMON BLOCKS.                 CRTHSP 
C                                2/27/85  CHANGED CALLS TO WRTARC.       CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C *                                                                    * CRTHSP 
C ********************  D E C L A R A T I O N S  *********************** CRTHSP 
C *                                                                    * CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
C  ------------------------                                              CRTHSP 
C  ---P A R A M E T E R S .                                              CRTHSP 
C  ------------------------                                              CRTHSP 
C                                                                        CRTHSP 
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
C                                                                        CRTHSP 
C  ----------------------------------                                    CRTHSP 
C  ---L O C A L   V A R I A B L E S .                                    CRTHSP 
C  ----------------------------------                                    CRTHSP 
C                                                                        CRTHSP 
      INTEGER           IATYPE,  ICAT  ,  NODTYP,  NUMBRS,  IDSCHG,      CRTHSP 
     *                  MCCPTR,  IFROM ,  MCCTYP,  IFLAG ,               CRTHSP 
     *                  IFRTYP,  ITOTYP                                  CRTHSP 
C                                                                        CRTHSP 
      REAL              REQMNT,  CPACTY,  COST  ,  BEDLMT,  UPRBND       CRTHSP 
C                                                                        CRTHSP 
      DIMENSION         IDSCHG(MDAYS),  REQMNT(MCAT),  BEDLMT(MCAT),     CRTHSP 
     *                  MCCPTR( MCAT),  UPRBND(MCAT),  NUMBRS(  10)      CRTHSP 
C                                                                        CRTHSP 
C  ------------------------------------                                  CRTHSP 
C  ---G L O B A L   V A R I A B L E S .                                  CRTHSP 
C  ------------------------------------                                  CRTHSP 
C                                                                        CRTHSP 
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
C                                                                        CRTHSP 
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        CRTHSP 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        CRTHSP 
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        CRTHSP 
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
C                                                                        CRTHSP 
C  ------------------------------                                        CRTHSP 
C  ---C O M M O N   B L O C K S .                                        CRTHSP 
C  ------------------------------                                        CRTHSP 
C                                                                        CRTHSP 
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
C                                                                        CRTHSP 
C  ----------------------------------                                    CRTHSP 
C  ---D A T A   S T A T E M E N T S .                                    CRTHSP 
C  ----------------------------------                                    CRTHSP 
C                                                                        CRTHSP 
      DATA              REQMNT /MCAT* 0.0/, NUMBRS /10*0/,               CRTHSP 
     *                  MCCPTR /MCAT*0/                                  CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C *                                                                    * CRTHSP 
C ************************  E N T R A N C E  *************************** CRTHSP 
C ***                                                                *** CRTHSP 
C ********************************************************************** CRTHSP 
C ***                                                                *** CRTHSP 
C *** STEP 1.                                                        *** CRTHSP 
C ***        CREATE HOSPITALS.                                       *** CRTHSP 
C ***                                                                *** CRTHSP 
C ***                      *** DO-LOOP ***                           *** CRTHSP 
C ***                                                                *** CRTHSP 
C ***        FOR EACH HOSPITAL:                                      *** CRTHSP 
C ***                CREATE A NEW HOSPITAL NODE                      *** CRTHSP 
C ***                CREATE CAPACITY ARCS FROM THE PREVIOUS DAY      *** CRTHSP 
C ***                CREATE DISCHARGE ARCS                           *** CRTHSP 
C ***                                                                *** CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
      IFLAG    = 0                                                       CRTHSP 
      NODES(17) = 0                                                      CRTHSP 
      DO 100 IHSPTL=1,MC9BAS                                             CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C ***                                                                *** CRTHSP 
C *** STEP 1A.                                                       *** CRTHSP 
C ***         CREATE HOSPITAL NODE (NODE TYPE #12).                  *** CRTHSP 
C ***                                                                *** CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
             NODTYP   = 12                                               CRTHSP 
             NUMBRS(1) = IHSPTL                                          CRTHSP 
             NUMBRS(2) = IDAY                                            CRTHSP 
             CALL WRNODE(NODTYP,REQMNT,NUMBRS)                           CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C ***                                                                *** CRTHSP 
C *** STEP 1B.                                                       *** CRTHSP 
C ***         FOR DAY 2 AND LATER:                                   *** CRTHSP 
C ***                                                                *** CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
             IF((IDAY.GT.1))                                             CRTHSP 
     *           THEN                                                    CRTHSP 
C ********************************************************************** CRTHSP 
C ***                                                                *** CRTHSP 
C ***                      *** DO-LOOP ***                           *** CRTHSP 
C ***                                                                *** CRTHSP 
C ***        FOR EACH CATEGORY:                                      *** CRTHSP 
C ***                 CREATE HOSPITAL CAPACITY ARCS (ARC TYPE #21).  *** CRTHSP 
C ***                                                                *** CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
                     NUMBRS(1)=IHSPTL                                    CRTHSP 
                     NUMBRS(2)=IDAY                                      CRTHSP 
                     IATYPE = 21                                         CRTHSP 
                     IFROM  = IHSPDTL(IHSPTL,IPRHSP)                     CRTHSP 
                     COST = ARCCIJ(IATYPE)                               CRTHSP 
                     DO 110 ICAT=1,MCAT                                  CRTHSP 
                           BEDLMT(ICAT) = HSPCAP(IDAY-1,ICAT,IHSPTL)     CRTHSP 
 110                 CONTINUE                                            CRTHSP 
                     IFRTYP = 12                                         CRTHSP 
                     ITOTYP = 12                                         CRTHSP 
                     CALL WRTARC(IATYPE,IFROM,COST,BEDLMT,MCCPTR,        CRTHSP 
     *                           NUMBRS,IFRTYP,ITOTYP)                   CRTHSP 
             ENDIF                                                       CRTHSP 
             IHSPDTL(IHSPTL,IPRHSP)=NODES(NODTYP)                        CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C ***                                                                *** CRTHSP 
C *** STEP 1D.                                                       *** CRTHSP 
C ***                      *** DO-LOOP ***                           *** CRTHSP 
C ***                                                                *** CRTHSP 
C ***        FOR EACH CATEGORY:                                      *** CRTHSP 
C ***                          IF IDAY IS GREATER THAN LOS, SET      *** CRTHSP 
C ***                          IFLAG TO 1 AND CREATE DISCHARGE       *** CRTHSP 
C ***                          ARCS (ARC TYPE #22) AND DAILY TO      *** CRTHSP 
C ***                          SUPERSINK ARCS (ARC TYPE #7).         *** CRTHSP 
C ***                                                                *** CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
             DO 120 ICAT=1,MCAT                                          CRTHSP 
                    UPRBND(ICAT) = 0.0                                   CRTHSP 
                    IF (IDAY .GE. (LOS(ICAT)+1))                         CRTHSP 
     *                 THEN                                              CRTHSP 
                            ICASPT       = IDAY - LOS(ICAT)              CRTHSP 
                            UPRBND(ICAT) = FLOAT(ICASLT(ICASPT,ICAT))    CRTHSP 
                            IDSCHG(ICAT) = 0                             CRTHSP 
                            IFLAG = 1                                    CRTHSP 
                  ENDIF                                                  CRTHSP 
  120        CONTINUE                                                    CRTHSP 
             IF(IFLAG.GT.0)                                              CRTHSP 
     *          THEN                                                     CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C ***                                                                *** CRTHSP 
C *** STEP 1E.                                                       *** CRTHSP 
C ***         IF DAILY SINK NODE HAS NOT BEEN CREATED, THEN          *** CRTHSP 
C ***         CREATE DAILY SINK NODE (NODE TYPE #17).                *** CRTHSP 
C ***                                                                *** CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
                    IF(NODES(17).LE.0)                                   CRTHSP 
     *                 THEN                                              CRTHSP 
                           NODTYP = 17                                   CRTHSP 
                           NUMBRS(1) = IDAY                              CRTHSP 
                           CALL WRNODE(NODTYP,REQMNT,NUMBRS)             CRTHSP 
                    ENDIF                                                CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C ***                                                                *** CRTHSP 
C *** STEP 1F.                                                       *** CRTHSP 
C ***         CREATE DISCHARGE ARCS TO THE DAILY SINK (ARC TYPE #22) *** CRTHSP 
C ***                                                                *** CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
                    IATYPE = 22                                          CRTHSP 
                    NUMBRS(1) = IHSPTL                                   CRTHSP 
                    NUMBRS(2) = IDAY                                     CRTHSP 
                    IARCPT = IARCS (IATYPE,NODEFR)                       CRTHSP 
                    IFROM  = NODES(IARCPT)                               CRTHSP 
                    COST   = ARCCIJ(IATYPE  )                            CRTHSP 
                    IFRTYP = 12                                          CRTHSP 
                    ITOTYP = 17                                          CRTHSP 
                    CALL WRTARC(IATYPE,IFROM,COST,UPRBND,IDSCHG,NUMBRS,  CRTHSP 
     *                          IFRTYP,ITOTYP)                           CRTHSP 
             ENDIF                                                       CRTHSP 
  100 CONTINUE                                                           CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C ***                                                                *** CRTHSP 
C *** STEP 2.                                                        *** CRTHSP 
C ***        IF IDAY IS GREATER THAN LENGTH OF STAY FOR ANY PATIENT  *** CRTHSP 
C ***        CREATE ARCS TO THE SUPERSINK (ARC TYPE #7)              *** CRTHSP 
C ***                                                                *** CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
      IF(IFLAG.GT.0)                                                     CRTHSP 
     *   THEN                                                            CRTHSP 
             DO 200 ICAT=1,MCAT                                          CRTHSP 
                    UPRBND(ICAT) = 0.0                                   CRTHSP 
                    IDSCHG(ICAT) = 0.0                                   CRTHSP 
                    IF (IDAY .GE. (LOS(ICAT)+1))                         CRTHSP 
     *                 THEN                                              CRTHSP 
                            ICASPT       = IDAY - LOS(ICAT)              CRTHSP 
                            UPRBND(ICAT) = FLOAT(ICASLT(ICASPT,ICAT))    CRTHSP 
                    ENDIF                                                CRTHSP 
  200        CONTINUE                                                    CRTHSP 
             NUMBRS(1) = IDAY                                            CRTHSP 
             IATYPE    = 7                                               CRTHSP 
             COST      = ARCCIJ(IATYPE)                                  CRTHSP 
             IARCPT    = IARCS(IATYPE,NODEFR)                            CRTHSP 
             IFROM     = NODES(IARCPT)                                   CRTHSP 
             NUMBRS(1) = IDAY                                            CRTHSP 
             IFRTYP = 17                                                 CRTHSP 
             ITOTYP = 13                                                 CRTHSP 
             CALL WRTARC(IATYPE,IFROM,COST,UPRBND,IDSCHG,NUMBRS,         CRTHSP 
     *                   IFRTYP,ITOTYP)                                  CRTHSP 
      ENDIF                                                              CRTHSP 
C                                                                        CRTHSP 
C ********************************************************************** CRTHSP 
C *                                                                    * CRTHSP 
C ****************************  E X I T  ******************************* CRTHSP 
C *                                                                    * CRTHSP 
C ********************************************************************** CRTHSP 
C                                                                        CRTHSP 
      RETURN                                                             CRTHSP 
      END                                                                CRTHSP 
