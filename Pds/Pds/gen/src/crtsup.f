      SUBROUTINE CRTSUP(IDAY)                                            CRTSUP 
C                                                                        CRTSUP 
C ********************************************************************** CRTSUP 
C *                                                                    * CRTSUP 
C *********************  D E S C R I P T I O N  ************************ CRTSUP 
C *                                                                    * CRTSUP 
C ********************************************************************** CRTSUP 
C                                                                        CRTSUP 
C     SUBROUTINE CRTSUP (CREATE PATIENT SUPPLY) RESTRICTS TOTAL          CRTSUP 
C     DAILY FLOW TO ALL IAPS OF PATIENTS IN A GIVEN CATEGORY TO          CRTSUP 
C     THE NUMBER OF CASUALTIES GENERATED IN THAT CATEGORY THAT           CRTSUP 
C     DAY.  THE VECTOR MCCDPF SAVES THE CONSTRAINT INDICES.              CRTSUP 
C                                                                        CRTSUP 
C ********************************************************************** CRTSUP 
C *                                                                    * CRTSUP 
C ***************************  S T A T U S  **************************** CRTSUP 
C *                                                                    * CRTSUP 
C ********************************************************************** CRTSUP 
C                                                                        CRTSUP 
C      ORIGINAL AUTHOR        : MCLAIN                                   CRTSUP 
C      ORIGINAL VERSION DATE  : 12/28/83                                 CRTSUP 
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           CRTSUP 
C                                2/15/85  FORMAL PARAMETERS MOVED TO     CRTSUP 
C                                         COMMON BLOCKS.                 CRTSUP 
C                                                                        CRTSUP 
C ********************************************************************** CRTSUP 
C *                                                                    * CRTSUP 
C ********************  D E C L A R A T I O N S  *********************** CRTSUP 
C *                                                                    * CRTSUP 
C ********************************************************************** CRTSUP 
C                                                                        CRTSUP 
C  ------------------------                                              CRTSUP 
C  ---P A R A M E T E R S .                                              CRTSUP 
C  ------------------------                                              CRTSUP 
C                                                                        CRTSUP 
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
C                                                                        CRTSUP 
C  ----------------------------------                                    CRTSUP 
C  ---L O C A L   V A R I A B L E S .                                    CRTSUP 
C  ----------------------------------                                    CRTSUP 
C                                                                        CRTSUP 
      INTEGER           ICAT ,  MCCTYP,  NUMBRS,  IFRTYP,  ITOTYP        CRTSUP 
C                                                                        CRTSUP 
      REAL              CPACT,  UPRBND,  REQMNT                          CRTSUP 
C                                                                        CRTSUP 
      DIMENSION         NUMBRS(10),  UPRBND(MCAT),  REQMNT(MCAT)         CRTSUP 
C                                                                        CRTSUP 
C  ------------------------------------                                  CRTSUP 
C  ---G L O B A L   V A R I A B L E S .                                  CRTSUP 
C  ------------------------------------                                  CRTSUP 
C                                                                        CRTSUP 
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
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        CRTSUP 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        CRTSUP 
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
C                                                                        CRTSUP 
C  ------------------------------                                        CRTSUP 
C  ---C O M M O N   B L O C K S .                                        CRTSUP 
C  ------------------------------                                        CRTSUP 
C                                                                        CRTSUP 
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
C                                                                        CRTSUP 
C  ----------------------------------                                    CRTSUP 
C  ---D A T A   S T A T E M E N T S .                                    CRTSUP 
C  ----------------------------------                                    CRTSUP 
C                                                                        CRTSUP 
      DATA  NUMBRS /10*0/,  MCCTYP /2/,  REQMNT/MCAT*0.0/                CRTSUP 
C                                                                        CRTSUP 
C ********************************************************************** CRTSUP 
C *                                                                    * CRTSUP 
C ************************  E N T R A N C E  *************************** CRTSUP 
C *                                                                    * CRTSUP 
C ********************************************************************** CRTSUP 
C ***                                                                *** CRTSUP 
C *** STEP 1.                                                        *** CRTSUP 
C ***        CREATE DAILY SOURCE NODE (NODE TYPE # 16)               *** CRTSUP 
C ***                                                                *** CRTSUP 
C ********************************************************************** CRTSUP 
C                                                                        CRTSUP 
      NUMBRS(1) = IDAY                                                   CRTSUP 
      NODTYP    = 16                                                     CRTSUP 
      CALL WRNODE(NODTYP,REQMNT,NUMBRS)                                  CRTSUP 
C                                                                        CRTSUP 
C ********************************************************************** CRTSUP 
C ***                                                                *** CRTSUP 
C *** STEP 2.                                                        *** CRTSUP 
C ***        CREATE DAILY CASUALTY ARCS (ARC TYPE #6)                *** CRTSUP 
C ***                                                                *** CRTSUP 
C ********************************************************************** CRTSUP 
C                                                                        CRTSUP 
      IATYPE    = 6                                                      CRTSUP 
      NUMBRS(1) = IDAY                                                   CRTSUP 
      COST      = ARCCIJ(IATYPE)                                         CRTSUP 
      IARCPT    = IARCS(IATYPE,NODEFR)                                   CRTSUP 
      IFROM     = NODES(IARCPT)                                          CRTSUP 
      DO 200 ICAT=1,MCAT                                                 CRTSUP 
             UPRBND(ICAT) = FLOAT(ICASLT(IDAY,ICAT))                     CRTSUP 
             MCCDPF(ICAT) = 0                                            CRTSUP 
  200 CONTINUE                                                           CRTSUP 
      IFRTYP =  1                                                        CRTSUP 
      ITOTYP = 16                                                        CRTSUP 
      CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCDPF,NUMBRS,                CRTSUP 
     *            IFRTYP,ITOTYP)                                         CRTSUP 
C                                                                        CRTSUP 
C ********************************************************************** CRTSUP 
C *                                                                    * CRTSUP 
C ****************************  E X I T  ******************************* CRTSUP 
C *                                                                    * CRTSUP 
C ********************************************************************** CRTSUP 
C                                                                        CRTSUP 
      RETURN                                                             CRTSUP 
      END                                                                CRTSUP 
