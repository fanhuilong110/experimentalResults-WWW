      SUBROUTINE CRTIPN                                                  CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C *                                                                    * CRTIPN 
C *********************  D E S C R I P T I O N  ************************ CRTIPN 
C *                                                                    * CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
C     SUBROUTINE CRTIPN (CREATE IN-PROCESS NODE) FLOWS PATIENTS          CRTIPN 
C     REMAINING IN THE SYSTEM (IN ASFS AND HOSPITALS) AT THE END         CRTIPN 
C     OF THE SCENARIO TO THE SINK.                                       CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C *                                                                    * CRTIPN 
C ***************************  S T A T U S  **************************** CRTIPN 
C *                                                                    * CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
C      ORIGINAL AUTHOR        : MCLAIN                                   CRTIPN 
C      ORIGINAL VERSION DATE  : 12/28/83                                 CRTIPN 
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           CRTIPN 
C                                2/15/85  FORMAL PARAMETERS MOVED TO     CRTIPN 
C                                         COMMON BLOCKS.                 CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C *                                                                    * CRTIPN 
C ********************  D E C L A R A T I O N S  *********************** CRTIPN 
C *                                                                    * CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
C  ------------------------                                              CRTIPN 
C  ---P A R A M E T E R S .                                              CRTIPN 
C  ------------------------                                              CRTIPN 
C                                                                        CRTIPN 
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
C                                                                        CRTIPN 
C  ----------------------------------                                    CRTIPN 
C  ---L O C A L   V A R I A B L E S .                                    CRTIPN 
C  ----------------------------------                                    CRTIPN 
C                                                                        CRTIPN 
      INTEGER           IDAY  ,  NUMBRS,  MCCPTR,  IHSPTL,  ICAT  ,      CRTIPN 
     *                  IASF  ,  IAP   ,  NODTYP,  IFROM ,  IATYPE,      CRTIPN 
     *                  IFRTYP,  ITOTYP,  MSTRBAS                        CRTIPN 
C                                                                        CRTIPN 
      REAL              CPACTY,  COST  ,  UPRBND                         CRTIPN 
C                                                                        CRTIPN 
      DIMENSION         MCCPTR(MCAT), UPRBND(MCAT),  NUMBRS(10),         CRTIPN 
     *                  REQMNT(MCAT)                                     CRTIPN 
C                                                                        CRTIPN 
C  ------------------------------------                                  CRTIPN 
C  ---G L O B A L   V A R I A B L E S .                                  CRTIPN 
C  ------------------------------------                                  CRTIPN 
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
C                                                                        CRTIPN 
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        CRTIPN 
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
C                                                                        CRTIPN 
C  ------------------------------                                        CRTIPN 
C  ---C O M M O N   B L O C K S .                                        CRTIPN 
C  ------------------------------                                        CRTIPN 
C                                                                        CRTIPN 
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
C                                                                        CRTIPN 
C  ----------------------------------                                    CRTIPN 
C  ---D A T A   S T A T E M E N T S .                                    CRTIPN 
C  ----------------------------------                                    CRTIPN 
C                                                                        CRTIPN 
       DATA MCCPTR/MCAT*0/, REQMNT/MCAT*0.0/, NUMBRS /10*0/,             CRTIPN 
     *      UPRBND/MCAT*-1.0/, COST/10000.0/                             CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C *                                                                    * CRTIPN 
C ************************  E N T R A N C E  *************************** CRTIPN 
C *                                                                    * CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C ***                                                                *** CRTIPN 
C *** STEP 1.                                                        *** CRTIPN 
C ***        CREATE THE IN-PROCESS NODE (NODE TYPE #14).             *** CRTIPN 
C ***                                                                *** CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
      NODTYP=14                                                          CRTIPN 
      CALL WRNODE(NODTYP,REQMNT,NUMBRS)                                  CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C ***                                                                *** CRTIPN 
C *** STEP 2.                                                        *** CRTIPN 
C ***        CREATE IN-PROCESS ARCS FROM THE SUPPLY NODE.            *** CRTIPN 
C ***                                                                *** CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
             IATYPE    = 23                                              CRTIPN 
             IFROM     = NODES(1)                                        CRTIPN 
             IFRTYP = 1                                                  CRTIPN 
             ITOTYP = NODTYP                                             CRTIPN 
             COST   = ARCCIJ(IATYPE)                                     CRTIPN 
             CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,NUMBRS,         CRTIPN 
     *                   IFRTYP,ITOTYP)                                  CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C ***                                                                *** CRTIPN 
C *** STEP 3.                                                        *** CRTIPN 
C ***        CREATE IN-PROCESS ARCS FROM THE LAST IAP RELEASE NODES. *** CRTIPN 
C ***                                                                *** CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
      DO 300 IAP = 1, MIAPS                                              CRTIPN 
             NUMBRS(1) = IAP                                             CRTIPN 
             IATYPE    = 24                                              CRTIPN 
             IFROM     = IAPDTL(IAP,IAPPNN)                              CRTIPN 
             IFRTYP = 3                                                  CRTIPN 
             ITOTYP = NODTYP                                             CRTIPN 
             COST   = ARCCIJ(IATYPE)                                     CRTIPN 
             CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,NUMBRS,         CRTIPN 
     *                   IFRTYP,ITOTYP)                                  CRTIPN 
  300 CONTINUE                                                           CRTIPN 
C                                                                        CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C ***                                                                *** CRTIPN 
C *** STEP 4.                                                        *** CRTIPN 
C ***        CREATE IN-PROCESS ARCS FROM THE LAST HOSPITAL NODES.    *** CRTIPN 
C ***                                                                *** CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
      DO 400 IHSPTL=1,MC9BAS                                             CRTIPN 
             NUMBRS(1) = IHSPTL                                          CRTIPN 
             IATYPE    = 25                                              CRTIPN 
             IFROM     = IHSPDTL(IHSPTL,IPRHSP)                          CRTIPN 
             IFRTYP = 12                                                 CRTIPN 
             ITOTYP = 14                                                 CRTIPN 
             COST   = ARCCIJ(IATYPE)                                     CRTIPN 
             CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,NUMBRS,         CRTIPN 
     *                   IFRTYP,ITOTYP)                                  CRTIPN 
  400 CONTINUE                                                           CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C ***                                                                *** CRTIPN 
C *** STEP 5.                                                        *** CRTIPN 
C ***        CREATE IN-PROCESS ARCS FROM THE LAST ASF NODES.         *** CRTIPN 
C ***                                                                *** CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
      DO 500 IASF=1,MASF                                                 CRTIPN 
             IF (IASFDTL(IASF,IASFPNN) .EQ. 0) GO TO 500                 CRTIPN 
             NUMBRS(1) = IASF                                            CRTIPN 
             IATYPE    = 26                                              CRTIPN 
             IFROM     = IASFDTL(IASF,IASFPNN)                           CRTIPN 
             IFRTYP = 0                                                  CRTIPN 
             MSTRBAS = IASFDTL(IASF,LNKASF)                              CRTIPN 
             IF (IBASES(MSTRBAS,INDDOS) .EQ. 1) IFRTYP = 11              CRTIPN 
             IF (IBASES(MSTRBAS,INDPOD) .EQ. 1) IFRTYP =  6              CRTIPN 
             ITOTYP = 14                                                 CRTIPN 
             COST   = ARCCIJ(IATYPE)                                     CRTIPN 
             CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,NUMBRS,         CRTIPN 
     *                   IFRTYP,ITOTYP)                                  CRTIPN 
  500 CONTINUE                                                           CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C ***                                                                *** CRTIPN 
C *** STEP 6.                                                        *** CRTIPN 
C ***        CREATE ARCS FROM THE IN-PROCESS NODE TO THE SINK.       *** CRTIPN 
C ***                                                                *** CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
      NUMBRS(1) = 0                                                      CRTIPN 
      IATYPE    = 27                                                     CRTIPN 
      IARCPT    = IARCS(IATYPE,NODEFR)                                   CRTIPN 
      IFROM     = NODES(IARCPT)                                          CRTIPN 
      IFRTYP = 14                                                        CRTIPN 
      ITOTYP = 13                                                        CRTIPN 
      COST   = ARCCIJ(IATYPE)                                            CRTIPN 
      CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,NUMBRS,                CRTIPN 
     *            IFRTYP,ITOTYP)                                         CRTIPN 
C                                                                        CRTIPN 
C ********************************************************************** CRTIPN 
C *                                                                    * CRTIPN 
C ****************************  E X I T  ******************************* CRTIPN 
C *                                                                    * CRTIPN 
C ********************************************************************** CRTIPN 
C                                                                        CRTIPN 
      RETURN                                                             CRTIPN 
      END                                                                CRTIPN 
