      SUBROUTINE CRTC9(IDAY)                                             CRTC9  
C                                                                        CRTC9  
C ********************************************************************** CRTC9  
C *                                                                    * CRTC9  
C *********************  D E S C R I P T I O N  ************************ CRTC9  
C *                                                                    * CRTC9  
C ********************************************************************** CRTC9  
C                                                                        CRTC9  
C     SUBROUTINE CRTC9                                                   CRTC9  
C                                                                        CRTC9  
C     CREATE ARCS FROM APOD'S AND DOS'S TO HOSPITALS BEYOND THE MAXIMUM  CRTC9  
C     GROUND TRANSPORTATION LIMIT.                                       CRTC9  
C                                                                        CRTC9  
C ********************************************************************** CRTC9  
C *                                                                    * CRTC9  
C ***************************  S T A T U S  **************************** CRTC9  
C *                                                                    * CRTC9  
C ********************************************************************** CRTC9  
C                                                                        CRTC9  
C      ORIGINAL AUTHOR        : MCLAIN                                   CRTC9  
C      ORIGINAL VERSION DATE  : 12/28/83                                 CRTC9  
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           CRTC9  
C                                2/15/85  FORMAL PARAMETERS MOVED TO     CRTC9  
C                                         COMMON BLOCKS.                 CRTC9  
C                                2/27/85                                 CRTC9  
C                                                                        CRTC9  
C ********************************************************************** CRTC9  
C *                                                                    * CRTC9  
C ********************  D E C L A R A T I O N S  *********************** CRTC9  
C *                                                                    * CRTC9  
C ********************************************************************** CRTC9  
C                                                                        CRTC9  
C  ------------------------                                              CRTC9  
C  ---P A R A M E T E R S .                                              CRTC9  
C  ------------------------                                              CRTC9  
C                                                                        CRTC9  
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
C                                                                        CRTC9  
C  ----------------------------------                                    CRTC9  
C  ---L O C A L   V A R I A B L E S .                                    CRTC9  
C  ----------------------------------                                    CRTC9  
C                                                                        CRTC9  
      INTEGER           ICAT  ,  IHSPTL,  NUMBRS, MCCPTR,                CRTC9  
     *                  IFRTYP,  ITOTYP                                  CRTC9  
C                                                                        CRTC9  
      REAL              UPRBND,  REQMNT                                  CRTC9  
C                                                                        CRTC9  
      DIMENSION   UPRBND(MCAT),  NUMBRS(10),  MCCPTR(MCAT),              CRTC9  
     *            REQMNT(MCAT)                                           CRTC9  
C                                                                        CRTC9  
C  ------------------------------------                                  CRTC9  
C  ---G L O B A L   V A R I A B L E S .                                  CRTC9  
C  ------------------------------------                                  CRTC9  
C                                                                        CRTC9  
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
C                                                                        CRTC9  
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        CRTC9  
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        CRTC9  
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        CRTC9  
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
C                                                                        CRTC9  
C  ------------------------------                                        CRTC9  
C  ---C O M M O N   B L O C K S .                                        CRTC9  
C  ------------------------------                                        CRTC9  
C                                                                        CRTC9  
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
C                                                                        CRTC9  
C  ----------------------------------                                    CRTC9  
C  ---D A T A   S T A T E M E N T S .                                    CRTC9  
C  ----------------------------------                                    CRTC9  
C                                                                        CRTC9  
      DATA NUMBRS/10*0/,  REQMNT/MCAT*0.0/                               CRTC9  
C                                                                        CRTC9  
C  --------------------------------------                                CRTC9  
C  ---F O R M A T   S T A T E M E N T S .                                CRTC9  
C  --------------------------------------                                CRTC9  
 9000 FORMAT(1X,'ILLEGAL MISSION TYPE =',I3,' IN CRTC9')                 CRTC9  
C                                                                        CRTC9  
C ********************************************************************** CRTC9  
C *                                                                    * CRTC9  
C ************************  E N T R A N C E  *************************** CRTC9  
C *                                                                    * CRTC9  
C ********************************************************************** CRTC9  
C                                                                        CRTC9  
C ********************************************************************** CRTC9  
C ***                                                                *** CRTC9  
C *** STEP 1.                                                        *** CRTC9  
C ***        CREATE C-9 DISTRIBUTION NODES (NODE TYPES #7, #8 AND #9)*** CRTC9  
C ***                                                                *** CRTC9  
C ********************************************************************** CRTC9  
C                                                                        CRTC9  
C     COST = TRAVEL TIME FROM MATRIX IDMAT PLUS PROCESSING TIME          CRTC9  
      DO 100 I = 1, IC9PTR                                               CRTC9  
             MBASE  = IC9MSN(I,LNKC9M)                                   CRTC9  
             IORGIN = IBASES(MBASE,NDXDOS)                               CRTC9  
             ITYPE  = IC9MSN(I,IC9ONT)                                   CRTC9  
             IF(ITYPE .EQ. 1)                                            CRTC9  
     *          THEN                                                     CRTC9  
                    IASF      = IC9MSN(I,IC9NDX)                         CRTC9  
                    IARC2     = 17                                       CRTC9  
                    NODE2     = 7                                        CRTC9  
                    NUMBRS(1) = IASF                                     CRTC9  
                ELSE                                                     CRTC9  
     *              IF(ITYPE .EQ. 2)                                     CRTC9  
     *                 THEN                                              CRTC9  
                           IDOS      = IC9MSN(I,IC9NDX)                  CRTC9  
                           IARC2     = 20                                CRTC9  
                           NODE2     = 9                                 CRTC9  
                           NUMBRS(1) = IDOS                              CRTC9  
                       ELSE                                              CRTC9  
                           WRITE(99,9000)ITYPE                           CRTC9  
             ENDIF                                                       CRTC9  
C                                                                        CRTC9  
C ********************************************************************** CRTC9  
C ***                                                                *** CRTC9  
C *** STEP 2.                                                        *** CRTC9  
C ***        CREATE ARCS FROM DISTRIBUTION NODES TO HOSPITALS        *** CRTC9  
C ***                                                                *** CRTC9  
C ********************************************************************** CRTC9  
C                                                                        CRTC9  
             NODTYP    = NODE2                                           CRTC9  
             IATYPE    = IARC2                                           CRTC9  
             NUMBRS(1) = IDOS                                            CRTC9  
             NUMBRS(3) = IDAY                                            CRTC9  
             IF(NODTYP .EQ. 8)                                           CRTC9  
     *          THEN                                                     CRTC9  
                    NUMBRS(1) = IAPOD                                    CRTC9  
                ELSE                                                     CRTC9  
     *              IF(NODTYP .EQ. 7)                                    CRTC9  
     *                 THEN                                              CRTC9  
                           NUMBRS(1) = IASF                              CRTC9  
             ENDIF                                                       CRTC9  
             DO 200 IHSPTL=1,MC9BAS                                      CRTC9  
                    TYME = FLOAT(IDMAT(1,IORGIN,IHSPTL))                 CRTC9  
                    IF ((TYME .GT. 0.0) .AND. (TYME .LE. FLOAT(MAXFTT))) CRTC9  
     *                 THEN                                              CRTC9  
                           NUMBRS(2) = IHSPTL                            CRTC9  
                           COST = ARCCIJ(IATYPE) + TYME                  CRTC9  
                           IFROM = IC9MSN(I,IC9NOD)                      CRTC9  
                           IARCPT  = IARCS(IATYPE,NODETO)                CRTC9  
                           NODES(IARCPT) = IHSPDTL(IHSPTL,IPRHSP)        CRTC9  
                           DO 210 ICAT=1,MCAT                            CRTC9  
                                  UPRBND(ICAT)=ARCUIJ(IATYPE,ICAT)       CRTC9  
                                  MCCPTR(ICAT)=0                         CRTC9  
  210                      CONTINUE                                      CRTC9  
                           IFRTYP = NODTYP                               CRTC9  
                           ITOTYP = 12                                   CRTC9  
                           CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,  CRTC9  
     *                                 NUMBRS,IFRTYP,ITOTYP)             CRTC9  
                   ENDIF                                                 CRTC9  
  200       CONTINUE                                                     CRTC9  
  100 CONTINUE                                                           CRTC9  
C                                                                        CRTC9  
C ********************************************************************** CRTC9  
C *                                                                    * CRTC9  
C ****************************  E X I T  ******************************* CRTC9  
C *                                                                    * CRTC9  
C ********************************************************************** CRTC9  
C                                                                        CRTC9  
      RETURN                                                             CRTC9  
      END                                                                CRTC9  
