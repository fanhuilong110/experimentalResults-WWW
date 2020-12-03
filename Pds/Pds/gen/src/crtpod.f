      SUBROUTINE CRTPOD(IAPOD,IDAY)                                      CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C *                                                                    * CRTPOD 
C *********************  D E S C R I P T I O N  ************************ CRTPOD 
C *                                                                    * CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
C     SUBROUTINE CRTPOD CREATES THE APOD NODE AND ITS ASSOCIATED ASF.    CRTPOD 
C     IT ALSO CREATES THE ARCS FROM THE APOD TO THE ASF AND FROM THE     CRTPOD 
C     ASF TO LOCAL HOSPITALS AND TO THE C-9 SYSTEM.                      CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C *                                                                    * CRTPOD 
C ***************************  S T A T U S  **************************** CRTPOD 
C *                                                                    * CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
C      ORIGINAL AUTHOR        : MCLAIN                                   CRTPOD 
C      ORIGINAL VERSION DATE  : 12/28/83                                 CRTPOD 
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           CRTPOD 
C                                2/15/85  FORMAL PARAMETERS MOVED TO     CRTPOD 
C                                         COMMON BLOCKS.                 CRTPOD 
C                                2/28/85  MISSION NETWORK REDUCED.       CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C *                                                                    * CRTPOD 
C ********************  D E C L A R A T I O N S  *********************** CRTPOD 
C *                                                                    * CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
C  ------------------------                                              CRTPOD 
C  ---P A R A M E T E R S .                                              CRTPOD 
C  ------------------------                                              CRTPOD 
C                                                                        CRTPOD 
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
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
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
C  ----------------------------------                                    CRTPOD 
C  ---L O C A L   V A R I A B L E S .                                    CRTPOD 
C  ----------------------------------                                    CRTPOD 
C                                                                        CRTPOD 
      INTEGER           NODTYP,  NUMBRS,  IWORK,  IDAY,  IATYPE,         CRTPOD 
     *                  IFLAG1,  IFLAG2,  IASF,   IAPOD, IDOS            CRTPOD 
C                                                                        CRTPOD 
      REAL              CPACTY,  ADMIT,  RELEAS, CPACTYL, CPACTYA        CRTPOD 
C                                                                        CRTPOD 
      DIMENSION         NUMBRS(  10),  REQMNT(MCAT),  MCCPTR(MCAT),      CRTPOD 
     *                  UPRBND(MCAT),  IWORK(25)                         CRTPOD 
      DATA NUMBRS /10*0/, REQMNT /MCAT*0.0/
C                                                                        CRTPOD 
                              NODTYP=5                                   CRTPOD 
                              NUMBRS(1) = IAPOD                          CRTPOD 
                              NUMBRS(2) = IDAY                           CRTPOD 
                              NUMBRS(3) = 0                              CRTPOD 
                              CALL WRNODE(NODTYP,REQMNT,NUMBRS)          CRTPOD 
                              PODXIST(IAPOD) = .TRUE.                    CRTPOD 
                              IPODDTL(IAPOD,IPODCHK) = NBRNOD            CRTPOD 
                              IDOS = IPODDTL(IAPOD,IPODDOS)              CRTPOD 
                              IF (IDOS .GT. 0)                           CRTPOD 
     *                           THEN                                    CRTPOD 
                                     DOSXIST(IDOS) = .TRUE.              CRTPOD 
                                     IDOSDTL(IDOS,IDOSCHK) = NBRNOD      CRTPOD 
                                 ENDIF                                   CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C ***                                                                *** CRTPOD 
C *** STEP 2A2.                                                      *** CRTPOD 
C ***         IF THE INTERTHEATER ARRIVAL POINT HAS AN ASF, ADD      *** CRTPOD 
C ***         THE ASF AND LINKS TO LOCAL HOSPITALS AND THE C-9       *** CRTPOD 
C ***         SYSTEM.                                                *** CRTPOD 
C ***                                                                *** CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
                              IF(IPODDTL(IAPOD,IPODIND).EQ.1)            CRTPOD 
     *                           THEN                                    CRTPOD 
                                     IASF = IPODDTL(IAPOD,IPODASF)       CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C ***                                                                *** CRTPOD 
C *** STEP 2A2.1                                                     *** CRTPOD 
C ***           CREATE THE ASF NODE (NODE TYPE #6)                   *** CRTPOD 
C ***                                                                *** CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
                                     NODTYP    = 6                       CRTPOD 
                                     NUMBRS(1) = IASF                    CRTPOD 
                                     NUMBRS(2) = IAPOD                   CRTPOD 
                                     NUMBRS(3) = IDAY                    CRTPOD 
                                     CALL WRNODE(NODTYP,REQMNT,NUMBRS)   CRTPOD 
                                      IASFDTL(IASF,IASFCHK) = NBRNOD     CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C ***                                                                *** CRTPOD 
C *** STEP 2A2.2                                                     *** CRTPOD 
C ***           IF THE ASF WAS CREATED ON A PREVIOUS DAY:            *** CRTPOD 
C ***                                                                *** CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
                                     IF(IASFDTL(IASF,IASFPNN) .GT. 0)    CRTPOD 
     *                                  THEN                             CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C ***                                                                *** CRTPOD 
C *** STEP 2A2.2.1                                                   *** CRTPOD 
C ***         CREATE THE APOD ASF CAPACITY CONSTRAINT (MCC TYPE #6)  *** CRTPOD 
C ***                                                                *** CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
                                            MCCTYP    = 6                CRTPOD 
                                            CPACTY = FLOAT(IASFDTL(IASF, CRTPOD 
     *                                                      IASFMX))     CRTPOD 
                                            NUMBRS(1) = IASF             CRTPOD 
                                            NUMBRS(2) = IDAY             CRTPOD 
                                            CALL WRTMCC(MCCTYP,CPACTY,   CRTPOD 
     *                                                      NUMBRS)      CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C ***                                                                *** CRTPOD 
C *** STEP 2A2.2.2                                                   *** CRTPOD 
C ***         CREATE ARCS FROM THE PREVIOUS ASF RELEASE NODE.        *** CRTPOD 
C ***         (ARC TYPE #12)                                         *** CRTPOD 
C ***                                                                *** CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
                                            IATYPE    = 12               CRTPOD 
                                            COST      = 1440             CRTPOD 
                                            NUMBRS(1) = IASF             CRTPOD 
                                            NUMBRS(2) = IAPOD            CRTPOD 
                                            NUMBRS(3) = IDAY             CRTPOD 
                                            IFROM     =                  CRTPOD 
     *                                             IASFDTL(IASF,IASFPNN) CRTPOD 
                                                                         CRTPOD 
                                            DO 230 ICAT=1,MCAT           CRTPOD 
                                                   UPRBND(ICAT) =        CRTPOD 
     *                                             ARCUIJ(IATYPE,ICAT)   CRTPOD 
                                            IF (UPRBND(ICAT) .EQ. -1.00) CRTPOD 
     *                                          UPRBND(ICAT) = CPACTY    CRTPOD 
                                                   MCCPTR(ICAT) =NBRMCC  CRTPOD 
  230                                       CONTINUE                     CRTPOD 
                                            IFRTYP = 6                   CRTPOD 
                                            ITOTYP = 5                   CRTPOD 
                                            CALL WRTARC(IATYPE,IFROM,    CRTPOD 
     *                                           COST,UPRBND,MCCPTR,     CRTPOD 
     *                                           NUMBRS,IFRTYP,ITOTYP)   CRTPOD 
                                     ENDIF                               CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C ***                                                                *** CRTPOD 
C *** STEP 2A2.3                                                     *** CRTPOD 
C ***           CREATE APOD-TO-ASF ARC (ARC TYPE #5)                 *** CRTPOD 
C ***                                                                *** CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
                                     IATYPE = 5                          CRTPOD 
                                     COST   = ARCCIJ(IATYPE)             CRTPOD 
                                     IARCPT = IARCS(IATYPE,NODEFR)       CRTPOD 
                                     IFROM  = NODES(IARCPT)              CRTPOD 
                                     NUMBRS(1) = IAPOD                   CRTPOD 
                                     NUMBRS(2) = IASF                    CRTPOD 
                                     NUMBRS(3) = IDAY                    CRTPOD 
                                     DO 240 ICAT=1,MCAT                  CRTPOD 
                                            UPRBND(ICAT)= ARCUIJ(IATYPE, CRTPOD 
     *                                                            ICAT)  CRTPOD 
                                            MCCPTR(ICAT) = 0             CRTPOD 
  240                                CONTINUE                            CRTPOD 
                                     IFRTYP = 5                          CRTPOD 
                                     ITOTYP = 6                          CRTPOD 
                                     CALL WRTARC(IATYPE,IFROM,COST,      CRTPOD 
     *                                        UPRBND,MCCPTR,NUMBRS,      CRTPOD 
     *                                        IFRTYP,ITOTYP)             CRTPOD 
                                   IASFDTL(IASF,IASFPNN) = NODES(NODTYP) CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C ***                                                                *** CRTPOD 
C *** STEP 2A2.4                                                     *** CRTPOD 
C ***           CREATE C-9 ASF NODE (NODE TYPE #7), ARCS FROM        *** MODMCN 
C ***           THE APOD ASF TO THE C-9 SYSTEM (ARC TYPE #9)         *** CRTPOD 
C ***           AND MUTUAL CAPACITY CONSTRAINTS (MCC TYPE #9)        *** MODMCN 
C ***                                                                *** CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
                                      NODTYP = 7                         CRTPOD 
                                      NUMBRS(1) = IASF                   CRTPOD 
                                      NUMBRS(2) = IDAY                   CRTPOD 
                                      CALL WRNODE(NODTYP,REQMNT,NUMBRS)  CRTPOD 
                                      IC9PTR = IC9PTR + 1                CRTPOD 
                                      IC9MSN(IC9PTR,IC9NOD) =            CRTPOD 
     1                                              NODES(NODTYP)        CRTPOD 
C                                                                        CRTPOD 
                                      MCCTYP = 9                         MODMCN 
                                      CPACTY = MAXC9(IDAY)               MODMCN 
                                      CALL WRTMCC(MCCTYP,CPACTY,NUMBRS)  MODMCN 
C                                                                        MODMCN 
                                      NODTYP = 6                         CRTPOD 
                                      IATYPE    = 9                      CRTPOD 
                                      COST      = ARCCIJ(IATYPE)         CRTPOD 
                                      IFROM     = NODES(NODTYP)          CRTPOD 
                                      NUMBRS(1) = IAPOD                  CRTPOD 
                                      NUMBRS(2) = IASF                   CRTPOD 
                                      NUMBRS(3) = IDAY                   CRTPOD 
                                      DO 250 ICAT = 1, MCAT              CRTPOD 
                                             UPRBND(ICAT) =              CRTPOD 
     *                                            ARCUIJ(IATYPE,ICAT)    CRTPOD 
                                             IF(UPRBND(ICAT).EQ.-1.00)   CRTPOD 
     *                                          UPRBND(ICAT)=            CRTPOD 
     *                                               FLOAT(MAXC9(IDAY))  CRTPOD 
                                             MCCPTR(ICAT) = NBRMCC       MODMCN 
  250                                 CONTINUE                           CRTPOD 
                                      IFRTYP = 6                         CRTPOD 
                                      ITOTYP = 7                         CRTPOD 
                                      CALL WRTARC(IATYPE,IFROM,COST,     CRTPOD 
     *                                        UPRBND,MCCPTR,NUMBRS,      CRTPOD 
     *                                        IFRTYP,ITOTYP)             CRTPOD 
                                      MBASE = IASFDTL(IASF,LNKASF)       CRTPOD 
                                      IC9MSN(IC9PTR,LNKC9M) = MBASE      CRTPOD 
                                      IC9MSN(IC9PTR,IC9ONT) = 1          CRTPOD 
                                      IC9MSN(IC9PTR,IC9NDX) = IASF       CRTPOD 
                                      IC9MSN(IC9PTR,IC9AMT) = 0          CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C ***                                                                *** CRTPOD 
C *** STEP 2A2.5                                                     *** CRTPOD 
C ***         IF THE ARRIVAL STATION WITHOUT AN ASF IS SERVED BY C-9 *** CRTPOD 
C ***         CREATE A C-9 APOD NODE (NODE TYPE #8) AND AN ARC FROM  *** CRTPOD 
C ***         THIS NODE TO DISTANT HOSPITALS (ARC TYPE #8).          *** CRTPOD 
C ***         NOTE:  THIS SHOULD NEVER OCCUR.  ALL APODS SHOULD HAVE *** CRTPOD 
C ***                ASFS.                                           *** CRTPOD 
C ***                                                                *** CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
                                   ELSE                                  CRTPOD 
                                      NODTYP = 8                         CRTPOD 
                                      NUMBRS(1) = IAPOD                  CRTPOD 
                                      NUMBRS(2) = IDAY                   CRTPOD 
                                      CALL WRNODE(NODTYP,REQMNT,NUMBRS)  CRTPOD 
                                      IC9PTR = IC9PTR + 1                CRTPOD 
                                      IC9MSN(IC9PTR,IC9NOD) =            CRTPOD 
     1                                              NODES(NODTYP)        CRTPOD 
C                                                                        CRTPOD 
                                      NODTYP = 5                         CRTPOD 
                                      IF(IPODDTL(IAPOD,IPODIND).GE.1)    CRTPOD 
     *                                   THEN                            CRTPOD 
                                             IATYPE    = 8               CRTPOD 
                                             COST      = ARCCIJ(IATYPE)  CRTPOD 
                                             IFROM     = NODES(NODTYP)   CRTPOD 
                                             NUMBRS(1) = IAPOD           CRTPOD 
                                             NUMBRS(2) = IDAY            CRTPOD 
                                             DO 260 ICAT = 1, MCAT       CRTPOD 
                                                    UPRBND(ICAT) =       CRTPOD 
     *                                              ARCUIJ(IATYPE,ICAT)  CRTPOD 
                                             IF(UPRBND(ICAT).EQ.-1.00)   CRTPOD 
     *                                          UPRBND(ICAT)=            CRTPOD 
     *                                               FLOAT(MAXC9(IDAY))  CRTPOD 
                                                    MCCPTR(ICAT) = 0     CRTPOD 
  260                                        CONTINUE                    CRTPOD 
                                             IFRTYP = 5                  CRTPOD 
                                             ITOTYP = 8                  CRTPOD 
                                             CALL WRTARC(IATYPE,IFROM,   CRTPOD 
     *                                            COST,UPRBND,MCCPTR,    CRTPOD 
     *                                            NUMBRS,IFRTYP,ITOTYP)  CRTPOD 
                                             MBASE=IPODDTL(IAPOD,LNKPOD) CRTPOD 
                                             IC9MSN(IC9PTR,LNKC9M)=MBASE CRTPOD 
                                             IC9MSN(IC9PTR,IC9ONT) = 3   CRTPOD 
                                             IC9MSN(IC9PTR,IC9NDX)=IAPOD CRTPOD 
                                             IC9MSN(IC9PTR,IC9AMT) = 0   CRTPOD 
                                      ENDIF                              CRTPOD 
                               ENDIF                                     CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C ***                                                                *** CRTPOD 
C *** STEP 2A3.                                                      *** CRTPOD 
C ***         CREATE ARCS TO LOCAL HOSPITALS (ARC TYPE #16)          *** CRTPOD 
C ***                                                                *** CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
                               IATYPE = 16                               CRTPOD 
C                                                                        CRTPOD 
C                              BECAUSE IDMAT ONLY CONTAINS DISTANCES     CRTPOD 
C                              FROM DOS'S TO C-9 BASES, WE MUST PASS     CRTPOD 
C                              A DOS INDEX TO CRTLCL.                    CRTPOD 
C                                                                        CRTPOD 
                               IDOS = IPODDTL(IAPOD,IPODDOS)             CRTPOD 
                               CALL CRTLCL(IATYPE,NODTYP,IDAY,IDOS,IASF) CRTPOD 
C                                                                        CRTPOD 
C ********************************************************************** CRTPOD 
C *                                                                    * CRTPOD 
C ****************************  E X I T  ******************************* CRTPOD 
C *                                                                    * CRTPOD 
C ********************************************************************** CRTPOD 
C                                                                        CRTPOD 
      RETURN                                                             CRTPOD 
      END                                                                CRTPOD 
