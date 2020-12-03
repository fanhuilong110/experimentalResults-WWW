      SUBROUTINE CRTDOS(IAPOD,IDOS,IDAY,IMISSNS)                         CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C *                                                                    * CRTDOS 
C *********************  D E S C R I P T I O N  ************************ CRTDOS 
C *                                                                    * CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
C     SUBROUTINE CRTDOS (CREATE DOS) CREATES THE NODES AND ARCS          CRTDOS 
C     THRU WHICH PATIENTS FLOW FROM THE APOD TO THE MISSION              CRTDOS 
C     DESTINATION ONLOAD STATION (DOS).  IF THE DOS HAS AN ASF,          CRTDOS 
C     AN ARC IS CREATED FOR PATIENT ADMISSION; OTHERWISE, ARCS           CRTDOS 
C     LINK DIRECTLY INTO A HOSPITAL OR INTO THE C-9 SYSTEM.              CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C *                                                                    * CRTDOS 
C ***************************  S T A T U S  **************************** CRTDOS 
C *                                                                    * CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
C      ORIGINAL AUTHOR        : MCLAIN                                   CRTDOS 
C      ORIGINAL VERSION DATE  : 12/28/83                                 CRTDOS 
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           CRTDOS 
C                                2/15/85  FORMAL PARAMETERS MOVED TO     CRTDOS 
C                                         COMMON BLOCKS.                 CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C *                                                                    * CRTDOS 
C ********************  D E C L A R A T I O N S  *********************** CRTDOS 
C *                                                                    * CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
C  ------------------------                                              CRTDOS 
C  ---P A R A M E T E R S .                                              CRTDOS 
C  ------------------------                                              CRTDOS 
C                                                                        CRTDOS 
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
C                                                                        CRTDOS 
C  ----------------------------------                                    CRTDOS 
C  ---L O C A L   V A R I A B L E S .                                    CRTDOS 
C  ----------------------------------                                    CRTDOS 
C                                                                        CRTDOS 
      INTEGER           IDAY  , NUMBRS,  MCCPTR,  IDOS,                  CRTDOS 
     *                  IFLAG1, IFLAG2                                   CRTDOS 
C                                                                        CRTDOS 
      REAL              REQMNT, UPRBND, DIST, CPACTY, CPACTYL, CPACTYA   CRTDOS 
C                                                                        CRTDOS 
      DIMENSION   NUMBRS(  10),     REQMNT(MCAT),     UPRBND(MCAT),      CRTDOS 
     *            MCCPTR(MCAT)                                           CRTDOS 
C                                                                        CRTDOS 
C                                                                        CRTDOS 
C  ------------------------------------                                  CRTDOS 
C  ---G L O B A L   V A R I A B L E S .                                  CRTDOS 
C  ------------------------------------                                  CRTDOS 
C                                                                        CRTDOS 
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
C                                                                        CRTDOS 
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        CRTDOS 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        CRTDOS 
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        CRTDOS 
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
C                                                                        CRTDOS 
C  ------------------------------                                        CRTDOS 
C  ---C O M M O N   B L O C K S .                                        CRTDOS 
C  ------------------------------                                        CRTDOS 
C                                                                        CRTDOS 
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
C                                                                        CRTDOS 
C  ----------------------------------                                    CRTDOS 
C  ---D A T A   S T A T E M E N T S .                                    CRTDOS 
C  ----------------------------------                                    CRTDOS 
C                                                                        CRTDOS 
      DATA  REQMNT /MCAT * 0.0/,  NUMBRS /10 * 0/                        CRTDOS 
C                                                                        CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C *                                                                    * CRTDOS 
C ************************  E N T R A N C E  *************************** CRTDOS 
C *                                                                    * CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A.                                                       *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
      IPLANE  = ILEG2(3,IDOS,IAPOD)                                      CRTDOS 
      IFLYTYM = ILEG2(2,IDOS,IAPOD)                                      CRTDOS 
      ILITLMT = IACRAFT(IPLANE,MAXLIT)                                   CRTDOS 
      IAMBLMT = IACRAFT(IPLANE,MAXAMB)                                   CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A1.                                                      *** CRTDOS 
C ***          IF DOS NODE HAS NOT BEEN CREATED ON IDAY,             *** CRTDOS 
C ***          CREATE DOS NODE (NODE TYPE #10).                      *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
      IF(.NOT.(DOSXIST(IDOS)))                                           CRTDOS 
     *    THEN                                                           CRTDOS 
              NUMBRS(1) = IDOS                                           CRTDOS 
              NUMBRS(2) = IDAY                                           CRTDOS 
              NUMBRS(3) = 0                                              CRTDOS 
              NODTYP    = 10                                             CRTDOS 
              CALL WRNODE(NODTYP,REQMNT,NUMBRS)                          CRTDOS 
              DOSXIST(IDOS) = .TRUE.                                     CRTDOS 
              IDOSDTL(IDOS,IDOSCHK) = NBRNOD                             CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A2.                                                      *** CRTDOS 
C ***         IF THE DOS HAS A STAGING FACILITY (ASF), ADD AN ASF    *** CRTDOS 
C ***         NODE, A CAPACITY ARC, AND AN ARC FROM THE PREVIOUS ASF *** CRTDOS 
C ***         RELEASE NODE (IF ONE EXISTS).                          *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
              IF(IDOSDTL(IDOS,IDOSIND).EQ.1)                             CRTDOS 
     *           THEN                                                    CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A2.1                                                     *** CRTDOS 
C ***             CREATE DOS ASF NODE (NODE TYPE #11).               *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
                     NODTYP    = 11                                      CRTDOS 
                     IASF = IDOSDTL(IDOS,IDOSASF)                        CRTDOS 
                     NUMBRS(1) = IASF                                    CRTDOS 
                     NUMBRS(2) = IDOS                                    CRTDOS 
                     NUMBRS(3) = IDAY                                    CRTDOS 
                     CALL WRNODE(NODTYP,REQMNT,NUMBRS)                   CRTDOS 
                     IASFDTL(IASF,IASFCHK) = NBRNOD                      CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A2.2                                                     *** CRTDOS 
C ***           CREATE DOS -TO- ASF ARCS (ARC TYPE #14).             *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
                     IATYPE    = 14                                      CRTDOS 
                     NUMBRS(1) = IDOS                                    CRTDOS 
                     NUMBRS(2) = IASF                                    CRTDOS 
                     NUMBRS(3) = IDAY                                    CRTDOS 
                     COST      = ARCCIJ(IATYPE)                          CRTDOS 
                     IARCPT    = IARCS(IATYPE,NODEFR)                    CRTDOS 
                     IFROM     = NODES(IARCPT)                           CRTDOS 
                     DO 300 ICAT=1,MCAT                                  CRTDOS 
                            UPRBND(ICAT) = ARCUIJ(IATYPE,ICAT)           CRTDOS 
                            MCCPTR(ICAT) = 0                             CRTDOS 
  300                CONTINUE                                            CRTDOS 
                     IFRTYP = 10                                         CRTDOS 
                     ITOTYP = 11                                         CRTDOS 
                     CALL WRTARC (IATYPE,IFROM,COST,UPRBND,MCCPTR,       CRTDOS 
     *                            NUMBRS,IFRTYP,ITOTYP)                  CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A2.3                                                     *** CRTDOS 
C ***           IF A PREVIOUS ASF NODE EXISTS:                       *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
                     IF(IASFDTL(IASF,IASFPNN).GT.0)                      CRTDOS 
     *                  THEN                                             CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A2.3.1                                                   *** CRTDOS 
C ***             CREATE DOS ASF CAPACITY CONSTRAINT (MCC TYPE #6)   *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
                            MCCTYP    = 6                                CRTDOS 
                            NUMBRS(1) = IASF                             CRTDOS 
                            NUMBRS(2) = IDAY                             CRTDOS 
                            CPACTY    = IASFDTL(IASF,IASFMX)             CRTDOS 
                            CALL WRTMCC(MCCTYP,CPACTY,NUMBRS)            CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A2.3.2                                                   *** CRTDOS 
C ***             CREATE DOS ASF CAPACITY ARCS (ARC TYPE #15)        *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
                            IATYPE    = 15                               CRTDOS 
                            COST      = ARCCIJ(IATYPE)                   CRTDOS 
                            IFROM     = IASFDTL(IASF,IASFPNN)            CRTDOS 
                            NUMBRS(1) = IASF                             CRTDOS 
                            NUMBRS(2) = IDOS                             CRTDOS 
                            NUMBRS(3) = IDAY                             CRTDOS 
                            DO 310 ICAT=1,MCAT                           CRTDOS 
                                   UPRBND(ICAT) = ARCUIJ(IATYPE,ICAT)    CRTDOS 
                                     IF (UPRBND(ICAT) .EQ. -1.00)        CRTDOS 
     *                                   UPRBND(ICAT) = CPACTY           CRTDOS 
                                   MCCPTR(ICAT) = NBRMCC                 CRTDOS 
  310                       CONTINUE                                     CRTDOS 
                            IFRTYP = 11                                  CRTDOS 
                            ITOTYP = 10                                  CRTDOS 
                            CALL WRTARC (IATYPE,IFROM,COST,UPRBND,       CRTDOS 
     *                                   MCCPTR,NUMBRS,IFRTYP,ITOTYP)    CRTDOS 
                     ENDIF                                               CRTDOS 
                     IASFDTL(IASF,IASFPNN) = NODES(NODTYP)               CRTDOS 
                     IASFDTL(IASF,IASFPDY) = IDAY                        CRTDOS 
                     IASFDTL(IASF,IASFPTM) = IDAY*1440                   CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A2.4                                                     *** CRTDOS 
C ***             CREATE A C-9 ASF NODE (NODE TYPE #7), A MUTUAL     *** MODMCN 
C ***             CAPACITY CONSTRAINT (MCC TYPE #9), AND ARCS        *** MODMCN 
C ***             FROM THE ASF TO THE C-9 SYSTEM (ARC TYPE #29)      *** MODMCN 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
                     NODTYP = 7                                          CRTDOS 
                     NUMBRS(1) = IASF                                    CRTDOS 
                     NUMBRS(2) = IDAY                                    CRTDOS 
                     CALL WRNODE (NODTYP,REQMNT,NUMBRS)                  CRTDOS 
                     IC9PTR = IC9PTR + 1                                 CRTDOS 
                     IC9MSN(IC9PTR,IC9NOD) = NODES(NODTYP)               CRTDOS 
C                                                                        CRTDOS 
                     MCCTYP = 9                                          MODMCN 
                     CPACTY = MAXC9(IDAY)                                MODMCN 
                     CALL WRTMCC(MCCTYP,CPACTY,NUMBRS)                   MODMCN 
C                                                                        MODMCN 
                     NODTYP = 11                                         CRTDOS 
                     IATYPE = 29                                         CRTDOS 
                     COST      = ARCCIJ(IATYPE)                          CRTDOS 
                     IFROM     = NODES(NODTYP)                           CRTDOS 
                     DO 320 ICAT = 1, MCAT                               CRTDOS 
                            UPRBND(ICAT) = ARCUIJ(IATYPE,ICAT)           CRTDOS 
                            IF (UPRBND(ICAT) .EQ. -1.00)                 CRTDOS 
     *                          UPRBND(ICAT) = FLOAT(MAXC9(IDAY))        CRTDOS 
                            MCCPTR(ICAT) = NBRMCC                        MODMCN 
  320                CONTINUE                                            CRTDOS 
                     NUMBRS(1) = IDOS                                    CRTDOS 
                     NUMBRS(2) = IASF                                    CRTDOS 
                     NUMBRS(3) = IDAY                                    CRTDOS 
                     IFRTYP = 11                                         CRTDOS 
                     ITOTYP =  7                                         CRTDOS 
                     CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,        CRTDOS 
     *                           NUMBRS,IFRTYP,ITOTYP)                   CRTDOS 
                     IC9MSN(IC9PTR,LNKC9M) = IDOSDTL(IDOS,LNKDOS)        CRTDOS 
                     IC9MSN(IC9PTR,IC9ONT) = 1                           CRTDOS 
                     IC9MSN(IC9PTR,IC9NDX) = IASF                        CRTDOS 
                     IC9MSN(IC9PTR,IC9AMT) = 0                           CRTDOS 
                 ELSE                                                    CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A2.5                                                     *** CRTDOS 
C ***           IF THE DOS WITHOUT AN ASF IS SERVED BY C-9,          *** CRTDOS 
C ***           CREATE A C-9 DOS NODE (NODE TYPE #9), A MUTUAL       *** MODMCN 
C ***           CAPACITY CONSTRAINT (MCC TYPE #10), AND ARCS TO      *** MODMCN 
C ***           THE C-9 SYSTEM (ARC TYPE #19)                        *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
                     MBASE  = IDOSDTL(IDOS,LNKDOS)                       CRTDOS 
                     IF (IBASES(MBASE,INDC9B) .GT. 0)                    CRTDOS 
     *                   THEN                                            CRTDOS 
                             NODTYP = 9                                  CRTDOS 
                             NUMBRS(1) = IDOS                            CRTDOS 
                             NUMBRS(2) = IDAY                            CRTDOS 
                             CALL WRNODE(NODTYP,REQMNT,NUMBRS)           CRTDOS 
                             IC9PTR = IC9PTR + 1                         CRTDOS 
                             IC9MSN(IC9PTR,IC9NOD) = NODES(NODTYP)       CRTDOS 
C                                                                        CRTDOS 
                             MCCTYP = 10                                 MODMCN 
                             CPACTY = MAXC9(IDAY)                        MODMCN 
                             CALL WRTMCC(MCCTYP,CPACTY,NUMBRS)           MODMCN 
C                                                                        MODMCN 
                             NODTYP = 10                                 CRTDOS 
                             IATYPE    = 19                              CRTDOS 
                             COST      = ARCCIJ(IATYPE)                  CRTDOS 
                             IFROM     = NODES(NODTYP)                   CRTDOS 
                             DO 330 ICAT = 1, MCAT                       CRTDOS 
                                    UPRBND(ICAT) = ARCUIJ(IATYPE,ICAT)   CRTDOS 
                                    IF (UPRBND(ICAT) .EQ. -1.00)         CRTDOS 
     *                                UPRBND(ICAT) = FLOAT(MAXC9(IDAY))  CRTDOS 
                                    MCCPTR(ICAT) = NBRMCC                MODMCN 
  330                        CONTINUE                                    CRTDOS 
                             IFRTYP = 10                                 CRTDOS 
                             ITOTYP =  9                                 CRTDOS 
                             CALL WRTARC(IATYPE,IFROM,COST,UPRBND,       CRTDOS 
     *                                   MCCPTR,NUMBRS,IFRTYP,ITOTYP)    CRTDOS 
                             IC9MSN(IC9PTR,LNKC9M) = MBASE               CRTDOS 
                             IC9MSN(IC9PTR,IC9ONT) = 2                   CRTDOS 
                             IC9MSN(IC9PTR,IC9NDX) = IDOS                CRTDOS 
                             IC9MSN(IC9PTR,IC9AMT) = 0                   CRTDOS 
                     ENDIF                                               CRTDOS 
               ENDIF                                                     CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3A3                                                       *** CRTDOS 
C ***          CREATE ARCS TO LOCAL HOSPITAL(S) (ARC TYPE # 16).     *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
              IATYPE = 16                                                CRTDOS 
              CALL CRTLCL(IATYPE,NODTYP,IDAY,IDOS,IASF)                  CRTDOS 
          ELSE                                                           CRTDOS 
C                                                                        CRTDOS 
C             THE DOS HAS ALREADY BEEN CREATED FOR THIS DAY.             CRTDOS 
C             STORE ITS NODE NUMBER IN NODES(10).                        CRTDOS 
C                                                                        CRTDOS 
              NODES(10) = IDOSDTL(IDOS,IDOSCHK)                          CRTDOS 
      ENDIF                                                              CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3B.                                                       *** CRTDOS 
C ***        CREATE LITTER CAPACITY CONSTRAINT (MCC TYPE #11)        *** MODMCN 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
                    IFLAG1 = 0                                           CRTDOS 
                    IFLAG2 = 0                                           CRTDOS 
                    DO 265 ICAT=1,MCAT                                   CRTDOS 
                       IF (LITAMB(ICAT) .EQ. 1)                          CRTDOS 
     *                    THEN                                           CRTDOS 
                             IFLAG1 = 1                                  CRTDOS 
                          ELSE                                           CRTDOS 
                             IFLAG2 = 1                                  CRTDOS 
                       ENDIF                                             CRTDOS 
  265               CONTINUE                                             CRTDOS 
C                                                                        CRTDOS 
                    NUMBRS(1) = IAPOD                                    MODMCN 
                    NUMBRS(2) = IDOS                                     MODMCN 
                    NUMBRS(3) = IDAY                                     MODMCN 
C                                                                        MODMCN 
                    IF (IFLAG1 .GE. 1)                                   CRTDOS 
     *                 THEN                                              CRTDOS 
                           MCCTYP = 11                                   MODMCN 
                           CPACTYL   = FLOAT(ILITLMT*IMISSNS)            MODMCN 
                           CALL WRTMCC(MCCTYP,CPACTYL,NUMBRS)            MODMCN 
                           MCCLTR=NBRMCC                                 MODMCN 
                    ENDIF                                                CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3C.                                                       *** CRTDOS 
C ***        CREATE AMBULATORY CAPACITY CONSTRAINT (MCC TYPE #12)    *** MODMCN 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
                    IF (IFLAG2 .GE. 1)                                   CRTDOS 
     *                 THEN                                              CRTDOS 
                           MCCTYP = 12                                   MODMCN 
                           CPACTYA = FLOAT(IAMBLMT*IMISSNS)              MODMCN 
                           CALL WRTMCC(MCCTYP,CPACTYA,NUMBRS)            MODMCN 
                           MCCAMB=NBRMCC                                 MODMCN 
                    ENDIF                                                CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C ***                                                                *** CRTDOS 
C *** STEP 3D.                                                       *** CRTDOS 
C ***        CREATE APOD-TO-DOS ARC (ARC TYPE #13).                  *** CRTDOS 
C ***                                                                *** CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
      IATYPE = 13                                                        CRTDOS 
      COST   = FLOAT(IFLYTYM)                                            CRTDOS 
      IARCPT = IARCS(IATYPE,NODEFR)                                      CRTDOS 
      IFROM  = NODES(IARCPT)                                             CRTDOS 
      NUMBRS(1) = IAPOD                                                  CRTDOS 
      NUMBRS(2) = IDOS                                                   CRTDOS 
      NUMBRS(3) = IDAY                                                   CRTDOS 
      DO 400 ICAT=1,MCAT                                                 CRTDOS 
             UPRBND(ICAT)=ARCUIJ(IATYPE,ICAT)                            CRTDOS 
             IF(LITAMB(ICAT).EQ.1)                                       CRTDOS 
     *          THEN                                                     CRTDOS 
                    IF (UPRBND(ICAT) .EQ. -1.00)                         CRTDOS 
     *                  UPRBND(ICAT) = CPACTYL                           CRTDOS 
                    MCCPTR(ICAT)=MCCLTR                                  CRTDOS 
                ELSE                                                     CRTDOS 
                    IF (UPRBND(ICAT) .EQ. -1.00)                         CRTDOS 
     *                  UPRBND(ICAT) = CPACTYA                           CRTDOS 
                    MCCPTR(ICAT)=MCCAMB                                  CRTDOS 
             ENDIF                                                       CRTDOS 
  400 CONTINUE                                                           CRTDOS 
      IFRTYP =  5                                                        CRTDOS 
      ITOTYP = 10                                                        CRTDOS 
      CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,NUMBRS,IFRTYP,ITOTYP)  CRTDOS 
C                                                                        CRTDOS 
C ********************************************************************** CRTDOS 
C *                                                                    * CRTDOS 
C ****************************  E X I T  ******************************* CRTDOS 
C *                                                                    * CRTDOS 
C ********************************************************************** CRTDOS 
C                                                                        CRTDOS 
      RETURN                                                             CRTDOS 
      END                                                                CRTDOS 
