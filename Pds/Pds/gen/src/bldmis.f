      SUBROUTINE BLDMIS(IDAY)                                            BLDMIS 
C                                                                        BLDMIS 
C ********************************************************************** BLDMIS 
C *                                                                    * BLDMIS 
C *********************  D E S C R I P T I O N  ************************ BLDMIS 
C *                                                                    * BLDMIS 
C ********************************************************************** BLDMIS 
C                                                                        BLDMIS 
C     SUBROUTINE BLDMIS CREATES THE SUBNETWORK ASSOCIATED WITH           BLDMIS 
C     EACH MISSION.  IT CREATES AN INTERTHEATER ARRIVAL NODE             BLDMIS 
C     AND AN ARC FROM THE IAP TO THE ARRIVAL NODE.  IF THERE             BLDMIS 
C     IS A SECOND LEG FROM THE ARRIVAL NODE TO ANOTHER STATION           BLDMIS 
C     (TYPICALLY, A DESTINATION ONLOAD STATION), THE ADDITIONAL          BLDMIS 
C     NODES AND ARCS ARE ADDED.  IF THE ARRIVAL STAION IS AN             BLDMIS 
C     AERIAL PORT OF DEBARKATION (APOD) WITH AN AEROMEDICAL              BLDMIS 
C     STAGING FACILITY (ASF), THE APOD ASF IS ADDED.                     BLDMIS 
C                                                                        BLDMIS 
C ********************************************************************** BLDMIS 
C *                                                                    * BLDMIS 
C ***************************  S T A T U S  **************************** BLDMIS 
C *                                                                    * BLDMIS 
C ********************************************************************** BLDMIS 
C                                                                        BLDMIS 
C      ORIGINAL AUTHOR        : MCLAIN                                   BLDMIS 
C      ORIGINAL VERSION DATE  : 12/28/83                                 BLDMIS 
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           BLDMIS 
C                                2/15/85  FORMAL PARAMETERS MOVED TO     BLDMIS 
C                                         COMMON BLOCKS.                 BLDMIS 
C                                2/28/85  MISSION NETWORK REDUCED.       BLDMIS 
C                                                                        BLDMIS 
C ********************************************************************** BLDMIS 
C *                                                                    * BLDMIS 
C ********************  D E C L A R A T I O N S  *********************** BLDMIS 
C *                                                                    * BLDMIS 
C ********************************************************************** BLDMIS 
C                                                                        BLDMIS 
C  ------------------------                                              BLDMIS 
C  ---P A R A M E T E R S .                                              BLDMIS 
C  ------------------------                                              BLDMIS 
C                                                                        BLDMIS 
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
C                                                                        BLDMIS 
C  ----------------------------------                                    BLDMIS 
C  ---L O C A L   V A R I A B L E S .                                    BLDMIS 
C  ----------------------------------                                    BLDMIS 
C                                                                        BLDMIS 
      INTEGER           NODTYP,  NUMBRS,  IWORK,  IDAY,  IATYPE,         BLDMIS 
     *                  IFLAG1,  IFLAG2,  IASF,   IAPOD, IDOS            BLDMIS 
C                                                                        BLDMIS 
      REAL              CPACTY,  ADMIT,  RELEAS, CPACTYL, CPACTYA        BLDMIS 
C                                                                        BLDMIS 
      DIMENSION         NUMBRS(  10),  REQMNT(MCAT),  MCCPTR(MCAT),      BLDMIS 
     *                  UPRBND(MCAT),  IWORK(25)                         BLDMIS 
C                                                                        BLDMIS 
C  ------------------------------------                                  BLDMIS 
C  ---G L O B A L   V A R I A B L E S .                                  BLDMIS 
C  ------------------------------------                                  BLDMIS 
C                                                                        BLDMIS 
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
C                                                                        BLDMIS 
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        BLDMIS 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        BLDMIS 
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        BLDMIS 
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
C                                                                        BLDMIS 
C  ------------------------------                                        BLDMIS 
C  ---C O M M O N   B L O C K S .                                        BLDMIS 
C  ------------------------------                                        BLDMIS 
C                                                                        BLDMIS 
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
C                                                                        BLDMIS 
C  ----------------------------------                                    BLDMIS 
C  ---D A T A   S T A T E M E N T S .                                    BLDMIS 
C  ----------------------------------                                    BLDMIS 
C                                                                        BLDMIS 
      DATA REQMNT /MCAT*0.0/, NUMBRS/10*0/                               BLDMIS 
C                                                                        BLDMIS 
C ********************************************************************** BLDMIS 
C *                                                                    * BLDMIS 
C ************************  E N T R A N C E  *************************** BLDMIS 
C *                                                                    * BLDMIS 
C ********************************************************************** BLDMIS 
C ***                                                                *** BLDMIS 
C *** STEP 1.                                                        *** BLDMIS 
C ***                  *** DO-LOOP ***                               *** BLDMIS 
C ***                                                                *** BLDMIS 
C ***        COUNT THE NUMBER OF MISSIONS OPERATING OVER EACH        *** BLDMIS 
C ***        (IAP,APOD) AND (APOD,DOS) FLIGHT SEGMENT.               *** BLDMIS 
C ***                                                                *** BLDMIS 
C ********************************************************************** BLDMIS 
C                                                                        BLDMIS 
      IQPTR = 1                                                          BLDMIS 
      DO 100 MSNBR  = 1, ICOUNT                                          BLDMIS 
             IQPTR  = IQUEUE(IQPTR, IFWD)                                BLDMIS 
             MSNPTR = IQUEUE(IQPTR, IVAL)                                BLDMIS 
             IAPOD         = IDETAIL(MSNPTR,MSNPOD)                      BLDMIS 
             IAP           = IDETAIL(MSNPTR,MSNIAP)                      BLDMIS 
             IDOS          = IDETAIL(MSNPTR,MSNDOS)                      BLDMIS 
             IF(IAP.GT.0.AND.IAP.LE.MIAPS)                               BLDMIS 
     *          THEN                                                     BLDMIS 
                    ILEG1(1,IAP,IAPOD) = ILEG1(1,IAP,IAPOD    ) + 1      BLDMIS 
                    ILEG1(2,IAP,IAPOD) = IDETAIL(MSNPTR,MSNFT1)          BLDMIS 
                    ILEG1(3,IAP,IAPOD) = IDETAIL(MSNPTR,MSNAC )          BLDMIS 
             ENDIF                                                       BLDMIS 
             IF(IDOS.GT.0.AND.IDOS.LE.MDOS)                              BLDMIS 
     *          THEN                                                     BLDMIS 
                    ILEG2(1,IDOS,IAPOD) = ILEG2(1,IDOS,IAPOD   ) + 1     BLDMIS 
                    ILEG2(2,IDOS,IAPOD) = IDETAIL(MSNPTR,MSNFT2)         BLDMIS 
                    ILEG2(3,IDOS,IAPOD) = IDETAIL(MSNPTR,MSNAC )         BLDMIS 
             ENDIF                                                       BLDMIS 
 100  CONTINUE                                                           BLDMIS 
C                                                                        BLDMIS 
C ********************************************************************** BLDMIS 
C ***                                                                *** BLDMIS 
C *** STEP 2.                                                        *** BLDMIS 
C ***        CREATE INTERTHEATER FLIGHT SEGMENT AND APOD SUBNETWORK. *** BLDMIS 
C ***                                                                *** BLDMIS 
C ********************************************************************** BLDMIS 
C                                                                        BLDMIS 
      DO 200 IAP = 1,MIAPS                                               BLDMIS 
         DO 210 IAPOD = 1,MAPOD                                          BLDMIS 
                IMISSNS = ILEG1(1,IAP,IAPOD)                             BLDMIS 
                IF(IMISSNS.GE.1)                                         BLDMIS 
     *             THEN                                                  BLDMIS 
                       IPLANE  = ILEG1(3,IAP,IAPOD)                      BLDMIS 
                       IFLYTYM = ILEG1(2,IAP,IAPOD)                      BLDMIS 
                       ILITLMT = IACRAFT(IPLANE,MAXLIT)                  BLDMIS 
                       IAMBLMT = IACRAFT(IPLANE,MAXAMB)                  BLDMIS 
C                                                                        BLDMIS 
C ********************************************************************** BLDMIS 
C ***                                                                *** BLDMIS 
C *** STEP 2A1.                                                      *** BLDMIS 
C ***         IF THE APOD HAS NOT BEEN CREATED, CREATE THE           *** BLDMIS 
C ***         INTERTHEATER ARRIVAL NODE (NODE TYPE #5).              *** BLDMIS 
C ***                                                                *** BLDMIS 
C ********************************************************************** BLDMIS 
                       IF(.NOT.(PODXIST(IAPOD)))                         BLDMIS 
     *                    THEN                                           BLDMIS 
                               CALL CRTPOD(IAPOD,IDAY)                   BLDMIS 
                           ELSE                                          BLDMIS 
C                                                                        BLDMIS 
C                              THE APOD HAS ALREADY BEEN CREATED         BLDMIS 
C                              FOR THIS DAY.  STORE ITS NODE NUMBER      BLDMIS 
C                              IN NODES(5).                              BLDMIS 
C                                                                        BLDMIS 
                               NODES(5) = IPODDTL(IAPOD,IPODCHK)         BLDMIS 
                       ENDIF                                             BLDMIS 
C                                                                        BLDMIS 
C ********************************************************************** BLDMIS 
C ***                                                                *** BLDMIS 
C *** STEP 2B.                                                       *** BLDMIS 
C ***        CREATE INTERTHEATER LITTER CAPACITY CONSTRAINT          *** BLDMIS 
C ***        (MCC TYPE #4)                                           *** BLDMIS 
C ***                                                                *** BLDMIS 
C ********************************************************************** BLDMIS 
C                                                                        BLDMIS 
                    IFLAG1 = 0                                           BLDMIS 
                    IFLAG2 = 0                                           BLDMIS 
                    DO 265 ICAT=1,MCAT                                   BLDMIS 
                       IF (LITAMB(ICAT) .EQ. 1)                          BLDMIS 
     *                    THEN                                           BLDMIS 
                             IFLAG1 = 1                                  BLDMIS 
                          ELSE                                           BLDMIS 
                             IFLAG2 = 1                                  BLDMIS 
                       ENDIF                                             BLDMIS 
  265               CONTINUE                                             BLDMIS 
C                                                                        BLDMIS 
                    NUMBRS(1) = IAP                                      MODMCN 
                    NUMBRS(2) = IAPOD                                    MODMCN 
                    NUMBRS(3) = IDAY                                     MODMCN 
C                                                                        MODMCN 
                    IF (IFLAG1 .GE. 1)                                   BLDMIS 
     *                 THEN                                              BLDMIS 
                       MCCTYP    = 4                                     BLDMIS 
                       CPACTYL   = FLOAT(ILITLMT*IMISSNS)                BLDMIS 
                       CALL WRTMCC(MCCTYP,CPACTYL,NUMBRS)                BLDMIS 
                       MCCLTR=NBRMCC                                     BLDMIS 
                    ENDIF                                                BLDMIS 
C                                                                        BLDMIS 
C ********************************************************************** BLDMIS 
C ***                                                                *** BLDMIS 
C *** STEP 2C.                                                       *** BLDMIS 
C ***        CREATE INTERTHEATER AMBULATORY CAPACITY CONSTRAINT      *** BLDMIS 
C ***        (MCC TYPE #5)                                           *** BLDMIS 
C ***                                                                *** BLDMIS 
C ********************************************************************** BLDMIS 
C                                                                        BLDMIS 
                    IF (IFLAG2 .GE. 1)                                   BLDMIS 
     *                 THEN                                              BLDMIS 
                       MCCTYP = 5                                        BLDMIS 
                       CPACTYA= FLOAT(IAMBLMT*IMISSNS)                   BLDMIS 
                       CALL WRTMCC(MCCTYP,CPACTYA,NUMBRS)                BLDMIS 
                       MCCAMB=NBRMCC                                     BLDMIS 
                    ENDIF                                                BLDMIS 
C                                                                        BLDMIS 
C ********************************************************************** BLDMIS 
C ***                                                                *** BLDMIS 
C *** STEP 2C.                                                       *** BLDMIS 
C ***        CREATE INTERTHEATER ARC TO APOD (ARC TYPE #4).          *** BLDMIS 
C ***                                                                *** BLDMIS 
C ********************************************************************** BLDMIS 
C                                                                        BLDMIS 
                       IATYPE    = 4                                     BLDMIS 
                       COST      = FLOAT(IFLYTYM)                        BLDMIS 
                       IFROM     = IAPDTL(IAP,IAPPNN)                    BLDMIS 
                       NUMBRS(1) = IAP                                   BLDMIS 
                       NUMBRS(2) = IAPOD                                 BLDMIS 
                       NUMBRS(3) = IDAY                                  BLDMIS 
                       DO 270 ICAT=1,MCAT                                BLDMIS 
                              UPRBND(ICAT)=ARCUIJ(IATYPE,ICAT)           BLDMIS 
                              IF(LITAMB(ICAT).EQ.1)                      BLDMIS 
     *                           THEN                                    BLDMIS 
                                     IF (UPRBND(ICAT) .EQ. -1.00)        BLDMIS 
     *                                   UPRBND(ICAT) = CPACTYL          BLDMIS 
                                     MCCPTR(ICAT)=MCCLTR                 BLDMIS 
                                 ELSE                                    BLDMIS 
                                     IF (UPRBND(ICAT) .EQ. -1.00)        BLDMIS 
     *                                   UPRBND(ICAT) = CPACTYA          BLDMIS 
                                     MCCPTR(ICAT)=MCCAMB                 BLDMIS 
                              ENDIF                                      BLDMIS 
  270                  CONTINUE                                          BLDMIS 
                    IFRTYP = 3                                           BLDMIS 
                    ITOTYP = 5                                           BLDMIS 
                    CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,NUMBRS,  BLDMIS 
     *                          IFRTYP,ITOTYP)                           BLDMIS 
                ENDIF                                                    BLDMIS 
  210    CONTINUE                                                        BLDMIS 
  200 CONTINUE                                                           BLDMIS 
C ********************************************************************** BLDMIS 
C ***                                                                *** BLDMIS 
C *** STEP 3.                                                        *** BLDMIS 
C ***        IF THE MISSION INCLUDES A SECOND LEG FROM THE INTER-    *** BLDMIS 
C ***        THEATER ARRIVAL POINT TO A DOS, CREATE THE DOS.         *** BLDMIS 
C ***                                                                *** BLDMIS 
C ********************************************************************** BLDMIS 
C                                                                        BLDMIS 
      DO 300 IDOS=1,MDOS                                                 BLDMIS 
             DO 310 IAPOD = 1,MAPOD                                      BLDMIS 
                    IMISSNS = ILEG2(1,IDOS,IAPOD)                        BLDMIS 
                    IF(IMISSNS.GE.1)                                     BLDMIS 
     *                 THEN                                              BLDMIS 
                           IF(.NOT.(PODXIST(IAPOD)))                     BLDMIS 
     *                        THEN                                       BLDMIS 
                                  CALL CRTPOD(IAPOD,IDAY)                BLDMIS 
                              ELSE                                       BLDMIS 
                                  NODES(5) = IPODDTL(IAPOD,IPODCHK)      BLDMIS 
                           ENDIF                                         BLDMIS 
                           CALL CRTDOS(IAPOD,IDOS,IDAY,IMISSNS)          BLDMIS 
                    ENDIF                                                BLDMIS 
  310        CONTINUE                                                    BLDMIS 
  300 CONTINUE                                                           BLDMIS 
C                                                                        BLDMIS 
C ********************************************************************** BLDMIS 
C *                                                                    * BLDMIS 
C ****************************  E X I T  ******************************* BLDMIS 
C *                                                                    * BLDMIS 
C ********************************************************************** BLDMIS 
C                                                                        BLDMIS 
      RETURN                                                             BLDMIS 
      END                                                                BLDMIS 
