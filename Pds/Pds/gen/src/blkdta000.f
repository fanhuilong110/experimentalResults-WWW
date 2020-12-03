      BLOCK DATA                                                         BLKDAT0
C                                                                        BLKDAT0
C ********************************************************************** BLKDAT0
C *                                                                    * BLKDAT0
C *********************  D E S C R I P T I O N  ************************ BLKDAT0
C *                                                                    * BLKDAT0
C ********************************************************************** BLKDAT0
C                                                                        BLKDAT0
C     BLOCK DATA INITIALIZES ALL VARIABLES AND ARRAYS IN NAMED           BLKDAT0
C     COMMON BLOCKS WITH CONSTANT VALUES.  (SUBROUTINE INITAL WILL       BLKDAT0
C     READ IN VARIABLE DATA FROM DISK FILES.)                            BLKDAT0
C                                                                        BLKDAT0
C ********************************************************************** BLKDAT0
C *                                                                    * BLKDAT0
C ***************************  S T A T U S  **************************** BLKDAT0
C *                                                                    * BLKDAT0
C ********************************************************************** BLKDAT0
C                                                                        BLKDAT0
C      ORIGINAL AUTHOR        : MCLAIN                                   BLKDAT0
C      ORIGINAL VERSION DATE  : 12/19/84                                 BLKDAT0
C      REVISIONS              :  2/15/85  DOCUMENTATION ADDED.           BLKDAT0
C                                2/20/85  ASF,APOD VARIABLES ADDED TO    BLKDAT0
C                                         COMMON BLOCKS.                 BLKDAT0
C                                                                        BLKDAT0
C ********************************************************************** BLKDAT0
C *                                                                    * BLKDAT0
C ********************  D E C L A R A T I O N S  *********************** BLKDAT0
C *                                                                    * BLKDAT0
C ********************************************************************** BLKDAT0
C                                                                        BLKDAT0
C  ------------------------                                              BLKDAT0
C  ---P A R A M E T E R S .                                              BLKDAT0
C  ------------------------                                              BLKDAT0
C                                                                        BLKDAT0
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
C                                                                        BLKDAT0
C  ------------------------------------                                  BLKDAT0
C  ---G L O B A L   V A R I A B L E S .                                  BLKDAT0
C  ------------------------------------                                  BLKDAT0
C                                                                        BLKDAT0
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
C                                                                        BLKDAT0
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        BLKDAT0
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        BLKDAT0
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        BLKDAT0
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
C                                                                        BLKDAT0
C  ------------------------------                                        BLKDAT0
C  ---C O M M O N   B L O C K S .                                        BLKDAT0
C  ------------------------------                                        BLKDAT0
C                                                                        BLKDAT0
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
C                                                                        BLKDAT0
C  ----------------------------------                                    BLKDAT0
C  ---D A T A   S T A T E M E N T S .                                    BLKDAT0
C  ----------------------------------                                    BLKDAT0
C                                                                        BLKDAT0
      DATA ICATS   /'MIM','OPG','SGS','SOR','SNS','SMF','SUR','SVP',     MODMCN 
     *                                            'SBN','STH','SCI'/     MODMCN 
      DATA IUNITS                                     /MFILES*   0 /     BLKDAT0
      DATA IDMAND                                     /MCAT  *   0 /     BLKDAT0
      DATA MCCDPF                                     /MCAT  *   0 /     BLKDAT0
      DATA NODES                                      /NTYPEN*   0 /     BLKDAT0
      DATA IARCS                                      /NSIZE *   0 /     BLKDAT0
      DATA NODFMT                                     /MFMTN *  0 /      BLKDAT0
      DATA IACFMT                                     /MFMTA *  0 /      BLKDAT0
      DATA MCCFMT                                     /MFMTM *  0 /      BLKDAT0
      DATA NBRNOD,  NBRMCC,  NBRARC                   /3     *   0 /     BLKDAT0
      DATA NBREAT,  NBRBAT,  NODEFR,  NODETO          /1,2,3,4     /     BLKDAT0
      DATA IASN  ,  IDSN  ,  ICSN                     /3     *   0 /     BLKDAT0
      DATA  IBASES                                        /MBASARY * 0/  BLKDAT0
      DATA  ITAS,    IALTIM,  ITAXI,   MAXRNG,  IPODGT,  IDOSGT,         BLKDAT0
     *      IC9GT,   MAXLIT,  MAXAMB,  MAXTOT    /1,2,3,4,5,6,7,8,9,10/  BLKDAT0
      DATA  LNKC9B,  IC9IND,  IC9POD,  IC9DOS,  IC9ASF      /1,2,3,4,5/  BLKDAT0
      DATA IPRHSP,  IC9BAS                            /1,2         /     BLKDAT0
      DATA  LNKIAP,  IAPSMX,  IAPTMX,  IAPPNN,  IAPPDY   /1, 2, 3, 4, 5/ BLKDAT0
      DATA  LNKPOD,  IPODIND, IPODASF, IPODC9B, IPODDOS  /1, 2, 3, 4, 5/ BLKDAT0
      DATA  LNKASF,  IASFMX,  IASFPOD, IASFDOS, IASFC9B,                 BLKDAT0
     *               IASFPNN, IASFPDY, IASFPTM  /1, 2, 3, 4, 5, 6, 7, 8/ BLKDAT0
      DATA  LNKDOS,  IDOSIND, IDOSASF, IDOSC9B, IDOSPOD  /1, 2, 3, 4, 5/ BLKDAT0
      DATA INDIAP,  INDPOD,  INDASF,  INDC9B,  INDDOS,                   BLKDAT0
     *              LATDEG,  LATMIN,  LNGDEG,  LNGMIN,                   BLKDAT0
     *              MAXSTG,  MAXTRX,  MAXASF,  NDXIAP,                   BLKDAT0
     *              NDXPOD,  NDXASF,  NDXC9B,  NDXDOS /1,2,3,4,5,6,7,8,  BLKDAT0
     *                                                9,10,11,12,13,14,  BLKDAT0
     *                                                15,16,17/          BLKDAT0
      DATA            MSNITA,  MSNITD,  MSNDDY,  MSNDTM,                 BLKDAT0
     *                MSNADY,  MSNATM,  MSNFT1,  MSNSDY,                 BLKDAT0
     *               MSNSTM,  MSNONL,  MSNODY,  MSNOTM,                  BLKDAT0
     *                  MSNTYP /1,2,3,4,6,7,5,8,9,10,11,12,13/           BLKDAT0
      DATA            MSNGTM,  MSNFT2,  MSNAC ,  MSNSEQ,                 BLKDAT0
     *                MSNTOT,  MSNIAP,  MSNPOD,  MSNDOS,                 BLKDAT0
     *                LMTLIT,  LMTAMB,  LMTTOT                           BLKDAT0
     *                /14,15,16,17,18,19,20,21,22,23,24/                 BLKDAT0
      DATA  IVAL  ,  IFWD  ,  IBCK                    /1,2,3       /     BLKDAT0
      DATA  ISUC  ,  IPRD  ,  NBRMSN,  HEADG          /2,3,0,0     /     BLKDAT0
      DATA  HSPTBD                                    /NHOSDAY * 0 /     BLKDAT0
      DATA  LINES  /'  1-30  ',' 31-60  ',' 61-90  ',                    BLKDAT0
     *              ' LENGTH ','OF STAY ','CATEGORY','1=LITTER'/         BLKDAT0
      DATA  LOS                                            /MCAT * 0/    BLKDAT0
      DATA  LITAMB                                         /MCAT * 0/    BLKDAT0
      DATA  ITCBP                                          /   9 * 0/    BLKDAT0
      DATA  MAXC9                                         /MDAYS * 0/    BLKDAT0
      DATA  ARCCIJ                                       /NTYPEA * 0/    BLKDAT0
      DATA  NOMORE                                         /       0/    BLKDAT0
      DATA  IASFCHK,  IPODCHK,  IDOSCHK                    /9, 6, 6 /    BLKDAT0
      DATA  LNKC9M,  IC9ONT,  IC9NDX,  IC9AMT,  IC9NOD  /1, 2, 3, 4, 5/  BLKDAT0
      DATA  NDOSCUM                                         /MDOS * 0/   BLKDAT0
C                                                                        BLKDAT0
C ********************************************************************** BLKDAT0
C *                                                                    * BLKDAT0
C ****************************  E X I T  ******************************* BLKDAT0
C *                                                                    * BLKDAT0
C ********************************************************************** BLKDAT0
C                                                                        BLKDAT0
      END                                                                BLKDAT0
