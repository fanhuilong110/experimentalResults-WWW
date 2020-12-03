      SUBROUTINE INIT                                                    INIT   
C                                                                        INIT   
C ********************************************************************** INIT   
C *                                                                    * INIT   
C *********************  D E S C R I P T I O N  ************************ INIT   
C *                                                                    * INIT   
C ********************************************************************** INIT   
C                                                                        INIT   
C     SUBROUTINE INIT INITIALIZES THE LINKED LIST AND MISSION            INIT   
C     DETAIL MATRIX, AND RESETS ALL POINTERS AND COUNTERS.               INIT   
C                                                                        INIT   
C ********************************************************************** INIT   
C *                                                                    * INIT   
C ***************************  S T A T U S  **************************** INIT   
C *                                                                    * INIT   
C ********************************************************************** INIT   
C                                                                        INIT   
C      ORIGINAL AUTHOR        : MCLAIN                                   INIT   
C      ORIGINAL VERSION DATE  : 2/27/85                                  INIT   
C                                                                        INIT   
C ********************************************************************** INIT   
C *                                                                    * INIT   
C ********************  D E C L A R A T I O N S  *********************** INIT   
C *                                                                    * INIT   
C ********************************************************************** INIT   
C                                                                        INIT   
C  ------------------------                                              INIT   
C  ---P A R A M E T E R S .                                              INIT   
C  ------------------------                                              INIT   
C                                                                        INIT   
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
C                                                                        INIT   
C  ----------------------------------                                    INIT   
C  ---L O C A L   V A R I A B L E S .                                    INIT   
C  ----------------------------------                                    INIT   
C                                                                        INIT   
      INTEGER          I     ,  J                                        INIT   
C                                                                        INIT   
C  ------------------------------------                                  INIT   
C  ---G L O B A L   V A R I A B L E S .                                  INIT   
C  ------------------------------------                                  INIT   
C                                                                        INIT   
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
C                                                                        INIT   
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        INIT   
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        INIT   
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        INIT   
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
C                                                                        INIT   
C  ------------------------------                                        INIT   
C  ---C O M M O N   B L O C K S .                                        INIT   
C  ------------------------------                                        INIT   
C                                                                        INIT   
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
C                                                                        INIT   
C ********************************************************************** INIT   
C *                                                                    * INIT   
C ************************  E N T R A N C E  *************************** INIT   
C *                                                                    * INIT   
C ********************************************************************** INIT   
C ***                                                                *** INIT   
C ***   RESET EVENT COUNTERS.                                        *** INIT   
C ***                                                                *** INIT   
C ********************************************************************** INIT   
      MSNNBR = 0                                                         INIT   
      ICOUNT = 0                                                         INIT   
C ********************************************************************** INIT   
C ***                                                                *** INIT   
C ***   SET LINKED LIST AND FREE LIST POINTERS TO SHOW LIST IS EMPTY.*** INIT   
C ***                                                                *** INIT   
C ********************************************************************** INIT   
      IQUEUE(1,IFWD) = 1                                                 INIT   
      IQUEUE(1,IBCK) = 1                                                 INIT   
      HEADG          = 2                                                 INIT   
C ********************************************************************** INIT   
C ***                                                                *** INIT   
C ***   RESET ALL FORWARD AND BACKWARD POINTERS IN FREE LIST.        *** INIT   
C ***                                                                *** INIT   
C ********************************************************************** INIT   
      DO 100 I = 2,300                                                   INIT   
             IQUEUE(I,IFWD) = I+1                                        INIT   
             IQUEUE(I,IBCK) = I-1                                        INIT   
  100 CONTINUE                                                           INIT   
      IQUEUE(300,IFWD) = 1                                               INIT   
C ********************************************************************** INIT   
C ***                                                                *** INIT   
C ***   CLEAR C-141 MISSION DETAIL MATRIX.                           *** INIT   
C ***                                                                *** INIT   
C ********************************************************************** INIT   
      DO 110 I=1,300                                                     INIT   
              DO 120 J=1,MEND                                            INIT   
                     IDETAIL(I,J)=0                                      INIT   
  120         CONTINUE                                                   INIT   
  110 CONTINUE                                                           INIT   
C ***   CLEAR C-9 MISSION DETAIL MATRIX.                             *** INIT   
C ***                                                                *** INIT   
C ***   CLEAR MISSION DETAIL MATRIX.                                 *** INIT   
C ***                                                                *** INIT   
C ********************************************************************** INIT   
      IC9PTR = 0                                                         INIT   
      DO 200 I = 1, MC9MSN                                               INIT   
             DO 210 J = 1,5                                              INIT   
                    IC9MSN(I,J) = 0                                      INIT   
  210        CONTINUE                                                    INIT   
  200 CONTINUE                                                           INIT   
      DO 220 I = 1, MDOS                                                 INIT   
             NDOSDAY(I) = 0                                              INIT   
             IDOSDTL(I,IDOSCHK) = 0                                      INIT   
             DOSXIST(I)         = .FALSE.                                INIT   
             DO 225 J = 1,MAPOD                                          INIT   
                    DO 226 K = 1,3                                       INIT   
                           ILEG2(K,I,J) = 0                              INIT   
  226               CONTINUE                                             INIT   
  225        CONTINUE                                                    INIT   
  220 CONTINUE                                                           INIT   
      DO 230 I = 1, MASF                                                 INIT   
             IASFDTL(I,IASFCHK) = 0                                      INIT   
             ASFXIST(I)         = .FALSE.                                INIT   
  230 CONTINUE                                                           INIT   
      DO 240 I = 1, MAPOD                                                INIT   
             IPODDTL(I,IPODCHK) = 0                                      INIT   
             PODXIST(I)         = .FALSE.                                INIT   
             DO 250 J = 1,MIAPS                                          INIT   
                    DO 260 K = 1,3                                       INIT   
                           ILEG1(K,J,I) = 0                              INIT   
  260               CONTINUE                                             INIT   
  250        CONTINUE                                                    INIT   
  240 CONTINUE                                                           INIT   
C                                                                        INIT   
C ********************************************************************** INIT   
C *                                                                    * INIT   
C ****************************  E X I T  ******************************* INIT   
C *                                                                    * INIT   
C ********************************************************************** INIT   
C                                                                        INIT   
      RETURN                                                             INIT   
      END                                                                INIT   
