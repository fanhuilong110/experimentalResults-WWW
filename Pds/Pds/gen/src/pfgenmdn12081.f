      PROGRAM PFGEN							 MDN1208     1                                        
C                                                                        PFGEN  
C PFGEN WAS DEVELOPED BY    LT COL DENNIS R MCLAIN                       PFGEN  
C                           CAPT ROBERT P CHMIELEWSKI                    PFGEN  
C                           CAPT MATTHEW L DURCHHOLZ                     PFGEN  
C                           OPERATIONS RESEARCH DIVISION                 PFGEN  
C                           DIRECTORATE OF STUDIES AND ANALYSIS          PFGEN  
C                           DCS/PLANS                                    PFGEN  
C                           HQ MILITARY AIRLIFT COMMAND                  PFGEN  
C                           SCOTT AFB IL 62225                           PFGEN  
C                           (618)-256-5560                               PFGEN  
C                                                                        PFGEN  
C                                                                        PFGEN  
C VERSION 1.0, 1 DECEMBER 1983                                           PFGEN  
C                                                                        PFGEN  
C   PFGEN (PATIENT FLOW GENERATOR) IS DESIGNED TO GENERATE               PFGEN  
C   THE INTERTHEATER CASUALTY EVACUATION NETWORK.  IT IS                 PFGEN  
C   INTENDED FOR USE WITH A MULTICOMMODITY MINIMUM COST                  PFGEN  
C   NETWORK FLOW ALGORITHM DEVELOPED BY THE SOUTHERN                     PFGEN  
C   METHODIST UNIVERSITY OPERATIONS RESEARCH DEPARTMENT.                 PFGEN  
C                                                                        PFGEN  
C VERSION 2.0, 17 MAY 1985                                               PFGEN  
C                                                                        PFGEN  
C   VERSION 2.0 INCORPORATES EXTENSIVE CHANGES TO THE WAY THE NETWORK    PFGEN  
C   IS GENERATED.  SEVERAL ARC AND NODE TYPES ARE ELIMINATED, AND THE    PFGEN  
C   CAPABILITY TO ROUTE PATIENTS STILL IN THE SYSTEM ("IN-PROCESS")      PFGEN  
C   IS ADDED.                                                            PFGEN  
C                                                                        PFGEN  
C VERSION 3.0, 17 JULY 1985                                              PFGEN  
C                                                                        PFGEN  
C   VERSION 3.0 GENERATES THE SIMPLIFIED NETWORK FOR OPTIMIZATION. IT    PFGEN  
C   DOES NOT INCLUDE SPECIFIC MISSION DETAIL.  ALL OTHER ENTITIES IN     PFGEN  
C   THE NETWORK ARE CREATED.                                             PFGEN  
C                                                                        PFGEN  
C    CURRENT CODE                                                        PFGEN  
C OLD PROGRAM LIB: SMUPL7                                                PFGEN  
C NEW PROGRAM LIB: SMUPL8  23 AUGUST 1985, 10 SEP 1985                   PFGEN  
C LATEST MODSET  : SMUMOD9 23 AUGUST 1985, 10 SEP 1985                   PFGEN  
C SOURCE CODE    : NETSRC  23 AUGUST 1985, 10 SEP 1985                   PFGEN  
C BINARY CODE    : NETBIN  23 AUGUST 1985                                PFGEN  
C                                                                        PFGEN  
C ********************************************************************** PFGEN  
C *                                                                    * PFGEN  
C ********************** D E C L A R A T I O N S *********************** PFGEN  
C *                                                                    * PFGEN  
C ********************************************************************** PFGEN  
C                                                                        PFGEN  
C -----------------------                                                PFGEN  
C ---P A R A M E T E R S.                                                PFGEN  
C -----------------------                                                PFGEN  
C                                                                        PFGEN  
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
C                                                                        PFGEN  
C ------------------------------------                                   PFGEN  
C ---G L O B A L   V A R I A B L E S .                                   PFGEN  
C ------------------------------------                                   PFGEN  
C                                                                        PFGEN  
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
C                                                                        PFGEN  
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        PFGEN  
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        PFGEN  
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        PFGEN  
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
C                                                                        PFGEN  
C -----------------------------                                          PFGEN  
C ---C O M M O N   B L O C K S.                                          PFGEN  
C -----------------------------                                          PFGEN  
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
C                                                                        PFGEN  
      open (unit=2, file='patdatx', status='old')
      open (unit=3, file='bednfmx', status='old')
      open (unit=4, file='basesx', status='old')
      open (unit=5, file='acraft', status='old')
      open (unit=16, file='netwrk4', status='old')
      open (unit=7, file='sgenms', status='old')
      open (unit=8, file='nodes', status='unknown')
      open (unit=9, file='arcs', status='unknown')
      open (unit=10, file='mutual', status='unknown')
      open (unit=88, file='sumary', status='unknown')
      open (unit=99, file='errer', status='unknown')

      PRINT 1000                                                         PFGEN  
C                                                                        PFGEN  
      CALL INITAL                                                        PFGEN  
      PRINT 1010                                                         PFGEN  
C                                                                        PFGEN  
C                                                                        PFGEN  
      CALL BILDER                                                        PFGEN  
C                                                                        PFGEN  
      PRINT 1020                                                         PFGEN  
C                                                                        PFGEN  
      CALL BLDNET                                                        PFGEN  
C                                                                        PFGEN  
c     WRITE(8,2000)                                                      PFGEN  
c     WRITE(9,2000)                                                      PFGEN  
c     WRITE(10,2000)                                                     MDN1208
C                                                                        PFGEN  
      WRITE(88,2010) MCAT,NBRARC,NBRNOD,NBRMCC                           MDN1208
C                                                                        PFGEN  
 1000 FORMAT(' ','CALL INITAL')                                          PFGEN  
 1010 FORMAT(' ','CALL BILDER')                                          PFGEN  
 1020 FORMAT(' ','CALL BLDNET')                                          PFGEN  
 2000 FORMAT(80('0'))                                                    PFGEN  
 2010 FORMAT('0',' NUMBER OF COMMODITIES = ',I7,/,                       MDN1208
     &           ' NUMBER OF ARCS = ',I7,/,' NUMBER OF NODES = ',I7,/,   MDN1208
     &           ' NUMBER OF MUTUAL CAPACITY CONSTRAINTS = ',I7)         MDN1208
C                                                                        PFGEN  
      STOP                                                               PFGEN  
      END                                                                PFGEN  
