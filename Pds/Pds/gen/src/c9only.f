      SUBROUTINE C9ONLY(IDAY)                                            C9ONLY 
C                                                                        C9ONLY 
C ********************************************************************** C9ONLY 
C *                                                                    * C9ONLY 
C *********************  D E S C R I P T I O N  ************************ C9ONLY 
C *                                                                    * C9ONLY 
C ********************************************************************** C9ONLY 
C                                                                        C9ONLY 
C     SUBROUTINE C9ONLY TAKES CARE OF THE MISSIONS WHEN THERE ARE        C9ONLY 
C     NO INTERTHEATER AIRCRAFT MISSIONS SCHEDULED AND ONLY CONUS         C9ONLY 
C     REDISTRIBUTION OF PATIENTS ALREADY IN STAGING FACILITIES IS REQUIR C9ONLY 
                                                                         C9ONLY 
C ********************************************************************** C9ONLY 
C *                                                                    * C9ONLY 
C ***************************  S T A T U S  **************************** C9ONLY 
C *                                                                    * C9ONLY 
C ********************************************************************** C9ONLY 
C                                                                        C9ONLY 
C      ORIGINAL AUTHOR        : MCLAIN                                   C9ONLY 
C      ORIGINAL VERSION DATE  :  5/20/85                                 C9ONLY 
C      REVISIONS              :                                          C9ONLY 
C                                                                        C9ONLY 
C ********************************************************************** C9ONLY 
C *                                                                    * C9ONLY 
C ********************  D E C L A R A T I O N S  *********************** C9ONLY 
C *                                                                    * C9ONLY 
C ********************************************************************** C9ONLY 
C                                                                        C9ONLY 
C  ------------------------                                              C9ONLY 
C  ---P A R A M E T E R S .                                              C9ONLY 
C  ------------------------                                              C9ONLY 
C                                                                        C9ONLY 
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
C                                                                        C9ONLY 
C  ----------------------------------                                    C9ONLY 
C  ---L O C A L   V A R I A B L E S .                                    C9ONLY 
C  ----------------------------------                                    C9ONLY 
C                                                                        C9ONLY 
      INTEGER  NUMBRS ,  IDAY  ,  IASF,  MCCTYP,  IATYPE,  NODTYP,       C9ONLY 
     *         IORIGIN,  IFROM ,  ICAT,  MCCPTR,  IFRTYP,  ITOTYP,       C9ONLY 
     *         NODE1                                                     C9ONLY 
C                                                                        C9ONLY 
      REAL     REQMNT ,  CPACTY,  COST,  UPRBND                          C9ONLY 
C                                                                        C9ONLY 
      DIMENSION     NUMBRS(10) , REQMNT(MCAT),                           C9ONLY 
     *              UPRBND(MCAT), MCCPTR(MCAT)                           C9ONLY 
C                                                                        C9ONLY 
C  ------------------------------------                                  C9ONLY 
C  ---G L O B A L   V A R I A B L E S .                                  C9ONLY 
C  ------------------------------------                                  C9ONLY 
C                                                                        C9ONLY 
C                                                                        C9ONLY 
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
C                                                                        C9ONLY 
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        C9ONLY 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        C9ONLY 
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        C9ONLY 
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
C                                                                        C9ONLY 
C  ------------------------------                                        C9ONLY 
C  ---C O M M O N   B L O C K S .                                        C9ONLY 
C  ------------------------------                                        C9ONLY 
C                                                                        C9ONLY 
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
C                                                                        C9ONLY 
C  ------------------------                                              C9ONLY 
C  ---D A T A   S T A T E M E N T S.                                     C9ONLY 
C  ------------------------                                              C9ONLY 
C                                                                        C9ONLY 
c      DATA NUMBRS /10*0/, REQMNT /MCAT*0.0/, UPRBND/MCAT*0.0/            C9ONLY 
      do 1223 i = 1, MCAT
        REQMNT(i) = 0.0 
        UPRBND(i) = 0.0 
 1223 continue
      do 1224 i = 1, 10
        NUMBRS(i) = 0 
 1224 continue 
C                                                                        C9ONLY 
C ***               DO - LOOP                                        *** C9ONLY 
C *** FOR EACH ASF WITH A PREVIOUS NODE NUMBER NOT VISITED ON IDAY:  *** C9ONLY 
C                                                                        C9ONLY 
      DO 100 IASF = 1, MASF                                              C9ONLY 
             IF(IASFDTL(IASF,IASFPNN) .GT. 0 .AND.                       C9ONLY 
     *          IASFDTL(IASF,IASFCHK) .LE. 0)                            C9ONLY 
     *          THEN                                                     C9ONLY 
C                                                                        C9ONLY 
C ********************************************************************** C9ONLY 
C ***                                                                *** C9ONLY 
C *** STEP 1.                                                        *** C9ONLY 
C ***        CREATE AN ASF MUTUAL CAPACITY CONSTRAINT (MCC TYPE 6)   *** C9ONLY 
C ***                                                                *** C9ONLY 
C ********************************************************************** C9ONLY 
C                                                                        C9ONLY 
                    MCCTYP = 6                                           C9ONLY 
                    CPACTY = FLOAT(IASFDTL(IASF,IASFMX))                 C9ONLY 
                    NUMBRS(1) = IASF                                     C9ONLY 
                    NUMBRS(2) = IDAY                                     C9ONLY 
                    CALL WRTMCC(MCCTYP,CPACTY,NUMBRS)                    C9ONLY 
C                                                                        C9ONLY 
C ********************************************************************** C9ONLY 
C ***                                                                *** C9ONLY 
C *** STEP 2.                                                        *** C9ONLY 
C ***        CREATE THE NEW ASF NODE.  IF THE ASF IS COLOCATED WITH  *** C9ONLY 
C ***        AN APOD, CREATE NODE TYPE #6.  OTHERWISE, CREATE NODE    ** C9ONLY 
C ***        TYPE #11.                                               *** C9ONLY 
C ***                                                                *** C9ONLY 
C ********************************************************************** C9ONLY 
C                                                                        C9ONLY 
                    IORGIN = IASFDTL(IASF,IASFDOS)                       C9ONLY 
                    IF(IASFDTL(IASF,IASFPOD) .GT. 0)                     C9ONLY 
     *                 THEN                                              C9ONLY 
                           NODTYP = 6                                    C9ONLY 
                           NUMBRS(1) = IASF                              C9ONLY 
                           NUMBRS(2) = IASFDTL(IASF,IASFPOD)             C9ONLY 
                           NUMBRS(3) = IDAY                              C9ONLY 
                           CALL WRNODE(NODTYP,REQMNT,NUMBRS)             C9ONLY 
                       ELSE                                              C9ONLY 
                           NODTYP = 11                                   C9ONLY 
                           NUMBRS(1) = IASF                              C9ONLY 
                           NUMBRS(2) = IORGIN                            C9ONLY 
                           NUMBRS(3) = IDAY                              C9ONLY 
                           CALL WRNODE(NODTYP,REQMNT,NUMBRS)             C9ONLY 
                    ENDIF                                                C9ONLY 
                    IASFDTL(IASF,IASFCHK) = NBRNOD                       C9ONLY 
C                                                                        C9ONLY 
C ********************************************************************** C9ONLY 
C ***                                                                *** C9ONLY 
C *** STEP 3.                                                        *** C9ONLY 
C ***        UPDATE ASF DATA                                         *** C9ONLY 
C ***                                                                *** C9ONLY 
C ********************************************************************** C9ONLY 
C                                                                        C9ONLY 
                    IFROM = IASFDTL(IASF,IASFPNN)                        C9ONLY 
                    IASFDTL(IASF,IASFPNN) = NBRNOD                       C9ONLY 
                    IASFDTL(IASF,IASFPTM) = IASFDTL(IASF,IASFPTM) + 1440 C9ONLY 
                    IASFDTL(IASF,IASFPDY) = IDAY                         C9ONLY 
C                                                                        C9ONLY 
C ********************************************************************** C9ONLY 
C ***                                                                *** C9ONLY 
C *** STEP 4.                                                        *** C9ONLY 
C ***        CREATE THE ASF CAPACITY ARCS (ARC TYPE #15)             *** C9ONLY 
C ***                                                                *** C9ONLY 
C ********************************************************************** C9ONLY 
C                                                                        C9ONLY 
                    IATYPE                = 15                           C9ONLY 
                    NODSAVE               = IARCS(IATYPE,NODETO)         C9ONLY 
                    IARCS(IATYPE,NODETO)  = NODTYP                       C9ONLY 
                    ARCTXT(IATYPE)(3:5) = 'DOS'                          C9ONLY 
                    ARCTXT(IATYPE)(18:20) = 'DOS'                        C9ONLY 
                    NUMBRS(1)             = IASF                         C9ONLY 
                    NUMBRS(2)             = IORGIN                       C9ONLY 
                    NUMBRS(3)             = IDAY                         C9ONLY 
                    IFRTYP = NODTYP                                      C9ONLY 
                    ITOTYP = 11                                          C9ONLY 
                    IF(NODTYP .EQ. 6)                                    C9ONLY 
     *                 THEN                                              C9ONLY 
                           NUMBRS(2) = IASFDTL(IASF,IASFPOD)             C9ONLY 
                           ARCTXT(IATYPE)(3:5) = 'POD'                   C9ONLY 
                           ARCTXT(IATYPE)(18:20) = 'POD'                 C9ONLY 
                           ITOTYP = 6                                    C9ONLY 
                    ENDIF                                                C9ONLY 
                    COST = ARCCIJ(IATYPE)                                C9ONLY 
                    DO 110 ICAT=1, MCAT                                  C9ONLY 
                           UPRBND(ICAT) = ARCUIJ(IATYPE,ICAT)            C9ONLY 
                           IF (UPRBND(ICAT) .EQ. -1.00)                  C9ONLY 
     *                         UPRBND(ICAT) = CPACTY                     C9ONLY 
                           MCCPTR(ICAT) = NBRMCC                         C9ONLY 
  110               CONTINUE                                             C9ONLY 
                    CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,NUMBRS,  C9ONLY 
     *                          IFRTYP,ITOTYP)                           C9ONLY 
                    IARCS(IATYPE,NODETO)  = NODSAVE                      C9ONLY 
                    ARCTXT(IATYPE)(3:5) = 'DOS'                          C9ONLY 
                    ARCTXT(IATYPE)(18:20) = 'DOS'                        C9ONLY 
C                                                                        C9ONLY 
C ********************************************************************** C9ONLY 
C ***                                                                *** C9ONLY 
C *** STEP 5.                                                        *** C9ONLY 
C ***        CREATE A MUTUAL CAPACITY CONSTRAINT (MCC TYPE #9)       *** MODMCN 
C ***        AND ARCS TO LOCAL HOSPITALS AND TO DISTANT HOSPITALS    *** MODMCN 
C ***        VIA C-9.                                                *** C9ONLY 
C ***                                                                *** C9ONLY 
C ********************************************************************** C9ONLY 
C                                                                        C9ONLY 
                    NODE1 = NODTYP                                       C9ONLY 
                    NODTYP = 7                                           C9ONLY 
                    NUMBRS(1) = IASF                                     C9ONLY 
                    NUMBRS(2) = IDAY                                     C9ONLY 
                    CALL WRNODE(NODTYP,REQMNT,NUMBRS)                    C9ONLY 
                    IC9PTR = IC9PTR + 1                                  C9ONLY 
                    IC9MSN(IC9PTR,IC9NOD) = NODES(NODTYP)                C9ONLY 
                    NODTYP = NODE1                                       C9ONLY 
C                                                                        C9ONLY 
                    MCCTYP = 9                                           MODMCN 
                    CPACTY = MAXC9(IDAY)                                 MODMCN 
                    CALL WRTMCC(MCCTYP,CPACTY,NUMBRS)                    MODMCN 
C                                                                        MODMCN 
                    IF (NODTYP .EQ. 6)                                   C9ONLY 
     *                 THEN                                              C9ONLY 
                          IATYPE = 9                                     C9ONLY 
                          ITOTYP = 7                                     C9ONLY 
                          NUMBRS(1) = IASFDTL(IASF,IASFPOD)              C9ONLY 
                          NUMBRS(2) = IASF                               C9ONLY 
                          NUMBRS(3) = IDAY                               C9ONLY 
                       ELSE                                              C9ONLY 
                          IF (NODTYP .EQ. 11)                            C9ONLY 
     *                       THEN                                        C9ONLY 
                                IATYPE = 29                              C9ONLY 
                                ITOTYP = 7                               C9ONLY 
                                NUMBRS(1) = IASFDTL(IASF,IASFDOS)        C9ONLY 
                                NUMBRS(2) = IASF                         C9ONLY 
                                NUMBRS(3) = IDAY                         C9ONLY 
                             ELSE                                        C9ONLY 
                                WRITE(99,900)NODTYP                      C9ONLY 
                          ENDIF                                          C9ONLY 
                    ENDIF                                                C9ONLY 
                    COST      = ARCCIJ(IATYPE)                           C9ONLY 
                    IFROM     = NODES(NODTYP)                            C9ONLY 
                    DO 120 ICAT = 1, MCAT                                C9ONLY 
                           UPRBND(ICAT) = ARCUIJ(IATYPE,ICAT)            C9ONLY 
                           IF (UPRBND(ICAT) .EQ. -1.00)                  C9ONLY 
     *                         UPRBND(ICAT) = FLOAT(MAXC9(IDAY))         C9ONLY 
                           MCCPTR(ICAT) = NBRMCC                         MODMCN 
  120               CONTINUE                                             C9ONLY 
                    CALL WRTARC(IATYPE,IFROM,COST,UPRBND,MCCPTR,NUMBRS,  C9ONLY 
     *                          IFRTYP,ITOTYP)                           C9ONLY 
                    IC9MSN(IC9PTR,LNKC9M) = IASFDTL(IASF,LNKASF)         C9ONLY 
                    IC9MSN(IC9PTR,IC9ONT) = 1                            C9ONLY 
                    IC9MSN(IC9PTR,IC9NDX) = IASF                         C9ONLY 
                    IC9MSN(IC9PTR,IC9AMT) = IASFDTL(IASF,IASFPTM)        C9ONLY 
                    IATYPE = 16                                          C9ONLY 
                    CALL CRTLCL(IATYPE,NODTYP,IDAY,IORGIN,IASF)          C9ONLY 
             ENDIF                                                       C9ONLY 
  100 CONTINUE                                                           C9ONLY 
C                                                                        C9ONLY 
  900 FORMAT('0','ILLEGAL NODE TYPE IN C9ONLY.  NODTYP = ',I3)           C9ONLY 
      RETURN                                                             C9ONLY 
      END                                                                C9ONLY 
