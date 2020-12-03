      SUBROUTINE DCODER(VERBAG,IDATA,IFTYPE,NBRFMT)                      DCODER 
C                                                                        DCODER 
C ********************************************************************** DCODER 
C *                                                                    * DCODER 
C *********************  D E S C R I P T I O N  ************************ DCODER 
C *                                                                    * DCODER 
C ********************************************************************** DCODER 
C                                                                        DCODER 
C     SUBROUTINE DCODER CONVERTS INTEGER DATA TO CHARACTER STRINGS       DCODER 
C     TO BE INSERTED INTO THE ARC AND NODE CARD TEXT FIELD.              DCODER 
C                                                                        DCODER 
C ********************************************************************** DCODER 
C *                                                                    * DCODER 
C ***************************  S T A T U S  **************************** DCODER 
C *                                                                    * DCODER 
C ********************************************************************** DCODER 
C                                                                        DCODER 
C      ORIGINAL AUTHOR        : MCLAIN                                   DCODER 
C      ORIGINAL VERSION DATE  : 12/28/83                                 DCODER 
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           DCODER 
C                               2/15/85  FORMAL PARAMETERS MOVED TO      DCODER 
C                                        COMMON BLOCKS.                  DCODER 
C                                                                        DCODER 
C ********************************************************************** DCODER 
C *                                                                    * DCODER 
C ********************  D E C L A R A T I O N S  *********************** DCODER 
C *                                                                    * DCODER 
C ********************************************************************** DCODER 
C                                                                        DCODER 
C  ------------------------                                              DCODER 
C  ---P A R A M E T E R S .                                              DCODER 
C  ------------------------                                              DCODER 
C                                                                        DCODER 
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
C                                                                        DCODER 
C  ----------------------------------                                    DCODER 
C  ---L O C A L   V A R I A B L E S .                                    DCODER 
C  ----------------------------------                                    DCODER 
C                                                                        DCODER 
      INTEGER           ILIMIT,  ITEST ,  NEXT  ,  IDIGIT,  ISIZE ,  I,  DCODER 
     *                  J     ,  IWIDTH,  ISTART,  IEND                  DCODER 
C                                                                        DCODER 
      CHARACTER*9       INTGRS                                           DCODER 
C                                                                        DCODER 
C  ------------------------------------                                  DCODER 
C  --- P A S S E D   V A R I A B L E S .                                 DCODER 
C  -------------------------------------                                 DCODER 
C                                                                        DCODER 
      INTEGER         IDATA,  IFTYPE,  NBRFMT                            DCODER 
C                                                                        DCODER 
      CHARACTER*50    VERBAG                                             DCODER 
C                                                                        DCODER 
      DIMENSION       IDATA (10)                                         DCODER 
C                                                                        DCODER 
C  ------------------------------------                                  DCODER 
C  ---G L O B A L   V A R I A B L E S .                                  DCODER 
C  ------------------------------------                                  DCODER 
C                                                                        DCODER 
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
C                                                                        DCODER 
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        DCODER 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        DCODER 
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        DCODER 
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
C                                                                        DCODER 
C  ------------------------------                                        DCODER 
C  ---C O M M O N   B L O C K S .                                        DCODER 
C  ------------------------------                                        DCODER 
C                                                                        DCODER 
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
C  -------------------------------------------                           DCODER 
C  --- I N T R I N S I C   F U N C T I O N S .                           DCODER 
C  -------------------------------------------                           DCODER 
C                                                                        DCODER 
      INTRINSIC   MAX0,  MIN0                                            DCODER 
C                                                                        DCODER 
C  ----------------------------------                                    DCODER 
C  ---D A T A   S T A T E M E N T S .                                    DCODER 
C  ----------------------------------                                    DCODER 
C                                                                        DCODER 
      DATA INTGRS /'123456789'/                                          DCODER 
C                                                                        DCODER 
C  --------------------------------------                                DCODER 
C  ---F O R M A T   S T A T E M E N T S .                                DCODER 
C  --------------------------------------                                DCODER 
C                                                                        DCODER 
 9000 FORMAT(1X,'WIDTH ERROR IN DCODER; VALUE=',I6,' MAX WIDTH=',I2)     DCODER 
C                                                                        DCODER 
C ********************************************************************** DCODER 
C *                                                                    * DCODER 
C ************************  E N T R A N C E  *************************** DCODER 
C *                                                                    * DCODER 
C ********************************************************************** DCODER 
C                                                                        DCODER 
      IF (IFTYPE .EQ. 1)                                                 MODMCN 
     *   THEN                                                            DCODER 
             ILIMIT = NODFMT(NBRFMT,1)                                   DCODER 
         ELSE IF (IFTYPE .EQ. 2) then                                    MODMCN 
             ILIMIT = IACFMT(NBRFMT,1)                                   MODMCN 
         ELSE                                                            DCODER 
             ILIMIT = MCCFMT(NBRFMT,1)                                   MODMCN 
      ENDIF                                                              DCODER 
      IF(ILIMIT .LE. 0)GO TO 9999                                        DCODER 
      DO 100 I = 1, ILIMIT                                               DCODER 
         IF(IDATA(I) .EQ. 0)GO TO 100                                    DCODER 
         ISIZE = 1                                                       DCODER 
         IMIN1 = MIN0((IDATA(I)/10)*2,2)                                 DCODER 
         IMIN2 = MIN0((IDATA(I)/100)*3,3)                                DCODER 
         IMIN3 = MIN0((IDATA(I)/1000)*4,4)                               DCODER 
         IMIN4 = MIN0((IDATA(I)/10000)*5,5)                              DCODER 
         ISIZE = MAX0(IMIN1,IMIN2,IMIN3,IMIN4,ISIZE)                     DCODER 
C                                                                        MODMCN 
         IWDPT = (2*I)+1                                                 MODMCN 
         ISTPT = 2*I                                                     MODMCN 
C                                                                        MODMCN 
         IF(IFTYPE.EQ.1)                                                 MODMCN 
     *       THEN                                                        MODMCN 
                 IWIDTH = NODFMT(NBRFMT,IWDPT)                           MODMCN 
                 ISTART = NODFMT(NBRFMT,ISTPT)                           MODMCN 
             ELSE IF (IFTYPE .EQ. 2) then                                MODMCN 
                 IWIDTH = IACFMT(NBRFMT,IWDPT)                           MODMCN 
                 ISTART = IACFMT(NBRFMT,ISTPT)                           MODMCN 
             ELSE                                                        MODMCN 
                 IWIDTH = MCCFMT(NBRFMT,IWDPT)                           MODMCN 
                 ISTART = MCCFMT(NBRFMT,ISTPT)                           MODMCN 
         ENDIF                                                           MODMCN 
C                                                                        MODMCN 
             IF(ISIZE.GT.IWIDTH)WRITE(99,9000)IDATA(I),IWIDTH            DCODER 
             IEND   = ISTART + IWIDTH - 1                                DCODER 
             NEXT   = IEND-ISIZE+1                                       DCODER 
             ITEST  = IDATA(I)                                           DCODER 
             DO 110 J = ISIZE-1, 0, -1                                   DCODER 
                    IDIGIT = (ITEST-MOD(ITEST,10**J))/(10**J)            DCODER 
                      ITEST  = ITEST-(IDIGIT*(10**J))                    DCODER 
                      IF(IDIGIT.NE.0)                                    DCODER 
     *                   THEN                                            DCODER 
                             VERBAG(NEXT:NEXT)=INTGRS(IDIGIT:IDIGIT)     DCODER 
                         ELSE                                            DCODER 
                             VERBAG(NEXT:NEXT)='0'                       DCODER 
                      ENDIF                                              DCODER 
                      NEXT   = NEXT+1                                    DCODER 
  110        CONTINUE                                                    DCODER 
  100 CONTINUE                                                           DCODER 
C                                                                        DCODER 
C ********************************************************************** DCODER 
C *                                                                    * DCODER 
C ****************************  E X I T  ******************************* DCODER 
C *                                                                    * DCODER 
C ********************************************************************** DCODER 
C                                                                        DCODER 
 9999 RETURN                                                             DCODER 
      END                                                                DCODER 
