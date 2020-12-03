      SUBROUTINE INSERT(DATA)                                            INSERT 
C                                                                        INSERT 
C ********************************************************************** INSERT 
C *                                                                    * INSERT 
C *********************  D E S C R I P T I O N  ************************ INSERT 
C *                                                                    * INSERT 
C ********************************************************************** INSERT 
C                                                                        INSERT 
C     SUBROUTINE INSERT INSERTS MISSION NUMBERS IN THE LINKED LIST       INSERT 
C     IQUEUE.IN ORDER OF INTERTHEATER ARRIVAL TIME.                      INSERT 
C                                                                        INSERT 
C ********************************************************************** INSERT 
C *                                                                    * INSERT 
C ***************************  S T A T U S  **************************** INSERT 
C *                                                                    * INSERT 
C ********************************************************************** INSERT 
C                                                                        INSERT 
C      ORIGINAL AUTHOR        : MCLAIN                                   INSERT 
C      ORIGINAL VERSION DATE  : 2/27/85                                  INSERT 
C                                                                        INSERT 
C ********************************************************************** INSERT 
C *                                                                    * INSERT 
C ********************  D E C L A R A T I O N S  *********************** INSERT 
C *                                                                    * INSERT 
C ********************************************************************** INSERT 
C                                                                        INSERT 
C  ------------------------                                              INSERT 
C  ---P A R A M E T E R S .                                              INSERT 
C  ------------------------                                              INSERT 
C                                                                        INSERT 
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
C                                                                        INSERT 
C  ----------------------------------                                    INSERT 
C  ---L O C A L   V A R I A B L E S .                                    INSERT 
C  ----------------------------------                                    INSERT 
C                                                                        INSERT 
      INTEGER           I     ,  NEW   ,  IDATA                          INSERT 
C                                                                        INSERT 
C  ------------------------------------                                  INSERT 
C  ---G L O B A L   V A R I A B L E S .                                  INSERT 
C  ------------------------------------                                  INSERT 
C                                                                        INSERT 
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
C                                                                        INSERT 
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        INSERT 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
C                                                                        INSERT 
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        INSERT 
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
C                                                                        INSERT 
C  ------------------------------                                        INSERT 
C  ---C O M M O N   B L O C K S .                                        INSERT 
C  ------------------------------                                        INSERT 
C                                                                        INSERT 
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
C                                                                        INSERT 
C  --------------------------------------                                INSERT 
C  ---F O R M A T   S T A T E M E N T S .                                INSERT 
C  --------------------------------------                                INSERT 
C                                                                        INSERT 
  100 FORMAT(/,' CALL INSERT(DATA): MSNNBR IS OUT OF RANGE(-',I6,')')    INSERT 
  101 FORMAT(/,' CALL INSERT(DATA): LIST IS FULL')                       INSERT 
      IDATA = 0
C                                                                        INSERT 
C ********************************************************************** INSERT 
C *                                                                    * INSERT 
C ************************  E N T R A N C E  *************************** INSERT 
C *                                                                    * INSERT 
C ********************************************************************** INSERT 
C ***                                                                *** INSERT 
C *** IF THE EVENT TIME IS OUT OF RANGE OR THE LINKED LIST IS FULL,  *** INSERT 
C *** WRITE OUT ERROR MESSAGE AND RETURN.                            *** INSERT 
C ***                                                                *** INSERT 
C ********************************************************************** INSERT 
C                                                                        INSERT 
      IF(MSNNBR.LT.0 .OR. MSNNBR.GE.300.OR.HEADG.LE.1)                   INSERT 
     *    THEN                                                           INSERT 
              IF(MSNNBR.LT.0 .OR. MSNNBR.GE.300)                         INSERT 
     *          THEN                                                     INSERT 
                    WRITE(99,100) MSNNBR                                 INSERT 
                ELSE                                                     INSERT 
                    WRITE(99,101)                                        INSERT 
              ENDIF                                                      INSERT 
              RETURN                                                     INSERT 
      ENDIF                                                              INSERT 
C                                                                        INSERT 
C ********************************************************************** INSERT 
C ***                                                                *** INSERT 
C *** TAKE THE TOP MEMBER OF THE FREE LIST TO STORE THE NEW EVENT.   *** INSERT 
C ***                                                                *** INSERT 
C ********************************************************************** INSERT 
C                                                                        INSERT 
      NEW   = HEADG                                                      INSERT 
      HEADG = IQUEUE(HEADG,IFWD)                                         INSERT 
      ISUC  = IQUEUE(1,IFWD)                                             INSERT 
      IPRD  = 1                                                          INSERT 
C                                                                        INSERT 
C ********************************************************************** INSERT 
C ***                                                                *** INSERT 
C ***                       *** DO -UNTIL ***                        *** INSERT 
C ***                                                                *** INSERT 
C *** TRAVERSE LIST UNTIL CORRECT LOCATION FOR NEW EVENT FOUND       *** INSERT 
C ***                                                                *** INSERT 
C ********************************************************************** INSERT 
C                                                                        INSERT 
      I = 0                                                              INSERT 
  10  I = I + 1                                                          INSERT 
      IF ((ISUC .GT. 1) .AND. (I .LE. MSNNBR)) THEN                      MODMCN 
         IF (IDATA .LE. IDETAIL(IQUEUE(ISUC,IVAL),MSNATM)) THEN          MODMCN 
             IPRD = ISUC                                                 MODMCN 
             ISUC = IQUEUE(IPRD,IFWD)                                    MODMCN 
             GO TO 10                                                    MODMCN 
         ENDIF                                                           MODMCN 
      ENDIF                                                              MODMCN 
C                                                                        INSERT 
C ********************************************************************** INSERT 
C ***                                                                *** INSERT 
C *** UPDATE THE EVENT COUNTER, MISSION DETAIL MATRIX ROW COUNTER,   *** INSERT 
C *** LIST POINTERS, AND EVENT NUMBER.                               *** INSERT 
C ***                                                                *** INSERT 
C ********************************************************************** INSERT 
C                                                                        INSERT 
      MSNNBR = MSNNBR + 1                                                INSERT 
      ICOUNT = ICOUNT + 1                                                INSERT 
      IQUEUE(NEW ,  IFWD) = ISUC                                         INSERT 
      IQUEUE(NEW ,  IBCK) = IPRD                                         INSERT 
      IQUEUE(ISUC,  IBCK) = NEW                                          INSERT 
      IQUEUE(IPRD,  IFWD) = NEW                                          INSERT 
      IQUEUE(NEW ,IVAL) = ICOUNT                                         INSERT 
C                                                                        INSERT 
C ********************************************************************** INSERT 
C *                                                                    * INSERT 
C ****************************  E X I T  ******************************* INSERT 
C *                                                                    * INSERT 
C ********************************************************************** INSERT 
C                                                                        INSERT 
      RETURN                                                             INSERT 
      END                                                                INSERT 
