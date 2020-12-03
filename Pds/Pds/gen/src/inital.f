      SUBROUTINE INITAL                                                  INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C *                                                                    * INITAL 
C *********************  D E S C R I P T I O N  ************************ INITAL 
C *                                                                    * INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
C     SUBROUTINE INITAL INITIALIZES ALL DATA STRUCTURES THAT             INITAL 
C     REQUIRE EXTERNAL DATA.  (THE BLOCK DATA SUBPROGRAM IS USED         INITAL 
C     TO INITIALIZE ALL OTHER DATA.)                                     INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C *                                                                    * INITAL 
C ***************************  S T A T U S  **************************** INITAL 
C *                                                                    * INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
C      ORIGINAL AUTHOR        : MCLAIN                                   INITAL 
C      ORIGINAL VERSION DATE  : 12/28/83                                 INITAL 
C      REVISIONS              : 12/19/84  FORMAL PARAMETERS MOVED TO     INITAL 
C                                         COMMON BLOCKS.                 INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C *                                                                    * INITAL 
C *****************  D A T A   S T R U C T U R E S  ******************** INITAL 
C *                                                                    * INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
C----FILE TABLE                                                          INITAL 
C                                                                        INITAL 
C       FYLNAM (MFILES        ) - FILE NAME                              INITAL 
C       IUNITS (MFILES        ) - DEVICE FILE ASSIGNED TO                INITAL 
C                                                                        INITAL 
C----ARC TABLE                                                           INITAL 
C                                                                        INITAL 
C       IARCS  (NTYPEA, NBREAT) - CURRENT NUMBER OF EACH ARC TYPE        INITAL 
C       IARCS  (NTYPEA, NBRBAT) - CUMULATIVE COUNTS BY ARC TYPE          INITAL 
C       IARCS  (NTYPEA, NODEFR) - FROM NODE TYPE                         INITAL 
C       IARCS  (NTYPEA, NODETO) - TO NODE TYPE                           INITAL 
C       ARCTXT (NTYPEA        ) - ARC CARD FIXED TEXT                    INITAL 
C       IACFMT (NTYPEA,     27) - ARC CARD VARIABLE FORMAT SPECS         INITAL 
C       ARCCIJ (NTYPEA        ) - ARC COST, BY ARC TYPE                  INITAL 
C       ARCUIJ (NTYPEA,   MCAT) - ARC CAPACITY BY ARC TYPE AND CATEGORY  INITAL 
C                                                                        INITAL 
C----NODE TABLE                                                          INITAL 
C                                                                        INITAL 
C       NODES  (NTYPEN        ) - CURRENT NUMBER OF EACH NODE TYPE       INITAL 
C       NODTXT (NTYPEN        ) - NODE CARD FIXED TEXT                   INITAL 
C       NODFMT (NTYPEN,     27) - NODE CARD VARIABLE FORMAT SPECS        INITAL 
C                                                                        INITAL 
C----MUTUAL CAPACITY CONSTRAINT TABLE                                    INITAL 
C                                                                        INITAL 
C       MCCDPF (MCAT          ) - MAX. DAILY PATIENT FLOW BY CATEGORY    INITAL 
C       MCCTXT (NTYPEM        ) - MCC CARD FIXED TEXT                    INITAL 
C       MCCFMT (NTYPEM,     27) - MCC CARD VARIABLE FORMAT SPECS         INITAL 
C                                                                        INITAL 
C----AEROMEDICAL STAGING FACILITY (ASF) DATA TABLE                       INITAL 
C                                                                        INITAL 
C       IASFDTL(MASF , LNKASF ) - INDEX TO IBASES TABLE                  INITAL 
C       IASFDTL(MASF , IASFMX ) - MAXIMUM CAPACITY AT EACH ASF           INITAL 
C       IASFDTL(MASF , IASFPOD) - INDEX OF ASSOCIATED APOD (IF ANY)      INITAL 
C       IASFDTL(MASF , IASFDOS) - INDEX OF ASSOCIATED DOS (IF ANY)       INITAL 
C       IASFDTL(MASF , IASFC9B) - INDEX OF ASSOCIATED C-9 BASE           INITAL 
C       IASFDTL(MASF , IASFPNN) - ASF PREVIOUS NODE NUMBER               INITAL 
C       IASFDTL(MASF , IASFPDY) - ASF PREVIOUS DAY                       INITAL 
C       IASFDTL(MASF , IASFPTM) - ASF PREVIOUS TIME                      INITAL 
C       IASFDTL(MASF , IASFCHK) - DAILY VISIT COUNTER                    INITAL 
C                                                                        INITAL 
C----DESTINATION ONLOAD STATION (DOS) DATA TABLE                         INITAL 
C                                                                        INITAL 
C       IDOSDTL(MDOS , LNKDOS ) - INDEX TO IBASES TABLE                  INITAL 
C       IDOSDTL(MDOS , IDOSIND) - CO-LOCATED ASF INDICATOR               INITAL 
C       IDOSDTL(MDOS , IDOSASF) - CO-LOCATED ASF INDEX                   INITAL 
C       IDOSDTL(MDOS , IDOSC9B) - ASSOCIATED C-9 BASE                    INITAL 
C       IDOSDTL(MDOS , IDOSPOD) - ASSOCIATED APOD                        INITAL 
C       IDOSDTL(MDOS , IDOSCHK) - DAILY VISIT COUNTER                    INITAL 
C                                                                        INITAL 
C----BASES MASTER DATA TABLE                                             INITAL 
C                                                                        INITAL 
C       IBASES (MBASES, INDIAP) - IAP INDICATOR                          INITAL 
C       IBASES (MBASES, INDPOD) - APOD INDICATR                          INITAL 
C       IBASES (MBASES, INDASF) - ASF INDICATOR                          INITAL 
C       IBASES (MBASES, INDC9B) - C-9 BASE INDICATOR                     INITAL 
C       IBASES (MBASES, INDDOS) - DOS INDICATOR                          INITAL 
C       IBASES (MBASES, LATDEG) - LATITUDE DEGREES                       INITAL 
C       IBASES (MBASES, LATMIN) - LATITUDE MINUTES                       INITAL 
C       IBASES (MBASES, LNGDEG) - LONGITUDE DEGREES                      INITAL 
C       IBASES (MBASES, LNGMIN) - LONGITUDE MINUTES                      INITAL 
C       NSEW   (MBASES,      2) - NORTH/SOUTH & EAST/WEST INDICATORS     INITAL 
C       IBASES (MBASES, MAXSTG) - MAX IAP STAGING CAPACITY               INITAL 
C       IBASES (MBASES, MAXTRX) - MAX IAP TRANSIENT CAPACITY             INITAL 
C       IBASES (MBASES, MAXASF) - MAX ASF CAPACITY                       INITAL 
C       IBASES (MBAXES, NDXIAP) - INDEX TO IAP TABLE                     INITAL 
C       IBASES (MBASES, NDXPOD) - INDEX TO APOD TABLE                    INITAL 
C       IBASES (MBASES, NDXASF) - INDEX TO ASF TABLE                     INITAL 
C       IBASES (MBASES, NDXC9B) - INDEX TO C?9 BASE TABLE                INITAL 
C       IBASES (MBASES, NDXDOS) - INDEX TO DOS TABLE                     INITAL 
C                                                                        INITAL 
C----CASUALTY DATA TABLE                                                 INITAL 
C                                                                        INITAL 
C       ICASLT (MDAYS , MCAT  ) - DAILY CASUALTIES BY CATEGORY           INITAL 
C       IDMAND (MCAT          ) - TOTAL CASUALTIES BY CATEGORY           INITAL 
C       ITCBP  (        9     ) - TOTAL CATEGORIES BY 10-DAY PERIOD      INITAL 
C       LOS    (MCAT          ) - LENGTH OF STAY BY CATEGORY             INITAL 
C       LITAMB (MCAT          ) - LITTER (=1)/AMBULATORY (=0) INDICATOR  INITAL 
C       FACTOR (     3, MCAT  ) - PROPORTIONS OF CASUALTIES IN           INITAL 
C                                    EACH 30-DAY PERIOD BY CATEGORY      INITAL 
C----HOSPITAL DATA TABLE                                                 INITAL 
C                                                                        INITAL 
C       HSPTBD (MDAYS , MHOS  ) - TOTAL DAILY HOSPITAL CAPACITY          INITAL 
C       HSPCAP(MDAYS,MCAT,MHOS) - DAILY HOSPITAL CAPACITY BY             INITAL 
C       IHSPOTL(MHOS , IPRHSP ) - NODE NUMBER OF HOSPITAL ON PERVIOUS DA INITAL 
C       IHSPDTL(MHOS , IC9BAS ) - C9 SERVICING BASE FOR EACH HOSPITAL    INITAL 
C                                                                        INITAL 
C----INTERTHEATER AIREVAC POINT (IAP) DATA TABLE                         INITAL 
C                                                                        INITAL 
C       IAPDTL (MIAPS, LNKIAP ) - INDEX TO IBASES TABLE                  INITAL 
C       IAPDTL (MIAPS, IAPSMX ) - IAP MAXIMUM STAGING CAPACITY           INITAL 
C       IAPDTL (MIAPS, IAPTMX ) - IAP MAXIUM TRANSIENT CAPACITY          INITAL 
C       IAPDTL (MIAPS, IAPPNN ) - IAP PREVIOUS NODE NUMBER               INITAL 
C       IAPDTL (MIAPS, IAPPDY ) - IAP PREVIOUS DAY                       INITAL 
C                                                                        INITAL 
C----AERIAL PORT OF DEBARKATION (POD) DATA TABLE                         INITAL 
C                                                                        INITAL 
C       IPODDTL(MAPOD , LNKPOD ) - INDEX TO IBASES TABLE                 INITAL 
C       IPODDTL(MAPOD , IPODIND) - ASF INDICATOR                         INITAL 
C       IPODDTL(MAPOD , IPODASF) - ASF INDEX                             INITAL 
C       IPODDTL(MAPOD , IPODC9B) - INDEX TO C-9 BASE TABLE               INITAL 
C       IPODDTL(MAPOD , IPODDOS) - INDEX TO DOS TABLE                    INITAL 
C       IPODDTL(MAPOD , IPODCHK) - DAILY VISIT COUNTER                   INITAL 
C                                                                        INITAL 
C----C-9 BASE DATA TABLE                                                 INITAL 
C                                                                        INITAL 
C       IC9DTL (MC9BAS, LNKC9B) - INDEX TO IBASES TABLE                  INITAL 
C       IC9DTL (MC9BAS, IC9IND) - ASF INDICATOR                          INITAL 
C       IC9DTL (MC9BAS, IC9POD) - INDEX TO POD TABLE                     INITAL 
C       IC9DTL (MC9BAS, IC9DOS) - INDEX TO DOS TABLE                     INITAL 
C       IC9DTL (MC9BAS, IC9ASF) - INDEX TO ASF TABLE                     INITAL 
C                                                                        INITAL 
C----AIRCRAFT DATA TABLE                                                 INITAL 
C                                                                        INITAL 
C       AIRCRFT(MAXAC         ) - AIRCRAFT IDENTIFIERS                   INITAL 
C       IACRAFT(MAXAC, ITAS   ) - TRUE AIR SPEED                         INITAL 
C       IACRAFT(MAXAC, IALTIM ) - APPROACH/LANDING TIME                  INITAL 
C       IACRAFT(MAXAC, ITAXI  ) - TAXI-IN TIME                           INITAL 
C       IACRAFT(MAXAC, MAXRNG ) - MAXIMUM RANGE                          INITAL 
C       IACRAFT(MAXAC, IPODGT ) - APOD GROUND TIME                       INITAL 
C       IACRAFT(MAXAC, IDOSGT ) - DOS GROUND TIME                        INITAL 
C       IACRAFT(MAXAC, IC9GT  ) - C-9 SERVICING BASE GROUND TIME         INITAL 
C       IACRAFT(MAXAC, MAXLIT ) - MAX LITTER CAPACITY                    INITAL 
C       IACRAFT(MAXAC, MAXAMB ) - MAX AMBULATORY CAPACITY                INITAL 
C       IACRAFT(MAXAC, MAXTOT ) - MAX TOTAL PATIENT CAPACITY             INITAL 
C                                                                        INITAL 
C----DAILY C-141 INTERTHEATER MISSION DETAIL TABLE                       INITAL 
C                                                                        INITAL 
C       IDETAIL(ICOUNT, MSNITA) - INTERTHEATER ARRIVAL STATION           INITAL 
C       IDETAIL(ICOUNT, MSNITD) - INTERTHEATER DEPARTURE STATION         INITAL 
C       IDETAIL(ICOUNT, MSNDDY) - INTERTHEATER DEPARTURE DATE            INITAL 
C       IDETAIL(ICOUNT, MSNDTM) - INTERTHEATER DEPARTURE TIME            INITAL 
C       IDETAIL(ICOUNT, MSNFT1) - INTERTHEATER FLYING TIME               INITAL 
C       IDETAIL(ICOUNT, MSNADY) - INTERTHEATER ARRIVAL DATE              INITAL 
C       IDETAIL(ICOUNT, MSNATM) - INTERTHEATER ARRIVAL TIME              INITAL 
C       IDETAIL(ICOUNT, MSNGTM) - INTERTHEATER ARRIVAL GROUND TIME       INITAL 
C       IDETAIL(ICOUNT, MSNSDY) - INTRACONUS DEPARTURE DATE              INITAL 
C       IDETAIL(ICOUNT, MSNSTM) - INTRACONUS DEPARTURE TIME              INITAL 
C       IDETAIL(ICOUNT, MSNFT2) - INTRACONUS FLYING TIME                 INITAL 
C       IDETAIL(ICOUNT, MSNONL) - DESTINATION ONLOAD STATION(DOS)        INITAL 
C       IDETAIL(ICOUNT, MSNODY) - DOS ARRIVAL DATE                       INITAL 
C       IDETAIL(ICOUNT, MSNOTM) - DOS ARRIVAL TIME                       INITAL 
C       IDETAIL(ICOUNT, MSNTYP) - MISSION TYPE                           INITAL 
C       IDETAIL(ICOUNT,  MSNAC) - MISSION AIRCRAFT                       INITAL 
C       IDETAIL(ICOUNT, MSNSEQ) - DAILY MISSION SEQUENCE NUMBER          INITAL 
C       IDETAIL(ICOUNT, MSNTOT) - CUMULATIVE MISSION SEQUENCE NUMBER     INITAL 
C       IDETAIL(ICOUNT, MSNIAP) - INDEX TO IAP TABLE                     INITAL 
C       IDETAIL(ICOUNT, MSNPOD) - INDEX TO APOD TABLE                    INITAL 
C       IDETAIL(ICOUNT, MSNDOS) - INDEX TO DOS TABLE                     INITAL 
C       IDETAIL(ICOUNT, LMTLIT) - LITTER CAPACITYCITY                    INITAL 
C       IDETAIL(ICOUNT, LMTAMB) - AMBULATORY CAPACITY                    INITAL 
C       IDETAIL(ICOUNT, LMTTOT) - TOTAL PATIENT CAPACITY                 INITAL 
C                                                                        INITAL 
C---DAILY C-9 MISSION DATA TABLE                                         INITAL 
C                                                                        INITAL 
C       IC9MSN (MC9MSN, LNKC9M) - INDEX TO IBASES TABLE                  INITAL 
C       IC9MSN (MC9MSN, IC9ONT) - ORIGINATING NODE TYPE                  INITAL 
C       IC9MSN (MC9MSN, IC9NDX) - INDES TO NODE TYPE TABLE               INITAL 
C       IC9MSN (MC9MSN, IC9AMT) - AVAILABLE TO MOVE TIME                 INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C *                                                                    * INITAL 
C ********************  D E C L A R A T I O N S  *********************** INITAL 
C *                                                                    * INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
C  ------------------------                                              INITAL 
C  ---P A R A M E T E R S .                                              INITAL 
C  ------------------------                                              INITAL 
C                                                                        INITAL 
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
C                                                                        INITAL 
C  ----------------------------------                                    INITAL 
C  ---L O C A L   V A R I A B L E S .                                    INITAL 
C  ----------------------------------                                    INITAL 
C                                                                        INITAL 
      PARAMETER         (IWKDIM = MCAT + MDAYS)                          INITAL 
      INTEGER           IDAY  ,  IWORK ,  IFORMAT, IOS   ,  IDATA,       INITAL 
     *                  IFILE ,  ICAT  ,  I10DAY,  I30DAY,  IERRNBR,     INITAL 
     *                  NBRFYL,  IEOF  ,  IERROR,  IDVICE,               INITAL 
     *                  NBRIAP,  NBRPOD,  NBRASF,  NBRC9B,  NBRDOS,      INITAL 
     *                  IRECL ,  IFROM ,  ITO                            INITAL 
C                                                                        INITAL 
      REAL              WORK,  COST,  CPACTY                             INITAL 
C                                                                        INITAL 
      CHARACTER*2       PREFIX                                           INITAL 
      CHARACTER*6       FYLE ,  PLANE                                    INITAL 
      CHARACTER*7       FYLSTAT,FN                                       INITAL 
      CHARACTER*10      FYLACC                                           INITAL 
      CHARACTER*11      FYLFMT                                           INITAL 
      CHARACTER*36      MCCCARD                                          MODMCN 
      CHARACTER*30      ARCCARD                                          INITAL 
      CHARACTER*42      NODCARD                                          INITAL 
C                                                                        INITAL 
      LOGICAL           LWORK , FYLXIST , NMD , UNTXIST                  INITAL 
C                                                                        INITAL 
      DIMENSION         IWORK(IWKDIM), WORK(MCAT),  LWORK(NTYPEA)        INITAL 
C                                                                        INITAL 
C  ------------------------------------                                  INITAL 
C  ---G L O B A L   V A R I A B L E S .                                  INITAL 
C  ------------------------------------                                  INITAL 
C                                                                        INITAL 
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
C                                                                        INITAL 
      LOGICAL           RITE  ,  PODXIST, DOSXIST, ASFXIST               GLBLOG 
C                                                                        INITAL 
      CHARACTER*1       NSEW                                             GLBCHAR
      CHARACTER*3       ICATS                                            GLBCHAR
      CHARACTER*4       HOSPTL                                           GLBCHAR
      CHARACTER*6       AIRCRFT,  FYLNAM                                 GLBCHAR
      CHARACTER*8       LINES                                            GLBCHAR
      CHARACTER*50      ARCTXT,  NODTXT,  MCCTXT                         GLBCHAR
      CHARACTER*80      RUNLBL                                           GLBCHAR
      REAL              HSPCAP,  HSPTBD,  FACTOR,  ARCCIJ,  ARCUIJ       GLBREAL
C                                                                        INITAL 
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
C                                                                        INITAL 
C  ------------------------------                                        INITAL 
C  ---C O M M O N   B L O C K S .                                        INITAL 
C  ------------------------------                                        INITAL 
C                                                                        INITAL 
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
C                                                                        INITAL 
C  ----------------------------------                                    INITAL 
C  ---D A T A   S T A T E M E N T S .                                    INITAL 
C  ----------------------------------                                    INITAL 
C                                                                        INITAL 
      DATA  NBRIAP,NBRPOD,NBRASF,NBRC9B,NBRDOS /5*0/                     INITAL 
C                                                                        INITAL 
C  --------------------------------------                                INITAL 
C  ---F O R M A T   S T A T E M E N T S .                                INITAL 
C  --------------------------------------                                INITAL 
C                                                                        INITAL 
  800 FORMAT('1',///,14X,'I N P U T   F I L E   P R O C E S S I N G',    INITAL 
     *       '   S U M M A R Y',//,                                      INITAL 
     *       47X,'FILE',/,                                               INITAL 
     *       7X,'FILE',6X,'UNIT',5X,'ERROR',5X,'FILE',6X,'ACCESS',       INITAL 
     *            10X,'FORM',9X,'RECORD',/,                              INITAL 
     *       7X,'NAME',5X,'NUMBER',4X,'NUMBER',3X,'STATUS',5X,'METHOD',  INITAL 
     *            10X,'USED',9X,'LENGTH',/)                              INITAL 
  802 FORMAT('0',6X,A7,5X,I2,7X,I3,4X,A7,3X,A10,3X,A11,6X,I2,/)          INITAL 
  810 FORMAT('0',33X,'C A S U A L T Y   D A T A   S U M M A R Y',//,     INITAL 
     *       1X,15X,'MIN     OPG     SGS     SOR     SNS',               INITAL 
     *      '     SMF     SUR     SOR     SBN     STH     SCI')          INITAL 
  812 FORMAT('0','LENGTH',/,1X,'OF STAY',3X,11(5X,I3))                   INITAL 
  814 FORMAT('0','LITTER',/,1X,'AMBULATORY',/,1X,'INDICATOR ',           INITAL 
     *       11(7X,I1))                                                  INITAL 
  816 FORMAT('0','30-DAY',/,1X,'CATEGORY',/,1X,'PROPORTIONS',/,3X,       INITAL 
     *       'PERIOD')                                                   INITAL 
  818 FORMAT(' ',2X,I2,'-',I2,3X,11(3X,F5.3))                            INITAL 
  820 FORMAT('0','TOTAL',/,1X,'CASUALTIES',11(2X,I6),/)                  INITAL 
  822 FORMAT('0','RUN NAME: ',A80)                                       INITAL 
  830 FORMAT('1',//,34X,'M A S T E R   B A S E   T A B L E   D A T A',   INITAL 
     *       //,5X,'MASTER',68X,'IAP',13X,'ASF',/,                       INITAL 
     *          6X,'BASE',7X,'I N D I C A T O R S',13X,'I N D I C E S',  INITAL 
     *         14X,'CAPACITIES',7X,'STAGING',/,                          INITAL 
     *          5X,'RECORD',20X,'C-9',26X,'C-9',9X,'STAGING  TRANSIENT', INITAL 
     *             '   CAPACITY   LATITUDE   LONGITUDE',/,               INITAL 
     *          5X,'NUMBER',3X,2('IAP  APOD  ASF  BASE  DOS    '),       INITAL 
     *          28X,2(4X,'DEG   MIN'),/)                                 INITAL 
  832 FORMAT(6X,I3,6X,2(I1,5X,I1,4X),I1,5X,I3,2(2X,I3,3X,I3),            INITAL 
     *       5X,I4,6X,I4,7X,I4,2X,2(4X,I3,3X,I2))                        INITAL 
  834 FORMAT('1',//1X,'I A P   T A B L E   D A T A',//,1X,'MASTER',/,    INITAL 
     *       2X,'BASE',8X,'CAPACITIES',/,                                INITAL 
     *       1X,'INDEX    STAGING  TRANSIENT',/)                         INITAL 
  836 FORMAT(' ',2X,I3,6X,2(I4,6X))                                      INITAL 
  838 FORMAT('0',3X,'A P O D   T A B L E   D A T A',//,1X,               INITAL 
     *       'MASTER',16X,'ASSOCIATED',/,2X,'BASE',6X,'ASF',18X,'C-9',/, INITAL 
     *       1X,'INDEX   INDICATOR   ASF   DOS  BASE',/)                 INITAL 
  840 FORMAT(' ',1X,I3,7X,I3,3X,3(3X,I3))                                INITAL 
  842 FORMAT('0',4X,'A S F   T A B L E   D A T A',//,1X,                 INITAL 
     *       'MASTER',14X,'ASSOCIATED',/,2X,'BASE    STAGING',17X,'C-9', INITAL 
     *       /,1X,'INDES   CAPACITY   APOD   DOS   BASE',/)              INITAL 
  844 FORMAT(' ',1X,I3,7X,I3,1X,3(4X,I3))                                INITAL 
  846 FORMAT('0',4X,'D O S   T A B L E   D A T A',//,1X,                 INITAL 
     *       'MASTER',16X,'ASSOCIATED',/,2X,'BASE      ASF  ',17X,'C-9', INITAL 
     *       /,1X,'INDEX   INDICATOR  APOD   ASF   BASE',/)              INITAL 
  850 FORMAT('0',4X,'C - 9   T A B L E   D A T A',//,1X,                 INITAL 
     *       'MASTER',16X, 'ASSOCIATED',/,2X,'BASE',6X,'ASF',/,1X,       INITAL 
     *       'INDEX   INDICATOR   APOD  ASF   DOS',/)                    INITAL 
  860 FORMAT('1',//,34X,'A I R C R A F T   T A B L E   D A T A',//,      INITAL 
     *       16X,'TRUE   APPROACH',/,5X,                                 INITAL 
     *       'AIRCRAFT   AIR    LANDING    TAXI    MAX',5X,              INITAL 
     *       'GROUND TIMES (MINUTES)',7X,'PATIENT CAPACITY',/,7X,        INITAL 
     *       'TYPE    SPEED',2(5X,'TIME'),3X,'RANGE',5X,                 INITAL 
     *       'APOD   DOS  C-9 STOPS   LITTER  AMBULATORY  TOTAL',/)      INITAL 
  862 FORMAT(' ',5X,A6,4X,I3,2(7X,I2),4X,I5,6X,2(I3,3X),1X,I3,1X,        INITAL 
     *           3(7X,I3))                                               INITAL 
  870 FORMAT('1','ARC',/,1X,'ABBR   FROM    TO    COST',5X,              INITAL 
     *       'CAPACITY',14X,'ARC NAME',/)                                INITAL 
  872 FORMAT(' ',1X,A2,2(5X,I2),3X,F7.1,4X,F5.2,5X,A30,5X,               INITAL 
     *       I2,4(4X,I3,2X,I3))                                          INITAL 
  874 FORMAT('0','MCC CARD TEXTS',/)                                     INITAL 
  876 FORMAT(' ',A31)                                                    INITAL 
  878 FORMAT('0','NODE CARD TEXTS',/)                                    INITAL 
  880 FORMAT(' ',A42,5X,I2,3(4X,I3,2X,I3))                               INITAL 
  882 FORMAT('0','LOGICAL MATRIX RITE',/)                                INITAL 
  884 FORMAT(' ',30(2X,L1))                                              INITAL 
  886 FORMAT(' ','END OF INITAL.')                                       INITAL 
  900 FORMAT(11I2)                                                       INITAL 
  902 FORMAT(I5)                                                         MDN1208
  905 FORMAT(A6,1X,I2)                                                   INITAL 
  910 FORMAT(11I6)                                                       INITAL 
  915 FORMAT(11F5.3)                                                     INITAL 
  917 FORMAT(3X,I2,3X,11I6)                                              INITAL 
  918 FORMAT(A80)                                                        INITAL 
  920 FORMAT(37X,5I1,2I2,A1,I3,I2,A1,3I4)                                INITAL 
  922 FORMAT(A2,2I2,F7.1,F5.2,A30)                                       INITAL 
  923 FORMAT(I2,26I3)                                                    INITAL 
  924 FORMAT(A31)                                                        INITAL 
  925 FORMAT(A6,15I4)                                                    INITAL 
  926 FORMAT(A42)                                                        INITAL 
  927 FORMAT(30L1)                                                       INITAL 
  928 FORMAT(16I5)                                                       INITAL 
  940 FORMAT(1X, 'NO LENGTH OF STAY RECORD')                             INITAL 
  950 FORMAT(1X, 'ZERO LENGTH OF STAY VALUE')                            INITAL 
  960 FORMAT(1X, 'NO LITTER/AMBULATORY INDICATOR RECORD FOUND')          INITAL 
  962 FORMAT(1X, 'LITTER/AMBULATORY INDICATOR NOT VALID')                INITAL 
  965 FORMAT(1X, 'MISSING LOAD FACTOR RECORD')                           INITAL 
  966 FORMAT(1X, 'MISSING TOTAL CASUALTIES BY PERIOD')                   INITAL 
  970 FORMAT(1X, 'MISSING AIRCRAFT DATA RECORD(S)')                      INITAL 
  978 FORMAT(1X, 'MISSING MCC CARD FORMAT')                              INITAL 
  979 FORMAT(1X, 'MISSING NODE CARD TEXT RECORD')                        INITAL 
  980 FORMAT(1X, 'MISSING RUN NAME')                                     INITAL 
  981 FORMAT(1X, 'MISSING NODE CARD FORMAT RECORD')                      INITAL 
  982 FORMAT(1X, 'MISSING WRITE SWITCH RECORD')                          INITAL 
  983 FORMAT(1X, 'MISSING DAILY C-9 CAPACITY RECORD(S)')                 INITAL 
  985 FORMAT(1X, 'MISSING DATA FILES')                                   INITAL 
  986 FORMAT(1X, 'FILE ',A6,' DOES NOT EXIST')                           INITAL 
  989 FORMAT(1X, 'UNIT ',I4,' DOES NOT EXIST')                           INITAL 
  990 FORMAT(1X, 'MISSING MASTER BASE RECORDS')                          INITAL 
 1920 FORMAT(1X, 'BEGINNING PARAMETER PROCESSING')                       INITAL 
 1940 FORMAT(1X, 'PARAMETERS PROCESSED; BEGINNING BED DATA INPUT')       INITAL 
 1950 FORMAT(1X, 'BED DATA PROCESSED; INITIALIZING BED AVAILABILITY')    INITAL 
 9110 FORMAT(1X, 'ERROR NUMBER=',I6,' IOSTAT=',I4)                       INITAL 
 9120 FORMAT(1X, 'ERROR NUMBER=',I6,'   APOD',I3,'IS NOT ALSO A DOS',/)  INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C *                                                                    * INITAL 
C ************************  E N T R A N C E  *************************** INITAL 
C *                                                                    * INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      PRINT 1920                                                         INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 2A.                                                        ** INITAL 
C ***         READ IN LENGTH OF RUN (MAXDAYS).                       *** MDN1208
C ***         READ IN LENGTH OF STAY FOR EACH MEDICAL CATEGORY.      *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      READ(2,902) MAXDAYS                                                MDN1208
C The following line of code inserted by Gary Schultz
C so that the problem name appears in the data.
      write(88,*) 'Patient Distribution System, ', MAXDAYS, ' days.'
  200 ASSIGN 900 TO IFORMAT                                              INITAL 
      ASSIGN 940 TO IEOF                                                 INITAL 
      IERRNBR = 9908                                                     INITAL 
      NBRREC  = 0                                                        INITAL 
      READ(2,900,END=9000,ERR=9100,IOSTAT=IOS)(IWORK(J),J=1,MCAT)        INITAL 
      NBRREC  = 1                                                        INITAL 
      DO 210 I=1,MCAT                                                    INITAL 
        IF(IWORK(I).EQ.0)                                                INITAL 
     *          THEN                                                     INITAL 
                   ASSIGN 950 TO IDATA                                   INITAL 
                   GO TO 9200                                            INITAL 
                ELSE                                                     INITAL 
                   LOS(I)=IWORK(I)                                       INITAL 
             ENDIF                                                       INITAL 
  210 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 2B.                                                       *** INITAL 
C ***         READ IN LITTER/AMBULATORY INDICATOR FOR EACH CATEGORY. *** INITAL 
C ***         (1 INDICATES LITTER, 0 INDICATES AMBULATORY)           *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 960 TO IEOF                                                 INITAL 
      READ(2,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)(IWORK(J),J=1,MCAT)    INITAL 
      DO 220 ICAT=1,MCAT                                                 INITAL 
         IF(.NOT.(IWORK(ICAT).EQ.1.OR.IWORK(ICAT).EQ.0)) THEN            INITAL 
            ASSIGN 962 TO IDATA                                          INITAL 
            GO TO 9200                                                   INITAL 
         ELSE                                                            INITAL 
            LITAMB(ICAT)=IWORK(ICAT)                                     INITAL 
         ENDIF                                                           INITAL 
  220 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 2C.                                                       *** INITAL 
C ***         READ IN CATEGORY PROPORTIONS.                          *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 915 TO IFORMAT                                              INITAL 
      ASSIGN 965 TO IEOF                                                 INITAL 
      DO 230 I30DAY=1,3                                                  INITAL 
             READ(2,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)                INITAL 
     *           (WORK(J),J=1,MCAT)                                      INITAL 
             DO 240 ICAT=1,MCAT                                          INITAL 
                    FACTOR(I30DAY,ICAT)=WORK(ICAT)                       INITAL 
  240        CONTINUE                                                    INITAL 
  230 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 2D.                                                       *** INITAL 
C ***         READ IN TOTAL COUNT OF PATIENTS TO BE MOVED            *** INITAL 
C ***         IN EACH TEN DAY PERIOD                                 *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 910 TO IFORMAT                                              INITAL 
      ASSIGN 966 TO IEOF                                                 INITAL 
      MAX10D  = MDAYS/10                                                 INITAL 
      READ(2,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)(IWORK(J),J=1,9)       INITAL 
      DO 250 I10DAY=1,MAX10D                                             INITAL 
             ITCBP(I10DAY)=IWORK(I10DAY)                                 INITAL 
  250 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 2E.                                                       *** INITAL 
C ***         CALCULATE CASUALTIES TO BE MOVED BY DAY AND CATEGORY.  *** INITAL 
C ***         COMPUTE CUMULATIVE TOTALS BY CATEGORY.                 *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      DO 260 IDAY=1,MDAYS                                                INITAL 
             DO 270 ICAT=1,MCAT                                          INITAL 
                    I10DAY = 1 + (IDAY - 1)*.1                           INITAL 
                    I30DAY = 1 + (I10DAY*.333333333)                     INITAL 
                    ICASLT(IDAY,ICAT) =                                  INITAL 
     *                 IFIX(.5+(.1)*FACTOR(I30DAY,ICAT)*ITCBP(I10DAY))   INITAL 
                    IDMAND(ICAT) = IDMAND(ICAT) + ICASLT(IDAY,ICAT)      INITAL 
  270        CONTINUE                                                    INITAL 
  260 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 2F.                                                       *** INITAL 
C ***        READ IN RUN NAME TO IDENTIFY DATA SET USED.             *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 918 TO IFORMAT                                              INITAL 
      ASSIGN 980 TO IEOF                                                 INITAL 
      READ(2,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS) RUNLBL                INITAL 
      PRINT 1940                                                         INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 2G.                                                       *** INITAL 
C ***         WRITE OUT ECHO-CHECK OF CASUALTY DATA                  *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      WRITE(88,810)                                                      INITAL 
      WRITE(88,812) (LOS(ICAT),ICAT=1,MCAT)                              INITAL 
      WRITE(88,814) (LITAMB(ICAT),ICAT=1,MCAT)                           INITAL 
      WRITE(88,816)                                                      INITAL 
      DO 280 I30DAY=1,3                                                  INITAL 
         IDAY1 = (I30DAY - 1) * 30 + 1                                   INITAL 
         IDAY2 = I30DAY * 30                                             INITAL 
         WRITE(88,818) IDAY1,IDAY2,(FACTOR(I30DAY,ICAT),ICAT=1,MCAT)     INITAL 
  280 CONTINUE                                                           INITAL 
      WRITE(88,820) (IDMAND(ICAT),ICAT=1,MCAT)                           INITAL 
      WRITE(88,822) RUNLBL                                               INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 3A.                                                       *** INITAL 
C ***        READ IN BED AVAILABILITY DATA FOR EACH HOSPITAL, FOR    *** INITAL 
C ***        EACH SCENARIO DAY AND CATEGORY.  SINCE THERE ARE MORE   *** INITAL 
C ***        HOSPITALS THAN C-9 BASES SERVING THEM, BEDS AT DIF-     *** INITAL 
C ***        FERENT HOSPITALS SERVED BY THE SAME ("C-9 SERVICING")   *** INITAL 
C ***        BASES ARE SUMMED TOGETHER.                              *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 917 TO IFORMAT                                              INITAL 
      ASSIGN 985 TO IEOF                                                 INITAL 
      DO 300 IHSPTL=1,MHOS                                               INITAL 
             DO 310 IDAY=1,MDAYS                                         INITAL 
                    READ(3,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)         INITAL 
     *                   ISERV,(IWORK(ICAT),ICAT=1,MCAT)                 INITAL 
                    DO 320 ICAT=1,MCAT                                   INITAL 
                           IF(IWORK(ICAT).LT.0)IWORK(ICAT)=0             INITAL 
                           BEDATA=FLOAT(IWORK(ICAT))                     INITAL 
                           HSPCAP(IDAY,ICAT,ISERV) =                     INITAL 
     *                        HSPCAP(IDAY,ICAT,ISERV) + BEDATA           INITAL 
                              HSPTBD(IDAY,ISERV) = HSPTBD(IDAY,ISERV) +  INITAL 
     *                                             BEDATA                INITAL 
  320               CONTINUE                                             INITAL 
  310        CONTINUE                                                    INITAL 
  300 CONTINUE                                                           INITAL 
      IDAY=0                                                             INITAL 
      PRINT 1950                                                         INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 4A.                                                       *** INITAL 
C ***         READ IN THE MASTER BASE FILE. CREATE EACH MASTER BASE  *** INITAL 
C ***         TABLE ROW. CREATE POINTERS BETWEEN EACH ADDITIONAL     *** INITAL 
C ***         TABLE ENTRY AND THE MASTER BASE TABLE IN BOTH          *** INITAL 
C ***         DIRECTIONS.  IF INDICATOR VALUE(S) = 1, CREATE         *** INITAL 
C ***         ADDITIONAL TABLE ENTRIES.                              *** INITAL 
C ********************************************************************** INITAL 
      ASSIGN 920 TO IFORMAT                                              INITAL 
      ASSIGN 990 TO IEOF                                                 INITAL 
      NBRREC = 0                                                         INITAL 
      DO 400 NBRBAS = 1, MBASES                                          INITAL 
             READ(4,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)                INITAL 
     *           (IWORK(I),I=1,5),(IWORK(I),I=6,7),NSEW(NBRBAS,1),       INITAL 
     *           (IWORK(I),I=8,9), NSEW(NBRBAS,2),(IWORK(I),I=10,12)     INITAL 
             DO 410 J = 1,12                                             INITAL 
                    IBASES(NBRBAS,J) = IWORK(J)                          INITAL 
  410        CONTINUE                                                    INITAL 
             NBRIAP = NBRIAP + IBASES(NBRBAS,INDIAP)                     INITAL 
             NBRPOD = NBRPOD + IBASES(NBRBAS,INDPOD)                     INITAL 
             NBRASF = NBRASF + IBASES(NBRBAS,INDASF)                     INITAL 
             NBRC9B = NBRC9B + IBASES(NBRBAS,INDC9B)                     INITAL 
             NBRDOS = NBRDOS + IBASES(NBRBAS,INDDOS)                     INITAL 
             IF(IBASES(NBRBAS,INDIAP) .GE. 1)                            INITAL 
     *         THEN                                                      INITAL 
                    IBASES(NBRBAS,NDXIAP) = NBRIAP                       INITAL 
                    IAPDTL(NBRIAP,LNKIAP) = NBRBAS                       INITAL 
                    IAPDTL(NBRIAP,IAPSMX) = IBASES(NBRBAS,MAXSTG)        INITAL 
                    IAPDTL(NBRIAP,IAPTMX) = IBASES(NBRBAS,MAXTRX)        INITAL 
                    IAPDTL(NBRIAP,IAPPNN) = 0                            INITAL 
                    IAPDTL(NBRIAP,IAPPDY) = 0                            INITAL 
             ENDIF                                                       INITAL 
             IF(IBASES(NBRBAS,INDASF) .GE. 1)                            INITAL 
     *         THEN                                                      INITAL 
                    IBASES (NBRBAS,NDXASF ) = NBRASF                     INITAL 
                    IASFDTL(NBRASF,LNKASF ) = NBRBAS                     INITAL 
                    IASFDTL(NBRASF,IASFMX ) = IBASES(NBRBAS,MAXASF)      INITAL 
                    IASFDTL(NBRASF,IASFPOD) = IBASES(NBRBAS,INDPOD)*     INITAL 
     *                                        NBRPOD                     INITAL 
                    IASFDTL(NBRASF,IASFDOS) = IBASES(NBRBAS,INDDOS)*     INITAL 
     *                                        NBRDOS                     INITAL 
                    IASFDTL(NBRASF,IASFC9B) = IBASES(NBRBAS,INDC9B)*     INITAL 
     *                                        NBRC9B                     INITAL 
                    IASFDTL(NBRASF,IASFPNN) = 0                          INITAL 
                    IASFDTL(NBRASF,IASFPDY) = 0                          INITAL 
                    IASFDTL(NBRASF,IASFPTM) = 0                          INITAL 
             ENDIF                                                       INITAL 
             IF(IBASES(NBRBAS,INDDOS) .GE. 1)                            INITAL 
     *         THEN                                                      INITAL 
                   IBASES (NBRBAS,NDXDOS )= NBRDOS                       INITAL 
                   IDOSDTL(NBRDOS,LNKDOS )= NBRBAS                       INITAL 
                   IDOSDTL(NBRDOS,IDOSIND)= IBASES(NBRBAS,INDASF)        INITAL 
                   IDOSDTL(NBRDOS,IDOSASF)= IBASES(NBRBAS,INDASF)*NBRASF INITAL 
                   IDOSDTL(NBRDOS,IDOSC9B)= IBASES(NBRBAS,INDC9B)*NBRC9B INITAL 
                   IDOSDTL(NBRDOS,IDOSPOD)= IBASES(NBRBAS,INDPOD)*NBRPOD INITAL 
             ENDIF                                                       INITAL 
             IF(IBASES(NBRBAS,INDPOD) .GE. 1)                            INITAL 
     *         THEN                                                      INITAL 
                    IBASES(NBRBAS,NDXPOD) = NBRPOD                       INITAL 
                    IPODDTL(NBRPOD,LNKPOD)= NBRBAS                       INITAL 
                    IPODDTL(NBRPOD,IPODIND)=IBASES(NBRBAS,INDPOD)        INITAL 
                    IPODDTL(NBRPOD,IPODASF)=IBASES(NBRBAS,INDASF)        INITAL 
     *                                      * NBRASF                     INITAL 
                    IPODDTL(NBRPOD,IPODC9B)=IBASES(NBRBAS,INDC9B)        INITAL 
     *                                      * NBRC9B                     INITAL 
                    IPODDTL(NBRPOD,IPODDOS)=IBASES(NBRBAS,INDDOS)        INITAL 
     *                                      * NBRDOS                     INITAL 
C                                                                        INITAL 
C                   BECAUSE IDMAT ONLY CONTAINS DISTANCES FROM DOS'S     INITAL 
C                   TO C-9 BASES, AN APOD MUST ALSO BE A DOS (FOR        INITAL 
C                   CALLS TO CRTLCL).  IF THE APOD IS NOT ALSO A DOS,    INITAL 
C                   PRINT AN ERROR MESSAGE AND TERMINATE EXECUTION.      INITAL 
C                                                                        INITAL 
                    IF (IPODDTL(NBRPOD,IPODDOS) .EQ. 0) GO TO 9500       INITAL 
C                                                                        INITAL 
             ENDIF                                                       INITAL 
             IF(IBASES(NBRBAS,INDC9B) .GE. 1)                            INITAL 
     *         THEN                                                      INITAL 
                    IBASES(NBRBAS,NDXC9B) = NBRC9B                       INITAL 
                    IC9DTL(NBRC9B,LNKC9B) = NBRBAS                       INITAL 
                    IC9DTL(NBRC9B,IC9IND) = IBASES(NBRBAS,INDC9B)        INITAL 
                    IC9DTL(NBRC9B,IC9ASF) = IBASES(NBRBAS,INDASF)        INITAL 
     *                                      * NBRASF                     INITAL 
                    IC9DTL(NBRC9B,IC9POD) = IBASES(NBRBAS,INDPOD)        INITAL 
     *                                      * NBRPOD                     INITAL 
                    IC9DTL(NBRC9B,IC9DOS) = IBASES(NBRBAS,INDDOS)        INITAL 
     *                                      * NBRDOS                     INITAL 
             ENDIF                                                       INITAL 
  400 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 4B.                                                       *** INITAL 
C ***         WRITE DATA TABLES TO SUMMARY FILE.                     *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      WRITE(88,830)                                                      INITAL 
      DO 420 IBASE=1,MBASES                                              INITAL 
         WRITE(88,832) IBASE,IBASES(IBASE,INDIAP),IBASES(IBASE,INDPOD),  INITAL 
     *                       IBASES(IBASE,INDASF),IBASES(IBASE,INDC9B),  INITAL 
     *                       IBASES(IBASE,INDDOS),IBASES(IBASE,NDXIAP),  INITAL 
     *                       IBASES(IBASE,NDXPOD),IBASES(IBASE,NDXASF),  INITAL 
     *                       IBASES(IBASE,NDXC9B),IBASES(IBASE,NDXDOS),  INITAL 
     *                       IBASES(IBASE,MAXSTG),IBASES(IBASE,MAXTRX),  INITAL 
     *                       IBASES(IBASE,MAXASF),IBASES(IBASE,LATDEG),  INITAL 
     *                       IBASES(IBASE,LATMIN),IBASES(IBASE,LNGDEG),  INITAL 
     *                       IBASES(IBASE,LNGMIN)                        INITAL 
  420 CONTINUE                                                           INITAL 
      WRITE(88,834)                                                      INITAL 
      DO 440 IAP=1,MIAPS                                                 INITAL 
         WRITE(88,836) IAPDTL(IAP,LNKIAP), IAPDTL(IAP,IAPSMX),           INITAL 
     *                 IAPDTL(IAP,IAPTMX)                                INITAL 
  440 CONTINUE                                                           INITAL 
      WRITE(88,838)                                                      INITAL 
      DO 450 IPOD=1,MAPOD                                                INITAL 
         WRITE(88,840) IPODDTL(IPOD,LNKPOD ), IPODDTL(IPOD,IPODIND),     INITAL 
     *                 IPODDTL(IPOD,IPODASF), IPODDTL(IPOD,IPODDOS),     INITAL 
     *                 IPODDTL(IPOD,IPODC9B)                             INITAL 
  450 CONTINUE                                                           INITAL 
      WRITE(88,842)                                                      INITAL 
      DO 455 IASF=1,MASF                                                 INITAL 
         WRITE(88,844) IASFDTL(IASF,LNKASF), IASFDTL(IASF,IASFMX),       INITAL 
     *                 IASFDTL(IASF,IASFPOD), IASFDTL(IASF,IASFDOS),     INITAL 
     *                 IASFDTL(IASF,IASFC9B)                             INITAL 
  455 CONTINUE                                                           INITAL 
      WRITE(88,846)                                                      INITAL 
      DO 460 IDOS=1,MDOS                                                 INITAL 
         WRITE(88,840) IDOSDTL(IDOS,LNKDOS), IDOSDTL(IDOS,IDOSIND),      INITAL 
     *                 IDOSDTL(IDOS,IDOSPOD), IDOSDTL(IDOS,IDOSASF),     INITAL 
     *                 IDOSDTL(IDOS,IDOSC9B)                             INITAL 
  460 CONTINUE                                                           INITAL 
      WRITE(88,850)                                                      INITAL 
      DO 465 IC9B=1,MC9BAS                                               INITAL 
         WRITE(88,840) IC9DTL(IC9B,LNKC9B), IC9DTL(IC9B,IC9IND),         INITAL 
     *                 IC9DTL(IC9B,IC9POD), IC9DTL(IC9B,IC9ASF),         INITAL 
     *                 IC9DTL(IC9B,IC9DOS)                               INITAL 
  465 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 5A.                                                       *** INITAL 
C ***         READ IN AIRCRAFT DATA.                                 *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 925 TO IFORMAT                                              INITAL 
      ASSIGN 970 TO IEOF                                                 INITAL 
      DO 500 IPLANE = 1, MAXAC                                           INITAL 
             READ(5,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)PLANE,          INITAL 
     *                  (IWORK(J),J=1,10)                                INITAL 
             IACRAFT(IPLANE,ITAS)  = IWORK(ITAS)                         INITAL 
             IACRAFT(IPLANE,IALTIM)= IWORK(IALTIM)                       INITAL 
             IACRAFT(IPLANE,ITAXI) = IWORK(ITAXI)                        INITAL 
             IACRAFT(IPLANE,MAXRNG)= IWORK(MAXRNG)                       INITAL 
             IACRAFT(IPLANE,IPODGT)= IWORK(IPODGT)                       INITAL 
             IACRAFT(IPLANE,IDOSGT)= IWORK(IDOSGT)                       INITAL 
             IACRAFT(IPLANE,IC9GT) = IWORK(IC9GT)                        INITAL 
             IACRAFT(IPLANE,MAXLIT)= IWORK(MAXLIT)                       INITAL 
             IACRAFT(IPLANE,MAXAMB)= IWORK(MAXAMB)                       INITAL 
             IACRAFT(IPLANE,MAXTOT)= IWORK(MAXTOT)                       INITAL 
             AIRCRFT(IPLANE)       = PLANE                               INITAL 
  500 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C *** STEP 5B.                                                        ** INITAL 
C ***        READ IN DAILY C-9 TOTAL SYSTEM CAPACITIES               *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 928 TO IFORMAT                                              INITAL 
      ASSIGN 983 TO IEOF                                                 INITAL 
      READ(5,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)                       INITAL 
     *           (IWORK(I),I=1,MDAYS)                                    INITAL 
      DO 510 IDAY = 1, MDAYS                                             INITAL 
             MAXC9(IDAY) = IWORK(IDAY)                                   INITAL 
  510 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 5C.                                                       *** INITAL 
C ***         WRITE AIRCRAFT DATA TO SUMMARY FILE.                   *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      WRITE(88,860)                                                      INITAL 
      DO 520 IAC=1,MAXAC                                                 INITAL 
         WRITE(88,862) AIRCRFT(IAC), IACRAFT(IAC,ITAS),                  INITAL 
     *                 IACRAFT(IAC,IALTIM), IACRAFT(IAC,ITAXI),          INITAL 
     *                 IACRAFT(IAC,MAXRNG), IACRAFT(IAC,IPODGT),         INITAL 
     *                 IACRAFT(IAC,IDOSGT), IACRAFT(IAC,IC9GT),          INITAL 
     *                 IACRAFT(IAC,MAXLIT), IACRAFT(IAC,MAXAMB),         INITAL 
     *                 IACRAFT(IAC,MAXTOT)                               INITAL 
  520 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C *** STEP 6A.                                                       *** INITAL 
C ***         READ IN ARC CARD TEXT AND FORMAT SPECIFICATIONS        *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 922 TO IFORMAT                                              INITAL 
c     ASSIGN 976 TO IEOF                                                 INITAL 
      IERRNBR = 9920                                                     INITAL 
      DO 600 ICARD = 1, NTYPEA                                           INITAL 
             READ(16,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)                INITAL 
     *                  PREFIX,IFROM,ITO,COST,CPACTY,ARCCARD             INITAL 
             ARCTXT(ICARD)( 3:10) = ARCCARD( 1: 8)                       INITAL 
             ARCTXT(ICARD)(11:26) = ARCCARD(15:30)                       INITAL 
             ARCTXT(ICARD)(1:2 ) = PREFIX                                INITAL 
             IARCS(ICARD,NODEFR) = IFROM                                 INITAL 
             IARCS(ICARD,NODETO) = ITO                                   INITAL 
             ARCCIJ(ICARD)       = COST                                  INITAL 
             DO 605 JCAT = 1, MCAT                                       INITAL 
                    ARCUIJ(ICARD,JCAT) = CPACTY                          INITAL 
  605        CONTINUE                                                    INITAL 
  600 CONTINUE                                                           INITAL 
      ASSIGN 923 TO IFORMAT                                              INITAL 
c     ASSIGN 977 TO IEOF                                                 INITAL 
      IERRNBR = 9922                                                     INITAL 
      DO 610 ICARD = 1, NTYPEA                                           INITAL 
             READ(16,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)                INITAL 
     *                  (IWORK(J),J=1,27)                                INITAL 
             DO 620 ITEM = 1, 27                                         INITAL 
                    IACFMT(ICARD,ITEM) = IWORK(ITEM)                     INITAL 
  620        CONTINUE                                                    INITAL 
  610 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 6B.                                                       *** INITAL 
C ***         READ IN MCC CARD TEXT AND FORMAT SPECIFICATIONS.       *** MODMCN 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 924 TO IFORMAT                                              INITAL 
      ASSIGN 978 TO IEOF                                                 INITAL 
      IERRNBR = 9924                                                     INITAL 
      DO 625 ICARD = 1, NTYPEM                                           INITAL 
             READ(16,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)                INITAL 
     *                  MCCCARD                                          INITAL 
             MCCTXT(ICARD)(1:36) = MCCCARD                               MODMCN 
  625 CONTINUE                                                           INITAL 
C                                                                        MODMCN 
      ASSIGN 923 TO IFORMAT                                              MODMCN 
      IERRNBR = 9925                                                     MODMCN 
      DO 628 ICARD = 1, NTYPEM                                           MODMCN 
             READ(16,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)                MODMCN 
     *           (IWORK(J),J=1,27)                                       MODMCN 
             DO 626 ITEM = 1, 27                                         MODMCN 
                    MCCFMT(ICARD,ITEM) = IWORK(ITEM)                     MODMCN 
  626        CONTINUE                                                    MODMCN 
  628 CONTINUE                                                           MODMCN 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C *** STEP 6C.                                                       *** INITAL 
C ***         READ IN NODE CARD TEXT AND FORMAT SPECIFICATIONS.      *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 926 TO IFORMAT                                              INITAL 
      ASSIGN 979 TO IEOF                                                 INITAL 
      IERRNBR = 9926                                                     INITAL 
      DO 630 ICARD = 1, NTYPEN                                           INITAL 
             READ(16,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)                INITAL 
     *                  NODCARD                                          INITAL 
             NODTXT(ICARD)(1:42) = NODCARD                               INITAL 
  630 CONTINUE                                                           INITAL 
      ASSIGN 923 TO IFORMAT                                              INITAL 
      ASSIGN 981 TO IEOF                                                 INITAL 
      IERRNBR = 9928                                                     INITAL 
      DO 640 ICARD = 1, NTYPEN                                           INITAL 
             READ(16,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)                INITAL 
     *                  (IWORK(J),J=1,27)                                INITAL 
             DO 650 ITEM = 1, 27                                         INITAL 
                    NODFMT(ICARD,ITEM) = IWORK(ITEM)                     INITAL 
  650        CONTINUE                                                    INITAL 
  640 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C *** STEP 6D.                                                       *** INITAL 
C ***         READ IN THE LOGICAL MATRIX RITE. THE THREE ROW CORRESPOND* INITAL 
C ***         TO NODES, ARCS AND MUTUAL CAPACITY CONSTRAINTS.        *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      ASSIGN 927 TO IFORMAT                                              INITAL 
      ASSIGN 982 TO IEOF                                                 INITAL 
      IERRNBR = 9930                                                     INITAL 
      JLIMIT  = MAX0(NTYPEA,NTYPEN,NTYPEM)                               INITAL 
      DO 660 ICARD = 1, 3                                                INITAL 
             READ(16,IFORMAT,END=9000,ERR=9100,IOSTAT=IOS)                INITAL 
     *                  (LWORK(J),J=1,JLIMIT)                            INITAL 
             DO 670 J = 1, JLIMIT                                        INITAL 
                    RITE(ICARD,J) = LWORK(J)                             INITAL 
  670        CONTINUE                                                    INITAL 
  660 CONTINUE                                                           INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** STEP 6E.                                                       *** INITAL 
C ***         WRITE ARC CARD, NODE CARD, AND MCC CARD INPUT TO       *** INITAL 
C ***         SUMMARY FILE.                                          *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
      WRITE(88,870)                                                      INITAL 
      DO 680 I=1,NTYPEA                                                  INITAL 
         WRITE(88,872) ARCTXT(I)(1:2),IARCS(I,NODEFR),IARCS(I,NODETO),   INITAL 
     *                 ARCCIJ(I),ARCUIJ(I,1),ARCTXT(I)(3:32),            INITAL 
     *                 (IACFMT(I,J),J=1,9)                               INITAL 
  680 CONTINUE                                                           INITAL 
      WRITE(88,874)                                                      INITAL 
      DO 690 I=1,NTYPEM                                                  INITAL 
         WRITE(88,876) MCCTXT(I)(1:36)                                   MODMCN 
  690 CONTINUE                                                           INITAL 
      WRITE(88,878)                                                      INITAL 
      DO 700 I=1,NTYPEN                                                  INITAL 
         WRITE(88,880) NODTXT(I)(1:42),(NODFMT(I,J),J=1,7)               INITAL 
  700 CONTINUE                                                           INITAL 
      WRITE(88,882)                                                      INITAL 
      DO 710 I=1,3                                                       INITAL 
         WRITE(88,884) (RITE(I,J),J=1,JLIMIT)                            INITAL 
  710 CONTINUE                                                           INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** NORMAL TERMINATION                                             *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
C                                                                        INITAL 
      GO TO 2000                                                         INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C ***                                                                *** INITAL 
C *** ABNORMAL TERMINATION                                           *** INITAL 
C ***                                                                *** INITAL 
C ********************************************************************** INITAL 
C                                                                        INITAL 
 9000 CONTINUE                                                           INITAL 
      WRITE(99,IEOF)                                                     INITAL 
      STOP'END OF FILE ERROR'                                            INITAL 
 9100 CONTINUE                                                           INITAL 
      WRITE(99,IERROR)IERRNBR,IOS                                        INITAL 
      STOP 'I/O ERROR'                                                   INITAL 
 9200 CONTINUE                                                           INITAL 
      WRITE(99,IDATA)                                                    INITAL 
      STOP'DATA VALUE ERROR'                                             INITAL 
 9300 CONTINUE                                                           INITAL 
      WRITE(99,IFILE)FYLE                                                INITAL 
      STOP 'MISSING DATA FILE'                                           INITAL 
 9400 CONTINUE                                                           INITAL 
      WRITE(99,IFILE) IDVICE                                             INITAL 
      STOP 'INVALID DEVICE NUMBER'                                       INITAL 
 9500 CONTINUE                                                           INITAL 
      IERRNBR = 9940                                                     INITAL 
      WRITE(99,9120) IERRNBR,NBRPOD                                      INITAL 
      STOP 'APOD-DOS ERROR'                                              INITAL 
C                                                                        INITAL 
C ********************************************************************** INITAL 
C *                                                                    * INITAL 
C ****************************  E X I T  ******************************* INITAL 
C *                                                                    * INITAL 
C ********************************************************************** INITAL 
 2000 PRINT 886                                                          INITAL 
      RETURN                                                             INITAL 
      END                                                                INITAL 
