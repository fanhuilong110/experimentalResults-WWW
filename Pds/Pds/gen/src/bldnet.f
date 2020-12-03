      SUBROUTINE BLDNET                                                  BLDNET 
C                                                                        BLDNET 
C ********************************************************************** BLDNET 
C *                                                                    * BLDNET 
C *********************  D E S C R I P T I O N  ************************ BLDNET 
C *                                                                    * BLDNET 
C ********************************************************************** BLDNET 
C                                                                        BLDNET 
C     SUBROUTINE BLDNET (BUILD NETWORK) CONSTRUCTS THE PATIENT FLOW      BLDNET 
C     NETWORK.  IT CALLS THREE ROUTINES. CRTSNK FIRST CONTRUCTS THE      BLDNET 
C     PATIENT FLOW SOURCES AND SINKS FROM PATIENT DEMAND DATA.  THE      BLDNET 
C     ACTUAL FLOW NETWORK IS CONSTRUCTED FROM AN AIRCRAFT ITINERARY      BLDNET 
C     DATA FILE CONTAINING ALL AIRCRAFT FLIGHT SEGMENTS. TO ACCOUNT      BLDNET 
C     FOR PATIENTS STILL IN THE SYSTEM AT THE END OF THE SCENARIO,       BLDNET 
C     CRTIPN CREATES IN-PROCESS NODES TO FLOW PATIENTS TO THE SINK.      BLDNET 
C                                                                        BLDNET 
C ********************************************************************** BLDNET 
C *                                                                    * BLDNET 
C ***************************  S T A T U S  **************************** BLDNET 
C *                                                                    * BLDNET 
C ********************************************************************** BLDNET 
C                                                                        BLDNET 
C      ORIGINAL AUTHOR        : MCLAIN                                   BLDNET 
C      ORIGINAL VERSION DATE  : 12/28/83                                 BLDNET 
C      REVISIONS              : 12/19/84  DOCUMENTATION ADDED.           BLDNET 
C                                                                        BLDNET 
C ********************************************************************** BLDNET 
C *                                                                    * BLDNET 
C ************************  E N T R A N C E  *************************** BLDNET 
C *                                                                    * BLDNET 
C ********************************************************************** BLDNET 
C ***                                                                *** BLDNET 
C *** STEP 1.                                                        *** BLDNET 
C ***        CALL ROUTINES TO CREATE PATIENT SUPPLIES AND            *** BLDNET 
C ***        DEMANDS (CRTSNK), BUILD THE PATIENT FLOW NETWORK        *** BLDNET 
C ***        (BLDDPB), AND ADD AN ADDITIONAL SUBNETWORK THAT         *** BLDNET 
C ***        WILL FLOW ALL PATIENTS REMAINING IN THE SYSTEM ON       *** BLDNET 
C ***        THE LAST DAY OF THE SCENARIO TO THE SINK (CRTIPN).      *** BLDNET 
C ***                                                                *** BLDNET 
C ********************************************************************** BLDNET 
C                                                                        BLDNET 
       CALL CRTSNK                                                       BLDNET 
      PRINT 100                                                          BLDNET 
       CALL BLDDPB                                                       BLDNET 
      PRINT 110                                                          BLDNET 
       CALL CRTIPN                                                       BLDNET 
      PRINT 120                                                          BLDNET 
C                                                                        BLDNET 
C ********************************************************************** BLDNET 
C *                                                                    * BLDNET 
C ****************************  E X I T  ******************************* BLDNET 
C *                                                                    * BLDNET 
C ********************************************************************** BLDNET 
C                                                                        BLDNET 
  100 FORMAT(1X,'COMPLETED CALL TO CRTSNK IN BLDNET')                    BLDNET 
  110 FORMAT(1X,'COMPLETED CALL TO BLDDPB IN BLDNET')                    BLDNET 
  120 FORMAT(1X,'COMPLETED CALL TO CRTIPN IN BLDNET')                    BLDNET 
      RETURN                                                             BLDNET 
      END                                                                BLDNET 
