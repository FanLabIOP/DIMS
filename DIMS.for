*******************************************************************************
*                          DIMS (stand-alone version)                         *
*                                                                             *
*            Direct Methods for Incommensurate Modulated Structures           *
*                                                                             *
*              Original: Fu Zheng-qing & Fan Hai-fu,  1992 - 1994             *
*        Revised: Wan Zheng-hua, Liu Yu-dong & Fan Hai-fu,  1995, 2000        *
*                                                                             *
*   Institute of Physics, Chinese Academy of Sciences, Beijing, P. R. China   *
*                                                                             *
*******************************************************************************
      USE MSFLIB
      INTEGER*2 N1,NuArgs,STATUS
      INTEGER FILENAMELEN,NLC
C     CHARACTER*8 HSS,HEE
      CHARACTER TITLE*128,SPACE*1,PERIOD*1,TBL*1
      CHARACTER*80 FILE1,FILE2,FILE5,FILE_IN
      CHARACTER*36 SY_SPG
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),H(4),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH,PHSHIFT
      INTEGER*4 IHP3(20000),NFSET(0:6)
      DIMENSION NCHLD(80)
      INTEGER HV(36),HC(36),TCL(4,4),MMX(48,4,4),MTRANL(48,4)
      REAL    QVECT(3)
C
      COMMON /CZ0/FILE1,FILE2,FILE5
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
      COMMON /CZ25/ Y1(50),Y2(50),Y3(50),NWILSTEP
      COMMON /CZ26/HV,HC
C
      SY_SPG='Direct from Generators'
      NLC=1
      SPACE=' '
      PERIOD='.'
      TBL=CHAR(9)
C      CALL TIME(HSS)
      IDIN=4
      IALTER=0
      RADI=0
      SKAMAX=50.0
      EKAPAPER=0.2
      MREL=300
      NSETM=1024
      NSETM0=1024
      ISKIP=0
      ISKIP0=0
      INDIC=0
      DO I=0,6
        NFSET(I)=0
      END DO
        NCLMAX=10
      CYCLCTR=0.005
      NCYCFIX=2
      W1=0.0
      W2=1.5
      W3=1.5
      DO I=1,3
         HS(I)=0
         OMIGS(I)=0
         QVECT(I)=0.0
      END DO
C     Start
      DO I=1,3
         WRITE(*,*)
      END DO
      WRITE(*,*) '          *****************************',
     *                  '*****************************'
      WRITE(*,*) '          *    DIMS---PHASING PROGRAM FOR ',
     *                'MODULATED STRUCTURES     *'
      WRITE(*,*) '          *                      ',
     *                '                                  *'
      WRITE(*,*) '          *                  Z.Q. Fu & H.F. Fan',
     *                '                    *'
      WRITE(*,*) '          *      Group 405, Inst. of ',
     *                'Phys., Academia Sinica        *'
      WRITE(*,*) '          *      Beijing 100080,',
     *                ' P.R.China    Jan. 15, 1992        *'
      WRITE(*,*) '          *****************************',
     *                  '*****************************'
      WRITE(*,*)
        WRITE(*,*)
C get command line arguments if there exsists. use msflib lib.
      WRITE(*,10) ' Job_file: '
 10   FORMAT(A,$)
      NuArgs=IARGC()
      IF (NuArgs.GT.0) THEN
          N1=1
          CALL GETARG(N1,FILE_IN,STATUS)
          WRITE(*,*) FILE_IN(1:STATUS)
      ELSE
          READ(*,20) FILE_IN
      ENDIF
 20   FORMAT(A80)
      FILENAMELEN=LEN(FILE_IN)
      ILEFT=FILENAMELEN
      DO I=1,FILENAMELEN
          IF(FILE_IN(I:I).NE.SPACE.AND.FILE_IN(I:I).NE.TBL
     +       .AND.ILEFT.EQ.FILENAMELEN) ILEFT=I
      ENDDO
      IRIGHT1=FILENAMELEN
      IRIGHT2=FILENAMELEN
      DO I=FILENAMELEN,ILEFT,-1
          IF(FILE_IN(I:I).NE.SPACE.AND.FILE_IN(I:I).NE.TBL
     +       .AND.IRIGHT1.EQ.FILENAMELEN) IRIGHT1=I
          IF(FILE_IN(I:I).EQ.PERIOD.AND.IRIGHT2.EQ.FILENAMELEN)
     +           IRIGHT2=I-1
      ENDDO
      IRIGHT=MIN(IRIGHT1,IRIGHT2)
      IF(ILEFT.EQ.FILENAMELEN) STOP 'MUST INPUT A Job File Name'
      FILE1=FILE_IN(ILEFT:IRIGHT)//'.OUT1'
      FILE2=FILE_IN(ILEFT:IRIGHT)//'.OUT2'
      FILE5=FILE_IN(ILEFT:IRIGHT)//'.HKLM'
      FILE_IN=FILE_IN(ILEFT:FILENAMELEN)
      OPEN(1,FILE=FILE_IN,FORM='FORMATTED',STATUS='OLD')
      OPEN(10,FILE=FILE1,FORM='FORMATTED',STATUS='UNKNOWN')
      OPEN(11,FILE=FILE2,FORM='FORMATTED',STATUS='UNKNOWN')
      OPEN(13,FILE=FILE5,FORM='FORMATTED',STATUS='UNKNOWN')
      NATOM=0
      LINE=0
C
C     Start reading input file
C
      READ(1,'(A128)') TITLE
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
      READ(1,*) ITE,MFS,IPATH,IVOID
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
      READ(1,*) NSETM,ISKIP,IVOID2,IRANTP
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
      READ(1,*) (NFSET(J),J=0,6)
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
      READ(1,*) (ZERPI(I),I=0,6)
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
      READ(1,*) CYCLCTR,NCLMAX,NCYCFIX,RADI
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
      READ(1,*) W1,W2,W3
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
      READ(1,*) MREL,EKAPAPER,SKAMAX,PHASPERC
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
C     Read unit-cell parameters for sub-structure 1
      READ(1,*) (CELL(I,1),I=1,6)
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
C     Read unit-cell parameters for sub-structure 2
      READ(1,*) (CELL(I,2),I=1,6)
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
C     Read modulation wave vector q
      READ(1,*) (QVECT(I),I=1,3)
           LINE=LINE+1
      READ(1,*)
           LINE=LINE+1
        READ(1,*) NATOMINF,NORTYP,IWILS,BFACT,NWILSTEP
           LINE=LINE+1
        READ(1,*)
           LINE=LINE+1
C     Read cell contents
      DO I=1,120
         READ(1,*) IA,LZA
           LINE=LINE+1
         IF(IA.NE.0) THEN
            IATOM(I)=IA
            ZATOM(I)=LZA
            NATOM=NATOM+1
         ELSE
            GOTO 50 
         ENDIF
      END DO
 50      READ(1,*)
           LINE=LINE+1
C     Read space-group symbol or generators
      NUMB=1
      READ(1,60) NCHLD
           LINE=LINE+1
 60   FORMAT(80A1)
      DO I=1,80
        IF(NCHLD(I).NE.HV(1)) THEN
          J=I
          GOTO 70
        ENDIF
      END DO
 70   IF(NH(NCHLD(J)).LT.HC(11)) THEN
        IFORSPGR=0
        BACKSPACE(1)
      ELSE
        IFORSPGR=1
        READ(1,*)
           LINE=LINE+1
        READ(1,*)
           LINE=LINE+1
      ENDIF
      IF(IFORSPGR.EQ.1) THEN
        CALL SPGR4D(NCHLD,MMX,MTRANL,TCL,NG,NLC,JERR,ICENT,NUMB,SY_SPG)
        WRITE(10,*)
        WRITE(10,*)
        DO I=1,48
          DO J=1,4
            TRANL(J,I)=MTRANL(I,J)/24.0
            DO K=1,4
              MX(J,K,I)=MMX(I,J,K)
            END DO
          END DO
        END DO
        IF(ICENT.EQ.0) THEN
          NG=NG+1
        DO I=1,IDIN
            DO J=1,IDIN
             IF(I.EQ.J) THEN
              MX(I,J,NG)=-1
             ELSE
              MX(I,J,NG)=0
             ENDIF
          END DO
          TRANL(I,NG)=0.0
        END DO
          CALL MATRIX(NG)
        ELSE
          NSYM=NG
        ENDIF       
        IF(JERR.EQ.99) GOTO 650
      ELSEIF(IFORSPGR.EQ.0) THEN
      READ(1,*) NG
      DO I=1,NG
         DO J=1,IDIN
             READ(1,*) (MX(J,K,I),K=1,IDIN),TRANL(J,I)
             LINE=LINE+1
         END DO
         READ(1,*)
           LINE=LINE+1
      END DO
      READ(1,*)
           LINE=LINE+1
C----added by lyd, add a space line between generators and comments of data
      READ(1,*)
           LINE=LINE+1
      CALL MATRIX(NG)
            ICENT=0
      DO I=1,NG
         IF(MX(1,1,I)+MX(2,2,I)+MX(3,3,I)+MX(4,4,I).EQ.-4) ICENT=1
      END DO
      IF(ICENT.EQ.0) THEN
         NG=NG+1
         DO I=1,IDIN
              DO J=1,IDIN
               MX(I,J,NG)=0
            END DO
            TRANL(I,NG)=0.0
         END DO
         DO I=1,IDIN
            MX(I,I,NG)=-1
         END DO
         CALL MATRIX(NG)  
      ENDIF
      ENDIF
C-------------------------------------------------------------------
C Output HKLM file for calculating Fourier maps in VEC
C
      WRITE(13,30) 'File from DIMS for Fourier calculations in VEC',0
 30   FORMAT(7HPRIVATE,5X,A/1X,4HCONV,5X,I1/4H*EOS/)
C-------------------------------------------------------------------
C------------------------------------------------------------------
C     Write to HKLM file
      WRITE(13,40) (CELL(I,1),I=1,6),(QVECT(I),I=1,3)
 40   FORMAT('CELL DIMension',
     + '  a         b         c      alpha     beta      gamma'/,
     + 1X,4HCELL,5X,6F10.4,/,1X,4HQVEC,5X,3F10.4/4H*EOS/)
C------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C     Write to HKLM file
      ING=NG*NLC
      WRITE(13,41) ING,ICENT,SY_SPG
 41   FORMAT('SYMMETRY'/' SGSY',5X,I2,2X,I2,2X,A36)
      DO INLC=1,NLC
       DO ING=1,NG
        WRITE(13,42) 
     +  ((MX(I,J,ING),J=1,IDIN),TRANL(I,ING)+TCL(INLC,I)/24.0,I=1,IDIN)
       ENDDO
      ENDDO
 42   FORMAT(' SYOP    ',4(4I3,F7.4))
      WRITE(13,43)
 43   FORMAT('*EOS'/)
C-----------------------------------------------------------------------
      WRITE(*,90) '$ ... Please wait ... '
 90      FORMAT(A22)
      MREL2=2*MREL
       PI=180
      DTOR=ATAN(1.0)/45.0
      DO 100 I=0,450
         SINT(I)=SIN(DTOR*I)
         COST(I)=COS(DTOR*I)
 100      CONTINUE
      SG2=0.0
      SG3=0.0
      DO I=1,NATOM
         ZA2=ZATOM(I)*ZATOM(I)
         ZA3=ZATOM(I)*ZA2
         SG2=SG2+IATOM(I)*ZA2
         SG3=SG3+IATOM(I)*ZA3
      END DO
      SGM23=2.0*SG3/SQRT(SG2**3)
C
C      INPUT INDEPENDENT REF.S AND CALCULATE SCALED-SFs
      MFS0=0
        MFSORIG=MFS
      NTOTAL=0
      DO I=0,6
         NSAT(I)=0
      END DO
      RTD=45.0/ATAN(1.0)
      SUMFS(0)=0.0
        jj=0
C
      IF(IPATH.EQ.4) THEN
         IF(MFS.EQ.128.OR.MFS.EQ.129.OR.MFS.EQ.130) THEN
         ELSE
            CALL ERRPATH4(MFS)
         ENDIF
         IF(MFS.EQ.128) THEN
          MFS=0
          I129=0
        IF(RADI.EQ.1) THEN
         DO I=1,20000
            READ(1,*,END=140) H(1),H(2),H(3),H(4),F0,
     *                           PHAS0,MARKN0
              IF(F0.LT.0.0) GOTO 140
            IF(F0.EQ.0.0) CALL ERR1(F0)
            PHAS0=PHAS0*RTD
              II=H(1)*H(4)
              IF(II.EQ.0) THEN
                 I129=I129+1
                 NSAT(0)=NSAT(0)+1
               SUMFS(0)=SUMFS(0)+F0
               DO J=1,IDIN
                  HKLM(J,I129)=H(J)
               END DO
               FO(I129)=F0
               PHASO(I129)=PHAS0
               MARKNO(I129)=MARKN0
              ENDIF
         END DO
        ELSE
         DO I=1,20000
            READ(1,*,END=140) H(1),H(2),H(3),H(4),F0,
     *                           PHAS0,MARKN0
              IF(F0.LT.0.0) GOTO 140
            IF(F0.EQ.0.0) CALL ERR1(F0)
              II=H(1)*H(4)
              IF(II.EQ.0) THEN
                 I129=I129+1
                 NSAT(0)=NSAT(0)+1
               SUMFS(0)=SUMFS(0)+F0
               DO J=1,IDIN
                  HKLM(J,I129)=H(J)
               END DO
               FO(I129)=F0
               PHASO(I129)=PHAS0
               MARKNO(I129)=MARKN0
              ENDIF
         END DO
        ENDIF
         ELSEIF(MFS.EQ.129) THEN
          MFS=1
          I129=0
        IF(RADI.EQ.1) THEN
            DO I=1,20000
            READ(1,*,END=110) H(1),H(2),H(3),H(4),F0,
     *                           PHAS0,MARKN0
              IF(F0.LT.0.0) GOTO 110
            IF(F0.EQ.0.0) CALL ERR1(F0)
            PHAS0=PHAS0*RTD
              II=H(1)*H(4)
              IF(II.EQ.0) THEN
                I129=I129+1
                NSAT(0)=NSAT(0)+1
              SUMFS(0)=SUMFS(0)+F0
              DO J=1,IDIN
                 HKLM(J,I129)=H(J)
              END DO
              FO(I129)=F0
              PHASO(I129)=PHAS0
              MARKNO(I129)=MARKN0
              ENDIF
           END DO
 110        REWIND(1)
           DO II=1,LINE
              READ(1,*)
           END DO
         DO I=1,20000
            READ(1,*,END=140) H(1),H(2),H(3),H(4),F0,
     *                           PHAS0,MARKN0
              IF(F0.LT.0.0) GOTO 140
            IF(F0.EQ.0.0) CALL ERR1(F0)
            PHAS0=PHAS0*RTD
              II=H(1)*H(4)
              IF(II.NE.0) THEN
                I129=I129+1
                NSAT(1)=NSAT(1)+1
              SUMFS(1)=SUMFS(1)+F0
              DO J=1,IDIN
                 HKLM(J,I129)=H(J)
              END DO
              FO(I129)=F0
              PHASO(I129)=PHAS0
              MARKNO(I129)=MARKN0
              ENDIF
           END DO
        ELSE
         DO I=1,20000
            READ(1,*,END=120) H(1),H(2),H(3),H(4),F0,
     *                           PHAS0,MARKN0
              IF(F0.LT.0.0) GOTO 120
            IF(F0.EQ.0.0) CALL ERR1(F0)
              II=H(1)*H(4)
              IF(II.EQ.0) THEN
                I129=I129+1
                NSAT(0)=NSAT(0)+1
              SUMFS(0)=SUMFS(0)+F0
              DO J=1,IDIN
                 HKLM(J,I129)=H(J)
              END DO
              FO(I129)=F0
              PHASO(I129)=PHAS0
              MARKNO(I129)=MARKN0
              ENDIF
           END DO
 120       REWIND(1)
           DO II=1,LINE
              READ(1,*)
           END DO
         DO I=1,20000
            READ(1,*,END=140) H(1),H(2),H(3),H(4),F0,
     *                           PHAS0,MARKN0
              IF(F0.LT.0.0) GOTO 140
            IF(F0.EQ.0.0) CALL ERR1(F0)
              II=H(1)*H(4)
              IF(II.NE.0) THEN
                I129=I129+1
                NSAT(1)=NSAT(1)+1
              SUMFS(1)=SUMFS(1)+F0
              DO J=1,IDIN
                 HKLM(J,I129)=H(J)
              END DO
              FO(I129)=F0
              PHASO(I129)=PHAS0
              MARKNO(I129)=MARKN0
              ENDIF
           END DO
          ENDIF
         ELSEIF(MFS.EQ.130) THEN
          MFS=0
        IF(RADI.EQ.1) THEN
         DO I=1,20000
            READ(1,*,END=140) H(1),H(2),H(3),H(4),F0,
     *                           PHAS0,MARKN0
              IF(F0.LT.0.0) GOTO 140
            IF(F0.EQ.0.0) CALL ERR1(F0)
            PHAS0=PHAS0*RTD
            DO J=1,IDIN
               HKLM(J,I)=H(J)
            END DO
            FO(I)=F0
            PHASO(I)=PHAS0
            MARKNO(I)=MARKN0
              NSAT(0)=NSAT(0)+1
            SUMFS(0)=SUMFS(0)+F0
         END DO
        ELSE
         DO I=1,20000
            READ(1,*,END=140) H(1),H(2),H(3),H(4),F0,
     *                           PHAS0,MARKN0
              IF(F0.LT.0.0) GOTO 140
            IF(F0.EQ.0.0) CALL ERR1(F0)
            DO J=1,IDIN
               HKLM(J,I)=H(J)
            END DO
            FO(I)=F0
            PHASO(I)=PHAS0
            MARKNO(I)=MARKN0
              NSAT(0)=NSAT(0)+1
            SUMFS(0)=SUMFS(0)+F0
         END DO
        ENDIF
         ENDIF
C
      ELSE
      IF(RADI.EQ.1) THEN
         DO I=1,20000
          READ(1,*,END=140) H(1),H(2),H(3),H(4),F0,
     *                           PHAS0,MARKN0
            IF(F0.LT.0.0) GOTO 140
          II=IABS(H(4))
          IF(II.LE.MFS) THEN
              JJ=JJ+1
            IF(II.GT.MFS0) THEN
               MFS0=II
            ENDIF
            IF(F0.EQ.0.0) CALL ERR1(F0)
            PHAS0=PHAS0*RTD
            DO J=1,IDIN
               HKLM(J,JJ)=H(J)
            END DO
            FO(JJ)=F0
              PHASO(JJ)=PHAS0
            MARKNO(JJ)=MARKN0
            IF(II.EQ.0) THEN
               NSAT(0)=NSAT(0)+1
               SUMFS(0)=SUMFS(0)+F0
            ELSEIF(II.EQ.1) THEN
               NSAT(1)=NSAT(1)+1
            ELSEIF(II.EQ.2) THEN
               NSAT(2)=NSAT(2)+1
            ELSEIF(II.EQ.3) THEN
               NSAT(3)=NSAT(3)+1
            ELSEIF(II.EQ.4) THEN
               NSAT(4)=NSAT(4)+1
            ELSEIF(II.EQ.5) THEN
               NSAT(5)=NSAT(5)+1
            ELSE
               NSAT(6)=NSAT(6)+1
            ENDIF
          ENDIF
         END DO
      ELSE
         DO I=1,20000
          READ(1,*,END=140) H(1),H(2),H(3),H(4),F0,
     *                           PHAS0,MARKN0
            IF(F0.LT.0.0) GOTO 140
          II=IABS(H(4))
          IF(II.LE.MFS) THEN
              JJ=JJ+1
            IF(II.GT.MFS0) THEN
               MFS0=II
            ENDIF
            IF(F0.EQ.0.0) CALL ERR1(F0)
            DO J=1,IDIN
               HKLM(J,JJ)=H(J)
            END DO
            FO(JJ)=F0
            PHASO(JJ)=PHAS0
            MARKNO(JJ)=MARKN0
            IF(II.EQ.0) THEN
               NSAT(0)=NSAT(0)+1
               SUMFS(0)=SUMFS(0)+F0
            ELSEIF(II.EQ.1) THEN
               NSAT(1)=NSAT(1)+1
            ELSEIF(II.EQ.2) THEN
               NSAT(2)=NSAT(2)+1
            ELSEIF(II.EQ.3) THEN
               NSAT(3)=NSAT(3)+1
            ELSEIF(II.EQ.4) THEN
               NSAT(4)=NSAT(4)+1
            ELSEIF(II.EQ.5) THEN
               NSAT(5)=NSAT(5)+1
            ELSE
               NSAT(6)=NSAT(6)+1
            ENDIF            
          ENDIF
         END DO
      ENDIF
      ENDIF
C
 140      IF(IPATH.EQ.4) THEN
        ELSE
      IF(MFS0.LT.MFS) THEN
         CALL ERRMFS(MFS0,MFS)
      ENDIF
        ENDIF
      DO I=0,MFS
         NTOTAL=NTOTAL+NSAT(I)
      END DO
        DO 170 I=1,NTOTAL
           IF(PHASO(I).LT.0) THEN
              PHASO(I)=MOD(PHASO(I),360)+360
           ELSE
              PHASO(I)=MOD(PHASO(I),360)
           ENDIF
 170      END DO
C
        CALL ATOMSCAT
      IF(IPATH.EQ.4) THEN
      ELSE
        IF(MFS.GT.0) THEN
         CALL SEQUHKLM
        ENDIF
      ENDIF
C
        IF(MFS.EQ.0) THEN
           NSEQU1=0
         NSEQU2=NSAT(0)
           CALL SEQUFACT(NSEQU1,NSEQU2)
        ELSE
        DO I=0,MFS
         NSEQU1=0
         NSEQU2=0
           IF(I.EQ.0) THEN
              NSEQU1=0
            NSEQU2=NSAT(0)
           ELSE
            DO J=0,I
                 NSEQU2=NSEQU2+NSAT(J)
              END DO
            NSEQU1=NSEQU2-NSAT(I)
           ENDIF
           CALL SEQUFACT(NSEQU1,NSEQU2)
        END DO
        ENDIF
        IF(IPATH.EQ.4.AND.MFS.EQ.0) THEN
           I=NTOTAL*PHASPERC
           WRITE(11,*) I 
        ELSE
         WRITE(11,*) NTOTAL
        ENDIF
C
        IF(IPATH.EQ.4.AND.MFS.EQ.0) THEN
           CALL MULTIP
           CALL CALRHO
           IF(IWILS.GE.1) THEN
             CALL WILSON
           ELSE
             B(1)=1.0
             B(2)=0.0
           ENDIF
      ELSE
         CALL RINDW
        ENDIF
C
      MFSCTR=MFS
      IF(MFS.EQ.0) THEN
           MFS=0
      ELSEIF(MFS.GE.1) THEN
         MFS=1
      ELSE
      ENDIF
C
        ISKIP0=ISKIP
      NSETM0=NSETM
C
      IF(ITE.EQ.1) THEN
         DO I=1,NTOTAL
            PHAS00(I)=PHASO(I)
         END DO
      ELSE
      ENDIF
C
 190  IF(IPATH.EQ.3) THEN
        JPATH=IPATH
        IPATH=2
      ELSE
        JPATH=0
      ENDIF
C
      NSTART=0
      NEND=0
      IF(MFS.EQ.0) THEN
         NSTART=1
         NEND=NSAT(0)
      ELSE
           DO I=1,MFS
            NSTART=NSTART+NSAT(I-1)
         END DO
         NSTART=NSTART+1
         NEND=NSTART-1+NSAT(MFS)
      ENDIF
 210      IOVER=0
        IF(JPATH.EQ.3.AND.IPATH.EQ.1) THEN
        ELSE
        IF(IPATH.EQ.4) THEN
          WRITE(10,290) MFSORIG,NSAT(MFS)
        ELSE
          WRITE(10,280) MFS,NSAT(MFS)
        ENDIF
        WRITE(10,'(A128)') TITLE
        WRITE(10,260) ISKIP,NSETM,NCLMAX,NCYCFIX
        WRITE(10,270) W1,W2,W3,EKAPAPER,SKAMAX
        WRITE(10,240)
        ENDIF
        IF(JPATH.EQ.0) THEN
         WRITE(10,250) IPATH
         WRITE(10,*)
         WRITE(10,*)
        ELSEIF(JPATH.EQ.3.AND.IPATH.EQ.2) THEN
         WRITE(10,250) JPATH
         WRITE(10,*)
         WRITE(10,*)
        ENDIF
        IF(IPATH.EQ.4.AND.MFSORIG.EQ.128) THEN
         IF(IWILS.EQ.1) THEN
           WRITE(10,220) B(1),B(2)
         WRITE(10,*)
         WRITE(10,*)
         ELSEIF(IWILS.EQ.2) THEN
           WRITE(10,230)
         WRITE(10,*)
         WRITE(10,*)
         ENDIF
        ENDIF
 220      FORMAT(1X,'BASED UPON WILSON METHODS: SCALING-FACTOR = ',
     *       F10.4,'     B = ',F10.4)
 230      FORMAT(1X,'SCALING BASED UPON K-CURVE METHOD')
 240      FORMAT(1X,'-----------------------------------------------')
 250      FORMAT(1X,'PATH:',I2)
 260      FORMAT(1X,'SKIP=',I4,5X,'NTRIAL=',I4,5X,
     *            'MAXCL=',I3,5x,'NFIX=',I3)
 270      FORMAT(1X,'W1=',F5.3,5X,'W2=',F5.3,5X,'W3=',F5.3,5X,
     *             'KPMIN=',F6.2,5X,'KPMAX=',F7.2)
 280      FORMAT(1X,'PHASING RESULT',
     *            ' FOR SATELLITES OF ORDER',I2,'  NREF=',I5) 
 290      FORMAT(1X,'PHASING RESULT FOR',
     * ' COMPOSITE STRUCTURE: ORDER OF SATELLITES =',I3,'  NREF=',I5) 
C
      SUMFS(MFS)=0.0
      NWONK=0
      DO I=NSTART,NEND
         IF(MARKNO(I).EQ.1) THEN
            NWONK=NWONK+1
         ENDIF
         SUMFS(MFS)=SUMFS(MFS)+FO(I)
        END DO
        IF(IPATH.EQ.4.AND.MFS.EQ.0) THEN
           CALL NORMALF
           NSEQU1=0
           NSEQU2=NSAT(0)
           CALL SEQUNORF(NSEQU1,NSEQU2)
           NSTART=1
           KEND=NEND
           NEND=NEND*PHASPERC
           NTOTAL=NEND
           CALL RINDW
        ELSE
         FMAIN=SUMFS(0)/NSAT(0)
         FSATE=SUMFS(MFS)/NSAT(MFS)
         SCBOT=(SGM23*FMAIN*FSATE*FSATE)**(1.0/3.0)
         SCALE(MFS)=1.0/SCBOT
         DO I=1,NEND
           E(I)=FO(I)*SCALE(MFS)
         END DO
        ENDIF
C
      EMAX=E(NSTART)
      KAPC1=(NEND+NSTART-1)/2
        IF(IPATH.EQ.4.AND.MFS.EQ.0) THEN
         KAPC2=NEND/2
        ELSE
         KAPC2=NSAT(0)/2
        ENDIF
C
      EKAPC1=E(KAPC1)
      EKAPC2=E(KAPC2)
        IF(IPATH.EQ.4.AND.MFS.EQ.0) THEN
         MPSNO=(KEND-NSTART+1)/10
         NFORPS=KEND-MPSNO
        ELSE
         MPSNO=(NEND-NSTART+1)/10
         NFORPS=NEND-MPSNO
        ENDIF
        IF(IPATH.EQ.4.AND.MFS.EQ.0.AND.NORTYP.EQ.1) THEN
           DO I=NSTART,NEND
              ECAL(I)=EFOM(I)
           END DO
           NSEQU1=0
           NSEQU2=NSAT(0)
           CALL SEQUEFOM(NSEQU1,NSEQU2)
           PZPER=ECAL(NFORPS)/ECAL(NSTART)
        ELSE
           PZPER=E(NFORPS)/EMAX
        ENDIF
C
      TOTREL=0.0
           IF(IPATH.EQ.1) THEN
              CALL SIGM1
           ELSEIF(IPATH.EQ.2) THEN
            CALL SIGM2
           ELSEIF(IPATH.EQ.4.AND.MFSORIG.EQ.128) THEN
            CALL SIGM3(NSTART,NEND)
              IKEND=KEND/2
              EKAPC1=E(IKEND)
              EKAPC2=EKAPC1
              CALL SIGM3(NFORPS,KEND)
           ELSEIF(IPATH.EQ.4.AND.MFSORIG.GE.129) THEN
            CALL SIGM4
           ENDIF
      IF(IPATH.EQ.1) THEN
         IF(NWONK.LE.0.AND.MFS.LE.1) THEN
            CALL ORDREF
         ELSE
         ENDIF
      ELSEIF(IPATH.EQ.2.AND.MFS.GT.0) THEN
            CALL ORDREF
      ELSE
      ENDIF
C
        IF(NFSET(MFS).LT.0) THEN
         ISKIP=IABS(NFSET(MFS))-1
         NFXST=1
          NSETM=1
         IOVER=1
      ELSE
      ENDIF
 310      IXR=1
      IYR=1
      IXR2=2511
      IYR2=2511
      IF(ISKIP.GE.1) THEN
           DO 330 I=1,ISKIP
            DO 320 J=NSTART,NEND
            IF(IPATH.EQ.1) THEN
             IF(NWONK.LE.0.AND.MFS.LE.1) THEN
                 DO IJ=1,4
                 IF(HKLM(1,J).EQ.IHORID(1,IJ).AND.
     *               HKLM(2,J).EQ.IHORID(2,IJ).AND.
     *               HKLM(3,J).EQ.IHORID(3,IJ).AND.
     *               HKLM(4,J).EQ.IHORID(4,IJ)) GOTO 320
               END DO
             ELSEIF(NWONK.GE.1) THEN
               IF(MARKNO(J).EQ.1) GOTO 320
             ENDIF
            ELSEIF(IPATH.EQ.2) THEN
             IF(NWONK.LE.0) THEN
                 DO IJ=1,4
                 IF(HKLM(1,J).EQ.IHORID(1,IJ).AND.
     *               HKLM(2,J).EQ.IHORID(2,IJ).AND.
     *               HKLM(3,J).EQ.IHORID(3,IJ).AND.
     *               HKLM(4,J).EQ.IHORID(4,IJ)) GOTO 320
               END DO
             ELSE
               IF(MARKNO(J).EQ.1) GOTO 320
             ENDIF
            ELSE
             IF(MARKNO(J).EQ.1) GOTO 320
            ENDIF
               PMIL=RAND(IXR,IYR)
               IF(ICENT.NE.1) THEN
                  PMIL=RAND(IXR2,IYR2)
               ELSE
               ENDIF
 320            CONTINUE
 330         CONTINUE
      ELSE
      ENDIF
      DO II=1,NSTART
            WT(II)=1.0
      END DO
       DO 350 I=1,NSETM
         IF(MFS.EQ.0) THEN
            IORP=0
         ELSE
            IORP=3
         ENDIF
         NSET=I
         NSETRC(NSET)=NSET
         DO 340 II=NSTART,NEND
           WT(II)=0.25
           IF(IPATH.EQ.1) THEN
            IF(NWONK.LE.0.AND.MFS.LE.1) THEN
                 DO IOR=1,4
                 IF(HKLM(1,II).EQ.IHORID(1,IOR).AND.
     *                HKLM(2,II).EQ.IHORID(2,IOR).AND.
     *                HKLM(3,II).EQ.IHORID(3,IOR).AND.
     *                HKLM(4,II).EQ.IHORID(4,IOR)) THEN
                    IORP=IORP+1
                    PHASO(II)=PORID(IORP)
                    WT(II)=1.0
                    GOTO 340
                 ELSE
                 ENDIF
               END DO
            ELSEIF(NWONK.GE.1) THEN
               IF(MARKNO(II).EQ.1) THEN
                  WT(II)=1.0
                  GOTO 340
               ELSE
               ENDIF
            ENDIF
           ELSEIF(IPATH.EQ.2) THEN
            IF(NWONK.LE.0) THEN
                 DO IOR=1,4
                 IF(HKLM(1,II).EQ.IHORID(1,IOR).AND.
     *                HKLM(2,II).EQ.IHORID(2,IOR).AND.
     *                HKLM(3,II).EQ.IHORID(3,IOR).AND.
     *                HKLM(4,II).EQ.IHORID(4,IOR)) THEN
                    IORP=IORP+1
                    PHASO(II)=PORID(IORP)
                    WT(II)=1.0
                    GOTO 340
                  ELSE
                  ENDIF
                END DO
            ELSE
                 IF(MARKNO(II).EQ.1) THEN
                  WT(II)=1.0
                  GOTO 340
               ENDIF
            ENDIF
           ELSE
            IF(MARKNO(II).EQ.1) THEN
               WT(II)=1.0
               GOTO 340
            ELSEIF(IPATH.EQ.4.AND.MARKNO(II).EQ.2) THEN
                 WT(II)=1.0
            ENDIF
           ENDIF
C
            IPHA=90.0-90.0*SIGN(1.0,(RAND(IXR,IYR)-0.5))
            IF(ICENT.EQ.1) THEN
             PHASO(II)=IPHA
            ELSE
               IF(IPATH.EQ.4.AND.IRANTP.EQ.1) THEN
             PHASO(II)=IPHA
               ELSEIF(IRANTP.EQ.-1) THEN
             PHASO(II)=IPHA
               ELSE
               PHASO(II)=IPHA+
     *              45.0*SIGN(1.0,(RAND(IXR2,IYR2)-0.5))
               ENDIF                                                      
            ENDIF
C
 340            CONTINUE
            PSIZ(NSET)=0.0
            PSI(NSET)=0.0
            CALL PHATAN
 350      CONTINUE
      IF(IOVER.EQ.1) GOTO 370
      CALL CFOM
      WRITE(10,*)
      WRITE(10,*)
      WRITE(10,*)
      NFXST=1
      ISKIP=ISKIP+NSETRC(NFXST)-1
      NSETM=1
      IOVER=1
      GOTO 310            
C
 370  IF(JPATH.EQ.0) GOTO 390
      IF(JPATH.EQ.3) THEN
        IF(MFS.LE.1) THEN
              GOTO 380
        ELSEIF(MFS.GT.1.AND.IPATH.EQ.2) THEN
          DO I=NSTART,NEND
            PHASAV(I)=PHASO(I)
          END DO
          IPATH=1
          NSETM=NSETM0
          ISKIP=ISKIP0
          GOTO 210
        ELSEIF(MFS.GT.1.AND.IPATH.EQ.1) THEN
          GOTO 380
        ELSE
        ENDIF
      ELSE
      ENDIF
 380  IF(JPATH.EQ.3) THEN
        IF(IPATH.EQ.1) THEN
          IPATH=3
          PHSHIFT=0
          CALL SOLVOR(PHSHIFT,ICENT)
          GOTO 390
        ELSEIF(IPATH.EQ.2) THEN
          IPATH=3
          GOTO 390
        ENDIF
      ELSE
      ENDIF
C
 390      DO I=NSTART,NEND
        E(I)=E(I)*MARK(I)
      END DO
        IF(NFSET(MFS).LT.0) THEN
           COFOM(NFXST)=3.0
        ELSE
        ENDIF
      IF(ITE.EQ.1) THEN
        IF(MFS.EQ.1) THEN
          DO II=1,NSAT(0)
            WRITE(10,530) (HKLM(K,II),K=1,IDIN),FO(II),
     *            PHASO(II),MARKNO(II),MARK(II)
          END DO
        ELSE
        ENDIF
        WRITE(10,*)
C         COMPARE RESULTANT PHASES WITH GIVEN ONES OR NOT
        WRITE(10,410) (NSETRC(NFXST)+ISKIP),COFOM(NFXST)
        IF(JPATH.EQ.3.AND.MFS.GT.1) THEN
          WRITE(10,420) PHSHIFT
        ELSE
        ENDIF
 410        FORMAT(1X,'**** COMPARISON BETWEEN RESULTANT AND GIVEN',
     *            ' PHASES **** SET NO.:',I3,
     *            '  COFOM=',F5.3)
 420    FORMAT(1X,'-- ORIGIN-DEPENDING PHASE-SHIFT DERIVED',
     *            ' USING WEAK-WEAK-WEAK RELATIONS: ',I3)
        WRITE(10,460)
        NDIF=0
        NDIFDP=0  
        IF(ICENT.EQ.1) THEN
          DO 430 II=NSTART,NEND
            NDIFID(II)=0
            IF(PHAS00(II).EQ.PHASO(II)) GOTO 430
            NDIF=NDIF+1
            NDIFID(II)=-1  
 430          CONTINUE
        ELSE
          DO 440 II=NSTART,NEND
            NDIFD=PHAS00(II)-PHASO(II)
            IF(NDIFD.LT.0) NDIFD=NDIFD+360
            IF(NDIFD.GT.180) NDIFD=360-NDIFD
            NDIFDP=NDIFDP+NDIFD
            NDIFID(II)=NDIFD
            NDIF=NDIF+1
 440          CONTINUE
          NDIF=NDIFDP/NDIF
        ENDIF
        DO II=NSTART,NEND
          WRITE(10,480) (HKLM(K,II),K=1,IDIN),FO(II),
     *      PHAS00(II),PHASO(II),NDIFID(II),MARKNO(II),MARK(II)
        END DO
        IF(ICENT.EQ.1) THEN
          WRITE(10,540) NDIF
        ELSE
          WRITE(10,550) NDIF
        ENDIF
      ELSE
        WRITE(10,450) (NSETRC(NFXST)+ISKIP),COFOM(NFXST)
          IF(JPATH.EQ.3.AND.MFS.GT.1) THEN
            WRITE(10,420) PHSHIFT
          ELSE
          ENDIF
 450        FORMAT(1X,'**** PHASING ',
     *               'RESULT ****    SET NO.:',I3,
     *               '   COFOM=',F5.3)
        WRITE(10,*)
        WRITE(10,470)
C-------OUTPUT THE MAIN-REFS
        IF(MFS.EQ.1) THEN
          DO II=1,NSAT(0)
            WRITE(10,530) (HKLM(K,II),K=1,IDIN),FO(II),
     *                    PHASO(II),MARKNO(II),MARK(II)
            END DO
        ELSE
        ENDIF
C-------OUTPUT THE SATELITE-REFS
        DO II=NSTART,NEND
          WRITE(10,490) (HKLM(K,II),K=1,IDIN),FO(II),
     *                   PHASO(II),MARKNO(II),MARK(II)
        END DO
      ENDIF
C-------FOR FOURIER MAP
C
      IF(IPATH.EQ.4.AND.MFS.EQ.0) THEN
          PTOTAL=0.0 
        DO I=1,NTOTAL
          PTOTAL=PTOTAL+E0(I)*E0(I)
        END DO
        PTOTAL=PTOTAL/NTOTAL
        ESCAL=1.0/SQRT(PTOTAL)
        DO I=1,NTOTAL
          E0(I)=E0(I)*ESCAL
        END DO
      ELSE
      ENDIF
      IF(MFS.EQ.1) THEN
        DO II=1,NSAT(0)
          WRITE(11,520) (HKLM(K,II),K=1,IDIN),FO(II),
     *                 PHASO(II)
        END DO
      ENDIF
C
      IF(IPATH.EQ.4.AND.MFS.EQ.0) THEN
        DO II=NSTART,NEND
        WRITE(11,510) (HKLM(K,II),K=1,IDIN),FO(II),E0(II),
     *                 PHASO(II)
        END DO
      ELSE
        DO II=NSTART,NEND
          WRITE(11,520) (HKLM(K,II),K=1,IDIN),FO(II),
     *                 PHASO(II)
        END DO
      ENDIF
      MFS=MFS+1
      IF(MFS.GT.MFSCTR) THEN
        GOTO 560
      ELSE
        NSETM=NSETM0
        ISKIP=ISKIP0
        WRITE(10,*)
        WRITE(10,*)
        WRITE(10,*)
        GOTO 190
      ENDIF
 460      FORMAT(1X,'   H1   H2   H3   H4     ',
     *         'Fobs    PHAS0  PHAS',
     *         '    DN    KN    MK')
 470      FORMAT(1X,'   H1   H2   H3   H4     ',
     *         ' F0    PHAS    KN    MK')
 480      FORMAT(1X,4I5,F10.3,I8,4I6)
 490      FORMAT(1X,4I5,F10.2,3I6)
 510      FORMAT(1X,4I5,F10.2,F10.4,I7)
 520      FORMAT(1X,4I5,F10.2,I7)
 530      FORMAT(1X,4I5,F10.2,3I6)
C
 540      FORMAT(1x,'Number of different phases:',I4)
 550      FORMAT(1x,'Average differences SUM{ABS(PHAS0-PHAS)}/NREF:',I4)
 560  CALL WHOLHKLM
C
C-----------------------------------------------------------------------
C     Write to HKLM file
      WRITE(13,551)
 551  FORMAT('HKL         H    K    L    M     F(obs)    Phase')
      DO I=1,NTOTAL
      WRITE(13,552) (HKLM(IJ,I),IJ=1,4),FO(I),PHASO(I),E0(I)
      ENDDO
 552  FORMAT(1X,'CALC   ',4I5,F12.4,I6,F12.4)
      WRITE(13,554)
 554      FORMAT('*EOS'//'END')
      CLOSE(13)
C-----------------------------------------------------------------------
C
C                        *** Job completed ***
C
      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*) 'The program have terminated normally with '
      WRITE(*,*) ' three output files created:'
      WRITE(*,610) FILE1
      WRITE(*,620) FILE2
      WRITE(*,640) FILE5
 610  FORMAT(1X,A,'   (Phasing procedure and result analysis:)')
 620  FORMAT(1X,A,'   (Independent reflections with derived phases:)')
 640  FORMAT(1X,A,'   (File for Fourier calculations in VEC:)')  
C      CALL TIME(HEE)
C      WRITE(*,*)      
C      WRITE(*,198) HSS
C      WRITE(*,199) HEE
C198  FORMAT(1X,'   ---START TIME:',A8)
C199  FORMAT(1X,'   -----END TIME:',A8)
 650  STOP'                      --- ALL OVER ---'
      END
C      *** FUNC ***
      FUNCTION RAND(IXR,IYR)
      IXR=MOD(251*IXR,1048576)
      IYR=MOD(179*IXR,1048576)
      RAND=FLOAT(MOD(IXR+IYR,1048576))/1048576.0
      RETURN
      END
C      *** SUB ***
      SUBROUTINE HPACK(H,IPACK)
      INTEGER*2 H(4)
      INTEGER*4 IPACK
      IPACK=H(1)*128*128*128+H(2)*128*128+H(3)*128+H(4)
      RETURN
      END
C      *** SUB ***
C      REARRANGE REFS ACCORDING TO M'S VALUE
      SUBROUTINE SEQUHKLM
      LOGICAL ALL
      INTEGER*2 NSAT(0:6),MARKNO(2000),PHASAV(2000)
      INTEGER*2 HKLM(4,2000),PHASO(2000),IPATH
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      JUMP=NTOTAL
130      IF(JUMP.LE.1) GOTO 128
      JUMP=JUMP/2
      IEND=NTOTAL-JUMP
134      ALL=.TRUE.
      DO 135 ITOP=1,IEND
         IBOT=ITOP+JUMP
         XX=IABS(HKLM(4,ITOP))
         YY=IABS(HKLM(4,IBOT))
         IF(XX.LE.YY) GOTO 135
         EE0=FO(ITOP)
         FO(ITOP)=FO(IBOT)
         FO(IBOT)=EE0
         IHH0=PHASO(ITOP)
         PHASO(ITOP)=PHASO(IBOT)
         PHASO(IBOT)=IHH0
         IHH0=MARKNO(ITOP)
         MARKNO(ITOP)=MARKNO(IBOT)
         MARKNO(IBOT)=IHH0
         DO  I=1,IDIN
            IHH0=HKLM(I,ITOP)
            HKLM(I,ITOP)=HKLM(I,IBOT)
            HKLM(I,IBOT)=IHH0
         END DO
         ALL=.FALSE.
135      CONTINUE
      IF(.NOT.ALL) THEN
         GOTO 134
      ELSE
         GOTO 130
      ENDIF
         GOTO 130
128      RETURN
      END
C       *** SUB ***
C      REARRANGE SET OF EACH-ORDER REFS ACCORDING TO F'S VALUE
      SUBROUTINE SEQUFACT(NSEQU1,NSEQU2)
      LOGICAL ALL
      INTEGER*2 NSAT(0:6),MARKNO(2000),PHASAV(2000)
      INTEGER*2 HKLM(4,2000),PHASO(2000),IPATH
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      JUMP=NSEQU2-NSEQU1
140      IF(JUMP.LE.1) GOTO 146
      JUMP=JUMP/2
      IEND=NSEQU2-JUMP
144      ALL=.TRUE.
      DO 145 ITOP=NSEQU1+1,IEND
         IBOT=ITOP+JUMP
         XX=FO(IBOT)
         YY=FO(ITOP)
         IF(XX.LE.YY) GOTO 145
         EE0=FO(ITOP)
         FO(ITOP)=FO(IBOT)
         FO(IBOT)=EE0
         IHH0=PHASO(ITOP)
         PHASO(ITOP)=PHASO(IBOT)
         PHASO(IBOT)=IHH0
         IHH0=MARKNO(ITOP)
         MARKNO(ITOP)=MARKNO(IBOT)
         MARKNO(IBOT)=IHH0
         DO  I=1,IDIN
            IHH0=HKLM(I,ITOP)
            HKLM(I,ITOP)=HKLM(I,IBOT)
            HKLM(I,IBOT)=IHH0
         END DO
         ALL=.FALSE.
145      CONTINUE
      IF(.NOT.ALL) THEN
         GOTO 144
      ELSE
         GOTO 140
      ENDIF
         GOTO 140
146      RETURN
      END
C
C       *** SUB ***
C      REARRANGE SET OF EACH-ORDER REFS ACCORDING TO F'S VALUE
      SUBROUTINE SEQUNORF(NSEQU1,NSEQU2)
      LOGICAL ALL
      INTEGER*2 NSAT(0:6),MARKNO(2000),PHASAV(2000)
      INTEGER*2 HKLM(4,2000),PHASO(2000),IPATH,PHNF(2000)
      INTEGER*2 PHAS00(2000),NDIFID(2000)
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      JUMP=NSEQU2-NSEQU1
140      IF(JUMP.LE.1) GOTO 146
      JUMP=JUMP/2
      IEND=NSEQU2-JUMP
144      ALL=.TRUE.
      DO 145 ITOP=NSEQU1+1,IEND
         IBOT=ITOP+JUMP
         XX=E(IBOT)
         YY=E(ITOP)
         IF(XX.LE.YY) GOTO 145
         EE0=E(ITOP)
         E(ITOP)=E(IBOT)
         E(IBOT)=EE0
         EE0=E0(ITOP)
         E0(ITOP)=E0(IBOT)
         E0(IBOT)=EE0
         EE0=EFOM(ITOP)
         EFOM(ITOP)=EFOM(IBOT)
         EFOM(IBOT)=EE0
         EE0=FO(ITOP)
         FO(ITOP)=FO(IBOT)
         FO(IBOT)=EE0
         IHH0=PHASO(ITOP)
         PHASO(ITOP)=PHASO(IBOT)
         PHASO(IBOT)=IHH0
         IHH0=PHAS00(ITOP)
         PHAS00(ITOP)=PHAS00(IBOT)
         PHAS00(IBOT)=IHH0
         IHH0=MARKNO(ITOP)
         MARKNO(ITOP)=MARKNO(IBOT)
         MARKNO(IBOT)=IHH0
         DO  I=1,IDIN
            IHH0=HKLM(I,ITOP)
            HKLM(I,ITOP)=HKLM(I,IBOT)
            HKLM(I,IBOT)=IHH0
         END DO
         ALL=.FALSE.
145      CONTINUE
      IF(.NOT.ALL) THEN
         GOTO 144
      ELSE
         GOTO 140
      ENDIF
         GOTO 140
146      RETURN
      END
C
C       *** SUB ***
C      REARRANGE SET OF EACH-ORDER REFS ACCORDING TO F'S VALUE
      SUBROUTINE SEQUEFOM(NSEQU1,NSEQU2)
      LOGICAL ALL
      INTEGER*2 NSAT(0:6),MARKNO(2000),PHASAV(2000)
      INTEGER*2 HKLM(4,2000),PHASO(2000),IPATH,PHNF(2000)
      INTEGER*2 PHAS00(2000),NDIFID(2000)
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      JUMP=NSEQU2-NSEQU1
140      IF(JUMP.LE.1) GOTO 146
      JUMP=JUMP/2
      IEND=NSEQU2-JUMP
144      ALL=.TRUE.
      DO 145 ITOP=NSEQU1+1,IEND
         IBOT=ITOP+JUMP
         XX=ECAL(IBOT)
         YY=ECAL(ITOP)
         IF(XX.LE.YY) GOTO 145
         EE0=ECAL(ITOP)
         ECAL(ITOP)=ECAL(IBOT)
         ECAL(IBOT)=EE0
         ALL=.FALSE.
145      CONTINUE
      IF(.NOT.ALL) THEN
         GOTO 144
      ELSE
         GOTO 140
      ENDIF
         GOTO 140
146      RETURN
      END
C
C      *** SUB ***
      SUBROUTINE NORMALF
C
      REAL AF(130)
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6)
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
      COMMON /CZ25/ Y1(50),Y2(50),Y3(50),NWILSTEP
C
        IF(IWILS.EQ.0) THEN
        SUMFAT=0.0
        SUMFAT0=0.0
        DO I=1,130
           AF(I)=0.0
        END DO
C      CALCULATE NORMALIZED STRUCTURE FACTORS
        IF(NORTYP.EQ.1) THEN
        SUMFAT=0.0
      DO 222 I=NSTART,NEND
         SUMFAT0=0.0
       D=0.25*RHO(I)
         IF(HKLM(4,I).EQ.0.AND.HKLM(1,I).NE.0) THEN
           DO 10 II=1,NATOMINF
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
10         CONTINUE
           DO II=1,NATOMINF
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).NE.0) THEN
           DO 11 II=NATOMINF+1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
11         CONTINUE
           DO II=NATOMINF+1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).EQ.0) THEN
           DO 12 II=1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
12         CONTINUE
           DO II=1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ENDIF
         RN=(NSYM/NNNSYM(I))
       RN0=1.0/SQRT(SUMFAT0)
       RNFOM=1.0/(RN*SQRT(SUMFAT))
       RN=RN0/SQRT(RN)
         RNTBFO=FO(I)
       E(I)=RNTBFO*RN
         E0(I)=E(I)
       EFOM(I)=RNTBFO*RNFOM
222      CONTINUE
        PTOTAL=0.0
        PTOTAL0=0.0
        DO I=1,NTOTAL
          PTOTAL=PTOTAL+E(I)*E(I)
          PTOTAL0=PTOTAL0+EFOM(I)*EFOM(I)
        END DO
        PTOTAL=PTOTAL/NTOTAL
        PTOTAL0=PTOTAL0/NTOTAL
        ESCAL=1.0/SQRT(PTOTAL)
        ESCAL0=1.0/SQRT(PTOTAL0)
        DO I=1,NTOTAL
          E(I)=E(I)*ESCAL
          EFOM(I)=EFOM(I)*ESCAL0
        END DO
C
        ELSEIF(NORTYP.EQ.0) THEN
        SUMFAT=0.0
      DO 28 I=NSTART,NEND
         SUMFAT0=0.0
       D=0.25*RHO(I)
         IF(HKLM(4,I).EQ.0.AND.HKLM(1,I).NE.0) THEN
           DO 6 II=1,NATOMINF
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
6          CONTINUE
           DO II=1,NATOMINF
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).NE.0) THEN
           DO 16 II=NATOMINF+1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
16         CONTINUE
           DO II=NATOMINF+1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).EQ.0) THEN
           DO 26 II=1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
26         CONTINUE
           DO II=1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ENDIF
         RN=(NSYM/NNNSYM(I))
       RN0=1.0/SQRT(SUMFAT0)
       RN=1.0/SQRT(RN*SUMFAT)
         RNTBFO=FO(I)
       E0(I)=RNTBFO*RN0
       E(I)=RNTBFO*RN
28      CONTINUE
        ENDIF
C
        ELSEIF(IWILS.EQ.1) THEN
        IF(BFACT.EQ.0.0) THEN
        ELSE
          B(2)=BFACT
        ENDIF
        B(1)=SQRT(1.0/B(1))
        B(2)=0.5*B(2)
        SUMFAT=0.0
        SUMFAT0=0.0
        DO I=1,130
           AF(I)=0.0
        END DO
C      CALCULATE NORMALIZED STRUCTURE FACTORS
        IF(NORTYP.EQ.1) THEN
        SUMFAT=0.0
      DO 122 I=NSTART,NEND
         SUMFAT0=0.0
       D=0.25*RHO(I)
         IF(HKLM(4,I).EQ.0.AND.HKLM(1,I).NE.0) THEN
           DO 100 II=1,NATOMINF
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
100        CONTINUE
           DO II=1,NATOMINF
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).NE.0) THEN
           DO 110 II=NATOMINF+1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
110        CONTINUE
           DO II=NATOMINF+1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).EQ.0) THEN
           DO 120 II=1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
120        CONTINUE
           DO II=1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ENDIF
         RN=(NSYM/NNNSYM(I))
       RN0=1.0/SQRT(SUMFAT0)
       RNFOM=1.0/(RN*SQRT(SUMFAT))
       RN=RN0/SQRT(RN)
         RNTB=B(1)*EXP(B(2)*RHO(I))
         RNTBFO=RNTB*FO(I)
       E(I)=RNTBFO*RN
         E0(I)=E(I)
       EFOM(I)=RNTBFO*RNFOM
122      CONTINUE
        PTOTAL=0.0
        PTOTAL0=0.0
        DO I=1,NTOTAL
          PTOTAL=PTOTAL+E(I)*E(I)
          PTOTAL0=PTOTAL0+EFOM(I)*EFOM(I)
        END DO
        PTOTAL=PTOTAL/NTOTAL
        PTOTAL0=PTOTAL0/NTOTAL
        ESCAL=1.0/SQRT(PTOTAL)
        ESCAL0=1.0/SQRT(PTOTAL0)
        DO I=1,NTOTAL
          E(I)=E(I)*ESCAL
          EFOM(I)=EFOM(I)*ESCAL0
        END DO
C
        ELSEIF(NORTYP.EQ.0) THEN
        SUMFAT=0.0
      DO 128 I=NSTART,NEND
         SUMFAT0=0.0
       D=0.25*RHO(I)
         IF(HKLM(4,I).EQ.0.AND.HKLM(1,I).NE.0) THEN
           DO 106 II=1,NATOMINF
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
106        CONTINUE
           DO II=1,NATOMINF
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).NE.0) THEN
           DO 116 II=NATOMINF+1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
116        CONTINUE
           DO II=NATOMINF+1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).EQ.0) THEN
           DO 126 II=1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
126        CONTINUE
           DO II=1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ENDIF
         RN=(NSYM/NNNSYM(I))
       RN0=1.0/SQRT(SUMFAT0)
       RN=1.0/SQRT(RN*SUMFAT)
         RNTB=B(1)*EXP(B(2)*RHO(I))
         RNTBFO=RNTB*FO(I)
       E0(I)=RNTBFO*RN0
       E(I)=RNTBFO*RN
128      CONTINUE
        ENDIF
C
        ELSEIF(IWILS.EQ.2) THEN
        RHOSTEP=(RHOMAX-RHOMIN)/NWILSTEP
        SUMFAT=0.0
        SUMFAT0=0.0
        DO I=1,130
           AF(I)=0.0
        END DO
C      CALCULATE NORMALIZED STRUCTURE FACTORS
        IF(NORTYP.EQ.1) THEN
        SUMFAT=0.0
      DO 123 I=NSTART,NEND
         SUMFAT0=0.0
       D=0.25*RHO(I)
         IF(HKLM(4,I).EQ.0.AND.HKLM(1,I).NE.0) THEN
           DO 101 II=1,NATOMINF
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
101        CONTINUE
           DO II=1,NATOMINF
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).NE.0) THEN
           DO 111 II=NATOMINF+1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
111        CONTINUE
           DO II=NATOMINF+1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).EQ.0) THEN
           DO 121 II=1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
121        CONTINUE
           DO II=1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ENDIF
         RN=(NSYM/NNNSYM(I))
       RN0=1.0/SQRT(SUMFAT0)
       RNFOM=1.0/(RN*SQRT(SUMFAT))
       RN=RN0/SQRT(RN)
C
         IF(HKLM(4,I).EQ.0.AND.HKLM(1,I).NE.0) THEN
            DO J=1,NWILSTEP
              RRRMIN=RHOMIN+(J-1)*RHOSTEP
              RRRMAX=RRRMIN+RHOSTEP
              IF(RHO(I).GE.RRRMIN.AND.RHO(I).LE.RRRMAX) THEN
                RNTB=1.0/Y1(J)
                RNTB=SQRT(RNTB)
                GOTO 326
              ENDIF
            END DO
326          CONTINUE
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).NE.0) THEN
            DO J=1,NWILSTEP
              RRRMIN=RHOMIN+(J-1)*RHOSTEP
              RRRMAX=RRRMIN+RHOSTEP
              IF(RHO(I).GE.RRRMIN.AND.RHO(I).LE.RRRMAX) THEN
                RNTB=1.0/Y2(J)
                RNTB=SQRT(RNTB)
                GOTO 327
              ENDIF
            END DO
327         CONTINUE
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).EQ.0) THEN
            DO J=1,NWILSTEP
              RRRMIN=RHOMIN+(J-1)*RHOSTEP
              RRRMAX=RRRMIN+RHOSTEP
              IF(RHO(I).GE.RRRMIN.AND.RHO(I).LE.RRRMAX) THEN
                RNTB=1.0/Y3(J)
                RNTB=SQRT(RNTB)
                GOTO 328
              ENDIF
            END DO
328         CONTINUE
         ELSE
         ENDIF
C
         RNTBFO=RNTB*FO(I)
       E(I)=RNTBFO*RN
         E0(I)=E(I)
       EFOM(I)=E(I)
123      CONTINUE
C
        ELSEIF(NORTYP.EQ.0) THEN
        SUMFAT=0.0
      DO 129 I=NSTART,NEND
         SUMFAT0=0.0
       D=0.25*RHO(I)
         IF(HKLM(4,I).EQ.0.AND.HKLM(1,I).NE.0) THEN
           DO 107 II=1,NATOMINF
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
107        CONTINUE
           DO II=1,NATOMINF
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).NE.0) THEN
           DO 117 II=NATOMINF+1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
117        CONTINUE
           DO II=NATOMINF+1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).EQ.0) THEN
           DO 127 II=1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *         FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *         FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *         FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *         FATOM(9,JJ)
127        CONTINUE
           DO II=1,NATOM
           SUMFAT=SUMFAT+IATOM(II)*AF(II)*AF(II)
           SUMFAT0=SUMFAT0+IATOM(II)*AF(II)*AF(II)
           END DO
         ENDIF
         RN=(NSYM/NNNSYM(I))
       RN0=1.0/SQRT(SUMFAT0)
       RN=1.0/SQRT(RN*SUMFAT)
C
         IF(HKLM(4,I).EQ.0.AND.HKLM(1,I).NE.0) THEN
            DO J=1,NWILSTEP
              RRRMIN=RHOMIN+(J-1)*RHOSTEP
              RRRMAX=RRRMIN+RHOSTEP
              IF(RHO(I).GE.RRRMIN.AND.RHO(I).LE.RRRMAX) THEN
                RNTB=1.0/Y1(J)
                RNTB=SQRT(RNTB)
                GOTO 329
              ENDIF
            END DO
329         CONTINUE
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).NE.0) THEN
            DO J=1,NWILSTEP
              RRRMIN=RHOMIN+(J-1)*RHOSTEP
              RRRMAX=RRRMIN+RHOSTEP
              IF(RHO(I).GE.RRRMIN.AND.RHO(I).LE.RRRMAX) THEN
                RNTB=1.0/Y2(J)
                RNTB=SQRT(RNTB)
                GOTO 330
              ENDIF
            END DO
330         CONTINUE
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).EQ.0) THEN
            DO J=1,NWILSTEP
              RRRMIN=RHOMIN+(J-1)*RHOSTEP
              RRRMAX=RRRMIN+RHOSTEP
              IF(RHO(I).GE.RRRMIN.AND.RHO(I).LE.RRRMAX) THEN
                RNTB=1.0/Y3(J)
                RNTB=SQRT(RNTB)
                GOTO 331
              ENDIF
            END DO
331         CONTINUE
         ELSE
         ENDIF
C
         RNTBFO=RNTB*FO(I)
       E0(I)=RNTBFO*RN0
       E(I)=RNTBFO*RN
129      CONTINUE
        ENDIF
        ENDIF
      RETURN
      END
C
C      *** SUB ***
      SUBROUTINE CALRHO
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6)
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
        FA=CELL(1,1)
        FB=CELL(2,1)
        FC=CELL(3,1)
        SA=CELL(1,2)
        SB=CELL(2,2)
        SC=CELL(3,2)
        IALFA1=INT(CELL(4,1))
        IBETA1=INT(CELL(5,1))
        IGAMA1=INT(CELL(6,1))
        IALFA2=INT(CELL(4,2))
        IBETA2=INT(CELL(5,2))
        IGAMA2=INT(CELL(6,2))
        CALFA1=COST(IALFA1)
        SALFA1=SINT(IALFA1)
        CBETA1=COST(IBETA1)
        SBETA1=SINT(IBETA1)
        CGAMA1=COST(IGAMA1)
        SGAMA1=SINT(IGAMA1)
        CALFA2=COST(IALFA2)
        SALFA2=SINT(IALFA2)
        CBETA2=COST(IBETA2)
        SBETA2=SINT(IBETA2)
        CGAMA2=COST(IGAMA2)
        SGAMA2=SINT(IGAMA2)
        VOL1=FA*FB*FC*SQRT(1.0-CALFA1*CALFA1-CBETA1*CBETA1-
     *          CGAMA1*CGAMA1+2.0*CALFA1*CBETA1*CGAMA1)
        VOL1=1.0/(VOL1*VOL1)
        VOL2=SA*SB*SC*SQRT(1.0-CALFA2*CALFA2-CBETA2*CBETA2-
     *          CGAMA2*CGAMA2+2.0*CALFA2*CBETA2*CGAMA2)
        VOL2=1.0/(VOL2*VOL2)
        FS11=FB*FB*FC*FC*SALFA1*SALFA1
        FS22=FA*FA*FC*FC*SBETA1*SBETA1
        FS33=FA*FA*FB*FB*SGAMA1*SGAMA1
        FS12=FA*FB*FC*FC*(CALFA1*CBETA1-CGAMA1)
        FS23=FA*FA*FB*FC*(CBETA1*CGAMA1-CALFA1)
        FS31=FA*FB*FB*FC*(CGAMA1*CALFA1-CBETA1)
        SS11=SB*SB*SC*SC*SALFA2*SALFA2
        SS22=SA*SA*SC*SC*SBETA2*SBETA2
        SS33=SA*SA*SB*SB*SGAMA2*SGAMA2
        SS12=SA*SB*SC*SC*(CALFA2*CBETA2-CGAMA2)
        SS23=SA*SA*SB*SC*(CBETA2*CGAMA2-CALFA2)
        SS31=SA*SB*SB*SC*(CGAMA2*CALFA2-CBETA2)
        FS11=FS11*VOL1
        FS22=FS22*VOL1
        FS33=FS33*VOL1
        FS12=2.0*FS12*VOL1
        FS23=2.0*FS23*VOL1
        FS31=2.0*FS31*VOL1
        SS11=SS11*VOL2
        SS22=SS22*VOL2
        SS33=SS33*VOL2
        SS12=2.0*SS12*VOL2
        SS23=2.0*SS23*VOL2
        SS31=2.0*SS31*VOL2
        J=0
      DO 122 I=1,NTOTAL
         J=J+1
         IF(HKLM(4,I).EQ.0.AND.HKLM(1,I).NE.0) THEN
           RHO(J)=FS11*HKLM(1,I)*HKLM(1,I)+FS22*HKLM(2,I)*HKLM(2,I)+
     *         FS33*HKLM(3,I)*HKLM(3,I)+FS12*HKLM(1,I)*HKLM(2,I)+
     *         FS23*HKLM(2,I)*HKLM(3,I)+FS31*HKLM(3,I)*HKLM(1,I) 
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).NE.0) THEN
           RHO(J)=SS11*HKLM(4,I)*HKLM(4,I)+SS22*HKLM(2,I)*HKLM(2,I)+
     *         SS33*HKLM(3,I)*HKLM(3,I)+SS12*HKLM(4,I)*HKLM(2,I)+
     *         SS23*HKLM(2,I)*HKLM(3,I)+SS31*HKLM(3,I)*HKLM(4,I)
         ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).EQ.0) THEN
           RHO(J)=FS22*HKLM(2,I)*HKLM(2,I)+
     *         FS33*HKLM(3,I)*HKLM(3,I)+
     *         FS23*HKLM(2,I)*HKLM(3,I)
         ENDIF
122      CONTINUE
        DO I=1,NTOTAL
           ECAL(I)=RHO(I)
        END DO
        NSEQU1=0
        NSEQU2=NTOTAL
        CALL SEQUEFOM(NSEQU1,NSEQU2)
      RHOMAX=ECAL(1)
      RHOMIN=ECAL(NTOTAL)
      RETURN
        END
C
C      *** SUB ***
      SUBROUTINE WILSON
C
      REAL AF(130)
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6)
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
      COMMON /CZ25/ Y1(50),Y2(50),Y3(50),NWILSTEP
C
        DO I=1,130
           AF(I)=0.0
        END DO
        RHOSTEP=(RHOMAX-RHOMIN)/NWILSTEP
        DO 112 J=1,NWILSTEP
          SUMFAT=0.0
          SUMFAT1=0.0
          SUMFAT2=0.0
          SUMFAT0=0.0
          SUMFAT10=0.0
          SUMFAT20=0.0
          SUMFAT30=0.0
          RRRMIN=RHOMIN+(J-1)*RHOSTEP
          RRRMAX=RRRMIN+RHOSTEP
          X(J)=0.5*(RRRMIN+RRRMAX)
        D=0.25*X(J)
          DO 120 II=1,NATOM
            JJ=ZATOM(II)
          AF(II)=FATOM(1,JJ)*EXP(-FATOM(2,JJ)*D)+
     *             FATOM(3,JJ)*EXP(-FATOM(4,JJ)*D)+
     *             FATOM(5,JJ)*EXP(-FATOM(6,JJ)*D)+
     *             FATOM(7,JJ)*EXP(-FATOM(8,JJ)*D)+
     *             FATOM(9,JJ)
120       CONTINUE
          DO II=1,NATOMINF
          SUMFAT1=SUMFAT1+IATOM(II)*AF(II)*AF(II)
          END DO
          DO II=NATOMINF+1,NATOM
          SUMFAT2=SUMFAT2+IATOM(II)*AF(II)*AF(II)
          END DO
        SUMFAT=SUMFAT1+SUMFAT2
        DO 122 I=1,NTOTAL
            IF(RHO(I).GE.RRRMIN.AND.RHO(I).LE.RRRMAX) THEN
              IF(HKLM(4,I).EQ.0.AND.HKLM(1,I).NE.0) THEN
                SUMFAT10=SUMFAT10+FO(I)*FO(I)*NNNSYM(I)
              ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).NE.0) THEN
                SUMFAT20=SUMFAT20+FO(I)*FO(I)*NNNSYM(I)
              ELSEIF(HKLM(1,I).EQ.0.AND.HKLM(4,I).EQ.0) THEN
                SUMFAT30=SUMFAT30+FO(I)*FO(I)*NNNSYM(I)
              ENDIF
            ENDIF
122        CONTINUE
          Y(J)=(SUMFAT10+SUMFAT20+SUMFAT30)/SUMFAT
          Y1(J)=(SUMFAT10+0.25*SUMFAT30)/SUMFAT1
          Y2(J)=(SUMFAT20+0.25*SUMFAT30)/SUMFAT2
          Y3(J)=(SUMFAT10+SUMFAT20+0.25*SUMFAT30)/SUMFAT
112      CONTINUE
        IF(IWILS.EQ.2) GOTO 128
        DO J=1,NWILSTEP
          Y(J)=LOG(Y(J))
        END DO
        DO I=1,NWILSTEP
           W(I)=1.0
        END DO
        NWLS=NWILSTEP
      CALL ORTHPL(2,NWLS,X,Y,W,B,A)
        IF(B(1).LE.0.0) THEN
          WRITE(10,*)
          WRITE(10,*)
          WRITE(10,125)
          WRITE(10,*)
          WRITE(10,*)
        ELSE
        ENDIF
      DO I=1,NWILSTEP
           P=X(I)
           Y(I)=B(1)+B(2)*P
      END DO
125     FORMAT(1X,'**** WILSON STATISTICS BYPASSED !')
128     CONTINUE
      RETURN
        END
C
C       *** SUB-ORTHPL ***
      SUBROUTINE ORTHPL(M1,N,X,Y,W,B,A)
      REAL X(N),Y(N),W(N),B(M1),A(2,M1)
      DO 10 K=1,M1
         A(1,K)=0.0
         A(2,K)=0.0
         B(K)=0.0
10      CONTINUE
      D2=1.0
      DO 15 J=1,M1
         A(2,J)=1.0
         IF(J.EQ.1) GOTO 12
         DO 11 K=2,J
           S=A(1,K-1)-AA*A(2,K-1)-BB*A(1,K)
           A(1,K)=A(2,K-1)
           A(2,K-1)=S
11         CONTINUE
12         D1=0.0
         XPP=0.0
         FP=0.0
           DO 14 I=1,N
           P=0.0
           DO 13 K=1,J
               JK=J-K
               P=P*X(I)+A(2,JK+1)
13           CONTINUE
             Q=W(I)*P
             D1=D1+Q*P
             XPP=XPP+Q*X(I)*P
             FP=FP+Q*Y(I)
14         CONTINUE
           AA=XPP/D1
           BB=D1/D2
           C=FP/D1
           D2=D1
           DO 16 K=1,J
              B(K)=B(K)+C*A(2,K)
16         CONTINUE
15      CONTINUE
      RETURN
      END
C
C      *** SUB ***
      SUBROUTINE ATOMSCAT
C
        COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
C
      DATA (FATOM(1,J),J=1,98)/0.489918, 0.873400,
     *    1.128200,   1.591900,   2.054500,
     *    2.310000,  12.212600,   3.048500,   3.539200,   3.955300,
     *    4.762600,   5.420400,   6.420200,   6.291500,   6.434500,
     *    6.905300,  11.460400,   7.484500,   8.218600,   8.626600,
     *    9.189000,   9.759500,  10.297100,  10.640600,  11.281900,
     *   11.769500,  12.284100,  12.837600,  13.338000,  14.074300,
     *   15.235400,  16.081600,  16.672300,  17.000601,  17.178900,
     *   17.355499,  17.178400,  17.566299,  17.775999,  17.876499,
     *   17.614201,   3.702500,  19.130100,  19.267401,  19.295700,
     *   19.331900,  19.280800,  19.221399,  19.162399,  19.188900,
     *   19.641800,  19.964399,  20.147200,  20.293301,  20.389200,
     *   20.336100,  20.577999,  21.167101,  22.044001,  22.684500,
     *   23.340500,  24.004200,  24.627399,  25.070900,  25.897600,
     *   26.507000,  26.904900,  27.656300,  28.181900,  28.664101,
     *   28.947599,  29.143999,  29.202400,  29.081800,  28.762100,
     *   28.189400,  27.304899,  27.005899,  16.881901,  20.680901,
     *   27.544600,  31.061701,  33.368900,  34.672600,  35.316299,
     *   35.563099,  35.929901,  35.763000,  35.659698,  35.564499,
     *   35.884701,  36.022800,  36.187401,  36.525398,  36.670601,
     *   36.648800,  36.788101,  36.918499/
      DATA (FATOM(2,J),J=1,98)/20.659300,9.103700,
     *   3.954600,  43.642700,  23.218500,
     *   20.843901,   0.005700,  13.277100,  10.282500,   8.404200,
     *    3.285000,   2.827500,   3.038700,   2.438600,   1.906700,
     *    1.467900,   0.010400,   0.907200,  12.794900,  10.442100,
     *    9.021300,   7.850800,   6.865700,   6.103800,   5.340900,
     *    4.761100,   4.279100,   3.878500,   3.582800,   3.265500,
     *    3.066900,   2.850900,   2.634500,   2.409800,   2.172300,
     *    1.938400,   1.788800,   1.556400,   1.402900,   1.276180,
     *    1.188650,   0.277200,   0.864132,   0.808520,   0.751536,
     *    0.698655,   0.644600,   0.594600,   0.547600,   5.830300,
     *    5.303400,   4.817420,   4.347000,   3.928200,   3.569000,
     *    3.216000,   2.948170,   2.812190,   2.773930,   2.662480,
     *    2.562700,   2.472740,   2.387900,   2.253410,   2.242560,
     *    2.180200,   2.070510,   2.073560,   2.028590,   1.988900,
     *    1.901820,   1.832620,   1.773330,   1.720290,   1.671910,
     *    1.629030,   1.592790,   1.512930,   0.461100,   0.545000,
     *    0.655150,   0.690200,   0.704000,   0.700999,   0.685870,
     *    0.663100,   0.646453,   0.616341,   0.589092,   0.563359,
     *    0.547751,   0.529300,   0.511929,   0.499384,   0.483629,
     *    0.465154,   0.451018,   0.437533/
      DATA (FATOM(3,J),J=1,98)/0.262003,0.630900,
     *    0.750800,   1.127800,   1.332600,
     *    1.020000,   3.132200,   2.286800,   2.641200,   3.112500,
     *    3.173600,   2.173500,   1.900200,   3.035300,   4.179100,
     *    5.203400,   7.196400,   6.772300,   7.439800,   7.387300,
     *    7.367900,   7.355800,   7.351100,   7.353700,   7.357300,
     *    7.357300,   7.340900,   7.292000,   7.167600,   7.031800,
     *    6.700600,   6.374700,   6.070100,   5.819600,   5.235800,
     *    6.728600,   9.643500,   9.818400,  10.294600,  10.948000,
     *   12.014400,  17.235600,  11.094800,  12.918200,  14.350100,
     *   15.501700,  16.688499,  17.644400,  18.559601,  19.100500,
     *   19.045500,  19.013800,  18.994900,  19.029800,  19.106199,
     *   19.297001,  19.599001,  19.769501,  19.669701,  19.684700,
     *   19.609501,  19.425800,  19.088600,  19.079800,  18.218500,
     *   17.638300,  17.294001,  16.428499,  15.885100,  15.434500,
     *   15.220800,  15.172600,  15.229300,  15.430000,  15.718900,
     *   16.155001,  16.729601,  17.763901,  18.591299,  19.041700,
     *   19.158400,  13.063700,  12.951000,  15.473300,  19.021099,
     *   21.281601,  23.054701,  22.906401,  23.103201,  23.421900,
     *   23.294800,  23.412800,  23.596399,  23.808300,  24.099199,
     *   24.409599,  24.773600,  25.199499/
      DATA (FATOM(4,J),J=1,98)/7.740390,3.356800,
     *    1.052400,   1.862300,   1.021000,
     *   10.207500,   9.893300,   5.701100,   4.294400,   3.426200,
     *    8.842200,  79.261101,   0.742600,  32.333698,  27.157000,
     *   22.215099,   1.166200,  14.840700,   0.774800,   0.659900,
     *    0.572900,   0.500000,   0.438500,   0.392000,   0.343200,
     *    0.307200,   0.278400,   0.256500,   0.247000,   0.233300,
     *    0.241200,   0.251600,   0.264700,   0.272600,  16.579599,
     *   16.562300,  17.315100,  14.098800,  12.800600,  11.916000,
     *   11.766000,   1.095800,   8.144870,   8.434670,   8.217580,
     *    7.989290,   7.472600,   6.908900,   6.377600,   0.503100,
     *    0.460700,   0.420885,   0.381400,   0.344000,   0.310700,
     *    0.275600,   0.244475,   0.226836,   0.222087,   0.210628,
     *    0.202088,   0.196451,   0.194200,   0.181951,   0.196143,
     *    0.202172,   0.197940,   0.223545,   0.238849,   0.257119,
     *    9.985190,   9.599900,   9.370460,   9.225900,   9.092270,
     *    8.979480,   8.865530,   8.811740,   8.621600,   8.448400,
     *    8.707510,   2.357600,   2.923800,   3.550780,   3.974580,
     *    4.069100,   4.176190,   3.871350,   3.651550,   3.462040,
     *    3.415190,   3.325300,   3.253960,   3.263710,   3.206470,
     *    3.089970,   3.046190,   3.007750/
      DATA (FATOM(5,J),J=1,98)/0.196767,0.311200,
     *    0.617500,   0.539100,   1.097900,
     *    1.588600,   2.012500,   1.546300,   1.517000,   1.454600,
     *    1.267400,   1.226900,   1.593600,   1.989100,   1.780000,
     *    1.437900,   6.255600,   0.653900,   1.051900,   1.589900,
     *    1.640900,   1.699100,   2.070300,   3.324000,   3.019300,
     *    3.522200,   4.003400,   4.443800,   5.615800,   5.165200,
     *    4.359100,   3.706800,   3.431300,   3.973100,   5.637700,
     *    5.549300,   5.139900,   5.422000,   5.726290,   5.417320,
     *    4.041830,  12.887600,   4.649010,   4.863370,   4.734250,
     *    5.295370,   4.804500,   4.461000,   4.294800,   4.458500,
     *    5.037100,   6.144870,   7.513800,   8.976700,  10.662000,
     *   10.888000,  11.372700,  11.851300,  12.385600,  12.774000,
     *   13.123500,  13.439600,  13.760300,  13.851800,  14.316700,
     *   14.559600,  14.558300,  14.977900,  15.154200,  15.308700,
     *   15.100000,  14.758600,  14.513500,  14.432700,  14.556400,
     *   14.930500,  15.611500,  15.713100,  25.558201,  21.657499,
     *   15.538000,  18.441999,  16.587700,  13.113800,   9.498870,
     *    8.003700,  12.143900,  12.473900,  12.597700,  12.747300,
     *   14.189100,  14.949100,  15.640200,  16.770700,  17.341499,
     *   17.399000,  17.891899,  18.331699/
      DATA (FATOM(6,J),J=1,98)/49.551899,22.927601,
     *   85.390503,103.483002, 60.349800,
     *    0.568700,  28.997499,   0.323900,   0.261500,   0.230600,
     *    0.313600,   0.380800,  31.547199,   0.678500,   0.526000,
     *    0.253600,  18.519400,  43.898300, 213.186996,  85.748398,
     *  136.108002,  35.633801,  26.893801,  20.262600,  17.867399,
     *   15.353500,  13.535900,  12.176300,  11.396600,  10.316300,
     *   10.780500,  11.446800,  12.947900,  15.237200,   0.260900,
     *    0.226100,   0.274800,   0.166400,   0.125599,   0.117622,
     *    0.204785,  11.004000,  21.570700,  24.799700,  25.874901,
     *   25.205200,  24.660500,  24.700800,  25.849899,  26.890900,
     *   27.907400,  28.528400,  27.766001,  26.465900,  24.387899,
     *   20.207300,  18.772600,  17.608299,  16.766899,  15.885000,
     *   15.100900,  14.399600,  13.754600,  12.933100,  12.664800,
     *   12.189900,  11.440700,  11.360400,  10.997500,  10.664700,
     *    0.261033,   0.275116,   0.295977,   0.321703,   0.350500,
     *    0.382661,   0.417916,   0.424593,   1.482600,   1.572900,
     *    1.963470,   8.618000,   8.793700,   9.556420,  11.382400,
     *   14.042200,  23.105200,  19.988701,  18.599001,  17.830900,
     *   16.923500,  16.092699,  15.362200,  14.945500,  14.313600,
     *   13.434600,  12.894600,  12.404400/
      DATA (FATOM(7,J),J=1,98)/0.049879,0.178000,
     *    0.465300,   0.702900,   0.706800,
     *    0.865000,   1.166300,   0.867000,   1.024300,   1.125100,
     *    1.112800,   2.307300,   1.964600,   1.541000,   1.490800,
     *    1.586300,   1.645500,   1.644200,   0.865900,   1.021100,
     *    1.468000,   1.902100,   2.057100,   1.492200,   2.244100,
     *    2.304500,   2.348800,   2.380000,   1.673500,   2.410000,
     *    2.962300,   3.683000,   4.277900,   4.354300,   3.985100,
     *    3.537500,   1.529200,   2.669400,   3.265880,   3.657210,
     *    3.533460,   3.742900,   2.712630,   1.567560,   1.289180,
     *    0.605844,   1.046300,   1.602900,   2.039600,   2.466300,
     *    2.682700,   2.523900,   2.273500,   1.990000,   1.495300,
     *    2.695900,   3.287190,   3.330490,   2.824280,   2.851370,
     *    2.875160,   2.896040,   2.922700,   3.545450,   2.953540,
     *    2.965770,   3.638370,   2.982330,   2.987060,   2.989630,
     *    3.716010,   4.300130,   4.764920,   5.119820,   5.441740,
     *    5.675890,   5.833770,   5.783700,   5.860000,   5.967600,
     *    5.525930,   5.969600,   6.469200,   7.025880,   7.425180,
     *    7.443300,   2.112530,   3.210970,   4.086550,   4.807030,
     *    4.172870,   4.188000,   4.185500,   3.479470,   3.493310,
     *    4.216650,   4.232840,   4.243910/
      DATA (FATOM(8,J),J=1,98)/2.201590,0.982100,
     *  168.261002,  0.542000,   0.140300,
     *   51.651199,   0.582600,  32.908901,  26.147600,  21.718399,
     *  129.423996,   7.193700,  85.088600,  81.693703,  68.164497,
     *   56.172001,  47.778400,  33.392899,  41.684101, 178.436996,
     *   51.353100, 116.105003, 102.477997,  98.739899,  83.754303,
     *   76.880501,  71.169197,  66.342102,  64.812599,  58.709702,
     *   61.413502,  54.762501,  47.797199,  43.816299,  41.432800,
     *   39.397202, 164.934006, 132.376007, 104.353996,  87.662697,
     *   69.795700,  61.658401,  86.847198,  94.292801,  98.606201,
     *   76.898598,  99.815598,  87.482498,  92.802902,  83.957100,
     *   75.282501,  70.840302,  66.877602,  64.265800, 213.904007,
     *  167.201996, 133.123993, 127.112999, 143.643997, 137.903000,
     *  132.720993, 128.007004, 123.174004, 101.398003, 115.362000,
     *  111.874001,  92.656601, 105.703003, 102.960999, 100.417000,
     *   84.329803,  72.028999,  63.364399,  57.056000,  52.086102,
     *   48.164700,  45.001099,  38.610298,  36.395599,  38.324600,
     *   45.814899,  47.257900,  48.009300,  47.004501,  45.471500,
     *   44.247299, 150.645004, 142.324997, 117.019997,  99.172203,
     *  105.250999, 100.612999,  97.490799, 105.980003, 102.273003,
     *   88.483398,  86.002998,  83.788101/
      DATA (FATOM(9,J),J=1,98)/0.001305,0.006400,
     *    0.037700,   0.038500,  -0.193200,
     *    0.215600, -11.529000,   0.250800,   0.277600,   0.351500,
     *    0.676000,   0.858400,   1.115100,   1.140700,   1.114900,
     *    0.866900,  -9.557400,   1.444500,   1.422800,   1.375100,
     *    1.332900,   1.280700,   1.219900,   1.183200,   1.089600,
     *    1.036900,   1.011800,   1.034100,   1.191000,   1.304100,
     *    1.718900,   2.131300,   2.531000,   2.840900,   2.955700,
     *    2.825000,   3.487300,   2.506400,   1.912130,   2.069290,
     *    3.755910,   4.387500,   5.404280,   5.378740,   5.328000,
     *    5.265930,   5.179000,   5.069400,   4.939100,   4.782100,
     *    4.590900,   4.352000,   4.071200,   3.711800,   3.335200,
     *    2.773100,   2.146780,   1.862640,   2.058300,   1.984860,
     *    2.028760,   2.209630,   2.574500,   2.419600,   3.583240,
     *    4.297280,   4.567960,   5.920460,   6.756210,   7.566720,
     *    7.976280,   8.581540,   9.243540,   9.887500,  10.472000,
     *   11.000500,  11.472200,  11.688300,  12.065800,  12.608900,
     *   13.174600,  13.411800,  13.578200,  13.677000,  13.710800,
     *   13.690500,  13.724700,  13.621100,  13.526600,  13.431400,
     *   13.428700,  13.396600,  13.357300,  13.381200,  13.359200,
     *   13.288700,  13.275400,  13.267400/
      RETURN
      END
C
C     *** SUB ***
      SUBROUTINE SOLVOR(PHSHIFT,ICENT)
      INTEGER*2 IPATH,MARKNO(2000),PHASO(2000),PHASAV(2000)
      INTEGER*2 ISHIFT(360),PHSHIFT
      REAL PHFOM(360)
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ999/PHFOM,ISHIFT,NSHI
        NSTEP=45
        NSHI=180
C
        DO I=NSTART,NEND
           J=PHASO(I)
           PHASO(I)=PHASAV(I)
           PHASAV(I)=J
        END DO
C
        DO I=1,360
           PHFOM(I)=0
        END DO
        IF(ICENT.EQ.1) THEN
           DO I=NSTART,NEND
              IPHFOM1=PHASO(I)-PHASAV(I)
              IPHFOM2=IPHFOM1+180
              IF(IPHFOM2.EQ.360) IPHFOM2=0
              PHFOM(1)=PHFOM(1)+IABS(IPHFOM1)
              PHFOM(2)=PHFOM(2)+IABS(IPHFOM2)
           END DO
           IF(PHFOM(1).LE.PHFOM(2)) THEN
              PHSHIFT=0
           ELSE
              PHSHIFT=180
           ENDIF
        ELSEIF(ICENT.NE.1) THEN
           DO J=-NSHI,NSHI,NSTEP
              ISHIFT(J)=J
              DO I=NSTART,NEND
                 PHFOM(J)=PHFOM(J)+IABS(PHASO(I)+J-PHASAV(I))
              END DO
              PHFOM(J)=PHFOM(J)/FLOAT(NEND-NSTART)
           END DO
              CALL SEQUSHIF
              PHSHIFT=ISHIFT(1)
        ELSE
        ENDIF
        DO I=NSTART,NEND
           PHASO(I)=PHASO(I)+PHSHIFT
           IF(PHASO(I).GE.360) THEN
              PHASO(I)=PHASO(I)-360
           ELSEIF(PHASO(I).LT.0) THEN
              PHASO(I)=PHASO(I)+360
           ENDIF
        END DO
        RETURN
        END
C      *** SUB ***
      SUBROUTINE RINDW
      LOGICAL ALL
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),H(4),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6),IPACK
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
      NRE=0
      NREF=1
      DO 2100 I=1,NTOTAL
         NNNSYM(I)=0
         DO 2200 J=1,NSYM
            DO 2300 K=1,IDIN
               H(K)=0
               DO 2400 L=1,IDIN
                  H(K)=H(K)+HKLM(L,I)*MX(L,K,J)
2400               CONTINUE
2300            CONTINUE
            CALL HPACK(H,IPACK)
            PPTR=0.0
            DO IPTR=1,IDIN
               PPTR=PPTR+HKLM(IPTR,I)*TRANL(IPTR,J)
            END DO
            DO 2020 IJK=NREF,NRE
             IF(IPACK.EQ.IHP3(IJK)) GOTO 2200
2020            CONTINUE
            NRE=NRE+1
            NNNSYM(I)=NNNSYM(I)+1
            IHP3(NRE)=IPACK
              HM1(NRE)=H(1)
            HM4(NRE)=H(4)
            PHAS(1,NRE)=I
            PHAS(2,NRE)=PPTR*360.0
2200         CONTINUE
         NREF=NRE
2100      CONTINUE
      NREF=NRE
C
      JUMP=NREF
130      IF(JUMP.LE.1) GOTO 128
      JUMP=JUMP/2
      IEND=NREF-JUMP
134      ALL=.TRUE.
      DO 135 ITOP=1,IEND
         IBOT=ITOP+JUMP
         XX=IHP3(ITOP)
         YY=IHP3(IBOT)
         IF(XX.LE.YY) GOTO 135
         IHH0=IHP3(ITOP)
         IHP3(ITOP)=IHP3(IBOT)
         IHP3(IBOT)=IHH0
         IHH0=HM4(ITOP)
         HM4(ITOP)=HM4(IBOT)
         HM4(IBOT)=IHH0
         IHH0=HM1(ITOP)
         HM1(ITOP)=HM1(IBOT)
         HM1(IBOT)=IHH0
         IHH0=PHAS(1,ITOP)
         PHAS(1,ITOP)=PHAS(1,IBOT)
         PHAS(1,IBOT)=IHH0
         IHH0=PHAS(2,ITOP)
         PHAS(2,ITOP)=PHAS(2,IBOT)
         PHAS(2,IBOT)=IHH0         
         ALL=.FALSE.
135      CONTINUE
      IF(.NOT.ALL) THEN
         GOTO 134
      ELSE
         GOTO 130
      ENDIF
         GOTO 130
128      RETURN
      END
C      *** SUB ***
      SUBROUTINE WHOLHKLM
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),H(4),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6),IPACK
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
      NRE=0
      NREF=1
      DO 2500 I=1,NTOTAL
         NNNSYM(I)=0
         DO 2400 J=1,NSYM
            DO 2150 K=1,IDIN
               H(K)=0
               DO 2100 L=1,IDIN
                  H(K)=H(K)+HKLM(L,I)*MX(L,K,J)
 2100               CONTINUE
 2150            CONTINUE
            CALL HPACK(H,IPACK)
            PPTR=0.0
            DO IPTR=1,IDIN
               PPTR=PPTR+HKLM(IPTR,I)*TRANL(IPTR,J)
            END DO
            DO 2170 IJK=NREF,NRE
             IF(IPACK.EQ.IHP3(IJK)) GOTO 2400
 2170            CONTINUE
            NRE=NRE+1
            NNNSYM(I)=NNNSYM(I)+1
            IHP3(NRE)=IPACK
              IPHAS12=PHASO(I)+PPTR*360.0
              IF(IPHAS12.LT.0) THEN
                 IPHAS12=MOD(IPHAS12,360)+360
              ELSE
                 IPHAS12=MOD(IPHAS12,360)
              ENDIF
 2400         CONTINUE
         NREF=NRE
 2500      CONTINUE
      RETURN
      END
C      *** SUB ***
      SUBROUTINE MULTIP
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),H(4),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6),IPACK
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
      NRE=0
      NREF=1
      DO 2100 I=1,NTOTAL
         NNNSYM(I)=0
         DO 2200 J=1,NSYM
            DO 2300 K=1,IDIN
               H(K)=0
               DO 2400 L=1,IDIN
                  H(K)=H(K)+HKLM(L,I)*MX(L,K,J)
2400               CONTINUE
2300            CONTINUE
            CALL HPACK(H,IPACK)
            DO 2020 IJK=NREF,NRE
             IF(IPACK.EQ.IHP3(IJK)) GOTO 2200
2020            CONTINUE
            NRE=NRE+1
            NNNSYM(I)=NNNSYM(I)+1
            IHP3(NRE)=IPACK
2200         CONTINUE
         NREF=NRE
2100      CONTINUE
      NREF=NRE
      RETURN
      END
C      *** SUB ***
      SUBROUTINE SIGM1
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),MMHA,PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),H(4),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6),INDEX
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
      TOTREL=0.0
      LGOON=NSTART
      DO I=NSTART,NEND
         MARK(I)=1
      END DO
3400      SKAMAX=SKAMAX/SGM23
      EKAPMIN=EKAPAPER*EKAPC1
      EMKAPMIN=EKAPAPER*EKAPC2
3501      DO 3500 I=LGOON,NEND
         MM=0
         SKAPAM=E(I)*EMKAPMIN*EKAPMIN
         DO J=1,IDIN
            H(J)=HKLM(J,I)
         END DO
           MMHA=IABS(H(4))
         CALL HPACK(H,INDEX)
         IND1=1
         IND2=NREF
           IF(IABS(HM4(IND1)).GT.MMHA) GOTO 3120
           IF(IABS(HM4(IND2)).GT.MMHA) GOTO 3180
3025         IF(IHP3(IND2)+IHP3(IND1)-INDEX) 3120,3200,3180
3120         IND1=IND1+1
C         IF(IND1.GT.IND2) GOTO 3260
           IF(IABS(HM4(IND1)).GT.MMHA) GOTO 3120
3121         IF(IHP3(IND2)+IHP3(IND1)-INDEX) 3120,3200,3180 
3180         IND2=IND2-1
         IF(IND2.LE.IND1) GOTO 3260
           IF(IABS(HM4(IND2)).GT.MMHA) GOTO 3180
         GOTO 3121
3200         IM=PHAS(1,IND2)
         IS=PHAS(1,IND1)
         SKAPA=E(I)*E(IM)*E(IS)
         IF(SKAPA.LT.SKAPAM.OR.SKAPA.GT.SKAMAX) GOTO 3320
         MM=MM+1
         N2=2*MM
         N1=N2-1
         IREL(N1,I)=IND1
         IREL(N2,I)=IND2
         IF(MM.GE.MREL) GOTO 3260
3320         GOTO 3120 
3260         MMM(I)=MM
         IF(MMM(I).LE.0) THEN
           CALL ERR2(H,I,LGOON,*3400)
         ELSE
         ENDIF
         TOTREL=TOTREL+MMM(I)
3500      CONTINUE
      SKAMAX=SKAMAX*SGM23
      RETURN
      END
C      *** SUB ***
        SUBROUTINE SIGM2
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),H(4),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6),INDEX
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
      TOTREL=0.0
      LGOON=NSTART
      DO I=NSTART,NEND
         MARK(I)=1
      END DO
8000      FORMAT(1X,I12,2I5)
3400      SKAMAX=SKAMAX/SGM23
      EKAPMIN=EKAPAPER*EKAPC1
      EMKAPMIN=EKAPAPER*EKAPC2
3501      DO 3500 I=LGOON,NEND
         MM=0
         SKAPAM=E(I)*EMKAPMIN*EKAPMIN
         DO J=1,IDIN
            H(J)=HKLM(J,I)
         END DO
         CALL HPACK(H,INDEX)
         IND1=1
         IND2=NREF
           IF(HM4(IND1).NE.H(4).AND.HM4(IND1).NE.0) GOTO 3120
           IF(HM4(IND2).NE.H(4).AND.HM4(IND2).NE.0) GOTO 3180
3025         IF(IHP3(IND2)+IHP3(IND1)-INDEX) 3120,3200,3180
3120         IND1=IND1+1
C         IF(IND1.GT.IND2) GOTO 3260
           IF(HM4(IND1).NE.H(4).AND.HM4(IND1).NE.0) GOTO 3120
3121         IF(IHP3(IND2)+IHP3(IND1)-INDEX) 3120,3200,3180 
3180         IND2=IND2-1
         IF(IND2.LE.IND1) GOTO 3260
           IF(HM4(IND2).NE.H(4).AND.HM4(IND2).NE.0) GOTO 3180
         GOTO 3121
3200         IM=PHAS(1,IND2)
         IS=PHAS(1,IND1)
         SKAPA=E(I)*E(IM)*E(IS)
         IF(SKAPA.LT.SKAPAM.OR.SKAPA.GT.SKAMAX) GOTO 3320
         MM=MM+1
         N2=2*MM
         N1=N2-1
         IREL(N1,I)=IND1
         IREL(N2,I)=IND2
         IF(MM.GE.MREL) GOTO 3260
3320         GOTO 3120 
3260         MMM(I)=MM
         IF(MMM(I).LE.0) THEN
           CALL ERR2(H,I,LGOON,*3400)
         ELSE
         ENDIF
         TOTREL=TOTREL+MMM(I)
3500      CONTINUE
      SKAMAX=SKAMAX*SGM23
      RETURN
      END
C      *** SUB ***
      SUBROUTINE SIGM3(IJSTART,IJEND)
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),H(4),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6),INDEX
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
      TOTREL=0.0
      LGOON=IJSTART
      DO I=IJSTART,IJEND
         MARK(I)=1
      END DO
3400      SKAMAX=SKAMAX/SGM23
      EKAPMIN=EKAPAPER*EKAPC1
      EMKAPMIN=EKAPAPER*EKAPC2
3501      DO 3500 I=LGOON,IJEND
         MM=0
         SKAPAM=E(I)*EMKAPMIN*EKAPMIN
         DO J=1,IDIN
            H(J)=HKLM(J,I)
         END DO
         CALL HPACK(H,INDEX)
         IND1=1
         IND2=NREF
3025         IF(IHP3(IND2)+IHP3(IND1)-INDEX) 3120,3200,3180
3120         IND1=IND1+1
3121         IF(IHP3(IND2)+IHP3(IND1)-INDEX) 3120,3200,3180 
3180         IND2=IND2-1
         IF(IND2.LE.IND1) GOTO 3260
         GOTO 3121
3200         IM=PHAS(1,IND2)
         IS=PHAS(1,IND1)
         SKAPA=E(I)*E(IM)*E(IS)
         IF(SKAPA.LT.SKAPAM.OR.SKAPA.GT.SKAMAX) GOTO 3320
         MM=MM+1
         N2=2*MM
         N1=N2-1
         IREL(N1,I)=IND1
         IREL(N2,I)=IND2
         IF(MM.GE.MREL) GOTO 3260
3320         GOTO 3120 
3260         MMM(I)=MM
         IF(MMM(I).LE.0) THEN
           CALL ERR2(H,I,LGOON,*3400)
         ELSE
         ENDIF
         TOTREL=TOTREL+MMM(I)
3500      CONTINUE
      SKAMAX=SKAMAX*SGM23
      RETURN
      END
C      *** SUB ***
      SUBROUTINE SIGM4
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),H(4),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6),INDEX
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
      TOTREL=0.0
      LGOON=NSTART
      DO I=NSTART,NEND
         MARK(I)=1
      END DO
3400      SKAMAX=SKAMAX/SGM23
      EKAPMIN=EKAPAPER*EKAPC1
      EMKAPMIN=EKAPAPER*EKAPC2
3501      DO 3500 I=LGOON,NEND
         MM=0
         SKAPAM=E(I)*EMKAPMIN*EKAPMIN
         DO J=1,IDIN
            H(J)=HKLM(J,I)
         END DO
         CALL HPACK(H,INDEX)
         IND1=1
         IND2=NREF
           IF(HM1(IND1).NE.0.AND.HM4(IND1).NE.0) GOTO 3120
           IF(HM1(IND2).NE.0.AND.HM4(IND2).NE.0) GOTO 3180
3025         IF(IHP3(IND2)+IHP3(IND1)-INDEX) 3120,3200,3180
3120         IND1=IND1+1
           IF(HM1(IND1).NE.0.AND.HM4(IND1).NE.0) GOTO 3120
3121         IF(IHP3(IND2)+IHP3(IND1)-INDEX) 3120,3200,3180 
3180         IND2=IND2-1
         IF(IND2.LE.IND1) GOTO 3260
           IF(HM1(IND2).NE.0.AND.HM4(IND2).NE.0) GOTO 3180
         GOTO 3121
3200         IM=PHAS(1,IND2)
         IS=PHAS(1,IND1)
         SKAPA=E(I)*E(IM)*E(IS)
         IF(SKAPA.LT.SKAPAM.OR.SKAPA.GT.SKAMAX) GOTO 3320
         MM=MM+1
         N2=2*MM
         N1=N2-1
         IREL(N1,I)=IND1
         IREL(N2,I)=IND2
         IF(MM.GE.MREL) GOTO 3260
3320         GOTO 3120 
3260         MMM(I)=MM
         IF(MMM(I).LE.0) THEN
           CALL ERR2(H,I,LGOON,*3400)
         ELSE
         ENDIF
         TOTREL=TOTREL+MMM(I)
3500      CONTINUE
      SKAMAX=SKAMAX*SGM23
      RETURN
      END
C
      SUBROUTINE ERR2(H,I,LGOON,*)
      CHARACTER*1 YN,YN1,YN2,YN3,YN4
      INTEGER*2 H(4),MARK(2000)
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      YN1='Y'
      YN2='y'
      YN3='N'
      YN4='n'
      WRITE(*,*)
      WRITE(*,*) '# KPMIN too large or KPMAX too small,'
      WRITE(*,*) '  NO sigma-2 relations found for the',
     *                ' following reflection'
      WRITE(*,10) I,(H(J),J=1,4)
10      FORMAT(1X,'  NO.:',I4,'    HKLM:',4I4)
1      WRITE(*,*)
      WRITE(*,*) '  Ignore?'
      WRITE(*,*) '  If Yes, its phase will not be DERIVED',
     *             ' and a RANDOM'
      WRITE(*,31)
31      FORMAT('$  value will be ASSIGNED to it as the',
     *             ' final result (Y/N)? ')
      READ(*,'(A)') YN
      WRITE(*,*)
      IF(YN.EQ.YN1.OR.YN.EQ.YN2) THEN
         MARK(I)=-1
         LGOON=I+1
           SKAMAX=SKAMAX*SGM23
         WRITE(10,30) (H(J),J=1,4)
30         FORMAT('  REFLECTION (HKLM):',4I4,
     *              ' is neglected in phasing!')
      WRITE(*,55) '$ ... Please wait ... '
55      FORMAT(A22)
            RETURN 1
      ELSEIF(YN.EQ.YN3.OR.YN.EQ.YN4) THEN
         WRITE(*,*) '  Please give a smaller KPMIN',
     *                ' or a larger KPMAX.'
           SKAMAX=SKAMAX*SGM23
         WRITE(*,20) EKAPAPER,SKAMAX
         WRITE(*,*)
20         FORMAT('   Current values of KPMIN and KPMAX:',F6.2,F10.2)
         WRITE(*,21) '$  New KPMIN: '
         READ(*,*) EKAPAPER
         WRITE(*,21) '$  New KPMAX: '
         READ(*,*) SKAMAX
         WRITE(10,40) EKAPAPER,SKAMAX
21         FORMAT(A14)
40         FORMAT(' # Your new EKAPAPER and SKAMAX: ',F6.2,F10.2,
     *            ' is used from')
         WRITE(10,50) (H(J),J=1,4)
           WRITE(10,*)
           WRITE(10,*)
50         FORMAT('    the reflection (HKLM): ',4I4,'  and on!')
         LGOON=I
      WRITE(*,55) '$ ... Please wait ... '
         RETURN 1
      ELSE
         WRITE(*,*) '# Incorrect answer, try again!'
         GOTO 1
      ENDIF
      RETURN
      END
C
C      *** SUB ***
      SUBROUTINE ORDREF
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6)
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
        DO I=1,IDIN
         PORID(I)=ZERPI(MFS)
      END DO
      NORREF=0
        IF(MFS.GE.1) THEN
         DO L=1,IDIN
            IHORID(L,4)=HKLM(L,NSTART)
         END DO
         NORREF=NORREF+4

         WRITE(10,4655)
         WRITE(10,*)
         WRITE(10,4658) (IHORID(JD,4),JD=1,IDIN),PORID(4)
         WRITE(10,*)
         WRITE(10,*)
         WRITE(10,*)
      ELSE
         IL=0
         NORREF=0      
         DO 4600 II=NSTART,NEND
         DO 4601 I=1,3
            IF(HS(I).EQ.0) GOTO 4601
            NORREF=NORREF+1
            N1=HS(I)/100
            N2=(HS(I)-N1*100)/10
            N3=HS(I)-N1*100-N2*10
            HS(NORREF)=HKLM(1,II)*N1+HKLM(2,II)*N2+HKLM(3,II)*N3
4601         CONTINUE
         DO I=1,NORREF
            IF(OMIGS(I).EQ.0) THEN
               HS(I)=HS(I)
            ELSE
               HS(I)=MOD(HS(I),OMIGS(I))
            ENDIF
         END DO
         IF(HS(1).EQ.0.AND.HS(2).EQ.0.AND.HS(3).EQ.0) GOTO 4600
         IL=IL+1
         DO J=1,3
            IHORID(J,IL)=HKLM(J,I)
         END DO
         IF(IL.EQ.NORREF) GOTO 4610
4600         CONTINUE
4610         WRITE(10,4655)
         WRITE(10,*)
         DO I=1,NORREF
            WRITE(10,4658) (IHORID(J,I),J=1,IDIN),PORID(I)
         END DO
         WRITE(10,*)
         WRITE(10,*)
         WRITE(10,*)
      ENDIF
4655  FORMAT(1X,'*** ORIGIN-FIXING REFLECTION & ITS PHASE ****')
4658  FORMAT(1X,4I4,I6)
4660  FORMAT(1X,'NUMBER OF ORIGIN-FIXING REFS:',I1)
4551  RETURN
      END
C      *** SUB *** PHASING PROGRAM
      SUBROUTINE PHATAN
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6)
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
      NCYCCTR=1
      NCYCL=1
      PHCTR=0.0
      IPHCTR1=0
      IPHCTR2=0
        NIOR=NEND-NSTART+1
      SGM0=0.5*SGM23
         NSET0=NSET+ISKIP
        IF(JPATH.EQ.3.AND.IPATH.EQ.1) THEN
        ELSE
       IF(ITE.EQ.1) THEN
5009      IF(ICENT.EQ.1) THEN          
         WRITE(10,5001)
          ELSE
         WRITE(10,5003)
          ENDIF
        WRITE(10,5002) NSET0
5002        FORMAT(1X,I6)
5001        FORMAT(1X,'SETNO.      CYCLNO.     DIFNO.')
5003        FORMAT(1X,'SETNO.      CYCLNO.     AVDIF.')
       ELSE
       ENDIF
        ENDIF
5010         DO 5000  I=NSTART,NEND
          IF(MARK(I).LT.0) GOTO 5000
          IF(IPATH.EQ.1) THEN
           IF(NWONK.LE.0.AND.MFS.LE.1) THEN
            IF(NCYCL.LE.NCYCFIX) THEN
                 DO IOR=1,4
                 IF(HKLM(1,I).EQ.IHORID(1,IOR).AND.
     *                HKLM(2,I).EQ.IHORID(2,IOR).AND.
     *                HKLM(3,I).EQ.IHORID(3,IOR).AND.
     *                HKLM(4,I).EQ.IHORID(4,IOR)) THEN
                    IPHASN=PORID(IOR)
                    WTN=1.0
                    GOTO 5025
                 ELSE
                 ENDIF
               END DO
            ELSE
            ENDIF
           ELSEIF(NWONK.GE.1) THEN
            IF(NCYCL.LE.NCYCFIX) THEN
              IF(MARKNO(I).GE.1) THEN
                IPHASN=PHASO(I)
                WTN=1.0
                GOTO 5025
              ELSE
              ENDIF
            ELSE
            ENDIF
           ENDIF
C
          ELSEIF(IPATH.EQ.2) THEN
           IF(NWONK.LE.0) THEN
            IF(NCYCL.LE.NCYCFIX) THEN
                 DO IOR=1,4
                 IF(HKLM(1,I).EQ.IHORID(1,IOR).AND.
     *                HKLM(2,I).EQ.IHORID(2,IOR).AND.
     *                HKLM(3,I).EQ.IHORID(3,IOR).AND.
     *                HKLM(4,I).EQ.IHORID(4,IOR)) THEN
                    IPHASN=PORID(IOR)
                    WTN=1.0
                    GOTO 5025
                 ELSE
                 ENDIF
               END DO
            ELSE
            ENDIF
           ELSE
            IF(NCYCL.LE.NCYCFIX) THEN
              IF(MARKNO(I).GE.1) THEN
                IPHASN=PHASO(I)
                WTN=1.0
                GOTO 5025
              ELSE
              ENDIF
            ELSE
            ENDIF
           ENDIF
C
          ELSEIF(IPATH.EQ.4) THEN
            IF(NCYCL.LE.NCYCFIX) THEN
              IF(MARKNO(I).GE.1) THEN
                IPHASN=PHASO(I)
                WTN=1.0
                GOTO 5025
              ELSE
              ENDIF
            ELSE
            ENDIF
          ENDIF
5015         TH=0.0
         BH=0.0
         DO 5100 ITH=1,MMM(I)
            JTH=2*ITH
            IR1=IREL(JTH-1,I)
            IR2=IREL(JTH,I)
            JM1=PHAS(1,IR1)
            JM2=PHAS(1,IR2)
            IPHASAT=PHASO(JM1)+PHAS(2,IR1)
            IPHASMA=PHASO(JM2)+PHAS(2,IR2)
            IPHHP=IPHASAT+IPHASMA
            EHHP=E(JM1)*E(JM2)
              IF(IPHHP.LT.0) THEN
               IPHHP=MOD(IPHHP,360)+360
              ELSE
               IPHHP=MOD(IPHHP,360)
              ENDIF
            WEHP=WT(JM1)*WT(JM2)*EHHP
            TH=TH+WEHP*SINT(IPHHP)
            BH=BH+WEHP*COST(IPHHP)
5100         CONTINUE
         XP=E(I)*SQRT(TH*TH+BH*BH)
         IF(ICENT.EQ.1) THEN
            IF(BH.GE.0.0) THEN
               IPHASN=0
            ELSE
               IPHASN=180
            ENDIF
         ELSE
            IF(BH.NE.0) THEN
C               AP=TH/BH
               PHASN=ATAN2(TH,BH)
               PHASN=PHASN*57.302
               IPHASN=INT(PHASN)
                 IF(IPHASN.LT.0) THEN
                    IPHASN=IPHASN+360
                 ENDIF
            ELSE
               IF(TH.GE.0) THEN
                  IPHASN=90
               ELSE
                  IPHASN=270
               ENDIF
            ENDIF
         ENDIF
         BP=SGM0*XP
         WTN=TANH(BP)
5025         IF(NCYCL.GT.NCYCCTR) THEN
              IPHCTR1=IPHASN-PHASO(I)
              IF(IPHCTR1.LT.0) IPHCTR1=IPHCTR1+360
              IF(IPHCTR1.GT.180) IPHCTR1=360-IPHCTR1
C            IPHCTR2=MAX(IPHASN,PHASO(I))
C            IF(IPHCTR2.NE.0) THEN
C               PHCTR=PHCTR+IPHCTR1/IPHCTR2
               PHCTR=PHCTR+IPHCTR1/180
C            ELSE
C            ENDIF
         ELSE
         ENDIF
C         IALTER.NE.1/EACH NEW PHASE AND WT WAS USED
C            IN PHASING THE NEXT REFLECTIONS
         IF(IALTER.EQ.1) THEN
            PHNF(I)=IPHASN
            WTNF(I)=WTN
         ELSE
            PHASO(I)=IPHASN
            WT(I)=WTN
         ENDIF
5000      CONTINUE
      IF(IALTER.EQ.1) THEN
         DO I=NSTART,NEND
            PHASO(I)=PHNF(I)
            WT(I)=WTNF(I)
         END DO
      ELSE
      ENDIF
      IF(NCYCL.GT.NCYCCTR) THEN
         CYCCTR=PHCTR/NIOR
C         CYCCTR=PHCTR*100.0/(NIOR*NIOR)*100.0/(NIOR*NIOR)
      ELSE
      ENDIF
        IF(JPATH.EQ.3.AND.IPATH.EQ.1) THEN
        ELSE
       IF(ITE.EQ.1) THEN
        NDIF=0
          IF(ICENT.EQ.1) THEN
         DO 5301 II=NSTART,NEND
            IF(PHAS00(II).EQ.IABS(PHASO(II))) GOTO 5301
            NDIF=NDIF+1
5301         CONTINUE
        ELSEIF(ICENT.EQ.0) THEN
           NDIFDP=0
         DO II=NSTART,NEND
            NDIFD=PHAS00(II)-PHASO(II)
              IF(NDIFD.LT.0) NDIFD=NDIFD+360
              IF(NDIFD.GT.180) NDIFD=360-NDIFD
              NDIFDP=NDIFDP+NDIFD
            NDIF=NDIF+1
           END DO
           NDIF=NDIFDP/NDIF
          ENDIF
        WRITE(10,5305) NCYCL,NDIF
5305        FORMAT(1X,10X,I6,6X,I6)
       ELSE
       ENDIF
        ENDIF
      IF(NCYCL.GT.NCYCCTR) THEN
         IF(CYCCTR.LE.CYCLCTR) GOTO 5329
         PHCTR=0.0
         IPHCTR1=0
         IPHCTR2=0
      ELSE
      ENDIF
      NCYCL=NCYCL+1
      IF(NCYCL.GT.NCLMAX) GOTO 5329
      GOTO 5010
5329    IF(JPATH.EQ.3.AND.IPATH.EQ.1) THEN
        ELSE
        IF(ITE.EQ.1) THEN
          WRITE(10,*)
          WRITE(10,*)
        ELSE
        ENDIF
        ENDIF
        CALL CALFOM
        RETURN
        END
C
C      ****** SUB-CALFOM ******
      SUBROUTINE CALFOM
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6)
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
        IF(IPATH.EQ.4.AND.MFS.EQ.0) THEN
           JSTART=NFORPS
           JEND=KEND
        ELSE
           JSTART=NSTART
           JEND=NEND
        ENDIF
      ALFH=0.0          
      Z=0.0
      ZEXP=0.0
      ZRAND=0.0
      EHCAL2=0.0
      EHOBS2=0.0
      SN=2.0*SGM23
      SGM2=SGM23*SGM23
        IF(IPATH.EQ.4.AND.MFS.EQ.0.AND.NORTYP.EQ.1) THEN
        EFOMMAX=ECAL(NSTART)
      DO 5415 I=NSTART,NEND
         IF(MARK(I).LT.0) GOTO 5415
         TH=0.0
         BH=0.0
         ALFAXP=0.0
         ALFAAN=0.0
         SUMEHP=0.0
         SUMEPS=0.0
         SUMEPC=0.0
         EH=EFOM(I)
         EH2=EH*EH
         DO 5515 ITH=1,MMM(I)
            JTH=2*ITH
            IR1=IREL(JTH-1,I)
            IR2=IREL(JTH,I)
            JM1=PHAS(1,IR1)
            JM2=PHAS(1,IR2)
            IPHASAT=PHASO(JM1)+PHAS(2,IR1)
            IPHASMA=PHASO(JM2)+PHAS(2,IR2)
            IPHHP=IPHASAT+IPHASMA
            WEHP=WT(JM1)*WT(JM2)
            EHHP=EFOM(JM1)*EFOM(JM2)
            WEHP=WEHP*EHHP
            SKAP2=SGM2*EH2*EHHP*EHHP
            SKAP1=SGM23*EH*EHHP
            IPHHP=MOD(IPHHP,360)+360
            IPHHP=MOD(IPHHP,360)
            ST=SINT(IPHHP)
            CT=COST(IPHHP)
            IF(SKAP1.LE.6.0) THEN
               ALFAXP=ALFAXP+SKAP2*(0.5658-0.1304*SKAP1+0.0106*SKAP2)
              ELSE
               ALFAXP=ALFAXP+SKAP1
            ENDIF
            ALFAAN=ALFAAN+SKAP2
            SUMEHP=SUMEHP+EHHP
            SUMEPS=SUMEPS+EHHP*ST
            SUMEPC=SUMEPC+EHHP*CT
            TH=TH+WEHP*ST
            BH=BH+WEHP*CT
5515            CONTINUE
            ALFH=SN*EH*SQRT(TH*TH+BH*BH)
            Z=Z+ALFH
            ZEXP=ZEXP+ALFAXP
            ZRAND=ZRAND+SQRT(ALFAAN)
            EAL2=SUMEPS*SUMEPS+SUMEPC*SUMEPC
            ECAL(I)=SQRT(EAL2)
            DO J=1,NNNSYM(I)
               EHCAL2=EHCAL2+EAL2
               EHOBS2=EHOBS2+EH2
            END DO
5415      CONTINUE
      ABSFOM(NSET)=(Z-ZRAND)/(ZEXP-ZRAND)
        ABSFOM(NSET)=ABS(ABSFOM(NSET))
      ABSF(NSET)=ABSFOM(NSET)
C
        SMU=SQRT(EHOBS2)/SQRT(EHCAL2)
      EHCAL2=0.0
      EHOBS2=0.0
      PNSIZ=0.0
      SN=2.0*SGM23
      SGM2=SGM23*SGM23
      DO 5416 I=JSTART,JEND
         IF(MARK(I).LT.0) GOTO 5416
         TH=0.0
         BH=0.0
         SUMEHP=0.0
         SUMEPS=0.0
         SUMEPC=0.0
         EH=EFOM(I)
         EH2=EH*EH
         EPER=EH/EFOMMAX
         DO 5516 ITH=1,MMM(I)
            JTH=2*ITH
            IR1=IREL(JTH-1,I)
            IR2=IREL(JTH,I)
            JM1=PHAS(1,IR1)
            JM2=PHAS(1,IR2)
            IPHASAT=PHASO(JM1)+PHAS(2,IR1)
            IPHASMA=PHASO(JM2)+PHAS(2,IR2)
            IPHHP=IPHASAT+IPHASMA
            WEHP=WT(JM1)*WT(JM2)
            EHHP=EFOM(JM1)*EFOM(JM2)
            WEHP=WEHP*EHHP
            SKAP2=SGM2*EH2*EHHP*EHHP
            SKAP1=SGM23*EH*EHHP
            IPHHP=MOD(IPHHP,360)+360
            IPHHP=MOD(IPHHP,360)
            ST=SINT(IPHHP)
            CT=COST(IPHHP)
            SUMEHP=SUMEHP+EHHP
            SUMEPS=SUMEPS+EHHP*ST
            SUMEPC=SUMEPC+EHHP*CT
            TH=TH+WEHP*ST
            BH=BH+WEHP*CT
5516            CONTINUE
            EAL2=SUMEPS*SUMEPS+SUMEPC*SUMEPC
            ECAL(I)=SQRT(EAL2)
              IF(EPER.LT.PZPER) THEN
                PSIZ(NSET)=PSIZ(NSET)+ECAL(I)*SMU
              PNSIZ=PNSIZ+1.0
            ELSE
            ENDIF
5416      CONTINUE
C
        IF(IPATH.EQ.4.AND.MFS.EQ.0) THEN
        PSI(NSET)=PSIZ(NSET)
        ELSE
       IF(PNSIZ.NE.0.0) THEN
        PSI(NSET)=PSIZ(NSET)/PNSIZ
         ELSE
         ENDIF
        ENDIF
      PSIZ(NSET)=PSI(NSET)
      XRK=0.0
      YRK=0.0
      DO 5615 I=NSTART,NEND
         XRK=XRK+ABS(EFOM(I)-ECAL(I)*SMU)
         YRK=YRK+EFOM(I)
5615      CONTINUE
      RKARL(NSET)=XRK/YRK
      RKAR(NSET)=RKARL(NSET)
CCC
        ELSE
      DO 5400 I=NSTART,NEND
         IF(MARK(I).LT.0) GOTO 5400
         TH=0.0
         BH=0.0
         ALFAXP=0.0
         ALFAAN=0.0
         SUMEHP=0.0
         SUMEPS=0.0
         SUMEPC=0.0
         EH=E(I)
         EH2=EH*EH
         DO 5500 ITH=1,MMM(I)
            JTH=2*ITH
            IR1=IREL(JTH-1,I)
            IR2=IREL(JTH,I)
            JM1=PHAS(1,IR1)
            JM2=PHAS(1,IR2)
            IPHASAT=PHASO(JM1)+PHAS(2,IR1)
            IPHASMA=PHASO(JM2)+PHAS(2,IR2)
            IPHHP=IPHASAT+IPHASMA
            WEHP=WT(JM1)*WT(JM2)
            EHHP=E(JM1)*E(JM2)
            WEHP=WEHP*EHHP
            SKAP2=SGM2*EH2*EHHP*EHHP
            SKAP1=SGM23*EH*EHHP
            IPHHP=MOD(IPHHP,360)+360
            IPHHP=MOD(IPHHP,360)
            ST=SINT(IPHHP)
            CT=COST(IPHHP)
            IF(SKAP1.LE.6.0) THEN
               ALFAXP=ALFAXP+SKAP2*(0.5658-0.1304*SKAP1+0.0106*SKAP2)
              ELSE
               ALFAXP=ALFAXP+SKAP1
            ENDIF
            ALFAAN=ALFAAN+SKAP2
            SUMEHP=SUMEHP+EHHP
            SUMEPS=SUMEPS+EHHP*ST
            SUMEPC=SUMEPC+EHHP*CT
            TH=TH+WEHP*ST
            BH=BH+WEHP*CT
5500            CONTINUE
            ALFH=SN*EH*SQRT(TH*TH+BH*BH)
            Z=Z+ALFH
            ZEXP=ZEXP+ALFAXP
            ZRAND=ZRAND+SQRT(ALFAAN)
            EAL2=SUMEPS*SUMEPS+SUMEPC*SUMEPC
            ECAL(I)=SQRT(EAL2)
            DO J=1,NNNSYM(I)
               EHCAL2=EHCAL2+EAL2
               EHOBS2=EHOBS2+EH2
            END DO
5400      CONTINUE
      ABSFOM(NSET)=(Z-ZRAND)/(ZEXP-ZRAND)
        ABSFOM(NSET)=ABS(ABSFOM(NSET))
      ABSF(NSET)=ABSFOM(NSET)
C
        SMU=SQRT(EHOBS2)/SQRT(EHCAL2)
      EHCAL2=0.0
      EHOBS2=0.0
      PNSIZ=0.0
      SN=2.0*SGM23
      SGM2=SGM23*SGM23
      DO 5401 I=JSTART,JEND
         IF(MARK(I).LT.0) GOTO 5401
         TH=0.0
         BH=0.0
         SUMEHP=0.0
         SUMEPS=0.0
         SUMEPC=0.0
         EH=E(I)
         EH2=EH*EH
         EPER=EH/EMAX
         DO 5501 ITH=1,MMM(I)
            JTH=2*ITH
            IR1=IREL(JTH-1,I)
            IR2=IREL(JTH,I)
            JM1=PHAS(1,IR1)
            JM2=PHAS(1,IR2)
            IPHASAT=PHASO(JM1)+PHAS(2,IR1)
            IPHASMA=PHASO(JM2)+PHAS(2,IR2)
            IPHHP=IPHASAT+IPHASMA
            WEHP=WT(JM1)*WT(JM2)
            EHHP=E(JM1)*E(JM2)
            WEHP=WEHP*EHHP
            SKAP2=SGM2*EH2*EHHP*EHHP
            SKAP1=SGM23*EH*EHHP
            IPHHP=MOD(IPHHP,360)+360
            IPHHP=MOD(IPHHP,360)
            ST=SINT(IPHHP)
            CT=COST(IPHHP)
            SUMEHP=SUMEHP+EHHP
            SUMEPS=SUMEPS+EHHP*ST
            SUMEPC=SUMEPC+EHHP*CT
            TH=TH+WEHP*ST
            BH=BH+WEHP*CT
5501            CONTINUE
            EAL2=SUMEPS*SUMEPS+SUMEPC*SUMEPC
            ECAL(I)=SQRT(EAL2)
              IF(EPER.LT.PZPER) THEN
                PSIZ(NSET)=PSIZ(NSET)+ECAL(I)*SMU
              PNSIZ=PNSIZ+1.0
            ELSE
            ENDIF
5401      CONTINUE
C
        IF(IPATH.EQ.4.AND.MFS.EQ.0) THEN
        PSI(NSET)=PSIZ(NSET)
        ELSE
       IF(PNSIZ.NE.0.0) THEN
        PSI(NSET)=PSIZ(NSET)/PNSIZ
         ELSE
         ENDIF
        ENDIF
      PSIZ(NSET)=PSI(NSET)
      XRK=0.0
      YRK=0.0
      DO 5600 I=NSTART,NEND
         XRK=XRK+ABS(E(I)-ECAL(I)*SMU)
         YRK=YRK+E(I)
5600      CONTINUE
      RKARL(NSET)=XRK/YRK
      RKAR(NSET)=RKARL(NSET)
        ENDIF
      RETURN
      END
C      ****** SUB-CFOM ******
      SUBROUTINE CFOM
      LOGICAL ALL
C
      INTEGER*2 MARKNO(2000),HKLM(4,2000),NATOMINF
      INTEGER*2 MMM(2000),IREL(600,2000),HM4(20000),HM1(20000)
      INTEGER*2 MARK(2000),NNNSYM(2000),NSAT(0:6),PHASAV(2000)
      INTEGER*2 IHORID(4,4),HS(3),OMIGS(3),MX(4,4,48)
      INTEGER*2 IATOM(130),ZATOM(130),NSETRC(1024),NDIFID(2000)
      INTEGER*2 PHAS(2,20000),PHASO(2000),PHAS00(2000),PHNF(2000)
      INTEGER*2 ZERPI(0:6),PI,RADI,PORID(4),IPATH
      INTEGER*4 IHP3(20000),NFSET(0:6)
C
      COMMON /CZ1/IDIN,ITE,NSETM,ISKIP,NSTART,NEND,IPATH,JPATH
      COMMON /CZ2/ZERPI,PI,IHORID,HS,OMIGS,RADI,PORID
      COMMON /CZ3/INDIC,IOVER,NSETRC,IVOID2,PZPER
      COMMON /CZ4/MREL,MREL2,NFSET,NSET,NFXST
      COMMON /CZ5/W1,W2,W3,NCYCFIX,NCLMAX,CYCLCTR
      COMMON /CZ7/NATOM,ZATOM,IATOM,NATOMINF,IVOID,NORTYP
      COMMON /CZ8/PHAS00,NDIFID,TOTREL
      COMMON /CZ9/WT(2000),WTNF(2000),IREL,MMM
      COMMON /CZ10/E(2000),ECAL(2000),E0(2000),EFOM(2000),PHNF
      COMMON /CZ11/EMAX,SUMFS(0:6),SCALE(6)
      COMMON /CZ12/SINT(0:450),COST(0:450)
      COMMON /CZ13/ABSFOM(1024),PSIZ(1024),RKARL(1024)
      COMMON /CZ14/ABSF(1024),PSI(1024),RKAR(1024),COFOM(1024)
      COMMON /CZ15/ABMAX,ABMIN,PSIMAX,PSIMIN,RKAMAX,RKAMIN
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      COMMON /CZ17/IALTER,MARK,NWONK,NFORPS,KEND
      COMMON /CZ18/EMKAPMIN,EKAPMIN,EKAPC1,EKAPC2
      COMMON /CZ19/FO(2000),MARKNO,PHASO,PHASAV
      COMMON /CZ20/NTOTAL,MFS,NSAT,HKLM
      COMMON /CZ21/NREF,IHP3,PHAS,HM4,HM1
      COMMON /CZ22/SKAMAX,EKAPAPER,SGM23
      COMMON /CZ23/CELL(6,2),FATOM(9,130),RHO(2000),RHOMIN,RHOMAX
      COMMON /CZ24/X(50),Y(50),W(50),B(2),A(2,2),BFACT,IWILS
C
        CALL SEQUFOM(ABSF,NSETM)
        CALL SEQUFOM(PSI,NSETM)
        CALL SEQUFOM(RKAR,NSETM)
C
      ABMIN=ABSF(1)
      ABMAX=ABSF(NSETM)
      IF((ABMAX-ABMIN).EQ.0.0) THEN
         ABXN=0.0
         IF(JPATH.EQ.3.AND.IPATH.EQ.1) THEN
         ELSE
         WRITE(10,*)
         WRITE(10,*)
         ENDIF
      ELSE
         ABXN=1.0/(ABMAX-ABMIN)      
      ENDIF
      PSIMIN=PSI(1)
      PSIMAX=PSI(NSETM)
        ESCAL=2.0/(PSIMIN+PSIMAX)
      IF((PSIMAX-PSIMIN).EQ.0.0) THEN
         PSIXN=0.0
         IF(JPATH.EQ.3.AND.IPATH.EQ.1) THEN
          ELSE
         WRITE(10,*)
         WRITE(10,*)
         ENDIF
      ELSE
         PSIXN=1.0/(PSIMAX-PSIMIN)
      ENDIF
      RKAMIN=RKAR(1)
      RKAMAX=RKAR(NSETM)
      IF((RKAMAX-RKAMIN).EQ.0.0) THEN
         RKAXN=0.0
         IF(JPATH.EQ.3.AND.IPATH.EQ.1) THEN
         ELSE
         WRITE(10,*)
         WRITE(10,*)
         ENDIF
      ELSE
         RKAXN=1.0/(RKAMAX-RKAMIN)
      ENDIF
C
      DO 6100 I=1,NSETM
         COFOM(I)=W1*(ABSFOM(I)-ABMIN)*ABXN+
     *              W2*(PSIMAX-PSIZ(I))*PSIXN+
     *              W3*(RKAMAX-RKARL(I))*RKAXN
           PSIZ(I)=PSIZ(I)*ESCAL
6100      CONTINUE
C
        IF(JPATH.EQ.3.AND.IPATH.EQ.1) THEN
        ELSE
          WRITE(10,6102) NCLMAX
6102          FORMAT(1x,'SUMMRY OF FOMS WITH ',I3,' CYCLES REFINEMENT')
          WRITE(10,*)
          WRITE(10,6103)
        ENDIF
6103      FORMAT(1X,'NUMSET      ABSFOM     PSIZERO       ',
     *            'RESID       COFOM')
C
C      SELECT THE BEST SET
6110      JUMP=NSETM
6113      IF(JUMP.LE.1) GOTO 6155
      JUMP=JUMP/2
      IEND=NSETM-JUMP
6114      ALL=.TRUE.
      DO 6115 ITOP=1,IEND
         IBOT=ITOP+JUMP
         XX=COFOM(IBOT)
         YY=COFOM(ITOP)
         IF(XX.LE.YY) GOTO 6115
         COF=COFOM(ITOP)
         COFOM(ITOP)=COFOM(IBOT)
         COFOM(IBOT)=COF
         FMABF=ABSFOM(ITOP)
         ABSFOM(ITOP)=ABSFOM(IBOT)
         ABSFOM(IBOT)=FMABF
         PSCOF=PSIZ(ITOP)
         PSIZ(ITOP)=PSIZ(IBOT)
         PSIZ(IBOT)=PSCOF
         RKCOF=RKARL(ITOP)
         RKARL(ITOP)=RKARL(IBOT)
         RKARL(IBOT)=RKCOF
         NSRC=NSETRC(ITOP)
         NSETRC(ITOP)=NSETRC(IBOT)
         NSETRC(IBOT)=NSRC
         ALL=.FALSE.
6115      CONTINUE
      IF(.NOT.ALL) THEN
         GOTO 6114
      ELSE
         GOTO 6113
      ENDIF
         GOTO 6113
C
6155    IF(JPATH.EQ.3.AND.IPATH.EQ.1) THEN
        ELSE
      DO 6109 I=1,NSETM
         WRITE(10,6108) (NSETRC(I)+ISKIP),ABSFOM(I),PSIZ(I),
     *                    RKARL(I),COFOM(I)
6109      CONTINUE
        ENDIF
6108      FORMAT(1X,I6,F12.3,F12.3,F12.3,F12.3)
      RETURN
      END
C      *** SUB ***
      SUBROUTINE SEQUFOM(APR,NSETM)
      REAL APR(1024)
      LOGICAL ALL
6010      JUMP=NSETM
6013      IF(JUMP.LE.1) GOTO 6030
      JUMP=JUMP/2
      IEND=NSETM-JUMP
6014      ALL=.TRUE.
      DO 6015 ITOP=1,IEND
         IBOT=ITOP+JUMP
         XX=APR(ITOP)
         YY=APR(IBOT)
         IF(XX.LE.YY) GOTO 6015
         AB=APR(ITOP)
         APR(ITOP)=APR(IBOT)
         APR(IBOT)=AB
         ALL=.FALSE.
6015      CONTINUE
      IF(.NOT.ALL) THEN
         GOTO 6014
      ELSE
         GOTO 6013
      ENDIF
         GOTO 6013
6030    RETURN
        END
C      *** SUB ***
      SUBROUTINE SEQUSHIF
      INTEGER*2 ISHIFT(360)
      REAL PHFOM(360)
      LOGICAL ALL
      COMMON /CZ999/PHFOM,ISHIFT,NSHI
C
6010      JUMP=NSHI
6013      IF(JUMP.LE.1) GOTO 6030
      JUMP=JUMP/2
      IEND=NSHI-JUMP
6014      ALL=.TRUE.
      DO 6015 ITOP=1,IEND
         IBOT=ITOP+JUMP
         XX=PHFOM(ITOP)
         YY=PHFOM(IBOT)
         IF(XX.LE.YY) GOTO 6015
         AB=PHFOM(ITOP)
         PHFOM(ITOP)=PHFOM(IBOT)
         PHFOM(IBOT)=AB
         IHH0=ISHIFT(ITOP)
         ISHIFT(ITOP)=ISHIFT(IBOT)
         ISHIFT(IBOT)=IHH0
         ALL=.FALSE.
6015      CONTINUE
      IF(.NOT.ALL) THEN
         GOTO 6014
      ELSE
         GOTO 6013
      ENDIF
         GOTO 6013
6030    RETURN
        END
C     ****** SUB. FOR CALCULATING MATRIX ****** 
      SUBROUTINE MATRIX(NG)
      DIMENSION TSS(4)
      INTEGER*2 MX(4,4,48),JSSS(4,4),NNNSYM(2000)
      COMMON /CZ16/MX,NSYM,NNNSYM,TRANL(4,48),ICENT
      NU=0
      K=0
 1420 K=K+1
      IF (K.GT.NG) GO TO 1560
      L=1
      IF (MX(1,1,K)+MX(2,2,K)+MX(3,3,K)+MX(4,4,K).EQ.4) NU=K
 1430 IF (L.GT.NG) GO TO 1420
      DO 1450 I=1,4
       TN=TRANL(I,K)
      DO 1450 J=1,4
       N=0
       TN=TN+MX(I,J,K)*TRANL(J,L)
      DO 1440 M=1,4
 1440  N=N+MX(I,M,K)*MX(M,J,L)
       TN=TN-INT(TN)
       IF(TN.LT.0.0) TN=TN+1.
       JSSS(I,J)=N
 1450 TSS(I)=TN
      DO 1470 KK=1,NG
       N=0
      DO 1460 I=1,4
      DO 1460 J=1,4
 1460 IF (JSSS(I,J).NE.MX(I,J,KK)) N=1
      IF (N.EQ.0) GO TO 1490
 1470 CONTINUE
      NG=NG+1
      DO 1480 I=1,4
       TRANL(I,NG)=TSS(I)
      DO 1480 J=1,4
 1480  MX(I,J,NG)=JSSS(I,J)
 1490 L=L+1
 1550 GO TO 1430
C
 1560 IF(NU.EQ.1) GOTO 1568
      DO I=NU,2,-1
        II=I-1
        DO J=1,4
          TRANL(J,I)=TRANL(J,II)
          DO K=1,4
            MX(J,K,I)=MX(J,K,II)
          END DO
        END DO
      END DO
      DO I=1,4
        TRANL(I,1)=0.0
        DO J=1,4
          IF(I.EQ.J) THEN
            MX(I,J,1)=1
          ELSE
            MX(I,J,1)=0
          ENDIF
        END DO
      END DO
1568  NSYM=NG
      RETURN
      END
C
C      *** SUB ***
      SUBROUTINE NORMAL(X)
100      CONTINUE
      IF(X.GE.1.) THEN
        X=X-1.
      ELSE IF(X.LT.0.) THEN
        X=X+1.
      ELSE
        RETURN
      END IF
      GOTO 100
      END
C      ****** SUB-ERR1 ******
      SUBROUTINE ERR1(ER)
      WRITE(*,*)
      IF(ER.EQ.0.0) THEN
      WRITE(*,*) '#Diffraction data does not ended with a negative Fobs'
      STOP
      ELSE
      ENDIF
      RETURN
      END
C      *** SUB ***
      SUBROUTINE ERRMFS(MFS0,MFS)
      CHARACTER*1 YN,YN1,YN2,YN3,YN4
      YN1='Y'
      YN2='y'
      YN3='N'
      YN4='n'
        WRITE(*,*)
      WRITE(*,8888) MFS0
      WRITE(*,*) '#Your indicated ORDER larger than ',
     *             'MAXORDER.'
        WRITE(*,*) ' The program will go on with',
     *             ' ORDER=MAXORDER.'
8889    WRITE(*,8887)
        READ(*,'(A)') YN
C
        IF(YN.EQ.YN1.OR.YN.EQ.YN2) THEN
           MFS=MFS0
      WRITE(*,55) '$ ... Please wait ... '
55      FORMAT(A22)
           RETURN
      ELSEIF(YN.EQ.YN3.OR.YN.EQ.YN4) THEN
         WRITE(*,8886)
         READ(*,*) MFS
      WRITE(*,55) '$ ... Please wait ... '
        ELSE
           WRITE(*,*) ' Not correct answer, try again!'
           GOTO 8889
        ENDIF
8886    FORMAT('$ Please give your new ORDER: ')
8887      FORMAT('$ Do you agree (Y/N)? ')
8888      FORMAT(1X,'#MAXORDER (max-order in input data): ',I2)
      RETURN
      END
C
C      *** SUB ***
      SUBROUTINE ERRPATH4(MFS)
      CHARACTER*1 YN,YN1,YN2,YN3,YN4
      YN1='Y'
      YN2='y'
      YN3='N'
      YN4='n'
      WRITE(*,8888) MFS
      WRITE(*,*) '#Your indicated ORDER is not 128, 129 or 130,'
        WRITE(*,*) ' which is needed by following PATH=4. The program'
8889    WRITE(*,8887)
        WRITE(*,*)
        READ(*,'(A)') YN
        IF(YN.EQ.YN1.OR.YN.EQ.YN2) THEN
           MFS=128
      WRITE(*,55) '$ ... Please wait ... '
55      FORMAT(A22)
           RETURN
      ELSEIF(YN.EQ.YN3.OR.YN.EQ.YN4) THEN
         WRITE(*,8886)
         READ(*,*) MFS
      WRITE(*,55) '$ ... Please wait ... '
        ELSE
           WRITE(*,*) ' Not correct answer, try again!'
           GOTO 8889
        ENDIF
8886    FORMAT('$ Please give your new ORDER: ')
8887      FORMAT('$  will go on with ORDER=128.  Do you agree (Y/N)? ')
8888      FORMAT(1X,' ORDER: ',I2)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                            SPGR4D    .1994                            C
C   PROGRAM FOR DERIVING SYMMETRY OPERATIONS OF 4D SUPERSPACE GROUPS         C
C                 FROM THE CORRESPONDING TWO-LINE SYMBOL                C
C                                                                       C
C                        By Z.Q. FU and H.F. Fan.                       C
C                                                                       C
C                THE PROGRAM BY H. BURZLAFF & A. HOUNTAS                C
C          [J.APPL.CRYST.15 (1982) 464-467] IS ADOPTED FOR THE          C
C             DERIVATION OF THE CORRESPONDING 3D SPACE GROUP            C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE SPGR4D(NCHLD,ROT,TSL,TCL,NG,NLC,JERR,ICENT,NUMB,CH)
      DIMENSION NCH(80),NCHM(80),NCHTOT(80),NCHLD(80),NNSV(3,18)
      INTEGER SYS,NT(16),GES(3,3,21),NET(3),IN(13),NES(3),GET(3,19),
     *    ROT(48,4,4),TSL(48,4),SSS(3,3),TSS(3),TSA(3,3),H(36),HC(36),
     *    NSV(3,18),NV(12),NSW(3),NTEX(42),GGES(3,3,21),NTM(16),
     *    INM(13),QRAT(3),MEMDEV(48),TAO(3),TCL(4,4)
      REAL TX(6,3),TEXB(9)
      CHARACTER*36 CH
      COMMON /CZ26/H,HC
      EQUIVALENCE     (NBR,IN(1)),(I1,IN(2)),(I2,IN(3)),(I3,IN(4)),
     *      (I4,IN(5)),(J1,IN(6)),(J2,IN(7)),(J3,IN(8)),(J4,IN(9)),
     * (K1,IN(10)),(K2,IN(11)),(K3,IN(12)),(K4,IN(13)),(M1,NES(1)),
     * (M2,NES(2)),(N1,NET(1)),(N2,NET(2)),(IT1,INM(1)),(IT2,INM(2)),
     * (IT3,INM(3)),(JT1,INM(4)),(JT2,INM(5)),(JT3,INM(6)),
     * (KT1,INM(7)),(KT2,INM(8)),(KT3,INM(9)),(LT1,INM(10)),
     * (LT2,INM(11)),(LT3,INM(12))
      DATA GES/ 1,3*0,1,3*0,1,-1,3*0,-1,3*0,1,0,1,0,2*-1,3*0,1,0,1,0,-1,
     *4*0,1,0,1,3*0,2*1,2*0,2*1,0,-1,4*0,1,-1,3*0,-1,3*0,-1,1,3*0,1,3*0,
     * -1,0,-1,0,2*1,3*0,-1,0,-1,0,1,4*0,2*-1,3*0,1,3*0,3*-1,0,1,4*0,-1,
     *1,3*0,-1,3*0,1,0,1,0,1,4*0,-1,0,-1,0,-1,4*0,2*1,2*0,2*-1,3*0,2*-1,
     *2*0,2*1,3*0,1,-1,3*0,1,3*0,1,0,1,0,1,4*0,2*1,3*0,-1,3*0,-1,0,-1,0,
     *-1,4*0,-1/
      DATA NTEX/2H  ,2H  ,4*0,2H+1,2H/8,2H+1,2H/6,2*0,2H+1,2H/4,
     *           2*0,2H+1,2H/3,2H+3,2H/8,4*0,2H+1,2H/2,6*0,2H+2,
     *               2H/3,2*0,2H+3,2H/4,2*0,2H+5,2H/6/
      DATA TEXB/4H  -Z,4H  -Y,4H X-Y,4H  -X,0.0,
     *          4H   X,4H Y-X,4H   Y,4H   Z/
      DATA GET/3*0,12,3*0,12,3*0,12,0,3*12,0,3*12,0,
     *         3*12,0,3*6,0,3*6,0,3*6,18,2*6,24,3*0,24,3*0,24,2*8,0,
     *         16,3*8,2*16/
      DATA H/1H ,1H-,1H/,1H0,1H1,1H2,1H3,1H4,1H5,1H6,1HA,1HB,1HC,1HI,1HR
     * ,1HF,1HP,1HD,1HM,1HN,1HY,1H:,1HE,1HU,1HO,1HG,1HQ,1HT,1HL,1HH,1HX,
     *  1HS,1HV,1HW,1H[,1H]/
      DATA HC/32,45,47,48,49,50,51,52,53,54,65,66,67,73,82,
     *  70,80,68,77,78,89,58,69,85,79,71,81,84,76,72,88,83,86,87,91,93/
      DATA NSV/6,3*0,6,3*0,6,2*0,4,2*0,8,2*9,0,5*6,0,6,18,0,6,18,
     *         3,6,18,2*6,18,9,12,6,9,0,12,0,0,18,4*0,18,4*6,18/,
     *         NV/12*1H0/
C
 88   IERR=0
      JERR=0
      KEPS=0
      ICENT=0
      KS=0
      JN=2
      JM=0
      JP=0
      NSQ=0
      NN=0
      INTJ=0
      INVJ=0
      IIS=1
      SYS=2
      IR=-1
      NG=0
      NS=1
      JS=1
      JR=-1
      KL=0
      NSS=-1
      MS=16
      ILIN=1
      DO 900 I=1,16
      NT(I)=0
      NTM(I)=0
      IF(I.GT.13) GOTO 900
      IN(I)=0
      INM(I)=0
 900  CONTINUE
      DO 910 I=1,80
      NCHTOT(I)=H(1)
      NCH(I)=H(1)
 910  NCHM(I)=H(1)
      DO 1020 I=1,3
      NES(I)=1
      NSW(I)=0
      DO 1020 J=1,3
      TSA(I,J)=0
      DO 1010 K=1,48
      TSL(K,J)=0
      IF(K.GT.18) GOTO 1000
      NNSV(J,K)=NSV(J,K)
 1000 IF(K.GT.21) GOTO 1010
      GGES(I,J,K)=GES(I,J,K)
 1010 CONTINUE
 1020 CONTINUE
      DO 1025 I=1,4
      DO 1025 J=1,4
 1025 TCL(I,J)=0
      J=0
      DO 1118 I=1,79
      IF(NCHLD(I).EQ.H(1).AND.NCHLD(I+1).EQ.H(1)) GOTO 1118
      J=J+1
      NCHTOT(J)=NCHLD(I)
 1118 CONTINUE
      NLC=1
      J0=0
      L0=0
      J=0
      IUN=0
      L=81
      DO 1021 I=1,80
       IF(NCHTOT(I).NE.H(1)) J=J+1
       IF(J.EQ.1.AND.NCHTOT(I).NE.H(1)) K=I
       IF(NCHTOT(I).EQ.H(35)) J0=I
       IF(NCHTOT(I).EQ.H(36)) L0=I
       IF(NCHTOT(I).EQ.H(22).AND.NCHTOT(I+1).EQ.H(11)) THEN
       IUN=1
       L=I
       ENDIF
       IF(NCHTOT(I).EQ.H(22).AND.NCHTOT(I+1).EQ.H(12)) THEN
        IUN=2
        L=I
       ENDIF
 1021 CONTINUE
      IF(J.EQ.0) THEN
       NUMB=NUMB-1
       GOTO 2400
      ENDIF
      IF(J0.EQ.0.AND.L0.EQ.0) THEN
       ISPGCM=0
       DO 1022 I=K,L-1
 1022  NCH(I-K+1)=NCHTOT(I)
      ELSEIF(J0.LT.L0.AND.J0.GT.0) THEN
       ISPGCM=1
       DO 1023 I=J0+1,L0-1
 1023  NCH(I-J0)=NCHTOT(I)
       MBR=NCHTOT(K)
       J=0
       DO 1024 I=L0+1,L-1
       IF(I.EQ.(L0+1).AND.NCHTOT(I).EQ.H(1)) GOTO 1024
       J=J+1
       NCHM(J)=NCHTOT(I)
 1024  CONTINUE
      ELSE
       JERR=1
       GOTO 1308
      ENDIF
C     TRY TO FIND OUT IF THERE IS THE SIGN '-3' AT SECOND POSITION
C     ON THE RECORD OF THE SPACE GROUP'S INPUT
 1030 KS=MAX0(1,MOD(KS+1,81))
      IF(NCH(KS).EQ.H(1)) GOTO 1030
      NCHH=0
      ICT=0
      ISP=0 
      DO 1040 I=KS,80
      IF (NCH(I).EQ.H(1)) GO TO 1040
      IF (ISP.EQ.0) GO TO 1040
      ICT=ICT+1
      ISP=0
      IF (ICT.NE.2) GO TO 1040
      IF (NCH(I).NE.H(2)) GO TO 1045
      IF (NCH(I+1).NE.H(7)) GO TO 1045
      NCHH=I
      GO TO 1045
 1040 ISP=1
C     SEPREATE TWO PARTS OF CONTROL CARD
 1045 J0=0
      DO 1080 I=KS,80
      IF (I.EQ.NCHH) GO TO 1080
      IF (J0.EQ.2) GO TO 1070
      IF (NCH(I).EQ.H(1)) GO TO 1050
      J0=0
      GO TO 1060
 1050 J0=J0+1
 1060 INTJ=INTJ+1
      NT(INTJ)=NCH(I)
      IF (INTJ.EQ.16) IERR=1
      IF(INTJ.EQ.16) GOTO 2200
      GO TO 1080
 1070 IF (INVJ.GT.12.OR.NCH(I).EQ.H(1).AND.IIS.EQ.1) GO TO 1080
      IIS=2
      INVJ=INVJ+1
      NV(INVJ)=NCH(I)
 1080 CONTINUE
C     BEGIN DERIVING
      DO 1100 I=1,16
      IF (JS.LT.14) IN(JS)=NT(I)
      IF (NT(I).NE.H(1)) GO TO 1090
      JR=JR+1
      JS=4*JR+1
 1090 JS=JS+1
 1100 IF (NT(I).EQ.H(7).OR.NT(I).EQ.H(10)) SYS=5
      IF (I1.EQ.H(8).OR.I1.EQ.H(2).AND.I2.EQ.H(8)) SYS=4
      IF (J1.EQ.H(7)) SYS=6
      IF (SYS.EQ.2.AND.NH(I1).GE.HC(6).AND.NH(J1).GE.HC(6)) SYS=3
      IF (I1.EQ.H(5).AND.NH(J1).LE.HC(4).OR.I1.EQ.H(2).AND.
     *                      I2.EQ.H(5)) SYS=1
      KSYS=SYS+MAX0(0,SYS-4)
      IF (I1.EQ.H(7).OR.I1.EQ.H(2).AND.I2.EQ.H(7)) KSYS=5
      DO 1200 I=1,7
      LO=I+10
 1200 IF (NBR.EQ.H(LO)) NBR=I
      LATT=MOD(NBR,7)+1
      IF (LATT.LE.5) PTS=MIN0(2,LATT)
      IF (LATT.GE.6) PTS=LATT-3
      IF (NBR.EQ.6) LATT=6
      IF (NBR.EQ.5) LATT=7
C     ABSTRACT CONTRAL CARDS FROM THE BOTTOM LINE
      IF(ISPGCM.EQ.0) GOTO 1301
      JS=1
      INTJ=0
      J=0
      JR=0
      DO 1032 I=1,3
 1032 TAO(I)=0
 1031 J=J+1
      IF(NCHM(J).EQ.H(1)) GOTO 1031
      J0=0
      DO 1081 I=J,80
       IF(NCHM(I).EQ.H(1)) GOTO 1051
       J0=0
       GOTO 1061
 1051  J0=J0+1
       IF(J0.GE.2) GOTO 1081
 1061  INTJ=INTJ+1
       NTM(INTJ)=NCHM(I)
       IF(INTJ.GT.16) THEN
        JERR=2
        GOTO 1308
       ENDIF
 1081 CONTINUE
      DO 1101 I=1,16
       IF(JS.LT.14) INM(JS)=NTM(I)
       IF(NTM(I).NE.H(1)) GOTO 1091
       JR=JR+1
       JS=3*JR
 1091  JS=JS+1
 1101 CONTINUE
      J0=0
      IF(MBR.EQ.H(17)) J0=1
      IF(MBR.EQ.H(11)) J0=2
      IF(MBR.EQ.H(12)) J0=3
      IF(MBR.EQ.H(13)) J0=4
      IF(MBR.EQ.H(29)) J0=14
      IF(MBR.EQ.H(19)) J0=15
      IF(MBR.EQ.H(20)) J0=16
      IF(MBR.EQ.H(24)) J0=5
      IF(MBR.EQ.H(33)) J0=6
      IF(MBR.EQ.H(34)) J0=7
      IF(MBR.EQ.H(15)) J0=17
      IF(J0.EQ.0) THEN
        JERR=5
        GOTO 1308
      ELSE
      DO 1102 I=1,3
 1102 QRAT(I)=GET(I,J0)
      ENDIF
C     SELECTION OF GENERATORS FOR THE 4D
      IF(KSYS.GT.2) GOTO 1302
      IF(KSYS.EQ.1) THEN
       IF(I1.EQ.H(5)) THEN
        IF(IT1.EQ.H(5)) THEN
        ELSE
         JERR=4
        ENDIF
       ELSEIF(I1.EQ.H(2)) THEN
        IF(IT1.EQ.H(2).AND.IT2.EQ.H(5)) THEN
        ELSE
         JERR=4
        ENDIF
       ELSE
       JERR=3
      ENDIF
      GOTO 1308
      ELSEIF(KSYS.EQ.2) THEN
       IF(I1.EQ.H(6).AND.NH(I3).LE.HC(1)) THEN
        IF(IT1.EQ.H(5)) THEN
        ELSEIF(IT1.EQ.H(2).AND.IT2.EQ.H(5)) THEN
         KEPS=1
        ELSEIF(IT1.EQ.H(32)) THEN
         TAO(1)=12
        ELSE
         JERR=4
        ENDIF
       ELSEIF(NH(I1).GE.HC(6).AND.NH(I3).LE.HC(1)) THEN
        IF(IT1.EQ.H(5)) THEN
        KEPS=1
        ELSEIF(IT1.EQ.H(2).AND.IT2.EQ.H(5)) THEN
        ELSEIF(IT1.EQ.H(32)) THEN
         KEPS=1
         TAO(1)=12
        ELSE
         JERR=4
        ENDIF
       ELSEIF(I1.EQ.H(6).AND.NH(I3).GE.HC(3)) THEN
        IF(JT1.EQ.H(5)) THEN
         KEPS=1
        ELSEIF(JT1.EQ.H(32)) THEN
         KEPS=1
         TAO(1)=12
         TAO(2)=12
        ELSEIF(IT1.EQ.H(5)) THEN
        ELSEIF(IT1.EQ.H(32)) THEN
         TAO(1)=12
         TAO(2)=12
        ELSE
         JERR=4
        ENDIF
       ELSE
        JERR=3
       ENDIF
      ENDIF
      GOTO 1308
 1302 IF(KSYS.GT.3) GOTO 1303
      IF(IUN-1) 1310,1311,1312
 1310 IF(I1.EQ.H(6).AND.J1.EQ.H(6)) THEN
       IF(KT1.EQ.H(5)) THEN
       ELSEIF(KT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(J1).GT.HC(10).AND.K1.EQ.H(6)) THEN
       IF(IT1.EQ.H(5).AND.JT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(32).AND.JT1.EQ.H(5)) THEN
        TAO(1)=12
       ELSEIF(IT1.EQ.H(32).AND.JT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(IT1.EQ.H(5).AND.JT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSEIF(IT1.EQ.H(27).AND.JT1.EQ.H(27)) THEN
        TAO(1)=6
        TAO(2)=6
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(I1).GT.HC(10).AND.J1.EQ.H(6)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(6).AND.NH(J1).GT.HC(10)) THEN
       IF(JT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(JT1.EQ.H(27)) THEN
        TAO(1)=6
        TAO(2)=6
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(I1).GT.HC(10).AND.NH(J1).GT.HC(10).AND.
     *      NH(K1).GT.HC(10)) THEN
       IF(IT1.EQ.H(5).AND.JT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(32).AND.JT1.EQ.H(5)) THEN
        TAO(1)=12
        TAO(3)=12
       ELSEIF(IT1.EQ.H(32).AND.JT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(IT1.EQ.H(5).AND.JT1.EQ.H(32)) THEN
        TAO(2)=12
        TAO(3)=12
       ELSEIF(IT1.EQ.H(27).AND.JT1.EQ.H(27)) THEN
        TAO(1)=6
        TAO(2)=6
       ELSE
        JERR=4
       ENDIF
      ELSE
       JERR=3
      ENDIF
      GOTO 1308
 1311 IF(I1.EQ.H(6).AND.J1.EQ.H(6)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(6).AND.NH(J1).GT.HC(10)) THEN
       IF(JT1.EQ.H(5).AND.KT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32).AND.KT1.EQ.H(5)) THEN
        TAO(1)=12
       ELSEIF(JT1.EQ.H(32).AND.KT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(JT1.EQ.H(5).AND.KT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSEIF(JT1.EQ.H(27).AND.KT1.EQ.H(27)) THEN
        TAO(1)=6
        TAO(2)=6
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(I1).GT.HC(10).AND.K1.EQ.H(6)) THEN
       IF(JT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(I1).GT.HC(10).AND.J1.EQ.H(6)) THEN
       IF(KT1.EQ.H(5)) THEN
       ELSEIF(KT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(KT1.EQ.H(27)) THEN
        TAO(1)=6
        TAO(2)=6
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(I1).GT.HC(10).AND.NH(J1).GT.HC(10).AND.
     * NH(K1).GT.HC(10)) THEN
       IF(JT1.EQ.H(5).AND.KT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32).AND.KT1.EQ.H(5)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(JT1.EQ.H(32).AND.KT1.EQ.H(32)) THEN
        TAO(2)=12
        TAO(3)=12
       ELSEIF(JT1.EQ.H(5).AND.KT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(3)=12
       ELSEIF(JT1.EQ.H(27).AND.KT1.EQ.H(27)) THEN
        TAO(2)=6
        TAO(3)=6
       ELSE
        JERR=4
       ENDIF
      ELSE
       JERR=3
      ENDIF
      GOTO 1308
 1312 IF(I1.EQ.H(6).AND.J1.EQ.H(6)) THEN
       IF(JT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(I1).GT.HC(10).AND.J1.EQ.H(6)) THEN
       IF(IT1.EQ.H(5).AND.KT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(32).AND.KT1.EQ.H(5)) THEN
        TAO(1)=12
       ELSEIF(IT1.EQ.H(32).AND.KT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(IT1.EQ.H(5).AND.KT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSEIF(IT1.EQ.H(27).AND.KT1.EQ.H(27)) THEN
        TAO(1)=6
        TAO(2)=6
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(6).AND.NH(J1).GT.HC(10)) THEN
       IF(KT1.EQ.H(5)) THEN
       ELSEIF(KT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(I1).GT.HC(10).AND.K1.EQ.H(6)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(IT1.EQ.H(27)) THEN
        TAO(1)=6
        TAO(2)=6
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(I1).GT.HC(10).AND.NH(J1).GT.HC(10).AND.
     * NH(K1).GT.HC(10)) THEN
       IF(IT1.EQ.H(5).AND.KT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(32).AND.KT1.EQ.H(5)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(IT1.EQ.H(32).AND.KT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(3)=12
       ELSEIF(IT1.EQ.H(5).AND.KT1.EQ.H(32)) THEN
        TAO(2)=12
        TAO(3)=12
       ELSEIF(IT1.EQ.H(27).AND.KT1.EQ.H(27)) THEN
        TAO(1)=6
        TAO(3)=6
       ELSE
        JERR=4
       ENDIF
      ELSE
       JERR=3
      ENDIF
      GOTO 1308
 1303 IF(KSYS.GT.4) GOTO 1304
      IF(I1.EQ.H(8).AND.NH(I3).LE.HC(1).AND.NH(J1).LE.HC(1)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(27)) THEN
        TAO(1)=6
       ELSEIF(IT1.EQ.H(32)) THEN
        TAO(1)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(2).AND.NH(J1).LE.HC(1)) THEN
       IF(IT1.EQ.H(2)) THEN
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(I3).GE.HC(3).AND.NH(J1).LE.HC(1)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(32)) THEN
        TAO(1)=12
       ELSEIF(IT1.EQ.H(27)) THEN
        TAO(1)=6
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(8).AND.J1.EQ.H(6)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(27)) THEN
        TAO(2)=6
       ELSEIF(IT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(8).AND.NH(I3).LE.HC(1).AND.NH(J1).GT.HC(10)) THEN
       IF(JT1.EQ.H(5).AND.KT1.EQ.H(5)) THEN 
       ELSEIF(JT1.EQ.H(32).AND.KT1.EQ.H(5)) THEN 
        TAO(1)=12
       ELSEIF(JT1.EQ.H(32).AND.KT1.EQ.H(32)) THEN 
        TAO(1)=12
        TAO(2)=12
       ELSEIF(JT1.EQ.H(5).AND.KT1.EQ.H(32)) THEN 
        TAO(2)=12
       ELSEIF(JT1.EQ.H(27).AND.KT1.EQ.H(5)) THEN 
        TAO(1)=6
       ELSEIF(JT1.EQ.H(27).AND.KT1.EQ.H(32)) THEN 
        TAO(1)=6
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(2).AND.J1.EQ.H(6)) THEN
       IF(KT1.EQ.H(5)) THEN
       ELSEIF(KT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(2).AND.NH(J1).GT.HC(10)) THEN
       IF(JT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(27)) THEN
        TAO(1)=6
       ELSEIF(JT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(8).AND.NH(I3).GE.HC(3).AND.NH(J1).GT.HC(10)) THEN
       IF(KT1.EQ.H(5).AND.LT1.EQ.H(5)) THEN
       ELSEIF(KT1.EQ.H(32).AND.LT1.EQ.H(5)) THEN
        TAO(1)=12
       ELSEIF(KT1.EQ.H(32).AND.LT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(KT1.EQ.H(5).AND.LT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSEIF(KT1.EQ.H(27).AND.LT1.EQ.H(5)) THEN
        TAO(1)=6
       ELSEIF(KT1.EQ.H(27).AND.LT1.EQ.H(32)) THEN
        TAO(1)=6
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSE
       JERR=3
      ENDIF
      GOTO 1308
 1304 IF(KSYS.GT.5) GOTO 1305
      IF(I1.EQ.H(7).AND.NH(J1).LE.HC(1)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(28)) THEN
        TAO(1)=8
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(2).AND.NH(J1).LE.HC(1)) THEN
       IF(IT1.EQ.H(2)) THEN
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(7).AND.J1.EQ.H(6).AND.NH(K1).LE.HC(1)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(28)) THEN
        TAO(1)=8
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(7).AND.NH(J1).GT.HC(10).AND.NH(K1).LE.HC(1)) THEN
       IF(JT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(2).AND.NH(J1).GT.HC(10).AND.NH(K1).LE.HC(1)) THEN
       IF(JT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(7).AND.J1.EQ.H(5).AND.K1.EQ.H(6)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(28)) THEN
        TAO(1)=8
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(7).AND.J1.EQ.H(6).AND.K1.EQ.H(5)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(28)) THEN
        TAO(1)=8
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(7).AND.NH(J1).GT.HC(10).AND.K1.EQ.H(5)) THEN
       IF(JT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(7).AND.J1.EQ.H(5).AND.NH(K1).GT.HC(10)) THEN
       IF(KT1.EQ.H(5)) THEN
       ELSEIF(KT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(2).AND.J1.EQ.H(5)) THEN
       IF(KT1.EQ.H(5)) THEN
       ELSEIF(KT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(2).AND.K1.EQ.H(5)) THEN
       IF(JT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSE
       JERR=3
      ENDIF
      GOTO 1308
 1305 IF(KSYS.GT.6) THEN
       JERR=4
      GOTO 1308
      ENDIF
      IF(I1.EQ.H(10).AND.NH(I3).LE.HC(1).AND.NH(J1).LE.HC(1)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(30)) THEN
        TAO(1)=4
       ELSEIF(IT1.EQ.H(28)) THEN
        TAO(1)=8
       ELSEIF(IT1.EQ.H(32)) THEN
        TAO(1)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(2).AND.NH(J1).LE.HC(1)) THEN
      ELSEIF(I1.EQ.H(10).AND.NH(I3).GE.HC(3).AND.NH(J1).LE.HC(1)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(32)) THEN
        TAO(1)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(10).AND.J1.EQ.H(6)) THEN
       IF(IT1.EQ.H(5)) THEN
       ELSEIF(IT1.EQ.H(30)) THEN
        TAO(2)=4
       ELSEIF(IT1.EQ.H(28)) THEN
        TAO(2)=8
       ELSEIF(IT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(10).AND.NH(I3).LE.HC(1).AND.NH(J1).GT.HC(10)) THEN
       IF(JT1.EQ.H(5).AND.KT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32).AND.KT1.EQ.H(5)) THEN
        TAO(1)=12
       ELSEIF(JT1.EQ.H(32).AND.KT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(JT1.EQ.H(5).AND.KT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(2).AND.K1.EQ.H(6)) THEN
       IF(JT1.EQ.H(5)) THEN
       ELSEIF(JT1.EQ.H(32)) THEN
        TAO(1)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(I1.EQ.H(2).AND.J1.EQ.H(6)) THEN
       IF(KT1.EQ.H(5)) THEN
       ELSEIF(KT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSEIF(NH(I3).GE.HC(3).AND.NH(J1).GT.HC(10)) THEN
       IF(KT1.EQ.H(5).AND.LT1.EQ.H(5)) THEN
       ELSEIF(KT1.EQ.H(32).AND.LT1.EQ.H(5)) THEN
        TAO(1)=12
       ELSEIF(KT1.EQ.H(32).AND.LT1.EQ.H(32)) THEN
        TAO(1)=12
        TAO(2)=12
       ELSEIF(KT1.EQ.H(5).AND.LT1.EQ.H(32)) THEN
        TAO(2)=12
       ELSE
        JERR=4
       ENDIF
      ELSE
       JERR=3
      ENDIF
 1308 IF(JERR.GE.1) CALL ERRSPG(NCHTOT,NCHLD,JERR,*88,*2400)
C
C     DETERMINATION OF MONOCLINIC SETTING.
 1301 IF (SYS.NE.2.OR.NH(J1).GT.HC(4)) GO TO 1320
      IF(IUN.EQ.0) J0=8
      IF(IUN.EQ.1) J0=0
      IF(IUN.EQ.2) J0=4
      DO 1300 I=2,5
      LO=I+J0
 1300 IN(LO)=IN(I)
      DO 1309 I=2,J0+1
 1309  IN(I)=0
C      SELECTION OF GENERATORS FOR THE 3D 
 1320 IF (SYS.GE.2.AND.SYS.LE.3) GO TO 1330
C      1. POINT GROUPS 1,-1,3,-3,4,-4,6,-6,4/M,6/M.
      N1=NH(I1)-HC(4)
      KL=24*(NH(I2)-HC(4))/N1
      IF (I1.EQ.H(2)) N1=NH(I2)-HC(4)+6
      IF (N1.GT.2.AND.N1.LT.7.AND.NH(I2).GT.HC(4)) TSA(1,3)=KL
      IF (NH(J1).LE.HC(4).OR.I1.EQ.H(7).OR.I1.EQ.H(2).AND.I2.EQ.H(7))
     *                     NG=1
      IF (NH(J1).GT.HC(4)) GO TO 1330
      IF (NH(I3).LT.HC(3)) GO TO 1400
      N2=8
      IF (I4.EQ.H(11).OR.I3.EQ.H(11)) M2=2
      IF (I4.EQ.H(20).OR.I3.EQ.H(20)) M2=7
      NG=2
      IF (NH(J1).LE.HC(4)) GO TO 1400
C      2. MONOCLINIC-ORTHORHOMBIC.
 1330 DO 1390 I=1,3
      DO 1390 J=1,4
      L=1+4*(I-1)+J
      M=L+4
      IF (M.GT.13) M=M-12
      IF (SYS.GT.3) GO TO 1350
      IF (IN(L).NE.H(6).OR.NH(IN(M)).GE.HC(11)) GO TO 1340
      NG=NG+1
      NET(NG)=29-9*I
      LO=L+1
      IF (IN(LO).EQ.H(5)) NES(NG)=I+1
      IF (NG.EQ.2.AND.K2.EQ.H(5)) TSA(2,3)=12
      IF (NG.EQ.2) GO TO 1400
 1340 IF (NH(IN(L)).LE.HC(10)) GO TO 1390
      NG=NG+1
      NET(NG)=23-5*I
      IF (NH(IN(L)).GE.HC(11).AND.NH(IN(L)).LE.HC(13))
     *         NES(NG)=NH(IN(L))-HC(11)+2
      IF (IN(L).EQ.H(20)) NES(NG)=I+4
      IF (IN(L).EQ.H(18)) NES(NG)=I+8
      IF (SYS.LT.4) GO TO 1390
C      3. TETRAGONAL-HEXAGONAL-CUBIC.
 1350 IF (NH(IN(M)).LE.HC(5)) GO TO 1390
      IF (I.EQ.3.AND.(SYS.LT.6.AND.NH(I3).LT.HC(3).OR.SYS.EQ.6.AND.
     *       NH(I1).LE.HC(10).AND.NH(K1).GT.HC(4))) GO TO 1390
      NG=NG+1
      IF (NG.EQ.4) NG=3
      IF (I.NE.1) GO TO 1360
      IF (SYS.EQ.6) N1=5
      IF (J1.EQ.H(6)) NET(NG)=36-4*SYS
      IF (NH(J1).GE.HC(11)) NET(NG)=22-SYS
      IF (J2.EQ.H(5)) M1=2
 1360 IF (I.NE.2) GO TO 1370
      IF (K1.EQ.H(6)) NET(NG)=21
      IF (NH(K1).GE.HC(11)) NET(NG)=19
      IF (N1.EQ.5.AND.K1.EQ.H(6)) N2=14
      IF (I1.EQ.H(2).AND.N1.EQ.5) N2=15
 1370 IF (NH(IN(M)).GE.HC(11).AND.I.EQ.3) NET(NG)=8
      IF (IN(M).EQ.H(6).AND.I.EQ.3) NET(NG)=2
      IF (I1.EQ.H(6).AND.I2.EQ.H(5)) M2=6
      IF (J1.EQ.H(6).AND.K1.EQ.H(6).AND.I.EQ.1.AND.NH(I2).GT.HC(4))
     *               TSA(2,3)=24-TSA(1,3)
      IF (I1.NE.H(7)) TSA(1,3)=0
      DO 1380 II=1,3
      IF (N2.EQ.14.AND.NH(I2).GT.HC(4)) TSA(2,II)=48-3*KL+(2*KL-24)*II
 1380 IF (N2.EQ.14.AND.NH(I2).GT.HC(4)) TSA(2,3)=TSA(2,2)
      IF (NH(IN(M)).GE.HC(11).AND.NH(IN(M)).LE.HC(13))
     *             NES(NG)=NH(IN(M))-HC(11)+2
      IF (IN(M).EQ.H(20)) NES(NG)=4+I
      IF (IN(M).EQ.H(20).AND.I.EQ.2) NES(NG)=8
      IF (IN(M).EQ.H(18)) NES(NG)=14-I
      IF (K1.EQ.H(18).AND.N1.EQ.5) M2=13
      IF (M2.EQ.13.AND.I1.EQ.H(11)) M2=12
 1390 CONTINUE
 1400 NGENER=NG
C     COMPLETE SYMMETRY OPERATIONS.
      DO 1410 K=1,NG
      NESK=NES(K)
      NETK=NET(K)
      DO 1410 I=1,3
      TSL(K,I)=GET(I,NESK)+TSA(K,I)
      DO 1410 J=1,3
 1410 ROT(K,I,J)=GES(I,J,NETK)
      K=0
 1420 K=K+1
      IF (K.GT.NG) GO TO 1560
      L=1
      IF (ROT(K,1,1)+ROT(K,2,2)+ROT(K,3,3).EQ.-3) NSS=K
      IF (ROT(K,1,1)+ROT(K,2,2)+ROT(K,3,3).EQ.3) NU=K
 1430 IF (L.GT.NG) GO TO 1420
      DO 1450 I=1,3
      ITN=TSL(K,I)
      DO 1450 J=1,3
      N=0
      ITN=ITN+ROT(K,I,J)*TSL(L,J)
      DO 1440 M=1,3
 1440 N=N+ROT(K,I,M)*ROT(L,M,J)
      ITN=MOD(ITN,24)
      IF (ITN.LT.0) ITN=ITN+24
      SSS(I,J)=N
 1450 TSS(I)=ITN
      DO 1470 KK=1,NG
      N=0
      DO 1460 I=1,3
      DO 1460 J=1,3
 1460 IF (SSS(I,J).NE.ROT(KK,I,J)) N=1
      IF (N.EQ.0) GO TO 1490
 1470 CONTINUE
      NG=NG+1
      IF(ISPGCM.EQ.1) MEMDEV(NG)=K*100+L
      DO 1480 I=1,3
      TSL(NG,I)=TSS(I)
      DO 1480 J=1,3
 1480 ROT(NG,I,J)=SSS(I,J)
 1490 L=L+1
      IF (K.NE.NSS) GO TO 1550
      DO 1540 I=1,3
      J=I+1
      IK=I+2
      IM=I+4
      IF (J.GT.3) J=J-3
      IF (IK.GT.3) IK=IK-3
      IF (NBR.NE.4) GO TO 1510
      IF (I.GT.1) GO TO 1540
      IF (TSL(NSS,1).GE.12.AND.TSL(NSS,2).GE.12.AND.
     *   TSL(NSS,3).GE.12) NN=1
      IF (NN.NE.1) GO TO 1540
      DO 1500 IJ=1,3
 1500 TSL(NSS,IJ)=TSL(NSS,IJ)-12
 1510 NN=0
      IF (NBR.EQ.6) GO TO 1520
      IF (NBR.NE.I) GO TO 1540
 1520 IF (TSL(NSS,J).GE.12.AND.TSL(NSS,IK).GE.12) NN=1
      IF (NN.NE.1) GO TO 1540
      DO 1530 II=1,3
      TSL(NSS,II)=TSL(NSS,II)-GET(II,IM)
 1530 CONTINUE
 1540 CONTINUE
 1550 GO TO 1430
C      DETERMINE CENTROSYMMETRY AND SHIFT VECTOR TO A CENTRE OF SYMM..
 1560 IF (NSS.GT.0) NS=0
      IF (NS.EQ.0) ICENT=1
      DO 1570 I=1,3
 1570 TSA(1,I)=0
      IF (NS.EQ.1) GO TO 1590
      DO 1580 K=1,3
      TSA(1,K)=TSL(NSS,K)/2
      TSA(1,K)=MOD(TSA(1,K),24)
 1580 IF (TSA(1,K).LT.0) TSA(1,K)=TSA(1,K)+24
C      DETERMINE THE REF, NUMBER OF SHIFT VECTOR TO AN ORIGIN OF I. T..
 1590 IF (IIS.EQ.2) GO TO 1650
      NSQ=1
      DO 1600 I=1,13
 1600 IF (IN(I).EQ.H(1).OR.IN(I).EQ.0) IN(I)=H(4)
      IF (SYS.LT.3.OR.I3.NE.H(4).OR.SYS.GT.4) GO TO 1630
      IF (SYS.NE.3) GO TO 1610
      IF ((NH(I2)+NH(J2))/2.GT.HC(4))MS=8-6*(NH(K2)-HC(4))
      IF ((J1.EQ.H(11).OR.(NH(I1)+NH(J1))/2.EQ.HC(20)).AND.K1.EQ.H(6))
     *                      MS=8
      IF (MS.EQ.8.AND.(I1.EQ.H(19).OR.I1.EQ.H(13))) MS=1
      IF (NBR.EQ.3.AND.I1.EQ.H(19).AND.K1.EQ.H(11)) MS=8
      IF ((I1.EQ.H(20).OR.I1.EQ.H(12)).AND.(J1.EQ.H(13).OR.J1.EQ.H(19))
     *               .AND.K1.EQ.H(6)) MS=2
      IF (NBR.EQ.4.AND.K1.EQ.H(11)) MS=7
      IF (J1.EQ.H(18).AND.K1.EQ.H(6)) MS=6
      GO TO 1680
 1610 IF (NBR.EQ.4.AND.I2.EQ.H(5)) MS=9
      IF ((NH(J1)+NH(K1))/2.EQ.HC(6).AND.(NH(I2)-HC(4)+1)/2.GT.
     *       (NH(I2)-HC(4))/2) MS=3
      IF (MS.EQ.3.AND.NBR.EQ.4) MS=12
      DO 1620 I=1,4
 1620 IF (J2.EQ.H(5).AND.I2.EQ.H(I+3)) MS=8+I
      IF (K1.EQ.H(13).AND.J1.EQ.H(6)) MS=3+8*(NH(J2)-HC(4))
      IF (J2.EQ.H(5).AND.K1.EQ.H(19).OR.K1.EQ.H(18))
     *                               MS=13-4*(NH(J2)-HC(4))
      IF (J1.EQ.H(12).OR.J1.EQ.H(13).AND.I1.EQ.H(2))
     *                               MS=8-5*(NH(J1)-HC(12))
      IF (J1.EQ.H(20)) MS=9-(NH(I2)-HC(4))/2
      IF (J1.EQ.H(12).AND.NH(K1).GE.HC(11).OR.J1.EQ.H(13).AND.
     *                  K1.EQ.H(18)) MS=9-7*(J1-H(12))
      IF (J1.EQ.H(19).AND.K1.EQ.H(18).OR.J1.EQ.H(13).AND.
     *                  K1.EQ.H(19)) MS=14+NH(I2)-HC(4)
 1630 IF (SYS.NE.4) GO TO 1640
      IF (J1.EQ.H(12).OR.NBR.EQ.4.AND.J1.EQ.H(13)) MS=14
      IF (I4.EQ.H(20).AND.(J1.EQ.H(4).OR.J1.EQ.I4).OR.
     *     I3.EQ.H(20).AND.J1.EQ.I3) MS=14
      IF (I4.EQ.H(11).AND.NH(J1).GT.HC(10)) MS=18-((NH(J1)-HC(13))/10)
 1640 IF (I1.EQ.H(7).AND.I2.NE.H(4).AND.J1.NE.H(4)) MS=NH(I2)-HC(4)+3
      IF (NBR.EQ.6.AND.I2.EQ.H(5).OR.NBR.EQ.4.AND.I1.EQ.H(11)) MS=7
      IF (I3.EQ.H(19).AND.J1.EQ.H(20)) MS=14
      GO TO 1680
C      SHIFT TO ANOTHER ORIGIN.
 1650 DO 1660 I=1,12
      IF (JN.LT.14) IN(JN)=NV(I)
      IF (NV(I).NE.H(1)) GO TO 1660
      JM=JM+1
      JN=4*JM+1
 1660 JN=JN+1
      DO 1670 I=1,9,4
      JP=JP+1
      IO=I+1
      LO=I+3
 1670 IF (IN(LO).NE.H(4)) NSW(JP)=(NH(IN(IO))-HC(4))*24/
     *                           (NH(IN(LO))-HC(4))
C      APPLY THE SHIFT OF ORIGIN TO ALL OPERATIONS.
 1680 IF (NS.EQ.1.AND.IIS.EQ.0) GO TO 1730
      DO 1690 K=1,3
      IF (IIS.NE.1) NSV(K,MS)=0
      NSW(K)=NSV(K,MS)+TSA(1,K)+NSW(K)
      NSW(K)=MOD(NSW(K),24)
 1690 IF (NSW(K).LT.0) NSW(K)=NSW(K)+24
      DO 1720 I=1,NG
      DO 1720 J=1,3
      L=TSL(I,J)
      DO 1700 K=1,3
 1700 L=L-(ROT(NU,J,K)-ROT(I,J,K))*NSW(K)
 1710 IF (L.LT.24.AND.L.GE.0) GO TO 1715
      IF (L.LT.0) L=L+24
      IF (L.GE.24) L=L-24
      GO TO 1710
 1715 TSL(I,J)=L
 1720 CONTINUE
C      NORMALIZATION OF CENTRING TYPE.
 1730 DO 1780 I=1,NG
      DO 1780 J=1,3
      NN=0
      K=J+1
      KI=J+2
      IF (K.GT.3) K=K-3
      IF (KI.GT.3) KI=KI-3
      IF (NBR.NE.4) GO TO 1750
      IF (J.GT.1) GO TO 1780
      IF (TSL(I,1).GE.12.AND.TSL(I,2).GE.12.AND.TSL(I,3).GE.12) NN=1
      IF (NN.NE.1) GO TO 1780
      DO 1740 IJ=1,3
 1740 TSL(I,IJ)=TSL(I,IJ)-12
 1750 IF (NBR.EQ.6) GO TO 1760
      IF (NBR.NE.J) GO TO 1780
 1760 IF (TSL(I,K).GE.12.AND.TSL(I,KI).GE.12) NN=1
      IF (NN.NE.1) GO TO 1780
      DO 1770 II=1,3
      JI=J+4
 1770 TSL(I,II)=TSL(I,II)-GET(II,JI)
 1780 CONTINUE
C
      IF (LATT.GE.2.AND.LATT.LE.5) THEN
       NLC=2
       DO 1920 I=1,3
 1920  TCL(NLC,I)=GET(I,3+LATT)
      ELSEIF (LATT.EQ.6) THEN
       NLC=4
       DO 1921 I=2,4
       DO 1921 J=1,3
 1921  TCL(I,J)=GET(J,3+I)
      ELSEIF (LATT.EQ.7) then
       NLC=3
       DO 1922 I=2,3
       DO 1922 J=1,3
1922   TCL(I,J)=GET(J,16+I)
      ENDIF       
C     COMPLETE THE 4D OPERATION
      IF(ISPGCM.EQ.0) GOTO 1788
      DO 1781 I=1,NGENER
       IF(IUN-1) 1910,1911,1912
 1910  IF(KEPS.EQ.1) ROT(I,4,4)=ROT(I,1,1)
       IF(KEPS.EQ.0) ROT(I,4,4)=ROT(I,3,3)
       GOTO 1913
 1911  IF(KEPS.EQ.1) ROT(I,4,4)=ROT(I,3,3)
       IF(KEPS.EQ.0) ROT(I,4,4)=ROT(I,1,1)
       GOTO 1913
 1912  IF(KEPS.EQ.1) ROT(I,4,4)=ROT(I,3,3)
       IF(KEPS.EQ.0) ROT(I,4,4)=ROT(I,2,2) 
 1913  L0=0
       DO 1782 J=1,3
        J0=0
       DO 1783 K=1,3
 1783  J0=J0+QRAT(K)*ROT(I,K,J)
       J0=J0-ROT(I,4,4)*QRAT(J)
       J0=J0/24
       ROT(I,4,J)=J0
       ROT(I,J,4)=0
 1782  L0=L0+QRAT(J)*TSL(I,J)
       TSL(I,4)=TAO(I)+L0/24
 1781  TSL(I,4)=MOD(TSL(I,4),24)
      J0=NGENER+1
      DO 1784 I=J0,NG
       J=MEMDEV(I)/100
       K=MOD(MEMDEV(I),100)
       DO 1785 L=1,3
        L0=0
        DO 1786 M=1,4
 1786   L0=L0+ROT(J,4,M)*ROT(K,M,L)
        ROT(I,4,L)=L0
 1785   ROT(I,L,4)=0
       ROT(I,4,4)=ROT(J,4,4)*ROT(K,4,4)
       L0=0
       DO 1787 L=1,4
 1787  L0=L0+ROT(J,4,L)*TSL(K,L)
       L0=L0+TSL(J,4)
       TSL(I,4)=MOD(L0,24)
 1784 IF(TSL(I,4).LT.0) TSL(I,4)=TSL(I,4)+24
 1788 CONTINUE
C      OUTPUT ON LINE-PRINTER.
      IF (SYS.EQ.1) WRITE(10,500) NUMB,H(28),H(15),H(14),
     *                            H(13),H(29),H(14),
     *                             H(20),H(14),H(13)
      IF (SYS.EQ.2) WRITE(10,500) NUMB,H(19),H(25),H(20),
     *                             H(25),H(13),H(29),
     *                             H(14),H(20),H(14),H(13)
      IF (SYS.EQ.3) WRITE(10,500) NUMB,H(25),H(15),H(28),
     *                             H(30),H(25),H(15),
     *                             H(30),H(25),H(19),H(12),H(14),H(13)
      IF (SYS.EQ.4) WRITE(10,500) NUMB,H(28),H(23),H(28),
     *                             H(15),H(11),H(26),
     *                             H(25),H(20),H(11),H(29)
      IF (SYS.EQ.5) WRITE(10,500) NUMB,H(30),H(23),H(31),
     *                             H(11),H(26),H(25),
     *                             H(20),H(11),H(29)
      IF (SYS.EQ.6) WRITE(10,500) NUMB,H(13),H(24),H(12),H(14),H(13)
      WRITE(CH,'(36A1)') (NCHTOT(ISYCOUNT),ISYCOUNT=1,36)
      IF(ISPGCM.EQ.0) THEN
       WRITE(10,450) (NCHTOT(I),I=1,36)
      ELSEIF(ISPGCM.EQ.1) THEN
       WRITE(10,451) (NCHTOT(I),I=1,36)
      ENDIF
C     LOOK FOR 'X,Y,Z' OPERATION
      DO 1790 I=1,NG
      IZER=IABS(TSL(I,1))+IABS(TSL(I,2))+IABS(TSL(I,3))
      IF (IZER.NE.0) GO TO 1790
      IZER=IABS(ROT(I,1,2))+IABS(ROT(I,1,3))+IABS(ROT(I,2,3))+
     *     IABS(ROT(I,2,1))+IABS(ROT(I,3,1))+IABS(ROT(I,3,2))
      IF (IZER.NE.0) GO TO 1790
      IF (ROT(I,1,1).EQ.1.AND.ROT(I,2,2).EQ.1.AND.ROT(I,3,3).EQ.1)
     *                       GO TO 1800
 1790 CONTINUE
      IERR=2
      GOTO 2200
C     PRINT SYMMTRICAL SYMBOLS
 1800 IF(ISPGCM.EQ.0) THEN
      WRITE(10,800)
      IF (LATT.EQ.2) WRITE(10,1810)
 1810 FORMAT(1H+,10X,20H+(0 0 0   0 1/2 1/2)/)
      IF (LATT.EQ.3) WRITE(10,1820)
 1820 FORMAT(1H+,10X,20H+(0 0 0   1/2 0 1/2)/)
      IF (LATT.EQ.4) WRITE(10,1830)
 1830 FORMAT(1H+,10X,20H+(0 0 0   1/2 1/2 0)/)
      IF (LATT.EQ.5) WRITE(10,1840)
 1840 FORMAT(1H+,10X,22H+(0 0 0   1/2 1/2 1/2)/)
      IF (LATT.EQ.6) WRITE(10,1850)
 1850 FORMAT(1H+,10X,44H+(0 0 0   0 1/2 1/2   1/2 0 1/2   1/2 1/2 0)/)
      IF (LATT.EQ.7) WRITE(10,1860)
 1860 FORMAT(1H+,10X,36H+(0 0 0   2/3 1/3 1/3   1/3 2/3 2/3)/)
      ELSEIF(ISPGCM.EQ.1) THEN
      WRITE(10,800)
      IF (LATT.EQ.2) WRITE(10,1811)
 1811 FORMAT(1H+,10X,24H+(0 0 0 0   0 1/2 1/2 0)/)
      IF (LATT.EQ.3) WRITE(10,1821)
 1821 FORMAT(1H+,10X,24H+(0 0 0 0   1/2 0 1/2 0)/)
      IF (LATT.EQ.4) WRITE(10,1831)
 1831 FORMAT(1H+,10X,24H+(0 0 0 0   1/2 1/2 0 0)/)
      IF (LATT.EQ.5) WRITE(10,1841)
 1841 FORMAT(1H+,10X,26H+(0 0 0 0   1/2 1/2 1/2 0)/)
      IF (LATT.EQ.6) WRITE(10,1851)
 1851 FORMAT(52H+(0 0 0 0   0 1/2 1/2 0   1/2 0 1/2 0   1/2 1/2 0 0)/)
      IF (LATT.EQ.7) WRITE(10,1861)
 1861 FORMAT(1H+,10X,42H+(0 0 0 0   2/3 1/3 1/3 0   1/3 2/3 2/3 0)/)
      ENDIF
      I0=I-1
      IF(ISPGCM.EQ.0.AND.ILIN.EQ.1) THEN
      IR=I0-1
      JJ=0
 1890 IR=MOD(NG+IR+1,NG)+1
      IE=MOD(NG+IR,NG)+1
      DO 1900 I1=1,2
      JJ=JJ+1
      I=IR
      IF (I1.EQ.2) I=IE
      IU=I
      DO 1900 J=1,3
      MO=ROT(I,J,1)+ROT(I,J,2)*3+ROT(I,J,3)*4+5
      TX(J,I1)=TEXB(MO)
      DO 1900 K=1,2
      LO=2*TSL(I,J)+K
 1900 GGES(K,J,I1)=NTEX(LO)
      JJ1=JJ-1
      IF (JJ.LE.NG) WRITE(10,300) JJ1,TX(1,1),(GGES(I,1,1),I=1,2),
     *      TX(2,1),(GGES(I,2,1),I=1,2),TX(3,1),(GGES(I,3,1),I=1,2),
     *                              JJ,TX(1,2),(GGES(I,1,2),I=1,2),
     *      TX(2,2),(GGES(I,2,2),I=1,2),TX(3,2),(GGES(I,3,2),I=1,2)
      IF (JJ.GT.NG) WRITE(10,300) JJ1,TX(1,1),(GGES(I,1,1),I=1,2),
     *      TX(2,1),(GGES(I,2,1),I=1,2),TX(3,1),(GGES(I,3,1),I=1,2)
      IF (JJ.LT.NG) GO TO 1890
      ELSEIF(ISPGCM.EQ.0.AND.ILIN.NE.1) THEN
      DO 5012 J=I,2,-1
      M=J-1
      DO 5012 K=1,3
      DO 5013 L=1,3
 5013 ROT(J,K,L)=ROT(M,K,L)
 5012 TSL(J,K)=TSL(M,K)
      DO 5010 K=1,3
      DO 5011 L=1,3
      ROT(1,K,L)=0
 5011 IF(K.EQ.L) ROT(1,K,L)=1
 5010 TSL(1,K)=0
      I=MOD(NG,4)
      J=NG-I
      DO 5014 K=1,J,4
      II1=K
      II2=K+1
      II3=K+2
      II4=K+3
      WRITE(10,*)
      DO 5014 M=1,3
      TTS1=TSL(II1,M)/24.0
      TTS2=TSL(II2,M)/24.0
      TTS3=TSL(II3,M)/24.0
      TTS4=TSL(II4,M)/24.0
 5014 WRITE(10,5001) (ROT(II1,M,L),L=1,3),TTS1,
     * (ROT(II2,M,L),L=1,3),TTS2,(ROT(II3,M,L),L=1,3),TTS3,
     * (ROT(II4,M,L),L=1,3),TTS4
      IF(I.EQ.1) THEN
       WRITE(10,*)
       II1=J+1
       DO 5015 M=1,3
       TTS1=TSL(II1,M)/24.0
 5015  WRITE(10,5002) (ROT(II1,M,L),L=1,3),TTS1
      ELSEIF(I.EQ.2) THEN
       WRITE(10,*)
       II1=J+1
       II2=J+2
       DO 5016 M=1,3
       TTS1=TSL(II1,M)/24.0
       TTS2=TSL(II2,M)/24.0
 5016  WRITE(10,5003) (ROT(II1,M,L),L=1,3),TTS1,
     *  (ROT(II2,M,L),L=1,3),TTS2
      ELSEIF(I.EQ.3) THEN
       WRITE(10,*)
       II1=J+1
       II2=J+2
       II3=J+3
       DO 5017 M=1,3
       TTS1=TSL(II1,M)/24.0
       TTS2=TSL(II2,M)/24.0
       TTS3=TSL(II3,M)/24.0
 5017  WRITE(10,5004) (ROT(II1,M,L),L=1,3),TTS1,
     *  (ROT(II2,M,L),L=1,3),TTS2,(ROT(II3,M,L),L=1,3),TTS3
      ENDIF
 5001 FORMAT(4(3X,3I3,F5.2))
 5002 FORMAT(3X,3I3,F5.2)
 5003 FORMAT(2(3X,3I3,F5.2))
 5004 FORMAT(3(3X,3I3,F5.2))
      ELSEIF(ISPGCM.EQ.1) THEN
      DO 5022 J=I,2,-1
      M=J-1
      DO 5022 K=1,4
      DO 5023 L=1,4
 5023 ROT(J,K,L)=ROT(M,K,L)
 5022 TSL(J,K)=TSL(M,K)
      DO 5020 K=1,4
      DO 5021 L=1,4
      ROT(1,K,L)=0
 5021 IF(K.EQ.L) ROT(1,K,L)=1
 5020 TSL(1,K)=0
      I=MOD(NG,4)
      J=NG-I
      DO 5024 K=1,J,4
      II1=K
      II2=K+1
      II3=K+2
      II4=K+3
      WRITE(10,*)
      DO 5024 M=1,4
       TTS1=TSL(II1,M)/24.0
       TTS2=TSL(II2,M)/24.0
       TTS3=TSL(II3,M)/24.0
       TTS4=TSL(II4,M)/24.0
 5024 WRITE(10,5006) (ROT(II1,M,L),L=1,4),TTS1,
     * (ROT(II2,M,L),L=1,4),TTS2,(ROT(II3,M,L),L=1,4),TTS3,
     * (ROT(II4,M,L),L=1,4),TTS4
      IF(I.EQ.1) THEN
       WRITE(10,*)
       II1=J+1
       DO 5025 M=1,4
       TTS1=TSL(II1,M)/24.0
 5025  WRITE(10,5007) (ROT(II1,M,L),L=1,4),TTS1
      ELSEIF(I.EQ.2) THEN
       WRITE(10,*)
       II1=J+1
       II2=J+2
       DO 5026 M=1,4
       TTS1=TSL(II1,M)/24.0
       TTS2=TSL(II2,M)/24.0
 5026  WRITE(10,5008) (ROT(II1,M,L),L=1,4),TTS1,
     *  (ROT(II2,M,L),L=1,4),TTS2
      ELSEIF(I.EQ.3) THEN
       WRITE(10,*)
       II1=J+1
       II2=J+2
       II3=J+3
       DO 5027 M=1,4
       TTS1=TSL(II1,M)/24.0
       TTS2=TSL(II2,M)/24.0
       TTS3=TSL(II3,M)/24.0
 5027  WRITE(10,5009) (ROT(II1,M,L),L=1,4),TTS1,
     *  (ROT(II2,M,L),L=1,4),TTS2,(ROT(II3,M,L),L=1,4),TTS3
      ENDIF
 5006 FORMAT(4(2X,4I3,F5.2))
 5007 FORMAT(2X,4I3,F5.2)
 5008 FORMAT(2(2X,4I3,F5.2))
 5009 FORMAT(3(2X,4I3,F5.2))
        ENDIF
 300  FORMAT(3X,2(I2,1H.,3X,2(A4,2A2,2X),A4,2A2,6X))
 450  FORMAT(13H SPACE GROUP:,5X,36A1)
 451  FORMAT(18H SUPERSPACE GROUP:,5X,36A1)
 500  FORMAT(1X,'*** DIMS: PHASING RESULT ANALYSIS ***',I4//
     * 16H SYMMETRY CLASS:,5X,16A1//)
 800  FORMAT(23H SYMMETRICAL OPERATION:)
      GOTO 2400
 2200 IF(IERR.GE.1) THEN
       CALL ERRSPG(NCHTOT,NCHLD,JERR,*88,*2400)
      ENDIF
C 2200  WRITE(10,2300) NCH,IERR
C 2300  FORMAT(1X,'CAN NOT BE ACCEPTED SPG ',80A1/25X,'ERROR AT THE ',
C     *            'CHECKING POINT ',I1)
2400  RETURN
      END
C             ---------
      FUNCTION NH(ICH)
      INTEGER H(36),HC(36)
      DATA H/1H ,1H-,1H/,1H0,1H1,1H2,1H3,1H4,1H5,1H6,1HA,1HB,1HC,1HI,1HR
     * ,1HF,1HP,1HD,1HM,1HN,1HY,1H:,1HE,1HU,1HO,1HG,1HQ,1HT,1HL,1HH,1HX,
     *  1HS,1HV,1HW,1H[,1H]/
      DATA HC/32,45,47,48,49,50,51,52,53,54,65,66,67,73,82,
     *  70,80,68,77,78,89,58,69,85,79,71,81,84,76,72,88,83,86,87,91,93/
      IF (ICH.NE.0) GO TO 100
      NH=0
      RETURN
 100  DO 200 I=1,34
 200  IF (ICH.EQ.H(I)) GO TO 300
 300  NH=HC(I)
      RETURN
      END
C             ---------
      SUBROUTINE ERRSPG(NCHTOT,NCHLD,JERR,*,*)
      CHARACTER*1 YN,YN1,YN2,YN3,YN4
      DIMENSION  NCHTOT(80),NCHLD(80)
      YN1='Y'
      YN2='y'
      YN3='N'
      YN4='n'
      WRITE(*,*)
      WRITE(*,32) (NCHTOT(I),I=1,36)
      WRITE(*,33)
 33   FORMAT(' # The above SYM.-SYMBOL can not be accepted!') 
      WRITE(*,31)
 31   FORMAT('$   Stop this run (Y/N)?')
 32   FORMAT(13H SPACE GROUP:,5X,36A1)
      READ(*,'(A)') YN
      WRITE(*,*)
      IF(YN.EQ.YN1.OR.YN.EQ.YN2) THEN
       JERR=99
       RETURN 2
      ELSEIF(YN.EQ.YN3.OR.YN.EQ.YN4) THEN
       WRITE(*,55)
       READ(*,'(80A1)') NCHLD
       RETURN 1
      ENDIF
 55   FORMAT('$  Please give the correct SYMBOL:  ')
      RETURN
      END
C
C     SPGR4D END
C
