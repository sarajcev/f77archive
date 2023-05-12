C*******************************************************************************
C*    GLAVNI PROGRAM PRORA^UNA KRATKOG SPOJA * U K S X *                       *
C*PROGRAM -HTKSP005                                                            *
C*  umjesto PANELA  -MEK011,MEK012 ubacene naredbe READ i otvorene dat:        *
C*       konst1 i  konst2                                                      *
C*     SREDJENE NAREDBE OPEN                                                   *
C*              izbaceno: carriagecontrol, recordtype i organization           *
C*              promjenjeno NAME='...   u  FILE='...                           *
C*     izbaceni pomocni ispisi                                                 *
C*     dimensioni smanjeni na 500 umjesto 600                                  *
C*******************************************************************************
      REAL  IB(500)                                                     
      INTEGER  S(500),T(500),BS(500),BT(500),TR(500),DELTA(500),
     *RCR(500),SCR(500),TCR(500),SP(500),B(500),BMAX,
     *SNUL(500),TNUL(500),S1UL(500),T1UL(500),S1NUL(500),T1NUL(500),
     *CVOR(500),DELTAB(500)          
      DIMENSION  X(500),UNS(500),UNT(500),SN(500),XNUL(500),
     *XN(500),IZ(500),XTR(500),XNULTA(500),IE(500),X1ULTA(500),
     *X1NUL(500),XXX(50000),XXXNUL(20000),ANAP(500),KK3V1(20),
     *KK3V2(20),KK1V2(20),KK2V2(20),KK1V1(20),KK1V3(20),UB(500),XB(500),
     *IPI(500),ISIGMA(500),NAP(500)  
      CHARACTER*1 AIME(500)*7,AIME1(500)*3
      CHARACTER*5 POMKK(20),TITLE*71
      open (unit=1,status='old',file='konst.dat')
      open (unit=2,status='old',file='uksw.dat')
      open (unit=3,status='old',access='sequential',recl=24,file='koncv.
     *dat')
      open (unit=4,status='old',file='konstab.dat')
      open (unit=7,status='new',access='sequential',recl=132,form='forma
     *tted',file='htksl005.lis')
      READ(1,1001)BMAX,INTUB,NG,NKON                                      
1001  FORMAT(T7,2I3,3X,2I3)              
      DO 2000 I=1,NG                     
2000  READ(2,2001) S(I),T(I),BS(I),BT(I),X(I),UNS(I),UNT(I),SN(I),        
     *TR(I),DELTA(I),RCR(I),SCR(I),TCR(I),SP(I),XNUL(I)
2001  FORMAT(4I3,4F9.4,I1,I4,4I3,F9.4)                 
      READ(3,3001)CVOR(1),AIME(1),AIME1(1),NAP(1)
      DO 3000 I=2,NKON                                 
      J=I-1                                            
3000  READ(3,3001)CVOR(J),AIME(J),AIME1(J),NAP(J)
3001  FORMAT(I3,3X,A7,5X,A3,I3)         
      SB=100.                           
      N=NKON-1                          
      UB(1) = FLOAT(INTUB)              
      DO 3333 I=1,N                     
3333  ANAP(I) =FLOAT(NAP(I))            
      READ(4,10001)IBAZ,JEDREA,NAPNCV,IZVITR,KUTZAK,ITREZV,NULTER,
     *NULRSR,MATDIR,MATNUL
10001 FORMAT(10I1)                         
10005 READ(4,10003)KS3V1,(KK3V1(K),K=1,20),KS3V2,(kk3v2(k),k=1,20),KS1V1
     *,(KK1V1(K),K=1,20),KS1V2,(KK1V2(K),K=1,20),Ks1v3,(kk1v3(K),K=1,20)
     *,KS2V2,(KK2V2(K),K=1,20)
10003 FORMAT(I1,20I3)
      write(*,1941)
1941  format(////' *** PRORA^UN KRATKOG SPOJA U TOKU - PRI^EKAJ ***')
      NK2V2=0
      NK1V1=0
      NK1V2=0
      NK1V3=0
      NK3V1=0
      NK3V2=0
      WRITE(7,1004)                                                     UKS00240
1004  FORMAT(1H1, 30(/), T50, 'P R O G R A M',//,T51,'* U K S X *')     UKS00250
      WRITE(7,1005)  NG, N, BMAX, SB, UB(1)                             UKS00260
1005  FORMAT(1H1, T50, 'U^ITANI PODACI',/,T50,14(1H-),//,' BROJ GRANA:',UKS00270
     $10X,'BROJ ^VOROVA:',10X,'BROJ PODMRE@A:',10X,'BAZNA SNAGA:',10X,'BUKS00280
     $AZNI NAPON UB(1):',/,T5,I3,20X,I3,21X,I3,17X,F9.3,16X,F9.3)       UKS00290
      WRITE(7,1006) (S(IG),T(IG),BS(IG),BT(IG),X(IG),UNS(IG),UNT(IG),SN(UKS00300
     $IG),TR(IG),DELTA(IG),RCR(IG),SCR(IG),TCR(IG),SP(IG),XNUL(IG),IG,IGUKS00310
     $=1,NG)                                                            UKS00320
1006  FORMAT(///,T50,'PODACI GRANA MRE@E',//,T3,'S',T9,'T',T14,'BS',T19,UKS00330
     $'BT',T25,'REAKT.X',T39,'UNS',T51,'UNT',T64,'SN',T72,'TR',T77,'DELTUKS00340
     $A',T85,'RCR',T91,'SCR',T97,'TCR',T104,'SP',T111,'X-NUL',T121,'GRANUKS00350
     $A',//(I4,3X,I3,2X,I3,2X,I3,3X,F9.3,3X,F9.3,3X,F9.3,3X,F9.3,4X,I1,4UKS00360
     $X,I4,3X,I3,3X,I3,3X,I3,3X,I3,3X,F9.3,4X,I3))                      UKS00370

      WRITE(7,1007)(CVOR(I),AIME(I),AIME1(I),ANAP(I),I=1,N)             UKS00380
c     open(unit=11,access='sequential',form='formatted',recl=15,status='
c    *new',file='000.101')
c     write(11,fmt='(i3,1x,a7,1x,a3)')(cvor(i),aime(i),aime1(i),i=1,n)
c     close(unit=11)
1007  FORMAT(//,T3,'BROJ',T12,'NAZIV',T23,'NAZ.NAPON',/,T2,'^VORA',T12,'UKS00390
     $^VORA',T23,'^VORA(KV)',//(2X,I3,4X,A7,A3,5X,F5.1))                UKS00400
      IF(IBAZ) 51,50,51                                                 UKS00410
50    WRITE(7,1050)                                                     UKS00420
1050  FORMAT(//,T35,'DA LI SE POSREDSTVOM PROGRAMA  * U K S X *  ZAHTJEVUKS00430
     $A ?',/,T2,'A)',T5,'TISKANJE SLIJEDE]IH VELI^INA:',/,T5,'BAZNIH VRIUKS00440
     $JEDNOSTI',T23,38(1H.),T61,'NE')   
      GO TO 52                                                          UKS00460
51    WRITE(7,1051)                                                     UKS00470
1051  FORMAT(//,T35,'DA LI SE POSREDSTVOM PROGRAMA  * U K S X *  ZAHTJEVUKS00480
     $A ?',/,T2,'A)',T5,'TISKANJE SLIJEDE]IH VELICINA:',/,T5,'BAZNIH VRIUKS00490
     $JEDNOSTI',T23,38(1H.),T61,'DA')     
52    IF(JEDREA) 54,53,54                                               UKS00510
53    WRITE(7,1052)                                                     UKS00520
1052  FORMAT(T5,'JEDINI^NIH REAKTANCIJA',T27,34(1H.),T61,'NE')          UKS00530
      GO TO 55                                                          UKS00540
54    WRITE(7,1053)                                                     UKS00550
1053  FORMAT(T5,'JEDINI^NIH REAKTANCIJA',T27,34(1H.),T61,'DA')          UKS00560
55    IF(NAPNCV) 57,56,57                                               UKS00570
56    WRITE(7,1054)                                                     UKS00580
1054  FORMAT(T5,'NAPONSKIH NIVOA PO ^VOROVIMA',T33,28(1H.),T61,'NE')    UKS00590
      GO TO 58                                                          UKS00600
57    WRITE(7,1055)                                                     UKS00610
1055  FORMAT(T5,'NAPONSKIH NIVOA PO ^VOROVIMA',T33,28(1H.),T61,'DA')    UKS00620
58    IF(IZVITR) 60,59,60                                               UKS00630
59    WRITE(7,1056)                                                     UKS00640
1056  FORMAT(T5,'JEDIN. REAKT. TRONAMOTNOG TR. U SPOJU ZVIJEZDA',T51,10(UKS00650
     $1H.),T61,'NE')                                                    UKS00660
      GO TO 61                                                          UKS00670
60    WRITE(7,1057)                                                     UKS00680
1057  FORMAT(T5,'JEDIN. REAKT. TRONAMOTNOG TR. U SPOJU ZVIJEZDA',T51,10(UKS00690
     $1H.),T61,'DA')                                                    UKS00700
61    IF(KUTZAK) 63,62,63                                               UKS00710
62    WRITE(7,1058)                                                     UKS00720
1058  FORMAT(T5,'KUTOVA ZAKRETANJA',T22,39(1H.),T61,'NE')               UKS00730
      GO TO 64                                                          UKS00740
63    WRITE(7,1059)                                                     UKS00750
1059  FORMAT(T5,'KUTOVA ZAKRETANJA',T22,39(1H.),T61,'DA')               UKS00760
64    IF(ITREZV) 66,65,66                                               UKS00770
65    WRITE(7,1060)                                                     UKS00780
1060  FORMAT(T5,'UZEMLJENJA PODMRE@A',T24,37(1H.),T61,'NE')             UKS00790
      GO TO 67                                                          UKS00800
66    WRITE(7,1061)                                                     UKS00810
1061  FORMAT(T5,'UZEMLJENJA PODMRE@A',T24,37(1H.),T61,'DA')             UKS00820
67    IF(NULTER) 69,68,69                                               UKS00830
68    WRITE(7,1062)                                                     UKS00840
1062  FORMAT(T5,'PRVOG NIZA NULTIH REAKTANCIJA',T34,27(1H.),T61,'NE')   UKS00850
      GO TO 70                                                          UKS00860
69    WRITE(7,1063)                                                     UKS00870
1063  FORMAT(T5,'PRVOG NIZA NULTIH REAKTANCIJA',T34,27(1H.),T61,'DA')   UKS00880
70    IF(NULRSR) 72,71,72                                               UKS00890
71    WRITE(7,1064)                                                     UKS00900
1064  FORMAT(T5,'SRE\ENOG NIZA NULTIH REAKTANCIJA',T38,23(1H.),T61,'NE')UKS00910
      GO TO 73                                                          UKS00930
72    WRITE(7,1065)                                                     UKS00940
1065  FORMAT(T5,'SRE\ENOG NIZA NULTIH REAKTANCIJA',T38,23(1H.),T61,'DA')UKS00950
73    IF(MATDIR) 75,74,75                                               UKS00970
74    WRITE(7,1066)                                                     UKS00980
1066  FORMAT(T5,'MATRICU DIREKTNOG SISTEMA MRE@E',T36,25(1H.),T61,'NE') UKS00990
      GO TO 76                                                          UKS01000
75    WRITE(7,1067)                                                     UKS01010
1067  FORMAT(T5,'MATRICU DIREKTNOG SISTEMA MRE@E',T36,25(1H.),T61,'DA') UKS01020
76    IF(MATNUL) 78,77,78                                               UKS01030
77    WRITE(7,1068)                                                     UKS01040
1068  FORMAT(T5,'MATRICU NULTOG SISTEMA MRE@E',T33,28(1H.),T61,'NE')    UKS01050
      GO TO 79                                                          UKS01060
78    WRITE(7,1069)                                                     UKS01070
1069  FORMAT(T5,'MATRICU NULTOG SISTEMA MRE@E',T33,28(1H.),T61,'DA')    UKS01080
79    IF(KS3V1) 81,80,81                                                UKS01090
80    WRITE(7,1070)                                                     UKS01100
1070  FORMAT(T2,'B)',T5,'RA^UNANJE I PRIKAZ SLIJEDE]IH KRATKIH SPOJEVA  UKS01110
     $:',/,T5,'TROPOLNOG KRATKOG SPOJA - VARIJANTA 1',T42,19(1H.),T61,'NUKS01120
     $E')                                                               UKS01130
      GO TO 82                                                          UKS01140
81    WRITE(7,1071)                                                     UKS01150
1071  FORMAT(T2,'B)',T5,'RA^UNANJE I PRIKAZ SLIJEDE]IH KRATKIH SPOJEVA  UKS01160
     $:',/,T5,'TROPOLNOG KRATKOG SPOJA - VARIJANTA 1',T42,19(1H.),T61,'DUKS01170
     $A')                                                               UKS01180
82    IF(KS3V2) 84,83,84                                                UKS01190
83    WRITE(7,1072)                                                     UKS01200
1072  FORMAT(T5,'TROPOLNOG KRATKOG SPOJA - VARIJANTA 2',T42,19(1H.),T61,UKS01210
     $'NE')                                                             UKS01220
      GO TO 85                                                          UKS01230
84    WRITE(7,1073)                                                     UKS01240
1073  FORMAT(T5,'TROPOLNOG KRATKOG SPOJA - VARIJANTA 2',T42,19(1H.),T61,UKS01250
     $'DA')                                                             UKS01260
85    IF(KS1V1) 87,86,87                                                UKS01270
86    WRITE(7,1074)                                                     UKS01280
1074  FORMAT(T5,'JEDNOPOLNOG KRATKOG SPOJA - VARIJANTA 1',T44,17(1H.),T6UKS01290
     $1,'NE')                                                           UKS01300
      GO TO 88                                                          UKS01310
87    WRITE(7,1075)                                                     UKS01320
1075  FORMAT(T5,'JEDNOPOLNOG KRATKOG SPOJA - VARIJANTA 1',T44,17(1H.),T6UKS01330
     $1,'DA')                                                           UKS01340
88    IF(KS1V2) 90,89,90                                                UKS01350
89    WRITE(7,1076)                                                     UKS01360
1076  FORMAT(T5,'JEDNOPOLNOG KRATKOG SPOJA - VARIJANTA 2',T44,17(1H.),T6UKS01370
     $1,'NE')                                                           UKS01380
      GO TO 200                                                         UKS01390
90    WRITE(7,1077)                                                     UKS01400
1077  FORMAT(T5,'JEDNOPOLNOG KRATKOG SPOJA - VARIJANTA 2',T44,17(1H.),T6UKS01410
     $1,'DA')                                                           UKS01420
200   IF(KS1V3) 202,201,202                                             UKS01430
201   WRITE(7,1090)                                                     UKS01440
1090  FORMAT(T5,'JEDNOPOLNOG KRATKOG SPOJA - VARIJANTA 3',T44,17(1H.),T6UKS01450
     $1,'NE')                                                           UKS01460
      GO TO 203                                                         UKS01470
202   WRITE(7,1091)                                                     UKS01480
1091  FORMAT(T5,'JEDNOPOLNOG KRATKOG SPOJA - VARIJANTA 3',T44,17(1H.),T6UKS01490
     $1,'DA')                                                           UKS01500
203   IF(KS2V2) 205,204,205                                             UKS01510
204   WRITE(7,1092)                                                     UKS01520
1092  FORMAT(T5,'DVOPOLNOG KRATKOG SPOJA - VARIJANTA 2',T42,19(1H.),T61,UKS01530
     $'NE')                                                             UKS01540
      GO TO 91                                                          UKS01550
205   WRITE(7,1093)                                                     UKS01560
1093  FORMAT(T5,'DVOPOLNOG KRATKOG SPOJA - VARIJANTA 2',T42,19(1H.),T61,UKS01570
     $'DA')                                                             UKS01580
91    IF(KS3V1) 1,2,1                                                   UKS01590
1     DO 10006 I=1,20                                                           
      IF(KK3V1(I).EQ.0.AND.NK3V1.EQ.0)THEN                                      
      NK3V1=N                                                                   
      GO TO 10008                                                               
      ENDIF                                                                     
      IF(KK3V1(I).NE.0)NK3V1=I                                                  
10006 CONTINUE                                                                  
10008 IF(NK3V1.EQ.N) GO TO 2                                                    
      WRITE(7,1011) NK3V1                                               UKS01650
1011  FORMAT(//,' BROJ ^VOROVA U KOJIMA SE RA^UNA TROPOLNI K.S. PREMA VAUKS01660
     $R. 1 =',I3)                                                       UKS01670
      DO 11192 I=1,20
      WRITE(UNIT=POMKK(I),FMT=11198)KK3V1(I)
      IF(POMKK(I).EQ.'    0')POMKK(I)=' '
11192 CONTINUE
      WRITE(7,1012) (POMKK(I),I=1,20)                                   UKS01680
1012  FORMAT(/,T20,'^VOROVI U KOJIMA SE RA^UNA TROPOLNI K.S. PREMA VAR. UKS01690
     $1',/(20A5))                                                       UKS01700
2     IF(KS3V2) 30,31,30                                                UKS01710
30    DO 10009 I=1,20                                                           
      IF(KK3V2(I).EQ.0.AND.NK3V2.EQ.0)THEN                                      
      NK3V2=N                                                                   
      GO TO 10010                                                               
      ENDIF                                                                     
      IF(KK3V2(I).NE.0)NK3V2=I                                                  
10009 CONTINUE                                                                  
10010 IF(NK3V2.EQ.N) GO TO 31                                                   
      WRITE(7,1030) NK3V2                                               UKS01750
1030  FORMAT(//,' BROJ ^VOROVA U KOJIMA SE RA^UNA TROPOLNI K.S. PREMA VAUKS01760
     $R. 2 =',I3)                                                       UKS01770
      DO 11193 I=1,20
      WRITE(UNIT=POMKK(I),FMT=11198)KK3V2(I)
      IF(POMKK(I).EQ.'    0')POMKK(I)=' '
11193 CONTINUE
      WRITE(7,1031) (POMKK(I),I=1,20)                                   UKS01780
1031  FORMAT(/,T20,'^VOROVI U KOJIMA SE RA^UNA TROPOLNI K.S. PREMA VAR. UKS01790
     $2',/(20A5))                                                       UKS01800
31    IF(KS1V1) 33,34,33                                                UKS01810
33    DO 10011 I=1,20                                                           
      IF(KK1V1(I).EQ.0.AND.NK1V1.EQ.0)THEN                                      
      NK1V1=N                                                                   
      GO TO 10012                                                               
      ENDIF                                                                     
      IF(KK1V1(I).NE.0)NK1V1=I                                                  
10011 CONTINUE                                                                  
10012 IF(NK1V1.EQ.N) GO TO 34                                                   
      WRITE(7,1032) NK1V1                                               UKS01850
1032  FORMAT(//,' BROJ ^VOROVA U KOJIMA SE RA^UNA JEDNOPOLNI K.S. PREMA UKS01860
     $VAR. 1 =',I3)                                                     UKS01870
      DO 11194 I=1,20
      WRITE(UNIT=POMKK(I),FMT=11198)KK1V1(I)
      IF(POMKK(I).EQ.'    0')POMKK(I)=' '
11194 CONTINUE
      WRITE(7,1033) (POMKK(I),I=1,20)                                   UKS01880
1033  FORMAT(/,T20,'^VOROVI U KOJIMA SE RA^UNA JEDNOPOLNI K.S. PREMA VARUKS01890
     $. 1',/(20A5))                                                     UKS01900
34    IF(KS1V2) 36,37,36                                                UKS01910
36    DO 10013 I=1,20                                                           
      IF(KK1V2(I).EQ.0.AND.NK1V2.EQ.0)THEN                                      
      NK1V2=N                                                                   
      GO TO 10014                                                               
      ENDIF                                                                     
      IF(KK1V2(I).NE.0)NK1V2=I                                                  
10013 CONTINUE                                                                  
10014 IF(NK1V2.EQ.N) GO TO 37                                                   
      WRITE(7,1034) NK1V2                                               UKS01950
1034  FORMAT(//,' BROJ ^VOROVA U KOJIMA SE RA^UNA JEDNOPOLNI K.S. PREMA UKS01960
     $VAR. 2 =',I3)                                                     UKS01970
      DO 11195 I=1,20
      WRITE(UNIT=POMKK(I),FMT=11198)KK1V2(I)
      IF(POMKK(I).EQ.'    0')POMKK(I)=' '
11195 CONTINUE
      WRITE(7,1035) (POMKK(I),I=1,20)                                   UKS01980
1035  FORMAT(/,T20,'^VOROVI U KOJIMA SE RA^UNA JEDNOPOLNI K.S. PREMA VARUKS01990
     $. 2',/(20A5))                                                     UKS02000
37    IF(KS1V3) 206,207,206                                             UKS02010
206   DO 10015 I=1,20                                                           
      IF(KK1V3(I).EQ.0.AND.NK1V3.EQ.0)THEN                                      
      NK1V3=N                                                                   
      GO TO 10016                                                               
      ENDIF                                                                     
      IF(KK1V3(I).NE.0)NK1V3=I                                                  
10015 CONTINUE                                                                  
10016 IF(NK1V3.EQ.N) GO TO 207                                                  
      WRITE(7,1094) NK1V3                                               UKS02050
1094  FORMAT(//,' BROJ ^VOROVA U KOJIMA SE RA^UNA JEDNOPOLNI K.S. PREMA UKS02060
     $VAR. 3 =',I3)                                                     UKS02070
      DO 11196 I=1,20
      WRITE(UNIT=POMKK(I),FMT=11198)KK1V3(I)
      IF(POMKK(I).EQ.'    0')POMKK(I)=' '
11196 CONTINUE
      WRITE(7,1095) (POMKK(I),I=1,20)                                   UKS02080
1095  FORMAT(/,T20,'^VOROVI U KOJIMA SE RA^UNA JEDNOPOLNI K.S. PREMA VARUKS02090
     $. 3',/(20A5))                                                     UKS02100
207   IF(KS2V2) 209,210,209                                             UKS02110
209   DO 10017 I=1,20                                                           
      IF(KK2V2(I).EQ.0.AND.NK2V2.EQ.0)THEN                                      
      NK2V2=N                                                                   
      GO TO 10018                                                               
      ENDIF                                                                     
      IF(KK2V2(I).NE.0)NK2V2=I                                                  
10017 CONTINUE                                                                  
10018 IF(NK2V2.EQ.N) GO TO 210                                                  
      WRITE(7,1096) NK2V2                                               UKS02150
1096  FORMAT(//,' BROJ ^VOROVA U KOJIMA SE RA^UNA DVOPOLNI K.S. PREMA VAUKS02160
     $R. 2 =',I3)                                                       UKS02170
      DO 11197 I=1,20
      WRITE(UNIT=POMKK(I),FMT=11198)KK2V2(I)
11198 FORMAT(I5)
      IF(POMKK(I).EQ.'    0')POMKK(I)=' '
11197 CONTINUE
      WRITE(7,1097) (POMKK(I),I=1,20)                                   UKS02180
1097  FORMAT(/,T20,'^VOROVI U KOJIMA SE RA^UNA DVOPOLNI K.S. PREMA VAR. UKS02190
     $2',/(20A5))                                                       UKS02200
210   CALL JEDBAZ(NG,N,BMAX,SB,UB,S,T,BS,BT,X,UNS,UNT,SN,TR,DELTA,RCR,  UKS02230
     $SCR,TCR,SP,XNUL,XB,IB,XN,B)                                       UKS02240
      IF(IBAZ) 4,5,4                                                    UKS02250
4     WRITE(7,1014) (M,UB(M),XB(M),IB(M),M=1,BMAX)                      UKS02260
1014  FORMAT(1H1,//,T40,'BAZNE VRIJEDNOSTI MRE@E',//,' PODMRE@A:',10X,'BUKS02270
     $AZNI NAPON:',10X,'BAZNA REAKTANCIJA:',10X,'BAZNA STRUJA:',/(I6,15XUKS02280
     $,F9.4,16X,F9.4,16X,F9.4))                                         UKS02290
5     IF(JEDREA) 6,7,6                                                  UKS02300
6     WRITE(7,1015) (IG,XN(IG),IG=1,NG)                                 UKS02310
1015  FORMAT(1H1,//,T40,'JEDINI^NE REAKTANCIJE MRE@E',//,T38,'GRANA:',10UKS02320
     $X,'JEDIN. REAKT.:',/(T40,I3,15X,F9.5))                            UKS02330
7     IF(NAPNCV) 8,9,8                                                  UKS02340
8     WRITE(7,1016) (I,B(I),I=1,N)                                      UKS02350
1016  FORMAT(1H1,//,T40,'NAPONSKI NIVOI PO ^VOROVIMA',//,T42,'^VOR:',5X,UKS02360
     $'NAPONSKI NIVO:',/(T42,I5,12X,I3))                                UKS02370
9     CALL ZAKRET(NG,BMAX,BS,BT,DELTA,DELTAB)                           UKS02380
      IF(KUTZAK) 10,11,10                                               UKS02390
10    WRITE(7,1017) (M,DELTAB(M),M=1,BMAX)                              UKS02400
1017  FORMAT(1H1,//,T40,'KUTOVI ZAKRETANJA',//,T30,'PODMRE@A:',10X,'KUT UKS02410
     $ZAKRETANJA:',/(T35,I3,16X,I4))                                    UKS02420
11    CALL ZVIEK(NG,TR,XN,RCR,S,T,XTR,SCR,TCR)                          UKS02430
      IF(IZVITR) 12,13,12                                               UKS02440
12    WRITE(7,1018) (IG,XTR(IG),IG=1,NG)                                UKS02450
1018  FORMAT(1H1,//,T30,'REAKTANCIJE TRONAMOTNOG TRANSF. U SPOJU - ZVIJEUKS02460
     $ZDA',//,T40,'GRANA:',5X,'JEDIN. REAKTANCIJA:',/(T42,I3,10X,F9.5)) UKS02470
13    CALL MATRIX(NG,S,T,XN,XXX,NNN)                                    UKS02480
      IF(MATDIR) 14,115,14                                              UKS02490
14    WRITE(7,1019) NNN                                                 UKS02500
1019  FORMAT(1H1,//,T40,'MATRICA DIREKTNOG SISTEMA MRE@E',/,T45,'(DONJI UKS02510
     $TROKUT MATRICE)',//,2X,'BROJ REDAKA I STUPACA MATRICE (BROJ ^VOROVUKS02520
     $A MRE@E) = ',I3,//)                                               UKS02530
      NNK=NNN*(NNN-1)/2+NNN                                             UKS02540
      ID=1                                                              UKS02550
      N77=1                                                             UKS02560
      NOO=1                                                             UKS02570
113   WRITE(7,1020) (XXX(I),I=NOO,N77)                                  UKS02580
1020  FORMAT(10(E10.4,2X))                                              UKS02590
      IF(N77-NNK) 114,115,115                                           UKS02600
114   ID=ID+1                                                           UKS02610
      NOO=N77+1                                                         UKS02620
      N77=N77+ID                                                        UKS02630
      GO TO 113                                                         UKS02640
115   CALL TREZV(NG,BMAX,SP,BS,XNUL,T,B,TCR,SCR,IZ)                     UKS02650
      IF(ITREZV) 15,16,15                                               UKS02660
15    WRITE(7,1021) (M,IZ(M),M=1,BMAX)                                  UKS02670
1021  FORMAT(1H1,//,T40,'ISPITIVANJE UZEMLJENJA PODMREZA',//,T45,'PODMREUKS02680
     $ZA:',5X,'UZEMLJENO:',/(T48,I2,12X,I1))                            UKS02690
16    CALL NULTAR(NGNUL,SP,BS,XNUL,BT,S,T,SNUL,TNUL,XB,XNULTA,NG,IZ,XN,TUKS02700
     $CR,SCR,XTR,IE)                                                    UKS02710
      IF(NULTER) 17,18,17                                               UKS02720
17    WRITE(7,1022) NGNUL                                               UKS02730
1022  FORMAT(1H1,//,T40,'PRVI NIZ NULTIH REAKTANCIJA',//,2X,'UKUPNI BROJUKS02740
     $ NULTIH REAKTANCIJA = ',I3)                                       UKS02750
      WRITE(7,1023) (IGNUL,SNUL(IGNUL),TNUL(IGNUL),XNULTA(IGNUL),IE(IGNUUKS02760
     $L),IGNUL=1,NGNUL)                                                 UKS02770
1023  FORMAT(//,20X,'G-0',5X,'S-0',5X,'T-0',8X,'X-0',10X,'GRANA',//(20X,UKS02780
     $I3,5X,I3,5X,I3,5X,F9.5,8X,I3))                                    UKS02790
C     CUVANJE PRVOG NIZA NULTIH REAKTANCIJA                             UKS02800
18    DO 19 IGNUL=1,NGNUL                                               UKS02810
      S1UL(IGNUL)=SNUL(IGNUL)                                           UKS02820
      T1UL(IGNUL)=TNUL(IGNUL)                                           UKS02830
19    X1ULTA(IGNUL)=XNULTA(IGNUL)                                       UKS02840
      CALL SRNULR(NGNUL,S1UL,T1UL,X1ULTA,N1NUL,S1NUL,T1NUL,X1NUL,IPI,ISIUKS02850
     $GMA,INNUL,NULRSR)                                                 UKS02860
      IF(NULRSR) 20,21,20                                               UKS02870
20    WRITE(7,1024) N1NUL,INNUL                                         UKS02880
1024  FORMAT(1H1,//,T28,'SREDJENE NULTE REAKTANCIJE U NUMERACIJI NULTOG UKS02890
     $SISTEMA',//,2X,                                     'BROJ GRANA U UKS02900
     $STABLU NULTOG SISTEMA JE :',I4,/,2X,'UKUPAN BROJ CVOROVA NULTOG SIUKS02910
     $TEMA JE :',I4)                                                    UKS02920
      WRITE(7,1025) (S1NUL(IGNUL),T1NUL(IGNUL),X1NUL(IGNUL),IGNUL=1,N1NUUKS02930
     $L)                                                                UKS02940
1025  FORMAT(//,T40,'S1-0:',5X,'T1-0:',6X,'X1-0:',//(T41,I3,7X,I3,4X,F9.UKS02950
     $5))                                                               UKS02960
21    CALL MATRIX(N1NUL,S1NUL,T1NUL,X1NUL,XXXNUL,NNNNUL)                UKS02970
      IF(MATNUL) 22,118,22                                              UKS02980
22    WRITE(7,1026) NNNNUL                                              UKS02990
1026  FORMAT(1H1,//,T40,'MATRICA NULTOG SISTEMA MREZE',/,T43,'(DONJI TROUKS03000
     $KUT MATRICE)',//,2X,'BROJ REDAKA I STUPACA MATRICE (BROJ CVOROVA NUKS03010
     $ULTE MREZE) = ',I3,//)                                            UKS03020
      NNKNUL=NNNNUL*(NNNNUL-1)/2+NNNNUL                                 UKS03030
      IDNUL=1                                                           UKS03040
      N77NUL=1                                                          UKS03050
      NOONUL=1                                                          UKS03060
116   WRITE(7,1020) (XXXNUL(I),I=NOONUL,N77NUL)                         UKS03070
      IF(N77NUL-NNKNUL) 117,118,118                                     UKS03080
117   IDNUL=IDNUL+1                                                     UKS03090
      NOONUL=N77NUL+1                                                   UKS03100
      N77NUL=N77NUL+IDNUL                                               UKS03110
      GO TO 116                                                         UKS03120
118   IF(KS3V1) 23,24,23                                                UKS03130
23    CALL K3V1(NK3V1,N, XXX,B,IB,AIME,AIME1,ANAP,S,T,XN,BS,BT,NG,UB,   UKS03140
     *KK3V1,TITLE)  
24    IF(KS3V2) 25,26,25                                                UKS03150
25    CALL K3V2(NK3V2,N,KK3V2,XXX,B,IB,AIME,AIME1,ANAP,T,S,TR,XN,SCR,TCRUKS03160
     $,RCR,NG,TITLE)
26    IF(KS1V1) 27,28,27                                                UKS03180
27    CALL  K1V1      (NK1V1,N,KK1V1,XXX,XXXNUL,B,IB,ANAP,DELTAB,       UKS03190
     $BMAX,SP,BT,IZ,S,T,XN,NGNUL,IE,XNULTA,XNUL,BS,TCR,SCR,DELTA,RCR,NG,UKS03200
     $TR,SNUL,TNUL,UB,INNUL,AIME,AIME1,IPI,ISIGMA,TITLE)
28    IF(KS1V2) 29,100,29                                               UKS03220
29    CALL K1V2(NK1V2,N,KK1V2,XXX,XXXNUL,B,IB,T,S,TR,SP,XN,NGNUL,IE,XNULUKS03230
     $TA,XNUL,TCR,SCR,RCR,NG,SNUL,TNUL,INNUL,ANAP,AIME,AIME1,IPI,ISIGMA,
     $TITLE)
100   IF(KS1V3)  101,102,101                                            UKS03250
101   CALL K1V3(NK1V3,N,KK1V3,XXX,XXXNUL,B,IB,TNUL,SNUL,XNULTA,NGNUL,INNUKS03260
     $UL,AIME,AIME1,IPI,ISIGMA,TITLE)
102   IF(KS2V2) 103,104,103                                             UKS03280
103   CALL K2V2(NK2V2,N,KK2V2,XXX,B,IB,T,S,TR,XN,TCR,SCR,RCR,NG,AIME,AIMUKS03290
     $E1,ANAP,TITLE)
104   WRITE(7,1027)                                                     UKS03310
1027  FORMAT(//,T50,'KRAJ PRORACUNA')                                   UKS03320
5555  continue
      END                                                               UKS03340
C     P O T P R O G R A M - RACUNANJE BAZNIH I JEDINICNIH VRIJEDNOSTI   UKS03350
C                                 - J E D B A Z -                       UKS03360
C     ------------------                                                UKS03370
      SUBROUTINE  JEDBAZ(NG, N, BMAX, SB, UB, S, T, BS, BT, X, UNS, UNT,UKS03380
     $                   SN, TR, DELTA,RCR, SCR, TCR, SP, XNUL, XB, IB, UKS03390
     $                   XN, B)                                         UKS03400
      REAL  IB(*)                                                       UKS03410
      INTEGER  BMAX,S(*),T(*),BS(*),BT(*),TR(*),DELTA(*)
      INTEGER  RCR(*),SCR(*),TCR(*),SP(*),B(*)
      DIMENSION  UB(*),UNS(*),UNT(*),SN(*),XNUL(*),XB(*),XN(*),X(*)
      broj=1000000.
      DO 1 IG=1,NG                                                      UKS03460
      IF(SN(IG))1,1,2                                                   UKS03470
2     X(IG)=(X(IG)*UNT(IG)**2)/(100.*SN(IG))                            UKS03480
1     CONTINUE                                                          UKS03490
      M=1                                                               UKS03500
3     XB(M)=UB(M)**2/SB                                                 UKS03510
      if(ub(m).eq.0.)then
        ib(m)=broj
      else
        IB(M)=SB/((SQRT(3.))*UB(M))    
      endif
      IF(M-BMAX)4,5,5                                                   UKS03530
4     M=M + 1                                                           UKS03540
      IG=1                                                              UKS03550
6     IF(BS(IG))8,8,7                                                   UKS03560
7     IF(BS(IG)-M)10,9,10                                               UKS03570
9     IF(BS(IG)-BT(IG))8,8,11                                           UKS03580
11    R=UNS(IG)/UNT(IG)                                                 UKS03590
      IBT=BT(IG)                                                        UKS03600
      UBCR=UB(IBT)                                                      UKS03610
12    UB(M)=UBCR*R                                                      UKS03620
      GO TO 3                                                           UKS03630
8     IF(IG-NG)13,4,4                                                   UKS03640
13    IG=IG + 1                                                         UKS03650
      GO TO 6                                                           UKS03660
10    IF(BT(IG)-M)8,14,8                                                UKS03670
14    IF(BT(IG)-BS(IG))8,8,15                                           UKS03680
15    R=UNT(IG)/UNS(IG)                                                 UKS03690
      IBS=BS(IG)                                                        UKS03700
      IF(IBS.EQ.0)THEN
        UBCR=0
      ELSE
        UBCR=UB(IBS)  
      ENDIF
      GO TO 12                                                          UKS03720
5     IG=1                                                              UKS03730
18    M=BT(IG)                                                          UKS03740
      if(xb(m).eq.0.)then
        x(ig)=broj
      else
        X(IG)=X(IG)/XB(M) 
      endif
      IF(IG-NG)16,17,17                                                 UKS03760
16    IG=IG + 1                                                         UKS03770
      GO TO 18                                                          UKS03780
17    IG=1                                                              UKS03790
19    IF(TR(IG))21,21,20                                                UKS03800
20    ZA=X(IG)                                                          UKS03810
      ZB=X(IG+1)                                                        UKS03820
      ZC=X(IG+2)                                                        UKS03830
22    ZZ=ZA-ZB-ZC                                                       UKS03840
      IF(ABS(ZZ)-1.E-7) 40,40,41                                        UKS03850
40    XN(IG)=1.E7                                                       UKS03860
      GO TO 42                                                          UKS03870
41    XN(IG)=ZA-(ZA**2-(ZB-ZC)**2)/(2.*ZZ)                              UKS03880
      GO TO 42                                                          UKS03890
42    IF(TR(IG)-3) 23, 32, 32                                           UKS03900
23    IG=IG + 1                                                         UKS03910
      IF(TR(IG)-2)24,24,25                                              UKS03920
24    ZA=X(IG)                                                          UKS03930
      ZB=X(IG+1)                                                        UKS03940
      ZC=X(IG-1)                                                        UKS03950
      GO TO 22                                                          UKS03960
25    ZA=X(IG)                                                          UKS03970
      ZB=X(IG-2)                                                        UKS03980
      ZC=X(IG-1)                                                        UKS03990
      GO TO 22                                                          UKS04000
21    XN(IG)=X(IG)                                                      UKS04010
      GO TO 32                                                          UKS04020
32    IF (IG - NG)  33, 26, 26                                          UKS04030
33    IG = IG + 1                                                       UKS04040
      GO TO 19                                                          UKS04050
26    I=1                                                               UKS04060
      IG=1                                                              UKS04070
27    IF(T(IG)-I)29,28,29                                               UKS04080
29    IG=IG+1                                                           UKS04090
      GO TO 27                                                          UKS04100
28    B(I)=BT(IG)                                                       UKS04110
      IF(I-N)30,31,31                                                   UKS04120
30    I=I+1                                                             UKS04130
      GO TO 29                                                          UKS04140
31    RETURN                                                            UKS04150
      END                                                               UKS04160
C     P O T P R O G R A M  -  FORMIRANJE MATRICE                        UKS04170
C                          -  M A T R I X -                             UKS04180
      SUBROUTINE  MATRIX (NG, ISS, ITT, XX, X, N)                       UKS04190
      DIMENSION  ISS(*),ITT(*),XX(*),X(*),B(500)   
      N=0                                                               UKS04210
      IG=1                                                              UKS04220
   33 J=1                                                               UKS04230
      IS=ISS(IG)                                                        UKS04240
      IT=ITT(IG)                                                        UKS04250
      IF(IT-N)23,23,22                                                  UKS04260
  22  IF(IS-0)24,25,24                                                  UKS04270
   25 DO 102 J=1,N                                                      UKS04280
      IF(IT-J)150,151,151                                               UKS04290
  150 IT1=J                                                             UKS04300
      J1=IT                                                             UKS04310
      GO TO 102                                                         UKS04320
151   IT1=IT                                                            UKS04330
      J1=J                                                              UKS04340
      K=IT1*(IT1 - 1)/2+ J1                                             UKS04350
102   X(K)=0.                                                           UKS04360
      K=IT*(IT-1)/2+IT                                                  UKS04370
      X(K)=XX(IG)                                                       UKS04380
      GO TO 103                                                         UKS04390
   24 DO 104 J=1,N                                                      UKS04400
      IF(IT-J)152,153,153                                               UKS04410
  152 IT2=J                                                             UKS04420
      J2=IT                                                             UKS04430
      GO TO 154                                                         UKS04440
  153 IT2=IT                                                            UKS04450
      J2=J                                                              UKS04460
  154 IF(IS-J)155,156,156                                               UKS04470
  155 IS2=J                                                             UKS04480
      J3=IS                                                             UKS04490
      GO TO 157                                                         UKS04500
  156 IS2=IS                                                            UKS04510
      J3=J                                                              UKS04520
  157 K=IT2*(IT2-1)/2+J2                                                UKS04530
      L=IS2*(IS2-1)/2+J3                                                UKS04540
  104 X(K)=X(L)                                                         UKS04550
      K=IT*(IT-1)/2+IT                                                  UKS04560
      L=IS*(IS-1)/2+IS                                                  UKS04570
      X(K)=X(L)+XX(IG)                                                  UKS04580
  103 N=IT                                                              UKS04590
      GO TO 101                                                         UKS04600
  23  IF(IS-0)26,27,26                                                  UKS04610
  26  DO 106 J=1,N                                                      UKS04620
      IF(IS-J)158,159,159                                               UKS04630
  158 IS3=J                                                             UKS04640
      J4=IS                                                             UKS04650
      GO TO 160                                                         UKS04660
  159 IS3=IS                                                            UKS04670
      J4=J                                                              UKS04680
  160 IF(IT-J)161,162,162                                               UKS04690
  161 IT3=J                                                             UKS04700
      J5=IT                                                             UKS04710
      GO TO 163                                                         UKS04720
  162 IT3=IT                                                            UKS04730
      J5=J                                                              UKS04740
  163 K=IS3*(IS3-1)/2+J4                                                UKS04750
      L=IT3*(IT3-1)/2+J5                                                UKS04760
  106 B(J)=X(K)-X(L)                                                    UKS04770
      IF(IS-IT)164,165,165                                              UKS04780
  164 IS4=IT                                                            UKS04790
      IT4=IS                                                            UKS04800
      GO TO 166                                                         UKS04810
  165 IS4=IS                                                            UKS04820
      IT4=IT                                                            UKS04830
  166 M=IS4*(IS4-1)/2+IT4                                               UKS04840
      K=IS*(IS-1)/2+IS                                                  UKS04850
      L=IT*(IT-1)/2+IT                                                  UKS04860
      C=X(K)+X(L)-2*X(M)+XX(IG)                                         UKS04870
      GO TO 107                                                         UKS04880
27    DO 108 J=1,N                                                      UKS04890
      IF(IT-J)167,168,168                                               UKS04900
  167 IT5=J                                                             UKS04910
      J6=IT                                                             UKS04920
      GO TO  169                                                        UKS04930
  168 IT5=IT                                                            UKS04940
      J6=J                                                              UKS04950
  169 K=IT5*(IT5-1)/2+J6                                                UKS04960
  108 B(J)=-X(K)                                                        UKS04970
      K=IT*(IT-1)/2+IT                                                  UKS04980
      C=X(K)+XX(IG)                                                     UKS04990
  107 I=1                                                               UKS05000
   30 DO 109 J=1,N                                                      UKS05010
      IF(I-J)170,171,109                                                UKS05020
  170 I1=J                                                              UKS05030
      J7=I                                                              UKS05040
      GO TO 172                                                         UKS05050
  171 I1=I                                                              UKS05060
      J7=J                                                              UKS05070
  172 K=I1*(I1-1)/2+J7                                                  UKS05080
      X(K)=X(K)-B(I)*B(J)/C       
  109 CONTINUE                                                          UKS05100
      IF(I-N)28,101,101                                                 UKS05110
  28  I=I+1                                                             UKS05120
      GO TO 30                                                          UKS05130
  101 IF(IG-NG)31,32,32                                                 UKS05140
  31  IG=IG+1                                                           UKS05150
      GO TO 33                                                          UKS05160
32    RETURN                                                            UKS05170
      END                                                               UKS05180
C     P O T P R O G R A M - RACUNANJE ZAKRETANJA PREMA MREZI 1          UKS05190
C                                - Z A K R E T -                        UKS05200
      SUBROUTINE  ZAKRET(NG, BMAX, BS, BT, DELTA, DELTAB)               UKS05210
C     ------------------                                                UKS05220
      INTEGER  BMAX,BS(*),BT(*),DELTA(*),DELTAB(*)
      M=1                                                               UKS05240
      DELTAB(M)=0                                                       UKS05250
      M=2                                                               UKS05260
1     IG=1                                                              UKS05270
2     IF(BS(IG))3,3,4                                                   UKS05280
4     IF(BS(IG)-BT(IG))5,3,5                                            UKS05290
5     IF(BS(IG)-M)6,7,6                                                 UKS05300
6     IF(BT(IG)-M)3,8,3                                                 UKS05310
3     IF(IG-NG)9,10,10                                                  UKS05320
9     IG=IG + 1                                                         UKS05330
      GO TO 2                                                           UKS05340
7     IF(BS(IG)-BT(IG))3,3,11                                           UKS05350
11    IND1=BT(IG)                                                       UKS05360
      DELTAB(M)=DELTAB(IND1)+DELTA(IG)                                  UKS05370
      GO TO 10                                                          UKS05380
8     IF(BT(IG)-BS(IG))3,3,12                                           UKS05390
12    IND2=BS(IG)                                                       UKS05400
      IF(IND2.EQ.0)THEN
        DELTAB(M)=DELTA(IG)
      ELSE
        DELTAB(M)=DELTAB(IND2) + DELTA(IG)
      ENDIF
      GO TO 10                                                          UKS05420
10    IF(M-BMAX)13,14,14                                                UKS05430
13    M=M + 1                                                           UKS05440
      GO TO 1                                                           UKS05450
14    RETURN                                                            UKS05460
      END                                                               UKS05470
C     P O T P R O G R A M - UTVRDJIVANJE TRETMANA ZVIJEZDISTA MREZE     UKS05480
C                                 - T R E Z V -                         UKS05490
      SUBROUTINE  TREZV(NG, BMAX, SP, BS, XNUL, T, B, TCR, SCR, IZ)     UKS05500
C     -----------------                                                 UKS05510
      INTEGER  BMAX,SP(*),BS(*),T(*),B(*),TCR(*),SCR(*)
      DIMENSION  XNUL(*),IZ(*)
      DO 1 M=1,BMAX                                                     UKS05540
1     IZ(M)=0                                                           UKS05550
      IG=1                                                              UKS05560
2     IF(SP(IG)) 4,4,3                                                  UKS05570
3     IF(SP(IG)-1) 5,6,5                                                UKS05580
5     IF(SP(IG)-21) 7,6,7                                               UKS05590
7     IF(SP(IG)-11) 8,9,8                                               UKS05600
8     IF(SP(IG)-211) 10,9,10                                            UKS05610
10    IF(SP(IG)-311) 11,9,11                                            UKS05620
11    IF(SP(IG)-210) 12,13,12                                           UKS05630
12    IG=IG + 1                                                         UKS05640
      IF (IG-NG) 2,2,14                                                 UKS05650
4     IF(BS(IG)) 15,15,12                                               UKS05660
15    IF(XNUL(IG)) 16,12,16                                             UKS05670
16    II=T(IG)                                                          UKS05680
      III=B(II)                                                         UKS05690
      IZ(III)=1                                                         UKS05700
      GO TO 12                                                          UKS05710
6     II=TCR(IG)                                                        UKS05720
      III=B(II)                                                         UKS05730
      IZ(III)=1                                                         UKS05740
      GO TO 12                                                          UKS05750
9     II=SCR(IG)                                                        UKS05760
      III=B(II)                                                         UKS05770
      IZ(III)=1                                                         UKS05780
      JJ=TCR(IG)                                                        UKS05790
      JJJ=B(JJ)                                                         UKS05800
      IZ(JJJ)=1                                                         UKS05810
      GO TO 12                                                          UKS05820
13    II=SCR(IG)                                                        UKS05830
      III=B(II)                                                         UKS05840
      IZ(III)=1                                                         UKS05850
      GO TO 12                                                          UKS05860
14    RETURN                                                            UKS05870
      END                                                               UKS05880
C     P O T P R O G R A M  -  NULTE REAKTANCIJE  (PRVI NIZ)             UKS05890
C                                 - N U L T A R -                       UKS05900
      SUBROUTINE  NULTAR(NGNUL, SP, BS, XNUL, BT, S, T, SNUL, TNUL, XB, UKS05910
C     ------------------                                                UKS05920
     $              XNULTA, NG, IZ, XN, TCR, SCR, XTR, IE)              UKS05930
      INTEGER  SP(*),BS(*),BT(*),S(*),T(*),SNUL(*),TNUL(*),
     $IZ(*),TCR(*),SCR(*)
      DIMENSION  XNUL(*),XB(*),XNULTA(*),XN(*),XTR(*),IE(*)
      broj=1000000.
      IGNUL=0                                                           UKS05970
      NGNUL=0                                                           UKS05980
      IG=1                                                              UKS05990
1     IF(SP(IG)) 2,2,3                                                  UKS06000
2     IF(BS(IG)) 4,4,5                                                  UKS06010
4     IF(XNUL(IG)) 7,6,7                                                UKS06020
7     IGNUL=IGNUL + 1                                                   UKS06030
      M=BT(IG)                                                          UKS06040
      SNUL(IGNUL)=S(IG)                                                 UKS06050
      TNUL(IGNUL)=T(IG)                                                 UKS06060
      if(xb(m).eq.0.)then
        xnulta(ignul)=broj
      else
        XNULTA(IGNUL)=XNUL(IG)/XB(M)   
      endif
      IE(IGNUL) = IG                                                    UKS06080
      GO TO 8                                                           UKS06090
8     NGNUL=NGNUL + 1                                                   UKS06100
      GO TO 6                                                           UKS06110
6     IF(IG-NG) 9,10,10                                                 UKS06120
9     IG=IG + 1                                                         UKS06130
      GO TO 1                                                           UKS06140
5     L=BT(IG)                                                          UKS06150
      IF(IZ(L)) 6,6,11                                                  UKS06160
11    IF(XNUL(IG)) 7,12,7                                               UKS06170
12    IGNUL=IGNUL + 1                                                   UKS06180
      SNUL(IGNUL)=S(IG)                                                 UKS06190
      TNUL(IGNUL)=T(IG)                                                 UKS06200
      XNULTA(IGNUL)=3.*XN(IG)                                           UKS06210
      IE(IGNUL) = IG                                                    UKS06220
      GO TO 8                                                           UKS06230
3     IF(SP(IG)-1) 13, 14, 13                                           UKS06240
13    IF(SP(IG)-11) 15,16,15                                            UKS06250
15    IF(SP(IG)-21) 17,18,17                                            UKS06260
17    IF(SP(IG)-210) 19,20,19                                           UKS06270
19    IF(SP(IG)-211) 21,22,21                                           UKS06280
21    IF(SP(IG)-311) 6,22,6                                             UKS06290
14    IGNUL=IGNUL + 1                                                   UKS06300
      SNUL(IGNUL)=0                                                     UKS06310
      TNUL(IGNUL)=TCR(IG)                                               UKS06320
      XNULTA(IGNUL)=10.*XN(IG)                                          UKS06330
      IE(IGNUL) = IG                                                    UKS06340
      GO TO 8                                                           UKS06350
16    IGNUL=IGNUL + 1                                                   UKS06360
      IF(XNUL(IG)) 29,30,29                                             UKS06370
30    SNUL(IGNUL)=S(IG)                                                 UKS06380
      TNUL(IGNUL)=T(IG)                                                 UKS06390
      XNULTA(IGNUL)=XN(IG)                                              UKS06400
      IE(IGNUL) = IG                                                    UKS06410
      GO TO 8                                                           UKS06420
29    SNUL(IGNUL)=S(IG)                                                 UKS06430
      TNUL(IGNUL)=T(IG)                                                 UKS06440
      XNULTA(IGNUL)=XNUL(IG)                                            UKS06450
      IE(IGNUL)=IG                                                      UKS06460
      GO TO 8                                                           UKS06470
18    IGNUL=IGNUL + 1                                                   UKS06480
      SNUL(IGNUL)=0                                                     UKS06490
      TNUL(IGNUL)=TCR(IG)                                               UKS06500
      XNULTA(IGNUL)=XN(IG)                                              UKS06510
      IE(IGNUL) = IG                                                    UKS06520
      GO TO 8                                                           UKS06530
20    IGNUL=IGNUL + 1                                                   UKS06540
      SNUL(IGNUL)=0                                                     UKS06550
      TNUL(IGNUL)=SCR(IG)                                               UKS06560
      XNULTA(IGNUL)=XTR(IG) + XTR(IG+1)                                 UKS06570
      IE(IGNUL) = IG                                                    UKS06580
      IG=IG + 2                                                         UKS06590
      GO TO 8                                                           UKS06600
22    IGNUL=IGNUL + 1                                                   UKS06610
      SNUL(IGNUL)=0                                                     UKS06620
      TNUL(IGNUL)=SCR(IG)                                               UKS06630
      XNULTA(IGNUL)=XTR(IG)+XTR(IG+1)+(XTR(IG)*XTR(IG+1))/XTR(IG+2)     UKS06640
      IE(IGNUL) = IG                                                    UKS06650
      IGNUL=IGNUL + 1                                                   UKS06660
      SNUL(IGNUL)=0                                                     UKS06670
      TNUL(IGNUL)=TCR(IG)                                               UKS06680
      XNULTA(IGNUL)=XTR(IG)+XTR(IG+2)+(XTR(IG)*XTR(IG+2))/XTR(IG+1)     UKS06690
      IE(IGNUL) = IG + 1                                                UKS06700
      IGNUL=IGNUL + 1                                                   UKS06710
      SNUL(IGNUL)=SCR(IG)                                               UKS06720
      TNUL(IGNUL)=TCR(IG)                                               UKS06730
      XNULTA(IGNUL)=XTR(IG+1)+XTR(IG+2)+(XTR(IG+1)*XTR(IG+2))/XTR(IG)   UKS06740
      IE(IGNUL) = IG + 2                                                UKS06750
      NGNUL=NGNUL + 3                                                   UKS06760
      IG=IG + 2                                                         UKS06770
      GO TO 6                                                           UKS06780
10    RETURN                                                            UKS06790
      END                                                               UKS06800
C     P O T P R O G R A M - EKVIVALENTNA ZVIJEZDA TRONAMOTNOG TRANS.    UKS06810
C                                 - Z V I E K -                         UKS06820
      SUBROUTINE  ZVIEK(NG, TR, XN, RCR, S, T, XTR, SCR, TCR)           UKS06830
C     -----------------                                                 UKS06840
      INTEGER  TR(*),RCR(*),S(*),T(*),SCR(*),TCR(*)
      DIMENSION  XN(*),XTR(*)
      IG=1                                                              UKS06870
1     IF(TR(IG)-1) 2,3,2                                                UKS06880
2     IF(IG-NG) 4,5,5                                                   UKS06890
4     IG=IG + 1                                                         UKS06900
      GO TO 1                                                           UKS06910
3     SUMA=0.                                                           UKS06920
6     SUMA=SUMA + XN(IG)                                                UKS06930
      IF(TR(IG)-3) 7,8,8                                                UKS06940
7     IG=IG + 1                                                         UKS06950
      GO TO 6                                                           UKS06960
8     IG=IG - 2                                                         UKS06970
      IA=RCR(IG)                                                        UKS06980
9     IF(IA-S(IG)) 10,11,10                                             UKS06990
10    IF(IA- T(IG))  12,11,12                                           UKS07000
12    XA=XN(IG+1)*XN(IG+2)/SUMA                                         UKS07010
      GO TO 13                                                          UKS07020
11    IG=IG + 1                                                         UKS07030
      IF(IA-S(IG)) 14,15,14                                             UKS07040
14    IF(IA-T(IG)) 16,15,16                                             UKS07050
16    XA=XN(IG+1)*XN(IG-1)/SUMA                                         UKS07060
      IG=IG - 1                                                         UKS07070
      GO TO 13                                                          UKS07080
15    IG=IG - 1                                                         UKS07090
      XA=XN(IG)*XN(IG+1)/SUMA                                           UKS07100
      GO TO 13                                                          UKS07110
13    IF(RCR(IG)-IA) 18,17,18                                           UKS07120
C     XCRT=XTR(IG)                                                      UKS07130
17    XTR(IG) = XA                                                      UKS07140
      IA=SCR(IG)                                                        UKS07150
      GO TO 9                                                           UKS07160
18    IF(SCR(IG)-IA) 20,19,20                                           UKS07170
C     XSCRT=XTR(IG+1)                                                   UKS07180
19    XTR(IG+1)=XA                                                      UKS07190
      IA=TCR(IG)                                                        UKS07200
      GO TO 9                                                           UKS07210
C     XTCRT=XTR(IG+2)                                                   UKS07220
20    XTR(IG+2)=XA                                                      UKS07230
      IG=IG + 2                                                         UKS07240
      GO TO 2                                                           UKS07250
5     RETURN                                                            UKS07260
      END                                                               UKS07270
C     P O T P R O G R A M  -  SREDJENE NULTE REAKTANCIJE- 0 -TO STABLO  UKS07280
C                             -  S R N U L R  -                         UKS07290
      SUBROUTINE SRNULR(NGNUL,S1UL,T1UL,X1ULTA,N1NUL,S1NUL,T1NUL,X1NUL,IUKS07300
     *PI,ISIGMA,INNUL,NULRSR)                                           UKS07310
      INTEGER S1UL(*),T1UL(*),SSNUL(500),TTNUL(500),S1NUL(*),T1NUL(*)
      DIMENSION X1ULTA(*),X1NUL(*),IPI(*),ISIGMA(*)
      II=1                                                              UKS07340
      IPI(1) = 0                                                        UKS07350
      IGC = 0                                                           UKS07360
      NM = 1                                                            UKS07370
1     NI = IPI(II)                                                      UKS07380
      IG = 1                                                            UKS07390
2     IF(T1UL(IG).EQ.0)  GO TO 100                                      UKS07400
      IF(S1UL(IG).EQ.NI) GO TO 3                                        UKS07410
      IF(T1UL(IG).EQ.NI) GO TO 3                                        UKS07420
      GO TO 100                                                         UKS07430
3     IGC = IGC + 1                                                     UKS07440
      SSNUL(IGC) = S1UL(IG)                                             UKS07450
      TTNUL(IGC) = T1UL(IG)                                             UKS07460
      X1NUL(IGC) = X1ULTA(IG)                                           UKS07470
      S1UL(IG) = 0                                                      UKS07480
      T1UL(IG) = 0                                                      UKS07490
      KS = 0                                                            UKS07500
      DO 4 I =1,NM                                                      UKS07510
      IF(SSNUL(IGC).EQ.IPI(I)) GO TO 4                                  UKS07520
      KS = KS +1                                                        UKS07530
4     CONTINUE                                                          UKS07540
      IF(KS.EQ.NM)  GO TO 5                                             UKS07550
      KT = 0                                                            UKS07560
      DO 6 J =1,NM                                                      UKS07570
      IF(TTNUL(IGC).EQ.IPI(J)) GO TO 6                                  UKS07580
      KT = KT + 1                                                       UKS07590
6     CONTINUE                                                          UKS07600
      IF(KT.EQ.NM) GO TO 7                                              UKS07610
      GO TO 100                                                         UKS07620
5     NM = NM + 1                                                       UKS07630
      IPI(NM) = SSNUL(IGC)                                              UKS07640
      GO TO 100                                                         UKS07650
7     NM = NM + 1                                                       UKS07660
      IPI(NM) = TTNUL(IGC)                                              UKS07670
100   IF(IG.LT.NGNUL) GO TO 10                                          UKS07680
      IF(II.LT.NM) GO TO 11                                             UKS07690
      INNUL = NM                                                        UKS07700
      N1NUL = IGC                                                       UKS07710
      DO 12 K= 1,INNUL                                                  UKS07720
12    ISIGMA(K) = K- 1                                                  UKS07730
      GO TO 13                                                          UKS07740
10    IG = IG + 1                                                       UKS07750
      GO TO 2                                                           UKS07760
11    II = II + 1                                                       UKS07770
      GO TO 1                                                           UKS07780
13    I = 1                                                             UKS07790
51    DO 14 J=1,INNUL                                                   UKS07800
      IF(SSNUL(I).EQ.IPI(J))  GO TO 15                                  UKS07810
14    CONTINUE                                                          UKS07820
      GO TO 80                                                          UKS07830
15    S1NUL(I) = ISIGMA(J)                                              UKS07840
      DO 16 K =1,INNUL                                                  UKS07850
      IF(TTNUL(I).EQ.IPI(K)) GO TO 17                                   UKS07860
16    CONTINUE                                                          UKS07870
80    WRITE(7,1000) I,SSNUL(I),TTNUL(I)                                 UKS07880
1000  FORMAT(  //,T10,'*GRESKA U GRANI',I3,'MEDJU CVOROVIMA ',I3,' / ',IUKS07890
     *3)                                                                UKS07900
      GO TO 999                                                         UKS07910
17    T1NUL(I) = ISIGMA(K)                                              UKS07920
      IF(I.LT.N1NUL) GO TO 50                                           UKS07930
      GO TO 99                                                          UKS07940
50    I = I + 1                                                         UKS07950
      GO TO 51                                                          UKS07960
99    DO 53 L= 1,N1NUL                                                  UKS07970
      IF(S1NUL(L).LT.T1NUL(L)) GO TO 53                                 UKS07980
      IT1PR= T1NUL(L)                                                   UKS07990
      T1NUL(L) = S1NUL(L)                                               UKS08000
      S1NUL(L) =IT1PR                                                   UKS08010
53    CONTINUE                                                          UKS08020
      INNUL=INNUL-1                                                     UKS08030
      LL= 1                                                             UKS08040
54    IPI(LL) =IPI(LL+1)                                                UKS08050
      ISIGMA(LL) = ISIGMA(LL+1)                                         UKS08060
      IF(LL.EQ.INNUL) GO TO 55                                          UKS08070
      LL= LL + 1                                                        UKS08080
      GO TO 54                                                          UKS08090
55    IF(NULRSR.EQ.0) GO TO 999                                         UKS08100
      WRITE(7,66)                                                       UKS08110
66    FORMAT(1H1,/,T24,'VEZA MEDJU NUMERACIJOM CVOROVA U DIREKTNOM SISTEUKS08120
     *MU (PI) I U NULTOM SISTEMU (SIGMA)',//,T53,'PI',T74,'SIGMA',/)    UKS08130
      WRITE(7,67) (IPI(I),ISIGMA(I),I=1,INNUL)                          UKS08140
67    FORMAT(50X,I5,15X,I5)                                             UKS08150
999   RETURN                                                            UKS08160
      END                                                               UKS08170
C                                                                       UKS08180
C     POTPROGRAM ZA RACUNANJE JEDNOPOLNOG KRATKOG SPOJA PO VARIJANTI 1  UKS08190
C                                    NOVI                               UKS08200
      SUBROUTINE K1V1 (NK1V1,N,KK1V1,XXX,XXXNUL,B,IB,ANAP,DELTAB,       UKS08210
     $BMAX,SP,BT,IZ,S,T,XN,NGNUL,IE,XNULTA,XNUL,BS,TCR,SCR,DELTA,RCR,NG,UKS08220
     $TR,SNUL,TNUL,UB,INNUL,AIME,AIME1,PIN,SIGMAN,TITLE)
      REAL IB(*)
      INTEGER B(*),DELTAB(*),BMAX,SP(*),BT(*),S(*),T(*),BS(*),
     $TCR(*),SCR(*),DELTA(*),RCR(*),TR(*),SNUL(*),TNUL(*),
     $PIN(*),SIGMAN(*)
      DIMENSION KK1V1(20),XXX(*),XXXNUL(*),ANAP(*),FR1R(200),FR1I(200),
     $FS1R(200),FS1I(200),FT1R(200),FT1I(200),FR(200),
     $FS(200),FT(200),IZ(*),XN(*),IE(*),XNULTA(*),XNUL(*),UB(*)
      CHARACTER*1 AIME(500)*7,AIME1(500)*3,TITLE*71
c     open(unit=11,access='sequential',form='formatted',recl=34,status='
c    *new',file='000.104')
c     open(unit=12,access='sequential',form='formatted',recl=57,status='
c    *new',file='000.105')
      WRITE(7,4000)TITLE
4000  FORMAT(1H1,/////1X,A71,15(/),T55,'P R O R A ^ U N',//,T37,'J E D N
     $ O P O L N O G  K R A T K O G  S P O J A',//,T53,'V A R I J A N T 
     $A   1')
      WRITE(7,4001)                                                     UKS08330
4001  FORMAT(1H1,/,T19,'IZVJE[TAJ O PRORA^UNU STRUJA KRATKOG SPOJA U ^VOUKS08340
     $RI[TIMA MRE@E I U SVIM GRANAMA MRE@E,',/,T44,'KAO I NAPONA U SVIM UKS08350
     $^VORI[TIMA MRE@E',//)                                             UKS08360
      IF (NK1V1 -N)2,1,2                                                UKS08370
1     L1SORT = INNUL -1                                                 UKS08380
C     SORTIRANJE CVOROVA NULTOG SISTEMA U RASTUCEM NIZU NUMERACIJE DIREKUKS08390
C     TNOG ZA PRAVILAN REDOSLJED RACUNANJA INNUL=BROJ CVOROVA NULTOG SISUKS08400
C     TEMA; PIN = NUMERACIJA DIREKTNOG; SIGMAN = NUMERACIJA NULTOG      UKS08410
      DO 555 MSORTN =1,L1SORT                                           UKS08420
      L2SORT = INNUL - MSORTN                                           UKS08430
      DO 554 ISORT = 1,L2SORT                                           UKS08440
      IF(PIN(ISORT).LT.PIN(ISORT+1)) GO TO 554                          UKS08450
      IYSORT = PIN(ISORT)                                               UKS08460
      IXSORT = SIGMAN(ISORT)                                            UKS08470
      PIN(ISORT) = PIN(ISORT+1)                                         UKS08480
      SIGMAN(ISORT) = SIGMAN(ISORT+1)                                   UKS08490
      PIN(ISORT +1) = IYSORT                                            UKS08500
      SIGMAN(ISORT + 1) = IXSORT                                        UKS08510
554   CONTINUE                                                          UKS08520
555   CONTINUE                                                          UKS08530
      IVEZ = 1                                                          UKS08540
3     K = PIN(IVEZ)                                                     UKS08550
      KK = SIGMAN(IVEZ)                                                 UKS08560
      GO TO 333                                                         UKS08570
2     L1 = 1                                                            UKS08580
4     K = KK1V1(L1)                                                     UKS08590
      DO 444 IVEK =1,INNUL                                              UKS08600
      IF(K.EQ.PIN(IVEK)) GO TO 445                                      UKS08610
444   CONTINUE                                                          UKS08620
      WRITE(7,4444) AIME(K),AIME1(K)                                    UKS08630
4444  FORMAT( ' ^VOR : ',A7,A3,' NE OSTAJE U NULTOM SISTEMU')           UKS08640
      GO TO 192                                                         UKS08650
445   KK = SIGMAN(IVEK)                                                 UKS08660
333   J=K*(K-1)/2 +K                                                    UKS08670
      L=KK*(KK-1)/2+KK                                                  UKS08680
      AIKNUL=1./(2.*XXX(J)+XXXNUL(L))                                   UKS08690
      IF(AIKNUL-0.001)6,6,5                                             UKS08700
5     II=B(K)                                                           UKS08710
      AIKNV=AIKNUL*IB(II)                                               UKS08720
      AIK=3.*AIKNV                                                      UKS08730
      SK1=SQRT(3.)*AIK*ANAP(K)                                          UKS08740
      WRITE(7,4002)                                                     UKS08750
4002  FORMAT(T3,'BROJ',T10,'NAZIV',T20,'STRUJA',T32,'SNAGA',/,T2, '^VORAUKS08760
     $',T10,'^VORA',T21,'(KA)',T32,'(MVA)')
      WRITE(7,4003) K,AIME(K),AIME1(K),AIK,SK1                          UKS08780
c      write(11,5000)k,aik
c5000  format(i3,'K1V1',f8.2)
c      write(12,5000)k
4003  FORMAT(I4,T6,'-',T8,A7,A3,T19,F9.3,T29,F12.5,T42,85(1H-))         UKS08790
      WRITE(7,4004)                                                     UKS08800
4004  FORMAT(//,T70,'S T R U J E  G R A N A',//,T45,'OD',T50,           UKS08810
     $'NAZIV',T64,'DO',T69,'NAZIV',T79,'FAZA-R',T89,'FAZA-S',T99,'FAZA-TUKS08820
     $',T111,'IZ',T118,'NAPOMENE:',/,T42,'^VORA',T50,'^VORA',T61,'^VORA'UKS08830
     $,T69,'^VORA',T80,'(KA)',T90,'(KA)',T100,'(KA)',T110,'(KA)')
      IF(DELTAB(II))7,8,7                                               UKS08850
7     IDELTA=DELTAB(II)                                                 UKS08860
      M=1                                                               UKS08870
9     DELTAB(M)=DELTAB(M)-IDELTA                                        UKS08880
      IF(M-BMAX)10,8,8                                                  UKS08890
10    M=M+1                                                             UKS08900
      GO TO 9                                                           UKS08910
8     M=1                                                               UKS08920
11    IF(DELTAB(M)-360)13,12,12                                         UKS08930
12    DELTAB(M)=DELTAB(M)-360                                           UKS08940
      GO TO 11                                                          UKS08950
13    IF(DELTAB(M)+360)14,14,15                                         UKS08960
14    DELTAB(M)=DELTAB(M)+360                                           UKS08970
      GO TO 13                                                          UKS08980
15    IF(DELTAB(M))17,16,17                                             UKS08990
16    FR1R(M)=1.                                                        UKS09000
      FR1I(M)=0.                                                        UKS09010
      FS1R(M)=-0.5                                                      UKS09020
      FS1I(M)=-SQRT(3.)/2.                                              UKS09030
      FT1R(M)=-0.5                                                      UKS09040
      FT1I(M)=SQRT(3.)/2.                                               UKS09050
      FR(M)=2.                                                          UKS09060
      FS(M)=-1.                                                         UKS09070
      FT(M)=-1.                                                         UKS09080
      GO TO 18                                                          UKS09090
17    IF(DELTAB(M)-150) 20,19,20                                        UKS09100
19    FR1R(M)=-SQRT(3.)/2.                                              UKS09110
      FR1I(M)= 0.5                                                      UKS09120
      FS1R(M)=SQRT(3.)/2.                                               UKS09130
      FS1I(M)=0.5                                                       UKS09140
      FT1R(M)=0.                                                        UKS09150
      FT1I(M)=-1.                                                       UKS09160
      FR(M)=-SQRT(3.)                                                   UKS09170
      FS(M)=SQRT(3.)                                                    UKS09180
      FT(M)=0.                                                          UKS09190
      GO TO 18                                                          UKS09200
20    IF(DELTAB(M)+150) 22,21,22                                        UKS09210
21    FR1R(M)=-SQRT(3.)/2.                                              UKS09220
      FR1I(M)=-0.5                                                      UKS09230
      FS1R(M)=0.                                                        UKS09240
      FS1I(M)=1.                                                        UKS09250
      FT1R(M)=SQRT(3.)/2.                                               UKS09260
      FT1I(M)=-0.5                                                      UKS09270
      FR(M)=-SQRT(3.)                                                   UKS09280
      FS(M)=0.                                                          UKS09290
      FT(M)=SQRT(3.)                                                    UKS09300
      GO TO 18                                                          UKS09310
22    PI=3.14159                                                        UKS09320
      FR1R(M)=COS(DELTAB(M)*PI/180.)                                    UKS09330
      FR1I(M)=SIN(DELTAB(M)*PI/180.)                                    UKS09340
      FS1R(M)=COS((DELTAB(M)-120)*PI/180.)                              UKS09350
      FS1I(M)=SIN((DELTAB(M)-120)*PI/180.)                              UKS09360
      FT1R(M)=COS((DELTAB(M)+120)*PI/180.)                              UKS09370
      FT1I(M)=SIN((DELTAB(M)+120)*PI/180.)                              UKS09380
      FR(M)=2.*COS(DELTAB(M)*PI/180.)                                   UKS09390
      FS(M)=-COS(DELTAB(M)*PI/180.)+SQRT(3.)*SIN(DELTAB(M)*PI/180.)     UKS09400
      FT(M)=-COS(DELTAB(M)*PI/180.)-SQRT(3.)*SIN(DELTAB(M)*PI/180.)     UKS09410
18    IF(M-BMAX) 23,24,24                                               UKS09420
23    M=M+1                                                             UKS09430
      GO TO 11                                                          UKS09440
24    IG=1                                                              UKS09450
25    IF(SP(IG)) 27,26,27                                               UKS09460
26    INDX=BT(IG)                                                       UKS09470
      IF(IZ(INDX)) 28,29,28                                             UKS09480
28    IF(S(IG)) 30,31,30                                                UKS09490
30    I1=T(IG)                                                          UKS09500
      I2=S(IG)                                                          UKS09510
      IF(I1-K) 33,32,32                                                 UKS09520
32    IJ=I1*(I1-1)/2+K                                                  UKS09530
      GO TO 34                                                          UKS09540
33    IJ=K*(K-1)/2+I1                                                   UKS09550
34    IF(I2-K) 36,35,35                                                 UKS09560
35    IL=I2*(I2-1)/2+K                                                  UKS09570
      GO TO 37                                                          UKS09580
36    IL=K*(K-1)/2+I2                                                   UKS09590
37    AIST=(XXX(IJ)-XXX(IL))/XN(IG)*AIKNUL                              UKS09600
      DO 711 IZEK = 1,INNUL                                             UKS09610
      IF(I1.EQ.PIN(IZEK)) GO TO 712                                     UKS09620
711   CONTINUE                                                          UKS09630
      GO TO  7000                                                       UKS09640
712   I10 = SIGMAN(IZEK)                                                UKS09650
      DO 721 JZEK =1,INNUL                                              UKS09660
      IF(I2.EQ.PIN(JZEK)) GO TO 722                                     UKS09670
721   CONTINUE                                                          UKS09680
      GO TO 7000                                                        UKS09690
722   I20 = SIGMAN(JZEK)                                                UKS09700
      IF(I10-KK) 39,38,38                                               UKS09710
38    IJJ=I10*(I10-1)/2+KK                                              UKS09720
      GO TO 40                                                          UKS09730
39    IJJ=KK*(KK-1)/2+I10                                               UKS09740
40    IF(I20-KK) 42,41,41                                               UKS09750
41    ILL=I20*(I20-1)/2+KK                                              UKS09760
      GO TO 43                                                          UKS09770
42    ILL=KK*(KK-1)/2+I20                                               UKS09780
43    DO 44 IGNUL=1,NGNUL                                               UKS09790
      IF(IE(IGNUL)-IG) 44,45,44                                         UKS09800
44    CONTINUE                                                          UKS09810
45    XSTNUL=XNULTA(IGNUL)                                              UKS09820
      AISTN=(XXXNUL(IJJ)-XXXNUL(ILL))/XSTNUL*AIKNUL                     UKS09830
      GO TO 46                                                          UKS09840
31    IF(XNUL(IG)) 47,48,47                                             UKS09850
47    I1=T(IG)                                                          UKS09860
      IF(I1-K) 50,49,49                                                 UKS09870
49    IJ=I1*(I1-1)/2+K                                                  UKS09880
      GO TO 51                                                          UKS09890
50    IJ=K*(K-1)/2+I1                                                   UKS09900
51    AIST=XXX(IJ)/XN(IG)*AIKNUL                                        UKS09910
      DO 731 KVEZ = 1,INNUL                                             UKS09920
      IF(I1.EQ.PIN(KVEZ)) GO TO 732                                     UKS09930
731   CONTINUE                                                          UKS09940
      GO TO 7000                                                        UKS09950
732   I10 =SIGMAN(KVEZ)                                                 UKS09960
      IF(I10-KK) 53,52,52                                               UKS09970
52    IJJ=I10*(I10-1)/2+KK                                              UKS09980
      GO TO 54                                                          UKS09990
53    IJJ=KK*(KK-1)/2+I10                                               UKS10000
54    DO 55 IGNUL=1,NGNUL                                               UKS10010
      IF(IE(IGNUL)-IG) 55,56,55                                         UKS10020
55    CONTINUE                                                          UKS10030
56    XSTNUL=XNULTA(IGNUL)                                              UKS10040
      AISTN=XXXNUL(IJJ)/XSTNUL*AIKNUL                                   UKS10050
46    H=1.                                                              UKS10060
      INDX=BT(IG)                                                       UKS10070
      IF(IABS(DELTAB(INDX))-180) 57,58,57                               UKS10080
58    H=-1.                                                             UKS10090
57    AISTR=(AIST*FR(INDX)+H*AISTN)*IB(INDX)                            UKS10100
      AISTS=(AIST*FS(INDX)+H*AISTN)*IB(INDX)                            UKS10110
      AISTT=(AIST*FT(INDX)+H*AISTN)*IB(INDX)                            UKS10120
      I1=T(IG)                                                          UKS10130
      I2=S(IG)                                                          UKS10140
      IF(I2) 197,196,197                                                UKS10150
196   WRITE(7,4005) I2,I1,AIME(I1),AIME1(I1),AISTR,AISTS,AISTT          UKS10160
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i2,i1,aistr,aists,aistt
4005  FORMAT(T42,I3,T46,'-',T48,'SISTEMA',T58,'...',T61,I3,T65,'-',T67, UKS10170
     $A7,A3,T78,F9.3,T88,F9.3,T98,F9.3,T118,'GENERATOR')                UKS10180
      GO TO 198                                                         UKS10190
197   IF(BS(IG)-BT(IG)) 200,199,200                                     UKS10200
199   WRITE(7,4006) I2,AIME(I2),AIME1(I2),I1,AIME(I1),AIME1(I1),AISTR,AIUKS10210
     $STS,AISTT                                                         UKS10220
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i2,i1,aistr,aists,aistt
4006  FORMAT(T42,I3,T46,'-',T48,A7,A3,T58,'...',T61,I3,T65,'-',T67,A7,A3UKS10230
     $,T78,F9.3,T88,F9.3,T98,F9.3,T118,'VOD')                           UKS10240
      GO TO 198                                                         UKS10250
200   WRITE(7,4007) I2,AIME(I2),AIME1(I2),I1,AIME(I1),AIME1(I1),AISTR,AIUKS10260
     $STS,AISTT                                                         UKS10270
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i2,i1,aistr,aists,aistt
4007  FORMAT(T42,I3,T46,'-',T48,A7,A3,T58,'...',T61,I3,T65,'-',T67,A7,A3UKS10280
     $,T78,F9.3,T88,F9.3,T98,F9.3,T118,'TR SP=11')                      UKS10290
198   IF(SP(IG)-11) 60,59,60                                            UKS10300
59    AIZT=AISTR+AISTS+AISTT                                            UKS10310
      WRITE(7,4008) AIZT,I1                                             UKS10320
4008  FORMAT(T108,F9.3,T118,'^VOR',I4)                                  UKS10330
      H=1.                                                              UKS10340
      INDXX=BS(IG)                                                      UKS10350
      IF(IABS(DELTAB(INDXX))-180) 61,62,61                              UKS10360
62    H=-1.                                                             UKS10370
61    AISTR=(AIST*FR(INDXX)+H*AISTN)*IB(INDXX)                          UKS10380
      AISTS=(AIST*FS(INDXX)+H*AISTN)*IB(INDXX)                          UKS10390
      AISTT=(AIST*FT(INDXX)+H*AISTN)*IB(INDXX)                          UKS10400
      WRITE(7,4009) AISTR,AISTS,AISTT                                   UKS10410
4009  FORMAT(T78,F9.3,T88,F9.3,T98,F9.3)                                UKS10420
      AIZS=AISTR+AISTS+AISTT                                            UKS10430
      WRITE(7,4010) AIZS,I2                                             UKS10440
4010  FORMAT(T108,F9.3,T118,'^VOR',I4)                                  UKS10450
      GO TO 60                                                          UKS10460
29    IF(S(IG)) 63,48,63                                                UKS10470
63    I1=T(IG)                                                          UKS10480
      I2=S(IG)                                                          UKS10490
      IF(I1-K) 65,64,64                                                 UKS10500
64    IJ=I1*(I1-1)/2+K                                                  UKS10510
      GO TO 66                                                          UKS10520
65    IJ=K*(K-1)/2+I1                                                   UKS10530
66    IF(I2-K) 68,67,67                                                 UKS10540
67    IL=I2*(I2-1)/2+K                                                  UKS10550
      GO TO 69                                                          UKS10560
68    IL=K*(K-1)/2+I2                                                   UKS10570
69    AIST=(XXX(IJ)-XXX(IL))/XN(IG)*AIKNUL                              UKS10580
      GO TO 70                                                          UKS10590
48    I1=T(IG)                                                          UKS10600
      IF(I1-K) 72,71,71                                                 UKS10610
71    IJ=I1*(I1-1)/2+K                                                  UKS10620
      GO TO 73                                                          UKS10630
72    IJ=K*(K-1)/2+I1                                                   UKS10640
73    AIST=XXX(IJ)/XN(IG)*AIKNUL                                        UKS10650
70    INDX=BT(IG)                                                       UKS10660
      AISTR=AIST*FR(INDX)*IB(INDX)                                      UKS10670
      AISTS=AIST*FS(INDX)*IB(INDX)                                      UKS10680
      AISTT=AIST*FT(INDX)*IB(INDX)                                      UKS10690
      I1=T(IG)                                                          UKS10700
      I2=S(IG)                                                          UKS10710
      IF(I2) 202,201,202                                                UKS10720
201   WRITE(7,4005) I2,I1,AIME(I1),AIME1(I1),AISTR,AISTS,AISTT          UKS10730
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i2,i1,aistr,aists,aistt
      GO TO 203                                                         UKS10740
202   IF(BS(IG)-BT(IG)) 205,204,205                                     UKS10750
204   WRITE(7,4006) I2,AIME(I2),AIME1(I2),I1,AIME(I1),AIME1(I1),AISTR,AIUKS10760
     $STS,AISTT                                                         UKS10770
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i2,i1,aistr,aists,aistt
      GO TO 203                                                         UKS10780
205   WRITE(7,4011)I2,AIME(I2),AIME1(I2),I1,AIME(I1),AIME1(I1),AISTR,AISUKS10790
     $TS,AISTT                                                          UKS10800
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i2,i1,aistr,aists,aistt
4011  FORMAT(T42,I3,T46,'-',T48,A7,A3,T58,'...',T61,I3,T65,'-',T67,A7,A3UKS10810
     $,T78,F9.3,T88,F9.3,T98,F9.3,T118,'TR SP=22')                      UKS10820
203   IF(SP(IG)-22) 60,74,60                                            UKS10830
74    INDXX=BS(IG)                                                      UKS10840
      AISTR=AIST*FR(INDXX)*IB(INDXX)                                    UKS10850
      AISTS=AIST*FS(INDXX)*IB(INDXX)                                    UKS10860
      AISTT=AIST*FT(INDXX)*IB(INDXX)                                    UKS10870
      WRITE(7,4009) AISTR,AISTS,AISTT                                   UKS10880
      GO TO 60                                                          UKS10890
27    IF(SP(IG)-22) 75,63,75                                            UKS10900
75    IF(SP(IG)-11) 76,30,76                                            UKS10910
76    IF(SP(IG)-21) 78,77,78                                            UKS10920
78    IF(SP(IG)-1) 79,77,79                                             UKS10930
77    I1=TCR(IG)                                                        UKS10940
      I2=SCR(IG)                                                        UKS10950
      IF(I1-K) 81,80,80                                                 UKS10960
80    IJ=I1*(I1-1)/2+K                                                  UKS10970
      GO TO 82                                                          UKS10980
81    IJ=K*(K-1)/2+I1                                                   UKS10990
82    IF(I2-K) 84,83,83                                                 UKS11000
83    IL=I2*(I2-1)/2+K                                                  UKS11010
      GO TO 85                                                          UKS11020
84    IL=K*(K-1)/2+I2                                                   UKS11030
85    AISTC=(XXX(IJ)-XXX(IL))/XN(IG)*AIKNUL                             UKS11040
      DO 741 KVEZ = 1,INNUL                                             UKS11050
      IF(I1.EQ.PIN(KVEZ)) GO TO 742                                     UKS11060
741   CONTINUE                                                          UKS11070
      GO TO 7000                                                        UKS11080
742   I10 = SIGMAN(KVEZ)                                                UKS11090
      IF(I10-KK) 87,86,86                                               UKS11100
86    IJJ=I10*(I10-1)/2+KK                                              UKS11110
      GO TO 88                                                          UKS11120
87    IJJ=KK*(KK-1)/2+I10                                               UKS11130
88    DO 89 IGNUL=1,NGNUL                                               UKS11140
      IF(IE(IGNUL)-IG) 89,90,89                                         UKS11150
89    CONTINUE                                                          UKS11160
90    XOTCR=XNULTA(IGNUL)                                               UKS11170
      AIOTC=XXXNUL(IJJ)/XOTCR*AIKNUL                                    UKS11180
      H=1.                                                              UKS11190
      INDX=TCR(IG)                                                      UKS11200
      M=B(INDX)                                                         UKS11210
      IF(IABS(DELTAB(M))-180)91,92,91                                   UKS11220
92    H=-1.                                                             UKS11230
91    AISTCR=(AISTC*FR(M)+H*AIOTC )*IB(M)                               UKS11240
      AISTCS=(AISTC*FS(M)+H*AIOTC)*IB(M)                                UKS11250
      AISTCT=(AISTC*FT(M)+H*AIOTC)*IB(M)                                UKS11260
      IF(SP(IG)-21) 207,206,207                                         UKS11270
206   WRITE(7,4012) I2,AIME(I2),AIME1(I2),I1,AIME(I1),AIME1(I1),AISTCR,AUKS11280
     $ISTCS,AISTCT                                                      UKS11290
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i2,i1,aistcr,aistcs,aist
c    $ct
4012  FORMAT(T42,I3,T46,'-',T48,A7,A3,T58,'...',T61,I3,T65,'-',T67,A7,A3UKS11300
     $,T78,F9.3,T88,F9.3,T98,F9.3,T118,'TR SP=21')                      UKS11310
      GO TO 208                                                         UKS11320
207   WRITE(7,4013) I2,AIME(I2),AIME1(I2),I1,AIME(I1),AIME1(I1),AISTCR,AUKS11330
     $ISTCS,AISTCT                                                      UKS11340
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i2,i1,aistcr,aistcs,aist
c    $ct
4013  FORMAT(T42,I3,T46,'-',T48,A7,A3,T58,'...',T61,I3,T65,'-',T67,A7,A3UKS11350
     $,T78,F9.3,T88,F9.3,T98,F9.3,T118,'TR SP=01')                      UKS11360
208   AIZTCR=AISTCR+AISTCS+AISTCT                                       UKS11370
      WRITE(7,4010) AIZTCR,I1                                           UKS11380
93    IF(SP(IG)-1) 94,95,94                                             UKS11390
94    H=1.                                                              UKS11400
      IF(IABS(DELTA(IG))-150)97,96,97                                   UKS11410
96    H=-1.                                                             UKS11420
97    INDX=TCR(IG)                                                      UKS11430
      M=B(INDX)                                                         UKS11440
      INDXX=SCR(IG)                                                     UKS11450
      MM=B(INDXX)                                                       UKS11460
      AITRR=(H*AISTCR*IB(MM))/(SQRT(3.)*IB(M))                          UKS11470
      AITRS=(H*AISTCS*IB(MM))/(SQRT(3.)*IB(M))                          UKS11480
      AITRT=(H*AISTCT*IB(MM))/(SQRT(3.)*IB(M))                          UKS11490
      WRITE(7,4014) AITRR,AITRS,AITRT                                   UKS11500
4014  FORMAT(T78,F9.3,T88,F9.3,T98,F9.3,T118,'TROKUT TR.')              UKS11510
95    INDX=SCR(IG)                                                      UKS11520
      M=B(INDX)                                                         UKS11530
      AISTCR=AISTC*FR(M)*IB(M)                                          UKS11540
      AISTCS=AISTC*FS(M)*IB(M)                                          UKS11550
      AISTCT=AISTC*FT(M)*IB(M)                                          UKS11560
      WRITE(7,4009) AISTCR,AISTCS,AISTCT                                UKS11570
      GO TO 60                                                          UKS11580
79    IF(SP(IG)-20) 99,98,99                                            UKS11590
98    I1=TCR(IG)                                                        UKS11600
      I2=SCR(IG)                                                        UKS11610
      IF(I1-K) 101,100,100                                              UKS11620
100   IJ=I1*(I1-1)/2+K                                                  UKS11630
      GO TO 102                                                         UKS11640
101   IJ=K*(K-1)/2+I1                                                   UKS11650
102   IF(I2-K) 104,103,103                                              UKS11660
103   IL=I2*(I2-1)/2+K                                                  UKS11670
      GO TO 105                                                         UKS11680
104   IL=K*(K-1)/2+I2                                                   UKS11690
105   AISTC=(XXX(IJ)-XXX(IL))/XN(IG)*AIKNUL                             UKS11700
      INDX=TCR(IG)                                                      UKS11710
      M=B(INDX)                                                         UKS11720
      AISTCR=AISTC*FR(M)*IB(M)                                          UKS11730
      AISTCS=AISTC*FS(M)*IB(M)                                          UKS11740
      AISTCT=AISTC*FT(M)*IB(M)                                          UKS11750
      WRITE(7,4015) I2,AIME(I2),AIME1(I2),I1,AIME(I1),AIME1(I1),AISTCR,AUKS11760
     $ISTCS,AISTCT                                                      UKS11770
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i2,i1,aistcr,aistcs,aist
c    $ct
4015  FORMAT(T42,I3,T46,'-',T48,A7,A3,T58,'...',T61,I3,T65,'-',T67,A7,A3UKS11780
     $,T78,F9.3,T88,F9.3,T98,F9.3,T118,'TR SP=20')                      UKS11790
      GO TO 93                                                          UKS11800
99    I1=TCR(IG)                                                        UKS11810
      I2=SCR(IG)                                                        UKS11820
      I3=RCR(IG)                                                        UKS11830
      IF(I1-K) 107,106,106                                              UKS11840
106   IJ=I1*(I1-1)/2+K                                                  UKS11850
      GO TO 108                                                         UKS11860
107   IJ=K*(K-1)/2+I1                                                   UKS11870
108   IF(I2-K) 110,109,109                                              UKS11880
109   IL=I2*(I2-1)/2+K                                                  UKS11890
      GO TO 111                                                         UKS11900
110   IL=K*(K-1)/2+I2                                                   UKS11910
111   IF(I3-K) 113,112,112                                              UKS11920
112   IM=I3*(I3-1)/2+K                                                  UKS11930
      GO TO 114                                                         UKS11940
113   IM=K*(K-1)/2+I3                                                   UKS11950
114   DO 115 II=1,NG                                                    UKS11960
      IF(TR(II)) 116,115,116                                            UKS11970
116   IF(S(II)-I2) 117,118,117                                          UKS11980
117   IF(S(II)-I1) 115,119,115                                          UKS11990
118   IF(T(II)-I1) 115,120,115                                          UKS12000
119   IF(T(II)-I2) 115,120,115                                          UKS12010
115   CONTINUE                                                          UKS12020
120   XSTCRT=XN(II)                                                     UKS12030
      DO 121 II=1,NG                                                    UKS12040
      IF(TR(II)) 122,121,122                                            UKS12050
122   IF(S(II)-I2) 123,124,123                                          UKS12060
123   IF(S(II)-I3) 121,125,121                                          UKS12070
124   IF(T(II)-I3) 121,126,121                                          UKS12080
125   IF(T(II)-I2) 121,126,121                                          UKS12090
121   CONTINUE                                                          UKS12100
126   XSRCRT=XN(II)                                                     UKS12110
      DO 127 II=1,NG                                                    UKS12120
      IF(TR(II)) 128,127,128                                            UKS12130
128   IF(S(II)-I3) 129,130,129                                          UKS12140
129   IF(S(II)-I1) 127,131,127                                          UKS12150
130   IF(T(II)-I1) 127,132,127                                          UKS12160
131   IF(T(II)-I3) 127,132,127                                          UKS12170
127   CONTINUE                                                          UKS12180
132   XRTCRT=XN(II)                                                     UKS12190
      AISCC=((XXX(IJ)-XXX(IL))/XSTCRT+(XXX(IM)-XXX(IL))/XSRCRT)*AIKNUL  UKS12200
      AITCC=-((XXX(IJ)-XXX(IL))/XSTCRT+(XXX(IJ)-XXX(IM))/XRTCRT)*AIKNUL UKS12210
      AIRCC=-((XXX(IM)-XXX(IL))/XSRCRT-(XXX(IJ)-XXX(IM))/XRTCRT)*AIKNUL UKS12220
      IF(SP(IG)-211) 134,133,134                                        UKS12230
134   IF(SP(IG)-311) 135,133,135                                        UKS12240
133   DO 751 KVEZ = 1,INNUL                                             UKS12250
      IF(I1.EQ.PIN(KVEZ)) GO TO 752                                     UKS12260
751   CONTINUE                                                          UKS12270
      GO TO 7000                                                        UKS12280
752   I10 = SIGMAN(KVEZ)                                                UKS12290
      DO 753 JVEZ =1,INNUL                                              UKS12300
      IF(I2.EQ.PIN(JVEZ)) GO TO 754                                     UKS12310
753   CONTINUE                                                          UKS12320
      GO TO 7000                                                        UKS12330
754   I20 = SIGMAN(JVEZ)                                                UKS12340
      IF(I10-KK) 137,136,136                                            UKS12350
136   IJJ=I10*(I10-1)/2+KK                                              UKS12360
      GO TO 138                                                         UKS12370
137   IJJ=KK*(KK-1)/2+I10                                               UKS12380
138   IF(I20-KK) 140,139,139                                            UKS12390
139   ILL=I20*(I20-1)/2+KK                                              UKS12400
      GO TO 141                                                         UKS12410
140   ILL=KK*(KK-1)/2+I20                                               UKS12420
141   DO 142 IGNUL=1,NGNUL                                              UKS12430
      IF(IE(IGNUL)-IG) 142,143,142                                      UKS12440
142   CONTINUE                                                          UKS12450
143   IF(SNUL(IGNUL)) 145,144,145                                       UKS12460
144   IGNUL=IGNUL+1                                                     UKS12470
      GO TO 143                                                         UKS12480
145   XSTCRN=XNULTA(IGNUL)                                              UKS12490
      DO 146 IGNUL=1,NGNUL                                              UKS12500
      IF(IE(IGNUL)-IG) 146,147,146                                      UKS12510
146   CONTINUE                                                          UKS12520
147   IF(SNUL(IGNUL)) 148,149,148                                       UKS12530
148   IGNUL=IGNUL+1                                                     UKS12540
      GO TO 147                                                         UKS12550
149   IF(TNUL(IGNUL)-I2) 148,150,148                                    UKS12560
150   XOSCRN=XNULTA(IGNUL)                                              UKS12570
      DO 151 IGNUL=1,NGNUL                                              UKS12580
      IF(IE(IGNUL)-IG) 151,152,151                                      UKS12590
151   CONTINUE                                                          UKS12600
152   IF(SNUL(IGNUL)) 153,154,153                                       UKS12610
153   IGNUL=IGNUL+1                                                     UKS12620
      GO TO 152                                                         UKS12630
154   IF(TNUL(IGNUL)-I1) 153,155,153                                    UKS12640
155   XOTCRN=XNULTA(IGNUL)                                              UKS12650
      AISCCN=((XXXNUL(IJJ)-XXXNUL(ILL))/XSTCRN-XXXNUL(ILL)/XOSCRN)*     UKS12660
     $AIKNUL                                                            UKS12670
      AITCCN=-((XXXNUL(IJJ)-XXXNUL(ILL))/XSTCRN+XXXNUL(IJJ)/XOTCRN)*    UKS12680
     $AIKNUL                                                            UKS12690
      GO TO 156                                                         UKS12700
135   IF(SP(IG)-210) 158,157,158                                        UKS12710
157   DO 761 KVEZ =1,INNUL                                              UKS12720
      IF(I2.EQ.PIN(KVEZ)) GO TO 762                                     UKS12730
761   CONTINUE                                                          UKS12740
      GO TO 7000                                                        UKS12750
762   I20 = SIGMAN(KVEZ)                                                UKS12760
      IF(I20-KK) 160,159,159                                            UKS12770
159   ILL=I20*(I20-1)/2+KK                                              UKS12780
      GO TO 161                                                         UKS12790
160   ILL=KK*(KK-1)/2+I20                                               UKS12800
161   DO 162 IGNUL=1,NGNUL                                              UKS12810
      IF(IE(IGNUL)-IG) 162,163,162                                      UKS12820
162   CONTINUE                                                          UKS12830
163   IF(SNUL(IGNUL)) 164,165,164                                       UKS12840
164   IGNUL=IGNUL+1                                                     UKS12850
      GO TO 163                                                         UKS12860
165   IF(TNUL(IGNUL)-I2) 164,166,164                                    UKS12870
166   XOSCRN=XNULTA(IGNUL)                                              UKS12880
      AISCCN=-XXXNUL(ILL)/XOSCRN*AIKNUL                                 UKS12890
      AITCCN=0.                                                         UKS12900
      GO TO 156                                                         UKS12910
158   AISCCN=0.                                                         UKS12920
      AITCCN=0.                                                         UKS12930
156   H=1.                                                              UKS12940
      I1=TCR(IG)                                                        UKS12950
      M=B(I1)                                                           UKS12960
      IF(IABS(DELTAB(M))-180) 167,168,167                               UKS12970
168   H=-1.                                                             UKS12980
167   AITCRR=(AITCC*FR(M)+H*AITCCN)*IB(M)                               UKS12990
      AITCRS=(AITCC*FS(M)+H*AITCCN)*IB(M)                               UKS13000
      AITCRT=(AITCC*FT(M)+H*AITCCN)*IB(M)                               UKS13010
      WRITE(7,4016) I1,AIME(I1),AIME1(I1),AITCRR,AITCRS,AITCRT          UKS13020
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i1,i2,aitcrr,aitcrs,aitc
c    $rt
4016  FORMAT(T42,I3,T46,'-',T48,A7,A3,T78,F9.3,T88,F9.3,T98,F9.3,T118,'TUKS13030
     $RONAM. TR')                                                       UKS13040
      H=1.                                                              UKS13050
      I2=SCR(IG)                                                        UKS13060
      M=B(I2)                                                           UKS13070
      IF(IABS(DELTAB(M))-180) 169,170,169                               UKS13080
170   H=-1.                                                             UKS13090
169   AISCRR=(AISCC*FR(M)+H*AISCCN)*IB(M)                               UKS13100
      AISCRS=(AISCC*FS(M)+H*AISCCN)*IB(M)                               UKS13110
      AISCRT=(AISCC*FT(M)+H*AISCCN)*IB(M)                               UKS13120
      WRITE(7,4016) I2,AIME(I2),AIME1(I2),AISCRR,AISCRS,AISCRT          UKS13130
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i2,i3,aiscrr,aiscrs,aisc
c    $rt
      I3=RCR(IG)                                                        UKS13140
      M=B(I3)                                                           UKS13150
      AIRCRR=AIRCC*FR(M)*IB(M)                                          UKS13160
      AIRCRS=AIRCC*FS(M)*IB(M)                                          UKS13170
      AIRCRT=AIRCC*FT(M)*IB(M)                                          UKS13180
      WRITE(7,4016) I3,AIME(I3),AIME1(I3),AIRCRR,AIRCRS,AIRCRT          UKS13190
c     write(11,fmt='(2(i3,1x),2(f8.2,1x),f8.2)')i3,i1,aircrr,aircrs,airc
c    $rt
      IF(SP(IG)-200) 171,999,171                                        UKS13200
171   IF(SP(IG)-311) 172,173,172                                        UKS13210
172   IF(SP(IG)-300) 174,173,174                                        UKS13220
174   AIZSCR=AISCRR+AISCRS+AISCRT                                       UKS13230
      IF(SP(IG)-211) 210,209,210                                        UKS13240
209   WRITE(7,4017) AIZSCR                                              UKS13250
4017  FORMAT(T108,F9.3,T118,'SP=211')                                   UKS13260
      GO TO 211                                                         UKS13270
210   WRITE(7,4018) AIZSCR                                              UKS13280
4018  FORMAT(T108,F9.3,T118,'SP=210')                                   UKS13290
211   IF(SP(IG)-210) 175,999,175                                        UKS13300
175   AIZTCR=AITCRR+AITCRS+AITCRT                                       UKS13310
      WRITE(7,4019) AIZTCR,I1                                           UKS13320
4019  FORMAT(T108,F9.3,T118,'^VOR',I4)                                  UKS13330
      GO TO 999                                                         UKS13340
173   AIZR=AITCRR+AISCRR                                                UKS13350
      AIZS=AITCRS+AISCRS                                                UKS13360
      AIZT=AITCRT+AISCRT                                                UKS13370
      WRITE(7,4020) AIZR,AIZS,AIZT                                      UKS13380
4020  FORMAT(T78,F9.3,T88,F9.3,T98,F9.3,T118,'SP=311 300')              UKS13390
      IF(SP(IG)-300) 176,999,176                                        UKS13400
176   AIZ=AIZR+AIZS+AIZT                                                UKS13410
      WRITE(7,4021) AIZ                                                 UKS13420
4021  FORMAT(T108,F9.3,T118,'SP=311')                                   UKS13430
999   IG=IG+2                                                           UKS13440
60    IG=IG+1                                                           UKS13450
      IF(IG-NG) 25,25,177                                               UKS13460
177   WRITE(7,4022)                                                     UKS13470
4022  FORMAT(///,T68,'N A P O N I  ^ V O R O V A',//,T43,'BROJ',T51,'NAZUKS13480
     $IV',T67,'F A Z A - R',T89,'F A Z A - S',T111,'F A Z A - T',/,T42, UKS13490
     $'^VORA',T51,'^VORA',T70,'(KV)',T92,'(KV)',T114,'(KV)')
      I=1                                                               UKS13510
178   IF(I-K) 180,179,179                                               UKS13520
179   IJ = I*(I-1)/2+K                                                  UKS13530
      GO TO 181                                                         UKS13540
180   IJ = K*(K-1)/2+I                                                  UKS13550
181   VIF = -XXX(IJ)*AIKNUL                                             UKS13560
      INDX = B(I)                                                       UKS13570
      IF(IZ(INDX)) 182,183,182                                          UKS13580
182   DO 834 KVEZ = 1,INNUL                                             UKS13590
      IF (I.EQ.PIN(KVEZ)) GO TO 835                                     UKS13600
834   CONTINUE                                                          UKS13610
      GO TO 183                                                         UKS13620
835   III = SIGMAN (KVEZ)                                               UKS13630
      IF(III-KK) 185,184,184                                            UKS13640
184   IL = III*(III-1)/2 +KK                                            UKS13650
      GO TO 186                                                         UKS13660
185   IL=KK*(KK-1)/2+III                                                UKS13670
186   VIFNUL=-XXXNUL(IL)*AIKNUL                                         UKS13680
      GO TO 187                                                         UKS13690
183   VIFNUL=0.                                                         UKS13700
187   H=1.                                                              UKS13710
      IF(IABS(DELTAB(INDX))-180) 188,189,188                            UKS13720
189   H=-1.                                                             UKS13730
188   VRREAL=(FR1R(INDX)+VIF*FR(INDX)+H*VIFNUL)*UB(INDX)/SQRT(3.)       UKS13740
      VRIMAG =FR1I(INDX)*UB(INDX)/SQRT(3.)                              UKS13750
      VSREAL=(FS1R(INDX)+VIF*FS(INDX)+H*VIFNUL)*UB(INDX)/SQRT(3.)       UKS13760
      VSIMAG =FS1I(INDX)*UB(INDX)/SQRT(3.)                              UKS13770
      VTREAL=(FT1R(INDX)+VIF*FT(INDX)+H*VIFNUL)*UB(INDX)/SQRT(3.)       UKS13780
      VTIMAG =FT1I(INDX)*UB(INDX)/SQRT(3.)                              UKS13790
      WRITE(7,4023) I,AIME(I),AIME1(I),VRREAL,VRIMAG,VSREAL,VSIMAG,VTREAUKS13800
     $L,VTIMAG                                                          UKS13810
c     write(12,fmt='(i3,6(1x,f8.2))')i,VRREAL,VRIMAG,VSREAL,VSIMAG,VTREA
c    $L,VTIMAG                        
4023  FORMAT(T42,I3,T46,'-',T49,A7,A3,T63,F7.2,T71,'+J',T74,F7.2,       UKS13820
     $T85,F7.2,T93,'+J',T96,F7.2,T107,F7.2,T115,'+J',T118,F7.2)         UKS13830
      IF(I-N) 190,6,6                                                   UKS13840
190   I=I+1                                                             UKS13850
      GO TO 178                                                         UKS13860
6     IF(NK1V1-N) 192,191,192                                           UKS13870
191   IF(IVEZ.EQ.INNUL) GO TO 194                                       UKS13880
193   IVEZ =IVEZ +1                                                     UKS13890
      GO TO 3                                                           UKS13900
7000  WRITE(7,7001)                                                     UKS13910
7001  FORMAT(//,'***GRE[KA NOVA***')                                    UKS13920
      GO TO 194                                                         UKS13930
192   IF(L1-NK1V1) 195,194,194                                          UKS13940
195   L1=L1+1                                                           UKS13950
      GO TO 4                                                           UKS13960
194   RETURN                                                            UKS13970
      END                                                               UKS13980
C                                                                       UKS13990
C     POTPROGRAM ZA RACUNANJE JEDNOPOLNOG K. S. PO VARIJANTI 2          UKS14000
C                                                                       UKS14010
      SUBROUTINE K1V2(NK1V2,N,KK1V2,XXX,XXXNUL,B,IB,T,S,TR,SP,XN,NGNUL,IUKS14020
     *E,XNULTA,XNUL,TCR,SCR,RCR,NG,SNUL,TNUL,INNUL,ANAP,AIME,AIME1,PIN,SUKS14030
     *IGMAN,TITLE)                                                     
      REAL IB(*)
      INTEGER B(*),T(*),S(*),SP(*),TCR(*),SCR(*),RCR(*),SNUL(*),
     $TNUL(*),TR(*),PIN(*),SIGMAN(*),BRED
      DIMENSION KK1V2(20),XXX(*),XXXNUL(*),XN(*),IE(*),XNULTA(*),
     $XNUL(*),ANAP(*)
      CHARACTER*1 AIME(500)*7,AIME1(500)*3,TITLE*71
      WRITE(7,3000)TITLE
3000  FORMAT(1H1,/////1X,A71,15(/),T55,'P R O R A ^ U N',//,T37,'J E D N
     $ O P O L N O G  K R A T K O G  S P O J A',//,T53,'V A R I J A N T 
     $A  2')
      BRED=70
3001  FORMAT(1H1,//,T30,'IZVJE[TAJ O PRORA^UNU STRUJA KRATKOG SPOJA U ^V
     $ORI[TIMA MRE@E,',/,T33,'KAO I U GRANAMA MRE@E KOJE SU SPOJENE NA D
     $OTI^NA ^VORI[TA',//2x,128('-')/2x,'!',5x,'!',11x,'!',9x,'!',12x,'!
     $',30x,'U D I O   I Z   ^ V O R A',30X,'!'/2X,'!',5X,'!',11X,'!',9X
     $,'!',12X,'!',85('-'),'!'/2X,'!BROJ ! N A Z I V ! STRUJA  !   SNAGA 
     $    !',7X,'!',25X,'!',24X,'!',6X,'S T R U J A',9X,'!'/2X,'!^VORA! 
     $^ V O R A !  (KA)   !   (MVA)    ! BROJ  !  N A Z I V   ^ V O R A  
     $ !   T I P   G R A N E    !',26('-'),'!'/2X,'!',5X,'!',11X,'!',9X,
     $'!',12X,'! ^VORA !',25X,'!',24X,'! FAZA-R ! FAZA-S ! FAZA-T !'/2X,
     $'!',5X,'!',11X,'!',9X,'!',12X,'!',7X,'!',25X,'!',24X,'!  (KA)  !  
     $(KA)  !  (KA)  !'/2X,128('-')/)
      IF(NK1V2-N) 2,1,2                                                 UKS14220
1     L1SORT = INNUL -1                                                 UKS14230
C     SORTIRANJE CVOROVA NULTOG SISTEMA U RASTUCEM NIZU NUMERACIJE DIREKUKS14240
C     TNOG ZA PRAVILAN REDOSLJED RACUNANJA INNUL=BROJ CVOROVA NULTOG SISUKS14250
C     TEMA; PIN = NUMERACIJA DIREKTNOG; SIGMAN = NUMERACIJA NULTOG      UKS14260
      DO 555 MSORTN =1,L1SORT                                           UKS14270
      L2SORT = INNUL - MSORTN                                           UKS14280
      DO 554 ISORT = 1,L2SORT                                           UKS14290
      IF(PIN(ISORT).LT.PIN(ISORT+1)) GO TO 554                          UKS14300
      IYSORT = PIN(ISORT)                                               UKS14310
      IXSORT = SIGMAN(ISORT)                                            UKS14320
      PIN(ISORT) = PIN(ISORT+1)                                         UKS14330
      SIGMAN(ISORT) = SIGMAN(ISORT+1)                                   UKS14340
      PIN(ISORT +1) = IYSORT                                            UKS14350
      SIGMAN(ISORT + 1) = IXSORT                                        UKS14360
554   CONTINUE                                                          UKS14370
555   CONTINUE                                                          UKS14380
      IVEZ = 1                                                          UKS14390
3     K = PIN(IVEZ)                                                     UKS14400
      KK = SIGMAN(IVEZ)                                                 UKS14410
      GO TO 533                                                         UKS14420
2     L1 = 1                                                            UKS14430
44    K = KK1V2(L1)                                                     UKS14440
      DO 444 IVEK =1,INNUL                                              UKS14450
      IF(K.EQ.PIN(IVEK)) GO TO 445                                      UKS14460
444   CONTINUE                                                          UKS14470
      if(bred.gt.65)then
        write(7,3001)
        bred=14
      else
        bred=bred+1
      endif
      WRITE(7,4444)AIME(K),AIME1(K)
4444  FORMAT(2X,' ^VOR = ',A7,1X,A3,' NE OSTAJE U NULTOM SISTEMU')
      GO TO 6                                                           UKS14500
445   KK= SIGMAN(IVEK)                                                  UKS14510
533   J= K*(K-1)/2+K                                                    UKS14520
      L=KK*(KK-1)/2+KK                                                  UKS14530
      AIKNUL=1./(2.*XXX(J)+XXXNUL(L))                                   UKS14540
      II=B(K)                                                           UKS14550
      AIKNV=AIKNUL*IB(II)                                               UKS14560
      AIK=3.*AIKNV                                                      UKS14570
      SK1=SQRT(3.)*AIK*ANAP(K)                                          UKS14580
      if(bred.gt.65)then
        write(7,3001)
        bred=14
      else
        bred=bred+1
      endif
      WRITE(7,3002) K,AIME(K),AIME1(K),AIK,SK1                          UKS14590
3002  FORMAT(3x,I4,2x,A7,1x,A3,1x,F9.3,1x,F12.5,1x,85('.'))
      IG=1                                                              UKS14610
306   IF(T(IG)-K) 307,308,307                                           UKS14620
307   IF(S(IG)-K) 309,310,309                                           UKS14630
308   NMALO=S(IG)                                                       UKS14640
      GO TO 311                                                         UKS14650
310   NMALO=T(IG)                                                       UKS14660
311   IF(TR(IG)) 312,313,312                                            UKS14670
312   IF(TR(IG)-1) 315,314,315                                          UKS14680
315   IG=IG-1                                                           UKS14690
      GO TO 314                                                         UKS14700
313   IF(SP(IG)) 317,316,317                                            UKS14710
316   IF(NMALO) 318,319,318                                             UKS14720
318   DO 511 IVEK=1,INNUL                                               UKS14730
      IF(NMALO.EQ.PIN(IVEK)) GO TO 512                                  UKS14740
511   CONTINUE                                                          UKS14750
      GO TO 4                                                           UKS14760
512   NNMALO = SIGMAN(IVEK)                                             UKS14770
      IF(NMALO-K) 321,321,320                                           UKS14780
320   JJ=NMALO*(NMALO-1)/2+K                                            UKS14790
      GO TO 322                                                         UKS14800
321   JJ=K*(K-1)/2+NMALO                                                UKS14810
322   AINK=(XXX(J)-XXX(JJ))/XN(IG)*AIKNUL                               UKS14820
      IF(NNMALO-KK) 324,324,323                                         UKS14830
323   LL=NNMALO*(NNMALO-1)/2+KK                                         UKS14840
      GO TO 325                                                         UKS14850
324   LL=KK*(KK-1)/2+NNMALO                                             UKS14860
325   DO 326 IGNUL=1,NGNUL                                              UKS14870
      IF(IE(IGNUL)-IG) 326,327,326                                      UKS14880
326   CONTINUE                                                          UKS14890
327   AX1=XNULTA(IGNUL)                                                 UKS14900
      AINKN=(XXXNUL(L)-XXXNUL(LL))/AX1*AIKNUL                           UKS14910
      GO TO 328                                                         UKS14920
319   AINK=XXX(J)/XN(IG)*AIKNUL                                         UKS14930
      IF(XNUL(IG)) 330,329,330                                          UKS14940
329   AINKN=0.                                                          UKS14950
      GO TO 328                                                         UKS14960
330   DO 332 IGNUL=1,NGNUL                                              UKS14970
      IF(IE(IGNUL)-IG)332,333,332                                       UKS14980
332   CONTINUE                                                          UKS14990
333   AX1=XNULTA(IGNUL)                                                 UKS15000
      AINKN=XXXNUL(L)/AX1*AIKNUL                                        UKS15010
328   M=B(K)                                                            UKS15020
      AINKR=(2.*AINK+AINKN)*IB(M)                                       UKS15030
      AINKS=(AINKN-AINK)*IB(M)                                          UKS15040
      AINKT=(AINKN-AINK)*IB(M)                                          UKS15050
      if(bred.gt.65)then
        write(7,3001)
        bred=14
      else
        bred=bred+1
      endif
      IF(NMALO) 501,500,501                                             UKS15060
500   WRITE(7,3003)NMALO,AINKR,AINKS,AINKT                              UKS15070
3003  FORMAT(44x,I3,5x,'SISTEMA',19x,'GENERATOR',15X,3(1X,F8.3))
      GO TO 309                                                         UKS15100
501   WRITE(7,3004) NMALO,AIME(NMALO),AIME1(NMALO),AINKR,AINKS,AINKT    UKS15110
3004  FORMAT(44X,I3,5X,A7,1X,A3,15X,'VOD ILI TRANSFORMATOR',3X,3(1X,F8.3
     $))
      GO TO 309                                                         UKS15140
317   IF(SP(IG)-11) 335,318,335                                         UKS15150
335   IF(NMALO-K) 337,337,336                                           UKS15160
336   JJ=NMALO*(NMALO-1)/2+K                                            UKS15170
      GO TO 338                                                         UKS15180
337   JJ=K*(K-1)/2+NMALO                                                UKS15190
338   AINK=(XXX(J)-XXX(JJ))/XN(IG)*AIKNUL                               UKS15200
      IF(SP(IG)-20) 339,340,339                                         UKS15210
339   IF(SP(IG)-22) 341,340,341                                         UKS15220
341   IF(TCR(IG)-K) 340,342,340                                         UKS15230
342   DO 343 IGNUL=1,NGNUL                                              UKS15240
      IF(IE(IGNUL)-IG) 343,344,343                                      UKS15250
343   CONTINUE                                                          UKS15260
344   AX1=XNULTA(IGNUL)                                                 UKS15270
      AIOK=XXXNUL(L)/AX1*AIKNUL                                         UKS15280
      GO TO 345                                                         UKS15290
340   AIOK=0.                                                           UKS15300
345   M=B(K)                                                            UKS15310
      AINKR=(2.*AINK+AIOK)*IB(M)                                        UKS15320
      AINKS=(AIOK-AINK)*IB(M)                                           UKS15330
      AINKT=(AIOK-AINK)*IB(M)                                           UKS15340
      if(bred.gt.65)then
        write(7,3001)
        bred=14
      else
        bred=bred+1
      endif
      WRITE(7,3005) NMALO,AIME(NMALO),AIME1(NMALO),AINKR,AINKS,AINKT    UKS15350
3005  FORMAT(44X,I3,5X,A7,1X,A3,15X,'DVONAMOTNI TRANSFORMATOR',3(1X,F8.3
     $))
      GO TO 309                                                         UKS15380
314   IF(SCR(IG)-K) 347,346,347                                         UKS15390
346   IPPP=TCR(IG)                                                      UKS15400
      IQ=RCR(IG)                                                        UKS15410
      GO TO 348                                                         UKS15420
347   IF(TCR(IG)-K) 350,349,350                                         UKS15430
349   IPPP=SCR(IG)                                                      UKS15440
      IQ=RCR(IG)                                                        UKS15450
      GO TO 348                                                         UKS15460
350   IPPP=SCR(IG)                                                      UKS15470
      IQ=TCR(IG)                                                        UKS15480
348   DO 351 I =1,NG                                                    UKS15490
      IF(TR(I )) 352,351,352                                            UKS15500
352   IF(S(I )-IPPP) 353,354,353                                        UKS15510
353   IF(S(I )-K) 351,355,351                                           UKS15520
354   IF(T(I )-K) 351,356,351                                           UKS15530
355   IF(T(I )-IPPP) 351,356,351                                        UKS15540
351   CONTINUE                                                          UKS15550
356   XPK=XN(I )                                                        UKS15560
      DO 357 I =1,NG                                                    UKS15570
      IF(TR(I )) 358,357,358                                            UKS15580
358   IF(S(I )-IQ) 360,359,360                                          UKS15590
360   IF(S(I )-K) 357,361,357                                           UKS15600
359   IF(T(I )-K) 357,362,357                                           UKS15610
361   IF(T(I )-IQ) 357,362,357                                          UKS15620
357   CONTINUE                                                          UKS15630
362   XQK=XN(I )                                                        UKS15640
      IF(IPPP-K) 364,364,363                                            UKS15650
363   JJ=IPPP*(IPPP-1)/2+K                                              UKS15660
      GO TO 365                                                         UKS15670
364   JJ=K*(K-1)/2+IPPP                                                 UKS15680
365   IF(IQ-K) 367,367,366                                              UKS15690
366   JJJ=IQ*(IQ-1)/2+K                                                 UKS15700
      GO TO 368                                                         UKS15710
367   JJJ=K*(K-1)/2+IQ                                                  UKS15720
368   AIPQK=((XXX(J)-XXX(JJ))/XPK+(XXX(J)-XXX(JJJ))/XQK)*AIKNUL         UKS15730
      IF(SCR(IG)-K) 370,369,370                                         UKS15740
369   IF(SP(IG)-200) 371,372,371                                        UKS15750
371   IF(SP(IG)-300) 373,372,373                                        UKS15760
373   IF(SP(IG)-210) 374,375,374                                        UKS15770
374   DO 376 IGNUL=1,NGNUL                                              UKS15780
      IF(IE(IGNUL)-IG) 376,377,376                                      UKS15790
376   CONTINUE                                                          UKS15800
377   IF(SNUL(IGNUL)) 379,378,379                                       UKS15810
378   IGNUL=IGNUL+1                                                     UKS15820
      GO TO 377                                                         UKS15830
379   XPKNUL=XNULTA(IGNUL)                                              UKS15840
      DO 380 IGNUL=1,NGNUL                                              UKS15850
      IF(IE(IGNUL)-IG) 380,381,380                                      UKS15860
380   CONTINUE                                                          UKS15870
381   IF(SNUL(IGNUL)) 382,383,382                                       UKS15880
382   IGNUL=IGNUL+1                                                     UKS15890
      GO TO 381                                                         UKS15900
383   IF(TNUL(IGNUL)-K) 382,384,382                                     UKS15910
384   XOKNUL=XNULTA(IGNUL)                                              UKS15920
      DO 521 IVEK =1,INNUL                                              UKS15930
      IF(IPPP.EQ.PIN(IVEK)) GO TO 522                                   UKS15940
521   CONTINUE                                                          UKS15950
      GO TO 4                                                           UKS15960
522   IIPP = SIGMAN(IVEK)                                               UKS15970
      IF(IIPP-KK) 386,386,385                                           UKS15980
385   LL=IIPP*(IIPP-1)/2+KK                                             UKS15990
      GO TO 387                                                         UKS16000
386   LL=KK*(KK-1)/2+IIPP                                               UKS16010
387   AIPQKN=((XXXNUL(L)-XXXNUL(LL))/XPKNUL+XXXNUL(L)/XOKNUL)*AIKNUL    UKS16020
      GO TO 388                                                         UKS16030
375   DO 389 IGNUL=1,NGNUL                                              UKS16040
      IF(IE(IGNUL)-IG) 389,390,389                                      UKS16050
389   CONTINUE                                                          UKS16060
390   XOKNUL=XNULTA(IGNUL)                                              UKS16070
      AIPQKN=XXXNUL(L)/XOKNUL*AIKNUL                                    UKS16080
      GO TO 388                                                         UKS16090
370   IF(RCR(IG)-K) 391,372,391                                         UKS16100
391   IF(SP(IG)-211) 392,374,392                                        UKS16110
392   IF(SP(IG)-311) 372,374,372                                        UKS16120
372   AIPQKN=0.                                                         UKS16130
388   M=B(K)                                                            UKS16140
      AIPQKR=(2.*AIPQK+AIPQKN)*IB(M)                                    UKS16150
      AIPQKS=(AIPQKN-AIPQK)*IB(M)                                       UKS16160
      AIPQKT=(AIPQKN-AIPQK)*IB(M)                                       UKS16170
      if(bred.gt.65)then
        write(7,3001)
        bred=14
      else
        bred=bred+1
      endif
      WRITE(7,3006) IPPP,IQ,AIME(IPPP),AIME1(IPPP),AIME(IQ),AIME1(IQ),AIUKS16180
     $PQKR,AIPQKS,AIPQKT                                                UKS16190
3006  FORMAT(44X,I3,'/',I3,1X,A7,1X,A3,' / ',A7,1X,A3,' TRONAMOTNI TRANS
     $FORMATOR',3(1X,F8.3))
      IG=IG+2                                                           UKS16220
309   IG=IG+1                                                           UKS16230
      IF(IG-NG) 306,306,4                                               UKS16240
4     IF(NK1V2-N) 6,5,6                                                 UKS16250
5     IF(IVEZ.EQ.INNUL) GO TO 8                                         UKS16260
      IVEZ = IVEZ +1                                                    UKS16270
      GO TO 3                                                           UKS16280
6     IF(L1-NK1V2) 9,8,8                                                UKS16290
9     L1=L1+1                                                           UKS16300
      GO TO 44                                                          UKS16310
8     RETURN                                                            UKS16320
      END                                                               UKS16330
C                                                                       UKS16340
C     POTPROGRAM ZA RACUNANJE JEDNOPOLNOG KRATKOG SPOJA PO VARIJANTI 3  UKS16350
C                                                                       UKS16360
      SUBROUTINE K1V3(NK1V3,N,KK1V3,XXX,XXXNUL,B,IB,TNUL,SNUL,          UKS16370
     $XNULTA,NGNUL,INNUL,AIME,AIME1,PIN,SIGMAN,TITLE)
      REAL IB(*)
      INTEGER B(*),TNUL(*),SNUL(*),PIN(*),SIGMAN(*),BRED
      DIMENSION KK1V3(20),XXX(*),XXXNUL(*),XNULTA(*)
      CHARACTER*1 AIME(500)*7,AIME1(500)*3,TITLE*71
      WRITE(7,4100)TITLE
4100  FORMAT(1H1,/////1X,A71,15(/),T55,'P R O R A ^ U N ',//,T37,'J E D 
     $N O P O L N O G  K R A T K O G  S P O J A',//,T53,'V A R I J A N T
     $ A  3')
      BRED=70
4101  FORMAT(1H1,/,T25,'IZVJE[TAJ O PRORA^UNU TROSTRUKE NULTE KOMPONENTEUKS16460
     $ STRUJE U ^VORI[TIMA MRE@E,',/,T35,'KAO I U GRANAMA MRE@E KOJE SU UKS16470
     $SPOJENE NA DOTI^NA ^VORI[TA',//2X,80('-')/2X,'!',5X,'!',11X,'!',9X
     $,'!',12X,'U D I O   I Z   ^ V O R A',13X,'!'/2X,'!BROJ ! N A Z I V
     $ ! STRUJA  !',50('-'),'!'/2X,'!^VORA! ^ V O R A !  (KA)   !BROJ ! 
     $N A Z I V !  T I P   G R A N E  !  STRUJA  !'/2X,'!',5X,'!',11X,'!
     $',9X,'!^VORA! ^ V O R A !',21X,'!   (KA)   !'/2X,80('-')/)
      IF(NK1V3-N) 2,1,2                                                 UKS16530
1     L1SORT = INNUL -1                                                 UKS16540
C     SORTIRANJE CVOROVA NULTOG SISTEMA U RASTUCEM NIZU NUMERACIJE DIREKUKS16550
C     TNOG ZA PRAVILAN REDOSLJED RACUNANJA INNUL=BROJ CVOROVA NULTOG SISUKS16560
C     TEMA; PIN = NUMERACIJA DIREKTNOG; SIGMAN = NUMERACIJA NULTOG      UKS16570
      DO 555 MSORTN =1,L1SORT                                           UKS16580
      L2SORT = INNUL - MSORTN                                           UKS16590
      DO 554 ISORT = 1,L2SORT                                           UKS16600
      IF(PIN(ISORT).LT.PIN(ISORT+1)) GO TO 554                          UKS16610
      IYSORT = PIN(ISORT)                                               UKS16620
      IXSORT = SIGMAN(ISORT)                                            UKS16630
      PIN(ISORT) = PIN(ISORT+1)                                         UKS16640
      SIGMAN(ISORT) = SIGMAN(ISORT+1)                                   UKS16650
      PIN(ISORT +1) = IYSORT                                            UKS16660
      SIGMAN(ISORT + 1) = IXSORT                                        UKS16670
554   CONTINUE                                                          UKS16680
555   CONTINUE                                                          UKS16690
      IVEZ = 1                                                          UKS16700
3     K = PIN(IVEZ)                                                     UKS16710
      KK = SIGMAN(IVEZ)                                                 UKS16720
      GO TO 333                                                         UKS16730
2     L1 = 1                                                            UKS16740
4     K = KK1V3(L1)                                                     UKS16750
      DO 444 IVEK =1,INNUL                                              UKS16760
      IF(K.EQ.PIN(IVEK)) GO TO 445                                      UKS16770
444   CONTINUE                                                          UKS16780
      if(bred.gt.65)then
        write(7,4101)
        bred=11
      else
        bred=bred+1
      endif
      WRITE(7,4444) AIME(K),AIME1(K)                                    UKS16790
4444  FORMAT(2X,' ^VOR = ',A7,1X,A3,' NE OSTAJE U NULTOM SISTEMU')
      GO TO 20                                                          UKS16810
445   KK = SIGMAN (IVEK)                                                UKS16820
333   J= K*(K-1)/2+K                                                    UKS16830
      L= KK*(KK-1)/2 +KK                                                UKS16840
      AIKNUL =1./ (2.*XXX(J) +XXXNUL(L))                                UKS16850
      M=B(K)                                                            UKS16860
      AIK=3.*AIKNUL*IB(M)                                               UKS16870
      if(bred.gt.65)then
        write(7,4101)
        bred=11
      else
        bred=bred+1
      endif
      WRITE(7,4102) K,AIME(K),AIME1(K),AIK                              UKS16880
4102  FORMAT(3X,I4,2X,A7,1X,A3,1X,F9.3,1X,51('.'))
      IGNUL=1                                                           UKS16900
5     IF(TNUL(IGNUL)-K) 7,6,7                                           UKS16910
6     NMALO=SNUL(IGNUL)                                                 UKS16920
      GO TO 8                                                           UKS16930
7     IF(SNUL(IGNUL)-K) 10,9,10                                         UKS16940
9     NMALO=TNUL(IGNUL)                                                 UKS16950
8     if(bred.gt.65)then
        write(7,4101)
        bred=11
      else
        bred=bred+1
      endif
      IF(NMALO) 11,12,11                                                UKS16960
11    DO 511 IVEK=1,INNUL                                               UKS16970
      IF(NMALO.EQ.PIN(IVEK)) GO TO 512                                  UKS16980
511   CONTINUE                                                          UKS16990
      GO TO 700                                                         UKS17000
512   NNMALO = SIGMAN(IVEK)                                             UKS17010
      IF(NNMALO-KK) 14,13,13                                            UKS17020
13    LL=NNMALO*(NNMALO-1)/2+KK                                         UKS17030
      GO TO 15                                                          UKS17040
14    LL=KK*(KK-1)/2+NNMALO                                             UKS17050
15    AINKN=(XXXNUL(L)-XXXNUL(LL))/XNULTA(IGNUL)*AIKNUL                 UKS17060
      AINK=3.*AINKN*IB(M)                                               UKS17070
      WRITE(7,4104) NMALO,AIME(NMALO),AIME1(NMALO),AINK
4104  FORMAT(32X,I3,2X,A7,1X,A3,' VOD ILI TRANSFORMATOR',2X,F9.3)
      GO TO 10                                                          UKS17120
12    AINKN=XXXNUL(L)/XNULTA(IGNUL)*AIKNUL                              UKS17130
      AINK=3.*AINKN*IB(M)                                               UKS17140
      WRITE(7,4103) NMALO,AINK
4103  FORMAT(32X,I3,2X,'SISTEMA',5X,'GENERATOR',14X,F9.3)
10    IF(IGNUL-NGNUL) 17,18,18                                          UKS17180
17    IGNUL=IGNUL+1                                                     UKS17190
      GO TO 5                                                           UKS17200
18    IF(NK1V3-N) 20,19,20                                              UKS17210
19    IF(IVEZ.EQ.INNUL)  GO TO 22                                       UKS17220
      IVEZ= IVEZ +1                                                     UKS17230
      GO TO 3                                                           UKS17240
20    IF(L1-NK1V3) 23,22,22                                             UKS17250
23    L1=L1+1                                                           UKS17260
      GO TO 4                                                           UKS17270
700   if(bred.gt.65)then
        write(7,4101)
        bred=11
      else
        bred=bred+2
      endif
      WRITE(7,701)
701   FORMAT(//'***GRESKA NOVA***')
22    RETURN                                                            UKS17300
      END                                                               UKS17310
C                                                                       UKS17320
C     POTPROGRAM ZA RACUNANJE DVOPOLNOG K. S. PO VARIJANTI 2            UKS17330
C                                                                       UKS17340
      SUBROUTINE K2V2(NK2V2,N,KK2V2,XXX,B,IB,T,S,TR,XN,TCR,SCR,RCR,NG,AIUKS17350
     $ME,AIME1,ANAP,TITLE)
      REAL IB(*)
      INTEGER B(*),T(*),S(*),TR(*),TCR(*),SCR(*),RCR(*),bred
      DIMENSION KK2V2(20),XXX(*),XN(*),ANAP(*)
      CHARACTER*1 AIME(500)*7,AIME1(500)*3,TITLE*71
      WRITE(7,3500)TITLE
3500  FORMAT(1H1,/////1X,A71,15(/),T55,'P R O R A ^ U N',//,T39,'D V O P
     $ O L N O G  K R A T K O G  S P O J A',//,T53,'V A R I J A N T A  2
     $')
      bred=70
3501  FORMAT(1H1,//,T30,'IZVJE[TAJ O PRORA^UNU STRUJA KRATKOG SPOJA U ^V
     $ORI[TIMA MRE@E,',/,T33,'KAO I U GRANAMA MRE@E KOJE SU SPOJENE NA D
     $OTI^NA ^VORI[TA',//2x,128('-')/2x,'!',5x,'!',11x,'!',9x,'!',12x,'!
     $',30x,'U D I O   I Z   ^ V O R A',30X,'!'/2X,'!',5X,'!',11X,'!',9X
     $,'!',12X,'!',85('-'),'!'/2X,'!BROJ ! N A Z I V ! STRUJA  !   SNAGA 
     $    !',7X,'!',25X,'!',24X,'!',6X,'S T R U J A',9X,'!'/2X,'!^VORA! 
     $^ V O R A !  (KA)   !   (MVA)    ! BROJ  !  N A Z I V   ^ V O R A  
     $ !   T I P   G R A N E    !',26('-'),'!'/2X,'!',5X,'!',11X,'!',9X,
     $'!',12X,'! ^VORA !',25X,'!',24X,'! FAZA-R ! FAZA-S ! FAZA-T !'/2X,
     $'!',5X,'!',11X,'!',9X,'!',12X,'!',7X,'!',25X,'!',24X,'!  (KA)  !  
     $(KA)  !  (KA)  !'/2X,128('-')/)
      IF(NK2V2-N) 2,1,2                                                 UKS17530
1     K=1                                                               UKS17540
      GO TO 3                                                           UKS17550
2     L=1                                                               UKS17560
4     K=KK2V2(L)                                                        UKS17570
3     J=K*(K-1)/2+K                                                     UKS17580
      AIK=1./(2.*XXX(J))                                                UKS17590
      II=B(K)                                                           UKS17600
      AITS=SQRT(3.)*AIK*IB(II)                                          UKS17610
      SK2=SQRT(3.)*AITS*ANAP(K)                                         UKS17620
      if(bred.gt.65)then
        write(7,3501)
        bred=14
      else
        bred=bred+1
      endif
      WRITE(7,3502) K,AIME(K),AIME1(K),AITS,SK2                         UKS17630
3502  FORMAT(3X,I4,2X,A7,1X,A3,1X,F9.3,1X,F12.5,1X,86('.'))
      IG=1                                                              UKS17650
5     IF(T(IG)-K) 7,6,7                                                 UKS17660
6     NMALO=S(IG)                                                       UKS17670
      GO TO 8                                                           UKS17680
7     IF(S(IG)-K) 10,9,10                                               UKS17690
9     NMALO=T(IG)                                                       UKS17700
8     IF(TR(IG)) 11,12,11                                               UKS17710
11    IF(TR(IG)-1) 13,14,13                                             UKS17720
13    IG=IG-1                                                           UKS17730
      GO TO 14                                                          UKS17740
12    IF(NMALO) 15,16,15                                                UKS17750
15    IF(NMALO-K) 18,18,17                                              UKS17760
17    JJ=NMALO*(NMALO-1)/2+K                                            UKS17770
      GO TO 19                                                          UKS17780
18    JJ=K*(K-1)/2+NMALO                                                UKS17790
19    AINK=(XXX(J)-XXX(JJ))/XN(IG)*AIK                                  UKS17800
      GO TO 20                                                          UKS17810
16    AINK=XXX(J)/XN(IG)*AIK                                            UKS17820
20    AINKR=0.                                                          UKS17830
      AINKS=-SQRT(3.)*AINK*IB(II)                                       UKS17840
      AINKT= SQRT(3.)*AINK*IB(II)                                       UKS17850
      if(bred.gt.65)then
        write(7,3501)
        bred=14
      else
        bred=bred+1
      endif
      IF(NMALO) 100,99,100                                              UKS17860
99    WRITE(7,3503)NMALO,AINKR,AINKS,AINKT                              UKS17870
3503  FORMAT(44X,I3,5X,'SISTEMA',19X,'GENERATOR',15X,3(1X,F8.3))
      GO TO 10                                                          UKS17900
100   WRITE(7,3504) NMALO,AIME(NMALO),AIME1(NMALO),AINKR,AINKS,AINKT    UKS17910
3504  FORMAT(44X,I3,5X,A7,1X,A3,15X,'VOD ILI DVONAMOTNI TRAFO',3(1X,F8.3
     $))
      GO TO 10                                                          UKS17940
14    IF(SCR(IG)-K) 22,21,22                                            UKS17950
21    IPPP=TCR(IG)                                                      UKS17960
      IQ=RCR(IG)                                                        UKS17970
      GO TO 23                                                          UKS17980
22    IF(TCR(IG)-K) 25,24,25                                            UKS17990
24    IPPP=SCR(IG)                                                      UKS18000
      IQ=RCR(IG)                                                        UKS18010
      GO TO 23                                                          UKS18020
25    IPPP=SCR(IG)                                                      UKS18030
      IQ=TCR(IG)                                                        UKS18040
23    DO 26 I=1,NG                                                      UKS18050
      IF(TR(I)) 27,26,27                                                UKS18060
27    IF(S(I)-IPPP) 28,29,28                                            UKS18070
28    IF(S(I)-K) 26,30,26                                               UKS18080
29    IF(T(I)-K) 26,31,26                                               UKS18090
30    IF(T(I)-IPPP) 26,31,26                                            UKS18100
26    CONTINUE                                                          UKS18110
31    XPK=XN(I)                                                         UKS18120
      DO 32 I=1,NG                                                      UKS18130
      IF(TR(I)) 33,32,33                                                UKS18140
33    IF(S(I)-IQ) 34,35,34                                              UKS18150
34    IF(S(I)-K) 32,36,32                                               UKS18160
35    IF(T(I)-K) 32,37,32                                               UKS18170
36    IF(T(I)-IQ) 32,37,32                                              UKS18180
32    CONTINUE                                                          UKS18190
37    XQK=XN(I)                                                         UKS18200
      IF(IPPP-K) 39,39,38                                               UKS18210
38    JJ=IPPP*(IPPP-1)/2+K                                              UKS18220
      GO TO 40                                                          UKS18230
39    JJ=K*(K-1)/2+IPPP                                                 UKS18240
40    IF(IQ-K) 42,42,41                                                 UKS18250
41    JJJ=IQ*(IQ-1)/2+K                                                 UKS18260
      GO TO 43                                                          UKS18270
42    JJJ=K*(K-1)/2+IQ                                                  UKS18280
43    AIPQK=((XXX(J)-XXX(JJ))/XPK+(XXX(J)-XXX(JJJ))/XQK)*AIK            UKS18290
      AIPQKR=0.                                                         UKS18300
      AIPQKS=-SQRT(3.)*AIPQK*IB(II)                                     UKS18310
      AIPQKT= SQRT(3.)*AIPQK*IB(II)                                     UKS18320
      if(bred.gt.65)then
        write(7,3501)
        bred=14
      else
        bred=bred+1
      endif
      WRITE(7,3505) IPPP,IQ,AIME(IPPP),AIME1(IPPP),AIME(IQ),AIME1(IQ),AIUKS18330
     $PQKR,AIPQKS,AIPQKT                                                UKS18340
3505  FORMAT(44X,I3,'/',I3,1X,A7,1X,A3,' / ',A7,1X,A3,' TRONAMOTNI TRAFO
     $',8X,3(1X,F8.3))
      IG=IG+2                                                           UKS18370
10    IG=IG+1                                                           UKS18380
      IF(IG-NG) 5,5,44                                                  UKS18390
44    IF(NK2V2-N) 46,45,46                                              UKS18400
45    IF(K-N) 47,48,48                                                  UKS18410
47    K=K+1                                                             UKS18420
      GO TO 3                                                           UKS18430
46    IF(L-NK2V2) 49,48,48                                              UKS18440
49    L=L+1                                                             UKS18450
      GO TO 4                                                           UKS18460
48    RETURN                                                            UKS18470
      END                                                               UKS18480
C                                                                       UKS18490
C     POTPROGRAM ZA RACUNANJE TROPOLNOG KRATKOG SPOJA PO VARIJANTI 1    UKS18500
C                                                                       UKS18510
      SUBROUTINE K3V1(NK3V1,N,XXX,B,IB,AIME,AIME1,ANAP,S,T,XN,BS,BT,NG,UUKS18520
     $B,KK3V1,TITLE)
      REAL IB(*)
      INTEGER B(*),T(*),S(*),BS(*),BT(*),BRED
      DIMENSION  XXX(*),ANAP(*),XN(*),UB(*),KK3V1(20)
      CHARACTER*1 AIME(500)*7,AIME1(500)*3,TITLE*71,aimep*3,aime1p*3
      aimep=' '
      aime1p=' '
      ipoc=0
      WRITE(7,2500)TITLE
2500  FORMAT(1H1,/////1X,A71,15(/),T55,'P R O R A ^ U N',//,T39,'T R O P
     $ O L N O G  K R A T K O G  S P O J A',//,T53,'V A R I J A N T A  1
     $')
c     open(unit=11,access='sequential',form='formatted',recl=16,status='
c    *new',file='000.102')
c     open(unit=12,access='sequential',form='formatted',recl=12,status='
c    *new',file='000.103')
      BRED=70
2501  FORMAT(1H1,//,T19,'IZVJE[TAJ O PRORA^UNU STRUJA KRATKOG SPOJA U ^VUKS18610
     $ORI[TIMA MRE@E I U SVIM GRANAMA MRE@E,',/,T44,'KAO I NAPONA U SVIMUKS18620
     $ ^VORI[TIMA MRE@E',//2X,115('-')/2X,'!',5X,'!',11X,'!',9X,'!',12X,
     $'!      R A S P O D J E L A   S T R U J A',6X,'!      NAPONI ^VORO
     $VA      !'/2X,'!BROJ ! N A Z I V ! STRUJA  !   SNAGA    !',45('-')
     $,'!',26('-'),'!'/2X,'!^VORA! ^ V O R A !  (KA)   !   (MVA)    !   
     $ OD ^VORA     !    DO ^VORA     !',9X,'!',5X,'!',11X,'!',8X,'!'/2X
     $,'!',5X,'!',11X,'!',9X,'!',12X,'!',17('-'),'!',17('-'),'! STRUJA  
     $!BROJ ! N A Z I V ! NAPON  !'/2X,'!',5X,'!',11X,'!',9X,'!',12X,'!B
     $ROJ ! N A Z I V !BROJ ! N A Z I V !  (KA)   !^VORA! ^ V O R A !  (
     $KV)  !'/2X,115('-')/)
      IF(NK3V1-N) 2,1,2                                                 UKS18700
1     K=1                                                               UKS18710
      GO TO 3                                                           UKS18720
2     L=1                                                               UKS18730
35    K=KK3V1(L)                                                        UKS18740
3     LL=K*(K-1)/2+K                                                    UKS18750
      STJ=1./XXX(LL)                                                    UKS18760
      II=B(K)                                                           UKS18770
      STRCV=STJ*IB(II)                                                  UKS18780
      SK3=SQRT(3.)*STRCV*ANAP(K)                                        UKS18790
      if(bred.gt.65)then
        write(7,2501)
        bred=13
      else
        bred=bred+1
      endif
      WRITE(7,2502) K,AIME(K),AIME1(K),STRCV,SK3                        UKS18800
c      write(11,5000)k,strcv
c5000  format(i3,'K3V1',f8.2)
c      write(12,5000)k
2502  FORMAT(3X,I4,2X,A7,1X,A3,1X,F9.3,1X,F12.5,1X,73('.'))
      IG=1                                                              UKS18820
4     IF(S(IG)) 6,5,6                                                   UKS18830
5     IT=T(IG)                                                          UKS18840
      IF(IT-K) 7,8,8                                                    UKS18850
8     LLL=IT*(IT-1)/2+K                                                 UKS18860
      GO TO 9                                                           UKS18870
7     LLL=K*(K-1)/2+IT                                                  UKS18880
9     AIST=XXX(LLL)/XN(IG)*STJ                                          UKS18890
      GO TO 10                                                          UKS18900
6     IS=S(IG)                                                          UKS18910
      IT=T(IG)                                                          UKS18920
      IF(IS-K) 11,12,12                                                 UKS18930
12    L1=IS*(IS-1)/2+K                                                  UKS18940
      GO TO 13                                                          UKS18950
11    L1=K*(K-1)/2+IS                                                   UKS18960
13    IF(IT-K) 14,15,15                                                 UKS18970
15    L2=IT*(IT-1)/2+K                                                  UKS18980
      GO TO 16                                                          UKS18990
14    L2=K*(K-1)/2+IT                                                   UKS19000
16    AIST=(XXX(L2)-XXX(L1))/XN(IG)*STJ                                 UKS19010
10    IF(BS(IG)) 99,17,99                                               UKS19020
99    IF(BS(IG)-BT(IG)) 18,17,18                                        UKS19030
17    ITT=T(IG)                                                         UKS19040
      III=B(ITT)                                                        UKS19050
      AISTV=AIST*IB(III)                                                UKS19060
      if(bred.gt.65)then
        write(7,2501)
        bred=13
      else
        bred=bred+1
      endif
      IF(BS(IG)) 20,19,20                                               UKS19070
19    IS=S(IG)                                                          UKS19080
      IT=T(IG)                                                          UKS19090
      WRITE(7,2503) IS,IT,AIME(IT),AIME1(IT),AISTV                      UKS19100
      if(ipoc.eq.0)then
        if(isp.eq.is.and.itp.eq.it)then
          if(aime1(it).eq.aime1p.and.aime1(is).eq.aimep)then
c           write(11,fmt='(i3,1x,i3,1x,f8.2)')is,it,aistv
          endif
        else
c         write(11,fmt='(i3,1x,i3,1x,f8.2)')is,it,aistv
        endif
        isp=is
        itp=it
        aime1p=aime(it)
        aimep=aime(is)
      endif
2503  FORMAT(44X,I3,3X,'SISTEMA',5X,I3,3X,A7,1X,A3,1X,F9.3)
      GO TO 21                                                          UKS19130
20    IS=S(IG)                                                          UKS19140
      IT=T(IG)                                                          UKS19150
      WRITE(7,2504) IS,AIME(IS),AIME1(IS),IT,AIME(IT),AIME1(IT),AISTV   UKS19160
      if(ipoc.eq.0)then
        if(isp.eq.is.and.itp.eq.it)then
          if(aime1(it).eq.aime1p.and.aime1(is).eq.aimep)then
c           write(11,fmt='(i3,1x,i3,1x,f8.2)')is,it,aistv
          endif
        else
c         write(11,fmt='(i3,1x,i3,1x,f8.2)')is,it,aistv
        endif
        isp=is
        itp=it
        aime1p=aime(it)
        aimep=aime(is)
      endif
2504  FORMAT(44X,I3,3X,A7,1X,A3,1X,I3,3X,A7,1X,A3,1X,F9.3)
      GO TO 21                                                          UKS19200
18    ISS=S(IG)                                                         UKS19210
      III=B(ISS)                                                        UKS19220
      AISTV=AIST*IB(III)                                                UKS19230
      IS=S(IG)                                                          UKS19240
      IT=T(IG)                                                          UKS19250
      if(bred.gt.65)then
        write(7,2501)
        bred=13
      else
        bred=bred+1
      endif
      WRITE(7,2504) IS,AIME(IS),AIME1(IS),IT,AIME(IT),AIME1(IT),AISTV   UKS19260
      if(ipoc.eq.0)then
        if(isp.eq.is.and.itp.eq.it)then
          if(aime1(it).eq.aime1p.and.aime1(is).eq.aimep)then
c           write(11,fmt='(i3,1x,i3,1x,f8.2)')is,it,aistv
          endif
        else
c         write(11,fmt='(i3,1x,i3,1x,f8.2)')is,it,aistv
        endif
        isp=is
        itp=it
        aime1p=aime(it)
        aimep=aime(is)
      endif
      GO TO 17                                                          UKS19280
21    IF(IG-NG) 22,23,23                                                UKS19290
22    IG=IG+1                                                           UKS19300
      GO TO 4                                                           UKS19310
23    I=1                                                               UKS19320
24    LL=K*(K-1)/2+K                                                    UKS19330
      IF(I-K) 26,25,25                                                  UKS19340
25    L3=I*(I-1)/2+K                                                    UKS19350
      GO TO 27                                                          UKS19360
26    L3=K*(K-1)/2+I                                                    UKS19370
27    VI=(XXX(LL)-XXX(L3))*STJ                                          UKS19380
      I1=B(I)                                                           UKS19390
      UI=VI*UB(I1)                                                      UKS19400
      if(bred.gt.65)then
        write(7,2501)
        bred=13
      else
        bred=bred+1
      endif
      WRITE(7,2506) I,AIME(I),AIME1(I),UI                               UKS19410
      if(ipoc.eq.0)then
c       write(12,fmt='(i3,1x,f8.2)')i,ui
      endif
2506  FORMAT(90X,I3,3X,A7,1X,A3,1X,F8.3)
      IF(I-N) 28,29,29                                                  UKS19430
28    I=I+1                                                             UKS19440
      GO TO 24                                                          UKS19450
29    IF(NK3V1-N) 31,30,31                                              UKS19460
30    IF(K-N) 32,33,33                                                  UKS19470
32    K=K+1                                                             UKS19480
      GO TO 3                                                           UKS19490
31    IF(L-NK3V1) 34,33,33                                              UKS19500
34    L=L+1                                                             UKS19510
      ipoc=10
      GO TO 35                                                          UKS19520
33    continue
      close(unit=11)
      close(unit=12)
      RETURN                                                            UKS19530
      END                                                               UKS19540
C                                                                       UKS19550
C     POTPROGRAM ZA RACUNANJE TROPOLNOG KRATKOG SPOJA PO VARIJANTI 2    UKS19560
C                                                                       UKS19570
      SUBROUTINE K3V2(NK3V2,N,KK3V2,XXX,B,IB,AIME,AIME1,ANAP,T,S,TR,XN,SUKS19580
     $CR,TCR,RCR,NG,TITLE)
      REAL IB(*)
      INTEGER B(*),T(*),S(*),TR(*),SCR(*),TCR(*),RCR(*),bred
      DIMENSION KK3V2(20),XXX(*),ANAP(*),XN(*)
      CHARACTER*1 AIME(500)*7,AIME1(500)*3,TITLE*71
      WRITE(7,2000)TITLE
2000  FORMAT(1H1,/////1X,A71,15(/),T55,'P R O R A ^ U N',//,T39,'T R O P
     $ O L N O G  K R A T K O G  S P O J A',//,T53,'V A R I J A N T A  2
     $')
      bred=70
2001  FORMAT(1H1,//,T30,'IZVJE[TAJ O PRORA^UNU STRUJA KRATKOG SPOJA U ^V
     $ORI[TIMA MRE@E,',/,T33,'KAO I U GRANAMA MRE@E KOJE SU SPOJENE NA D
     $OTI^NA ^VORI[TA',//2x,116('-')/2x,'!',5x,'!',11x,'!',9x,'!',12x,'!
     $',17x,'U D I O   I Z   ^ V O R A',31X,'!'/2X,'!BROJ ! N A Z I V ! 
     $STRUJA  !   SNAGA    !',73('-'),'!'/2X,'!^VORA! ^ V O R A !  (KA)  
     $  !    (MVA)   ! BROJ  !     N A Z I V           !NAZ. !        TI
     $P             ! STRUJA !'/2X,'!',5X,'!',11X,'!',9X,'!',12X,'! ^VOR
     $A !     ^ V O R A           !NAPON!      GRANE             !  (KA)
     $  !'/2X,116('-')/)
      IF(NK3V2-N) 2,1,2  
1     K=1                
      GO TO 3            
2     L=1                
4     K=KK3V2(L)         
3     J=K*(K-1)/2+K      
      STJ=1./XXX(J)      
      M=B(K)             
      STR=STJ*IB(M)      
      SK3=SQRT(3.)*STR*ANAP(K)
      if(bred.gt.65)then
        write(7,2001)
        bred=12
      else
        bred=bred+1
      endif
      WRITE(7,2002) K,AIME(K),AIME1(K),STR,SK3                          UKS19890
2002  FORMAT(3X,I4,2X,A7,1X,A3,1X,F9.3,1X,F12.5,1X,74('.'))
      IG=1                                                              UKS19910
5     IF(T(IG)-K) 7,6,7                                                 UKS19920
6     NMALO=S(IG)                                                       UKS19930
      GO TO 8                                                           UKS19940
7     IF(S(IG)-K) 10,9,10                                               UKS19950
9     NMALO=T(IG)                                                       UKS19960
8     IF(TR(IG)) 11,12,11                                               UKS19970
11    IF(TR(IG)-1) 13,14,13                                             UKS19980
13    IG=IG-1                                                           UKS19990
      GO TO 14                                                          UKS20000
12    IF(NMALO) 15,16,15                                                UKS20010
15    IF(NMALO-K) 18,18,17                                              UKS20020
17    JJ=NMALO*(NMALO-1)/2+K                                            UKS20030
      GO TO 19                                                          UKS20040
18    JJ=K*(K-1)/2+NMALO                                                UKS20050
19    AINK=(XXX(J)-XXX(JJ))/XN(IG)*STJ                                  UKS20060
      GO TO 20                                                          UKS20070
16    AINK=XXX(J)/XN(IG)*STJ                                            UKS20080
20    STRGRA=AINK*IB(M)                                                 UKS20090
      if(bred.gt.65)then
        write(7,2001)
        bred=12
      else
        bred=bred+1
      endif
      IF(NMALO) 21,22,21                                                UKS20100
21    WRITE(7,2003) NMALO,AIME(NMALO),AIME1(NMALO),ANAP(NMALO),STRGRA   UKS20110
2003  FORMAT(44X,I3,5X,A7,1X,A3,15X,F5.1,' VOD ILI DVONAMOTNI TRAFO',1X,
     $F8.3)
      GO TO 10                                                          UKS20130
22    WRITE(7,2004) NMALO,STRGRA                                        UKS20140
2004  FORMAT(44X,I3,5X,'SISTEMA',25X,'GENERATOR',16X,F8.3)
      GO TO 10                                                          UKS20160
14    IF(SCR(IG)-K) 24,23,24                                            UKS20170
23    IPPP=TCR(IG)                                                      UKS20180
      IQ=RCR(IG)                                                        UKS20190
      GO TO 25                                                          UKS20200
24    IF(TCR(IG)-K) 27,26,27                                            UKS20210
26    IPPP=SCR(IG)                                                      UKS20220
      IQ=RCR(IG)                                                        UKS20230
      GO TO 25                                                          UKS20240
27    IPPP=SCR(IG)                                                      UKS20250
      IQ=TCR(IG)                                                        UKS20260
25    DO 28 I=1,NG                                                      UKS20270
      IF(TR(I)) 29,28,29                                                UKS20280
29    IF(S(I)-IPPP) 30,31,30                                            UKS20290
30    IF(S(I)-K) 28,32,28                                               UKS20300
31    IF(T(I)-K) 28,33,28                                               UKS20310
32    IF(T(I )-IPPP) 28,33,28                                           UKS20320
28    CONTINUE                                                          UKS20330
33    XPK=XN(I)                                                         UKS20340
      DO 34 I=1,NG                                                      UKS20350
      IF(TR(I)) 35,34,35                                                UKS20360
35    IF(S(I)-IQ) 36,37,36                                              UKS20370
36    IF(S(I)-K) 34,38,34                                               UKS20380
37    IF(T(I)-K) 34,39,34                                               UKS20390
38    IF(T(I)-IQ) 34,39,34                                              UKS20400
34    CONTINUE                                                          UKS20410
39    XQK=XN(I)                                                         UKS20420
      IF(IPPP-K) 41,41,40                                               UKS20430
40    JJ=IPPP*(IPPP-1)/2+K                                              UKS20440
      GO TO 42                                                          UKS20450
41    JJ=K*(K-1)/2+IPPP                                                 UKS20460
42    IF(IQ-K) 44,44,43                                                 UKS20470
43    JJJ=IQ*(IQ-1)/2+K                                                 UKS20480
      GO TO 45                                                          UKS20490
44    JJJ=K*(K-1)/2+IQ                                                  UKS20500
45    AIPQK=((XXX(J)-XXX(JJ))/XPK+(XXX(J)-XXX(JJJ))/XQK)*STJ            UKS20510
      AIPQKV=AIPQK*IB(M)                                                UKS20520
      if(bred.gt.65)then
        write(7,2001)
        bred=12
      else
        bred=bred+1
      endif
      WRITE(7,2005) IPPP,IQ,AIME(IPPP),AIME1(IPPP),AIME(IQ),AIME1(IQ),AIUKS20530
     *PQKV                                                              UKS20540
2005  FORMAT(44X,I3,'/',I3,1X,A7,1X,A3,' / ',A7,1X,A3,7X,'TRONAMOTNI TRA
     $FO',9X,F8.3)
      IG=IG+2                                                           UKS20560
10    IG=IG+1                                                           UKS20570
      IF(IG-NG) 5,5,46                                                  UKS20580
46    IF(NK3V2-N) 48,47,48                                              UKS20590
47    IF(K-N) 49,50,50                                                  UKS20600
49    K=K+1                                                             UKS20610
      GO TO 3                                                           UKS20620
48    IF(L-NK3V2) 51,50,50                                              UKS20630
51    L=L+1                                                             UKS20640
      GO TO 4                                                           UKS20650
50    RETURN                                                            UKS20660
      END                                                               UKS20670
