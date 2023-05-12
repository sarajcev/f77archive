C     Glavni program  R E Z A - 1
C     ---------------------------
      REAL Ib(20)
      INTEGER Tg(50),BTg(50),     Seq(30),Teq(30),BSeq(30),BTeq(30)
      INTEGER S2t(60),T2t(60),BS2t(60),BT2t(60),GSS2t(60),GST2t(60)
      INTEGER Sv(100),Tv(100),BSv(100),BTv(100),     Tt(40),BTt(40)
      INTEGER S3t(70),T3t(70),BS3t(70),BT3t(70),GSS3t(70),GST3t(70),
     *        SS3t(70)
      DIMENSION Un(20),Zb(20),   xd(50),Sgn(50),   Bv(100),Bvo(100)
      DIMENSION uk(60),S2tn(60),RS2t(60),RT2t(60)
      DIMENSION ukst(70),uks3(70),ukt3(70),Snst(70),Sns3(70),
     *          RS3t(70),RT3t(70)
      COMPLEX Zg(50),   Zeq(30),Zeqo(30),   Zv(100),Zvo(100)
      COMPLEX Z2t(60),Z2tso(60),Z2tsto(60),Z2tto(60)
      COMPLEX Z3t(70),Z3tso(70),Z3tsto(70),Z3tto(70),  Zt(40),Zto(40)
      COMPLEX Z(100,100),Zo(100,100),  Iko,Ik1
      COMPLEX Ids(100),Idt(100),Vds(100),Vdt(100),
     *        Iit(100),Vit(100),
     *        Ios(100),Iot(100),Vos(100),Vot(100)
      COMPLEX Ibs,Ibt,Izs,Izt,    Ius,Iut,   Zms,Zmt,  S,c,a,a2,
     *        Vbs,Vbt,Vzs1,Vzs2,Vzt1,Vzt2
      open(5,file='reza.dat')
      open(6,file='reza.prn')
c-----Kompleksne konstante
      a=cmplx(-0.5,0.866)
      a2=cmplx(-0.5,-0.866)
C++++ Ulazni podaci
      read(5,100) Nnr,Ng,Neq,N2t,Nv,N3t,Nt,Nc
100   format(16i5)
      write(6,101) Nnr,Ng,Neq,N2t,Nv,N3t,Nt,Nc
101   format(' Program: R E Z A (V1.0)',/,' Autor: Dr.sc. Ivan Sarajcev'
     *,//,t30,'U L A Z N I  P O D A C I',//,' Opci podaci mreze:',/,
     *' ------------------',//,' Broj naponskih razina:',i5,/,
     *' Broj generatora:',i5,/,' Broj nadomjesnih elemenata:',i5,/,
     *' Broj dvonamotnih transformatora:',i5,/,' Broj vodova:',i5,/,
     *' Broj tronamotnih transformatora:',i5,/,' Broj trosila:',i5,/,
     *' Broj cvorista:',i5,//,' NAPONSKE RAZINE:',/,' Broj-B',t10,
     *'U [kV]')
      read(5,102) (Un(i),i=1,Nnr)
102   format(8f10.3)
      do 1 i=1,Nnr
      write(6,103) i,Un(i)
103   format(i6,t10,f6.2)
1     continue
      if(Ng.eq.0) go to 2
      write(6,104)
104   format(/,' GENERATORI:',/,' Gen.',t10,'Cvor-t',t20,'B-t',t27,
     *'xd [%]',t37,'Sn [MVA]')
      do 3 i=1,Ng
      read(5,105) Tg(i),BTg(i),xd(i),Sgn(i)
105   format(2i5,7f10.3)
      write(6,106) i,Tg(i),BTg(i),xd(i),Sgn(i)
106   format(i4,t10,i4,t20,i2,t27,f5.1,t37,f7.2)
3     continue
2     if(Neq.eq.0) go to 4
      write(6,107)
107   format(/,' NADOMJESNI ELEMENTI:',/,' Elem.',t10,'Cvor-s',t20,
     *'Cvor-t',t30,'B-s',t37,'B-t',t44,' R + j X [Ohm]',t62,
     *'Ro + j Xo [Ohm]')
      do 5 i=1,Neq
      read(5,108) Seq(i),Teq(i),BSeq(i),BTeq(i),r,x,ro,xo
108   format(4i5,6f10.3)
      Zeq(i)=cmplx(r,x)
      Zeqo(i)=cmplx(ro,xo)
      write(6,109) i,Seq(i),Teq(i),BSeq(i),BTeq(i),Zeq(i),Zeqo(i)
109   format(i4,t10,i4,t20,i4,t30,i2,t37,i2,t44,2f7.2,t62,2f8.2)
5     continue
4     if(N2t.eq.0) go to 6
      write(6,110)
110   format(/,' DVONAMOTNI TRANSFORMATORI:',/,' Tr.',t8,'Cvor-s',t15,
     *'Cvor-t',t23,'B-s',t28,'B-t',t33,'GS-s',t39,'GS-t',t45,'uk [%]',
     *t52,'Sn [MVA]',t62,'R-s [Ohm]',t73,'R-t [Ohm]')
      do 7 i=1,N2t
      read(5,111) S2t(i),T2t(i),BS2t(i),BT2t(i),GSS2t(i),GST2t(i),uk(i),
     *S2tn(i),RS2t(i),RT2t(i)
111   format(6i5,5f10.3)
      write(6,112) i,S2t(i),T2t(i),BS2t(i),BT2t(i),GSS2t(i),GST2t(i),
     *uk(i),S2tn(i),RS2t(i),RT2t(i)
112   format(i3,t8,i4,t15,i4,t23,i2,t28,i2,t33,i3,t39,i3,t45,f6.2,t52,
     *f8.2,t62,f7.1,t73,f7.1)
7     continue
6     if(Nv.eq.0) go to 8
      write(6,113)
113   format(/,' VODOVI:',/,' Vod',t7,'Cvor-s',t15,'Cvor-t',t22,'B-s',
     *t26,'B-t',t31,' R + j X [Ohm]',t46,'Bv [uS]',t55,'Ro + j Xo [Ohm]'
     *,t72,'Bvo [uS]')
      do 9 i=1,Nv
      read(5,108) Sv(i),Tv(i),BSv(i),BTv(i),r,x,Bv(i),ro,xo,Bvo(i)
      Zv(i)=cmplx(r,x)
      Zvo(i)=cmplx(ro,xo)
      write(6,114)i,Sv(i),Tv(i),BSv(i),BTv(i),Zv(i),Bv(i),Zvo(i),Bvo(i)
114   format(i3,t7,i4,t15,i4,t22,i2,t26,i2,t31,2f7.2,t46,f7.1,t55,
     *2f8.2,t72,f7.2)
9     continue
8     if(N3t.eq.0) go to 10
      write(6,115)
115   format(/,' TRONAMOTNI TRANSFORMATORI:',/,' Tr.',t6,'Cv-s',t11,
     *'Cv-t',t16,'B-s',t20,'B-t',t24,'GS-s',t29,'GS-t',t34,'SS',t37,
     *'u12[%]',t44,'u13[%]',t51,'u23[%]',t58,'Sn12',t64,'Sn13',t70,
     *'R-s',t76,'R-t')
      do 11 i=1,N3t
      read(5,116) S3t(i),T3t(i),BS3t(i),BT3t(i),GSS3t(i),GST3t(i),
     *SS3t(i),ukst(i),uks3(i),ukt3(i),Snst(i),Sns3(i),RS3t(i),RT3t(i)
116   format(7i5,7f5.1)
      write(6,117) i,S3t(i),T3t(i),BS3t(i),BT3t(i),GSS3t(i),GST3t(i),
     *SS3t(i),ukst(i),uks3(i),ukt3(i),Snst(i),Sns3(i),RS3t(i),RT3t(i)
117   format(i3,t6,i3,t11,i3,t16,i2,t20,i2,t24,i3,t29,i3,t34,i2,t36,
     *f5.1,t43,f5.1,t50,f5.1,t57,f5.1,t63,f5.1,t69,f5.1,t75,f5.1)
11    continue
10    if(Nt.eq.0) go to 12
      write(6,118)
118   format(/,' TROSILA:',/,' Tros.',t10,'Cvor-t',t18,'B-t',t24,
     *' R + j X [Ohm]',t40,'Ro + j Xo [Ohm]')
      do 13 i=1,Nt
      read(5,105) Tt(i),BTt(i),r,x,ro,xo
      Zt(i)=cmplx(r,x)
      Zto(i)=cmplx(ro,xo)
      write(6,119) i,Tt(i),BTt(i),Zt(i),Zto(i)
119   format(i4,t10,i4,t18,i2,t24,2f7.2,t39,2f8.2)
13    continue
12    read(5,100) iUZB,JZg,JZeq,JZ2t,JZv,JZ3t,JZt,MATZ,MATZo,K1,K2,K3
      write(6,120)
120   format(/,' Opcijski izbor tiskanja i izracuna slijedecih velicina
     *i stanja:')
      if(iUZB.eq.0) write(6,121)
      if(iUZB.ne.0) write(6,122)
121   format(' a) Baznih vrijednosti napona, struja i impedancija: NE')
122   format(' a) Baznih vrijednosti napona, struja i impedancija: DA')
      if(JZg.eq.0) write(6,123)
      if(JZg.ne.0) write(6,124)
123   format(' b) Jedinicnih impedancija generatora dir./inv. redoslijed
     *a: NE')
124   format(' b) Jedinicnih impedancija generatora dir./inv. redoslijed
     *a: DA')
      if(JZeq.eq.0) write(6,125)
      if(JZeq.ne.0) write(6,126)
125   format(' c) Jedinicnih impedan. nadomjesnih elemenata dir./inv. i
     *nul. redoslijeda: NE')
126   format(' c) Jedinicnih impedan. nadomjesnih elemenata dir./inv. i
     *nul. redoslijeda: DA')
      if(JZ2t.eq.0) write(6,127)
      if(JZ2t.ne.0) write(6,128)
127   format(' d) Jedinicnih impedan. dvonamotnih transfor. dir./inv. i
     *nul. redoslijeda: NE')
128   format(' d) Jedinicnih impedan. dvonamotnih transfor. dir./inv. i
     *nul. redoslijeda: DA')
      if(JZv.eq.0) write(6,129)
      if(JZv.ne.0) write(6,130)
129   format(' e) Jedinicnih impedan. i susceptancija vodova dir./inv. i
     * nul. redoslijeda: NE')
130   format(' e) Jedinicnih impedan. i susceptancija vodova dir./inv. i
     * nul. redoslijeda: DA')
      if(JZ3t.eq.0) write(6,131)
      if(JZ3t.ne.0) write(6,132)
131   format(' f) Jedinicnih impedan. tronamotnih transfor. dir./inv. i
     *nul. redoslijeda: NE')
132   format(' f) Jedinicnih impedan. tronamotnih transfor. dir./inv. i
     *nul. redoslijeda: DA')
      if(JZt.eq.0) write(6,133)
      if(JZt.ne.0) write(6,134)
133   format(' g) Jedinicnih impedan. trosila dir./inv. i nultog redosli
     *jeda: NE')
134   format(' g) Jedinicnih impedan. trosila dir./inv. i nultog redosli
     *jeda: DA')
      if(MATZ.eq.0) write(6,135)
      if(MATZ.ne.0) write(6,136)
135   format(' h) Matrice vlastitih i medjusobnih impedan. cvorista dir.
     */inv. redoslijeda: NE')
136   format(' h) Matrice vlastitih i medjusobnih impedan. cvorista dir.
     */inv. redoslijeda: DA')
      if(MATZo.eq.0) write(6,137)
      if(MATZo.ne.0) write(6,138)
137   format(' i) Matrice vlastitih i medjusobnih impedan. cvorista nult
     *og redoslijeda: NE')
138   format(' i) Matrice vlastitih i medjusobnih impedan. cvorista nult
     *og redoslijeda: DA')
      if(K1.eq.0) write(6,139)
      if(K1.ne.0) write(6,140)
139   format(' j) Jednopolnog kratkog spoja: NE')
140   format(' j) Jednopolnog kratkog spoja: DA')
      if(K2.eq.0) write(6,141)
      if(K2.ne.0) write(6,142)
141   format(' k) Dvopolnog kratkog spoja: NE')
142   format(' k) Dvopolnog kratkog spoja: DA')
      if(K3.eq.0) write(6,143)
      if(K3.ne.0) write(6,144)
143   format(' l) Tropolnog kratkog spoja: NE')
144   format(' l) Tropolnog kratkog spoja: DA')
      write(6,145)
145   format(//,t30,'R E Z U L T A T I   P R O R A C U N A')
      CALL BVUIZ(Nnr,Un,Ib,Zb)
      if(iUZB.eq.0) go to 14
      write(6,146)
146   format(/,' BAZNE VRIJEDNOSTI:',/,' Broj-B',t10,'U [kV]',t18,
     *'Ib [kA]',t28,'Zb [Ohm]')
      do 15 i=1,Nnr
      write(6,147) i,Un(i),Ib(i),Zb(i)
147   format(i5,t10,f6.1,t18,f7.2,t28,f8.2)
15    continue
14    if(Ng.eq.0) go to 16
      CALL JVGEN(Ng,xd,Sgn,Zg)
      if(JZg.eq.0) go to 16
      write(6,148)
148   format(/,' JEDINICNE IMPEDANCIJE GENERATORA:',/,' Gen.',t10,
     *'Cvor-t',t20,' R + j X [p.u.]')
      do 17 i=1,Ng
      write(6,149) i,Tg(i),Zg(i)
149   format(i3,t10,i4,t20,2f8.4)
17    continue
16    if(Neq.eq.0) go to 18
      CALL JVEQ(Neq,BTeq,Zb,Zeq,Zeqo)
      if(JZeq.eq.0) go to 18
      write(6,150)
150   format(/,' JEDINICNE IMPEDANCIJE NADOMJESNIH ELEMENATA:',/,
     *' Elem.',t10,'Cvor-s',t20,'Cvor-t',t30,' R + j X [p.u.]',t50,
     *'Ro + j Xo [p.u.]')
      do 19 i=1,Neq
      write(6,151) i,Seq(i),Teq(i),Zeq(i),Zeqo(i)
151   format(i4,t10,i4,t20,i4,t30,2f8.4,t50,2f8.4)
19    continue
18    if(N2t.eq.0) go to 20
      CALL JV2TR(N2t,BS2t,BT2t,GSS2t,GST2t,uk,S2tn,RS2t,RT2t,Zb,Z2t,
     *Z2tso,Z2tsto,Z2tto,Zb)
      if(JZ2t.eq.0) go to 20
      write(6,152)
152   format(/,' JEDINICNE IMPEDANCIJE DVONAMOTNIH TRANSFORMATORA:',/,
     *' Tr.',t6,'Cv.-s',t12,'Cv.-t',t20,'R + j X [p.u.]',t38,
     *'R0so +j X0so',t52,'R0st +j X0st',t66,'R0to +j X0to')
      do 21 i=1,N2t
      write(6,153) i,S2t(i),T2t(i),Z2t(i),Z2tso(i),Z2tsto(i),Z2tto(i)
153   format(i3,t6,i4,t12,i4,t20,2f7.4,t38,2f6.3,t52,2f6.3,t66,2f6.3)
21    continue
20    if(Nv.eq.0) go to 22
      CALL JVVOD(Nv,BSv,Zb, Zv,Bv,Zvo,Bvo)
      if(JZv.eq.0) go to 22
      write(6,154)
154   format(/,' JEDINICNE IMPEDANCIJE I SUSCEPTANCIJE VODOVA:',/,
     *' Vod',t7,'Cv.-s',t13,'Cv.-t',t20,'R + j X [p.u.]',t36,
     *'Bv [p.u.]',t48,'Ro + j Xo [p.u.]',t65,'Bvo [p.u.]')
      do 23 i=1,Nv
      write(6,155) i,Sv(i),Tv(i),Zv(i),Bv(i),Zvo(i),Bvo(i)
155   format(i3,t7,i4,t13,i4,t20,2f7.4,t36,f9.6,t48,2f8.4,t65,f10.6)
23    continue
22    if(N3t.eq.0) go to 24
      CALL JV3TR(N3t,BS3t,BT3t,GSS3t,GST3t,SS3t,ukst,uks3,ukt3,Snst,
     *Sns3,RS3t,RT3t,Zb, Z3t,Z3tso,Z3tsto,Z3tto)
      if(JZ3t.eq.0) go to 24
      write(6,156)
156   format(/,' JEDINICNE IMPEDANCIJE TRONAMOTNIH TRANSFORMATORA:',/,
     *' Tr.',t6,'Cv.-s',t12,'Cv.-t',t20,'R = j X [p.u.]',t38,
     *'R0so +j X0so',t52,'R0st +j X0st',t66,'R0to +j X0to')
      do 25 i=1,N3t
      write(6,153) i,S3t(i),T3t(i),Z3t(i),Z3tso(i),Z3tsto(i),Z3tto(i)
25    continue
24    if(Nt.eq.0) go to 26
      CALL JVTROS(Nt,BTt,Zt,Zto,Zb)
      if(JZt.eq.0) go to 26
      write(6,157)
157   format(/,' JEDINICNE IMPEDANCIJE TROSILA:',/,' Tros.',t10,'Cvor-t'
     *,t20,'R + j X [p.u.]',t40,'Ro + j Xo [p.u.]')
      do 27 i=1,Nt
      write(6,158) i,Tt(i),Zt(i),Zto(i)
158   format(i4,t10,i4,t20,2f7.4,t40,2f8.4)
27    continue
26    CALL MATY(Nc,Ng,Neq,N2t,Nv,N3t,Nt, Tg,Zg, Seq,Teq,Zeq, S2t,T2t,Z2t
     *, Sv,Tv,Zv,Bv, S3t,T3t,Z3t, Tt,Zt, Z)
c
c     write(6,159)
c159  format(/,' MATRICA [Yc] DIR./INV. REDOSLIJEDA:')
c     do 28 i=1,Nc
c     write(6,160) (Z(i,j),j=1,i)
c160  format(8e10.3)
c28   continue
c
      CALL INVER(Nc,Z)
      if(MATZ.eq.0) go to 29
      write(6,161)
161   format(/,' MATRICA VLASTITIH I MEDJU. IMPEDANCIJA CVORISTA DIR./IN
     *V. REDOSLIJEDA:')
      do 30 i=1,Nc
      write(6,162) (Z(i,j),j=1,i)
162   format(8e10.3)
30    continue
29    if(K1.eq.0) go to 300
      Nco=Nc-Ng
      CALL MATY0(Nco,Neq,N2t,Nv,N3t,Nt, Seq,Teq,Zeqo, S2t,T2t,Z2tso,
     *Z2tsto,Z2tto, Sv,Tv,Zvo,Bvo, S3t,T3t,Z3tso,Z3tsto,Z3tto, Tt,Zto,
     * Zo)
c
c     write(6,163)
c163  format(/,' MATRICA [Yc] NULTOG REDOSLIJEDA:')
c     do 31 i=1,Nco
c     write(6,160) (Zo(i,j),j=1,i)
c31   continue
c
      CALL INVER(Nco,Zo)
      if(MATZo.eq.0) go to 32
      write(6,164)
164   format(/,' MATRICA VLASTITIH I MEDJU. IMPEDANCIJA CVORISTA NULTOG
     *REDOSLIJEDA:')
      do 33 i=1,Nco
      write(6,162) (Zo(i,j),j=1,i)
33    continue
32    read(5,100) Nck1
      write(6,165) Nck1
165   format(/,t15,'PRORACUNI JEDNOPOLNOG KRATKOG SPOJA U',i3,' CVORISTA
     *')
      if(Nck1-Nco) 34,35,35
34    do 36 i=1,Nck1
      read(5,100) k,iUg,iUeq,iU2t,iUZv,iU3t,iUtr
      Iko=1./(2.*Z(k,k)+Zo(k,k))
      write(6,166) k
166   format(/,' JEDNOPOLNI KRATKI SPOJ U CVORISTU:',i4,/,
     *' ---------------------------------',//,' Odredjivanje stanja u:')
      if(iUg.ne.0) write(6,167)
167   format(' - generatorima')
      if(iUeq.ne.0) write(6,168)
168   format(' - nadomjesnim elementima')
      if(iU2t.ne.0) write(6,169)
169   format(' - dvonamotnim transformatorima')
      if(iUZv.ne.0) write(6,170)
170   format(' - vodovima')
      if(iU3t.ne.0) write(6,171)
171   format(' - tronamotnim transformatorima')
      if(iUtr.ne.0) write(6,172)
172   format(' - trosilima')
      if(Ng.eq.0) go to 37
      if(iUg.eq.0) go to 37
      CALL K1GEN(k,Iko,Ng,Tg,Btg,Zg,Z,Ib,Un, Idt,Iit,Vdt,Vit)
      write(6,173)
173   format(/,' GENERATORI:',//,' Stanje direktnog redoslijeda:',/,
     *' Gen.',t10,'Cvor.',t20,'Struja [kA]',t35,'Napon [kV]',t50,
     *'P +j Q [MVA]')
      do 38 l=1,Ng
      S=3.*Vdt(l)*conjg(Idt(l))
      c=Idt(l)
      CALL ZMK(c,r,x)
      c=Vdt(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Tg(l),r,x,ro,xo,S
174   format(i4,t10,i4,t20,f5.2,' /',f5.1,t34,f5.1,' /',f5.1,t50,2f6.1)
38    continue
      write(6,555)
555   format(/,' Stanje inverznog redoslijeda:',/,' Gen.',t10,'Cvor.',
     *t20,'Struja [kA]',t35,'Napon [kV]',t50,'P +j Q [MVA]')
      do 556 l=1,Ng
      S=3.*Vit(l)*conjg(Iit(l))
      c=Iit(l)
      CALL ZMK(c,r,x)
      c=Vit(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Tg(l),r,x,ro,xo,S
556   continue
      write(6,175)
175   format(/,' Strujno stanje u fazama (R, S i T):',/,' Gen.',t10,
     *'Cvor.',t22,'I(R) [kA]',t35,'I(S) [kA]',t48,'I(T) [kA]')
      do 39 l=1,Ng
      Ibt=Idt(l)+Iit(l)
      Izt=a2*Idt(l)+a*Iit(l)
      CALL ZMK(Ibt,r,x)
      CALL ZMK(Izt,ro,xo)
      Izt=a*Idt(l)+a2*Iit(l)
      CALL ZMK(Izt,x1,x2)
      write(6,176) l,Tg(l),r,x,ro,xo,x1,x2
176   format(i4,t10,i4,t20,f5.2,' /',f5.1,t36,f5.2,' /',f5.1,t49,f5.2,
     *' /',f5.1)
39    continue
      write(6,177)
177   format(/,' Naponsko stanje u fazama (R, S i T):',/,' Gen.',t10,
     *'Cvor.',t22,'V(R) [kV]',t40,'V(S) [kV]',t58,'V(T) [kV]')
      do 40 l=1,Ng
      it=BTg(l)
      Vbt=(a2-1.)*Un(it)/3.+Vdt(l)+Vit(l)
      Vzt1=(a2-1.)*a2*Un(it)/3.+a2*Vdt(l)+a*Vit(l)
      Vzt2=(a2-1.)*a*Un(it)/3.+a*Vdt(l)+a2*Vit(l)
      CALL ZMK(Vbt,r,x)
      CALL ZMK(Vzt1,ro,xo)
      CALL ZMK(Vzt2,x1,x2)
      write(6,178) l,Tg(l),r,x,ro,xo,x1,x2
178   format(i4,t10,i4,t20,f5.1,' /',f5.1,t38,f5.1,' /',f5.1,t56,f5.1,
     *' /',f5.1)
40    continue
37    if(Neq.eq.0) go to 41
      if(iUeq.eq.0) go to 41
      CALL K1EQ(k,Iko,Neq,Seq,Teq,BSeq,BTeq,Zeq,Zeqo,Z,Zo,Ib,Un, Ids,
     *Idt,Ios,Iot,Vds,Vdt,Vos,Vot)
      write(6,179)
179   format(/,' NADOMJESNI ELEMENTI:',//,' Stanje dir./inv. redoslijeda
     *:',/,' Elem.',t10,'Cvor.',t20,'Struja [kA]',t35,'Napon [kV]',t50,
     *'P + j Q [MVA]')
      do 42 l=1,Neq
      S=3.*Vdt(l)*conjg(Idt(l))
      c=Idt(l)
      CALL ZMK(c,r,x)
      c=Vdt(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Teq(l),r,x,ro,xo,S
      if(Seq(l).eq.0) go to 42
      S=3.*Vds(l)*conjg(Ids(l))
      c=Ids(l)
      CALL ZMK(c,r,x)
      c=Vds(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Seq(l),r,x,ro,xo,S
42    continue
      write(6,180)
180   format(/,' Stanje nultog redoslijeda:',/,' Elem.',t10,'Cvor.',
     *t20,'Struja [kA]',t35,'Napon [kV]',t50,'P + j Q [MVA]')
      do 43 l=1,Neq
      S=3.*Vot(l)*conjg(Iot(l))
      c=Iot(l)
      CALL ZMK(c,r,x)
      c=Vot(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Teq(l),r,x,ro,xo,S
      if(Seq(l).eq.0) go to 43
      S=3.*Vos(l)*conjg(Ios(l))
      c=Ios(l)
      CALL ZMK(c,r,x)
      c=Vos(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Seq(l),r,x,ro,xo,S
43    continue
      write(6,181)
181   format(/,' Strujno stanje u fazama (R, S i T):',/,' Elem.',t10,
     *'Cvor.',t22,'I(R) [kA]',t35,'I(S)=I(T) [kA]')
      do 44 l=1,Neq
      Ibt=2.*Idt(l)+Iot(l)
      Izt=Iot(l)-Idt(l)
      CALL ZMK(Ibt,r,x)
      CALL ZMK(Izt,ro,xo)
      write(6,176) l,Teq(l),r,x,ro,xo
      if(Seq(l).eq.0) go to 44
      Ibs=2.*Ids(l)+Ios(l)
      Izs=Ios(l)-Ids(l)
      CALL ZMK(Ibs,r,x)
      CALL ZMK(Izs,ro,xo)
      write(6,176) l,Seq(l),r,x,ro,xo
44    continue
      write(6,182)
182   format(/,' Naponsko stanje u fazama (R, S i T):',/,' Elem.',t10,
     *'Cvor.',t22,'V(R) [kV]',t40,'V(S) [kV]',t58,'V(T) [kV]')
      do 45 l=1,Neq
      it=BTeq(l)
      Vbt=Un(it)/sqrt(3.)+2.*Vdt(l)+Vot(l)
      Vzt1=cmplx(-0.5,-0.866)*Un(it)/sqrt(3.)-Vdt(l)+Vot(l)
      Vzt2=cmplx(-0.5,0.866)*Un(it)/sqrt(3.)-Vdt(l)+Vot(l)
      CALL ZMK(Vbt,r,x)
      CALL ZMK(Vzt1,ro,xo)
      CALL ZMK(Vzt2,x1,x2)
      write(6,178) l,Teq(l),r,x,ro,xo,x1,x2
      if(Seq(l).eq.0) go to 45
      is=BSeq(l)
      Vbs=Un(is)/sqrt(3.)+2.*Vds(l)+Vos(l)
      Vzs1=cmplx(-0.5,-0.866)*Un(is)/sqrt(3.)-Vds(l)+Vos(l)
      Vzs2=cmplx(-0.5,0.866)*Un(is)/sqrt(3.)-Vds(l)+Vos(l)
      CALL ZMK(Vbs,r,x)
      CALL ZMK(Vzs1,ro,xo)
      CALL ZMK(Vzs2,x1,x2)
      write(6,178) l,Seq(l),r,x,ro,xo,x1,x2
45    continue
41    if(N2t.eq.0) go to 46
      if(iU2t.eq.0) go to 46
      CALL K12TR(k,Iko,N2t,S2t,T2t,BS2t,BT2t,Z2t,Z2tso,Z2tsto,Z2tto,Z,
     *Zo,Ib,Un, Ids,Idt,Iit,Ios,Vds,Vdt,Vit,Vos, Iot,Vot)
      write(6,183)
183   format(/,' DVONAMOTNI TRANSFORMATORI:',//,' Stanje direktnog redos
     *lijeda:',/,' Trafo',t10,'Cvor.',t20,'Struja [kA]',t35,'Napon [kV]'
     *,t50,'P + j Q [MVA]')
      do 47 l=1,N2t
      S=3.*Vds(l)*conjg(Ids(l))
      c=Ids(l)
      CALL ZMK(c,r,x)
      c=Vds(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,S2t(l),r,x,ro,xo,S
      S=3.*Vdt(l)*conjg(Idt(l))
      c=Idt(l)
      CALL ZMK(c,r,x)
      c=Vdt(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,T2t(l),r,x,ro,xo,S
47    continue
      write(6,557)
557   format(/,' Stanje inverznog redoslijeda:',/,' Trafo',t10,'Cvor',
     *t20,'Struja [kA]',t35,'Napon [kV]',t50,'P + j Q [MVA]')
      do 558 l=1,N2t
      S=3.*Vds(l)*conjg(Ids(l))
      c=Ids(l)
      CALL ZMK(c,r,x)
      c=Vds(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,S2t(l),r,x,ro,xo,S
      S=3.*Vit(l)*conjg(Iit(l))
      c=Iit(l)
      CALL ZMK(c,r,x)
      c=Vit(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,T2t(l),r,x,ro,xo,S
558   continue
      write(6,184)
184   format(/,' Stanje nultog redoslijeda:',/,' Trafo',t10,'Cvor.',
     *t20,'Struja [kA]',t35,'Napon [kV]',t50,'P + j Q [MVA]')
      do 48 l=1,N2t
      S=3.*Vos(l)*conjg(Ios(l))
      c=Ios(l)
      CALL ZMK(c,r,x)
      c=Vos(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,S2t(l),r,x,ro,xo,S
      S=3.*Vot(l)*conjg(Iot(l))
      c=Iot(l)
      CALL ZMK(c,r,x)
      c=Vot(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,T2t(l),r,x,ro,xo,S
48    continue
      write(6,185)
185   format(/,' Strujno stanje u fazama (R, S i T) i zvjezdistima:',/,
     *' Trafo',t10,'Cvor.',t22,'I(R) [kA]',t35,'I(S) [kA]',t48,
     *'I(T) [kA]',t61,'I(zvj.) [kA]')
      do 49 l=1,N2t
      Ibs=2.*Ids(l)+Ios(l)
      Izs=Ios(l)-Ids(l)
      Ius=3.*Ios(l)
      CALL ZMK(Ibs,r,x)
      CALL ZMK(Izs,ro,xo)
      CALL ZMK(Ius,x1,x2)
      write(6,560) l,S2t(l),r,x,ro,xo,ro,xo,x1,x2
560   format(i4,t10,i3,t22,f5.2,' /',f5.1,t35,f5.2,' /',f5.1,t48,f5.2,
     *' /',f5.1,t61,f5.2,' /',f5.1)
      Ibt=Idt(l)+Iit(l)
      Izt=a2*Idt(l)+a*Iit(l)
      CALL ZMK(Ibt,r,x)
      CALL ZMK(Izt,ro,xo)
      Izt=a*Idt(l)+a2*Iit(l)
      CALL ZMK(Izt,x1,x2)
      write(6,561) l,T2t(l),r,x,ro,xo,x1,x2
561   format(i4,t10,i3,t22,f5.2,' /',f5.1,t35,f5.2,' /',f5.1,t48,f5.2,
     *' /',f5.1)
49    continue
      write(6,186)
186   format(/,' Naponsko stanje u fazama (R, S i T):',/,' Trafo',t10,
     *'Cvor.',t22,'V(R) [kV]',t40,'V(S) [kV]',t58,'V(T) [kV]')
      do 50 l=1,N2t
      is=BS2t(l)
      it=BT2t(l)
      Vbs=Un(is)/sqrt(3.)+2.*Vds(l)+Vos(l)
      Vzs1=a2*Un(is)/sqrt(3.)-Vds(l)+Vos(l)
      Vzs2=a*Un(is)/sqrt(3.)-Vds(l)+Vos(l)
      CALL ZMK(Vbs,r,x)
      CALL ZMK(Vzs1,ro,xo)
      CALL ZMK(Vzs2,x1,x2)
      write(6,178) l,S2t(l),r,x,ro,xo,x1,x2
      Vbt=(a2-1.)*Un(it)/3.+Vdt(l)+Vit(l)
      Vzt1=(a2-1)*a2*Un(it)/3.+a2*Vdt(l)+a*Vit(l)
      Vzt2=(a2-1.)*a*Un(it)/3.+a*Vdt(l)+a2*Vit(l)
      CALL ZMK(Vbt,r,x)
      CALL ZMK(Vzt1,ro,xo)
      CALL ZMK(Vzt2,x1,x2)
      write(6,178) l,T2t(l),r,x,ro,xo,x1,x2
50    continue
46    if(Nv.eq.0) go to 51
      if(iUZv.eq.0) go to 51
      CALL K1VOD(k,Iko,Nv,Sv,Tv,BSv,Zv,Zvo,Z,Zo,Ib,Un, Ids,Idt,Ios,Iot,
     *Vds,Vdt,Vos,Vot, Bv,Bvo)
      write(6,187)
187   format(/,' VODOVI:',//,' Stanje dir./inv. redoslijeda:',/,' Vod',
     *t10,'Cvor.',t20,'Struja [kA]',t35,'Napon [kV]',t50,
     *'P + j Q [MVA]')
      do 52 l=1,Nv
      S=3.*Vds(l)*conjg(Ids(l))
      c=Ids(l)
      CALL ZMK(c,r,x)
      c=Vds(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Sv(l),r,x,ro,xo,S
      S=3.*Vdt(l)*conjg(Idt(l))
      c=Idt(l)
      CALL ZMK(c,r,x)
      c=Vdt(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Tv(l),r,x,ro,xo,S
52    continue
      write(6,188)
188   format(/,' Stanje nultog redoslijeda:',/,' Vod',t10,'Cvor.',t20,
     *'Struja [kA]',t35,'Napon [kV]',t50,'P + j Q [MVA]')
      do 53 l=1,Nv
      S=3.*Vos(l)*conjg(Ios(l))
      c=Ios(l)
      CALL ZMK(c,r,x)
      c=Vos(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Sv(l),r,x,ro,xo,S
      S=3.*Vot(l)*conjg(Iot(l))
      c=Iot(l)
      CALL ZMK(c,r,x)
      c=Vot(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Tv(l),r,x,ro,xo,S
53    continue
      write(6,189)
189   format(/,' Strujno stanje u fazama (R, S i T):',/,' Vod',t10,
     *'Cvor.',t22,'I(R) [kA]',t35,'I(S)=I(T) [kA]')
      do 54 l=1,Nv
      Ibs=2.*Ids(l)+Ios(l)
      Izs=Ios(l)-Ids(l)
      CALL ZMK(Ibs,r,x)
      CALL ZMK(Izs,ro,xo)
      write(6,176) l,Sv(l),r,x,ro,xo
      Ibt=2.*Idt(l)+Iot(l)
      Izt=Iot(l)-Idt(l)
      CALL ZMK(Ibt,r,x)
      CALL ZMK(Izt,ro,xo)
      write(6,176) l,Tv(l),r,x,ro,xo
54    continue
      write(6,190)
190   format(/,' Naponsko stanje u fazama (R, S i T):',/,' Vod',t10,
     *'Cvor.',t22,'V(R) [kV]',t40,'V(S) [kV]',t58,'V(T) [kV]')
      do 55 l=1,Nv
      is=BSv(l)
      Vbs=Un(is)/sqrt(3.)+2.*Vds(l)+Vos(l)
      Vzs1=a2*Un(is)/sqrt(3.)-Vds(l)+Vos(l)
      Vzs2=a*Un(is)/sqrt(3.)-Vds(l)+Vos(l)
      CALL ZMK(Vbs,r,x)
      CALL ZMK(Vzs1,ro,xo)
      CALL ZMK(Vzs2,x1,x2)
      write(6,178) l,Sv(l),r,x,ro,xo,x1,x2
      Vbt=Un(is)/sqrt(3.)+2.*Vdt(l)+Vot(l)
      Vzt1=a2*Un(is)/sqrt(3.)-Vdt(l)+Vot(l)
      Vzt2=a*Un(is)/sqrt(3.)-Vdt(l)+Vot(l)
      CALL ZMK(Vbt,r,x)
      CALL ZMK(Vzt1,ro,xo)
      CALL ZMK(Vzt2,x1,x2)
      write(6,178) l,Tv(l),r,x,ro,xo,x1,x2
55    continue
      write(6,191)
191   format(/,' Mjerene impedancije distantnom zastitom:',/,' Vod',
     *t10,'Cvor.',t20,'Rmj. +j Xmj. =  Zmj. [Ohm]')
      do 56 l=1,Nv
      is=BSv(l)
      c=(Zvo(l)-Zv(l))/Zv(l)
      Zms=(Un(is)/sqrt(3.)+2.*Vds(l)+Vos(l))/(2.*Ids(l)+(1.+c)*Ios(l))
      Zmt=(Un(is)/sqrt(3.)+2.*Vdt(l)+Vot(l))/(2.*Idt(l)+(1.+c)*Iot(l))
      CALL ZMK(Zms,r,x)
      CALL ZMK(Zmt,ro,xo)
      write(6,192) l,Sv(l),Zms,r,x
192   format(i4,t10,i4,t20,2f6.1,f8.1,' /',f5.1)
      write(6,192) l,Tv(l),Zmt,ro,xo
56    continue
51    if(N3t.eq.0) go to 57
      if(iU3t.eq.0) go to 57
      CALL K13TR(k,Iko,N3t,S3t,T3t,BS3t,BT3t,Z3t,Z3tso,Z3tsto,Z3tto,Z,
     *Zo,Ib,Un, Ids,Idt,Ios,Iot,Vds,Vdt,Vos,Vot)
      write(6,193)
193   format(/,' TRONAMOTNI TRANSFORMATORI:',//,' Stanje dir./inv. redos
     *lijeda:',/,' Trafo',t10,'Cvor.',t20,'Struja [kA]',t35,'Napon [kV]'
     *,t50,'P + j Q [MVA]')
      do 58 l=1,N3t
      S=3.*Vds(l)*conjg(Ids(l))
      c=Ids(l)
      CALL ZMK(c,r,x)
      c=Vds(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,S3t(l),r,x,ro,xo,S
      S=3.*Vdt(l)*conjg(Idt(l))
      c=Idt(l)
      CALL ZMK(c,r,x)
      c=Vdt(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,T3t(l),r,x,ro,xo,S
58    continue
      write(6,184)
      do 59 l=1,N3t
      S=3.*Vos(l)*conjg(Ios(l))
      c=Ios(l)
      CALL ZMK(c,r,x)
      c=Vos(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,S3t(l),r,x,ro,xo,S
      S=3.*Vot(l)*conjg(Iot(l))
      c=Iot(l)
      CALL ZMK(c,r,x)
      c=Vot(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,T3t(l),r,x,ro,xo,S
59    continue
      write(6,185)
      do 60 l=1,N3t
      Ibs=2.*Ids(l)+Ios(l)
      Izs=Ios(l)-Ids(l)
      if(SS3t(l).eq.0) Ius=3.*Ios(l)
      if(SS3t(l).ne.0) Ius=3.*(Ios(l)+Iot(l))
      CALL ZMK(Ibs,r,x)
      CALL ZMK(Izs,ro,xo)
      CALL ZMK(Ius,x1,x2)
      write(6,178) l,S3t(l),r,x,ro,xo,x1,x2
      Ibt=2.*Idt(l)+Iot(l)
      Izt=Iot(l)-Idt(l)
      if(SS3t(l).eq.0) Iut=3.*Iot(l)
      if(SS3t(l).ne.0) Iut=3.*(Ios(l)+Iot(l))
      CALL ZMK(Ibt,r,x)
      CALL ZMK(Izt,ro,xo)
      CALL ZMK(Iut,x1,x2)
      write(6,178) l,T3t(l),r,x,ro,xo,x1,x2
60    continue
      write(6,186)
      do 61 l=1,N3t
      is=BS3t(l)
      it=BT3t(l)
      Vbs=Un(is)/sqrt(3.)+2.*Vds(l)+Vos(l)
      Vzs1=cmplx(-0.5,-0.866)*Un(is)/sqrt(3.)-Vds(l)+Vos(l)
      Vzs2=cmplx(-0.5,0.866)*Un(is)/sqrt(3.)-Vds(l)+Vos(l)
      CALL ZMK(Vbs,r,x)
      CALL ZMK(Vzs1,ro,xo)
      CALL ZMK(Vzs2,x1,x2)
      write(6,178) l,S3t(l),r,x,ro,xo,x1,x2
      Vbt=Un(it)/sqrt(3.)+2.*Vdt(l)+Vot(l)
      Vzt1=cmplx(-0.5,-0.866)*Un(it)/sqrt(3.)-Vdt(l)+Vot(l)
      Vzt2=cmplx(-0.5,0.866)*Un(it)/sqrt(3.)-Vdt(l)+Vot(l)
      CALL ZMK(Vbt,r,x)
      CALL ZMK(Vzt1,ro,xo)
      CALL ZMK(Vzt2,x1,x2)
      write(6,178) l,T3t(l),r,x,ro,xo,x1,x2
61    continue
57    if(Nt.eq.0) go to 36
      if(iUtr.eq.0) go to 36
      CALL K1TROS(k,Iko,Nt,Tt,BTt,Zt,Zto,Z,Zo,Ib,Un, Idt,Iot,Vdt,Vot)
      write(6,194)
194   format(/,' TROSILA;',//,' Stanje dir./inv. redoslijeda:',/,
     *' Tros.',t10,'Cvor.',t20,'Struja [kA]',t35,'Napon [kV]',t50,
     *'P + j Q [MVA]')
      do 63 l=1,Nt
      S=3.*Vdt(l)*conjg(Idt(l))
      c=Idt(l)
      CALL ZMK(c,r,x)
      c=Vdt(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Tt(l),r,x,ro,xo,S
63    continue
      write(6,195)
195   format(/,' Stanje nultog redoslijeda:',/,' Tros.',t10,'Cvor.',
     *t20,'Struja [kA]',t35,'Napon [kV]',t50,'P + j Q [MVA]')
      do 64 l=1,Nt
      S=3.*Vot(l)*conjg(Iot(l))
      c=Iot(l)
      CALL ZMK(c,r,x)
      c=Vot(l)
      CALL ZMK(c,ro,xo)
      write(6,174) l,Tt(l),r,x,ro,xo,S
64    continue
      write(6,196)
196   format(/,' Strujno stanje u fazama (R, S i T):',/,' Tros.',t10,
     *'Cvor.',t22,'I(R) [kA]',t35,'I(S)=I(T) [kA]')
      do 65 l=1,Nt
      Ibt=2.*Idt(l)+Iot(l)
      Izt=Iot(l)-Idt(l)
      CALL ZMK(Ibt,r,x)
      CALL ZMK(Izt,ro,xo)
      write(6,176) l,Tt(l),r,x,ro,xo
65    continue
      write(6,197)
197   format(/,' Naponsko stanje u fazama (R, S i T):',/,' Tros.',t10,
     *'Cvor.',t22,'V(R) [kV]',t40,'V(S) [kV]',t58,'V(T) [kV]')
      do 66 l=1,Nt
      it=BTt(l)
      Vbt=Un(it)/sqrt(3.)+2.*Vdt(l)+Vot(l)
      Vzt1=cmplx(-0.5,-0.866)*Un(it)/sqrt(3.)-Vdt(l)+Vot(l)
      Vzt2=cmplx(-0.5,0.866)*Un(it)/sqrt(3.)-Vdt(l)+Vot(l)
      CALL ZMK(Vbt,r,x)
      CALL ZMK(Vzt1,ro,xo)
      CALL ZMK(Vzt2,x1,x2)
      write(6,178) l,Tt(l),r,x,ro,xo,x1,x2
66    continue
36    continue
      go to 300
35    do 62 k=1,Nco
      Iko=1./(2.*Z(k,k)+Zo(k,k))
      Ik1=cmplx(0.,0.)
      write(6,198) k
198   format(/, ' ------------------------------------------------------
     *----------------------',/,
     *' | JKS u cv.',i4,' | Udio iz cv. |   I(R) [kA]   | I(S)=I(T) [kA]
     * |   3Io [kA]   |',/,' ---------------------------------------------
     *-------------------------------')
      if(Neq.eq.0) go to 67
      do 68 l=1,Neq
      i=Seq(l)
      j=Teq(l)
      is=BSeq(l)
      it=BTeq(l)
      if(i) 70,69,70
69    if(j.ne.k) go to 68
      Ibs=(2.*Z(k,k)/Zeq(l)+Zo(k,k)/Zeqo(l))*Iko*Ib(it)
      Izs=(Zo(k,k)/Zeqo(l)-Z(k,k)/Zeq(l))*Iko*Ib(it)
      Ius=Ibs+2.*Izs
      Ik1=Ik1+Ibs
      CALL ZMK(Ibs,r,x)
      CALL ZMK(Izs,ro,xo)
      CALL ZMK(Ius,x1,x2)
      write(6,199) i,r,x,ro,xo,x1,x2
199   format(' |',t18,'|',t23,i3,t32,'|',t34,f5.2,' /',f5.1,t47,'|',
     *t51,f5.2,' /',f5.1,t63,'|',t65,f5.2,' /',f5.1,t77,'|')
      go to 68
70    if(i-k) 72,71,72
71    Ibt=(2.*(Z(k,k)-Z(j,k))/Zeq(l)+(Zo(k,k)-Zo(j,k))/Zeqo(l))*Iko*
     *Ib(is)
      Izt=((Zo(k,k)-Zo(j,k))/Zeqo(l)-(Z(k,k)-Z(j,k))/Zeq(l))*Iko*Ib(is)
      Iut=Ibt+2.*Izt
      Ik1=Ik1+Ibt
      CALL ZMK(Ibt,r,x)
      CALL ZMK(Izt,ro,xo)
      CALL ZMK(Iut,x1,x2)
      write(6,199) j,r,x,ro,xo,x1,x2
      go to 68
72    if(j-k) 68,73,68
73    Ibs=(2.*(Z(k,k)-Z(i,k))/Zeq(l)+(Zo(k,k)-Zo(i,k))/Zeqo(l))*Iko*
     *Ib(it)
      Izs=((Zo(k,k)-Zo(i,k))/Zeqo(l)-(Z(k,k)-Z(i,k))/Zeq(l))*Iko*Ib(it)
      Ius=Ibs+2.*Izs
      Ik1=Ik1+Ibs
      CALL ZMK(Ibs,r,x)
      CALL ZMK(Izs,ro,xo)
      CALL ZMK(Ius,x1,x2)
      write(6,199) i,r,x,ro,xo,x1,x2
68    continue
67    if(N2t.eq.0) go to 74
      do 75 l=1,N2t
      i=S2t(l)
      j=T2t(l)
      is=BS2t(l)
      it=BT2t(l)
      if(i-k) 77,76,77
76    Ibt=2.*(Z(k,k)-Z(j,k))/Z2t(l)*Iko*Ib(is)
      Izt=-(Z(k,k)-Z(j,k))/Z2t(l)*Iko*Ib(is)
      x=aimag(Z2tsto(l))
      if(x-100.) 78,79,79
78    c=(Zo(k,k)-Zo(j,k))/Z2tsto(l)*Iko*Ib(is)
      go to 80
79    x=aimag(Z2tso(l))
      if(x-100.) 81,82,82
81    c=Zo(k,k)/Z2tso(l)*Iko*Ib(is)
80    Ibt=Ibt+c
      Izt=Izt+c
82    Iut=Ibt+2.*Izt
      Ik1=Ik1+Ibt
      CALL ZMK(Ibt,r,x)
      CALL ZMK(Izt,ro,xo)
      CALL ZMK(Iut,x1,x2)
      write(6,199) j,r,x,ro,xo,x1,x2
      go to 75
77    if(j-k) 75,83,75
83    Ibs=2.*(Z(k,k)-Z(i,k))/Z2t(l)*Iko*Ib(it)
      Izs=-(Z(k,k)-Z(i,k))/Z2t(l)*Iko*Ib(it)
      x=aimag(Z2tsto(l))
      if(x-100.) 84,85,85
84    c=(Zo(k,k)-Zo(i,k))/Z2tsto(l)*Iko*Ib(it)
      go to 86
85    x=aimag(Z2tto(l))
      if(x-100.) 87,88,88
87    c=Zo(k,k)/Z2tto(l)*Iko*Ib(it)
86    Ibs=Ibs+c
      Izs=Izs+c
88    Ius=Ibs+2.*Izs
      Ik1=Ik1+Ibs
      CALL ZMK(Ibs,r,x)
      CALL ZMK(Izs,ro,xo)
      CALL ZMK(Ius,x1,x2)
      write(6,199) i,r,x,ro,xo,x1,x2
75    continue
74    if(Nv.eq.0) go to 89
      do 90 l=1,Nv
      i=Sv(l)
      j=Tv(l)
      is=BSv(l)
      if(i-k) 92,91,92
91    Ibt=(2.*(Z(k,k)-Z(j,k))/Zv(l)+(Zo(k,k)-Zo(j,k))/Zvo(l))*Iko*Ib(is)
      Izt=((Zo(k,k)-Zo(j,k))/Zvo(l)-(Z(k,k)-Z(j,k))/Zv(l))*Iko*Ib(is)
      Iut=Ibt+2.*Izt
      Ik1=Ik1+Ibt
      CALL ZMK(Ibt,r,x)
      CALL ZMK(Izt,ro,xo)
      CALL ZMK(Iut,x1,x2)
      write(6,199) j,r,x,ro,xo,x1,x2
      go to 90
92    if(j.ne.k) go to 90
      Ibs=(2.*(Z(k,k)-Z(i,k))/Zv(l)+(Zo(k,k)-Zo(i,k))/Zvo(l))*Iko*Ib(is)
      Izs=((Zo(k,k)-Zo(i,k))/Zvo(l)-(Z(k,k)-Z(i,k))/Zv(l))*Iko*Ib(is)
      Ius=Ibs+2.*Izs
      Ik1=Ik1+Ibs
      CALL ZMK(Ibs,r,x)
      CALL ZMK(Izs,ro,xo)
      CALL ZMK(Ius,x1,x2)
      write(6,199) i,r,x,ro,xo,x1,x2
90    continue
89    if(N3t.eq.0) go to 93
      do 94 l=1,N3t
      i=S3t(l)
      j=T3t(l)
      is=BS3t(l)
      it=BT3t(l)
      if(i-k) 96,95,96
95    Ibt=2.*(Z(k,k)-Z(j,k))/Z3t(l)*Iko*Ib(is)
      Izt=-(Z(k,k)-Z(j,k))/Z3t(l)*Iko*Ib(is)
      x=aimag(Z3tsto(l))
      if(x-100.) 97,98,98
97    c=((Zo(k,k)-Zo(j,k))/Z3tsto(l)+Zo(k,k)/Z3tso(l))*Iko*Ib(is)
      go to 99
98    x=aimag(Z3tso(l))
      if(x-100.) 250,251,251
250   c=Zo(k,k)/Z3tso(l)*Iko*Ib(is)
99    Ibt=Ibt+c
      Izt=Izt+c
251   Iut=Ibt+2.*Izt
      Ik1=Ik1+Ibt
      CALL ZMK(Ibt,r,x)
      CALL ZMK(Izt,ro,xo)
      CALL ZMK(Iut,x1,x2)
      write(6,199) j,r,x,ro,xo,x1,x2
      go to 94
96    if(j.ne.k) go to 94
      Ibs=2.*(Z(k,k)-Z(i,k))/Z3t(l)*Iko*Ib(it)
      Izs=-(Z(k,k)-Z(i,k))/Z3t(l)*Iko*Ib(it)
      x=aimag(Z3tsto(l))
      if(x-100.) 252,253,253
252   c=((Zo(k,k)-Zo(i,k))/Z3tsto(l)+Zo(k,k)/Z3tto(l))*Iko*Ib(it)
      go to 254
253   x=aimag(Z3tto(l))
      if(x-100.) 255,256,256
255   c=Zo(k,k)/Z3tto(l)*Iko*Ib(it)
254   Ibs=Ibs+c
      Izs=Izs+c
256   Ius=Ibs+2.*Izs
      Ik1=Ik1+Ibs
      CALL ZMK(Ibs,r,x)
      CALL ZMK(Izs,ro,xo)
      CALL ZMK(Ius,x1,x2)
      write(6,199) i,r,x,ro,xo,x1,x2
94    continue
93    if(Nt.eq.0) go to 257
      do 258 l=1,Nt
      i=0
      j=Tt(l)
      it=BTt(l)
      if(j.ne.k) go to 258
      Ibs=(2.*Z(k,k)/Zt(l)+Zo(k,k)/Zto(l))*Iko*Ib(it)
      Izs=(Zo(k,k)/Zto(l)-Z(k,k)/Zt(l))*Iko*Ib(it)
      Ius=Ibs+2.*Izs
      Ik1=Ik1+Ibs
      CALL ZMK(Ibs,r,x)
      CALL ZMK(Izs,ro,xo)
      CALL ZMK(Ius,x1,x2)
      write(6,199) i,r,x,ro,xo,x1,x2
258   continue
257   CALL ZMK(Ik1,r,x)
      write(6,200) r,x
200   format(' ---------------------------------------------------------
     *-------------------',/,' |',t18,'Ukupno:',t30,'Ik1 =',f5.1,' /',f5
     *.1,' [kA]',t77,'|',/,' -------------------------------------------
     *---------------------------------')
62    continue
300   stop
      end
C**** Potprogram BVUIZ za racunanje baznih vrijednosti
      SUBROUTINE BVUIZ(Nnr,Un,Ib,Zb)
      REAL Ib(20)
      DIMENSION Un(20),Zb(20)
      Sb=100.
      do 1 i=1,Nnr
      Ib(i)=Sb/(sqrt(3.)*Un(i))
      Zb(i)=Un(i)**2/Sb
1     continue
      return
      end
C**** Potp. JVGEN za racun jedin. impedancija dir/inv red. generatora
      SUBROUTINE JVGEN(Ng,xd,Sgn,Zg)
      DIMENSION xd(50),Sgn(50)
      COMPLEX Zg(50)
      do 1 i=1,Ng
      x=xd(i)/Sgn(i)
      Zg(i)=cmplx(0.,x)
1     continue
      return
      end
C**** Potp. JVEQ za racun jedin. imp. dir/inv i nultog red. ekvivalenata
      SUBROUTINE JVEQ(Neq,BTeq,Zb,Zeq,Zeqo)
      INTEGER BTeq(30)
      DIMENSION Zb(20)
      COMPLEX Zeq(30),Zeqo(30)
      do 1 i=1,Neq
      it=BTeq(i)
      Zeq(i)=Zeq(i)/Zb(it)
      Zeqo(i)=Zeqo(i)/Zb(it)
1     continue
      return
      end
C**** Potp. JV2TR za racun jedin.imp. dir/inv i nul.red. 2-namot.tran.
      SUBROUTINE JV2TR(N2t,BS2t,BT2t,GSS2t,GST2t,uk,S2tn,RS2t,RT2t,Zb,
     * Z2t,Z2tso,Z2tsto,Z2tto)
      INTEGER BS2t(60),BT2t(60),GSS2t(60),GST2t(60)
      DIMENSION uk(60),S2tn(60),RS2t(60),RT2t(60),Zb(20)
      COMPLEX Z2t(60),Z2tso(60),Z2tsto(60),Z2tto(60)
      Xm=1.e4
      do 1 i=1,N2t
      x=uk(i)/S2tn(i)
      Z2t(i)=cmplx(0.,x)
      is=BS2t(i)
      if(GSS2t(i)-1) 3,2,3
2     Z2tto(i)=cmplx(Xm,Xm)
      Z2tsto(i)=cmplx(Xm,Xm)
      Z2tso(i)=Z2t(i)+3.*RS2t(i)/Zb(is)
      go to 1
3     Z2tso(i)=cmplx(Xm,Xm)
      Z2tsto(i)=cmplx(Xm,Xm)
      Z2tto(i)=cmplx(Xm,Xm)
1     continue
      return
      end
C**** Potp. JVVOD za racun dir/inv i nul. impedancija vodova
      SUBROUTINE JVVOD(Nv,BSv,Zb, Zv,Bv,Zvo,Bvo)
      INTEGER BSv(100)
      DIMENSION Zb(20),Bv(100),Bvo(100)
      COMPLEX Zv(100),Zvo(100)
      do 1 i=1,Nv
      is=BSv(i)
      Zv(i)=Zv(i)/Zb(is)
      Zvo(i)=Zvo(i)/Zb(is)
      Bv(i)=Bv(i)*Zb(is)*1.e-6
      Bvo(i)=Bvo(i)*Zb(is)*1.e-6
1     continue
      return
      end
C**** Potp. JV3TR za racun dir/inv i nul. jedin.imp. 3-namot. trans.
      SUBROUTINE JV3TR(N3t,BS3t,BT3t,GSS3t,GST3t,SS3t,ukst,uks3,ukt3,
     *Snst,Sns3,RS3t,RT3t,Zb, Z3t,Z3tso,Z3tsto,Z3tto)
      INTEGER BS3t(70),BT3t(70),GSS3t(70),GST3t(70),SS3t(70)
      DIMENSION ukst(70),uks3(70),ukt3(70),Snst(70),Sns3(70),RS3t(70),
     *RT3t(70),Zb(20)
      COMPLEX Z3t(70),Z3tso(70),Z3tsto(70),Z3tto(70), z1,z2,z3,zz
      Xm=1.e4
      do 1 i=1,N3t
      x=ukst(i)/Snst(i)
      Z3t(i)=cmplx(0.,x)
      x1=(ukst(i)/Snst(i)+uks3(i)/Sns3(i)-ukt3(i)/Sns3(i))/2.
      x2=(ukst(i)/Snst(i)+ukt3(i)/Sns3(i)-uks3(i)/Sns3(i))/2.
      x3=(uks3(i)/Sns3(i)+ukt3(i)/Sns3(i)-ukst(i)/Snst(i))/2.
      is=BS3t(i)
      it=BT3t(i)
      if(SS3t(i)) 2,3,2
2     if(GSS3t(i)-1) 5,4,5
4     x=x1*x2+x1*x3+x2*x3
      x12=x/x3
      x13=x/x2
      x23=x/x1
      Z3tsto(i)=cmplx(0.,x12)
      Z3tso(i)=cmplx(0.,x13)
      Z3tto(i)=cmplx(0.,x23)
      go to 1
5     Z3tsto(i)=cmplx(Xm,Xm)
      Z3tso(i)=cmplx(Xm,Xm)
      Z3tto(i)=cmplx(Xm,Xm)
      go to 1
3     if(GSS3t(i)-1) 6,7,6
6     if(GST3t(i)-1) 5,8,5
8     x=ukt3(i)/Sns3(i)
      Z3tto(i)=cmplx(0.,x)+3.*RT3t(i)/Zb(it)
      Z3tsto(i)=cmplx(Xm,Xm)
      Z3tto(i)=cmplx(Xm,Xm)
      go to 1
7     if(GST3t(i)-1) 13,12,13
12    z1=cmplx(0.,x1)+3.*RS3t(i)/Zb(is)
      z2=cmplx(0.,x2)+3.*RT3t(i)/Zb(it)
      z3=cmplx(0.,x3)
      zz=z1*z2+z1*z3+z2*z3
      Z3tsto(i)=zz/z3
      Z3tso(i)=zz/z2
      Z3tto(i)=zz/z1
      go to 1
13    x=uks3(i)/Sns3(i)
      Z3tso(i)=cmplx(0.,x)+3.*RS3t(i)/Zb(is)
      Z3tsto(i)=cmplx(Xm,Xm)
      Z3tto(i)=cmplx(Xm,Xm)
1     continue
      return
      end
C**** Potp. JVTROS za racun jed. imp. dir/inv i nul. redos. trosila
      SUBROUTINE JVTROS(Nt,BTt,Zt,Zto,Zb)
      INTEGER BTt(40)
      DIMENSION Zb(20)
      COMPLEX Zt(40),Zto(40)
      do 1 i=1,Nt
      it=BTt(i)
      Zt(i)=Zt(i)/Zb(it)
      Zto(i)=Zto(i)/Zb(it)
1     continue
      return
      end
C**** Potp. MATY za racun matrice vlastitih i medjusobnih admitancija
C     cvorista dir/inv redoslijeda
      SUBROUTINE MATY(Nc,Ng,Neq,N2t,Nv,N3t,Nt, Tg,Zg, Seq,Teq,Zeq,
     *S2t,T2t,Z2t, Sv,Tv,Zv,Bv, S3t,T3t,Z3t, Tt,Zt, Z)
      INTEGER Tg(50), Seq(30),Teq(30), S2t(60),T2t(60), Sv(100),Tv(100)
     *, S3t(70),T3t(70), Tt(40)
      DIMENSION Bv(100)
      COMPLEX Zg(50), Zeq(30), Z2t(60), Zv(100), Z3t(70), Zt(40),
     *Z(100,100)
      do 1 i=1,Nc
      do 1 j=1,Nc
      Z(i,j)=cmplx(0.,0.)
1     continue
      if(Ng.eq.0) go to 2
      do 3 l=1,Ng
      i=Tg(l)
      Z(i,i)=Z(i,i)+1./Zg(l)
3     continue
2     if(Neq.eq.0) go to 4
      do 5 l=1,Neq
      i=Seq(l)
      j=Teq(l)
      Z(j,j)=Z(j,j)+1./Zeq(l)
      if(i.eq.0) go to 5
      Z(i,i)=Z(i,i)+1./Zeq(l)
      Z(i,j)=Z(i,j)-1./Zeq(l)
      Z(j,i)=Z(i,j)
5     continue
4     if(N2t.eq.0) go to 6
      do 7 l=1,N2t
      i=S2t(l)
      j=T2t(l)
      Z(i,i)=Z(i,i)+1./Z2t(l)
      Z(j,j)=Z(j,j)+1./Z2t(l)
      Z(i,j)=Z(i,j)-1./Z2t(l)
      Z(j,i)=Z(i,j)
7     continue
6     if(Nv.eq.0) go to 8
      do 9 l=1,Nv
      i=Sv(l)
      j=Tv(l)
      x=Bv(l)/2.
      Z(i,i)=Z(i,i)+cmplx(0.,x)+1./Zv(l)
      Z(j,j)=Z(j,j)+cmplx(0.,x)+1./Zv(l)
      Z(i,j)=Z(i,j)-1./Zv(l)
      Z(j,i)=Z(i,j)
9     continue
8     if(N3t.eq.0) go to 10
      do 11 l=1,N3t
      i=S3t(l)
      j=T3t(l)
      Z(i,i)=Z(i,i)+1./Z3t(l)
      Z(j,j)=Z(j,j)+1./Z3t(l)
      Z(i,j)=Z(i,j)-1./Z3t(l)
      Z(j,i)=Z(i,j)
11    continue
10    if(Nt.eq.0) go to 12
      do 13 l=1,Nt
      i=Tt(l)
      Z(i,i)=Z(i,i)+1./Zt(l)
13    continue
12    return
      end
C**** Potp. MATY0 racuna matricu vlastih imedjusobnih admitancija
C     cvorista nultog redoslijeda
      SUBROUTINE MATY0(Nco,Neq,N2t,Nv,N3t,Nt, Seq,Teq,Zeqo, S2t,T2t,
     *Z2tso,Z2tsto,Z2tto, Sv,Tv,Zvo,Bvo, S3t,T3t,Z3tso,Z3tsto,Z3tto,
     * Tt,Zto, Zo)
      INTEGER Seq(30),Teq(30), S2t(60),T2t(60), Sv(100),Tv(100),
     *S3t(70),T3t(70), Tt(40)
      DIMENSION Bvo(100)
      COMPLEX Zeqo(30), Z2tso(60),Z2tsto(60),Z2tto(70), Zvo(100),
     * Z3tso(70),Z3tsto(70),Z3tto(70), Zto(40), Zo(100,100)
      do 1 i=1,Nco
      do 1 j=1,Nco
      Zo(i,j)=cmplx(0.,0.)
1     continue
      if(Neq.eq.0) go to 2
      do 3 l=1,Neq
      i=Seq(l)
      j=Teq(l)
      Zo(j,j)=Zo(j,j)+1./Zeqo(l)
      if(i.eq.0) go to 3
      Zo(i,i)=Zo(i,i)+1./Zeqo(l)
      Zo(i,j)=Zo(i,j)-1./Zeqo(l)
      Zo(j,i)=Zo(i,j)
3     continue
2     if(N2t.eq.0) go to 4
      do 5 l=1,N2t
      i=S2t(l)
      j=T2t(l)
      x=aimag(Z2tsto(l))
      if(x-100.) 6,7,7
6     Zo(i,i)=Zo(i,i)+1./Z2tsto(l)
      Zo(j,j)=Zo(j,j)+1./Z2tsto(l)
      Zo(i,j)=Zo(i,j)-1./Z2tsto(l)
      Zo(j,i)=Zo(i,j)
      go to 5
7     x=aimag(Z2tso(l))
      if(x-100.) 8,9,9
8     Zo(i,i)=Zo(i,i)+1./Z2tso(l)
      go to 5
9     x=aimag(Z2tto(l))
      if(x-100.) 10,5,5
10    Zo(j,j)=Zo(j,j)+1./Z2tto(l)
5     continue
4     if(Nv.eq.0) go to 11
      do 12 l=1,Nv
      i=Sv(l)
      j=Tv(l)
      x=Bvo(l)/2.
      Zo(i,i)=Zo(i,i)+cmplx(0.,x)+1./Zvo(l)
      Zo(j,j)=Zo(j,j)+cmplx(0.,x)+1./Zvo(l)
      Zo(i,j)=Zo(i,j)-1./Zvo(l)
      Zo(j,i)=Zo(i,j)
12    continue
11    if(N3t.eq.0) go to 13
      do 14 l=1,N3t
      i=S3t(l)
      j=T3t(l)
      x=aimag(Z3tsto(l))
      if(x-100.) 15,16,16
15    Zo(i,i)=Zo(i,i)+1./Z3tsto(l)+1./Z3tso(l)
      Zo(j,j)=Zo(j,j)+1./Z3tsto(l)+1./Z3tto(l)
      Zo(i,j)=Zo(i,j)-1./Z3tsto(l)
      Zo(j,i)=Zo(i,j)
      go to 14
16    x=aimag(Z3tso(l))
      if(x-100.) 17,18,18
17    Zo(i,i)=Zo(i,i)+1./Z3tso(l)
      go to 14
18    x=aimag(Z3tto(l))
      if(x-100.) 19,14,14
19    Zo(j,j)=Zo(j,j)+1./Z3tto(l)
14    continue
13    if(Nt.eq.0) go to 20
      do 21  l=1,Nt
      i=Tt(l)
      Zo(i,i)=Zo(i,i)+1./Zto(l)
21    continue
20    return
      end
C**** Potp. K1GEN za racun raspodjele struja i napona dir/inv redoslijeda
C     u generatorima
      SUBROUTINE K1GEN(k,Iko,Ng,Tg,BTg,Zg,Z,Ib,Un, Idt,Iit,Vdt,Vit)
      REAL Ib(20)
      INTEGER Tg(50),BTg(50)
      DIMENSION Un(20)
      COMPLEX Iko,Zg(50), Z(100,100), Idt(100),Vdt(100), Iit(100),
     *Vit(100), c,a,a2
      a=cmplx(-0.5,0.866)
      a2=cmplx(-0.5,-0.866)
      do 1 l=1,100
      Idt(l)=cmplx(0.,0.)
      Iit(l)=cmplx(0.,0.)
      Vdt(l)=cmplx(0.,0.)
      Vit(l)=cmplx(0.,0.)
1     continue
      do 2 l=1,Ng
      it=BTg(l)
      j=Tg(l)
      c=-Z(j,k)*Iko*Ib(it)/Zg(l)
      Idt(l)=c*(a2-1.)/sqrt(3.)
      Iit(l)=c*(a-1.)/sqrt(3.)
      c=-Z(j,k)*Iko*Un(it)/sqrt(3.)
      Vdt(l)=c*(a2-1.)/sqrt(3.)
      Vit(l)=c*(a-1.)/sqrt(3.)
2     continue
      return
      end
C**** Potp. K1EQ za racun raspodjele struja i napona dir/inv i nultog
C     redoslijeda u nadomjesnim elementima
      SUBROUTINE K1EQ(k,Iko,Neq,Seq,Teq,BSeq,BTeq,Zeq,Zeqo,Z,Zo,Ib,Un,
     * Ids,Idt,Ios,Iot,Vds,Vdt,Vos,Vot)
      REAL Ib(20)
      INTEGER Seq(30),Teq(30),BSeq(30),BTeq(30)
      DIMENSION Un(20)
      COMPLEX Iko,Zeq(30),Zeqo(30),Z(100,100),Zo(100,100), Ids(100),
     *Idt(100),Ios(100),Iot(100),Vds(100),Vdt(100),Vos(100),Vot(100), c
      do 1 l=1,100
      Ids(l)=cmplx(0.,0.)
      Idt(l)=cmplx(0.,0.)
      Ios(l)=cmplx(0.,0.)
      Iot(l)=cmplx(0.,0.)
      Vds(l)=cmplx(0.,0.)
      Vdt(l)=cmplx(0.,0.)
      Vos(l)=cmplx(0.,0.)
      Vot(l)=cmplx(0.,0.)
1     continue
      do 2 l=1,Neq
      i=Seq(l)
      j=Teq(l)
      is=BSeq(l)
      it=BTeq(l)
      if(i) 4,3,4
3     Idt(l)=-Z(j,k)*Iko*Ib(it)/Zeq(l)
      Vdt(l)=-Z(j,k)*Iko*Un(it)/sqrt(3.)
      Iot(l)=-Zo(j,k)*Iko*Ib(it)/Zeqo(l)
      Vot(l)=-Zo(j,k)*Iko*Un(it)/sqrt(3.)
      go to 2
4     c=(Z(j,k)-Z(i,k))*Iko/Zeq(l)
      Ids(l)=c*Ib(is)
      Idt(l)=-c*Ib(it)
      Vds(l)=-Z(i,k)*Iko*Un(is)/sqrt(3.)
      Vdt(l)=-Z(j,k)*Iko*Un(it)/sqrt(3.)
      c=(Zo(j,k)-Zo(i,k))*Iko/Zeqo(l)
      Ios(l)=c*Ib(is)
      Iot(l)=-c*Ib(it)
      Vos(l)=-Zo(i,k)*Iko*Un(is)/sqrt(3.)
      Vot(l)=-Zo(j,k)*Iko*Un(it)/sqrt(3.)
2     continue
      return
      end
C**** Potp K12TR za racun raspodjele struja i napona dir/inv i nultog
C     redoslijeda u dvonamotnim transformatorima
      SUBROUTINE K12TR(k,Iko,N2t,S2t,T2t,BS2t,BT2t,Z2t,Z2tso,Z2tsto,
     *Z2tto,Z,Zo,Ib,Un, Ids,Idt,Iit,Ios,Vds,Vdt,Vit,Vos, Iot,Vot)
      REAL Ib(20)
      INTEGER S2t(60),T2t(60),BS2t(60),BT2t(60)
      DIMENSION Un(20)
      COMPLEX Z2t(60),Z2tso(60),Z2tsto(60),Z2tto(60), Ids(100),Idt(100)
     *,Ios(100),Vds(100),Vdt(100),Vos(100),Iot(100),Vot(100), Iko,c,
     *Z(100,100),Zo(100,100), Iit(100),Vit(100), a,a2
      a=cmplx(-0.5,0.866)
      a2=cmplx(-0.5,-0.866)
      do 1 l=1,100
      Iot(l)=cmplx(0.,0.)
      Vot(l)=cmplx(0.,0.)
      Ids(l)=cmplx(0.,0.)
      Idt(l)=cmplx(0.,0.)
      Iit(l)=cmplx(0.,0.)
      Ios(l)=cmplx(0.,0.)
      Vds(l)=cmplx(0.,0.)
      Vdt(l)=cmplx(0.,0.)
      Vit(l)=cmplx(0.,0.)
      Vos(l)=cmplx(0.,0.)
1     continue
      do 2 l=1,N2t
      i=S2t(l)
      j=T2t(l)
      is=BS2t(l)
      it=BT2t(l)
      c=(Z(j,k)-Z(i,k))*Iko/Z2t(l)
      Ids(l)=c*Ib(is)
      Idt(l)=-c*Ib(it)*(a2-1.)/sqrt(3.)
      Iit(l)=-c*Ib(it)*(a-1.)/sqrt(3.)
      Vds(l)=-Z(i,k)*Iko*Un(is)/sqrt(3.)
      Vdt(l)=-Z(j,k)*Iko*Un(it)/sqrt(3.)*(a2-1.)/sqrt(3.)
      Vit(l)=-Z(j,k)*Iko*Un(it)/sqrt(3.)*(a-1.)/sqrt(3.)
      Vos(l)=-Zo(i,k)*Iko*Un(is)/sqrt(3.)
      x=aimag(Z2tso(l))
      if(x-100.) 5,2,2
5     Ios(l)=-Zo(i,k)*Iko*Ib(is)/Z2tso(l)
2     continue
      return
      end
C**** Potp. K1VOD racuna stanje dir/inv i nultog redoslijeda u vodovima
      SUBROUTINE K1VOD(k,Iko,Nv,Sv,Tv,BSv,Zv,Zvo,Z,Zo,Ib,Un, Ids,Idt,
     *Ios,Iot,Vds,Vdt,Vos,Vot, Bv,Bvo)
      REAL Ib(20)
      INTEGER Sv(100),Tv(100),BSv(100)
      DIMENSION Un(20), Bv(100),Bvo(100)
      COMPLEX Iko, Zv(100),Zvo(100),Z(100,100),Zo(100,100), Ids(100),
     *Idt(100),Ios(100),Iot(100),Vds(100),Vdt(100),Vos(100),Vot(100)
      do 1 l=1,100
      Ids(l)=cmplx(0.,0.)
      Idt(l)=cmplx(0.,0.)
      Ios(l)=cmplx(0.,0.)
      Iot(l)=cmplx(0.,0.)
      Vds(l)=cmplx(0.,0.)
      Vdt(l)=cmplx(0.,0.)
      Vos(l)=cmplx(0.,0.)
      Vot(l)=cmplx(0.,0.)
1     continue
      do 2 l=1,Nv
      i=Sv(l)
      j=Tv(l)
      is=BSv(l)
      x=Bv(l)/2.
      Ids(l)=(Z(j,k)-Z(i,k))*Iko*Ib(is)/Zv(l)-Z(i,k)*Iko*Ib(is)*
     *cmplx(0.,x)
      Idt(l)=(Z(i,k)-Z(j,k))*Iko*Ib(is)/Zv(l)-Z(j,k)*Iko*Ib(is)*
     *cmplx(0.,x)
      Vds(l)=-Z(i,k)*Iko*Un(is)/sqrt(3.)
      Vdt(l)=-Z(j,k)*Iko*Un(is)/sqrt(3.)
      x=Bvo(l)/2.
      Ios(l)=(Zo(j,k)-Zo(i,k))*Iko*Ib(is)/Zvo(l)-Zo(i,k)*Iko*Ib(is)*
     *cmplx(0.,x)
      Iot(l)=(Zo(i,k)-Zo(j,k))*Iko*Ib(is)/Zvo(l)-Zo(j,k)*Iko*Ib(is)*
     *cmplx(0.,x)
      Vos(l)=-Zo(i,k)*Iko*Un(is)/sqrt(3.)
      Vot(l)=-Zo(j,k)*Iko*Un(is)/sqrt(3.)
2     continue
      return
      end
C**** Potp. K13TR racuna stanja dir./inv. i nultog redoslijeda
C     u tronamotnim transformatorima
      SUBROUTINE K13TR(k,Iko,N3t,S3t,T3t,BS3t,BT3t,Z3t,Z3tso,Z3tsto,
     *Z3tto,Z,Zo,Ib,Un, Ids,Idt,Ios,Iot,Vds,Vdt,Vos,Vot)
      REAL Ib(20)
      INTEGER S3t(70),T3t(70),BS3t(70),BT3t(70)
      DIMENSION Un(20)
      COMPLEX Iko,Z3t(70),Z3tso(70),Z3tsto(70),Z3tto(70),Z(100,100),
     *Zo(100,100), Ids(100),Idt(100),Ios(100),Iot(100),Vds(100),
     *Vdt(100),Vos(100),Vot(100), c
      do 1 l=1,100
      Ids(l)=cmplx(0.,0.)
      Idt(l)=cmplx(0.,0.)
      Ios(l)=cmplx(0.,0.)
      Iot(l)=cmplx(0.,0.)
      Vds(l)=cmplx(0.,0.)
      Vdt(l)=cmplx(0.,0.)
      Vos(l)=cmplx(0.,0.)
      Vot(l)=cmplx(0.,0.)
1     continue
      do 2 l=1,N3t
      i=S3t(l)
      j=T3t(l)
      is=BS3t(l)
      it=BT3t(l)
      c=(Z(j,k)-Z(i,k))*Iko/Z3t(l)
      Ids(l)=c*Ib(is)
      Idt(l)=-c*Ib(it)
      Vds(l)=-Z(i,k)*Iko*Un(is)/sqrt(3.)
      Vdt(l)=-Z(j,k)*Iko*Un(it)/sqrt(3.)
      Vos(l)=-Zo(i,k)*Iko*Un(is)/sqrt(3.)
      Vot(l)=-Zo(j,k)*Iko*Un(it)/sqrt(3.)
      x=aimag(Z3tsto(l))
      if(x-100.) 3,4,4
3     Ios(l)=((Zo(j,k)-Zo(i,k))/Z3tsto(l)-Zo(i,k)/Z3tso(l))*Iko*Ib(is)
      Iot(l)=((Zo(i,k)-Zo(j,k))/Z3tsto(l)-Zo(j,k)/Z3tto(l))*Iko*Ib(it)
      go to 2
4     x=aimag(Z3tso(l))
      if(x-100.) 5,6,6
5     Ios(l)=-Zo(i,k)*Iko*Ib(is)/Z3tso(l)
      go to 2
6     x=aimag(Z3tto(l))
      if(x-100.) 7,2,2
7     Iot(l)=-Zo(j,k)*Iko*Ib(it)/Z3tto(l)
2     continue
      return
      end
C**** Potp. K1TROS racuna stanja dir./inv. i nultog redos. u trosilima
      SUBROUTINE K1TROS(k,Iko,Nt,Tt,BTt,Zt,Zto,Z,Zo,Ib,Un, Idt,Iot,Vdt,
     *Vot)
      REAL Ib(20)
      INTEGER Tt(40),BTt(40)
      DIMENSION Un(20)
      COMPLEX Iko, Zt(40),Zto(40),Z(100,100),Zo(100,100), Idt(100),
     *Iot(100),Vdt(100),Vot(100)
      do 1 l=1,100
      Idt(l)=cmplx(0.,0.)
      Iot(l)=cmplx(0.,0.)
      Vdt(l)=cmplx(0.,0.)
      Vot(l)=cmplx(0.,0.)
1     continue
      do 2 l=1,Nt
      j=Tt(l)
      it=BTt(l)
      Idt(l)=-Z(j,k)*Iko*Ib(it)/Zt(l)
      Vdt(l)=-Z(j,k)*Iko*Un(it)/sqrt(3.)
      Iot(l)=-Zo(j,k)*Iko*Ib(it)/Zto(l)
      Vot(l)=-Zo(j,k)*Iko*Un(it)/sqrt(3.)
2     continue
      return
      end
C**** Potp. INVER racuna inverznu matricu
      SUBROUTINE INVER(N,Z)
      COMPLEX Z(100,100)
      do 7 ni=1,N
      do 4 i=1,N
      if(i-ni) 1,4,1
1     do 3 j=1,N
      if(j-ni) 2,3,2
2     Z(i,j)=Z(i,j)-Z(ni,j)*Z(i,ni)/Z(ni,ni)
3     continue
      j=ni
      Z(i,j)=-Z(i,ni)/Z(ni,ni)
4     continue
      i=ni
      do 6 j=1,N
      if(j-ni) 5,6,5
5     Z(i,j)=Z(ni,j)/Z(ni,ni)
6     continue
      j=ni
7     Z(i,j)=1./Z(ni,ni)
      return
      end
C**** Potp. ZMK za pretvaranje kompleksog broja u modul i kut
      SUBROUTINE ZMK(Z,Zmod,akut)
      COMPLEX Z
      pi=3.14159
      r=real(Z)
      ai=aimag(Z)
      Zmod=sqrt(r**2+ai**2)
      if(Zmod) 2,1,2
1     akut=0.
      go to 3
2     if(r) 5,4,5
4     if(ai) 7,7,6
6     akut=90.
      go to 3
7     akut=270.
      go to 3
5     if(ai) 9,8,9
8     if(r) 11,11,10
10    akut=0.
      go to 3
11    akut=180.
      go to 3
9     if(r) 13,13,12
12    akut=atan(ai/r)*180./pi
      go to 3
13    akut=atan(ai/r)*180./pi+180.
3     return
      end
