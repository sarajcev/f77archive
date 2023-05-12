C     Program KAUKS za proracun kratkog spoja u kabelu i susjednom
C                           postrojenju
      REAL L,Lcr,It
      DIMENSION x(3),y(3)
      COMPLEX Zua,Zub,Z1a,Z1b,Z0a,Z0b,Zsa,Zsb,Zma,Zmb,E(10),str,pom,
     *Vp(5),Vk(5),Vpf(5),Vkf(5),Vpu(5),Vku(5),Z(10,10),Ip(5),Ipf(5),
     *Ipu(5),Vf,Ik1,Ipet(10),Ivf(5),Ivu(5),Iu,Iuf,Iuu
      OPEN(unit=5,file='KAUKS.dat')
      OPEN(unit=6,file='KAUKS.prn')
      READ(5,100) rv,rp,Rv1,Rp1,L,ro
100   format(8f10.3)
      write(6,101) rv,rp,Rv1,Rp1,L,ro
101   format(t30,'ULAZNI PODACI',//,' Rv =',f5.1,' mm',/,' Rp =',f5.1,
     *' mm',/,' Rv1 =',f7.3,' Oma/km',/,' Rp1 =',f7.3,' Oma/km',/,
     *' l =',f7.3,' km',/,' ro =',f7.1,' Om*m',//,' Faza',t10,'X (m)',
     *t20,'Y (m)')
      DO 1 i=1,3
      READ(5,100) x(i),y(i)
1     write(6,102) i,x(i),y(i)
102   format(i4,t8,f9.3,t18,f9.3)
      READ(5,103) ia,ib,Zua,Zub
103   format(2i5,6f10.3)
      write(6,104) ia,ib,Zua,Zub
104   format(/,' Indikatori uzemlj. plasteva:',/,' ia =',i2,5x,'ib =',
     *i2,/,' Impedancije uzemlj. uzemljivaca:',/,' Za =',2f8.2,' Oma',
     */,' Zb =',2f8.2,' Oma')
      READ(5,100) It,Vt,fi,f
      write(6,105) It,Vt,fi,f
105   format(/,' It =',f7.1,' A',/,' Vt =',f7.2,' kV',/,' fi =',f6.1,
     *' stupnj.',/,' f =',f6.1,' Hz')
      READ(5,100) Z1a,Z0a,Z1b,Z0b
      write(6,106) Z1a,Z0a,Z1b,Z0b
106   format(/,' Mreza a:',/,' Z1 =',2f10.3,' Oma',/,' Z0 =',2f10.3,
     *' Oma',//' Mreza b:',/,' Z1 =',2f10.3,' Oma',/,' Z0 =',2f10.3,
     *' Oma')
      READ(5,103) KSpo,KSa,Lcr
      write(6,107) KSpo,KSa,Lcr
107   format(/,' K.S. u postr.:',i3,/,' K.S. u mrezi a:',i2,/,' Lcr =',
     *f7.3,' km',//,t30,'REZULTATI PRORACUNA',/)
c*****Konstante programa
      pi=3.14159
      Rz=pi*pi*f*1.e-4
      a=4.*pi*f*1.e-4
      De=658.*sqrt(ro/f)
      fi=fi*pi/180.
c*****Racun vlast. i medju. impedancija ekv. faznih vodica za mreze a i b
      Zsa=(Z0a+2.*Z1a)/3.
      Zsb=(Z0b+2.*Z1b)/3.
      Zma=(Z0a-Z1a)/3.
      Zmb=(Z0b-Z1b)/3.
C*****Racun STACIONARNOG STANJA
      DO 2 i=1,3
      E(i)=cmplx(0.,0.)
      DO 3 j=1,3
      zr=It*cos(-fi-(j-1)*2.*pi/3.)
      zi=It*sin(-fi-(j-1)*2.*pi/3.)
      str=cmplx(zr,zi)
      if(i.eq.j) D=Rp*1.e-3
      if(i.ne.j) D=sqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
      zr=Rz*L
      zi=a*L*alog(De/D)
      E(i)=E(i)+str*cmplx(zr,zi)
3     continue
      if(ia) 5,4,5
4     Vp(i)=E(i)*1.e-3
      Vk(i)=cmplx(0.,0.)
      go to 2
5     if(ib) 7,6,7
6     Vp(i)=cmplx(0.,0.)
      Vk(i)=-E(i)*1.e-3
      go to 2
7     Vp(i)=cmplx(0.,0.)
      Vk(i)=cmplx(0.,0.)
2     continue
      write(6,108)
108   format(' STACIONARNO STANJE:',/)
      DO 8 i=1,3
      pom=Vp(i)
      call ZMK(pom,b,c)
      pom=Vk(i)
      call ZMK(pom,zr,zi)
      write(6,109) i,b,c,zr,zi
109   format(' Faza',i2,5x,'Vp =',f6.2,'/',f6.1,' kV',5x,'Vk =',f6.2,
     *'/',f6.1,' kV')
8     continue
c*****Racun matrice vlast. i medju. impedancija plasteva kabela ako su
c     isti uzemljeni na oba kraja
      if(ia) 9,10,9
9     if(ib) 11,10,11
10    DO 12 i=1,3
      Ip(i)=cmplx(0.,0.)
12    continue
      go to 13
11    DO 14 j=1,3
      DO 14 i=1,j
      if(i-j) 16,15,16
15    zr=(Rp1+Rz)*L
      zi=a*L*alog(De*1.e3/Rp)
      Z(i,i)=cmplx(zr,zi)+Zua+Zub
      go to 14
16    D=sqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
      zr=Rz*L
      zi=a*L*alog(De/D)
      Z(i,j)=cmplx(zr,zi)+Zua+Zub
      Z(j,i)=Z(i,j)
14    continue
      write(6,110)
110   format(/,' Matrica [Z] plasteva kabela:')
      DO 17 i=1,3
      write(6,111) (Z(i,j),j=1,i)
111   format(8f10.3)
17    continue
      n=3
      call INVER(n,Z)
      write(6,112)
112   format(/,' Inverzna matr. [Z] plast. kabela:')
      DO 18 i=1,3
      write(6,111) (Z(i,j),j=1,i)
18    continue
c*****Proracun struja u plas. kabela
      DO 19 i=1,3
      Ip(i)=cmplx(0.,0.)
      DO 19 j=1,3
      Ip(i)=Ip(i)+Z(i,j)*E(j)
19    continue
13    Iu=cmplx(0.,0.)
      write(6,113)
113   format(/,' Struje u plastevima kabela:')
      DO 20 i=1,3
      pom=Ip(i)
      Iu=Iu+pom
      call ZMK(pom,b,c)
      write(6,114) i,b,c
114   format(' Ip(',i1,') =',f7.2,'/',f6.1,' A')
20    continue
      call ZMK(Iu,b,c)
      write(6,115) b,c
115   format(/,' Iu =',f7.2,'/',f6.1,' A')
c*****Odredjivanje fiktivnog napona Vf na mjestu kvara
      b=It*cos(fi)
      c=-It*sin(fi)
      zr=Rv1*(L-Lcr)
      zi=a*(L-Lcr)*alog(Rp/Rv)
      Vf=Vk(1)-cmplx(Vt,0.)-cmplx(b,c)*cmplx(zr,zi)*1.e-3-Ip(1)*Rp1*(L-
     *Lcr)*1.e-3
      call ZMK(Vf,b,c)
      write(6,116) b,c
116   format(/,' Vf =',f6.2,'/',f6.1,' kV')
C*****Racun stanja u FIKTIVNOM SUSTAVU
      DO 21 j=1,3
      E(j)=cmplx(0.,0.)
      DO 21 i=1,j
      if(i-j) 23,22,23
22    zr=(Rv1+Rz)*L
      zi=a*L*alog(De*1.e3/Rv)
      Z(i,i)=cmplx(zr,zi)+Zsa+Zsb+Zua+Zub
      go to 21
23    D=sqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
      zr=Rz*L
      zi=a*L*alog(De/D)
      Z(i,j)=cmplx(zr,zi)+Zma+Zmb+Zua+Zub
      Z(j,i)=Z(i,j)
21    continue
      zr=Rv1+Rp1
      zi=a*alog(Rp/Rv)
      pom=cmplx(zr,zi)
      zr=Rv1
      str=cmplx(zr,zi)
      if(ia) 24,25,24
24    if(ib) 27,26,27
25    n=4
      E(n)=-Vf*1.e3
      DO 28 i=1,n
      if(i-1) 30,29,30
29    Z(i,n)=Zsb+str*(L-Lcr)
      go to 31
30    if(i-n) 32,33,32
32    Z(i,n)=Zmb
31    Z(n,i)=Z(i,n)
      go to 28
33    Z(n,n)=Zsb+pom*(L-Lcr)
28    continue
      go to 34
26    n=4
      E(n)=Vf*1.e3
      DO 35 i=1,n
      if(i-1) 37,36,37
36    Z(i,n)=Zsa+str*Lcr
      go to 38
37    if(i-n) 39,40,39
39    Z(i,n)=Zma
38    Z(n,i)=Z(i,n)
      go to 35
40    Z(n,n)=Zsa+pom*Lcr
35    continue
      go to 34
27    n=8
      DO 41 i=4,6
41    E(i)=cmplx(0.,0.)
      E(n-1)=Vf*1.e3
      E(n)=-Vf*1.e3
      DO 42 j=7,n
      DO 42 i=1,n
      Z(i,j)=cmplx(0.,0.)
      Z(j,i)=Z(i,j)
42    continue
      DO 43 j=4,6
      DO 43 i=1,j
      if(i-3) 44,44,45
44    if(i-(j-3)) 47,46,47
46    D=Rp*1.e3
      go to 48
47    D=sqrt((x(i)-x(j-3))**2+(y(i)-y(j-3))**2)
48    zr=Rz*L
      zi=a*L*alog(De/D)
      Z(i,j)=cmplx(zr,zi)+Zua+Zub
      Z(j,i)=Z(i,j)
      go to 43
45    if(i-j) 50,49,50
50    D=sqrt((x(i-3)-x(j-3))**2+(y(i-3)-y(j-3))**2)
      go to 48
49    zr=(Rp1+Rz)*L
      zi=a*L*alog(De*1.e3/Rp)
      Z(i,i)=cmplx(zr,zi)+Zua+Zub
43    continue
      Z(1,7)=Zsa+str*Lcr
      Z(7,1)=Z(1,7)
      Z(1,8)=Zsb+str*(L-Lcr)
      Z(8,1)=Z(1,8)
      Z(7,7)=Zsa+pom*Lcr
      Z(8,8)=Zsb+pom*(L-Lcr)
      Z(4,7)=cmplx(-Rp1*Lcr,0.)
      Z(7,4)=Z(4,7)
      Z(4,8)=cmplx(-Rp1*(L-Lcr),0.)
      Z(8,4)=Z(4,8)
      Z(2,7)=Zma
      Z(7,2)=Zma
      Z(3,7)=Zma
      Z(7,3)=Zma
      Z(2,8)=Zmb
      Z(3,8)=Zmb
      Z(8,2)=Zmb
      Z(8,3)=Zmb
34    write(6,117)
117   format(//,' STANJE U FIKTIVNOM SUSTAVU:',//,' Matrica [Z] petlji:'
     *)
      DO 51 i=1,n
      write(6,111) (Z(i,j),j=1,i)
51    continue
      call INVER(n,Z)
      write(6,118)
118   format(/,' Inverzna matr. [Z] petlji:')
      DO 52 i=1,n
      write(6,111) (Z(i,j),j=1,i)
52    continue
c*****Racun struja petlji
      DO 53 i=1,n
      Ipet(i)=cmplx(0.,0.)
      DO 53 j=1,n
      Ipet(i)=Ipet(i)+Z(i,j)*E(j)
53    continue
      write(6,119) (Ipet(i),i=1,n)
119   format(/,' Struje petlji (A):',/,(2f10.3))
c*****Racun struja fiktivnog sustava
      Ivf(3)=Ipet(2)
      Ivf(4)=Ipet(3)
      if(ia) 55,54,55
54    Ivf(1)=Ipet(1)
      Ivf(2)=-Ipet(1)-Ipet(4)
      Ipf(1)=cmplx(0.,0.)
      Ipf(2)=-Ipet(4)
      go to 56
55    if(ib) 58,57,58
57    Ivf(1)=Ipet(1)+Ipet(4)
      Ivf(2)=-Ipet(1)
      Ipf(1)=Ipet(4)
      Ipf(2)=cmplx(0.,0.)
56    Ipf(3)=cmplx(0.,0.)
      Ipf(4)=cmplx(0.,0.)
      go to 59
58    Ivf(1)=Ipet(1)+Ipet(7)
      Ivf(2)=-Ipet(1)-Ipet(8)
      Ipf(1)=Ipet(7)-Ipet(4)
      Ipf(2)=Ipet(4)-Ipet(8)
      Ipf(3)=-Ipet(5)
      Ipf(4)=-Ipet(6)
59    Iuf=Ipf(1)+Ipf(3)+Ipf(4)-Ivf(1)-Ivf(3)-Ivf(4)
      DO 60 i=1,3
      if(i-1) 62,61,62
61    pom=Ivf(i)
      call ZMK(pom,zr,zi)
      pom=Ivf(i+1)
      call ZMK(pom,b,c)
      write(6,120) i,zr,zi,b,c
120   format(/,' Faza',i2,5x,'Iv(a) =',f7.2,'/',f6.1,' A',5x,'Iv(b) =',
     *f7.2,'/',f6.1,' A')
      go to 60
62    ii=i+1
      pom=Ivf(ii)
      call ZMK(pom,zr,zi)
      write(6,121) i,zr,zi
121   format(' Faza',i2,5x,'Iv =',f7.2,'/',f6.1,' A')
60    continue
      Ik1=Ivf(1)+Ivf(2)
      call ZMK(Ik1,zr,zi)
      write(6,122) zr,zi
122   format(/,' Ik1 =',f7.2,'/',f6.1,' A')
      DO 63 i=1,3
      if(i-1) 65,64,65
64    pom=Ipf(i)
      call ZMK(pom,zr,zi)
      pom=Ipf(i+1)
      call ZMK(pom,b,c)
      write(6,123) i,zr,zi,b,c
123   format(/,' Faza',i2,5x,'Ip(a) =',f7.2,'/',f6.1,' A',5x,'Ip(b) =',
     *f7.2,'/',f6.1,' A')
      go to 63
65    ii=i+1
      pom=Ipf(ii)
      call ZMK(pom,zr,zi)
      write(6,124) i,zr,zi
124   format(' Faza',i2,5x,'Ip =',f7.2,'/',f6.1,' A')
63    continue
      call ZMK(Iuf,zr,zi)
      write(6,125) zr,zi
125   format(/,' Iu =',f7.2,'/',f6.1,' A',/)
c*****Odredjivanje napona na krajevima plasta u fiktivnom sustavu
      DO 66 i=1,3
      Vpf(i)=cmplx(0.,0.)
      Vkf(i)=cmplx(0.,0.)
      if(ia) 68,67,68
67    if(i-1) 70,69,70
69    zr=(Rp1+Rz)*(L-Lcr)
      D=Rp*1.e-3
      go to 71
70    D=sqrt((x(i)-x(1))**2+(y(i)-y(1))**2)
      zr=Rz*(L-Lcr)
71    zi=a*(L-Lcr)*alog(De/D)
      Vpf(i)=Vpf(i)+cmplx(zr,zi)*Ipf(2)*1.e-3
      DO 72 j=1,4
      if(j-1) 74,73,74
73    b=Lcr
      c=1.
      go to 75
74    if(j-2) 77,76,77
76    b=L-Lcr
      c=-1.
75    if(i-1) 79,78,79
78    D=Rp*1.e-3
      go to 80
79    D=sqrt((x(i)-x(1))**2+(y(i)-y(1))**2)
80    zr=Rz*b
      zi=a*b*alog(De/D)
      Vpf(i)=Vpf(i)+c*cmplx(zr,zi)*Ivf(j)*1.e-3
      go to 72
77    if(j-1-i) 82,81,82
81    D=Rp*1.e-3
      go to 83
82    D=sqrt((x(i)-x(j-1))**2+(y(i)-y(j-1))**2)
83    zr=Rz*L
      zi=a*L*alog(De/D)
      Vpf(i)=Vpf(i)+cmplx(zr,zi)*Ivf(j)*1.e-3
72    continue
      Vpf(i)=Vpf(i)-Iuf*(Zua+Zub)*1.e-3
      go to 66
68    if(ib) 66,84,66
84    if(i-1) 86,85,86
85    zr=(Rp1+Rz)*Lcr
      D=Rp*1.e-3
      go to 87
86    zr=Rz*Lcr
      D=sqrt((x(i)-x(1))**2+(y(i)-y(1))**2)
87    zi=a*Lcr*alog(De/D)
      Vkf(i)=Vkf(i)+cmplx(zr,zi)*Ipf(1)*1.e-3
      DO 88 j=1,4
      if(j-1) 90,89,90
89    b=Lcr
      c=-1.
      go to 91
90    if(j-2) 93,92,93
92    b=L-Lcr
      c=1.
91    if(i-1) 95,94,95
94    D=Rp*1.e-3
      go to 96
95    D=sqrt((x(i)-x(1))**2+(y(i)-y(1))**2)
96    zr=Rz*b
      zi=a*b*alog(De/D)
      Vkf(i)=Vkf(i)+c*cmplx(zr,zi)*Ivf(j)*1.e-3
      go to 88
93    if(j-1-i) 98,97,98
97    D=Rp*1.e-3
      go to 99
98    D=sqrt((x(i)-x(j-1))**2+(y(i)-y(j-1))**2)
99    zr=Rz*L
      zi=a*L*alog(De/D)
      Vkf(i)=Vkf(i)-cmplx(zr,zi)*Ivf(j)*1.e-3
88    continue
      Vkf(i)=Vkf(i)+Iuf*(Zua+Zub)*1.e-3
66    continue
      DO 200 i=1,3
      pom=Vpf(i)
      call ZMK(pom,zr,zi)
      pom=Vkf(i)
      call ZMK(pom,b,c)
      write(6,109) i,zr,zi,b,c
200   continue
c*****Odredjivanje rezultirajuceg stanja - STANJE U KRATKOM SPOJU
      write(6,126)
126   format(//,' REZULTIRAJUCE STANJE (stanje pri KS-u)')
c-----Racun rezultirajucih struja u faznim vodicima
      DO 201 i=1,3
      zr=It*cos(-fi-(i-1)*2.*pi/3.)
      zi=It*sin(-fi-(i-1)*2.*pi/3.)
      if(i-1) 203,202,203
202   Ivu(i)=Ivf(i)+cmplx(zr,zi)
      Ivu(i+1)=Ivf(i+1)-cmplx(zr,zi)
      pom=Ivu(i)
      call ZMK(pom,zr,zi)
      pom=Ivu(i+1)
      call ZMK(pom,b,c)
      write(6,120) i,zr,zi,b,c
      go to 201
203   ii=i+1
      Ivu(ii)=Ivf(ii)+cmplx(zr,zi)
      pom=Ivu(ii)
      call ZMK(pom,zr,zi)
      write(6,121) i,zr,zi
201   continue
c-----Racun rezultirajucih struja u plastevima
      DO 204 i=1,3
      if(i-1) 206,205,206
205   Ipu(i)=Ipf(i)+Ip(i)
      Ipu(i+1)=Ipf(i+1)-Ip(i)
      pom=Ipu(i)
      call ZMK(pom,zr,zi)
      pom=Ipu(i+1)
      call ZMK(pom,b,c)
      write(6,123) i,zr,zi,b,c
      go to 204
206   ii=i+1
      Ipu(ii)=Ipf(ii)+Ip(i)
      pom=Ipu(ii)
      call ZMK(pom,zr,zi)
      write(6,124) i,zr,zi
204   continue
      Iuu=Iuf+Iu
      call ZMK(Iuu,zr,zi)
      write(6,125) zr,zi
c-----Racun rezultirajucih napona na krajevima plasteva
      DO 207 i=1,3
      Vpu(i)=Vp(i)+Vpf(i)
      Vku(i)=Vk(i)+Vkf(i)
      pom=Vpu(i)
      call ZMK(pom,zr,zi)
      pom=Vku(i)
      call ZMK(pom,b,c)
      write(6,109) i,zr,zi,b,c
207   continue
C+++++Racun K.S. U POSTROJENJU
      if(KSpo.EQ.0) go to 208
      n=4
      if(KSa) 210,209,210
209   Vf=cmplx(-Vt,0.)
      E(4)=-Vf*1.e3
      Z(1,4)=Zsb
      Z(4,1)=Z(1,4)
      Z(4,4)=Zsb
      Z(2,4)=Zmb
      Z(4,2)=Zmb
      Z(3,4)=Zmb
      Z(4,3)=Zmb
      go to 211
210   Vf=cmplx(Vt,0.)
      DO 212 i=1,3
      zr=It*cos(-fi-(i-1)*2.*pi/3.)
      zi=It*sin(-fi-(i-1)*2.*pi/3.)
      str=cmplx(zr,zi)
      if(i-1) 214,213,214
213   D=Rv*1.e-3
      zr=(Rv1+Rz)*L
      go to 215
214   D=sqrt((x(i)-x(1))**2+(y(i)-y(1))**2)
      zr=Rz*L
215   zi=a*L*alog(De/D)
      Vf=Vf-cmplx(zr,zi)*str*1.e-3
212   continue
      E(4)=Vf*1.e3
      Z(1,4)=Zsa
      Z(4,1)=Z(1,4)
      Z(4,4)=Zsa
      Z(2,4)=Zma
      Z(4,2)=Zma
      Z(3,4)=Zma
      Z(4,3)=Zma
211   DO 216 j=1,3
      E(j)=cmplx(0.,0.)
      DO 216 i=1,j
      if(i-j) 218,217,218
217   zr=(Rv1+Rz)*L
      zi=a*L*alog(De*1.e3/Rv)
      Z(i,i)=cmplx(zr,zi)+Zsa+Zsb+Zua+Zub
      go to 216
218   D=sqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
      zr=Rz*L
      zi=a*L*alog(De/D)
      Z(i,j)=cmplx(zr,zi)+Zma+Zmb+Zua+Zub
      Z(j,i)=Z(i,j)
216   continue
      write(6,150)
150   format(//,t30,'KRATKI SPOJ U POSTROJENJU',//,' STANJE U FIKTIVNOM
     *SUSTAVU:',//,' Matrica [Z] petlji:')
      DO 219 i=1,n
      write(6,111) (Z(i,j),j=1,i)
219   continue
      call INVER(n,Z)
      write(6,151)
151   format(/,' Inverzna matr. [Z] petlji:')
      DO 220 i=1,n
      write(6,111) (Z(i,j),j=1,i)
220   continue
c-----Racun struja petlji
      DO 221 i=1,n
      Ipet(i)=cmplx(0.,0.)
      DO 221 j=1,n
      Ipet(i)=Ipet(i)+Z(i,j)*E(j)
221   continue
      write(6,119) (Ipet(i),i=1,n)
c-----Odredjivanje struja u faznim vodicima fiktivnog sustava
      DO 222 i=1,3
      Ivf(i)=Ipet(i)
      pom=Ivf(i)
      call ZMK(pom,zr,zi)
      write(6,121) i,zr,zi
222   continue
      if(KSa.eq.0) Ik1=-Ipet(4)
      if(KSa.ne.0) Ik1=Ipet(4)
      call ZMK(Ik1,zr,zi)
      write(6,122) zr,zi
      Iuf=cmplx(0.,0.)
      DO 223 i=1,3
223   Iuf=Iuf-Ivf(i)
      call ZMK(Iuf,zr,zi)
      write(6,125) zr,zi
c-----Odredjivanje napona na krajevima plasta u fiktivnom sustavu
      DO 224 i=1,3
      pom=-Iuf*(Zua+Zub)
      DO 225 j=1,3
      if(i.eq.j) D=Rp*1.e-3
      if(i.ne.j) D=sqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
      zr=Rz*L
      zi=a*L*alog(De/D)
      pom=pom+cmplx(zr,zi)*Ivf(j)
225   continue
      if(KSa.eq.0) Vkf(i)=-pom*1.e-3
      if(KSa.ne.0) Vpf(i)=pom*1.e-3
224   continue
      if(KSa) 227,226,227
226   DO 229 i=1,3
      Vpf(i)=cmplx(0.,0.)
229   continue
      go to 228
227   DO 230 i=1,3
      Vkf(i)=cmplx(0.,0.)
230   continue
228   DO 231 i=1,3
      pom=Vpf(i)
      call ZMK(pom,zr,zi)
      pom=Vkf(i)
      call ZMK(pom,b,c)
      write(6,109) i,zr,zi,b,c
231   continue
C+++++Racun REZULTIRAJUCEG STANJA
      write(6,160)
160   format(//' REZULTIRAJUCE STANJE:',/)
c-----Rracun rezultirajucih struja
      DO 232 i=1,3
      zr=It*cos(-fi-(i-1)*2.*pi/3.)
      zi=It*sin(-fi-(i-1)*2.*pi/3.)
      Ivu(i)=Ivf(i)+cmplx(zr,zi)
      pom=Ivu(i)
      call ZMK(pom,b,c)
      write(6,121) i,b,c
232   continue
      do 233 i=1,3
      Vpu(i)=Vp(i)+Vpf(i)
      Vku(i)=Vk(i)+Vkf(i)
      pom=Vpu(i)
      call ZMK(pom,zr,zi)
      pom=Vku(i)
      call ZMK(pom,b,c)
      write(6,109) i,zr,zi,b,c
233   continue
208   stop
      end
c*****Potprogram  I N V E R
      subroutine INVER(n,Z)
      complex Z(10,10)
      do 7 ni=1,n
      do 4 i=1,n
      if(i-ni) 1,4,1
1     do 3 j=1,n
      if(j-ni) 2,3,2
2     Z(i,j)=Z(i,j)-Z(ni,j)*Z(i,ni)/Z(ni,ni)
3     continue
      j=ni
      Z(i,j)=-Z(i,ni)/Z(ni,ni)
4     continue
      i=ni
      do 6 j=1,n
      if(j-ni) 5,6,5
5     Z(i,j)=Z(ni,j)/Z(ni,ni)
6     continue
      j=ni
7     Z(i,j)=1./Z(ni,ni)
      return
      end
c*****Potprogram  Z M K
      subroutine ZMK(z,zmod,akut)
      complex z
      pi=3.14159
      r=real(z)
      ai=aimag(z)
      zmod=sqrt(r**2+ai**2)
      if(zmod) 2,1,2
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