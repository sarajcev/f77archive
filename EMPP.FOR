C     Program "EMPP" za analizu elektromagnetskih prijelaznih stanja
C                       u elektricnim mrezama
      integer p(50),q(50)
      real ivpqp(50),ipq,iqp,ivpp(20,500),ivqp(20,500),ipqp,itot(50)
      dimension z(20),tau(20),g(50),kon(50),par(50),ndt(20),icbop(10),
     *e0(10),t2(10),s(10),a(10),b(10),f(10),psi(10),ictis(10),igtis(10),
     *igtn(10),m(20),yaa(50,50),yab(50,10),tt(500),v(50),ucvor(10,500),
     *strpq(10,500),strqp(10,500)
      open(unit=1,file='emppul.dat')
      open(unit=2,file='emppiz.dat')
c*****Ucitavanje opcih podataka
      read(1,100) ngrp,ngkp,ngstr,ngnap
100   format(16i5)
      write(2,101)ngrp,ngkp,ngstr,ngnap
101   format(t30,'PROGRAM E M P P',//,' Broj grana s rasp. param.=',i3,/
     *,' Broj grana s konc. param.=',i3,/,' Broj grana u kojima se tiska
     *ju struje=',i3,/,' Broj grana u kojima se tiskaju naponi=',i3)
      ng=ngrp+ngkp
      read(1,100) nca,ncb,k,kop,ncnap
      write(2,102)nca,ncb,k,kop,ncnap
102   format(/,' Broj cvorova s nepoznatim naponima=',i3,/,' Broj cvorov
     *a s poznatim naponima=',i3,/,' Broj cvora u kojem je nastupio udar
     * groma=',i3,/,' Opcija struje groma=',i3,/,' Broj cvorova u kojima
     * se tiskaju naponi=',i3)
      read(1,103) dt,tmax,nh
103   format(2f10.3,i5)
      nt=tmax/dt+1
      write(2,104)dt,tmax,nh,nt
104   format(/,' Vremenski interval dt=',f10.3,' (mikrosec.)',/,' Vrijem
     *e promatranja pojave Tmax=',f10.2,' (mikrosec)',/,' NH =',i5,/,
     *' Broj vremenskih intervala nt=',i8)
c*****Ucitavanje podataka grana
      do 1 ig=1,ng
      ivpqp(ig)=0.
      if(ig-ngrp) 3,3,2
3     read(1,105) p(ig),q(ig),z(ig),tau(ig)
105   format(2i5,2f10.3)
      g(ig)=1./z(ig)
      ndt(ig)=tau(ig)/dt
      go to 1
2     read(1,106) p(ig),q(ig),kon(ig),par(ig)
106   format(3i5,f10.3)
      i=kon(ig)
      go to (4,5,6),i
4     g(ig)=1./par(ig)
      go to 1
5     g(ig)=dt*1.e-3/(2.*par(ig))
      go to 1
6     g(ig)=2.*par(ig)/dt
1     continue
      write(2,107) (ig,p(ig),q(ig),g(ig),ig=1,ng)
107   format(/,'Grana',t9,'P',t14,'Q',t16,'G (Simens)',/,(3i5,f10.3))
      if(ngrp)7,8,7
7     write(2,108) (ig,ndt(ig),ig=1,ngrp)
108   format(/,'Grana',t8,'NDT',/,(2i5))
c*****Ucitavanje podataka o zadanim naponima cvora
8     if(ncb) 10,9,10
10    do 11 i=1,ncb
      e0(i)=0.
      t2(i)=0.
      s(i)=0.
      a(i)=0.
      b(i)=0.
      f(i)=0.
11    psi(i)=0.
      read(1,100) (icbop(i),i=1,ncb)
      write(2,111)(icbop(i),i=1,ncb)
111   format(/,' Opcije zadanih napona cvorova:',/,(16i5))
      do 12 i=1,ncb
      ii=icbop(i)
      go to (13,14,15,16,17,18,19),ii
13    read(1,109) u0
109   format(8f10.3)
      e0(i)=u0
      go to 20
14    read(1,109) u0,t2c
      e0(i)=u0
      t2(i)=t2c
      go to 20
15    read(1,109) u0,ac
      e0(i)=u0
      a(i)=ac
      go to 20
16    read(1,109) u0,sc,t2c
      e0(i)=u0
      s(i)=sc
      t2(i)=t2c
      go to 20
17    read(1,109) u0,ac,bc
      e0(i)=u0
      a(i)=ac
      b(i)=bc
      go to 20
18    read(1,109) u0,ac,fc,psic
      e0(i)=u0
      a(i)=ac
      f(i)=fc
      psi(i)=psic
      go to 20
19    read(1,109) u0,fc,psic
      e0(i)=u0
      f(i)=fc
      psi(i)=psic
20    write(2,110) i,e0(i),t2(i),s(i),a(i),b(i),f(i),psi(i)
110   format('Cvor=',i2,2x,'Eo=',f6.2,2x,'T2=',f7.1,2x,'S=',f4.1,2x,'A='
     *,f6.4,2x,'B=',f6.4,2x,'F=',f6.1,2x,'PSI=',f5.1)
12    continue
c*****Ucitavanje podataka o struji groma
9     if(k) 21,22,21
21    go to (23,24,25,26,27,28,29),kop
23    read(1,109) u0i
      go to 22
24    read(1,109) u0i,t2ci
      go to 22
25    read(1,109) u0i,aci
      go to 22
26    read(1,109) u0i,sci,t2ci
      go to 22
27    read(1,109) u0i,aci,bci
      go to 22
28    read(1,109) u0i,aci,fci,psici
      go to 22
29    read(1,109) u0i,fci,psici
c*****Ucitavanje pocetnih uvjeta
22    do 30 ig=1,ng
      if(ig-ngrp) 31,31,32
31    nn=ndt(ig)
      do 33 i=1,nn
      read(1,109) epp,eqp,ipq,iqp
      ivpp(ig,i)=-iqp-eqp/z(ig)
      ivqp(ig,i)=-ipq-epp/z(ig)
      write(2,112) ig,i,ivpp(ig,i),ig,i,ivqp(ig,i)
112   format(' Ip(',i2,',',i3,')=',f10.3,5x,'Iq(',i2,',',i3,')=',f10.3)
33    continue
      go to 30
32    if(kon(ig)-1) 34,30,34
34    read(1,109) epp,eqp,ipqp
      pom=ipqp+g(ig)*(epp-eqp)
      if(kon(ig)-2) 36,35,36
35    ivpqp(ig)=pom
      go to 37
36    ivpqp(ig)=-pom
37    write(2,113) ig,ivpqp(ig)
113   format(' Ipq(',i2,')=',f10.3)
30    continue
c*****Ucitavanje cvorova ciji ce se naponi tiskati
      read(1,100) (ictis(i),i=1,ncnap)
      write(2,114)(ictis(i),i=1,ncnap)
114   format(/,' Brojevi cvorova ciji ce se naponi tiskati:',/,(16i5))
c*****Ucitavanje grana cije ce se struje tiskati
      if(ngstr) 38,39,38
38    read(1,100) (igtis(i),i=1,ngstr)
      write(2,115)(igtis(i),i=1,ngstr)
115   format(/,' Brojevi grana cije ce se struje tiskati:',/,(16i5))
c*****Ucitavanje grana ciji ce se naponi tiskati
39    if(ngnap) 40,41,40
40    read(1,100) (igtn(i),i=1,ngnap)
      write(2,116)(igtn(i),i=1,ngnap)
116   format(/,' Brojevi grana ciji ce se naponi tiskati:',/,(16i5))
c*****Formiranje matrica [Yaa] i [Yab]
41    do 42 i=1,nca
      do 42 j=1,nca
42    yaa(i,j)=0.
      if(ncb) 43,44,43
43    do 45 i=1,nca
      do 45 j=1,ncb
45    yab(i,j)=0.
44    do 46 ig=1,ng
      i=p(ig)
      j=q(ig)
      if(j-nca) 47,47,48
47    yaa(j,j)=yaa(j,j)+g(ig)
      if(i) 49,46,49
49    yaa(i,i)=yaa(i,i)+g(ig)
      if(ig-ngrp) 46,46,50
50    yaa(i,j)=-g(ig)
      yaa(j,i)=yaa(i,j)
      go to 46
48    if(i-nca) 51,51,46
51    if(i) 52,46,52
52    yaa(i,i)=yaa(i,i)+g(ig)
      if(ncb) 53,46,53
53    l=j-nca
      if(ig-ngrp) 300,300,301
300   yab(i,l)=0.
      go to 46
301   yab(i,l)=-g(ig)
46    continue
      write(2,117)
117   format(/,' MATRICA [Yaa]:')
      do 54 i=1,nca
54    write(2,118) (yaa(i,j),j=1,i)
118   format(8f10.6)
      if(ncb) 55,56,55
55    write(2,119)
119   format(/,' MATRICA [Yab]:')
      do 57 i=1,nca
57    write(2,118) (yab(i,j),j=1,ncb)
c*****Inverzija matrice [Yaa]
56    call inver(nca,yaa)
      write(2,120)
120   format(/,' Inverzna matrica [Yaa]:')
      do 58 i=1,nca
58    write(2,118) (yaa(i,j),j=1,i)
c*****Racun prijelaznog stanja
      t=0.
      ih=0
      jj=0
      do 59 ig=1,ngrp
59    m(ig)=0
      do 60 it=1,nt
      t=t+dt
      write(2,500) it,t
 500  format(10x,'IT=',i4,5x,'Vrijeme t=',f10.2,' (mikrosec.)')
      ih=ih+1
      if(ih-nh) 62,61,62
61    jj=jj+1
      tt(jj)=t
62    do 63 ig=1,ngrp
      nn=ndt(ig)
      if(it/nn*nn-it) 63,64,63
64    m(ig)=m(ig)+1
63    continue
c*****Formiranje stupcanog vektora na desnoj strani [i]-[I]-[Yab]*[Eab]
      do 65 ic=1,nca
      if(ic-k) 67,66,67
66    go to (68,69,70,71,72,73,74),kop
68    itot(ic)=f1(u0i)
      go to 75
69    itot(ic)=f2(u0i,t2ci,t)
      go to 75
70    itot(ic)=f3(u0i,aci,t)
      go to 75
71    itot(ic)=f4(u0i,sci,t2ci,t)
      go to 75
72    itot(ic)=f5(u0i,aci,bci,t)
      go to 75
73    itot(ic)=f6(u0i,aci,fci,psici,t)
      go to 75
74    itot(ic)=f7(u0i,fci,psici,t)
      go to 75
67    itot(ic)=0.
75    if(ncb) 76,77,76
76    do 78 i=1,ncb
      ii=icbop(i)
      call ems(i,ii,e0,t2,a,s,b,f,psi,t,pom)
78    itot(ic)=itot(ic)-yab(ic,i)*pom
77    do 79 ig=1,ng
      if(ig-ngrp) 80,80,81
80    if(p(ig)-ic) 83,82,83
82    if(it-ndt(ig)) 84,84,85
84    itot(ic)=itot(ic)-ivpp(ig,it)
      go to 79
85    if(it-m(ig)*ndt(ig)) 511,510,511
510   ii=it-(m(ig)-1)*ndt(ig)
      go to 512
511   ii=it-m(ig)*ndt(ig)
512   itot(ic)=itot(ic)-ivpp(ig,ii)
      go to 79
83    if(q(ig)-ic) 79,86,79
86    if(it-ndt(ig)) 87,87,88
87    itot(ic)=itot(ic)-ivqp(ig,it)
      go to 79
88    if(it-m(ig)*ndt(ig)) 516,515,516
515   ii=it-(m(ig)-1)*ndt(ig)
      go to 517
516   ii=it-m(ig)*ndt(ig)
517   itot(ic)=itot(ic)-ivqp(ig,ii)
      go to 79
81    if(kon(ig)-1) 89,79,89
89    if(p(ig)-ic) 91,90,91
90    itot(ic)=itot(ic)-ivpqp(ig)
      go to 79
91    if(q(ig)-ic) 79,92,79
92    itot(ic)=itot(ic)+ivpqp(ig)
79    continue
65    continue
      write(2,125)(itot(i),i=1,nca)
125   format(/,' Totalne struje cvorova [Itot]:',/,(f12.5))
c*****Rijesavanje matricne jednadzbe
      do 93 i=1,nca
      v(i)=0.
      do 93 j=1,nca
93    v(i)=v(i)+yaa(i,j)*itot(j)
      write(2,126) (v(i),i=1,nca)
126   format(/,' Naponi cvorova:',/,(f12.3))
c*****Zapis napona cvorova koji se kane tiskati
      if(ih-nh) 95,94,95
94    do 96 ii=1,ncnap
      do 96 i=1,nca
      if(ictis(ii)-i) 96,97,96
97    ucvor(ii,jj)=v(i)
96    continue
c*****Racun struja u granama mreze i strujnih izvora, te zapis istih u
c     trazenim granama
95    do 98 ig=1,ng
      i=p(ig)
      j=q(ig)
      if(i-nca) 99,99,200
99    if(i) 202,201,202
201   vp=0.
      go to 203
202   vp=v(i)
      go to 203
200   l=i-nca
      ii=icbop(l)
      call ems(l,ii,e0,t2,a,s,b,f,psi,t,pom)
      vp=pom
203   if(j-nca) 204,204,205
204   vq=v(j)
      go to 206
205   l=j-nca
      ii=icbop(l)
      call ems(l,ii,e0,t2,a,s,b,f,psi,t,pom)
      vq=pom
206   if(ig-ngrp) 207,207,208
207   if(it-ndt(ig)) 209,209,210
209   l=it
      go to 211
210   if(it-m(ig)*ndt(ig)) 556,555,556
555   l=it-(m(ig)-1)*ndt(ig)
      go to 211
556   l=it-m(ig)*ndt(ig)
211   ipq=g(ig)*vp+ivpp(ig,l)
      iqp=g(ig)*vq+ivqp(ig,l)
      ivpp(ig,l)=-iqp-g(ig)*vq
      ivqp(ig,l)=-ipq-g(ig)*vp
      write(2,127) ig,i,j,vp,vq,ipq,iqp,l,ivpp(ig,l),ivqp(ig,l)
127   format('Grana=',i2,2x,'P=',i2,2x,'Q=',i2,2x,'Vp=',f10.3,2x,'Vq=',
     *f10.3,2x,'Ipq=',f10.3,2x,'Iqp=',f10.3,/,'L=',i3,2x,'Ip=',f10.3,2x,
     *'Iq=',f10.3)
      go to 212
208   if(kon(ig)-1) 214,213,214
213   ipq=g(ig)*(vp-vq)
      write(2,128) ig,i,j,vp,vq,ipq
128   format('Grana=',i2,2x,'P=',i2,2x,'Q=',i2,2x,'Vp=',f10.3,2x,'Vq=',
     *f10.3,2x,'Ipq=',f10.3)
      go to 212
214   ipq=g(ig)*(vp-vq)+ivpqp(ig)
      if(kon(ig)-2) 216,215,216
215   pr=1.
      go to 217
216   pr=-1.
217   ivpqp(ig)=pr*(ipq+g(ig)*(vp-vq))
      write(2,129) ig,i,j,vp,vq,ipq,ivpqp(ig)
129   format('Grana=',i2,2x,'P=',i2,2x,'Q=',i2,2x,'Vp=',f10.3,2x,'Vq=',
     *f10.3,2x,'ipq=',f10.3,2x,'Ipq=',f10.3)
212   if(ngstr) 218,98,218
218   if(ih-nh) 98,219,98
219   do 220 l=1,ngstr
      if(igtis(l)-ig) 220,221,220
221   strpq(ig,jj)=ipq
      if(ig-ngrp) 222,222,220
222   strqp(ig,jj)=iqp
220   continue
98    continue
      if(ih-nh) 60,223,60
223   ih=0
60    continue
c*****Tiskanje napona cvorova
      do 224 i=1,ncnap
      ii=ictis(i)
      write(2,130) ii
130   format(/' Vrijeme',t10,'Napon cv.',i2)
      do 224 it=1,jj
224   write(2,131) tt(it),ucvor(ii,it)
131   format(f10.2,f10.3)
c*****Tiskanje struja grana
      if(ngstr) 225,226,225
225   do 227 i=1,ngstr
      ii=igtis(i)
      write(2,132) ii
132   format(/,'Vrijeme',t12,'Struja grane',i2)
      do 227 it=1,jj
      if(ii-ngrp) 228,228,229
228   write(2,133) tt(it),strpq(ii,it),strqp(ii,it)
133   format(f10.2,2f10.3)
      go to 227
229   write(2,131) tt(it),strpq(ii,it)
227   continue
226   stop
      end
c********************
c     Potprogram  EMS
c********************
      subroutine ems(i,ii,e0,t2,a,s,b,f,psi,t,pom)
      dimension e0(10),t2(10),s(10),a(10),b(10),f(10),psi(10)
      go to (10,11,12,13,14,15,16),ii
10    u0=e0(i)
      pom=f1(u0)
      go to 17
11    u0=e0(i)
      t2c=t2(i)
      pom=f2(u0,t2c,t)
      go to 17
12    u0=e0(i)
      ac=a(i)
      pom=f3(u0,ac,t)
      go to 17
13    u0=e0(i)
      sc=s(i)
      t2c=t2(i)
      pom=f4(u0,sc,t2c,t)
      go to 17
14    u0=e0(i)
      ac=a(i)
      bc=b(i)
      pom=f5(u0,ac,bc,t)
      go to 17
15    u0=e0(i)
      ac=a(i)
      fc=f(i)
      psic=psi(i)
      pom=f6(u0,ac,fc,psic,t)
      go to 17
16    u0=e0(i)
      fc=f(i)
      psic=psi(i)
      pom=f7(u0,fc,psic,t)
17    return
      end
c*********************
c     Potprogram INVER
c*********************
      subroutine inver(n,z)
      dimension z(50,50)
      do 7 ni=1,n
      do 4 i=1,n
      if(i-ni) 1,4,1
1     do 3 j=1,n
      if(j-ni) 2,3,2
2     z(i,j)=z(i,j)-z(ni,j)*z(i,ni)/z(ni,ni)
3     continue
      j=ni
      z(i,j)=-z(i,ni)/z(ni,ni)
4     continue
      i=ni
      do 6 j=1,n
      if(j-ni) 5,6,5
5     z(i,j)=z(ni,j)/z(ni,ni)
6     continue
      j=ni
7     z(i,j)=1./z(ni,ni)
      return
      end
c*****************************
c     Funkcijski potprogram F1
c*****************************
      function f1(u0)
      f1=u0
      return
      end
c*****************************
c     Funkcijski potprogram F2
c*****************************
      function f2(u0,t2,t)
      if(t-t2) 1,1,2
1     f2=u0
      go to 3
2     f2=0.
3     return
      end
c*****************************
c     Funkcijski potprogram F3
c*****************************
      function f3(u0,a,t)
      f3=u0*exp(-a*t)
      return
      end
c*****************************
c     Funkcijski potprogram F4
c*****************************
      function f4(u0,s,t2,t)
      t1=u0/s
      if(t-t1) 1,2,2
1     f4=s*t
      go to 3
2     if(t-t2) 4,4,5
4     f4=u0
      go to 3
5     f4=0.
3     return
      end
c*****************************
c     Funkcijski potprogram F5
c*****************************
      function f5(u0,a,b,t)
      f5=u0*(exp(-a*t)-exp(-b*t))
      return
      end
c*****************************
c     Funkcijski potprogram F6
c*****************************
      function f6(u0,a,f,psi,t)
      pi=3.14159
      omega=2.*pi*f*1.e-6
      f6=u0*(exp(-a*t)-cos(omega*t+psi*pi/180.))
      return
      end
c*****************************
c     Funkcijski potprogram F7
c*****************************
      function f7(u0,f,psi,t)
      pi=3.14159
      omega=2.*pi*f*1.e-6
      f7=u0*cos(omega*t+psi*pi/180.)
      return
      end
