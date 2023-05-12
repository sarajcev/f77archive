c     Program NSIMUK za analizu nesimultanog uklopa voda
      real ldv,L(3,3),Linv(3,3),LL,Lalfa,L0,Lmalfa,Lm0,mi0,mir(10),
     *Ip(3,2000),Ipabc(3,2000),Ipmax(3),Kpp(3),Kpk(3)
      dimension R1(10),rr(10),x(10),y(10),Pabc(3,3),Pii(3,2),Piit(2,3),
     *Piv(3,3),S(3,3),Scr(3,2),Cabc(3,3),Cab0(3,3),TT(2000),U1(50),
     *U2(50),STR(50),Up(3,2000),Uk(3,2000),Upabc(3,2000),Ukabc(3,2000),
     *Upmax(3),TUpmax(3),Ukmax(3),TUkmax(3),TIpmax(3)
      complex Ik3,Ik1,Zm,Zabc(3,3),Zii(3,2),Ziit(2,3),Ziv(3,3),Q(3,3),
     *Qcr(3,2),Zabcp(3,3),Zab0(3,3)
      fa(t)=sqrt(2./3.)*Un*cos(omeg*t+psi)
      fb(t)=sqrt(2./3.)*Un*cos(omeg*t+psi-120.*pi/180.)
      fc(t)=sqrt(2./3.)*Un*cos(omeg*t+psi+120.*pi/180.)
      open(5,file='nsimuk.dat')
      open(6,file='nsimuk.prn')
      read(5,100) Un,fn,psi
100   format(8f10.3)
      write(6,101) Un,fn,psi
101   format(' Program: NSIMUK',/,t30,'ULAZNI PODACI',/,' Un =',f6.1,
     *' [kV]',/,' fn =',f6.1,' [Hz]',/,' Kut u casu uklapanja:',f6.1,
     *' [stupnj.]')
      read(5,100) tb,tc,Tmax
      write(6,102) tb,tc,Tmax
102   format(/,' Trenutak uklapanja faze b:',f5.1,' [ms]',/,' Trenutak u
     *klapanja faze c:',f5.1,' [ms]',/,' Vrijeme promatranja prijelaznog
     * stanja:',f6.1,' [ms]')
      read(5,100) r,fi,Zr,Zi
      write(6,103) r,fi,Zr,Zi
103   format(/,' PODACI MREZE/IZVORA:',/,' Ik3 =',f8.1,' /',f6.1,' [A]',
     */,' Ik1 =',f8.1,' /',f6.1,' [A]')
      read(5,105) Nzu,pfv,pzu,ldv,ro
105   format(i5,4f10.3)
      write(6,104) Nzu,pfv,pzu,ldv,ro
104   format(/,' PODACI VODA:',/,' Broj zastitnih uzeta:',i3,/,' Max. pr
     *ovjes faznih vodica:',f6.1,' [m]',/,' Max. provjes zastitnih uzeta
     *:',f6.1,' [m]',/,' Duljina voda:',f7.2,' [km]',/,' Prosjecni speci
     *ficni el. otpor trase voda:',f8.1,' [Ohm*m]',//,' Podaci vodica:')
      Nuv=3+Nzu
      do 1 i=1,Nuv
      read(5,100) R1(i),rr(i),mir(i),x(i),y(i)
      write(6,106) i,R1(i),rr(i),mir(i),x(i),y(i)
106   format(i2,'. vodic',2x,'R1 =',f6.3,' Ohm/km',2x,'r =',f5.1,
     *' mm',2x,'mir =',f5.1,2x,'x =',f5.1,' m',2x,'y =',f5.1,' m')
      rr(i)=rr(i)*1.e-3
      if(i.le.3) y(i)=y(i)-2.*pfv/3.
      if(i.gt.3) y(i)=y(i)-2.*pzu/3.
1     continue
c---- Konstante programa
      pi=3.14159
      Nfv=3
      psi=psi*pi/180.
      df=100.
      ck=0.018
      omeg=2.*pi*fn*1.e-3
      mi0=4.*pi*1.e-4
c**** Racun alfa i 0 komp. mreze
      a=r*cos(fi*pi/180.)
      b=r*sin(fi*pi/180.)
      Ik3=cmplx(a,b)
      Zm=Un*1.e3/(sqrt(3.)*Ik3)
      Rmalfa=real(Zm)
      Lmalfa=aimag(Zm)/omeg
      write(6,107) Rmalfa,Lmalfa
107   format(/,t30,'REZULTATI PRORACUNA',/,' PARAMETRI MREZE/IZVORA:',
     */,' Alfa - sustav:',/,' Rm =',f7.2,' [Ohm]',3x,'Lm =',f9.3,' [mH]'
     *)
      a=Zr*cos(Zi*pi/180.)
      b=Zr*sin(Zi*pi/180.)
      Ik1=cmplx(a,b)
      Zm=Un*1.e3*(3./Ik1-2./Ik3)/sqrt(3.)
      Rm0=real(Zm)
      Lm0=aimag(Zm)/omeg
      write(6,108) Rm0,Lm0
108   format(' Nulti - sustav:',/,' Rm =',f7.2,' [Ohm]',3x,'Lm =',f9.3,
     *' [mH]')
c&&&& Matrice transformacije (Edit Clark)
c     Matrica [L]
      L(1,1)=1.
      L(1,2)=1.
      L(1,3)=0.
      L(2,1)=1.
      L(2,2)=-0.5
      L(2,3)=sqrt(3.)/2.
      L(3,1)=1.
      L(3,2)=-0.5
      L(3,3)=-sqrt(3.)/2.
      write(6,109)
109   format(/,' MATRICA TRANSFORMACIJE [L]:')
      do 2 i=1,3
      write(6,100) (L(i,j),j=1,3)
2     continue
c     Matrica 1/[L]
      Linv(1,1)=1./3.
      Linv(1,2)=1./3.
      Linv(1,3)=1./3.
      Linv(2,1)=2./3.
      Linv(2,2)=-1./3.
      Linv(2,3)=-1./3.
      Linv(3,1)=0.
      Linv(3,2)=sqrt(3.)/3.
      Linv(3,3)=-sqrt(3.)/3.
      write(6,110)
110   format(/,' INVERZNA MATRICA TRANSFORMACIJE 1/[L]:')
      do 3 i=1,3
      write(6,100) (Linv(i,j),j=1,3)
3     continue
c%%%% Racun potencijalnih koeficijenata voda
      do 4 i=1,3
      do 4 j=1,i
      if(i-j) 6,5,6
5     Pabc(i,i)=ck*alog(2.*y(i)/rr(i))
      go to 4
6     D=sqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
      Dcr=sqrt((x(i)-x(j))**2+(y(i)+y(j))**2)
      Pabc(i,j)=ck*alog(Dcr/D)
      Pabc(j,i)=Pabc(i,j)
4     continue
      write(6,111)
111   format(/,' MATRICA POTENCIJALNIH KOEF. FAZNIH VODICA [Pabc] u [km/
     *nF]:')
      do 7 i=1,3
      write(6,100) (Pabc(i,j),j=1,3)
7     continue
      if(Nzu.eq.0) go to 8
      do 9 i=1,3
      do 9 j=1,Nzu
      D=sqrt((x(i)-x(j+3))**2+(y(i)-y(j+3))**2)
      Dcr=sqrt((x(i)-x(j+3))**2+(y(i)+y(j+3))**2)
      Pii(i,j)=ck*alog(Dcr/D)
9     continue
      write(6,112)
112   format(/,' MATRICA POTENCIJALNIH KOEF. [Pii] u [km/nF]:')
      do 10 i=1,3
      write(6,100) (Pii(i,j),j=1,Nzu)
10    continue
      do 11 i=1,Nzu
      do 11 j=1,3
      Piit(i,j)=Pii(j,i)
11    continue
      write(6,113)
113   format(/,' TRANSPONIRANA MATRICA [Pii]:')
      do 12 i=1,Nzu
      write(6,100) (Piit(i,j),j=1,3)
12    continue
      do 13 i=1,Nzu
      do 13 j=1,Nzu
      if(i-j) 15,14,15
14    Piv(i,i)=ck*alog(2.*y(i+3)/rr(i+3))
      go to 13
15    D=sqrt((x(i+3)-x(j+3))**2+(y(i+3)-y(j+3))**2)
      Dcr=sqrt((x(i+3)-x(j+3))**2+(y(i+3)+y(j+3))**2)
      Piv(i,j)=ck*alog(Dcr/D)
      Piv(j,i)=Piv(i,j)
13    continue
      if(Nzu-1) 17,16,17
16    write(6,114) Piv(1,1)
114   format(/,' Potencijalni koef. zastitnog uzeta:',f10.3,'[km/nF]')
      do 18 i=1,3
      do 18 j=1,3
      S(i,j)=Pii(i,1)*Piit(1,j)/Piv(1,1)
18    continue
      go to 19
17    write(6,115)
115   format(/,' MATRICA POTENCIJALNIH KOEF. ZAST. UZETA [Piv] u [km/nF]
     *:')
      do 20 i=1,Nzu
      write(6,100) (Piv(i,j),j=1,Nzu)
20    continue
c---- Uracunavanje utjecaja zastitnog uzeta
      call INVER(Nzu,Piv)
      write(6,116)
116   format(/,' INVERZNA MATRICA 1/[Piv]:')
      do 21 i=1,Nzu
      write(6,100) (Piv(i,j),j=1,Nzu)
21    continue
      do 22 i=1,3
      do 22 j=1,Nzu
      Scr(i,j)=0.
      do 22 k=1,Nzu
      Scr(i,j)=Scr(i,j)+Pii(i,k)*Piv(k,j)
22    continue
      do 23 i=1,3
      do 23 j=1,3
      S(i,j)=0.
      do 23 k=1,Nzu
      S(i,j)=S(i,j)+Scr(i,k)*Piit(k,j)
23    continue
19    do 24 i=1,3
      do 24 j=1,3
      Pabc(i,j)=Pabc(i,j)-S(i,j)
24    continue
      write(6,117)
117   format(/,' MATRICA POTENCIJALNIH KOEF. EKV. FAZNIH VODICA [Pabc]:'
     *)
      do 25 i=1,3
      write(6,100) (Pabc(i,j),j=1,3)
25    continue
c$$$$ Racun matrice kapacitivnih koeficijenata
8     call INVER(Nfv,Pabc)
      write(6,118)
118   format(/,' MATRICA KAPACITIVNIH KOEF. EKV. FAZNIH VODICA [Cabc] u
     *[nF/km]:')
      do 26 i=1,3
      write(6,100) (Pabc(i,j),j=1,3)
26    continue
c%%%% Racun preplitanja
      a=(Pabc(1,1)+Pabc(2,2)+Pabc(3,3))/3.
      b=(Pabc(2,1)+Pabc(3,1)+Pabc(3,2))/3.
      do 27 i=1,3
      do 27 j=1,3
      if(i.eq.j) Cabc(i,i)=a
      if(i.ne.j) Cabc(i,j)=b
27    continue
      write(6,119)
119   format(/,' MATRICA KAPACITIVNIH KOEF. PREPLETENOG VODA [Cabc] u [n
     *F/km]:')
      do 28 i=1,3
      write(6,100) (Cabc(i,j),j=1,3)
28    continue
c@@@@ Odredjivanje alfa, beta, nula komponenata kapacitivnih koef.
      do 29 i=1,3
      do 29 j=1,3
      S(i,j)=0.
      do 29 k=1,3
      S(i,j)=S(i,j)+Linv(i,k)*Cabc(k,j)
29    continue
      do 30 i=1,3
      do 30 j=1,3
      Cab0(i,j)=0.
      do 30 k=1,3
      Cab0(i,j)=Cab0(i,j)+S(i,k)*L(k,j)
30    continue
      write(6,120)
120   format(/,' MATRICA alfa, beta, 0 KAPACITIVNIH KOEF. [Cab0] u [nF/k
     *m]:')
      do 31 i=1,3
      write(6,100) (Cab0(i,j),j=1,3)
31    continue
      C0=Cab0(1,1)
      Calfa=Cab0(2,2)
      write(6,121) Calfa,C0
121   format(/,' Calfa =',f6.3,' [nF/km]',/,' Co =',f6.3,' [nF/km]')
c#### Proracun induktiviteta i otpora voda
      iter=1
      f=fn
32    do 33 i=1,3
      do 33 j=1,i
      if(i-j) 35,34,35
34    Zr=R1(i)+pi**2*f*1.e-4
      Zi=mi0*f*(alog(658.*sqrt(ro/f)/rr(i))+mir(i)/4.)
      Zabc(i,i)=cmplx(Zr,Zi)
      go to 33
35    a=abs(x(i)-x(j))
      b=y(i)
      c=y(j)
      call ZEMED(a,b,c,ro,f,Zm)
      Zabc(i,j)=Zm
      Zabc(j,i)=Zabc(i,j)
33    continue
      write(6,122) iter,f
122   format(/,i3,'. iteracija',3x,'f =',f8.1,' [Hz]')
      write(6,123)
123   format(/,5x,'MATRICA [Zabc] FAZNIH VODICA u [Ohm/km]:')
      do 36 i=1,3
      write(6,100) (Zabc(i,j),j=1,3)
36    continue
      if(Nzu.eq.0) go to 37
c---- Racun matrice [Zii]
      do 38 i=1,3
      do 38 j=1,Nzu
      a=abs(x(i)-x(j+3))
      b=y(i)
      c=y(j+3)
      call ZEMED(a,b,c,ro,f,Zm)
      Zii(i,j)=Zm
38    continue
      write(6,124)
124   format(/,5x,'MATRICA [Zii] u [Ohm/km]:')
      do 39 i=1,3
      write(6,100) (Zii(i,j),j=1,Nzu)
39    continue
      do 40 i=1,Nzu
      do 40 j=1,3
40    Ziit(i,j)=Zii(j,i)
      write(6,125)
125   format(/,5x,'TRANSPONIRANA MATRICA [Zii]:')
      do 41 i=1,Nzu
41    write(6,100) (Ziit(i,j),j=1,3)
c---- Racun matrice [Ziv]
      do 42 i=1,Nzu
      do 42 j=1,i
      if(i-j) 44,43,44
43    Zr=R1(i+3)+pi**2*f*1.e-4
      Zi=mi0*f*(alog(658.*sqrt(ro/f)/rr(i+3))+mir(i+3)/4.)
      Ziv(i,i)=cmplx(Zr,Zi)
      go to 42
44    a=abs(x(i+3)-x(j+3))
      b=y(i+3)
      c=y(j+3)
      call ZEMED(a,b,c,ro,f,Zm)
      Ziv(i,j)=Zm
      Ziv(j,i)=Ziv(i,j)
42    continue
      if(Nzu-1) 46,45,46
45    write(6,126) Ziv(1,1)
126   format(/,5x,'Impedancija zastitnog uzeta:',2f8.3,' [Ohm/km]')
      do 47 i=1,3
      do 47 j=1,3
47    Q(i,j)=Zii(i,1)*Ziit(1,j)/Ziv(1,1)
      go to 48
46    write(6,127)
127   format(/,5x,'MATRICA [Ziv] u [Ohm/km]:')
      do 49 i=1,Nzu
49    write(6,100) (Ziv(i,j),j=1,Nzu)
      call INVEMA(Nzu,Ziv)
      write(6,128)
128   format(/,5x,'INVERZNA MATRICA [Ziv]:')
      do 50 i=1,Nzu
50    write(6,100) (Ziv(i,j),j=1,Nzu)
      do 51 i=1,3
      do 51 j=1,Nzu
      Qcr(i,j)=cmplx(0.,0.)
      do 51 k=1,Nzu
51    Qcr(i,j)=Qcr(i,j)+Zii(i,k)*Ziv(k,j)
      do 52 i=1,3
      do 52 j=1,3
      Q(i,j)=cmplx(0.,0.)
      do 52 k=1,Nzu
52    Q(i,j)=Q(i,j)+Qcr(i,k)*Ziit(k,j)
48    do 53 i=1,3
      do 53 j=1,3
53    Zabc(i,j)=Zabc(i,j)-Q(i,j)
      write(6,129)
129   format(/,5x,'MATRICA [Zabc] EKV. FAZ. VODICA u [Ohm/km]:')
      do 54 i=1,3
54    write(6,100) (Zabc(i,j),j=1,3)
c---- Racun preplitanja
37    Ik3=(Zabc(1,1)+Zabc(2,2)+Zabc(3,3))/3.
      Ik1=(Zabc(2,1)+Zabc(3,1)+Zabc(3,2))/3.
      do 55 i=1,3
      do 55 j=1,3
      if(i.eq.j) Zabcp(i,i)=Ik3
      if(i.ne.j) Zabcp(i,j)=Ik1
55    continue
      write(6,130)
130   format(/,5x,'MATRICA [Zabc] PREPLETENOG VODA u [Ohm/km]:')
      do 56 i=1,3
56    write(6,100) (Zabcp(i,j),j=1,3)
c++++ Racun matrice Z alfa, beta, 0 komponenata
      do 57 i=1,3
      do 57 j=1,3
      Q(i,j)=cmplx(0.,0.)
      do 57 k=1,3
57    Q(i,j)=Q(i,j)+Linv(i,k)*Zabcp(k,j)
      do 58 i=1,3
      do 58 j=1,3
      Zab0(i,j)=cmplx(0.,0.)
      do 58 k=1,3
58    Zab0(i,j)=Zab0(i,j)+Q(i,k)*L(k,j)
      write(6,131)
131   format(/,5x,'MATRICA alfa, beta, 0  voda [Zab0] u [Ohm/km]:')
      do 59 i=1,3
59    write(6,100) (Zab0(i,j),j=1,3)
      Lalfa=aimag(Zab0(2,2))*1.e3/(2.*pi*f)
      L0=aimag(Zab0(1,1))*1.e3/(2.*pi*f)
      fp=1.e6/(2.*ldv*sqrt(L0*C0))
      write(6,132) Lalfa,L0,fp
132   format(/,5x,'Lalfa =',f7.3,' [mH/km]',/,5x,'L0 =',f7.3,' [mH/km]',
     */,5x,'fp =',f7.1,' [Hz]',/)
      if(abs(f-fp)-df) 61,60,60
60    f=fp
      iter=iter+1
      go to 32
61    Ralfa=real(Zab0(2,2))
      R0=real(Zab0(1,1))
      Z0=sqrt(L0/C0)*1.e3
      v0=1.e3/sqrt(L0*C0)
      del0=R0*ldv/(2.*L0*v0)
      write(6,133) R0,Z0,v0,del0
133   format(/,' Parametri voda nultog sustava:',/,' R0 =',f7.3,' [Ohm/k
     *m]',/,' Z0 =',f8.2,' [Ohm]',/,' v0 =',f8.3,' [km/ms]',/,' Prigusen
     *je =',f7.4)
      Za=sqrt(Lalfa/Calfa)*1.e3
      va=1.e3/sqrt(Lalfa*Calfa)
      dela=Ralfa*ldv/(2.*Lalfa*va)
      write(6,134) Ralfa,Za,va,dela
134   format(/,' Parametri voda alfa sustava:',/,' Ralfa =',f7.3,' [Ohm/
     *km]',/,' Zalfa =',f8.2,' [Ohm]',/,' Valfa =',f8.3,' [km/ms]',/,
     *' Prigusenje =',f7.4)
      Tau0=ldv/v0
      Taua=ldv/va
      if(Tau0.ge.Taua) dt=Taua/10.
      if(Tau0.lt.Taua) dt=Tau0/10.
      ix=ifix(Tau0/dt)
      if(ix) 63,62,63
62    Ndt0=1
      Nh0=1
      go to 64
63    if(ix/2*2-ix) 66,65,66
65    Ndt0=ix
      go to 67
66    Ndt0=ix+1
67    Nh0=2
64    ix=ifix(Taua/dt)
      if(ix) 69,68,69
68    Ndta=1
      Nha=1
      go to 70
69    if(ix/2*2-ix) 72,71,72
71    Ndta=ix
      go to 73
72    Ndta=ix+1
73    Nha=2
70    write(6,135) Tau0,Ndt0,Nh0, Taua,Ndta,Nha, dt
135   format(/,' 0 - sustav:',/,' Tau0 =',f6.3,' [ms]',3x,'Ndt0 =',i4,
     *3x,'Nh0 =',i3,//,' alfa - sustav:',/,' Taua =',f6.3,' [ms]',3x,
     *'Ndta =',i4,3x,'Nha =',i3,//,' Korak proracuna dt =',f5.3,' [ms]')
c**** Proracun prijelazne pojave
      do 74 ksi=1,3
      t=0.
      it=0
      ih=0
      jj=0
      m=0
      go to(75,76,250),ksi
75    Nh=Nh0
      Ndt=Ndt0
      LL=Lm0
      R=Rm0
      Z=Z0
      del=del0
      go to 77
76    Nh=Nha
      Ndt=Ndta
      LL=Lmalfa
      R=Rmalfa
      Z=Za
      del=dela
      go to 77
250   Nh=Nha
      Ndt=Ndta
      LL=Lmalfa
      R=Rmalfa
      Z=Za
      del=dela
77    a=1./(2.*LL/dt+R+Z)
      b=a*(2.*LL/dt-R)
78    t=t+dt
      it=it+1
      ih=ih+1
      if(ih.ne.Nh) go to 79
      jj=jj+1
      TT(jj)=t
79    if(it/Ndt*Ndt-it) 81,80,81
80    m=m+1
81    if(tb-tc) 82,83,84
82    if(t-tb) 85,86,86
83    if(t-tb) 85,87,87
84    if(t-tc) 85,88,88
85    go to(89,90,91),ksi
89    E=fa(t)/3.
      tcr=t-dt
      EE=fa(tcr)/3.
      go to 92
90    E=2.*fa(t)/3.
      tcr=t-dt
      EE=2.*fa(tcr)/3.
      go to 92
91    E=0.
      EE=0.
      go to 92
86    if(t-tc) 93,87,87
93    go to(94,95,96),ksi
94    E=(fa(t)+fb(t))/3.
      tcr=t-dt
      EE=(fa(tcr)+fb(tcr))/3.
      go to 92
95    E=(2.*fa(t)-fb(t))/3.
      tcr=t-dt
      EE=(2.*fa(tcr)-fb(tcr))/3.
      go to 92
96    E=sqrt(3.)*fb(t)/3.
      tcr=t-dt
      EE=sqrt(3.)*fb(tcr)/3.
      go to 92
88    if(t-tb) 97,87,87
87    go to(98,99,200),ksi
98    E=(fa(t)+fb(t)+fc(t))/3.
      tcr=t-dt
      EE=(fa(tcr)+fb(tcr)+fc(tcr))/3.
      go to 92
99    E=(2.*fa(t)-fb(t)-fc(t))/3.
      tcr=t-dt
      EE=(2.*fa(tcr)-fb(tcr)-fc(tcr))/3.
      go to 92
200   E=sqrt(3.)*(fb(t)-fc(t))/3.
      tcr=t-dt
      EE=sqrt(3.)*(fb(tcr)-fc(tcr))/3.
      go to 92
97    go to(201,202,203),ksi
201   E=(fa(t)+fc(t))/3.
      tcr=t-dt
      EE=(fa(tcr)+fc(tcr))/3.
      go to 92
202   E=(2.*fa(t)-fc(t))/3.
      tcr=t-dt
      EE=(2.*fa(tcr)-fc(tcr))/3.
      go to 92
203   E=-sqrt(3.)*fc(t)/3.
      tcr=t-dt
      EE=-sqrt(3.)*fc(tcr)/3.
92    if(it-1) 204,204,205
204   U2(it)=0.
      STR(it)=a*(E+EE)
      U1(it)=Z*STR(it)
      go to 206
205   if(it-Ndt) 207,207,208
207   U2(it)=0.
      STR(it)=a*(E+EE-U1(it-1))+b*STR(it-1)
      U1(it)=Z*STR(it)
206   ii=it
      go to 209
208   if(it-m*Ndt) 210,211,210
210   ii=it-(m-1)*Ndt
      go to 212
211   ii=it-(m-2)*Ndt
212   i1=ii-Ndt
      U2(ii)=exp(-del)*(U1(i1)+Z*STR(i1))
      STR(ii)=a*(E+EE-U1(ii-1)-exp(-del)*U2(i1))+b*STR(ii-1)
      U1(ii)=exp(-del)*U2(i1)+Z*STR(ii)
209   if(ih.ne.Nh) go to 213
      Up(ksi,jj)=U1(ii)
      Uk(ksi,jj)=U2(ii)
      Ip(ksi,jj)=STR(ii)*1.e3
      ih=0
213   if(it.le.Ndt) go to 214
      if(it/Ndt*Ndt-it) 214,215,214
215   do 216 ii=1,Ndt
      U1(ii)=U1(ii+Ndt)
      U2(ii)=U2(ii+Ndt)
      STR(ii)=STR(ii+Ndt)
216   continue
214   if(t-Tmax) 78,74,74
74    continue
c---- Racun faznih napona i struja
      do 217 j=1,jj
      do 217 i=1,3
      Upabc(i,j)=0.
      Ukabc(i,j)=0.
      Ipabc(i,j)=0.
      do 217 k=1,3
      Upabc(i,j)=Upabc(i,j)+L(i,k)*Up(k,j)
      Ukabc(i,j)=Ukabc(i,j)+L(i,k)*Uk(k,j)
      Ipabc(i,j)=Ipabc(i,j)+L(i,k)*Ip(k,j)
217   continue
c&&&& Odredjivanje max. vrijednosti napona i struje
      do 218 i=1,3
      Upmax(i)=0.
      TUpmax(i)=0.
      Ukmax(i)=0.
      TUkmax(i)=0.
      Ipmax(i)=0.
      TIpmax(i)=0.
      do 218 j=1,jj
      if(Upmax(i).ge.Upabc(i,j)) go to 219
      Upmax(i)=Upabc(i,j)
      TUpmax(i)=TT(j)
219   if(Ukmax(i).ge.Ukabc(i,j)) go to 220
      Ukmax(i)=Ukabc(i,j)
      TUkmax(i)=TT(j)
220   if(Ipmax(i).ge.Ipabc(i,j)) go to 218
      Ipmax(i)=Ipabc(i,j)
      TIpmax(i)=TT(j)
218   continue
c$$$$ Racun koeficijenata prenapona
      do 221 i=1,3
      Kpp(i)=sqrt(3./2.)*Upmax(i)/Un
      Kpk(i)=sqrt(3./2.)*Ukmax(i)/Un
221   continue
      write(6,136)
136   format(/,' Max. prenaponi na pocetku voda:')
      do 222 i=1,3
      go to(223,224,225),i
223   write(6,137) Upmax(i),TUpmax(i),Kpp(i)
137   format(' Faza - a:  Umax =',f7.1,' [kV]',3x,'t =',f7.3,' [ms]',
     *3x,'Kp =',f5.2)
      go to 222
224   write(6,138) Upmax(i),TUpmax(i),Kpp(i)
138   format(' Faza - b:  Umax =',f7.1,' [kV]',3x,'t =',f7.3,' [ms]',
     *3x,'Kp =',f5.2)
      go to 222
225   write(6,139) Upmax(i),TUpmax(i),Kpp(i)
139   format(' Faza - c:  Umax =',f7.1,' [kV]',3x,'t =',f7.3,' [ms]',
     *3x,'Kp =',f5.2)
222   continue
      write(6,140)
140   format(/,' Max. prenaponi na kraju voda:')
      do 226 i=1,3
      go to(227,228,229),i
227   write(6,137) Ukmax(i),TUkmax(i),Kpk(i)
      go to 226
228   write(6,138) Ukmax(i),TUkmax(i),Kpk(i)
      go to 226
229   write(6,139) Ukmax(i),TUkmax(i),Kpk(i)
226   continue
      write(6,141)
141   format(/,' Max. struje na pocetku voda:')
      do 230 i=1,3
      go to(231,232,233),i
231   write(6,142) Ipmax(i),TIpmax(i)
142   format(' Faza - a:  Imax =',f7.1,' [A]',3x,'t =',f7.3,' [ms]')
      go to 230
232   write(6,143) Ipmax(i),TIpmax(i)
143   format(' Faza - b:  Imax =',f7.1,' [A]',3x,'t =',f7.3,' [ms]')
      go to 230
233   write(6,144) Ipmax(i),TIpmax(i)
144   format(' Faza - c:  Imax =',f7.1,' [A]',3x,'t =',f7.3,' [ms]')
230   continue
      write(6,145)
145   format(/,' NAPONSKO STANJE NA POCETKU VODA:',/,t3,'t [ms]',t13,
     *'Ua [kV]',t23,'Ub [kV]',t33,'Uc [kV]')
      do 234 j=1,jj
      write(6,146) TT(j),Upabc(1,j),Upabc(2,j),Upabc(3,j)
146   format(f10.3,3f10.1)
234   continue
      write(6,147)
147   format(/,' NAPONSKO STANJE NA KRAJU VODA:',/,t3,'t [ms]',t13,'Ua [
     *kV]',t23,'Ub [kV]',t33,'Uc [kV]')
      do 235 j=1,jj
      write(6,146) TT(j),Ukabc(1,j),Ukabc(2,j),Ukabc(3,j)
235   continue
      write(6,148)
148   format(/,' STRUJNO STANJE NA POCETKU VODA:',/,t3,'t [ms]',t14,'Ia
     *[A]',t24,'Ib [A]',t34,'Ic [A]')
      do 236 j=1,jj
      write(6,146) TT(j),Ipabc(1,j),Ipabc(2,j),Ipabc(3,j)
236   continue
      stop
      end
c*****Potprogram INVEMA
      subroutine INVEMA(n,z)
      complex z(3,3)
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
c*****Potprogram INVER
      subroutine INVER(n,z)
      dimension z(3,3)
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
c*****Potprogram   ZEMED
      SUBROUTINE ZEMED(a,b,c,ro,f,Zmed)
      complex Zmed
      real mi0
      pi=3.14159
      mi0=4.*pi*1.e-7
      g=1.7811
      omega=2.*pi*f
      alfa=sqrt(omega*mi0/ro)
      D=sqrt(a**2+(b-c)**2)
      Dcr=sqrt(a**2+(b+c)**2)
      x=alfa*a
      y=alfa*(b+c)
      r=alfa*Dcr
      if(y.ne.0.) theta=atan(x/y)
      if(y.eq.0.) theta=pi/2.
      s2=0.
      s2cr=0.
      s4=0.
      s4cr=0.
      sig1=0.
      sig2=0.
      sig3=0.
      sig4=0.
      do 1 k=1,10
      pr1=1.
      kk=2*k-1
      do 2 i=1,kk
2     pr1=pr1*i
      pr2=pr1*(kk+1)
      kk=2*(2*k-1)
      pr=1.
      do 3 i=1,kk
3     pr=pr*r/2.
      suma=0.
      k2=2*k
      do 4 i=1,k2
4     suma=suma+1./i
      suma=suma-1./(2.*k2)
      if(k/2*2-k) 5,6,5
5     s2=s2+1./(pr1*pr2)*pr*cos(kk*theta)
      s2cr=s2cr+1./(pr1*pr2)*pr*sin(kk*theta)
      sig2=sig2+suma/(pr1*pr2)*pr*cos(kk*theta)
      go to 7
6     s2=s2-1./(pr1*pr2)*pr*cos(kk*theta)
      s2cr=s2cr-1./(pr1*pr2)*pr*sin(kk*theta)
      sig2=sig2-suma/(pr1*pr2)*pr*cos(kk*theta)
7     pr1=1.
      do 8 i=1,k2
8     pr1=pr1*i
      pr2=pr1*(k2+1)
      kk=4*k
      pr=1.
      do 9 i=1,kk
9     pr=pr*r/2.
      suma=0.
      k2=2*k+1
      do 10 i=1,k2
10    suma=suma+1./i
      suma=suma-1./(2.*k2)
      if(k/2*2-k) 11,12,11
11    s4=s4+1./(pr1*pr2)*pr*cos(kk*theta)
      s4cr=s4cr+1./(pr1*pr2)*pr*sin(kk*theta)
      sig4=sig4+suma/(pr1*pr2)*pr*cos(kk*theta)
      go to 13
12    s4=s4-1./(pr1*pr2)*pr*cos(kk*theta)
      s4cr=s4cr-1./(pr1*pr2)*pr*sin(kk*theta)
      sig4=sig4-suma/(pr1*pr2)*pr*cos(kk*theta)
13    kk=2*k-1
      pr1=1.
      do 14 i=1,kk
      if(i-kk) 15,16,15
15    pr1=pr1*(2*i+1)**2
      go to 14
16    pr1=pr1*(2*i+1)
14    continue
      kk=4*k-3
      pr=1.
      do 19 i=1,kk
19    pr=pr*r
      if(k/2*2-k) 20,21,20
20    sig1=sig1+pr/pr1*cos(kk*theta)
      go to 22
21    sig1=sig1-pr/pr1*cos(kk*theta)
22    kk=2*k
      pr1=1.
      do 23 i=1,kk
      if(i-kk) 24,25,24
24    pr1=pr1*(2*i+1)**2
      go to 23
25    pr1=pr1*(2*i+1)
23    continue
      pr=1.
      kk=4*k-1
      do 26 i=1,kk
26    pr=pr*r
      if(k/2*2-k) 27,28,27
27    sig3=sig3+pr/pr1*cos(kk*theta)
      go to 1
28    sig3=sig3-pr/pr1*cos(kk*theta)
1     continue
      P=pi/8.*(1.-s4)+s2/2.*alog(2./(g*r))+theta*s2cr/2.-sig1/sqrt(2.)+
     *sig2/2.+sig3/sqrt(2.)
      Q=1./4.+(1.-s4)/2.*alog(2./(g*r))-theta*s4cr/2.+sig1/sqrt(2.)-
     *pi/8.*s2+sig3/sqrt(2.)-sig4/2.
      Zr=omega*mi0*P/pi*1.e3
      Zi=omega*mi0/(2.*pi)*(alog(Dcr/D)+2.*Q)*1.e3
      Zmed=cmplx(Zr,Zi)
      return
      end
