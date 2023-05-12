C     Glavni program RASTRUZ
c     Odredjuje raspodjelu struja u sustavu uzemljenja
      real L(7,50),Lm(7,50,50),Lma(7,50,10),mir(50)
      integer P(50),Q(50)
      dimension R1(50),rr(50),x(7,50),y(7,50),xx(7,10),yy(7,10),ro(7),
     *Nv(30),Rmu(30,30),Mst(50,30),Ms(30,50)
      complex Ik(10),Zu(30),Ic(30),Zm,Zs(50,50),E(50),Zap,Ie(50),Iu(30),
     *D(50,50),Ys(30,30),Zd(50,50),Yc(50,50),Vc(30),Ipot(50),Ipv(50)
      open(5,file='rastruz.dat')
      open(6,file='rastruz.prn')
      read(5,100) Npv,Nav,Nz,Nu,NuKS,mind,kond
100   format(16i5)
      write(6,101) Npv,Nav,Nz,Nu,NuKS
101   format(' Program RASRUZ',/,' Autor: Dr. Ivan Sarajcev',//,t30,
     *'ULAZNI PODACI',/,' Broj pasivnih vodica:',i3,/,' Broj aktivnih vo
     *dica:',i3,/,' Broj zona/podrucja razlicitih geofizikalnih svojstav
     *a tla:',i3,/,' Ukupni broj uzemljivaca:',i3,/,' Broj uzemljivaca k
     *oji je pogodjen JKS-em:',i3)
      if(mind.eq.0) write(6,102)
      if(mind.ne.0) write(6,103)
102   format(' Da li je nazocna medjusobna elektromagnetska sprega medju
     * pas. vodicima?  NE')
103   format(' Da li je nazocna medjusobna elektromagnetska sprega medju
     * pas. vodicima?  DA')
      if(kond.eq.0) write(6,104)
      if(kond.ne.0) write(6,105)
104   format(' Da li je nazocna medjusobna konduktivna sprega izmedju uz
     *emljivaca?  NE')
105   format(' Da li je nazocna medjusobna konduktivna sprega izmedju uz
     *emljivaca?  DA')
      if(Npv.eq.0) go to 1
c*****Podaci pasivnih vodica
      write(6,106)
106   format(/,' SUSTAV PASIVNIH VODICA:')
      do 2 i=1,Npv
      read(5,107) P(i),Q(i),R1(i),rr(i),mir(i)
107   format(2i5,3f10.3)
      write(6,108) i,P(i),Q(i),R1(i),rr(i),mir(i)
108   format(/,i3,'. vodic',2x,'P =',i2,2x,'Q =',i2,3x,'R1 =',f8.4, ' [O
     *hm/km]',3x,'r =',f6.1,' [mm]',3x,'mir =',f6.1)
c     R1(i)=R1(i)*1.e-3
      rr(i)=rr(i)*1.e-3
      do 2 k=1,Nz
      read(5,110) L(k,i),x(k,i),y(k,i)
110   format(8f10.3)
      write(6,109) k,L(k,i),x(k,i),y(k,i)
109   format(i8,'. zona',3x,'l =',f10.3,' [km]',3x,'x =',f10.3,' [m]',3x
     *,'y =',f10.3,' [m]')
2     continue
      if(mind.eq.0) go to 1
c*****Duzine paralelnog vodjenja
      do 3 k=1,Nz
      write(6,111) k
111   format(/,i3,'. zona; - duzine paralelnog vodjenja [km]:')
      do 3 i=2,Npv
      read(5,555) (Lm(k,i,j),j=1,i-1)
555   format(16f5.3)
      write(6,112) (Lm(k,i,j),j=1,i-1)
112   format(10f8.3)
3     continue
c*****Podaci struje JKS-a
1     read(5,110) r,fi,f
      write(6,113) r,fi,f
113   format(/,' Struja JKS-a =',f8.1,' /',f6.1,' [A]',3x,'f =',f6.1,
     *' [Hz]')
      pi=3.14159
      a=r*cos(fi*pi/180.)
      b=r*sin(fi*pi/180.)
      Ic(NuKS)=cmplx(a,b)
      if(Npv.eq.0) go to 4
      if(Nav.eq.0) go to 4
c*****Podaci aktivnih vodica
      do 5 i=1,Nav
      read(5,110) r,fi
      a=r*cos(fi*pi/180.)
      b=r*sin(fi*pi/180.)
      Ik(i)=cmplx(a,b)
      write(6,114) i,r,fi
114   format(/,i3,'. aktivni vodic:',3x,'Ik =',f8.2,' /',f6.1,' [A]')
      do 5 k=1,Nz
      read(5,110) xx(k,i),yy(k,i)
      write(6,115) k,xx(k,i),yy(k,i)
115   format(i8,'. zona',3x,'x =',f10.3,' [m]',3x,'y =',f10.3,' [m]')
5     continue
c*****Duljine paralelnog vodjenja akt. i pas vodica
      do 6 k=1,Nz
      write(6,116) k
116   format(/,i3,'. zona; - duzine paralelnog vodjenja izmedju aktivnih
     * i pasivnih vodica [km]:')
      do 6 i=1,Npv
      read(5,555) (Lma(k,i,j),j=1,Nav)
      write(6,112) (Lma(k,i,j),j=1,Nav)
6     continue
c*****Podaci impedancija i struja zvjezdista transformatora
4     do 7 i=1,Nu
      read(5,117) Zr,Zi,r,fi,Nv(i)
117   format(4f10.3,i5)
      Zu(i)=cmplx(Zr,Zi)
      a=r*cos(fi*pi/180.)
      b=r*sin(fi*pi/180.)
      if(i.eq.NuKS) Ic(i)=Ic(i)-cmplx(a,b)
      if(i.ne.NuKS) Ic(i)=-cmplx(a,b)
      write(6,118) i,Zr,Zi,r,fi,Nv(i)
118   format(/,i3,'. uzemlj.',3x,'Zu =',2f8.3,' [Ohm]',3x,'Iz =',f8.2,
     *' /',f6.1,' [A]',3x,'Nv =',i2)
7     continue
      if(kond.eq.0) go to 8
      write(6,119)
119   format(/,' Konduktivna sprega medju uzemljivacima [Ohm-i]:')
      do 9 i=2,Nu
      read(5,110) (Rmu(i,j),j=1,i-1)
      write(6,110) (Rmu(i,j),j=1,i-1)
9     continue
c*****Podaci o strujama 3Io dalekovoda/kabela i njihovim koef. redukcije
8     do 10 i=1,Nu
      j=Nv(i)
      if(j.eq.0) go to 10
      write(6,120) i
120   format(/,' Uzemljivac:',i3)
      do 11 k=1,j
      read(5,110) r,fi,a,b
      write(6,121) k,r,fi,a,b
121   format(' Dalekovod/kabel:',i2,3x,'3Io =',f8.2,' /',f6.1,' [A]',
     *3x,'Ki =',f6.3,' /',f6.1)
      Zr=r*cos(fi*pi/180.)
      Zi=r*sin(fi*pi/180.)
      c=a*cos(b*pi/180.)
      roo=a*sin(b*pi/180.)
11    Ic(i)=Ic(i)-(1.-cmplx(c,roo))*cmplx(Zr,Zi)
10    continue
c*****Podaci tla
      read(5,110) (ro(i),i=1,Nz)
      write(6,122) (ro(i),i=1,Nz)
122   format(/,' Specificni otpori zona [Ohm*m]:',/,(f10.3))
      write(6,123)
123   format(/,t30,'REZULTATI PRORACUNA',//,' Pocetne/zadane struje cvor
     *ista/uzemljivaca:')
      do 12 i=1,Nu
      Zap=Ic(i)
      call ZMK(Zap,r,fi)
      write(6,124) i,r,fi
124   format(i3,'. cvoriste',3x,'Ic =',f8.2,' /',f6.1,' [A]')
12    continue
      Rz=pi**2*f*1.e-4
      ak=4.*pi*f*1.e-4
      if(Npv) 14,13,14
c+++++Racun primitivne matrice [Zs], reda (Npv,Npv)
14    do 15 i=1,Npv
      do 15 j=1,i
      Zs(i,j)=cmplx(0.,0.)
      if(i-j) 17,16,17
16    do 18 k=1,Nz
      if(L(k,i).le.0.0001) go to 18
      Zr=(R1(i)+Rz)*L(k,i)
      Zi=ak*L(k,i)*(alog(658.*sqrt(ro(k)/f)/rr(i))+mir(i)/4.)
      Zs(i,i)=Zs(i,i)+cmplx(Zr,Zi)
18    continue
      go to 15
17    if(mind) 19,20,19
19    do 21 k=1,Nz
      if(Lm(k,i,j).le.0.0001) go to 21
      a=abs(x(k,i)-x(k,j))
      b=y(k,i)
      c=y(k,j)
      roo=ro(k)
      call ZEMED(a,b,c,roo,f,Zm)
      Zs(i,j)=Zs(i,j)+Zm*Lm(k,i,j)
21    continue
20    Zs(j,i)=Zs(i,j)
15    continue
c     write(6,125)
c125   format(/,' MATRICA [Zs]:')
c     do 22 i=1,Npv
c     write(6,110) (Zs(i,j),j=1,i)
c22    continue
      if(mind) 23,24,23
24    do 26 i=1,Npv
      Zs(i,i)=1./Zs(i,i)
26    continue
      go to 25
23    call INVER(Npv,Zs)
c25    write(6,126)
c126   format(/,' INVERZNA MATRICA 1/[Zs]:')
c     do 27 i=1,Npv
c     write(6,110) (Zs(i,j),j=1,i)
c27    continue
c-----Racun stupc. vektora [E], predstavlja EMS inducirane str. JKS-a
25    do 28 i=1,Npv
      E(i)=cmplx(0.,0.)
      do 28 j=1,Nav
      Zap=cmplx(0.,0.)
      do 29 k=1,Nz
      if(Lma(k,i,j).le.0.) go to 29
      if(abs(x(k,i)-xx(k,j))-0.001) 30,30,31
30    if(abs(y(k,i)-yy(k,j))-0.001) 32,32,31
32    Zr=Rz*Lma(k,i,j)
      Zi=ak*Lma(k,i,j)*alog(658.*sqrt(ro(k)/f)/rr(i))
      Zap=Zap+cmplx(Zr,Zi)
      go to 29
31    a=abs(x(k,i)-xx(k,j))
      b=y(k,i)
      c=yy(k,j)
      roo=ro(k)
      call ZEMED(a,b,c,roo,f,Zm)
      Zap=Zap+Zm*Lma(k,i,j)
29    continue
      E(i)=E(i)+Zap*Ik(j)
28    continue
      write(6,127)
127   format(/,' VEKTOR [E], predstavlja EMS inducirane strujama JKS-a:'
     *)
      do 33 i=1,Npv
      Zap=E(i)
      call ZMK(Zap,r,fi)
      write(6,128)i,r,fi
128   format(' E(',i2,') =',f8.1,' /',f6.1,' [V]')
33    continue
c#####Rijesavanje matricne jednadzbe [Ie]=1/[Zs]*[E]
      write(6,129)
129   format(/,' STRUJE PASIVNIH VODICA PROUZROCENE EM SPREGOM:')
      do 34 i=1,Npv
      Ie(i)=cmplx(0.,0.)
      do 34 j=1,Npv
      Ie(i)=Ie(i)+Zs(i,j)*E(j)
34    continue
      do 35 i=1,Npv
      Zap=Ie(i)
      call ZMK(Zap,r,fi)
      write(6,130) i,P(i),Q(i),r,fi
130   format(i3,'. vodic',3x,'Ipv(',i2,' -->',i3,') =',f8.2,' /',f6.1,
     *' [A]')
35    continue
c$$$$$Odredjivanje novih struja cvora mjerodavnih za proracun struja
c     prouzrocenih potencijalima uzemljivaca
      do 36 i=1,Npv
      ip=P(i)
      iq=Q(i)
      Ic(ip)=Ic(ip)-Ie(i)
      Ic(iq)=Ic(iq)+Ie(i)
36    continue
      write(6,131)
131   format(/,' STRUJE CVORISTA MJERODAVNE ZA TVORBU POTENCIJA UZEMLJIV
     *ACA:')
      do 37 i=1,Nu
      Zap=Ic(i)
      call ZMK(Zap,r,fi)
      write(6,124) i,r,fi
37    continue
c&&&&&Odredjivanje raspodjele struje uslijed potencijalnog lijevka
c-----Racun transponirane matrice incidencije [Mst]
      do 38 i=1,Npv
      do 38 j=1,Nu
      Mst(i,j)=0
38    continue
      do 39 i=1,Npv
      ip=P(i)
      iq=Q(i)
      Mst(i,ip)=-1
      Mst(i,iq)=1
39    continue
c     write(6,132)
c132   format(/,' TRANSPONIRANA MATRICA INCIDENCIJE [Ms]:')
c     do 40 i=1,Npv
c     write(6,133) (Mst(i,j),j=1,Nu)
c133   format(25i3)
c40    continue
c-----Odredjivanje matrice incidencije [Ms]
      do 41 i=1,Nu
      do 41 j=1,Npv
      Ms(i,j)=Mst(j,i)
41    continue
c     write(6,134)
c134   format(/,' MATRICA INCIDENCIJE [Ms]:')
c     do 42 i=1,Nu
c     write(6,133) (Ms(i,j),j=1,Npv)
c42    continue
c+++++Racun [Ys]=[Ms]*(1/[Zs])*[Mst]
      do 43 i=1,Nu
      do 43 j=1,Npv
      D(i,j)=cmplx(0.,0.)
      do 43 k=1,Npv
      D(i,j)=D(i,j)+Ms(i,k)*Zs(k,j)
43    continue
      do 44 i=1,Nu
      do 44 j=1,Nu
      Ys(i,j)=cmplx(0.,0.)
      do 44 k=1,Npv
      Ys(i,j)=Ys(i,j)+D(i,k)*Mst(k,j)
44    continue
c     write(6,135)
c135   format(/,' MATRICA [Ys]:')
c     do 45 i=1,Nu
c     write(6,110) (Ys(i,j),j=1,i)
c45    continue
c^^^^ Racun matrice [Zd]
      do 46 i=1,Nu
      do 46 j=1,i
      if(i-j) 48,47,48
47    Zd(i,i)=Zu(i)
      go to 46
48    if(kond) 49,50,49
49    Zd(i,j)=cmplx(Rmu(i,j),0.)
      go to 51
50    Zd(i,j)=cmplx(0.,0.)
51    Zd(j,i)=Zd(i,j)
46    continue
c     write(6,136)
c136   format(/,' MATRICA [Zd]:')
c     do 52 i=1,Nu
c     write(6,110) (Zd(i,j),j=1,i)
c52    continue
c~~~~ Racun [Yd]=1/[Zd]
      if(kond) 53,92,53
92    do 54 i=1,Nu
      Zd(i,i)=1./Zd(i,i)
54    continue
      go to 55
53    call INVER(Nu,Zd)
c55    write(6,137)
c137   format(/,' MATRICA [Yd]:')
c     do 56 i=1,Nu
c     write(6,110) (Zd(i,j),j=1,i)
c56    continue
c@@@@ Racun matrice vlastitih i medjusobnih  admitancija cvorista
55    do 57 i=1,Nu
      do 57 j=1,Nu
      Yc(i,j)=Ys(i,j)+Zd(i,j)
57    continue
c     write(6,138)
c138   format(/,' MATRICA VLASTITIH I MEDJUSOBNIH ADMITANCIJA CVORISTA [Y
c    *c]:')
c     do 58 i=1,Nu
c     write(6,110) (Yc(i,j),j=1,i)
c58    continue
c!!!! Odredjivanje napona cvorista [Vc]=(1/[Zc])*[Ic]
      call INVER(Nu,Yc)
c     write(6,139)
c139   format(/,' MATRICA [Zc]=1/[Yc]')
c     do 59 i=1,Nu
c     write(6,110) (Yc(i,j),j=1,i)
c59    continue
      do 60 i=1,Nu
      Vc(i)=cmplx(0.,0.)
      do 60 j=1,Nu
      Vc(i)=Vc(i)+Yc(i,j)*Ic(j)
60    continue
      write(6,140)
140   format(/,' POTENCIJALI UZEMLJIVACA:')
      do 61 i=1,Nu
      Zap=Vc(i)
      call ZMK(Zap,r,fi)
      write(6,141) i,r,fi
141   format(' Vu(',i2,') =',f9.2,' /',f6.1,' [V]')
61    continue
c$$$$ Odredjivanje struja uzemljivaca
      write(6,142)
142   format(/,' STRUJE UZEMLJIVACA:')
      do 62 i=1,Nu
      Iu(i)=cmplx(0.,0.)
      do 62 j=1,Nu
      Iu(i)=Iu(i)+Zd(i,j)*Vc(j)
62    continue
      do 63 i=1,Nu
      Zap=Iu(i)
      call ZMK(Zap,r,fi)
      write(6,143) i,r,fi
143   format(' Iu(',i2,') =',f8.2,' /',f6.1,' [A]')
63    continue
c@@@@@Odredjivanje struja u pas. vodicima uslijed potencijala uzemlj.
      do 64 i=1,Npv
      do 64 j=1,Nu
      D(i,j)=cmplx(0.,0.)
      do 64 k=1,Npv
      D(i,j)=D(i,j)+Zs(i,k)*Mst(k,j)
64    continue
      do 65 i=1,Npv
      Ipot(i)=cmplx(0.,0.)
      do 65 j=1,Nu
      Ipot(i)=Ipot(i)+D(i,j)*Vc(j)
65    continue
      write(6,144)
144   format(/,' STRUJE PASIVNIH VODICA PROUZROCENE POTENCIJALIMA UZEMLJ
     *IVACA:')
      do 66 i=1,Npv
      Zap=Ipot(i)
      call ZMK(Zap,r,fi)
      write(6,130) i,P(i),Q(i),r,fi
66    continue
ccccc Odredjivanje ukupnih struja u pasivnim vodicima
      write(6,146)
146   format(/,' UKUPNE STRUJE U PASIVNIM VODICIMA:')
      do 67 i=1,Npv
      Ipv(i)=Ie(i)+Ipot(i)
      Zap=Ipv(i)
      call ZMK(Zap,r,fi)
      write(6,130) i,P(i),Q(i),r,fi
67    continue
      go to 68
13    Zap=Ic(NuKS)*Zu(NuKS)
      call ZMK(Zap,r,fi)
      write(6,147) r,fi
147   format(/,' Vu =',f9.2,' /',f6.1,' [V]')
68    stop
      end
c*****Potprogram INVER
      subroutine INVER(n,z)
      complex z(50,50)
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
c*****Potprogram ZMK
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