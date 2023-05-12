c*****Program JKSDV za proracun JKS-a na dvostrukom vodu
      real lv,lrs
      integer Un,f,Tipus(100),lr(100),s(100),t(100)
      dimension x(100),y(100),ro(100),cr(100),Rs(100)
      complex Zd1,Zo1,Zmo1,Zm1cr,Zg1,ki,zg(100),z(100,100),Zds,Zos,Io,
     *Ik1,Ic(100),Vc(100),Ist(100),Zr(100),Zdv,Vu(100),Iu(100),Itt,Vu1
      open(5,file='jksdv.dat')
      open(6,file='jksdv.prn')
      READ(5,100) Un,f,k,Nr,NTipus,m
100   format(16i5)
      write(6,101) Un,f,k,Nr,NTipus,m
101   format(' Un =',i4,' [kV]',5x,'f =',i3,' [Hz]',/,
     *' k =',i3,5x,'Nr =',i3,5x,'NTipus =',i2,/,' Medju utjecaj =',i2)
      READ(5,200) Rf1,rfcr,dil
200   format(8f10.3)
      write(6,102) Rf1,rfcr,dil
102   format(/,' Rf1 =',f7.4,' [Ohm/km]',/,' rf-cr =',f7.3,' [mm]',/,
     *' dil =',f7.3,' [m]')
      READ(5,200) Rg1,rgcr
      write(6,103) Rg1,rgcr
103   format(/,' Rg1 =',f7.4,' [Ohm/km]',/,' rg-cr =',f7.3,' [mm]',//,
     *3x,'x [m]',5x,'y [m]')
      do 1 i=1,7
      READ(5,200) x(i),y(i)
      write(6,200) x(i),y(i)
1     continue
      READ(5,200) lv,lrs,ros,Rss
      write(6,104) lv,lrs,ros,Rss
104   format(/,' l =',f8.3,' [km]',/,' Prosjecne vrijednosti:',/,
     *' -raspona:',f6.3,' [km]',/,' -otp.tla:',f7.1,' [Ohm*m]',/,
     *' -uzem.stupa:',f7.1,' [Ohm]')
      READ(5,100) (lr(i),i=1,Nr)
      write(6,105) (lr(i),i=1,Nr)
105   format(/,' Duljine raspona [m]:',/,(16i5))
      Ns=Nr-1
      READ(5,100) (Tipus(i),i=1,Ns)
      write(6,106) (Tipus(i),i=1,Ns)
106   format(/,' Tipovi uzemljivaca stupova:',/,(16i5))
      READ(5,200) (ro(i),i=1,NTipus)
      write(6,107) (ro(i),i=1,NTipus)
107   format(/,' Ro-uzemlj. stupa [Ohm*m]:',/,(8f10.2))
      READ(5,200) Rad,Xad, Rao,Xao, Rau,Xau
      write(6,108) Rad,Xad,Rao,Xao,Rau,Xau
108   format(/,' Podaci strane A:',/,' Zad =',2f10.3,' [Ohm]',/,
     *' Zao =',2f10.3,' [Ohm]',/,' Zau =',2f10.5,' [Ohm]')
      READ(5,200) Rbd,Xbd, Rbo,Xbo, Rbu,Xbu
      write(6,109) Rbd,Xbd,Rbo,Xbo,Rbu,Xbu
109   format(/,' Podaci strane B:',/,' Zbd =',2f10.3,' [Ohm]',/,
     *' Zbo =',2f10.3,' [Ohm]',/,' Zbu =',2f10.5,' [Ohm]')
c-----Konstante programa
      pi=3.14159
      Rz1=pi**2*f*1.e-4
      De=658.*sqrt(ros/f)
      a=4.*pi*f*1.e-4
      cr(1)=0.0364
      cr(2)=0.0257
      cr(3)=0.02
      cr(4)=0.0624
      cr(5)=0.0389
      cr(6)=0.0253
c-----Odredjivanje duljina voda lak i lbk
      lak=0
      do 2 i=1,k
      lak=lak+lr(i)
2     continue
      lbk=ifix(lv*1000)-lak
      write(6,110) lak,lbk
110   format(//,t20,'REZULTATI PRORACUNA',/,
     *' la-k =',i5,' [m]',/,' lb-k =',i5,' [m]')
c-----Odredjivanje imp. Zd1
      zrr=Rf1
      Dm=1.
      do 4 i=1,2
      do 4 j=i,3
      if(i-j) 5,4,5
5     Dm=Dm*sqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
4     continue
      Dm=Dm**(1./3.)
      zi=a*alog(Dm*1.e3/rfcr)
      Zd1=cmplx(zrr,zi)
      write(6,111) Zd1
111   format(/,' Zd1 =',2f10.5,' [Ohm/km]')
c-----Odredjuvanje imp. Zo1
      zrr=Rf1+3.*Rz1
      ds=(rfcr*1.e-3*Dm**2)**(1./3.)
      zi=3.*a*alog(De/ds)
      Zo1=cmplx(zrr,zi)
      write(6,112) Zo1
112   format(' Zo1 (bez utjecaja zast.uzeta) =',2f10.5,' [Ohm/km]')
c + + Odr. imp. Zg1 i Zm1cr
      zrr=Rg1+Rz1
      zi=a*alog(De*1.e3/rgcr)
      Zg1=cmplx(zrr,zi)
      zrr=Rz1
      Dm=1.
      do 6 i=1,3
      Dm=Dm*sqrt((x(7)-x(i))**2+(y(7)-y(i)+dil)**2)
6     continue
      Dm=Dm**(1./3.)
      zi=a*alog(De/Dm)
      Zm1cr=cmplx(zrr,zi)
      write(6,300) Zg1,Zm1cr
300   format(' Zg1 =',2f10.5,' [Ohm/km]',/,' Zm1cr =',2f10.5,
     *' [Ohm/km]')
c + + +
      Zo1=Zo1-3.*Zm1cr**2/Zg1
      write(6,113) Zo1
113   format(' Zo1 (sa utjecajem zast.uzeta) =',2f10.5,' [Ohm/km]')
c-----Odredjivanje imp. Zmo1
      zrr=3.*Rz1
      Dm=1.
      do 7 i=1,3
      do 7 j=4,6
      Dm=Dm*sqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
7     continue
      Dm=Dm**(1./9.)
      zi=3.*a*alog(De/Dm)
      Zmo1=cmplx(zrr,zi)
      write(6,114) Zmo1
114   format(' Zmo1 (bez utjecaja zast.uyeta) =',2f10.5,' [Ohm/km]')
      Zmo1=Zmo1-3.*Zm1cr**2/Zg1
      if(m.eq.0) Zmo1=Zmo1*m
      write(6,115) Zmo1
115   format(' Zmo1 (sa utjecajem zast.uyeta) =',2f10.5,' [Ohm/km]')
c-----Odredjivanje koeficijenta ki
      ki=1.-Zm1cr/Zg1
      write(6,116) ki
116   format(/,' ki =',2f8.4)
c*****RACUN JKS-a
c-----Podaci grana mreze (dir.sustav)
      ih=1
      n=3
      s(1)=0
      t(1)=1
      zg(1)=cmplx(Rad,Xad)
      s(2)=0
      t(2)=2
      zg(2)=cmplx(Rbd,Xbd)
      s(3)=1
      t(3)=2
      zg(3)=Zd1*lv
      s(4)=1
      t(4)=3
      zg(4)=Zd1*lak*1.e-3
      s(5)=2
      t(5)=3
      zg(5)=Zd1*lbk*1.e-3
8     write(6,117) ih
117   format(/,i2,'. SUSTAV:',//,' Cvor-s',t10,'Cvor-t',t25,
     *'Zs-t [Ohm]')
      do 9 i=1,5
      write(6,118) s(i),t(i),zg(i)
118   format(i5,t10,i4,t20,2f10.5)
9     continue
      do 10 i=1,n
      do 10 j=1,n
      z(i,j)=cmplx(0.,0.)
10    continue
      do 11 i=1,5
      is=s(i)
      it=t(i)
      z(it,it)=z(it,it)+1./zg(i)
      if(is) 12,11,12
12    z(is,is)=z(is,is)+1./zg(i)
      z(is,it)=-1./zg(i)
      z(it,is)=z(is,it)
11    continue
c     write(6,119)
c119  format(/,' Matrica [Yc]:')
c     do 13 i=1,n
c     write(6,120) (z(i,j),j=1,n)
c120  format(8f10.4)
c13   continue
      call INVER(n,z)
c     write(6,121)
c121  format(/,' Matrica [Zc]:')
c     do 14 i=1,n
c     write(6,120) (z(i,j),j=1,n)
c14   continue
      if(ih) 15,16,15
15    Zds=z(3,3)
      s(1)=0
      t(1)=1
      zg(1)=cmplx(Rao,Xao)+Zmo1*lak*1.e-3
      s(2)=0
      t(2)=2
      zg(2)=cmplx(Rbo,Xbo)+Zmo1*lbk*1.e-3
      s(3)=1
      t(3)=2
      zg(3)=(Zo1-Zmo1)*lv
      s(4)=1
      t(4)=3
      zg(4)=(Zo1-Zmo1)*lak*1.e-3
      s(5)=2
      t(5)=3
      zg(5)=(Zo1-Zmo1)*lbk*1.e-3
      ih=ih-1
      go to 8
16    Zos=z(3,3)
      Io=1.1*Un*1.e3/(sqrt(3.)*(2.*Zds+Zos))
      write(6,255) Io
255   format(/,' Io =',2f10.3,' [A]')
      Ik1=3.*Io
      call ZMK(Ik1,rmod,akut)
      write(6,122) rmod,akut
122   format(/,' Ik1 =',f8.1,' /',f7.2,' [A]')
c/////Raspodjela struja JKS-a u mrezi
      do 17 i=1,n
      Ic(i)=cmplx(0.,0.)
17    continue
      Ic(3)=-Io
      write(6,123) (Ic(i),i=1,n)
123   format(/,' Struje cvora [A]:',/,(2f10.3))
      do 18 i=1,n
      Vc(i)=cmplx(0.,0.)
      do 18 j=1,n
      Vc(i)=Vc(i)+z(i,j)*Ic(j)
18    continue
      write(6,124) (Vc(i),i=1,n)
124   format(/,' Naponi cvora [V]:',/,(2f10.3))
      do 19 i=1,5
      is=s(i)
      it=t(i)
      if(is) 20,21,20
20    Ist(i)=(Vc(is)-Vc(it))/zg(i)
      go to 19
21    Ist(i)=-Vc(it)/zg(i)
19    continue
      write(6,125)
125   format(/,' Cvor-s',t10,'Cvor-t',t25,'Is-t [A]')
      do 22 i=1,5
      write(6,126) s(i),t(i),Ist(i)
126   format(i5,t10,i4,t20,2f10.3)
22    continue
      Itt=3.*Ist(1)
      call ZMK(Itt,rmod,akut)
      write(6,220) rmod,akut
220   format(/,' It =',f8.1,' /',f7.2,' [A]')
c-----Odredjivanja otp.uzem.stupa (Rs(i)) i imp. raspona (Zr(i))
      do 23 i=1,Ns
      ii=Tipus(i)
      Rs(i)=ro(ii)*cr(ii)
      Zr(i)=Zg1*lr(i)*1.e-3
23    continue
      Zr(Nr)=Zg1*lr(Nr)*1.e-3
      write(6,127) (Rs(i),i=1,Ns)
127   format(/,' Otpori uzemljenja stupova:',/,(8f10.3))
      write(6,128) (Zr(i),i=1,Nr)
128   format(/,' Impedancije zast.uzeta po rasponima:',/,(8f10.3))
c-----Odredjivanje imp. uzemljenja voda
      Zdv=Rss/(0.5+csqrt(Rss/(Zg1*lrs)+0.25))
      write(6,129) Zdv
129   format(/,' Zdv =',2f10.3,' [Ohm]')
      n=Nr+1
      do 24 i=1,n
      do 24 j=1,n
      z(i,j)=cmplx(0.,0.)
24    continue
      do 25 i=1,Nr
      j=i+1
      if(i-1) 27,26,27
26    z(i,i)=1./cmplx(Rau,Xau)+1./Zr(i)
      go to 28
27    z(i,i)=1./Rs(i-1)+1./Zr(i)+1./Zr(i-1)
28    z(i,j)=-1./Zr(j-1)
      z(j,i)=z(i,j)
25    continue
      z(n,n)=1./Zdv+1./Zr(Nr)
c     write(6,130)
c130  format(/,' Matrica [Yu]:')
c     do 29 i=1,n
c     write(6,200) (z(i,j),j=1,n)
c29   continue
      call INVER(n,z)
c     write(6,131)
c131  format(/,' Matrica [Zu]:')
c     do 30 i=1,n
c     write(6,200) (z(i,j),j=1,n)
c30   continue
c-----Odredjivanje struja cvora
      do 31 i=1,n
      Ic(i)=cmplx(0.,0.)
31    continue
      Ic(1)=-3.*Ist(1)*ki
      Ic(k+1)=Ik1*ki
      write(6,132) (Ic(i),i=1,n)
 132  format(/,' Struje [Ic]:',/,(8f10.3))
      do 32 i=1,n
      Vu(i)=cmplx(0.,0.)
      do 32 j=1,n
      Vu(i)=Vu(i)+z(i,j)*Ic(j)
32    continue
      write(6,233) (Vu(i),i=1,n)
233   format(/,' Potencijali uyemljivaca:',/,(8f10.3))
      Vu1=Vu(1)
      call ZMK(Vu1,rmod,akut)
      write(6,222) rmod,akut
222   format(/,' Vu(1) =',f8.1,' /',f7.2,' [V]')
      do 33 i=2,Nr
      Iu(i)=Vu(i)/Rs(i-1)
33    continue
      Iu(1)=Vu(1)/cmplx(Rau,Xau)
      Iu(n)=Vu(n)/Zdv
      write(6,133) (Iu(i),i=1,n)
133   format(/,' Struje uzemljivaca [A]:',/,(8f10.3))
      stop
      end
c*****Potprogram INVER
      subroutine INVER(n,z)
      complex z(100,100)
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
c*****Potprogram  Z M K
      subroutine ZMK(zz,zmod,akut)
      complex zz
      pi=3.14159
      r=real(zz)
      ai=aimag(zz)
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