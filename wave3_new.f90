                                                    program HELEN
                                                   include 'par.fi'
                                                   include 'com.fi'
character*80 NAME,NAME_REST
IP(0)=0; HG=0;                       call rdft2d(N,N,NY,1,HG,IP,WK)
                                                do NVAR=NBOT,NTOP
if (NVAR<10) then
write(NAME,fmt="('Data/results0',i1)") NVAR; else
write(NAME,fmt="('Data/results',i2)")  NVAR; endif
open(11,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/surface0',i1)") NVAR; else
write(NAME,fmt="('Data/surface',i2)")  NVAR; endif
open(12,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/integra0',i1)") NVAR; else
write(NAME,fmt="('Data/integra',i2)")  NVAR; endif
open(13,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/spectr0',i1)") NVAR; else
write(NAME,fmt="('Data/spectr',i2)")  NVAR; endif
open(16,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/omega0',i1)") NVAR; else
write(NAME,fmt="('Data/omega',i2)")  NVAR; endif
open(23,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/rnnli0',i1)") NVAR; else
write(NAME,fmt="('Data/rnnli',i2)")  NVAR; endif
open(25,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/rntai0',i1)") NVAR; else
write(NAME,fmt="('Data/rntai',i2)")  NVAR; endif
open(26,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/rbrea0',i1)") NVAR; else
write(NAME,fmt="('Data/rbrea',i2)")  NVAR; endif
open(27,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/rinpu0',i1)") NVAR; else
write(NAME,fmt="('Data/rinpu',i2)")  NVAR; endif
open(28,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/break0',i1)") NVAR; else
write(NAME,fmt="('Data/break',i2)")  NVAR; endif
open(29,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/spehx0',i1)") NVAR; else
write(NAME,fmt="('Data/spehx',i2)")  NVAR; endif
open(30,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/sphxx0',i1)") NVAR; else
write(NAME,fmt="('Data/sphxx',i2)")  NVAR; endif
open(31,file=NAME)
!if (NVAR<10) then
!write(NAME,fmt="('Data/reserv0',i1)") NVAR; else
!write(NAME,fmt="('Data/reserv',i2)")  NVAR; endif
!open(32,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/rtota0',i1)") NVAR; else
write(NAME,fmt="('Data/rtota',i2)")  NVAR; endif
open(33,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/spfzf0',i1)") NVAR; else
write(NAME,fmt="('Data/spfzf',i2)")  NVAR; endif
open(34,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/spwnf0',i1)") NVAR; else
write(NAME,fmt="('Data/spwnf',i2)")  NVAR; endif
open(35,file=NAME)
!
if (DT_VELO>0.) then
if (NVAR<10) then
write(NAME,fmt="('Data/grid0',i1)") NVAR; else
write(NAME,fmt="('Data/grid',i2)")  NVAR; endif
open(18,file=NAME);
if (NVAR<10) then
write(NAME,fmt="('Data/Uvelocity0',i1)") NVAR; else
write(NAME,fmt="('Data/Uvelocity',i2)")  NVAR; endif
open(19,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/Vvelocity0',i1)") NVAR; else
write(NAME,fmt="('Data/Vvelocity',i2)")  NVAR; endif
open(20,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/Wvelocity0',i1)") NVAR; else
write(NAME,fmt="('Data/Wvelocity',i2)")  NVAR; endif
open(21,file=NAME)
if (NVAR<10) then
write(NAME,fmt="('Data/En_spectr0',i1)") NVAR; else
write(NAME,fmt="('Data/En_spectr',i2)")  NVAR; endif
open(22,file=NAME);
endif
                            if (DT_FIELDS>0) then
if (NVAR<10) then
write(NAME,fmt="('Data\fields0',i1)") NVAR; else
write(NAME,fmt="('DATA\fields',i2)")  NVAR; endif
open(14,file=NAME);                         endif
if (NVAR<10) then
write(NAME_REST,fmt="('Data/restart0',i1)") NVAR; else
write(NAME_REST,fmt="('Data/restart',i2)")  NVAR; endif
open(15,file=NAME_REST,form='unformatted')
                                            call PRINTPAR
                                            call SHEININ_FILTER
if (NVAR==0) then; NSTI=0;              call INITIAL_CONDITIONS
                                                       endif
!*********************************************Reading restart
                   if (NVAR>0) then; NREST=NVAR-1
	print*,NBOT,NVAR
if (NREST<10) then
write(NAME,fmt="('Data/restart0',i1)") NREST; else
write(NAME,fmt="('Data/restart',i2)")  NREST; endif
open(15,file=NAME,form='unformatted')
read(15) NSTI,T,E0,E00,HF,FF
print*,     'Continuation, NVAR=',NVAR,' T=',T
write(11,*) 'Continuation, NVAR=',NVAR,' T=',T
close(15);	NSTI=NVAR*NSTF;  NST=NSTI;  				endif
!************************************************************** 
ENCR=1; COR=1.;                            TINI=VREMYA()
PRINT*,'TINI=',TINI
call FI(HF,HG,1)
HM=sum(HG)/N/NY; DH=sqrt(sum((HG-HM)**2)/N/NY)
AD=D0+D1*DH+D2*DH**2
print('(2(a4,f9.5))'),' DH=',DH,' AD=',AD
NPHYS=0; NNS=0; NBR=0; GROSS=0.
                                 call VERTICAL_GRID
9999 continue                                
                                            call RUNGE_KUTT
if (NST.lt.NSTI+NSTF) goto 9999			
        									call DISP_REL(1)
if (DT_VELO>0.)                             call VELOCITY										
                                            TFIN=VREMYA()
NSTM1=NST-1
open(15,file=NAME_REST,form='unformatted')
write(15) NSTM1,T,E0,E00,HF,FF
close(15)
!
DTIME=TFIN-TINI
IF(DTIME<0) DTIME=DTIME+86400.
SPEED=DTIME/(NST-NSTI)
print*,'Time=',DTIME,' sec,' ,SPEED,' secs for 1 step'
print*,86400./SPEED*DT,' advance for 1 day'
write(11,*)'Time=',TFIN-TINI,' sec,' ,SPEED,' secs for 1 step'
write(11,*) 86400./SPEED*DT,' advance for 1 day'
close(11); close(12); close(13); close(14); close(15); close(17)
close(18); close(19); close(20); close(21); close(22); close(23) 
close(25); close(26); close(27); close(28); close(29); close(30)
close(31); close(32); close(33); close(34); close(35)
											enddo! NVAR
0002 continue
print*,'COBCEM BCE'
                                                  ; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  subroutine call_OUTPUT                                                   
                                        include 'par.fi'
if (JUST(DT_PHY)>0.) 			        call PHYSICS_OUT
if (JUST(DT_SURF)>0.)			        call BREAK_WRITE
if (JUST(DT_DISP)>0)                    call DISP_REL(0)
if (JUST(DT_SURF)>0.)                   call SURFACE
if (JUST(DT_OUTP)>0.)                   call OUTPUT 
                                            return; end 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                  subroutine PHYSICS_OUT
                                        include 'par.fi'
                                        include 'com.fi'
write(16,fmt='(10e12.4)') SPECH
write(25,fmt='(10e12.4)') RNNLI
write(26,fmt='(10e12.4)') RNTAI
write(27,fmt='(10e12.4)') RBREA
write(28,fmt='(10e12.4)') RINPU
write(30,fmt='(10e12.4)') SPEHX
write(31,fmt='(10e12.4)') SPHXX
write(33,fmt='(10e12.4)') RTOTA
write(34,fmt='(10e12.4)') SPFZF
write(35,fmt='(10e12.4)') SPWNF
NPHYS=0; NBR=0
                                             return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                           subroutine INITIAL_CONDITIONS
                                        include 'par.fi'
                                        include 'com.fi'
real,dimension(-M:M)::ZF0,FF0
real,dimension(-M:M,-MY:MY)::EF
if (JPM>0)                                           call PM_JONSWAP 
!*******************************************************-D potential
SF=0.;                                   
call FI(HF,HG,1); call FI(FF,FG,1)
call MIMA(' HG  ',HG); call MIMA(' FG  ',FG);
call DFDX2(HF,HXF); call FI(HXF,HXG,1); call MIMA(' HXG ',HXG)
call DFDY2(HF,HYF); call FI(HYF,HYG,1); call MIMA(' HYG ',HYG)
FZF=AK*FF; call FI(FZF,FZG,1); call MIMA(' FZG ',FZG)
STEEP=sqrt(sum(HXG**2+HYG**2)/N/NY)
HM=sum(HG)/N/NY; EP=sum((HG-HM)**2)/N/NY/2.; HS=4.*sqrt(2.*EP)
call ENERGY(HF,FF,EF); E0=sum(EF); E00=E0
print(       '(4(a7,e14.6))'),                                     &
' STEEP=',STEEP,'    EP=',EP,'    HS=',HS,'    E0=',E0
write(11,fmt='(4(a7,e14.6))')                                      &
' STEEP=',STEEP,'    EP=',EP,'    HS=',HS,'    E0=',E0
call ENERGY(HF,FF,EF); E0=sum(EF); E00=E0
                                                         return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                             subroutine RUNGE_KUTT
                                                  include 'par.fi'
                                                  include 'com.fi'
real,dimension(-M:M,-MY:MY)::WF,RH1,RH2,RH3,RH4,FF1,HF1,RF1,RF2,RF3,RF4
call RS(FF ,HF ,RF1,RH1); FF1=FF+RF1*DT2;  HF1=HF+RH1*DT2
call RS(FF1,HF1,RF2,RH2); FF1=FF+RF2*DT2;  HF1=HF+RH2*DT2
call RS(FF1,HF1,RF3,RH3); FF1=FF+RF3*DT;   HF1=HF+RH3*DT
call RS(FF1,HF1,RF4,RH4)
RF=(RF1+2.*RF2+2.*RF3+RF4)/6.;  RH=(RH1+2.*RH2+2.*RH3+RH4)/6.
call SOURCES; 
NST=NST+1; T=T+DT; call CALL_OUTPUT
	                                                   return; end 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                                subroutine SOURCES
                                                  include 'par.fi'
                                                  include 'com.fi'
real,dimension(-M:M,-MY:MY)::EF,WF
!*************************************************Nonlinearity
call ENERGY(HF,FF,EBEF); EBE=sum(EBEF)
FF=FF+RF*DT; HF=HF+RH*DT
if (ISTAB>0) then; 
call ENERGY(HF,FF,EAFT); EAF=sum(EAFT)
COR=sqrt(EBE/EAF); 
FF=FF*COR; HF=HF*COR
call ENERGY(HF,FF,EAFT); E11=sum(EAFT)
SP2=(EAFT-EBEF)/DT; EBEF0=EAFT
RNNLI=(RNNLI*NPHYS+SP2)/(NPHYS+1); endif
!**************************************************Wind input
if (INPUT>0) then; call WIND_INPUT(HF)
EBEF=EAFT; FF=FF+PF*DT; 
call ENERGY(HF,FF,EAFT); SP2=(EAFT-EBEF)/DT 
RINPU=(RINPU*NPHYS+SP2)/(NPHYS+1); endif
!********************************************Tail dissipation
if (ITAIL>0) then; 
EBEF=EAFT; HF=HF-SHEI*HF*DT; FF=FF-SHEI*FF*DT; 
call ENERGY(HF,FF,EAFT); SP2=(EAFT-EBEF)/DT
RNTAI=(RNTAI*NPHYS+SP2)/(NPHYS+1); endif
!****************************************************Breaking
if (IBREAK>0) then; call BREAKING(FF,HF)
EBEF=EAFT; HF=HF+HBR*DT; FF=FF+FBR*DT 
call ENERGY(HF,FF,EAFT); SP2=(EAFT-EBEF)/DT
RBREA=(RBREA*NPHYS+SP2)/(NPHYS+1); endif
!******************************************************Total 
call ENERGY(HF,FF,EAFT); E22=sum(EAFT)
SP2=(EAFT-EBEF0)/DT
RTOTA=(RTOTA*NPHYS+SP2)/(NPHYS+1)
GROSS0=GROSS; GROSS=GROSS+sum(SP2)*DT
DCR=GROSS-GROSS0; DEPK=(E22-E11); ECOR=DCR-DEPK
!**************************************************Spectr H
call SPECTRUM(HF,SP2); IJS=maxloc(SP2)
SPECH=(SPECH*NPHYS+SP2)/(NPHYS+1)
!**************************************************Spectr HX
call SPECTRUM(HXF,SP2);
SPEHX=(SPEHX*NPHYS+SP2)/(NPHYS+1)
!************************************************Spectr HXXF
call SPECTRUM(HXXF,SP2);
SPHXX=(SPHXX*NPHYS+SP2)/(NPHYS+1)
!************************************************Spectra FZF
call SPECTRUM(FZF,SP2)
SPFZF=(SPFZF*NPHYS+SP2)/(NPHYS+1)
!***********************************************Spectra FFOM
call FD(W1G,WF,1); call SPECTRUM(WF,SP2)
SPWNF=(SPWNF*NPHYS+SP2)/(NPHYS+1)
call ENERGY(HF,FF,EF); EPK=sum(EF)
ETO=(ETO*NPHYS+EPK)/(NPHYS+1)
ENCR=EPK/E0; DE=EPK-E00; DIFF=DE-GROSS
NPHYS=NPHYS+1
	                                                   return; end													    
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                         subroutine RS(F,H,RFF,RHF)
                                                    include 'par.fi'
                                                    include 'com.fi'
real,dimension(-M:M,-MY:MY)::F,H,RFF,RHF
real,dimension(N,NY)::WG
call DFDX2(H,HXF); call FI(HXF,HXG,1)
call DFDY2(H,HYF); call FI(HYF,HYG,1)
call DFDX2(F,FXF); call DFDY2(F,FYF)
call FI(FXF,FXG,1); call FI(FYF,FYG,1);
                                       call POISSON(F,H)
if (JUST(DT_OUTP)>0.)                  call VERTICAL_VELOCITY(F,H)
WG=1.+HXY2
RHG=-HXG*FXG-HYG*FYG+WG*FZG
RFG=-.5*(FXG**2+FYG**2-WG*FZG**2)
call FD(RHG,RHF,1)
call FD(RFG,RFF,1); RFF=RFF-H
                                                         return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$NEW
                                              subroutine POISSON(F,H)
                                                    include 'par.fi'
                                                    include 'com.fi'
real,dimension(-M:M,-MY:MY)::F,H,WF,W0F
real,dimension(-M:M,-MY:MY,0:LW-1)::B,C
                                                      do ITL=1,MAXIT
                                                       call R_S(F,H)
!***********************************************************Progonka
WF=-AK2+A2(1); B(:,:,1)=-A3(1)/WF;  C(:,:,1)=R(:,:,1)/WF          !
do j=2,LW-1; WF=(-AK2+A2(j))+A1(j)*B(:,:,j-1); B(:,:,j)=-A3(j)/WF ! 
C(:,:,j)=(R(:,:,j)-A1(j)*C(:,:,j-1))/WF; enddo                    !
SF(:,:,LW-1)=C(:,:,LW-1)/(1.-B(:,:,LW-1))                         !
do j=LW-2,1,-1; SF(:,:,j)=B(:,:,j)*SF(:,:,j+1)+C(:,:,j); enddo    !
SF(:,:,LW)=SF(:,:,LW-1); FZF=Z3*SF(:,:,1)+Z4*SF(:,:,2)            !
if (ITL==1) then; W0F=FZF; else; ERR=maxval(abs(FZF-W0F)) 
                                                    if (NST<4) then; 
print(       '(2(a5,i5),a5,e12.4)'),'NST=',NST,'ITL=',ITL,'ERR=',ERR
write(11,fmt='(2(a5,i5),a5,e12.4)') 'NST=',NST,'ITL=',ITL,'ERR=',ERR
                                                              endif
if (ERR<ERRLI) exit; W0F=FZF; endif 
                                                        enddo !ITL
FZF=FZF+AK*F; call FI(FZF,FZG,1)
                                                       return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Right side for POISSON
                                                subroutine R_S(F,H)
                                                    include 'par.fi'
                                                    include 'com.fi'
real,dimension(-M:M,-MY:MY,0:LW-1)::WL,W2L,WF,W1F
real,dimension(N,NY,0:LW-1)::WG
real,dimension(-M:M,-MY:MY)::F,H,WF1
real,dimension(N,NY)::WG1
save WL,W2L
!****************************************************Linear components
if (ITL==1) then; do j=0,LW-1; WL(:,:,j)=AK*F*EAKZ(:,:,j)
                            W2L(:,:,j)=AK2*F*EAKZ(:,:,j); enddo; 
WF1=-AK2*H; call FI(WF1,D2HG,1); HXY2=HXG**2+HYG**2;      endif 
!******************************************************************WIF
do j=1,LW-1; WF(:,:,j)=                                              &
A4(j)*SF(:,:,j-1)+A5(j)*SF(:,:,j)+A6(j)*SF(:,:,j+1); enddo
    !WF(:,:,0)=WF(:,:,1)*Z1+WF(:,:,2)*Z2; 
     WF(:,:,0)=Z3*SF(:,:,1)+Z4*SF(:,:,2)
WF1=WF(:,:,0); call FI(WF1,W1G,1); WF=WF+WL 
call FI(WF,WIG,LW-1); WIG(:,:,LW)=0.
R=0.
!***************************************************************2HxFxz
call DFDX3(WF,W1F,LW-1); call FI(W1F,WG,LW-1)
do j=0,LW-1; WG(:,:,j)=2.*HXG*WG(:,:,j); enddo; call FD(WG,W1F,LW)
                                                              R=W1F
!***************************************************************2HyFyz
call DFDY3(WF,W1F,LW-1); call FI(W1F,WG,LW-1)
do j=0,LW-1; WG(:,:,j)=2.*HYG*WG(:,:,j); enddo; call FD(WG,W1F,LW)
                                                              R=R+W1F  
!*****************************************************(d2HdY+d2HdY2)Fz
do j=0,LW-1; WG(:,:,j)=D2HG*WIG(:,:,j); enddo; call FD(WG,W1F,LW-1)
                                                              R=R+W1F
!*******************************************************(HX^2+Hy^2)Fzz
do j=1,LW-1; WF(:,:,j)=                                              &
A1(j)*SF(:,:,j-1)+A2(j)*SF(:,:,j)+A3(j)*SF(:,:,j+1); enddo; 
WF(:,:,0)=WF(:,:,1)*Z1+WF(:,:,2)*Z2; 
WF1=WF(:,:,0); call FI(WF1,W2G,1)
WF=WF+W2L; call FI(WF,WG,LW-1);
do j=1,LW-1; WG(:,:,j)=HXY2*WG(:,:,j); enddo; call FD(WG,W1F,LW-1)
                                                              R=R-W1F
                                                           return; end 
 !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Adiabatic right side                                                       
                                   subroutine VERTICAL_VELOCITY(F,H)
                                                    include 'par.fi'
                                                    include 'com.fi'                                                      
real,dimension(-M:M,-MY:MY)::H,F,RFF,RHF,WF,WNF,WLF,VLF                                      
real,dimension(N,NY)::WLG,VLG,WLG1,XI,DETR,ER
real,dimension(N,NY)::HXYS,WXG,WYG
call DFDX2(H,HXF); call FI(HXF,HXG,1)
call DFDY2(H,HYF); call FI(HYF,HYG,1)
WF=-AK2*H; WF=WF*SHE; call FI(WF,D2HG,1)
WLF=AK*F; VLF=AK2*F; call FI(WLF,WLG,1); call FI(VLF,VLG,1)
HXYS=HXG**2+HYG**2; WLG1=D2HG*WLG-HXYS*VLG
DETR=1./(1.+HXYS-AD*D2HG); call FD(DETR,WF,1);
call FI(WF,DETR,1); DETMI=minval(DETR)
call FD(FZG,FZF,1); WNF=FZF-WLF
call FI(WNF,WNG,1); XI=WNG
do ITW=0,MAXIT
call DFDX2(FZF,WF); call FI(WF,WXG,1) 
call DFDY2(FZF,WF); call FI(WF,WYG,1)
WNG=AD*(2.*(HXG*WXG+HYG*WYG)+WLG1)
call FD(WNG,WF,1); call FI(WF,WNG,1)
WNG=WNG*DETR 
ER=XI-WNG; ERRITW=maxval(abs(ER))
    if (DETMI<0.) then
print*,'!!!!!!!!!!',' NST=',NST,' DETMI=',DETMI; stop; endif
    if (ITW==MAXIT) then 
print*,'!!!!!!!!!!',' NST=',NST,' ITW=',ITW; stop; endif
call FD(WNG,WNF,1); WNF=WNF*SHE; FZF=WNF+WLF
if (ERRITW<ERRLI) exit
XI=WNG
enddo !ITW    
                                                        return; end                                                             
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                                                       
                                   subroutine SECOND_DERIVATIVE(F,H)
                                                    include 'par.fi'
                                                    include 'com.fi'
real,dimension(-M:M,-MY:MY)::H,F,WF,WLF,VLF,WNF                                      
real,dimension(N,NY)::WG,WG1,WLG,VLG,WLG1
real,dimension(N,NY)::HXYS,WXG,WYG
WF=-AK2*H; call FI(WF,D2HG,1)
VLF=AK2*F; call FI(VLF,VLG,1)
HXYS=HXG**2+HYG**2; WLG1=D2HG*FZG-HXYS*VLG
call FD(WLG1,WF,1); call FI(WF,WLG1,1)
WG=1./(1.+HXYS); call FD(WG,WF,1); call FI(WF,WG,1) 
call DFDX2(FZF,WF); call FI(WF,WXG,1) 
call DFDY2(FZF,WF); call FI(WF,WYG,1)
WNG=2.*(HXG*WXG+HYG*WYG)+WLG1; call FD(WNG,WF,1); call FI(WF,WNG,1)
WNG=WNG*WG; call FD(WNG,WNF,1); call FI(WNF,WNG,1)
                                                       return; end                                                            
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                           subroutine WIND_INPUT(H)
                                                   include 'par.fi'
                                                   include 'com.fi'
real,dimension(-M:M,-MY:MY)::H  
real,dimension(N,NY)::WG                                                
real P0R,P0I
PHC=1./sqrt(real(kp)); WIND=UCP*PHC; CD=1.5e-3; SQCD=sqrt(CD)
WAVP=PI2/KP/2.; Z0=WAVP*exp(-XAP/SQCD); DET=WIND/alog(WAVP/Z0)
do k=1,M; X0=DET*alog(PI2/k/2./Z0)*sqrt(real(k))
do l=-MY,MY; XZ=X0*k/AK(k,l); call BETA(XZ,P0I,P0R)
PF( k, l)=RAW*(P0R*H( k, l)+P0I*H(-k,-l)); 
PF(-k,-l)=RAW*(P0R*H(-k,-l)-P0I*H( k, l))
enddo; enddo
                                                     return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Beta function
                                          subroutine BETA(X,BI,BR)
!Parameters:
X0=.58; X1=1.2; X2=-18.8; X3=21.2
A0=0.02277; A1=0.09476; A2=-.3718; A3=14.80
B0=-.02; B1=-148.0
!BI:
if (X.ge.X0) BI=B0+A0*(X-X0)+A1*(X-X0)**2 
if (X<X0)    BI=B0+A0*(X-X0)-A1*(X-X0)**2
!RE:
if (X<X2)                    BR=B1+A3*(X-X2)
if ((X.ge.X2).and.(X.le.X3)) BR=A2*(X-X1)**2
if (X>X3)                    BR=B1-A3*(X-X3)
                                                       return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                          subroutine BREAKING(F,H)
                                                   include 'par.fi'
                                                   include 'com.fi'
real,dimension(-M:M,-MY:MY)::F,H,WF,WF1
real,dimension(N,NY)::WG 
DXY=DX*DY; HBR=0.; FBR=0.; BK=0.                                       
WF=-AK2*H; call FI(WF,D2HG,1)
if (minval(D2HG)<-HXYCR.and.COOL>0.) then
do i=1,N; do j=1,NY; if (D2HG(i,j)<-HXYCR) then; NBR=NBR+1
BK(i,j)=DXy*COOL*D2HG(i,j)**2; endif; enddo; enddo
call FD(BK,WF,1); WF=WF*SHE; call FI(WF,BK,1); 
do i=1,N; do j=1,NY; if (BK(i,j)<0.) BK(i,j)=0.; enddo; enddo
WG=BK*HXG; call FD(WG,WF,1); call DFDX2(WF,WF1); HBR=WF1
WG=BK*HYG; call FD(WG,WF,1); call DFDY2(WF,WF1); HBR=HBR+WF1
WG=BK*FXG; call FD(WG,WF,1); call DFDX2(WF,WF1); FBR=WF1
WG=BK*FYG; call FD(WG,WF,1); call DFDY2(WF,WF1); FBR=FBR+WF1
                                     endif
                                                      return; end                                                         
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                               subroutine VERTICAL_GRID
                                               include 'par.fi'
                                               include 'com.fi'
!***************************************************************D,HW 
do k=1,M; do l=-MY,MY; SP2(k,l)=.5*(HF(k,l)**2+HF(-k,-l)**2); 
enddo; enddo 
SUMSP=sum(SP2)
SUMKSP=0.
do k=1,M; do l=-MY,MY; SUMKSP=SUMKSP+k*SP2(k,l); enddo; enddo 
WNK=SUMKSP/SUMSP;   D=2.*PI2/WNK; HW=D                                              
!**************************************************************Grids
DX=PI2/N;  do i=1,N; X(i)=DX*(i-1); enddo;  DX2=DX**2                            
DY=PI2/NY; do j=1,NY; Y(j)=DY*(j-1); enddo; DY2=DY**2
TOT=(GAM**real(LW)-1.)/(GAM-1.); DZ0=D/TOT
do j=1,LW; DZM(j)=DZ0*GAM**(j-1); enddo
ZI(0)=0.; do j=1,LW; ZI(j)=ZI(j-1)-DZM(j); enddo
do j=1,LW; ZM(j)=.5*(ZI(j-1)+ZI(j)); enddo
do j=1,LW-1; DZI(j)=ZM(j)-ZM(j+1); enddo
do j=0,LW; EAKZ(:,:,j)=exp(AK*ZI(j)); enddo
!do j=0,LW; EAKZ(:,:,j)=cosh(AK*(ZI(j)+HW))/cosh(AK*HW)
!******************************************Coefficients for progonka
do j=1,LW-1; DE=DZM(j+1)*DZM(j)**2+DZM(j)*DZM(j+1)**2
A1(j)= 2.*DZM(j+1)/DE; A3(j)= 2.*DZM(j)/DE; A2(j)=-A1(j)-A3(j) 
A4(j)= DZM(j+1)**2/DE; A6(j)=-DZM(j)**2/DE; A5(j)=-A4(j)-A6(j) 
enddo
! For interpolaton to 0:
Z1=-ZI(2)/DZM(2); Z2=ZI(1)/DZM(2)
! 
DI=ZI(1)*ZI(2)**2-ZI(2)*ZI(1)**2; Z3=ZI(2)**2/DI; Z4=-ZI(1)**2/DI 
print*,' LW=',LW,' GAM=',GAM,' DZ0/D=',DZ0/D
                                                        return
print(       '(11a12)'),' j','ZM','DZM','ZI','DZI',                 &
                       'A1','A2','A3','A4','A5','A6'  
                             !
write(11,fmt='(11a12)') ' j','ZM','DZM','ZI','DZI',                 &
                       'A1','A2','A3','A4','A5','A6'
do j=1,LW-1                                                         !
print('       (i12,4e12.6,6e12.6)'),j,ZM(j),DZM(j),ZI(j),DZI(j),    &
                            A1(j),A2(j),A3(j),A4(j),A5(j),A6(j)
write(11,fmt='(i12,4e12.6,6e12.6)') j,ZM(j),DZM(j),ZI(j),DZI(j),    &
                            A1(j),A2(j),A3(j),A4(j),A5(j),A6(j)
enddo
                                                         return; end!
!****************************************************************MIMA1
                                               subroutine MIMA(NAME,G)
                                                       include 'par.fi'
                                                       include 'com.fi'
character*5 NAME
real AVER,DISP
real G(N,NY)
DISP=0.; AVER=0.; GMIN=1.e10; GMAX=-1.e10; NU=1 
do i=1,N; do j=1,NY
if (G(i,j).lt.GMIN) then; IMI=i; JMI=j; GMIN=G(i,j); endif
if (G(i,j).gt.GMAX) then; IMA=i; JMA=j; GMAX=G(i,j); endif
DISP=(DISP*(NU-1)+G(i,j)**2)/NU; AVER=(AVER*(NU-1)+G(i,j))/NU
NU=NU+1; enddo; enddo
DISP=DISP-AVER**2; if (DISP.gt.0) DISP=sqrt(DISP)
print('(a6,x,a4,e15.7,2i6,a4,e15.7, 2i6,a3,e15.7,a3,e15.7)'), &
NAME,' MI=',GMIN,IMI,JMI,' MA=',GMAX,IMA,JMA,' A=',AVER,  &
' D=',DISP
write(11,fmt='(a6,x,a4,e15.7,2i6,a4,e15.7, 2i6,a3,e15.7,a3,e15.7)') &
NAME,' MI=',GMIN,IMI,JMI,' MA=',GMAX,IMA,JMA,' A=',AVER,  &
' D=',DISP
                                                         return; end 
!**************************************************************MIMA3
                                      subroutine MIMA3(NAME,G,L0,LB,LT)
                                                      include 'par.fi'
                                                      include 'com.fi'
character*5 NAME
real AVER,DISP
real G(N,NY,L0:LW)
print*,'EXTREMS IN ',NAME,'  NST=',NST 
write(11,*) 'EXTREMS IN ',NAME,'  NST=',NST 
do le=LB,LT; DISP=0.; AVER=0.; GMIN=1.e10; GMAX=-1.e10; NU=1 
do i=1,N; do j=1,NY
if (G(i,j,le).lt.GMIN) then; IMI=i; JMI=j; GMIN=G(i,j,le); endif
if (G(i,j,le).gt.GMAX) then; IMA=i; JMA=j; GMAX=G(i,j,le); endif
DISP=(DISP*(NU-1)+G(i,j,le)**2)/NU; AVER=(AVER*(NU-1)+G(i,j,le))/NU
NU=NU+1; enddo; enddo
DISP=DISP-AVER**2; if (DISP.gt.0) DISP=sqrt(DISP)
print(       '(i3,e14.7,a4,e14.7,2i4,a4,e14.7, 2i4,a3,e14.7,a3,e14.7)'),&
le,ZI(le),' MI=',GMIN,IMI,JMI,' MA=',GMAX,IMA,JMA,' A=',AVER,  &
' D=',DISP; 
write(11,fmt='(i3,e14.7,a4,e14.7,2i4,a4,e14.7, 2i4,a3,e14.7,a3,e14.7)') &
le,ZI(le),' MI=',GMIN,IMI,JMI,' MA=',GMAX,IMA,JMA,' A=',AVER,  &
' D=',DISP; 
enddo 
                                                  return; end !MIMA3
!**************************************************************MIMAF
                                             subroutine MIMAF(NAME,F)
                                                      include 'par.fi'
                                                      include 'com.fi'
character*5 NAME
real AVER,DISP
real F(-M:M,-MY:MY)
print*,'EXTREMS IN ',NAME,'  NST=',NST 
write(11,*)'EXTREMS IN ',NAME,'  NST=',NST 
DISP=0.; AVER=0.; FMIN=1.e10; FMAX=-1.e10; NU=1 
do k=-M,M; do l=-MY,MY
if (F(k,l).lt.FMIN) then; KMI=k; LMI=l; FMIN=F(k,l); endif
if (F(k,l).gt.FMAX) then; KMA=k; LMA=l; FMAX=F(k,l); endif
DISP=(DISP*(NU-1)+F(k,l)**2)/NU; AVER=(AVER*(NU-1)+F(k,l))/NU
NU=NU+1; enddo; enddo
DISP=DISP-AVER**2; if (DISP.gt.0) DISP=sqrt(DISP)
print       ('(a4,e15.7,2i4,a4,e15.7,2i4,a3,e15.7,a3,e15.7)'), &
' MI=',FMIN,KMI,LMI,' MA=',FMAX,KMA,LMA,' A=',AVER,  &
' D=',DISP; 
write(11,fmt='(a4,e15.7,2i4,a4,e15.7,2i4,a3,e15.7,a3,e15.7)') &
' MI=',FMIN,KMI,LMI,' MA=',FMAX,KMA,LMA,' A=',AVER,  &
' D=',DISP;  
                                                 return; end !MIMASP
!**************************************************************MIMAF
                                             subroutine MIMASP(NAME,F)
                                                      include 'par.fi'
                                                      include 'com.fi'
character*5 NAME
real AVER,DISP
real F(M,-MY:MY)
print*,'EXTREMS IN ',NAME,'  NST=',NST 
write(11,*)'EXTREMS IN ',NAME,'  NST=',NST 
DISP=0.; AVER=0.; FMIN=1.e10; FMAX=-1.e10; NU=1 
do k=1,M; do l=-MY,MY
if (F(k,l).lt.FMIN) then; KMI=k; LMI=l; FMIN=F(k,l); endif
if (F(k,l).gt.FMAX) then; KMA=k; LMA=l; FMAX=F(k,l); endif
DISP=(DISP*(NU-1)+F(k,l)**2)/NU; AVER=(AVER*(NU-1)+F(k,l))/NU
NU=NU+1; enddo; enddo
DISP=DISP-AVER**2; if (DISP.gt.0) DISP=sqrt(DISP)
print       ('(a4,e15.7,2i4,a4,e15.7,2i4,a3,e15.7,a3,e15.7)'), &
' MI=',FMIN,KMI,LMI,' MA=',FMAX,KMA,LMA,' A=',AVER,  &
' D=',DISP; 
write(11,fmt='(a4,e15.7,2i4,a4,e15.7,2i4,a3,e15.7,a3,e15.7)') &
' MI=',FMIN,KMI,LMI,' MA=',FMAX,KMA,LMA,' A=',AVER,  &
' D=',DISP;  
                                                 return; end !MIMASP														 	
!**************************************************************MIMA3
                                        subroutine MIMAF3(NAME,FU,L0,LB,LT)
                                                      include 'par.fi'
                                                      include 'com.fi'
character*5 NAME
real AVER,DISP
real FU(-M:M,-MY:MY,L0:LW)
print*,    'EXTREMS IN ',NAME,'  NST=',NST 
write(11,*)'EXTREMS IN ',NAME,'  NST=',NST 
do le=LB,LT; DISP=0.; AVER=0.; FMIN=1.e10; FMAX=-1.e10; NU=1 
do k=-M,M; do l=-MY,MY
if (FU(k,l,le).lt.FMIN) then; KMI=k; LMI=l; FMIN=FU(k,l,le); endif
if (FU(k,l,le).gt.FMAX) then; KMA=k; LMA=l; FMAX=FU(k,l,le); endif
DISP=(DISP*(NU-1)+FU(k,l,le)**2)/NU; AVER=(AVER*(NU-1)+FU(k,l,le))/NU
NU=NU+1; enddo; enddo
DISP=DISP-AVER**2; if (DISP.gt.0) DISP=sqrt(DISP)
print(       '(i3,f7.3,a4,e15.6,2i5,a4,e15.6,2i5,a3,e15.6,a3,e15.6)'), &
le,ZI(le),' MI=',FMIN,KMI,LMI,' MA=',FMAX,KMA,LMA,' A=',AVER,  &
' D=',DISP; 
write(11,fmt='(i3,f7.3,a4,e15.6,2i5,a4,e15.6,2i5,a3,e15.6,a3,e15.6)') &
le,ZI(le),' MI=',FMIN,KMI,LMI,' MA=',FMAX,KMA,LMA,' A=',AVER,  &
' D=',DISP; 
enddo 
                                                     return; end !MIMA 																										 
!***************************************************DERIVATIVE*OVER*X
                                            subroutine DFDX3(F,FDX,LL)
                                                       include 'par.fi'
real F(-M:M,-MY:MY,LL),FDX(-M:M,-MY:MY,LL)
do le=1,LL; do k=-M,M; do l=-MY,MY; FDX(k,l,le)=-k*F(-k,-l,le)
enddo; enddo; enddo !levels
                                                    return; end !DFDX
!***************************************************DERIVATIVE*OVER*Y
                                            subroutine DFDY3(F,FDY,LL)
                                                      include 'par.fi'
real F(-M:M,-MY:MY,LL),FDY(-M:M,-MY:MY,LL)
do le=1,LL; do k=-M,M; do l=-MY,MY; FDY(k,l,le)=-l*F(-k,-l,le)
enddo; enddo; enddo
                                                    return; end !DFDY
!***************************************************DERIVATIVE*OVER*X
                                            subroutine DFDX2(F,FDX)
                                                       include 'par.fi'
real F(-M:M,-MY:MY),FDX(-M:M,-MY:MY)
do k=-M,M; do l=-MY,MY; FDX(k,l)=-k*F(-k,-l)
enddo; enddo 
                                                    return; end !DFDX
!***************************************************DERIVATIVE*OVER*Y
                                              subroutine DFDY2(F,FDY)
                                                      include 'par.fi'
real F(-M:M,-MY:MY),FDY(-M:M,-MY:MY)
do k=-M,M; do l=-MY,MY; FDY(k,l)=-l*F(-k,-l)
enddo; enddo
                                                    return; end !DFDY
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Direct
                                                  subroutine FD(G,F,LL)
                                                      include 'par.fi'
                                                      include 'com.fi'
real, dimension(N,NY,LL)::G 
real, dimension(-M:M,-MY:MY,LL)::F
real, dimension(-M:M,-MY:MY)::F0
real, dimension(N,NY)::G0
SCALE=2./N/NY
do le=1,LL; G0=G(:,:,le)
call rdft2d(N,N,NY,1,G0,IP,WK); G0=G0*SCALE
do k=1,M; F0(k,0)=G0(2*k+1,1); F0(-k,0)=-G0(2*(k+1),1); enddo       !
do k=-M,0; do l=-MY,0;  F0(k,l)=-G0(-2*k+2,-l+1); enddo; enddo      !
do k=0,M; do l=0,MY; F0(k,l)=G0(2*k+1,l+1); enddo; enddo            !
F0(0,0)=.5*G0(1,1)
do k=1,M; do l=-MY,-1; F0(k,l)=G0(2*k+1,l+NY+1);   enddo; enddo
do k=-M,-1; do l=1,MY; F0(k,l)=-G0(-2*k+2,-l+NY+1); enddo; enddo
F(:,:,le)=F0
enddo !le                 
                                                            return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Inverse
                                                  subroutine FI(F,G,LL)
                                                      include 'par.fi'
                                                      include 'com.fi'
real, dimension(N,NY,LL)::G 
real, dimension(-M:M,-MY:MY,LL)::F
real, dimension(-M:M,-MY:MY)::F0
real, dimension(N,NY)::G0 
do le=1,LL; G0=0.; F0=F(:,:,le)
do k=1,M; G0(2*k+1,1)=F0(k,0); G0(2*(k+1),1)=-F0(-k,0); enddo
do k=-M,0; do l=-MY,0; G0(-2*k+2,-l+1)=-F0(k,l); enddo; enddo
do k=0,M; do l=0,MY; G0(2*k+1,l+1)=F0(k,l); enddo; enddo 
G0(1,1)=2.*F0(0,0); !G0(2,1)=0.
do k=1,M; do l=-MY,-1; G0(2*k+1,l+NY+1)=F0(k,l);   enddo; enddo
do k=-M,-1; do l=1,MY; G0(-2*k+2,-l+NY+1)=-F0(k,l); enddo; enddo
call rdft2d(N,N,NY,-1,G0,IP,WK)
G(:,:,le)=G0
enddo !le
                                                           return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                                   subroutine PM_JONSWAP
                                                   include 'par.fi'
                                                   include 'com.fi'
real PHAK
real,dimension(M)::SP,BET
real,parameter::EXPM=1.386,HSLP=0.0244,BJ=1.25,GAMMA=3.3,SI1=.07,       &
                SI2=.09,APM=8.14e-3,AJ=.00988,OMIN=.855                 !
SP=0.; ALPHA=AJ*OMIN**.66/1.844                                         !
do k=1,NW; RKN=real(k)/real(kp);                                        !
if (RKN<1.) then; SI=SI1; else; SI=SI2; endif 
                          !
SP(k)=ALPHA/RKN**3*exp(-BJ/ RKN**2)*                                    &
GAMMA**(exp(-.5*((sqrt(RKN) -1.)/SI)**2));enddo                         !
S=0.; do k=1,NW; S=S+SP(k); enddo; HS=4.*sqrt(S) ;                      !
HSN=HS*kp/PI2; FACT=(HSLP/HSN)**2                                       ! 
SP=0.; ALPHA=AJ*OMPN**.66/(1.+.844*exp(-.7*(OMPN-OMIN)))*FACT           !
do k=1,NW; RKN=real(k)/real(kp);                                        !
if (RKN<1.) then; SI=SI1; else; SI=SI2; endif                           !
SP(k)=ALPHA/RKN**3*exp(-BJ/ RKN**2)*GAMMA**                             &
(exp(-.5*((sqrt(RKN) -1.)/SI)**2));enddo                                !
SP=SP*FACTOR                                                            !
S=0.; do k=1,NW; S=S+SP(k); enddo; HS=4.*sqrt(S) ; HSN=HS*kp/PI2        !
print('(6(a7,e12.4),a7,2i5)'),'    HS=',HS,'   HSN=',HSN,'  OMPN=',OMPN,&
' ALPHA=',ALPHA,'  FACT=',FACT,' MAXSP=',maxval(SP),' LOCSP=',          &
maxloc(SP,dim=1)    !
write(11,fmt='(6(a7,e12.4),a7,2i5)')                                    &
'    HS=',HS,'   HSN=',HSN,'  OMPN=',OMPN,' ALPHA=',ALPHA,'  FACT=',    &
        FACT,' MAXSP=',maxval(SP),' LOCSP=',maxloc(SP,dim=1) 
	 do k=1,M; if (SP(k)<1.e-20) SP(k)=1.e-20; enddo
SP2=0.; BET=0.; do k=1,NW; RKN=real(k)/real(kp)                         !
if (RKN<.902) BET(k)=2.61*RKN**.65                                      !
if (RKN>.902) BET(k)=2.28*RKN**(-.65)                                   !
enddo                                                                   !
do k=1,NW; SS=0.; do l=-MY,MY;                                          !
ARG=BET(k)*ATAN2(real(l),real(k)); SECH=2./(exp(ARG)+exp(-ARG))     !
SS=SS+ .5*BET(k)*SECH**JP; enddo                                        ! 
                 do l=-MY,MY;                                           !
ARG=BET(k)*ATAN2(real(l),real(k)); SECH=2./(exp(ARG)+exp(-ARG))     !
SP2(k,l)=.5*BET(k)*SECH**JP*SP(k)/SS; AMPL(k,l)=sqrt(2.*SP2(k,l));      !
if (SP2(k,l)<1.e-20) SP2(k,l)=1.e-20                                    !
enddo; enddo                                                            !
                                                      call random_seed  !
write(11,*) ' BET:',minval(BET),maxval(BET)                             !
write(11,*) ' SP:',minval(SP),maxval(SP)                                !
write(11,*) ' AMPL:',minval(AMPL),maxval(AMPL)                          !
do k=1,NW; do l=-MY,MY                                                  !
call random_number(PHAK); PHAK=PI2*PHAK                                 !
HF(k,l)=AMPL(k,l)*cos(PHAK); HF(-k,-l)=AMPL(k,l)*sin(PHAK);             !
enddo; enddo                                                            !
do k=1,NW; do l=-MY,MY; if (AK(k,l)<real(NW)) then                      !
FF( k, l)= HF(-k,-l)/OM(k,l); FF(-k,-l)=-HF( k, l)/OM(k,l)              !
endif; enddo; enddo													    !
                                                             return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                   subroutine ABSMAXF(NAME,F,LB,LT,ABSM,KM,LM,JM,FT)
                                                 include 'par.fi'
                                                 include 'com.fi'
character*5 NAME
real,dimension(-M:M,-MY:MY,LB:LT)::F
logical FT
ABSM=0.; do k=-M,M; do l=-MY,MY; do j=LB,LT; 
if (abs(F(k,l,j))>ABSM) then; ABSM=abs(F(k,l,j)); KM=k; LM=l; JM=j;
endif; enddo; enddo; enddo;
if (FT) then 
print(       '(2a6,e15.7,3(a4,i4))'),                              &
NAME,' ABSM=',ABSM,' KM=',KM,' LM=',LM,' JM=',JM
write(11,fmt='(2a5,e15.7,3(a4,i4))')                               &
NAME,'ABSM=',ABSM,' KM=',KM,' LM=',LM,' JM=',JM; endif
	                                           return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
                                                function JUST(DTIME)
                                                include 'par.fi'
                                                include 'com.fi'
if (DTIME==0.) then; JUST=0; return; endif
if (amod(T+DT/4.,DTIME)<DT/2.) then; JUST=1; else; JUST=0
endif;                                              return; end 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                                    function VREMYA()
                                                     include 'par.fi'
integer D_T(8)
call date_and_time(values=D_T)
VREMYA=3600.*D_T(5)+60.*D_T(6)+D_T(7)+.001*D_T(8)
                                                   return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                                  subroutine OUTPUT
											      include 'par.fi'
                                                  include 'com.fi'
real,dimension(N,NY)::WG,W0
real,dimension(-M:M,-MY:MY)::WF,WF0,U0F,V0F
call ENERGY(HF,FF,EKF); ET=sum(EKF)
HM=sum(HG)/N/NY; EP=sum((HG-HM)**2)/N/NY/2.
call FI(HF,HG,1)
CONS=1.; if (ICONS>0) then; CONS=1./sqrt(ENCR); FF=FF*CONS; HF=HF*CONS; endif
SUMKSP=0.
do k=1,M; do L=-MY,MY; SP2(k,L)=.5*(HF(k,L)**2+HF(-k,-L)**2); enddo; enddo
SUMSP=sum(SP2); HS=4.*sqrt(SUMSP)
do k=1,M; do l=-MY,MY; SUMKSP=SUMKSP+k*SP2(k,l); enddo; enddo 
WNK=SUMKSP/SUMSP
call FI(FF,FG,1)
call MIMA('  HG ',HG);   call MIMA('  FG ',FG)
WG=HG/HS; call MIMA('  HN ',WG)
call MIMA(' HXG ',HXG);  call MIMA(' HYG ',HYG)
call MIMA(' FZG ',FZG); call MIMA(' WNG ',WNG)
RNN=sum(RNNLI); RNT=sum(RNTAI); RIN=sum(RINPU);
RBR=sum(RBREA); RTO=sum(RTOTA)
EKN=0.; SRN=0.; EKL=0.; REST=ET*(1./COR**2-1.)/DT
CONSI=(EP+EK)*(1./CONS**2-1.)/DT_OUTP; 
call KINETIC_ENERGY(EKL,EKN); EK=EKL+EKN
print('       (a3,e13.5,4(a7,i7),a8,2i4,/,6(a6,e13.5),/,6(a6,e13.5),/,6(a6,e13.5))'),&   
' T=',T,'NST=',NST,'   NBR=',NBR,'   ITL=',ITL,' NPHYS=',NPHYS,' IJSP=',IJS(1),IJS(2),  &
' ENCR=',ENCR,' EP=',EP,' EK=',EK,'  HS=', HS,'  EKN=',EKN,' CORE=',1.-COR,&
'   WNK=',WNK,'  RNN=',RNN,'  RNT=',RNT,'  RBR=',RBR,'  RIN=',RIN,'  ET=',ET,     &
'   RTO=',RTO, ' CONSI=',CONSI,'  REST=',REST,'  DCR=',DCR,' DEPK=',DEPK,' ECOR=',ECOR
write(11,fmt='(a3,e13.5,4(a7,i7),a8,2i4,/,6(a6,e13.5),/,6(a6,e13.5),/,6(a6,e13.5))') &
' T=',T,'NST=',NST,'   NBR=',NBR,'   ITL=',ITL,' NPHYS=',NPHYS, ' IJSP=',IJS(1),IJS(2),  &
' ENCR=',ENCR,' EP=',EP,' EK=',EK,'  HS=', HS,'  EKN==',EKN,' CORE=',1.-COR,&
'   WNK=',WNK,'  RNN=',RNN,'  RNT=',RNT,'  RBR=',RBR,'  RIN=',RIN,'  ETO=',ETO,     &
'   RTO=',RTO, ' CONSI=',CONSI,'  REST=',REST,'  DCR=',DCR,' DEPK=',DEPK,' ECOR=',ECOR
print('(/)'); write(11,fmt='(/)')
RINT(0)=real(NST); RINT(1)=real(NBR); RINT(2)=real(ITL); RINT(3)=T; 
RINT(4)=ENCR; RINT(5)=EP; RINT(6)=EK; RINT(7)=HS; RINT(8)=EKN; RINT(9)=1.-COR; 
RINT(10)=WNK; RINT(11)=RNN; RINT(12)=RNT; RINT(13)=RBR; 
RINT(14)=RIN; RINT(15)=CONSI; RINT(16)=REST; RINT(17)=RATE; RINT(18)=COOL; RINT(19)=0.
write(13,fmt='(10e13.5)') RINT;
                                                       return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                                  subroutine SURFACE
                                                      include 'par.fi'
                                                      include 'com.fi'
real,dimension(N,NY)::WG
real,dimension(-M:M,-MY:MY)::WF
call FI(HF,HG,1)
write(12,fmt='(f10.1)') T
write(12,fmt='(10e12.4)') HG
write(12,fmt='(10e12.4)') W1G
write(12,fmt='(10e12.4)') W2G
WF=AK*FF; call FI(WF,WG,1)
write(12,fmt='(10e12.4)') WG
write(12,fmt='(10e12.4)') WNG
                                                    return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                              subroutine BREAK_WRITE
                                                     include 'par.fi'
                                                     include 'com.fi'
real,dimension(N,NY)::WG
real,dimension(-M:M,-MY:MY)::WF
WF=HBRX+HBRY
call FI(WF,WG,1)
write(29,fmt='(f10.1)') T
write(29,fmt='(10e12.4)') WG
                                                    return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                          subroutine SPECTRUM(FS,SS)
                                                      include 'par.fi'
                                                      include 'com.fi'
real,dimension(-M:M,-MY:MY)::FS
real,dimension(M,-MY:MY)::SS
do k=1,M; do l=-MY,MY; SS(k,l)=.5*(FS(k,l)**2+FS(-k,-l)**2); 
enddo; enddo
                                                    return; end 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                          subroutine SHEININ_FILTER
                                                    include 'par.fi'
                                                    include 'com.fi'
DX=PI2/N;  do i=1,N; X(i)=DX*(i-1); enddo;  DX2=DX**2                            
DY=PI2/NY; do j=1,NY; Y(j)=DY*(j-1); enddo; DY2=DY**2
do k=-M,M; do l=-MY,MY; AK(k,l)=sqrt(real(k**2+l**2)); 
enddo; enddo; AK2=AK**2; OM=sqrt(AK); AK(0,0)=1.
RM=real(M); RMY=real(MY)
do k=-M,M; do L=-MY,MY
RK=real(k); RL=real(L) 
SHEI(k,L)=((RK/RM)**2+(RL/RMY)**2)/DM**2
if (SHEI(k,L).le.1.) SHEI(k,L)=0.
if (SHEI(k,L).gt.1.) SHEI(k,L)=CM*(DM**2*(SHEI(k,L)-1.)/(1.-DM**2))**2
enddo; enddo
SHE=1.-SHEI; do k=-M,M; do l=-MY,MY; 
if (SHE(k,l)<0.) SHE(k,l)=0; enddo; enddo 
                                                        return; end                                                     
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                                                         
                                            subroutine ENERGY(H,F,EF)
                                                     include 'par.fi'
                                                     include 'com.fi'
real,dimension(M,-MY:MY)::EF
real,dimension(-M:M,-MY:MY)::H,F                                                    
do k=1,M; do l=-MY,MY; EF(k,l)=.25*((H(k,l)**2+H(-k,-l)**2)+   & 
AK(k,l)*(F(k,l)**2+F(-k,-l)**2)); enddo; enddo
                                                         return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
                                               subroutine DISP_REL(LF)
                                                    include 'par.fi'
                                                    include 'com.fi'
real,dimension(M,-MY:MY)::SRHH,SH2,SH4,SOM2,OME,DIS,OMT,SPM
save SRHH,SH2,SH4,SOM2,SPM
if (LF==0) then
do k=1,M; do l=-MY,MY; SP2(k,l)=.5*(HF(k,l)**2+HF(-k,-l)**2);enddo; enddo
do k=1,M; do l=-MY,MY; 
SRHH(k,l)=(SRHH(k,l)*NNS+RH(k,l)*HF(-k,-l))/(NNS+1)
SOM2(k,l)=(SOM2(k,l)*NNS+(RH(k,l)*HF(-k,-l))**2)/(NNS+1)
SH2(k,l)=(SH2(k,l)*NNS+HF(-k,-l)**2)/(NNS+1)
SH4(k,l)=(SH4(k,l)*NNS+HF(-k,-l)**4)/(NNS+1)
SPM(k,l)=(SPM(k,l)*NNS+SP2(k,l))/(NNS+1)
enddo; enddo
NNS=NNS+1
endif
if (LF>0) then
do k=1,M; do l=-MY,MY
if (SH2(k,l)>0.) then
OME(k,l)=SRHH(k,l)/SH2(k,l)
DIS(k,l)=SOM2(k,l)/SH4(k,l)
endif; enddo; enddo
DIS=(DIS-OME**2)
do k=1,M; do l=-MY,MY; if (DIS(k,l)>0.) then
DIS(k,l)=sqrt(DIS(k,l)); 
else; DIS(k,l)=0.; endif; enddo; enddo
do k=1,M; do l=-MY,MY; OMT(k,l)=sqrt(AK(k,l)); enddo; enddo
write(23,fmt='(10e12.4)') OMT,OME,DIS,SPM
endif
                                                         return; end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                                subroutine VELOCITY
                                                    include 'par.fi'
                                                    include 'com.fi'
integer,parameter::MN=4
real,dimension(N,NY,0:LW)::WG
real,dimension(M,MY,0:LW)::WS
real,dimension(-M:M,-MY:MY,0:LW)::WF
real,dimension(0:M,-MY:MY,0:LW)::SPK
!********************************************Horizontal coordinates:
do i=1,N; X(i)=DX*(i-1); enddo
do j=1,NY; Y(j)=DY*(j-1); enddo
!************************************************Vertical coordinates
do le=0,LW; WG(:,:,le)=ZI(le)+HG; enddo
do i=1,M; do j=1,MY; WS(i,j,:)=WG(MN*(i-1)+1,MN*(j-1)+1,:)
enddo; enddo
                                     write(18,fmt='(e12.4)') X,Y,WS
!*********************************************U velocity components
!************************************Calculation the full potential
do j=0,LW; SF(:,:,j)=SF(:,:,j)+FF*EAKZ(:,:,j); enddo
SPK=0.
call FI(SF,WG,LW+1)
call DFDX3(SF,WF,LW+1); call FI(WF,WG,LW+1)
do k=0,M; do l=-MY,MY; 
SPK(k,l,:)=.5*(WF(k,l,:)**2+WF(-k,-l,:)**2); enddo; enddo 
do le=0,LW; WG(:,:,le)=WG(:,:,le)-HXG*WIG(:,:,le); enddo
do i=1,M; do j=1,MY; WS(i,j,:)=WG(MN*(i-1)+1,MN*(j-1)+1,:)
enddo; enddo
                                     write(19,fmt='(e12.4)') WS
call MIMA3('  U  ',WG,0,0,LW)
!*********************************************V velocity components
call DFDY3(SF,WF,LW+1); call FI(WF,WG,LW+1)
do k=0,M; do l=-MY,MY; 
SPK(k,l,:)=SPK(k,l,:)+.5*(WF(k,l,:)**2+WF(-k,-l,:)**2); enddo; enddo 
do le=0,LW; WG(:,:,le)=WG(:,:,le)-HYG*WIG(:,:,le); enddo
do i=1,M; do j=1,MY; WS(i,j,:)=WG(MN*(i-1)+1,MN*(j-1)+1,:)
enddo; enddo
                                     write(20,fmt='(e12.4)') WS
call MIMA3('  V  ',WG,0,0,LW)
!*********************************************W velocity components
do i=1,M; do j=1,MY; WS(i,j,:)=WIG(MN*(i-1)+1,MN*(j-1)+1,:)
enddo; enddo
                                     write(21,fmt='(e12.4)') WS
call FD(WIG,WF,LW+1)
do k=0,M; do l=-MY,MY; 
SPK(k,l,:)=SPK(k,l,:)+.5*(WF(k,l,:)**2+WF(-k,-l,:)**2); enddo; enddo 
	                               write(22,fmt='(e12.4)') SPK
close(18)
call MIMA3('  W  ',WIG,0,0,LW)
!*****************************Restoration the nonlinear constituent.
do j=0,LW; SF(:,:,j)=SF(:,:,j)-FF*EAKZ(:,:,j); enddo

                                                       return; end  
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                                                         
                                  subroutine KINETIC_ENERGY(EKL,EKN)
                                                     include 'par.fi'
                                                     include 'com.fi'
real,dimension(0:LW)::EKI
real,dimension(-M:M,-MY:MY)::WF,WF0,WF1
real,dimension(N,NY)::WG,WG0,WG1
EKI=0.; EKIN=0.
do j=1,LW-1
WF=AK*FF*EAKZ(:,:,j); call FI(WF,WG,1) 
WF0=A4(j)*SF(:,:,j-1)+A5(j)*SF(:,:,j)+A6(j)*SF(:,:,j+1)           !W
call FI(WF0,WG0,1)                                                !w
EKI(j)=sum(2.*WG*WG0+WG0**2)/N/NY
!
WF=SF(:,:,j)
WF0=FF*EAKZ(:,:,j); call DFDX2(WF0,WF1); call FI(WF1,WG,1)        !U
call DFDX2(WF,WF1); call FI(WF1,WG1,1); 
WG1=WG1-HXG*WG0;                                                  !u
EKI(j)=EKI(j)+sum(2.*WG*WG1+WG1**2)/N/NY
!
                    call DFDY2(WF0,WF1); call FI(WF1,WG,1)        !V
call DFDY2(WF,WF1); call FI(WF1,WG1,1)
WG1=WG1-HYG*WG0;                                                  !v 
EKI(j)=EKI(j)+sum(2.*WG+WG1+WG1**2)/N/NY
enddo !j
EKI(LW)=EKI(LW-1)
do j=1,LW; EKIN=EKIN+(EKI(j-1)+EKI(j))*DZM(j); enddo
EKN=.5*EKIN
do k=1,M; do l=-MY,MY; EKF(k,l)=AK(k,l)*(FF(k,l)**2+FF(-k,-l)**2)
enddo; enddo; EKL=.25*sum(EKF); 
                                                         return; end 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Adiabatic right side                                                       
                                   subroutine VERTICAL_VELOCITY0(F,H)
                                                    include 'par.fi'
                                                    include 'com.fi'
real,dimension(-M:M,-MY:MY)::H,F,RFF,RHF,WF,WLF,VLF,WNF                                      
real,dimension(N,NY)::WLG,VLG,WLG1,XI,DETR,ER
real,dimension(N,NY)::HXYS,WXG,WYG
call DFDX2(H,HXF); call FI(HXF,HXG,1)
call DFDY2(H,HYF); call FI(HYF,HYG,1)
WF=-AK2*H; WF=WF*SHE; call FI(WF,D2HG,1)
WF=-AK2*H; call FI(WF,D2HG,1)
WLF=AK*F; VLF=AK2*F; call FI(WLF,WLG,1); call FI(VLF,VLG,1)
HXYS=HXG**2+HYG**2; WLG1=D2HG*WLG-HXYS*VLG 
DETR=1.+HXYS-AD*D2HG; DETMI=minval(DETR)
call FD(FZG,FZF,1); WNF=FZF-WLF
call FI(WNF,WNG,1); XI=WNG
do ITW=0,MAXIT
call DFDX2(FZF,WF); call FI(WF,WXG,1) 
call DFDY2(FZF,WF); call FI(WF,WYG,1)
WNG=AD*(2.*(HXG*WXG+HYG*WYG)+WLG1)/DETR 
ER=XI-WNG; ERRITW=maxval(abs(ER))
    if (DETMI<0.) then
print*,'!!!!!!!!!!',' NST=',NST,' DETMI=',DETMI; stop; endif
    if (ITW==MAXIT) then 
print*,'!!!!!!!!!!',' NST=',NST,' ITW=',ITW; stop; endif
print*,' NST=',NST,' ITW=',ITW,' ERRITW=',ERRITW
call FD(WNG,WNF,1); WNF=WNF*SHE; FZF=WNF+WLF
if (ERRITW<ERRLI) exit
XI=WNG
enddo !ITW
                                                       return; end 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                                                subroutine PRINTPAR
                                                include 'par.fi'
print('(3(a6,i6))'),                                              &
'     M=', M   ,'    MY=',MY,   '  NBOT=',NBOT,                   &
'  NSTF=', NSTF,'    KP=',KP,   '   JOM=',JPM,                    &
'    NW=',   NW,'    JP=',JP,   ' JSTOX=',JSTOX,                  &
' MAXIT=',MAXIT,' ITAIL=',ITAIL,'IBREAK=',IBREAK,                 &
' INPUT=',INPUT,'    IT=',0,'   LW=',LW
print('(3(a7,f7.4))'),                                            &
'    DT=',DT     ,' DTFIE=',DT_FIELDS,' DTSUR=',DT_SURF,          &
'DT_OUT=',DT_OUTP,'DT_DIS=',DT_DISP,  'DT_VEL=',DT_VELO,          &  
'DERRLI=',ERRLI, '    AD=',0.,       '    DM=',DM,                &
'    CM=',CM,    ' HXYCR=',HXYCR,    '  COOL=',COOL,              &
'   UCP=',UCP,'   GAM=',GAM
write(11,fmt='(3(a6,i6))')                                        &
'     M=', M   ,'    MY=',MY,   '  NBOT=',NBOT,                   &
'  NSTF=', NSTF,'    KP=',KP,   '   JOM=',JPM,                    &
'    NW=',   NW,'    JP=',JP,   ' JSTOX=',JSTOX,                  &
' MAXIT=',MAXIT,' ITAIL=',ITAIL,'IBREAK=',IBREAK,                 &
' INPUT=',INPUT,'    IT=',0,'   LW=',LW 
write(11,fmt='(3(a7,f7.4))')                                      &
'    DT=',DT     ,' DTFIE=',DT_FIELDS,' DTSUR=',DT_SURF,          &
'DT_OUT=',DT_OUTP,'DT_DIS=',DT_DISP,  'DT_VEL=',DT_VELO,          &  
'DERRLI=',ERRLI, '    AD=',0.,       '    DM=',DM,                &
'    CM=',CM,    ' HXYCR=',HXYCR,    '  COOL=',COOL,              &
'   UCP=',UCP,'   GAM=',GAM
                                                       return; end											                                                          
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                                                         
!COBCEM BCE                                                                                                                                                                                                      
