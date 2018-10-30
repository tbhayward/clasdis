c
        PROGRAM CLASDIS
c
        IMPLICIT NONE
c
C******************************************************************
C
C       Main program for PEPSI simulations for CLAS kinematics
C
C***************************************************************** 
#include "names.inc"
#include "claslepto.inc"
c disinC.. {\bf LFN} Number of the status files for random numbers 
      INTEGER       LFN,I,NEVENT
c
      call CL_OPTIONS          ! command line processing

      print *,'now call INIT_PAW'
c
#ifdef  hbook
      call INIT_PAW            ! command line processing
c
#endif
c     
C..   number of events   
c
      print *,'after call INIT_PAW'

       NEVENT=cl_triggers

C.. Constats and Parameter

C.. Number of the  RNDMSTATUS FILE
      LFN = 10
C.. Read in of the random number status file 
      CALL RNDMSTART(LFN)

C.. Initialization of the lepto program 
      CALL DISINIT
      if(cl_verblev.gt.0) WRITE(*,*) 'NEVENT=',NEVENT

C.. Event generation: 
       DO I = 1,NEVENT
       
C.. Output of the number of generated events 
        IF (MOD(I,cl_nprint).EQ.0.and.cl_verblev.gt.0)
     6 WRITE(*,*) I,'  from ',NEVENT,PARL(23),PARL(24)
         
C.. Generation of an event in {\sc Lepto-6.5}
 10   CONTINUE
      if (cl_single) then ! do only single track events
      call single_event()
c
      else                ! regular lepto      
       if(LST(37).eq.0) then
          CALL LEPTO
       else
          CALL LEPTOAK
       endif 
c
        if(cl_verblev.gt.100.and.cl_verblev.lt.104) 
     $         call lulist(cl_verblev-100)
C.. Message in case of an error in the event generation 
        IF ((LST(21).NE.0)) THEN
          WRITE(*,*) 'event-error at',I,LST(21)  
          GOTO 10
        ENDIF
      endif               ! do single or LUND
c
       CALL CLASFILL(I)
c
       ENDDO
c
#ifdef  hbook
       CALL CLOSEPAW()
#endif
       if(datfileOK) CLOSE(41) ! close the data file
c
       WRITE(*,*) 'Total ',NEVENT,' in pb ',PARL(23),PARL(24)
C.. Saving the last random number configuration 
 999  if(cl_rand.eq.1) CALL RNDMSAVE(LFN)
           if(cl_Printid.gt.0) close(41)
       END      
c
#ifdef  hbook      
       subroutine closepaw()
      implicit none
       INTEGER ICYCLE
         call HCDIR('//ZXC',' ')
         call HROUT(0,icycle,'')
         call HREND('ZXC')
         close(50)
         return
       end
#endif
c     
      subroutine get_datfilename()
      implicit none
#include "names.inc"
#include "claslepto.inc"
c      character*4  cpol,cdis
      character*4  cpol
      character*5  cdis
      character*10  cxsec
      character*21  ccuts
c
c
      integer jc,m,ncdis
ccc
      print *,'make the data file name',cl_beam_energy
      print *,'************************'
c
      if(cl_beam_energy.lt.10.0) then
      write(ccuts,'(A,F5.3,A,F3.2,A,F3.2)') 'e',cl_beam_energy,
     6'.emin',cl_emin,'tmin',cl_tmin 
      else
      write(ccuts,'(A,F6.3,A,F4.2,A,F3.2)') 'e',cl_beam_energy,
     6'.emn',cl_emin,'tmn',cl_tmin 
      endif
c
      print *,'get the x-section for ',PARL(23)
c
      if(cl_beam_energy.lt.15.0) then
      write(cxsec,'(A,F5.2,A)') '.xs',PARL(23)/1000.0,'nb'
      else
      write(cxsec,'(A,F5.1,A)') '.xs',PARL(23)/1000.0,'nb'
      endif
      print *,'**',ccuts,'++',cxsec
c
      if(cl_pol.gt.0.0) then
       cpol='.p1.'
      else if (cl_pol.lt.0.0) then
       cpol='.m1.'
      else
       cpol='.00.'
      endif
c
      if (cl_pid.eq.0) then 
       cdis='.dis'
       ncdis=4
      else
         if (cl_pid.lt.1000) then
            write (cdis,'(A,I3,A)') '.',cl_pid
            ncdis=4
         else
            write (cdis,'(A,I4,A)') '.',cl_pid
            ncdis=5
         endif
c       write (cdis,'(A,F3.1,I4)') '.',cl_zmin,cl_pid
      endif

      jc = 0
      do m=1,len(cl_path)
         if ((cl_path(m:m).eq.' ').and.(jc.eq.0)) then
            jc=m-1
         endif
      enddo
      if (jc.eq.0) jc=len(cl_path)


      print *,'PARL23,24',PARL(23),PARL(24)
      write(datfilename,'(A)') cl_path(1:jc)//'clasdis'//
     6cl_target(1:2)//cpol//ccuts//cxsec//cdis(1:ncdis)//'.'
      print *,'filename ',datfilename
      end
c
#ifdef  hbook
c
      SUBROUTINE INIT_PAW
       implicit none
       integer h,nwpawc
       parameter (nwpawc=5000000)
       integer  NTID_OUT

       common /PAWC/ h(nwpawc)
       print *,' call HLIMIT(nwpawc)',nwpawc
       call HLIMIT(5000000)
       NTID_OUT=22
       print *,' call hini(NTID_OUT)', NTID_OUT
       call hini(NTID_OUT)
       return
       end
 
      subroutine hini(NTID_OUT)
      implicit none
#include "ntupgkoko.inc"
#include "names.inc"
c
      integer NTID_OUT,IQUEST
      COMMON/QUEST/IQUEST(100) 
      integer lrec,istat
      lrec=4096
c     open koko
c
       IQUEST(10)=65000
       CALL HROPEN(50,'ZXC','clasDIS.hbook','NQE',LREC,ISTAT)
       call hbset('BSIZE',8176,istat)
      if (istat.ne.0) stop
      call HBNT(NTID_OUT,'semiout',' ')
      call HBNAME(NTID_OUT,'elegen',gnele,
     +'gnele:I,gelebits:I,ghpar:I,gproh:I,geleh:I,'//
     +'gelee:R,gelet:R,'//
     +'gelef:R,gelex:R,geley:R,'//
     +'gelez:R,gelewgt:R,gxb:R,gq2:R,gw:R')
c234567890123456789012345678901234567890123456789012345678901234567890123456
      call HBNAME(NTID_OUT,'hadgen',gnpi0,
     +'gnpi0:I,gnpip:I,gnpim:I,gnpro:I,gngam:I,'//
     +'gnneu:I,gpi0e:R,gpi0t:R,gpi0f:R,gpipe:R,'//
     +'gpipt:R,gpipf:R,gpime:R,'//
     +'gpimt:R,gpimf:R,gproe:R,gprot:R,gprof:R,ggame:R,'//
     +'ggamt:R,ggamf:R,'//
     +'ggam2t:R,ggam2e:R,ggam2f:R,gneue:R,gneut:R,gneuf:R,gpipx:R,'//
     +'gpipy:R,gpipz:R,gpimx:R,gpimy:R,gpimz:R,gprox:R,gproy:R,gproz:R')
      call HBNAME(NTID_OUT,'strgen',gnkap,
     +'gnkap:I,gnkam:I,nskap:I,nskam:I,'//
     +'gkape:R,gkapt:R,gkapf:R,gkame:R,gkamt:R,gkamf:R')
c
c
      if (cl_radgen) then    
       call HBNAME(NTID_OUT,'radgen',iproctype,
     +'iproctype:I,radq2:R,radnu:R,radweight:R,'//
     +'grade:R,gradt:R,gradf:R')
      endif
      end
#endif
c     
       SUBROUTINE CL_OPTIONS
       implicit none
#include "names.inc"
       integer i
       character*20 cnumber
       INTEGER  numopts
       real valnum
c
c    command line processing
c
c     defaults
c
      cl_lst15=145           ! default pDFgrsv2000 hep-ph/0011215 LO valence scenario
c                            ! 147 grsv2000 hep-ph/0011215 NLO,  173 -CTEQ6L (from Koko)
      cl_lst37=0             ! default LEPTO (no Cahn) =1 AK's Cahn cos 2-Cahn cs+cos2 3-Sivers
      cl_lst8=1              ! default LEPTO 12, 12-parton showers with PS 1- 1st order QCD 
      cl_her=.FALSE.         ! default Jetset, .true. hermes settings.
      cl_parl3=0.64          ! default PARL(3) <k_T> default in LEPTO =0.44

      cl_parj2=0.3          ! default PARJ(2) 0.3, HERMES 0.15, suppress ss-bar
      cl_parj3=0.4          ! default PARJ(3) 0.4, HERMES 0.25, the extra suppression of strange diquark production compared with the normal suppression of strange quarks. 
c      cl_parj2=0.1          ! harut
c      cl_parj3=0.2          ! harut


      cl_parj4=0.05
      cl_parj5=0.5
      cl_parj6=0.5
      cl_parj7=0.5


      cl_parj12=0.4          ! default PARJ(12) fraction of spin 1 (affects K*s)
      cl_parj21=0.3          ! default PARJ(21) for the width of P_T distribution  default in JETSET 0.36
      cl_parj23=0.01 
      cl_parj24=2.

      cl_parj33=0.3          ! default PARJ(33) 0.8 define the remaining energy below which the fragmentation of a parton system is stopped and two final hadrons formed.

c PARJ(41), PARJ(42) :  (D=0.3, 0.58 GeV$^{-2}$) give the $a$ and $b$ parameters of the symmetric Lund fragmentation function for MSTJ(11)=1, 4 and 5 (and MSTJ(11)=3 for ordinary hadrons).
      cl_parj41=0.30         ! default
      cl_parj42=0.58         ! default
c      cl_parj41=1.13         ! HERMES 
c      cl_parj42=0.37         ! HERMES 

      cl_mstu=1              ! default mstu(16)
      cl_zpos=0.0            ! z-position
      cl_zwidth=0            ! z-width ( zpos+/-zwidth)
      cl_rast=1.2            ! raster diameter in cm
      cl_trad=20             ! target radius
      cl_pol=0.0               ! unpolarized target
      cl_beam_energy=10.6   ! def eg1-2000 (e16-5.754,e1f-5.498)
      cl_emin=0.75           ! def e'min
      cl_emax=0.0  
      cl_tmin=0.087           ! e16->0.3           ! def e'tmin
      cl_tmax=1.57           ! def e'tmax
      cl_target='proton'     ! (def proton target)
c
      cl_xmin=0.05
      cl_xmax=0.95
      cl_qmin=0.85
      cl_qmax=20.0
      cl_wmin=4.0
      cl_wmax=400.0
c
      cl_zmin=0.3           ! def z-min
c
      cl_radgen=.FALSE.
      cl_radtab=.FALSE.
      cl_verblev=0
      cl_nprint=1000         ! print every cl_nprint event
      cl_rand=1              ! update the random number in end
      cl_triggers = 10  
      cl_nmax = 20000         ! max number of events in the file
      cl_ht=0
      cl_pid = 0               ! take all  
      cl_printid = 0           ! not print the tmp(kaon) file 
      cl_gagid=0               ! to write a new record gagid=211 
      cl_fld = 2250.0             ! default 2250 
      cl_single=.FALSE.
      ntOK=.TRUE.
      acce16=0
      acceg1=0
      accclas12=0
      datfileOK=.TRUE.               ! don't write a data file
      cl_path='eventfiles/'     ! path for the datafile
      cl_outform=1                    ! All events (2-LUND-GSIM)
      numopts  = IARGC()
c      
      if(numopts.gt.0 ) then
        i=0
      else
        goto 2
      endif
 1    i=i+1
        CALL GETARG(i,cnumber)
        if(cnumber.eq.'--trig'.and.i.lt.numopts) then
          i=i+1
          CALL GETARG(i,cnumber)
          cl_triggers=valnum(cnumber)
        elseif(cnumber.eq.'--nmax'.and.i.lt.numopts) then
          i=i+1
          CALL GETARG(i,cnumber)
          cl_nmax=valnum(cnumber)
        elseif(cnumber.eq.'--v'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_verblev=valnum(cnumber)
        elseif(cnumber.eq.'--printid'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_printid=valnum(cnumber)
        elseif(cnumber.eq.'--print'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_nprint=valnum(cnumber)
        elseif(cnumber.eq.'--pol'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_pol=valnum(cnumber)
        elseif(cnumber.eq.'--outform'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_outform=valnum(cnumber)
        elseif(cnumber.eq.'--beam'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_beam_energy=valnum(cnumber)
        elseif(cnumber.eq.'--pid'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_pid=valnum(cnumber)
        elseif(cnumber.eq.'--gagid'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_gagid=valnum(cnumber)
        elseif(cnumber.eq.'--z'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_zmin=valnum(cnumber)           
        elseif(cnumber.eq.'--t'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_tmin=valnum(cnumber)*3.1415926/180.0
           i=i+1
           CALL GETARG(i,cnumber)
           cl_tmax=valnum(cnumber)*3.1415926/180.0
        elseif(cnumber.eq.'--e'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_emin=valnum(cnumber)
           i=i+1
           CALL GETARG(i,cnumber)
           cl_emax=valnum(cnumber)
        elseif(cnumber.eq.'--x'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_xmin=valnum(cnumber)
           i=i+1
           CALL GETARG(i,cnumber)
           cl_xmax=valnum(cnumber)
        elseif(cnumber.eq.'--q'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_qmin=valnum(cnumber)
           i=i+1
           CALL GETARG(i,cnumber)
           cl_qmax=valnum(cnumber)
        elseif(cnumber.eq.'--w'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_wmin=valnum(cnumber)
           i=i+1
           CALL GETARG(i,cnumber)
           cl_wmax=valnum(cnumber)
        elseif(cnumber.eq.'--zpos'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_zpos=valnum(cnumber)
        elseif(cnumber.eq.'--trad'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_trad=valnum(cnumber)
        elseif(cnumber.eq.'--zwidth'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_zwidth=valnum(cnumber)           
         elseif(cnumber.eq.'--raster'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_rast=valnum(cnumber)          
       elseif(cnumber.eq.'--targ'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cl_target)
        elseif(cnumber.eq.'--path'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cl_path)
         elseif(cnumber.eq.'--nont') then
           ntOK=.FALSE.
        elseif(cnumber.eq.'--norand') then
          cl_rand=0
        elseif(cnumber.eq.'--radgen') then
           cl_radgen=.TRUE.
        elseif(cnumber.eq.'--radtab') then
           cl_radtab=.TRUE.
        elseif(cnumber.eq.'--HT'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_ht=valnum(cnumber)
        elseif(cnumber.eq.'--lst8'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_lst8=valnum(cnumber)
        elseif(cnumber.eq.'--lst15'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_lst15=valnum(cnumber)
        elseif(cnumber.eq.'--lst37'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_lst37=valnum(cnumber)
        elseif(cnumber.eq.'--mstu') then
          cl_mstu=2
        elseif(cnumber.eq.'--parj2'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj2=valnum(cnumber)
        elseif(cnumber.eq.'--parj3'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj3=valnum(cnumber)
        elseif(cnumber.eq.'--parj4'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj4=valnum(cnumber)
        elseif(cnumber.eq.'--parj5'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj5=valnum(cnumber)
        elseif(cnumber.eq.'--parj6'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj6=valnum(cnumber)
        elseif(cnumber.eq.'--parj7'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj7=valnum(cnumber)


        elseif(cnumber.eq.'--parj12'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj12=valnum(cnumber)
        elseif(cnumber.eq.'--parj21'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj21=valnum(cnumber)
        elseif(cnumber.eq.'--parj23'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj23=valnum(cnumber)
        elseif(cnumber.eq.'--parj24'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj24=valnum(cnumber)


        elseif(cnumber.eq.'--parj33'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj33=valnum(cnumber)
        elseif(cnumber.eq.'--parj41'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj41=valnum(cnumber)
        elseif(cnumber.eq.'--parj42'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parj42=valnum(cnumber)
        elseif(cnumber.eq.'--parl3'.and.i.lt.numopts) then
           i=i+1
           CALL GETARG(i,cnumber)
           cl_parl3=valnum(cnumber)
        elseif(cnumber.eq.'--datf') then
           datfileOK=.FALSE.
        elseif(cnumber.eq.'--acce16') then
           acce16=1
        elseif(cnumber.eq.'--single') then
           cl_single=.TRUE.
        elseif(cnumber.eq.'--hermes') then
           cl_her=.FALSE.
        elseif(cnumber.eq.'--acceg1') then
           acceg1=1
        elseif(cnumber.eq.'--accclas12') then
           accclas12=1
        else
          call printusage
          stop
        endif
        if(i.lt. numopts) goto 1
 2      continue
        if(cl_emax.eq.0.0) then
         cl_emax=0.95*cl_beam_energy            ! put some limit
        endif
        call printvalues
        return 
        end
        subroutine printusage
        print *,'clasDIS [options]            '
        print *,'      option  value      default  comment  '
        print *,'  --v    verbos_level    0     additional printout '
        print *,'  --trig nevents         10     number of triggers '
        print *,'  --beam beam_energy     10.6   e- momentum in GeV '
c23456789012345678901234567890123456789012345678901234567890123456789012  
        print *,'  --e e-min e-max  min[750] max[0.95*e] Eprime in GeV '
        print *,'  --t tmin tmax  5.0    theta min/max for \theta(deg)'
        print *,'  --x xmin xmax        0.05 0.95    x_b min/max'
        print *,'  --q Q2min Q2max        0.85 20.0 Q2 min max in GeV^2'
        print *,'  --w W2min W2max        4.0  50     W^2 min max GeV^2'
        print *,'  --z                 0.3       z=E_pi/nu min value '
        print *,'  --zpos z-position      0    target z position in cm '
        print *,'  --zwidth z-width 0    width in z cm (zpos+/-zwidth) '
        print *,'  --raster diameter 12   raster diameter in cm  '
        print *,'  --nont               do not write out the ntuple '
        print *,'  --radgen                   include radgen'
        print *,'     --radtab         generate radgen table for SIDIS'
        print *,'     --targ target proton   deuteron/neutron possible '
        print *,'     --pol  polarization    0%  -100 - 100% target pol'
        print *,'     --print nprint     1000 print every nprint event '
        print *,'     --printid id       0   print tmp-file forid=321 '
        print *,'     --acceg1                  acceptance for eg1 ON '
        print *,'     --acce16                  acceptance for e16 ON '
        print *,'     --accclas12            acceptance for clas12 ON '
        print *,'     --pid  LUND_ID  0   0-all,211-pip,3122-Lambda..'
        print *,'     --path  path    /home/avakian/w6   '
        print *,'     --norand             keep the old random number'
        print *,'     --mstu                     set mstu(16)=2'
        print *,'     --parj12        0.4        set PARJ(12)'
        print *,'     --parj33        0.3        set PARJ(33)'
        print *,'     --lst15         145        set the PDF set'
        print *,'     --lst8          1        set QCD mechanism'
        print *,'     --hermes              set HERMES JETSET variables'
        print *,'     --lst37   0          set LST(37) 1-Ak Cahn+Sivers'
        print *,'     --parl3         0.64       set PARL(3)'
        print *,'     --HT    iht    0   higher twists on (LST(11)=iht)'
        print *,'     --outform  1  output filr with 1-all 2-GSIM 3-ga'
        print *,'     --datf       .TRUE.      write the output file'
        print *,'     --nmax   value     0  maximum events in a file'
        print *,'     --single   value   .FALSE. /true gen fixed tracks'
        return
        end
c
        subroutine printvalues
        implicit none
#include "names.inc"
#include "claslepto.inc"
        print *,'clasDIS input values:'    
        print *,'NEVENTS:             ',cl_triggers    
        print *,'nmax-events       :   ',cl_nmax
        print *,'BEAM ENERGY:         ',cl_beam_energy  
        print *,'Eprime   min/max(GeV):         ',cl_emin,cl_emax  
        print *,'Eprime tmin/tmax(GeV):         ',cl_tmin,cl_tmax  
        print *,'TARGET:              ', cl_target 
        print *,'TARGET z-position:   ', cl_zpos 
        print *,'TARGET 1/2 width :   ', cl_zwidth
        print *,'raster diameter  :   ', cl_rast
        print *,'TARGET radius :      ', cl_trad
        print *,'polarization     :   ', cl_pol 
        print *,'acceptance       : ',acceg1,acce16,accclas12,datfileOK 
        print *,'pid to keep:         ',cl_pid  
        print *,'parj(2) set to   :   ',cl_parj2  
        print *,'parj(3) set to   :   ',cl_parj3
        print *,'parj(4) set to   :   ',cl_parj4  
        print *,'parj(5) set to   :   ',cl_parj5
        print *,'parj(6) set to   :   ',cl_parj6  
        print *,'parj(7) set to   :   ',cl_parj7
        print *,'parj(12) set to   :   ',cl_parj12  
        print *,'parj(21) set to   :   ',cl_parj21  
        print *,'parj(23) set to   :   ',cl_parj23  
        print *,'parj(24) set to   :   ',cl_parj24  
        print *,'parj(33) set to   :   ',cl_parj33
        print *,'parj(41) set to   :   ',cl_parj41  
        print *,'parj(42) set to   :   ',cl_parj42  
        print *,'parl(3)  set to   :   ',cl_parl3  
        print *,'LST(15) PDF set   :   ',cl_lst15  
        print *,'LST(8)  QCD mechanism   :   ',cl_lst8  
        print *,'SINGLE-tarcks     :   ', cl_single 
        print *,'RADGEN ON        :   ', cl_radgen 
        if(cl_her) print *,'set HERMES/JESTET variables'
        if(cl_radtab) print *,'Generating radgen table',cl_beam_energy
        if(cl_lst37.gt.0) print *,'Using AK Cahn+Sivers',cl_lst37  
        if(cl_ht.gt.0) print *,'Higher twists ON: LST(11)=',cl_ht 
        print *,'__________________________________'
        print *,'verbosity level',  cl_verblev
        return
        end

C******************************************************************


        SUBROUTINE DISINIT
        IMPLICIT NONE

C*******************************************************************
#include "names.inc"
#include "claslepto.inc"

C.. Set up cuts and parameters which are different from the default
C.. valules 
c
       REAL PLZ,PPZ
c
       integer lst40
       CHARACTER*1 CTARGET
c
       real pi,twopi,phimu
       integer iazuser
       common/twopi/pi,twopi
       data pi,twopi/3.1415926,6.2831852/
       common/azopt/iazuser,phimu
c
c$$$      lst(1)=2 ! x-y greed
c$$$      lst(3)=1 ! output
c$$$      lst(5)=3 ! lab system
c$$$      lst(7)=0 ! kinem. and parton level event generated
c$$$      lst(12)=2 ! max flavour in sea
c$$$      lst(18)=1 ! alfa_em=1/137
C.. $x$ min cut  
       CUT(1)=cl_xmin
C.. $x$ max cut  
       CUT(2)=cl_xmax
C.. $y$ min cut 
       CUT(3)=0.05
C.. $y$ max cut  
       CUT(4)=0.95
C.. $Q^2$ min cut
       CUT(5)=cl_qmin
C.. $Q^2$ max cut
       CUT(6)=cl_qmax
C.. $W^2$ min cut    
       CUT(7)= cl_wmin
C.. $W^2$ max cut   
       CUT(8)= cl_wmax
C.. lower limit scat. lep. energy
       CUT(11)=cl_emin !0.6 
       print *,'e_min,cut(11)',cl_emin,CUT(9),CUT(10),CUT(11),CUT(12)
C..  upper limit scat. lep. energy
       CUT(12)=cl_emax !1.65
C.. $\nu$ min cut
       CUT(9)=0.1
C.. $\nu$ max cut
       CUT(10)= cl_beam_energy-cl_emin
C.. lower limit scat lep. angle
       CUT(13)=cl_tmin !0.52-0.08          ! 29.75 degree+/-4
C..  upper limit scat lep. angle
       CUT(14)=cl_tmax !0.52+0.08
C
        LST(1)=1 ! x-Q^2 greed default
C.. Cuts from LEPTOU applied
c       LST(2) = 2
C.. Prints detailed output and MINUIT-output 
       LST(3)= 5          ! default 5
C.. With matrix elements 
c       LST(7)= 1          ! from Arams mail 3/13/2012 
       LST(8)= cl_lst8        ! default 12
C.. With matrix elements 
       LST(11)= cl_ht
C.. Number of intrinsic sea-quark flavours 
       LST(12) = 4        ! default 4
C.. Parton distribution:  145 grsv2000 hep-ph/0011215 LO valence scenario
C.. Parton distribution:  147 grsv2000 hep-ph/0011215 NLO valence scenario
C.. Parton distribution:  110 Gehrmann  et al hep-ph/9512406 Gluon A (LO)
       LST(15) = cl_lst15
C.. choice of the x W grid
C       LST(19) = 10
C.. Only $\gamma$ exchange in the simulation. 
       LST(23) = 1
C.. Only valence-isopin roation
       LST(36) = 0
C..
C..  LST(37)=0 standard PEPSI/LEPTO LST(37)=1 us AK Cahn patch
C.. 
       LST(37)=cl_lst37
       iazuser=cl_lst37
C.. Multiplication factor for the polarized parton distributions,
C.. This is designed in oder to shorten the CPU time necessary  
C.. considerably. For example, a multiplication factor of 100 yields 
C.. a shortening of the running time of a factor 10000, i.e in order  
C.. to generate the same result in a run without this multiplication  
C.. factor one has to produce 10000 times more events. 
       LST(38) = 1
C.. Rotation is happening in the program automatically
       LST(39) = 0
C.. Set up of the polarization configuration
       LST(40) = cl_pol        ! 0 unpolarized run
C.. set mstu
       MSTU(16)=cl_mstu

C-----------------------------
C.. set parj(2)-suppress ss-bar default 0.3, HERMES 0.15
       PARJ(2)=cl_parj2
       PARJ(3)=cl_parj3
C-----------------------------
       PARJ(4)=cl_parj4
       PARJ(5)=cl_parj5
       PARJ(6)=cl_parj6
       PARJ(7)=cl_parj7
C-----------------------------
C.. set parj(12)-suppress K*
       PARJ(12)=cl_parj12
C.. set parj(21)-P_T in fragmentation (default 0.36)
       PARJ(21)=cl_parj21
       PARJ(23)=cl_parj23
       PARJ(24)=cl_parj24
C.. set parj(33)-define the remaining energy below which the fragmentation of a parton system is stopped and two final hadrons formed. (default 0.8)
       PARJ(33)=cl_parj33
       PARJ(41)=cl_parj41
       PARJ(42)=cl_parj42
C. set HERMES/JETSET
       if(cl_her) call parjset4hermes()
C.. Momentum of incoming lepton (in GeV)
       PLZ = cl_beam_energy  

C.. Momentum of incoming nucleon  (in GeV)
       PPZ   = 0.0
c
c set <k_T^2>
c
        PARL(3)=cl_parl3
C.. Simulation for a proton
      if(cl_target.eq.'deuteron') then
        write(CTARGET,'(A)') 'D'
        PARL(1) = 2
        PARL(2) = 1 
        FMAXFH  = 1.7
      elseif(cl_target.eq.'neutron') then
        write(CTARGET,'(A)') 'D'
        PARL(1) = 1
        PARL(2) = 0
        FMAXFH  = 1.5
      elseif(cl_target.eq.'proton') then
        write(CTARGET,'(A)') 'P'
        PARL(1) = 1
        PARL(2) = 1
        FMAXFH  = 1.9
      else               ! proton
          call printusage
          stop       
      endif
C..
C..  used in PEPSI
C..  IF(PARI(LST(23)).EQ.0.0) PARI(LST(23))=FMAXFH

C.. Call of the {\sc Pepsi}/{\sc Lepto} initialization routine 
C..       CALL POLLINIT(0,11,PLZ,PPZ,1)
          CALL LINIT(0,11,PLZ,PPZ,1)
c
c  init radgen
c
        lst40=0  ! unpolarized case only
        if(cl_radtab) then  ! generate the radgen table first
         print *,'start generating the radgen Table'
         call radgen_init(ctarget,cl_beam_energy,lst40,0)
         print *,'end generating the radgen Table'
         stop
        endif 
        if(cl_radgen) then
         print *,'--------------------------------------------------'
         print *,'Using radgen. Please make sure the table is correct'
         print *,'--------------------------------------------------'
         call radgen_init(ctarget,cl_beam_energy,lst40,2)
        endif 

        RETURN
        END
      


C**********************************************************************
C      random number generator
C**********************************************************************
         real function random_num()
         implicit none
         random_num=RAND(0)
         return
         end



C**********************************************************************
C     gaussian-distributed random number generator
C**********************************************************************
       
         real function random_norm()
         
         real vect(1)
         CALL  MYRNORML(vect(1),1)
         random_norm=vect(1)

         return
         end




C**********************************************************************
C
C       Subroutine for saving satus of random number generator
C
C**********************************************************************


       SUBROUTINE RNDMSAVE(LFN)
C.. Saving of the satuts of the randum number generator 

       COMMON/LUDATR/MRLU(6),RRLU(100)
       INTEGER  MRLU
       REAL  RRLU
       SAVE /LUDATR/
       INTEGER       LFN,I1,I2    
c
       OPEN(LFN,FILE="RNDMSTATUS",status='UNKNOWN')
       WRITE(LFN,*) (MRLU(I1),I1=1,5),
     & (RRLU(I2),I2=1,100)
       CLOSE(LFN)

       RETURN
       END       
      
C***********************************************************************
C
C         Subroutine for reading satus of random number generator
C
C**********************************************************************


        SUBROUTINE RNDMSTART(LFN)
C.. Read in the current status of the random number generator 

        COMMON/LUDATR/MRLU(6),RRLU(100)
        INTEGER  MRLU
        REAL  RRLU
        SAVE /LUDATR/

        INTEGER       LFN,I1,I2    
        LOGICAL       EX
       
       INQUIRE(FILE="RNDMSTATUS",EXIST = EX)
       IF (EX) THEN

       OPEN(LFN,FILE="RNDMSTATUS",status='UNKNOWN')
       READ(LFN,*) (MRLU(I1),I1=1,5),
     & (RRLU(I2),I2=1,100)
       CLOSE(LFN)

       ENDIF

       RETURN
       END       
       

C***********************************************************************
      SUBROUTINE CLASFILL(IEVENT)
C***********************************************************************
       implicit none
#include "names.inc"
#include "ntupgkoko.inc"
#include "claslepto.inc"
c
c
c
          integer ievent,ierr,lunout
	  parameter (lunout=33)
          integer ind,indx,mctk,mcvx,mbank,nbank,part
          integer j,jj,lund2geantid,jparent
          real pmom,plu,random_num,random_norm
C ********************************************************
C       Write to the output event file
C ********************************************************
c
c       
c
       call VZERO(gnele,12)
       call VZERO(gnpi0,35)
       call VZERO(gnkap,10)
        jj=0
        jparent=0
c
         do 999 j=1,N
c
       if(k(j,1).gt.10) goto 999
c       if(k(j,2).eq.22.and.  k(k(j,3),2).eq.111) goto 999
         jj=jj+1
c
         call fillevent(j,jparent)

 999    CONTINUE
c 
          call goodevent(IEVENT)
c          call goodeventcahn(IEVENT)
c
c         call radgen to get true values and add the rad photon
c
          if(cl_radgen) then
           radq2=gq2
           radnu=gq2/2.0/0.93827/gxb
            print *,'St fil --',gq2,gxb,gq2/2.0/cl_beam_energy/gxb/0.938
            call fillradgen()
            print *,'End  fillradgen ---------------------'
          endif
c
c
c
#ifdef hbook
          if(clasdisOK.and.ntOK) then
c          if(LST(24).gt.1) print *,'Proc-LST(24) =  ',LST(24),LST(8)
c     6,LST(9),LST(12),LST(25)
            call HFNT(22)
         endif
#endif
c
          if(cl_printid.eq.0.and.clasdisOK.and.datfileOK) then
                 call write2file(jj,cl_outform,jparent)  ! 1-all 2-GSIM-lund
          else if(cl_printid.eq.321) then ! print kaons for RICH studies
            call write2filed()
          else if(cl_printid.gt.0) then ! print forward hadrons <cl_printid degree
            call write2filep(jj,cl_outform)
          endif               ! if(clasdisOK.and.datfileOK)

c
      RETURN
      END     

c
C***********************************************************************
          subroutine single_event()
C***********************************************************************
          implicit none
#include "names.inc"
#include "claslepto.inc"
      integer Nthet_S,Nmom_S
      PARAMETER (Nthet_S=32)
      PARAMETER (Nmom_S=24)
      REAL theta_s(Nthet_S),moment_s(Nmom_s)
      INTEGER isec
      DATA moment_s/
     +    0.2,   0.4,    0.6,    0.8,   1.0,    1.4,   1.8,   2.2,
     +    2.6,   3.0,    3.5,    4.0,   4.5,    5.0,   5.5,   6.0,
     +    6.5,   7.0,    7.5,    8.0,   8.5,    9.0,   10.0,  11.0/
      DATA theta_s/
     +    5.0,   5.5,    6.0,    6.5,   7.0,    7.5,   8.0,   8.5,
     +    9.0,   10.0,  11.0,    12.0, 13.0,   14.0,  15.0,   16.0,
     +    18.0,  20.0,  22.0,    25.0, 30.0,   35.0,  40.0,   45.0,
     +    50.0,  55.0,  60.0,    65.0, 70.0,   75.0,  80.0,  90.0/
       isec=rand(0)*6+1
      return
      end

C***********************************************************************
          subroutine write2file(jj,il,jparent)
C***********************************************************************
c write the LUND event:
c
c the header:
c N number of particles in the event
c PARL(1),PARL(2): number of nucleons and protons of target
c cl_pol,elpol: polarization of proton and electron
c LEX,LEYY,LEW2,LEQ2,LEU: x,y,W^2,Q^2, nu
c
c the body:
c i particle number
c plu(i,6) : particle charge
c
c K(I,1) = 3, 13} or \ttt{14}, instead, it  contains special
c K(I,2) : particle code
c K(I,3) : line number of parent particle, where known,otherwise 0. 
c K(I,4) : normally the line number of the first daughter and
c          0 for an undecayed particle or unfragmented parton.
c
c P(I,1) $p_x$, momentum in the $x$ direction, in GeV/$c$.
c P(I,2) $p_y$, momentum in the $y$ direction, in GeV/$c$.
c P(I,3) $p_z$, momentum in the $z$ direction, in GeV/$c$.
c P(I,4) $E$, energy, in GeV.
c P(I,5) $m$, mass, in GeV/$c^2$. 
c       In parton showers, with  space-like virtualities, 
c       i.e.\ where $Q^2 = - m^2 > 0$, one puts \ttt{P(I,5)}$ = -Q$.
c
c V(I,1) $x$ position of production vertex, in cm.
c V(I,2) $y$ position of production vertex, in cm.
c V(I,3) $z$ position of production vertex, in cm.
c V(I,4) time of production, in mm/$c$ ($\approx 3.33 \times 10^{-12}$ s).
c V(I,5) proper lifetime of particle, in mm/$c$
c      ($\approx 3.33 \times 10^{-12}$ s). 
c      If the particle is not expected to decay V(I,5)=0}.
c  A line with K(I,1)=4, i.e.\ a  particle that could have decayed,
c   but did not within the allowed region, has the proper non-zero V(I,5).
cIn the absence of electric or magnetic fields, or other
cdisturbances, the decay vertex \ttt{VP} of an unstable particle
cmay be calculated as VP(j) = V(I,j) + V(I,5)*P(I,j)/P(I,5)},
c
c
c
c
          implicit none
#include "names.inc"
#include "claslepto.inc"
#include "ntupgkoko.inc"
          integer ifile,itotal,imaxev
          DATA ifile /0/
          DATA itotal /0/
c          character*100  fname
          character*200  fname
          character*8  c8name
c
          real plu,z_pos,x_pos,y_pos,random_num,random_norm,trad,gpipmo
          integer jj,il,i,j,istat,jparent,parl1,parl2
          real elpol
          logical lFirst
          DATA lFirst /.TRUE./
          DATA elpol /1.0/

          integer jc,m,lund2geantid,gamp_id,icharge
          integer  beam_pid,targ_id,process_id
          data beam_pid/11/,targ_id/2212/,process_id/1/
c
          parl1=PARL(1)
          parl2=PARL(2)
          if(cl_nmax.gt.0) then
          imaxev=cl_nmax
          else
          imaxev=PARL(23)
          endif
          if(itotal.ge.imaxev) then   ! each file correspond to number ~ xsection
           close (41)
           lFirst=.TRUE.
           ifile=ifile+1
           itotal=0
          endif



          if (lFirst) then   ! open the file
             call get_datfilename()

             jc = 0
             do m=1,len(datfilename)
                if ((datfilename(m:m).eq.' ').and.(jc.eq.0)) then
                   jc=m-1
                endif
             enddo
             if (jc.eq.0) jc=len(datfilename)

           if(ifile.le.9) then
             write(c8name,'(A,i1,A)') '000',ifile,'.dat'
           else if(ifile.ge.10.and.ifile.le.99) then
             write(c8name,'(A,i2,A)') '00',ifile,'.dat'
           else if(ifile.ge.100.and.ifile.le.999) then
             write(c8name,'(A,i3,A)') '0',ifile,'.dat'
           else
             write(c8name,'(i4,A)')  ifile,'.dat'
c stay with numbers below 9999 for now
           endif 
             write(fname,'(A,A)') datfilename(1:jc),c8name
             write(6,'(A,A)') 'Data file Name ',fname

             OPEN(unit=41,file=fname,status='unknown')
             lFirst=.FALSE.
          endif
c
                itotal=itotal+1


            if (lFirst) then   ! open the file
             OPEN(unit=41,file=datfilename,status='unknown')
             lFirst=.FALSE.
            endif
c
c il=cl_outform defined by --outform, default 1
c

               if(il.eq.2) then
                 j=0
                 do i=1,N
                  if(k(i,1).lt.11) j=j+1   ! save space
                 enddo
c23456789012345678901234567890123456789012345678901234567890123456789012  
                write(41,101) j,PARL1,PARL2,cl_pol,elpol,
     6            beam_pid,cl_beam_energy,targ_id,process_id,PARL(23)
               else
                if(cl_gagid.ne.0.and.jparent.gt.0) then          ! Gagik's file
                 write(41,101) N+2,PARL1,PARL2,cl_pol,elpol,
     6            beam_pid,cl_beam_energy,targ_id,process_id,PARL(23)
                 else
                 write(41,101) N,PARL1,PARL2,cl_pol,elpol, ! regular file
     6            beam_pid,cl_beam_energy,targ_id,process_id,PARL(23)
                endif
               endif
c
 111           FORMAT(2x,I5,10F10.4)
 110           FORMAT(2x,10I10)
 112           FORMAT(2x,10F10.4)
 113           FORMAT(2x,2I4,10F10.5)
c 101      FORMAT(2x,I10,2F4.0,2I3,5F6.3)
 101      FORMAT(2x,I10,2I6,2F5.1,I5,F10.3,2I5,E14.7)
c changed for longer ID 102      FORMAT(2x,I3,F4.0,2I5,2I3,5F10.4,2X,5F10.4)
 102      FORMAT(2x,I3,F4.0,I5,I7,2I3,5F10.4,2X,5F10.4)
c
c        what is this?
c
c          z_pos=cl_zpos+(2.*random_num()-1)*cl_zwidth
c
c 120                x_pos=0.1*random_norm()
c                    if(x_pos.GT.cl_trad.OR.x_pos.LT.-cl_trad)goto 120 
c                 
c 121                y_pos=0.1*random_norm()
c                    if(y_pos.GT.cl_trad.OR.y_pos.LT.-cl_trad)goto 121 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

              do i=1,N
               if(abs(k(i,2)).gt.9999) k(i,2)=k(i,2)/100.0
c
               if(il.eq.2.or.il.eq.3) then
                 if(k(i,1).lt.11) then   ! save space
                  if(il.eq.2) then
                   write(41,110) (k(i,j),j=1,2),k(k(i,3),2),i,k(i,3)
                   write(41,112) (p(i,j),j=1,5) 

c23456789012345678901234567890123456789012345678901234567890123456789012  

                  write(41,112) (gelex+V(i,1)*0.1),(geley+V(i,2)
     6                           *0.1),(V(i,3)*0.1+gelez), 0.0,0.0

                 else    ! gamp
              gamp_id=lund2geantid(k(i,2))
              icharge=plu(i,6)
              write(41,113) gamp_id,icharge,(p(i,j),j=1,4)    
                 endif        ! il=2
                 endif        ! save space
               else           ! regular lund file
c23456789012345678901234567890123456789012345678901234567890123456789012 
                 if(P(i,5).gt.-100) then
                 gelex=gelex+V(i,1)*0.1
                 geley=geley+V(i,2)*0.1
                 gelez=V(i,3)*0.1+gelez
                 write (41,102) i,plu(i,6),(k(i,j),j=1,4),(P(i,j),j=1,5)
     6         ,gelex,geley,gelez
                 else
                 write (41,102) i,plu(i,6),(k(i,j),j=1,4),0.0,0.0
     6                            ,0.0,0.0,0.0,(V(i,j)*0.1,j=1,3)
                 print *,'ERROR! weird P(i,5)',P(i,5)
                 endif
               endif
              enddo
               if(cl_gagid.ne.0.and.jparent.gt.0) then          ! Gagik's file
           write(41,102) 1,-1.0, 2, 11, 4, 0
     6                  ,gelee*sin(gelet)*cos(gelef) 
     6                  ,gelee*sin(gelet)*sin(gelef) 
     6                  ,gelee*cos(gelet) 
     6                  ,gelee,0.0005,0.0,0.0,0.0 
           gpipmo=sqrt(gpipe*gpipe-0.0193)
           write(41,102) 2,1.0, 2, 211, jparent, 0 
     6                  ,gpipmo*sin(gpipt)*cos(gpipf)
     6                  ,gpipmo*sin(gpipt)*sin(gpipf) 
     6                  ,gpipmo*cos(gpipt) 
     6                  ,gpipe,0.139,0.0,0.0,0.0 
               endif

          return
          end
c
c
c
c
c
C***********************************************************************
      SUBROUTINE goodevent(j)
C***********************************************************************
       implicit none
#include "ntupgkoko.inc"
#include "names.inc"
       integer j,iaccept,eg1amcaccept
       real pi4(4),qiu4(4),el04(4),elf4(4),tnorm(4)
       real pi,anu,pien,amp,ebeam,lambda
       real pro4(4),tnorm2(4),gkapm
       real vmass,vangle,vdotm,zpi,zka
       real gt,gmismasrho,gphistar,pimis
       integer icountall,icount113,icount113a
       data icountall,icount113,icount113a/0,0,0/
c
       clasdisOK=.FALSE.
       pi=acos(-1.0)
       amp=0.93827
       ebeam=cl_beam_energy
       anu=ebeam-gelee
c     
       iaccept=eg1amcaccept(cl_beam_energy,gelee,gelet,gelef)
       if(acceg1.eq.1.and.iaccept.eq.0) then
          return
       endif
         icountall=icountall+1

       SELECT CASE (abs(cl_pid))  
       CASE (0)                 ! write all events
       clasdisOK=.TRUE.
       CASE (211)               ! pi+/-
       if(gpipe/(ebeam-gelee).gt.cl_zmin
     6      .or.gpime/(ebeam-gelee).gt.cl_zmin) clasdisOK=.TRUE.
c
       CASE (321)               ! exclusive K+/-
       if(gnkap.gt.0) then
        gkapm=gmismasrho(11)
        if(gkapm.gt.1.0.and.gkapm.lt.1.23) clasdisOK=.TRUE.
       endif
c
       CASE (111)               ! pi0 in sec4
      if(gnpi0.gt.0.and.gelef.gt.2.79.and.gelef.lt.3.5.and.gelet.gt.0.2) 
     6   clasdisOK=.TRUE.
       CASE (3122)              ! Lambda
       
       if(gnpim.gt.0.and.gnpro.gt.0.and.lambda(1,ebeam,1.15).gt.0) then
c          print*,j
          clasdisOK=.TRUE.
       endif
c       if(gnpi0.gt.0.and.gnneu.gt.0.and.lambda(2,ebeam,1.2).gt.0) then
c          print*,j
c          clasdisOK=.TRUE.
c       endif
       
       CASE (9211)              ! Exclusive pi+
       if(gnpip.eq.1.and.gmismasrho(1).lt.0.95) then
          clasdisOK=.TRUE.
       endif
       CASE (113)              ! Exclusive pi+/pi-
       if(gngam.eq.0.and.gnpip.eq.1.and.gnpim.eq.1.and.gnpro.eq.1.and.
     6    gnkap.eq.0.and.gnkam.eq.0) then
          icount113=icount113+1
c! 8<theta<35
         if(accclas12.eq.1) then
           if(gelet.gt.0.14.and.gelet.lt.0.6    
     6.and.gprot.gt.0.14.and.gprot.lt.0.6
     6.and.gpipt.gt.0.14.and.gpipt.lt.0.6
     6.and.gpimt.gt.0.14.and.gpimt.lt.0.6) then
           clasdisOK=.TRUE.
           icount113a=icount113a+1
           endif
         else
           clasdisOK=.TRUE.
         endif 
c         print *,icount113,icount113a,icountall    
       endif
       CASE (521)              ! K+/K- in acceptance
       if(gnkap.gt.0.or.gnkam.gt.0) then
          icount113=icount113+1
c! 8<theta<35
         if(accclas12.eq.1) then
           if(gelet.gt.0.087.and.gelet.lt.0.6    
     6.and.((gkapt.gt.0.087.and.gkapt.lt.0.6)
     6.or.(gkamt.gt.0.087.and.gkamt.lt.0.6))) then
           clasdisOK=.TRUE.
           icount113a=icount113a+1
           endif
         else
           clasdisOK=.TRUE.
         endif 
c         print *,icount113,icount113a,icountall    
       endif
      CASE DEFAULT
      END SELECT       
      return
      end

c
        real function  PolarTheta(vx,vy,vz)
       implicit none
        real vx,vy,vz,pmod,theta
        pmod=vx*vx+vy*vy+vz*vz
        if(pmod .gt. 0 ) then
         theta=acos(vz/sqrt(pmod))
        else
         theta=-100
        endif
        PolarTheta=theta
        return
        end

        real function Azimuthalphi(vx,vy )
      implicit none
        real pi
        parameter ( PI=3.1415926)
        real vx,vy,pmod,phi,cosf
        pmod=vx*vx+vy*vy
        if(pmod .gt. 0 ) then
         pmod=sqrt(pmod)
         cosf=vx/pmod
        else
         cosf=1.0
        endif
        if(abs(cosf) .le. 1.0) phi=acos(cosf);
        if(vy .lt. 0.0) phi= 2*PI-phi;
        Azimuthalphi=phi
        return
        end
c
       subroutine fillevent(j,jparent)
       implicit none
#include "names.inc"
#include "ntupgkoko.inc"
#include "claslepto.inc"
c
       real amp,el04(4),elf4(4),pro4(4),qiu4(4)
       parameter (amp=0.938)
       integer part,part_id,parent_id,jparent
       real cx,cy,cz,theta,phi,pmom,px,py,pz,pener
       real Azimuthalphi,PolarTheta
       real anu,gw2,gpi0u,pi0x,pi0y,pi0z
       integer j,lund2geantid
       real xrast,yrast, random_num,accep11
c
       part_id=lund2geantid(k(j,2))
       parent_id=k(k(j,3),2)
       px=p(j,1)
       py=p(j,2)
       pz=p(j,3)
       pmom=sqrt(px*px+py*py+pz*pz)
       pener=sqrt(p(j,5)*p(j,5)+pmom*pmom)
       geleh=cl_pol
c
       if(pmom.gt.0) then
        cz=pz/pmom
        cx=px/pmom
        cy=py/pmom
       else
        cx=0
        cy=0
        cz=0
       endif

        if(cz.le.1.0) then
           theta=acos(cz)
        else
           theta=0
        endif
        phi=Azimuthalphi(px,py) !atan2(cy,cx)
c        print *,PolarTheta(px,py,pz),theta,Azimuthalphi(px,py),phi,gnele
c
 9    xrast=(cl_rast*(random_num()-0.5))
      yrast=(cl_rast*(random_num()-0.5))
      if(sqrt(xrast**2+yrast**2).gt.cl_rast/2.0) goto 9

       SELECT CASE (part_id)  
        CASE (3) ! electron
         gnele=gnele+1
c         if(gnele.eq.1 .or. gelee.lt.pener ) then
          gelee=pener
          gelet=theta
          gelef=phi
          gelex=xrast !gelee
          geley=yrast !gelet
          gelez=cl_zpos+(2.*random_num()-1)*cl_zwidth !gelef
          if(cl_smear.eq.1) call smear(part_id,gelee,gelet,gelef)  ! smear 
          anu=cl_beam_energy-gelee
          gq2=2.*cl_beam_energy*gelee*(1.0-cos(gelet))
          gw2=amp*amp+2.0*amp*anu-gq2
          if(gw2.gt.0) then
            gw=sqrt(gw2)
          else
            gw=0
          endif
        el04(1)=0
        el04(2)=0
        el04(3)=cl_beam_energy
        el04(4)=cl_beam_energy
c
        elf4(1)=gelee*sin(gelet)*cos(gelef)
        elf4(2)=gelee*sin(gelet)*sin(gelef)
        elf4(3)=gelee*cos(gelet)
        elf4(4)=gelee
         call vdifm(el04,elf4,qiu4,4)
c        print *,'q2',gq2,vdotm(qiu4,qiu4,4)          
          gxb=gq2/2.0/amp/anu
          gelebits=parent_id

c         endif 
        CASE (9) ! pi-
         gnpim=gnpim+1
         if(gnpim.eq.1 .or. gpime.lt.pener) then
          gpime=pener
          gpimt=theta
          gpimf=phi
          gelewgt=parent_id
          if(cl_smear.eq.1) call smear(part_id,gpime,gpimt,gpimf)  ! smear
         endif 

        CASE (14) ! proton
         gnpro=gnpro+1
         if(gnpro.eq.1 .or. gproe.lt.pener) then
          gproe=pener
          gprot=theta
          gprof=phi
          if(cl_smear.eq.1) call smear(part_id,gproe,gprot,gprof)  ! smear           
         endif 

        CASE (13) ! neutron
         gnneu=gnneu+1
         if(gnneu.eq.1 .or. gneue.lt.pener) then
          gneue=pener
          gneut=theta
          gneuf=phi
          if(cl_smear.eq.1) call smear(part_id,gneue,gneut,gneuf)  ! smear           
         endif 

        CASE (8) ! pi+
         gnpip=gnpip+1
         if(gnpip.eq.1 .or. gpipe.lt.pener) then  ! get the highest energy in NT
         if(cl_gagid.ne.0.and.
     6accep11(211,pener,theta,phi,cl_fld).eq.1) then
          jparent=j
           if(accep11(11,gelee,gelet,gelef,cl_fld).eq.0) jparent=0 ! make sure electron in the acceptance           
         else
          jparent=0
         endif
          gpipe=pener
          gpipt=theta
          gpipf=phi
          gpipx=gpipe
          gpipy=gpipt
          gpipz=gpipf
          if(cl_smear.eq.1) call smear(part_id,gpipe,gpipt,gpipf)  ! smear 
c          gelebits=parent_id
c          if(gelebits.eq.213) call lulist(2)
         endif 

        CASE (11) ! K+
         gnkap=gnkap+1
         if(gnkap.eq.1 .or. gkape.lt.pener ) then
          gkape=pener
          gkapt=theta
          gkapf=phi
          if(cl_smear.eq.1) call smear(part_id,gkape,gkapt,gkapf)  ! smear 
          nskap=parent_id
c          if(geleh.eq.313) call lulist(2)
         endif 
        CASE (12) ! K-
         gnkam=gnkam+1
         if(gnkam.eq.1 .or. gkame.lt.pener ) then
          gkame=pener
          gkamt=theta
          gkamf=phi
          if(cl_smear.eq.1) call smear(part_id,gkame,gkamt,gkamf)  ! smear 
          nskam=parent_id
c          if(geleh.eq.313) call lulist(2)
         endif 

        CASE (7) ! pi0
         gpi0u=gpi0u+1
         if(gnpi0.eq.1 .or. gpi0e.lt.pener ) then
          gpi0e=pener
          gpi0t=theta
          gpi0f=phi
         endif 

        CASE (1) ! gamma
         gngam=gngam+1
         if(gngam.eq.1 .or. ggame.lt.pener) then
           if(gngam.gt.1) then
            ggam2e=ggame
            ggam2t=ggamt
            ggam2f=ggamf
           endif
          ggame=pener
          ggamt=theta
          ggamf=phi
         endif
         if(gngam.gt.1) then
          gnpi0=1
          gpi0e=ggam2e+ggame
          pi0x=game*sin(gamt)*cos(gamf)+gam2e*sin(gam2t)*cos(gam2f)
          pi0y=game*sin(gamt)*sin(gamf)+gam2e*sin(gam2t)*sin(gam2f)
          pi0z=game*cos(gamt)+gam2e*cos(gam2t)
          pi0f=Azimuthalphi(pi0x,pi0y)
          pi0t=PolarTheta(pi0x,pi0y,pi0z)
         endif

        CASE DEFAULT

       END SELECT       
         return
         end

          subroutine smear(pid,e,t,f)
          IMPLICIT NONE
#include "names.inc"
          integer pid
          real p,e,t,f,rn1,rn2,rn3,rn4,sin6
          real dts,ps,es,ts,fs,rn(4)
           es=e
           ts=t
           fs=f
           sin6=9.6                  ! sin6^o
           call MYRNORML(rn,4)       ! replacing normal random generator
c           call rannor(rn1,rn2)
c           call rannor(rn3,rn4)
            rn1=rn(1)
            rn2=rn(2)
            rn3=rn(3)
            rn4=rn(4)
          SELECT CASE (pid)  
          CASE (3) ! electron
           p=e
          CASE (14) ! proton
           p=e*e-0.83827**2
           if(p.gt.0) then 
            p=sqrt(p)
           else
            p=0
           endif
          CASE (8) ! pi+
           p=e*e-0.139**2
           if(p.gt.0) then 
            p=sqrt(p)
           else
            p=0
           endif
          CASE (9) ! pi-
           p=e*e-0.139**2
           if(p.gt.0) then 
            p=sqrt(p)
           else
            p=0
           endif
          CASE (11) ! K+
           p=e*e-0.494**2
           if(p.gt.0) then 
            p=sqrt(p)
           else
            p=0
           endif

          CASE DEFAULT
          END SELECT       

           ps=p+(cl_sma+cl_smb*p)*p*rn1
           dts=(cl_smd+cl_smc/p)*rn2
           ts=t+dts
           fs=f+dts/sin6*rn3
           
          SELECT CASE (pid)  
          CASE (3) ! electron
           es=ps
          CASE (14) ! proton
           es=sqrt(ps*ps+0.83827**2)
          CASE (8) ! pi+
           es=sqrt(ps*ps+0.139**2)
          CASE (9) ! pi-
           es=sqrt(ps*ps+0.139**2)
          CASE (11) ! K+
           es=sqrt(ps*ps+0.494**2)
          CASE DEFAULT
          END SELECT       
           e=es
           f=fs
           t=ts
          return
          end


      INTEGER FUNCTION LUND2GEANTID(I)
      implicit none
      integer i,NSEL 
      PARAMETER (NSEL=44)
      INTEGER IPLUND(NSEL),IGE
      DATA IPLUND/
     +    22,   -11,    11,    12,   -13,    13,   111,   211,
     +  -211,   130,   321,  -321,  2112,  2212, -2212,   310,
     +   221,  3122,  3222,  3212,  3112,  3322,  3312,  3334,
     + -2112, -3122, -3112, -3212, -3222, -3322, -3312, -3334,
     +   -15,    15,   411,  -411,   421,  -421,   431,  -431,
     +  4122,    24,   -24,    23/

       DO IGE=1,NSEL
        IF(I.EQ.IPLUND(IGE)) THEN
         LUND2GEANTID=IGE
         RETURN
        ENDIF
       ENDDO
         LUND2GEANTID=0 
       RETURN
       END
c
      INTEGER FUNCTION GEANT2LUNDID(I)
      implicit none
      INTEGER NSEL
      PARAMETER (NSEL=44)
      INTEGER IPLUND(NSEL),I
      DATA IPLUND/
     +    22,   -11,    11,    12,   -13,    13,   111,   211,
     +  -211,   130,   321,  -321,  2112,  2212, -2212,   310,
     +   221,  3122,  3222,  3212,  3112,  3322,  3312,  3334,
     + -2112, -3122, -3112, -3212, -3222, -3322, -3312, -3334,
     +   -15,    15,   411,  -411,   421,  -421,   431,  -431,
     +  4122,    24,   -24,    23/

      IF(I.GT.0.AND.I.LE.NSEL) THEN
       GEANT2LUNDID=IPLUND(I)
      ELSE
       GEANT2LUNDID=0
      ENDIF 
       RETURN
      END

C********************************************************************* 

       real function gmismasrho(ip)
       implicit none
#include "ntupgkoko.inc"
#include "names.inc"
       integer ip
c
c      calculating azimuthal angle from lab variables
c      of hadron  
c
c      input variables: pie,pit,pif-momentum,theta,phi of hadron
c                       posq2,pose,posf-Q^2,momentum,phi of positron                 
c
c     CALL CROSS(A,B,C) C=[AxB]
c     VDOT(A,B,N), CALL VMUL(A,B,X,N) X_i=A_i.B_i  N=3  reals
c     VDOTN(A,B,N)=ab/|a||b|
c     VMOD (A,N)  =|a|
       real pi,tetgam,anu,pien,beam
       real eleq,pitg1,cospff1
       real xm,amp,tetgam1,elety,ebeam
       real pi4(4),qiu4(4),el04(4),elf4(4),tnorm(4)
       real pro4(4),tnorm2(4)
       real vmass,vangle,vdotm,phigstar
       pi=acos(-1.0)

        amp=0.93827
        ebeam=cl_beam_energy

c
c     define all 4momenta
c
       if(ip.eq.1) then
c
c      pi+
c
        if(gnpip.eq.0) print *,'Bad pip'
         pien=sqrt(gpipe*gpipe-0.139*0.139)
         pi4(4)=gpipe
         pi4(1)=pien*cos(gpipf)*sin(gpipt) 
         pi4(2)=pien*sin(gpipf)*sin(gpipt)
         pi4(3)=pien*cos(gpipt)
c
       elseif(ip.eq.-1) then
c      pi-
c
        if(gnpim.eq.0) print *,'Bad pim'
        pien=sqrt(gpime*gpime-0.139*0.139)
        pi4(4)=gpime
        pi4(1)=pien*cos(gpimf)*sin(gpimt) 
        pi4(2)=pien*sin(gpimf)*sin(gpimt)
        pi4(3)=pien*cos(gpimt)
c
       elseif(ip.eq.0) then
c      pi0
c
        if(gnpi0.eq.0) print *,'Bad pi0'
        pien=sqrt(gpi0e*gpi0e-0.135*0.135)
        pi4(4)=gpi0e
        pi4(1)=pien*cos(gpi0f)*sin(gpi0t) 
        pi4(2)=pien*sin(gpi0f)*sin(gpi0t)
        pi4(3)=pien*cos(gpi0t)
c
       elseif(ip.eq.2) then
c      real gamma
c
        if(gngam.eq.0) print *,'Bad gamma'
        pien=ggame
        pi4(4)=pien
        pi4(1)=pien*cos(ggamf)*sin(ggamt) 
        pi4(2)=pien*sin(ggamf)*sin(ggamt)
        pi4(3)=pien*cos(ggamt)
c

       elseif(ip.eq.4) then
c      proton
c
        if(gnpro.eq.0) print *,'Bad pro'
        pien=sqrt(pien*pien-amp*amp)
        pi4(4)=gproe
        pi4(1)=pien*cos(gprof)*sin(gprot) 
        pi4(2)=pien*sin(gprof)*sin(gprot)
        pi4(3)=pien*cos(gprot)
c
c
       elseif(ip.eq.11) then
c      K+ 
c
        if(gnkap.eq.0) print *,'Bad K+',gnneu
        pien=sqrt(gkape*gkape-0.244)
        pi4(4)=gkape
        pi4(1)=pien*cos(gkapf)*sin(gkapt) 
        pi4(2)=pien*sin(gkapf)*sin(gkapt)
        pi4(3)=pien*cos(gkapt)
       endif

c       Now el and gamma
c
c
c       el0
c
        el04(1)=0
        el04(2)=0
        el04(3)=ebeam
        el04(4)=ebeam
        elf4(1)=gelee*sin(gelet)*cos(gelef)
        elf4(2)=gelee*sin(gelet)*sin(gelef)
        elf4(3)=gelee*cos(gelet)
        elf4(4)=gelee
c
c     gamma*
c
c proton
c
       pro4(4)=amp
       pro4(3)=0
       pro4(2)=0
       pro4(1)=0
c
c
         call vdifm(el04,elf4,qiu4,4)
c
         call vsumm(qiu4,pro4,tnorm,4)
         call vdifm(tnorm,pi4,tnorm2,4)
         gmismasrho=vmass(tnorm2)
         return
         end

c
c        fill radgen
c
       subroutine fillradgen()
       implicit none
#include "names.inc"
#include "ntupgkoko.inc"
#include "claslepto.inc"
#include "tailcom.inc"
c       radiative stuff
c
        real VPGEN(4),VPRAD(4),PhRAD(4),q2tr,utr,weight
     6,e_beam_energy,ggamp
        real PolarTheta,Azimuthalphi
        integer j
c
c define the generated g*
c
        print *,'in-radgen eprim,thet,phi',gelee,gelet,gelef
        VPGEN(1)=-gelee*sin(gelet)*cos(gelef)
        VPGEN(2)=-gelee*sin(gelet)*sin(gelef)
        VPGEN(3)=-gelee*cos(gelet)
c23456789012345678901234567890123456789012345678901234567890123456789012
       e_beam_energy=cl_beam_energy
       VPGEN(4)=e_beam_energy-sqrt(VPGEN(1)**2+VPGEN(2)**2+VPGEN(3)**2)
       VPGEN(3)=VPGEN(3)+e_beam_energy
 	call RADGEN(e_beam_energy,VPGEN,VPRAD,PhRAD,q2tr,utr,weight)
c
c insert the photon
c
         ggamp=PhRAD(1)**2+PhRAD(2)**2+PhRAD(3)**2
c
         if(ggamp.gt.0) then
           gradt=PolarTheta(PhRAD(1),PhRAD(2),PhRAD(3))
           gradf=Azimuthalphi(PhRAD(1),PhRAD(2))
           grade=PhRAD(4)
           radq2=q2tr
           radnu=utr
          else 
           gradt=0
           gradf=0
           grade=0
         endif   
           radweight=weight 
           iproctype=ita
c
c       also add in lund
c
         if(ggamp.gt.0) then
           N=N+1                ! add gamma
           k(N,1)=0             ! 1-for final particles
           k(N,2)=22            ! lund-ID for photon 
           k(N,3)=0             ! no parent 
           k(N,4)=0             ! no doughter 
           do j=1,3
             P(N,j)=PhRAD(j)
             V(N,j)=0.0
           enddo
             P(N,4)=PhRAD(4)
             P(N,5)=0.0
         endif
c
       return 
       end
c
c
       subroutine vsumm(a,b,c,n)
       implicit none
       real a(n),b(n),c(n)
       integer i,n
        do i=1,n
          c(i)=a(i)+b(i)
        enddo
       return
       end
c
       subroutine vdifm(a,b,c,n)
       implicit none
       real a(n),b(n),c(n)
       integer i,n
        do i=1,n
          c(i)=a(i)-b(i)
        enddo
       return
       end
c
c
       real function vdotm(a,b,n)
       implicit none
       real a(n),b(n),s
       integer i,n
       s=0.0
       do i=1,3
         s=s+a(i)*b(i)
       enddo
       if(n.eq.4) s=s-a(n)*b(n)
       vdotm=s
       return
       end
c   
       real function vangle(a,b,c,d)
       implicit none
       real a(3),b(3),c(3),d(3),xm,ym,vcos,vdotm
       real x(3),y(3),pi
       pi=acos(-1.0)
       call crossm(a,b,x)
       call crossm(c,d,y)
       xm=vdotm(x,x,3)
       ym=vdotm(y,y,3)
       if(xm.gt.0.0 .and. ym.gt.0.0) then
         vcos=vdotm(x,y,3)/sqrt(xm)/sqrt(ym)
         if(abs(vcos).lt.1.0) then
            vangle=acos(vcos)
         else
            if(vcos.ge.1.0)  vangle=0
            if(vcos.le.-1.0)  vangle=pi
         endif 
       else
         vangle=0
       endif
       return
       end
c
       subroutine crossm(a,b,c)
       implicit none
       real a(3),b(3),c(3)
       c(1)=a(2)*b(3)-a(3)*b(2)
       c(2)=a(3)*b(1)-a(1)*b(3)
       c(3)=a(1)*b(2)-a(2)*b(1)
       return
       end
c
c
       real function vmass(a)
       implicit none
       real vm,vdotm,a(4)
        vm= vdotm(a,a,4)
        if (vm.lt.0.0) then
          vmass=sqrt(-vm)
        else
          vmass=-1.0
        endif 
       return
       end



      integer function length(str)
c     return the string length without the blanks characters

      implicit integer (k-l)
      character *(*) str    
      
      lmax=len(str)      
       
c     search the last non blank character
      do i=lmax,1,-1
      if(str(i:i).ne.' ')then
      length=i
      return
      end if
      end do

      length=lmax

      return
      end     

      real function valnum(str)
c     return the real value contained into a string

      implicit integer (k-l)
      character *(*) str
      logical segno
      

      segno=.false.
      valnum=0.00
      lu=length(str)
      
c     check the number sign
      if(str(1:1).eq.'-')then
      segno=.true.
      str=str(2:lu)
      lu=lu-1
      end if

c     check if number is float or integer
      if(index(str,'.').ne.0)then
      iin=index(str,'.')-1
      else
      iin=lu
      end if

      ifr=lu-(iin+1)

c     translate the integer portion
      doi=1,iin  
      k=ichar(str(i:i))-48
      valnum=valnum+float(k)*10.00**float(iin-i)
      end do         

      if(iin.eq.lu)goto 10
      str=str(iin+2:lu)

c     translate the decimal portion
      doi=1,ifr  
      k=ichar(str(i:i))-48
      valnum=valnum+float(k)/10.00**float(i)
      end do

10    if(segno)valnum=-valnum

      return
      end     


      SUBROUTINE MYRNORML2(rran,lrun)
      integer lrun
      real rran(lrun)
c      call rnorml(rran,lrun)  ! from Mathlib
      call MYRNORML(rran,lrun)
      return
      end
******************************************************************
      SUBROUTINE MYRNORML(rran,lrun)
      integer lrun
      real rran(lrun)
      do i=1,lrun
           rran(i)=ZBQLNOR(0.0,1.0)
      enddo
      return
      end
c

      REAL FUNCTION ZBQLNOR(MU,SIGMA)
*
*       Returns a random number Normally distributed with mean
*       MU and standard deviation |SIGMA|, using the Box-Muller
*       algorithm
*
      DOUBLE PRECISION THETA,R,PI
      REAL ZBQLU01,MU,SIGMA
      DOUBLE PRECISION SPARE
      INTEGER STATUS
      SAVE STATUS,SPARE,PI
      DATA STATUS /-1/

      IF (STATUS.EQ.-1) PI = 4.0D0*DATAN(1.0D0)

      IF (STATUS.LE.0) THEN
       THETA = 2.0D0*PI*ZBQLU01(0.0)
       R = DSQRT( -2.0D0*DLOG(1.0D0*ZBQLU01(0.0)) )
       ZBQLNOR = (R*DCOS(THETA))
       SPARE = (R*DSIN(THETA))
       STATUS = 1
      ELSE
       ZBQLNOR = SPARE
       STATUS = 0
      ENDIF
      
      ZBQLNOR = MU + (SIGMA*ZBQLNOR)

      END
c
      REAL FUNCTION ZBQLU01(DUMMY)
      ZBQLU01=rand() !rndm(-1)
      return
      end 
******************************************************************
       FUNCTION ZBQLU02(DUMMY)
*
*       Returns a uniform random number between 0 & 1, using
*       a Marsaglia-Zaman type subtract-with-borrow generator.
*       Uses double precision, rather than integer, arithmetic 
*       throughout because MZ's integer constants overflow
*       32-bit integer storage (which goes from -2^31 to 2^31).
*       Ideally, we would explicitly truncate all integer 
*       quantities at each stage to ensure that the double
*       precision representations do not accumulate approximation
*       error; however, on some machines the use of DNINT to
*       accomplish this is *seriously* slow (run-time increased
*       by a factor of about 3). This double precision version 
*       has been tested against an integer implementation that
*       uses long integers (non-standard and, again, slow) -
*       the output was identical up to the 16th decimal place
*       after 10^10 calls, so we're probably OK ...
*
      DOUBLE PRECISION B,C,ZBQLIX(43),X,B2,BINV
      REAL ZBQLU01,DUMMY
      INTEGER CURPOS,ID22,ID43

      COMMON /ZBQL0001/ ZBQLIX,B,C
      SAVE /ZBQL0001/
      SAVE CURPOS,ID22,ID43
      DATA CURPOS,ID22,ID43 /1,22,43/
      print *,'now get the rndm--ZBQLU01',DUMMY
      B2 = B
      BINV = 1.0D0/B
 5    X = ZBQLIX(ID22) - ZBQLIX(ID43) - C
      IF (X.LT.0.0D0) THEN
       X = X + B
       C = 1.0D0
      ELSE
       C = 0.0D0
      ENDIF
      ZBQLIX(ID43) = X
*
*     Update array pointers. Do explicit check for bounds of each to
*     avoid expense of modular arithmetic. If one of them is 0 the others
*     won't be
*
      CURPOS = CURPOS - 1
      ID22 = ID22 - 1
      ID43 = ID43 - 1
      IF (CURPOS.EQ.0) THEN
       CURPOS=43
      ELSEIF (ID22.EQ.0) THEN
       ID22 = 43
      ELSEIF (ID43.EQ.0) THEN
       ID43 = 43
      ENDIF
*
*     The integer arithmetic there can yield X=0, which can cause 
*     problems in subsequent routines (e.g. ZBQLEXP). The problem
*     is simply that X is discrete whereas U is supposed to 
*     be continuous - hence if X is 0, go back and generate another
*     X and return X/B^2 (etc.), which will be uniform on (0,1/B). 
*
      IF (X.LT.BINV) THEN
       B2 = B2*B
       GOTO 5
      ENDIF

      ZBQLU01 = X/B2
      print *,'end ZBQLU01',x,b2
      END
******************************************************************
      FUNCTION ZBQLUAB(A,B)
*
*       Returns a random number uniformly distributed on (A,B)
*
      REAL ZBQLU01,ZBQLUAB,A,B
*
*       Even if A > B, this will work as B-A will then be -ve
*
      IF (A.NE.B) THEN
       ZBQLUAB = A + ( (B-A)*ZBQLU01(0.0) )
      ELSE
       ZBQLUAB = A
       WRITE(*,1)
      ENDIF
 1    FORMAT(/5X,'****WARNING**** (function ZBQLUAB) Upper and ',
     +'lower limits on uniform',/5X,'distribution are identical',/)
      END
******************************************************************
      SUBROUTINE RZERO(xarr,N)
      INTEGER N,i
      real xarr(N)
      do i=1,N
      xarr(i)=0.0
      enddo
      return
      end    
c  
      SUBROUTINE VZERO(xarr,N)
      INTEGER N,i,xarr(N)
      do i=1,N
      xarr(i)=0
      enddo
      return
      end

c
       real function vdotmn(a,b,n)
       implicit none
       real a(4),b(4),s,s1,s2
       integer i,n
       s=0.0
       s1=0.0
       s2=0.0
       vdotmn=0.0
       do i=1,3
         s=s+a(i)*b(i)
         s1=s1+a(i)*a(i)
         s2=s2+b(i)*b(i)
       enddo
       if(n.eq.4) s=s-a(n)*b(n)
       if(s1.gt.0.0.and.s2.gt.0.0) vdotmn=s/sqrt(s1*s2)
       return
       end


