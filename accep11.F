      integer function accep11(pid,p,t,f,torus)
      implicit none
       real torus 
       real newphi11,e,p,f,t,torcur,pi,phis,phis0,thetad,d_phi,weight
       integer pid
       pi=acos(-1.0)
       torcur=torus
       phis0=newphi11(f*180.0/pi)
       phis=phis0
       thetad=t*180.0/pi
c
        call clas_at12g(pid,p,thetad,phis,torcur,d_phi,weight)
        t=thetad*pi/180.0
        f=f+(phis-phis0)*pi/180.0
        
c       print *,thetad,gelef,phis,weight
       accep11=weight
       end
*     
C
      real Function newphi11(phi)
      real phi,phinew
      if (phi.gt.330.) then
        phinew = phi-360.
      elseif (phi.ge.0.0.and.phi.le.30.) then
        phinew = phi
      elseif (phi.gt.30.0.and.phi.le.90.) then
        phinew = phi-60.
      elseif (phi.gt.90.0.and.phi.le.150.) then
        phinew = phi-120.
      elseif (phi.gt.150.0.and.phi.le.210.) then
        phinew = phi-180.
      elseif (phi.gt.210.0.and.phi.le.270.) then
        phinew = phi-240.
      elseif (phi.gt.270.0.and.phi.le.330.) then
        phinew = phi-300.
      endif
c       print *,phi,phinew
      newphi11=phinew
      end  
