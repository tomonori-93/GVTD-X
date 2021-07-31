module sub_mod

contains

subroutine prod_vortex_structure( r, t, rmax, vmax, c1u, c2u,  &
  &                               Vt, Vr, Vt_pert, Vr_pert, Vt_pert_ang, Vr_pert_ang,  &
  &                               ropt, Vt_0, Vr_0 )
!-- Producing vortex structure with rmax, vmax, and umax
  implicit none
  double precision, intent(in) :: r(:)  ! radius [m]
  double precision, intent(in) :: t(:)  ! azimuthal angle [rad]
  double precision, intent(in) :: rmax  ! radius of maximum tangential wind speed [m]
  double precision, intent(in) :: vmax  ! maximum tangential wind speed [m s-1]
  double precision, intent(in) :: c1u   ! coefficient 1 for radial wind [s-1]
  double precision, intent(in) :: c2u   ! coefficient 2 for radial wind [s-1]
  double precision, intent(out) :: Vt(size(r),size(t))  ! Profile of tangential wind
  double precision, intent(out) :: Vr(size(r),size(t))  ! Profile of radial wind
  double precision, intent(in), optional :: Vt_pert(:)  ! perturbations of tangential wind [m s-1]
  double precision, intent(in), optional :: Vr_pert(:)  ! perturbations of radial wind [m s-1]
  double precision, intent(in), optional :: Vt_pert_ang(size(Vt_pert))  ! angles of tangential wind [rad]
  double precision, intent(in), optional :: Vr_pert_ang(size(Vr_pert))  ! angles of radial wind [rad]
  logical, intent(in), optional :: ropt  ! option for radial variation of perturbation Vt and Vr
  double precision, intent(out), optional :: Vt_0(size(r))  ! Radial profile of axisymmetric Vt [m s-1]
  double precision, intent(out), optional :: Vr_0(size(r))  ! Radial profile of axisymmetric Vr [m s-1]

  integer :: nr, nt, i, j, k, nvtp, nvrp, nvpmax
  double precision :: tmp_vtp1, tmp_vrp1, tmp_vtp2n, tmp_vrp2n, rad_coef, radp_coef, lin_coef, dr
  double precision, allocatable, dimension(:,:) :: zetap
  double precision, allocatable, dimension(:,:,:) :: gkrr, dgkrr
  logical rad_opt

  nr=size(r)
  nt=size(t)
  nvtp=size(Vt_pert)
  nvrp=size(Vr_pert)
  nvpmax=max(nvtp,nvrp)
  dr=r(2)-r(1)

  rad_opt=.false.
  if(present(ropt))then
     rad_opt=ropt
     if(nvpmax>0)then
        allocate(gkrr(nvpmax,nr+1,nr+1))
        allocate(dgkrr(nvpmax,nr+1,nr+1))
        allocate(zetap(nvpmax,nr+1))
        gkrr=0.0d0
        dgkrr=0.0d0
        zetap=0.0d0
        do k=1,nvpmax
           do j=1,nr
              do i=1,nr
                 if(r(i)-0.5d0*dr<0.0d0)then
                    gkrr(k,i,j)=green_func( 0.0d0, r(j), k )
                 else
                    gkrr(k,i,j)=green_func( r(i)-0.5d0*dr, r(j), k )
                 end if
              end do
           end do
           do j=1,nr
              gkrr(k,nr+1,j)=green_func( r(nr)+0.5d0*dr, r(j), k )
           end do
           do i=1,nr
              gkrr(k,i,nr+1)=green_func( r(i)+0.5d0*dr, r(nr)+dr, k )
           end do
           do j=1,nr
              do i=1,nr+1
                 dgkrr(k,i,j)=(gkrr(k,i,j+1)-gkrr(k,i,j))/dr
              end do
           end do
           do i=1,nr
!              if(r(i)<2.0d0*rmax)then
              if(r(i)>=0.1d0*rmax.and.r(i)<2.0d0*rmax)then
                 zetap(k,i)=abs(Vt_pert(k))/rmax
              end if
           end do
        end do
     end if
  end if

  do j=1,nt
     do i=1,nr
        tmp_vtp1=0.0d0
        tmp_vrp1=0.0d0
        tmp_vtp2n=0.0d0
        tmp_vrp2n=0.0d0
        if(present(Vt_pert))then
           tmp_vtp1=Vt_pert(1)*dcos((t(j)+Vt_pert_ang(1)))
           if(nvtp>1)then
              tmp_vtp1=0.0d0
!              do k=2,nvtp
              do k=1,nvtp
                 lin_coef=line_integral( nr-1, r, dgkrr(k,1:nr,i), zetap(k,1:nr) )*dr
                 tmp_vtp2n=tmp_vtp2n+(-lin_coef*dcos(dble(k)*(t(j)+Vt_pert_ang(k))))
              end do
           end if
        end if
        if(present(Vr_pert))then
           tmp_vrp1=Vr_pert(1)*dcos((t(j)+Vr_pert_ang(1)))
           if(nvrp>1)then
              tmp_vrp1=0.0d0
!              do k=2,nvrp
              do k=1,nvrp
                 lin_coef=line_integral( nr-1, r, gkrr(k,1:nr,i), zetap(k,1:nr) )*dr
                 tmp_vrp2n=tmp_vrp2n+(-dble(k)*lin_coef*dsin(dble(k)*(t(j)+Vt_pert_ang(k)))/r(i))  ! same as Vt_pert_ang
!ORG                 tmp_vrp2n=tmp_vrp2n+Vr_pert(k)*(-lin_coef*dsin(dble(k)*(t(j)+Vt_pert_ang(k)))/r(i))  ! same as Vt_pert_ang
              end do
           end if
        end if

        if(r(i)<=rmax)then
           rad_coef=r(i)/rmax
           radp_coef=r(i)/rmax
           Vt(i,j)=vmax*rad_coef
           Vr(i,j)=c1u*dsqrt((rmax-r(i))*r(i))
           Vr(i,j)=Vr(i,j)*4.0*1.0e-3  ! 多分これが必要 [m] -> [km]
        else
           rad_coef=rmax/r(i)
           if((r(i)>rmax).and.(r(i)<=1.5d0*rmax))then
              radp_coef=1.0d0
           else
              radp_coef=rmax/r(i)
           end if
           Vt(i,j)=vmax*rad_coef
           Vr(i,j)=-c2u*dsqrt(r(i)-rmax)*(rmax/r(i))
           Vr(i,j)=Vr(i,j)/dsqrt(1000.0d0)  ! 多分これが必要 [m] -> [km]
        end if
        if(j==1)then
           if(present(Vt_0))then
              Vt_0(i)=Vt(i,j)
           end if
           if(present(Vr_0))then
              Vr_0(i)=Vr(i,j)
           end if
        end if
        if(rad_opt.eqv..true.)then
           Vt(i,j)=Vt(i,j)+tmp_vtp1*radp_coef+tmp_vtp2n
           Vr(i,j)=Vr(i,j)+tmp_vrp1*radp_coef+tmp_vrp2n
        else
           Vt(i,j)=Vt(i,j)+tmp_vtp1+tmp_vtp2n
           Vr(i,j)=Vr(i,j)+tmp_vrp1+tmp_vrp2n
        end if
     end do
  end do

  if(present(ropt))then
     if(nvpmax>0)then
        deallocate(gkrr)
        deallocate(dgkrr)
        deallocate(zetap)
     end if
  end if

end subroutine prod_vortex_structure

subroutine prod_radar_along_vel()
  implicit none

end subroutine prod_radar_along_vel

subroutine conv_VtVr2VxVy( r, t, Vt, Vr, Vx, Vy )
!-- convert Vt and Vr to Vx and Vy on R-T coordinates
  implicit none
  double precision, intent(in) :: r(:)  ! R-coordinate [m]
  double precision, intent(in) :: t(:)  ! T-coordinate [rad]
  double precision, intent(in) :: Vt(size(r),size(t))   ! tangential wind component on R-T coordinates
  double precision, intent(in) :: Vr(size(r),size(t))   ! radial wind component on R-T coordinates
  double precision, intent(out) :: Vx(size(r),size(t))  ! X-component of wind on R-T coordinates
  double precision, intent(out) :: Vy(size(r),size(t))  ! Y-component of wind on R-T coordinates
  integer :: nr, nt, i, j

  nr=size(r)
  nt=size(t)

  do j=1,nt
     do i=1,nr
        Vx(i,j)=Vr(i,j)*dcos(t(j))-Vt(i,j)*dsin(t(j))
        Vy(i,j)=Vr(i,j)*dsin(t(j))+Vt(i,j)*dcos(t(j))
     end do
  end do

end subroutine conv_VtVr2VxVy

subroutine conv_VxVy2VtVr( r, t, Vx, Vy, Vt, Vr )
!-- convert Vx and Vy to Vr and Vt on R-T coordinates
  implicit none
  double precision, intent(in) :: r(:)  ! R-coordinate [m]
  double precision, intent(in) :: t(:)  ! T-coordinate [rad]
  double precision, intent(in) :: Vx(size(r),size(t))  ! X-component of wind on R-T coordinates
  double precision, intent(in) :: Vy(size(r),size(t))  ! Y-component of wind on R-T coordinates
  double precision, intent(out) :: Vt(size(r),size(t))   ! tangential wind component on R-T coordinates
  double precision, intent(out) :: Vr(size(r),size(t))   ! radial wind component on R-T coordinates
  integer :: nr, nt, i, j

  nr=size(r)
  nt=size(t)

  do j=1,nt
     do i=1,nr
        Vr(i,j)=Vx(i,j)*dcos(t(j))+Vy(i,j)*dsin(t(j))
        Vt(i,j)=-Vx(i,j)*dsin(t(j))+Vy(i,j)*dcos(t(j))
     end do
  end do

end subroutine conv_VxVy2VtVr

subroutine proj_VxVy2Vraxy( x, y, rax, ray, Vx, Vy, Vraxy, undef )
!-- calculate Vx and Vy to Vd along with radar beams on X-Y coodinates
  implicit none
  double precision, intent(in) :: x(:)  ! X-coordinate
  double precision, intent(in) :: y(:)  ! Y-coordinate
  double precision, intent(in) :: rax   ! X-coodinate of radar location
  double precision, intent(in) :: ray   ! Y-coodinate of radar location
  double precision, intent(in) :: Vx(size(x),size(y))  ! X-component of wind on X-Y coordinates
  double precision, intent(in) :: Vy(size(x),size(y))  ! Y-component of wind on X-Y coordinates
  double precision, intent(out) :: Vraxy(size(x),size(y))  ! velocity along with beam on X-Y coordinates
  double precision, intent(in), optional :: undef
  integer :: nx, ny, i, j
  double precision :: rad

  nx=size(x)
  ny=size(y)

  if(present(undef))then
     Vraxy=undef
     do j=1,ny
        do i=1,nx
           rad=dsqrt((x(i)-rax)**2+(y(j)-ray)**2)
           if(rad>0.0d0)then
              if(Vx(i,j)/=undef.and.Vy(i,j)/=undef)then
                 Vraxy(i,j)=((x(i)-rax)/rad)*Vx(i,j)+((y(j)-ray)/rad)*Vy(i,j)
              end if
           end if
        end do
     end do
  else
     Vraxy=0.0d0
     do j=1,ny
        do i=1,nx
           rad=dsqrt((x(i)-rax)**2+(y(j)-ray)**2)
           if(rad>0.0d0)then
              Vraxy(i,j)=((x(i)-rax)/rad)*Vx(i,j)+((y(j)-ray)/rad)*Vy(i,j)
           end if
        end do
     end do
  end if

end subroutine proj_VxVy2Vraxy

subroutine proj_VtVr2Vrart( r, t, td, Vt, Vr, Vra, undef )
!-- convert Vt and Vr to Vx and Vy on R-T coordinates
  implicit none
  double precision, intent(in) :: r(:)  ! R-coordinate [m]
  double precision, intent(in) :: t(:)  ! T-coordinate [rad]
  double precision, intent(in) :: td(size(r),size(t))   ! radar azimuthal angle on R-T coordinate [rad]
  double precision, intent(in) :: Vt(size(r),size(t))   ! tangential wind component on R-T coordinates
  double precision, intent(in) :: Vr(size(r),size(t))   ! radial wind component on R-T coordinates
  double precision, intent(out) :: Vra(size(r),size(t))  ! Velocity along beam on R-T coordinates
  double precision, intent(in), optional :: undef  ! undefined value
  integer :: nr, nt, i, j

  nr=size(r)
  nt=size(t)

  do j=1,nt
     do i=1,nr
        Vra(i,j)=-Vt(i,j)*dsin(t(j)-td(i,j))+Vr(i,j)*dcos(t(j)-td(i,j))
     end do
  end do

end subroutine proj_VtVr2Vrart

double precision function line_integral( nr, rdh, gkrr, div_r, undef )
!-- calculate a line integral (actually, sum for arguments)
  implicit none
  integer, intent(in) :: nr
  double precision, intent(in) :: rdh(nr+1)
  double precision, intent(in) :: gkrr(nr+1)
  double precision, intent(in) :: div_r(nr+1)
  double precision, intent(in), optional :: undef
  integer :: ii, jj
  double precision :: tmpval
  double precision :: vareps(nr+1)

  tmpval=0.0d0
  vareps=1.0d0
  vareps(1)=0.5d0
  vareps(nr+1)=0.5d0

  do ii=1,nr+1
     tmpval=tmpval+vareps(ii)*rdh(ii)*gkrr(ii)*div_r(ii)
  end do

  line_integral=tmpval

  return

end function line_integral

!subroutine prod_VM( x, y, us, vs, Usx, Vsy )
!!-- produce environmental wind on X-Y coordinates
!  implicit none
!  double precision, intent(in) :: x(:)  ! X-coordinate
!  double precision, intent(in) :: y(:)  ! Y-coordinate
!  double precision, intent(in) :: um0   ! coefficient for X-component of environmental wind
!  double precision, intent(in) :: vm0   ! coefficient for Y-component of environmental wind
!  double precision, intent(in) :: um1   ! coefficient 1 of environmental wind
!  double precision, intent(in) :: um2   ! coefficient 2 of environmental wind
!  double precision, intent(in) :: vm1   ! coefficient 3 of environmental wind
!  double precision, intent(out) :: Um(size(x),size(y))  ! X-component of environmental wind
!  double precision, intent(out) :: Vm(size(x),size(y))  ! Y-component of environmental wind
!  integer :: nx, ny, i, j
!
!  nx=size(x)
!  ny=size(y)
!
!  do j=1,ny
!     do i=1,nx
!        Um(i,j)=um0+um1*x(i)+um2*y(j)
!        Vm(i,j)=vm0+vm1*x(i)-um1*y(j)
!     end do
!  end do
!
!end subroutine prod_VM

!subroutine norm_VxVy2Vrnxy( x, y, rax, ray, Vx, Vy, Vrnxy, undef )
!!-- calculate Vx and Vy to Vn normal to radar beams on X-Y coodinates
!  implicit none
!  double precision, intent(in) :: x(:)  ! X-coordinate
!  double precision, intent(in) :: y(:)  ! Y-coordinate
!  double precision, intent(in) :: rax   ! X-coodinate of radar location
!  double precision, intent(in) :: ray   ! Y-coodinate of radar location
!  double precision, intent(in) :: Vx(size(x),size(y))  ! X-component of wind on X-Y coordinates
!  double precision, intent(in) :: Vy(size(x),size(y))  ! Y-component of wind on X-Y coordinates
!  double precision, intent(out) :: Vrnxy(size(x),size(y))  ! velocity normal to beam on X-Y coordinates
!  double precision, intent(in), optional :: undef
!  integer :: nx, ny, i, j
!  double precision :: rad
!
!  nx=size(x)
!  ny=size(y)
!
!  if(present(undef))then
!     Vrnxy=undef
!     do j=1,ny
!        do i=1,nx
!           rad=dsqrt((x(i)-rax)**2+(y(j)-ray)**2)
!           if(rad>0.0d0)then
!              if(Vx(i,j)/=undef.and.Vy(i,j)/=undef)then
!                 Vrnxy(i,j)=((x(i)-rax)/rad)*Vy(i,j)-((y(j)-ray)/rad)*Vx(i,j)
!              end if
!           end if
!        end do
!     end do
!  else
!     Vrnxy=0.0d0
!     do j=1,ny
!        do i=1,nx
!           rad=dsqrt((x(i)-rax)**2+(y(j)-ray)**2)
!           if(rad>0.0d0)then
!              Vrnxy(i,j)=((x(i)-rax)/rad)*Vy(i,j)-((y(j)-ray)/rad)*Vx(i,j)
!           end if
!        end do
!     end do
!  end if
!
!end subroutine norm_VxVy2Vrnxy

double precision function green_func( rc, r, nval )
!-- def: -(1/(2*nval))*(r/rc)^nval, r<rc
!-- def: -(1/(2*nval))*(rc/r)^nval, r>=rc
  implicit none
  double precision, intent(in) :: rc
  double precision, intent(in) :: r
  integer, intent(in) :: nval
  double precision :: res, nr

  nr=dble(nval)
  if(rc<0.0d0.or.r<0.0d0)then
     call stdout( "rc and r must not be negative", "green_func", 1 )
     write(*,*) "rc and r = ", rc, r
  end if
  if(r==rc)then
     res=-(0.5d0/nr)
  else
     if(r<rc)then
        res=-(0.5d0/nr)*((r/rc)**nval)
     else  ! (r/rc)**nval == exp(nval*(log(r/rc)))
        if(rc==0.0d0)then
           res=0.0d0
        else
           res=-(0.5d0/nr)*exp(dble(nval)*(log(rc)-log(r)))
        end if
     end if
  end if

  green_func=res

  return

end function green_func

subroutine div_curl_2d( r, t, ur, vt, divr, curl )
!-- def: drv_r/rdr + dvt/rdt  (divr)
!-- def: drv_t/rdr - dvr/rdt  (curl)
  implicit none
  double precision, intent(in) :: r(:)  ! radius [m]
  double precision, intent(in) :: t(:)  ! angle [rad]
  double precision, intent(in) :: ur(size(r),size(t))  ! Ur [m/s]
  double precision, intent(in) :: vt(size(r),size(t))  ! Vt [m/s]
  double precision, intent(out) :: divr(size(r),size(t))  ! divergence [1/s]
  double precision, intent(out) :: curl(size(r),size(t))  ! rotation [1/s]
  integer :: ii, jj, ni, nj
  double precision :: dr, dt

  ni=size(r)
  nj=size(t)
  dr=r(2)-r(1)
  dt=t(2)-t(1)
  divr=0.0d0
  curl=0.0d0

  do jj=2,nj-1
     do ii=2,ni-1
        divr(ii,jj)=0.5d0*(ur(ii+1,jj)-ur(ii-1,jj))/dr  &
  &                +ur(ii,jj)/r(ii)  &
  &                +0.5d0*(vt(ii,jj+1)-vt(ii,jj-1))/(r(ii)*dt)
        curl(ii,jj)=0.5d0*(vt(ii+1,jj)-vt(ii-1,jj))/dr  &
  &                +vt(ii,jj)/r(ii)  &
  &                -0.5d0*(ur(ii,jj+1)-ur(ii,jj-1))/(r(ii)*dt)
     end do
  end do

  if(r(1)>0.0d0)then
     do jj=2,nj-1
        divr(1,jj)=(ur(2,jj)-ur(1,jj))/dr  &
  &                +ur(1,jj)/r(1)  &
  &                +0.5d0*(vt(1,jj+1)-vt(1,jj-1))/(r(1)*dt)
        curl(1,jj)=(vt(2,jj)-vt(1,jj))/dr  &
  &                +vt(1,jj)/r(1)  &
  &                -0.d50*(ur(1,jj+1)-ur(1,jj-1))/(r(1)*dt)
        divr(ni,jj)=(ur(ni,jj)-ur(ni-1,jj))/dr  &
  &                +ur(ni,jj)/r(ni)  &
  &                +0.5d0*(vt(ni,jj+1)-vt(ni,jj-1))/(r(ni)*dt)
        curl(ni,jj)=(vt(ni,jj)-vt(ni-1,jj))/dr  &
  &                +vt(ni,jj)/r(ni)  &
  &                -0.5d0*(ur(ni,jj+1)-ur(ni,jj-1))/(r(ni)*dt)
     end do
  else
     do jj=2,nj-1
        divr(ni,jj)=(ur(ni,jj)-ur(ni-1,jj))/dr  &
  &                +ur(ni,jj)/r(ni)  &
  &                +0.5d0*(vt(ni,jj+1)-vt(ni,jj-1))/(r(ni)*dt)
        curl(ni,jj)=(vt(ni,jj)-vt(ni-1,jj))/dr  &
  &                +vt(ni,jj)/r(ni)  &
  &                -0.5d0*(ur(ni,jj+1)-ur(ni,jj-1))/(r(ni)*dt)
     end do
  end if

  do ii=2,ni-1
     divr(ii,1)=0.5d0*(ur(ii+1,1)-ur(ii-1,1))/dr  &
  &             +ur(ii,1)/r(ii)  &
  &             +(vt(ii,2)-vt(ii,1))/(r(ii)*dt)
     curl(ii,1)=0.5d0*(vt(ii+1,1)-vt(ii-1,1))/dr  &
  &             +vt(ii,1)/r(ii)  &
  &             -(ur(ii,2)-ur(ii,1))/(r(ii)*dt)
     divr(ii,nj)=0.5d0*(ur(ii+1,nj)-ur(ii-1,nj))/dr  &
  &             +ur(ii,nj)/r(ii)  &
  &             +(vt(ii,nj)-vt(ii,nj-1))/(r(ii)*dt)
     curl(ii,nj)=0.5d0*(vt(ii+1,nj)-vt(ii-1,nj))/dr  &
  &             +vt(ii,nj)/r(ii)  &
  &             -(ur(ii,nj)-ur(ii,nj-1))/(r(ii)*dt)
  end do

  if(r(1)>0.0d0)then
     divr(1,1)=(ur(2,1)-ur(1,1))/dr  &
  &           +ur(1,1)/r(1)  &
  &           +(vt(1,2)-vt(1,1))/(r(1)*dt)
     curl(1,1)=(vt(2,1)-vt(1,1))/dr  &
  &           +vt(1,1)/r(1)  &
  &           -(ur(1,2)-ur(1,1))/(r(1)*dt)
     divr(1,nj)=(ur(2,nj)-ur(1,nj))/dr  &
  &           +ur(1,nj)/r(1)  &
  &           +(vt(1,nj)-vt(1,nj-1))/(r(1)*dt)
     curl(1,nj)=(vt(2,nj)-vt(1,nj))/dr  &
  &           +vt(1,nj)/r(1)  &
  &           -(ur(1,nj)-ur(1,nj-1))/(r(1)*dt)
  end if

  divr(ni,1)=(ur(ni,1)-ur(ni-1,1))/dr  &
  &          +ur(ni,1)/r(ni)  &
  &          +(vt(ni,2)-vt(ni,1))/(r(ni)*dt)
  curl(ni,1)=(vt(ni,1)-vt(ni-1,1))/dr  &
  &          +vt(ni,1)/r(ni)  &
  &          -(ur(ni,2)-ur(ni,1))/(r(ni)*dt)
  divr(ni,nj)=(ur(ni,nj)-ur(ni-1,nj))/dr  &
  &          +ur(ni,nj)/r(ni)  &
  &          +(vt(ni,nj)-vt(ni,nj-1))/(r(ni)*dt)
  curl(ni,nj)=(vt(ni,nj)-vt(ni-1,nj))/dr  &
  &          +vt(ni,nj)/r(ni)  &
  &          -(ur(ni,nj)-ur(ni,nj-1))/(r(ni)*dt)

end subroutine div_curl_2d

subroutine conv_d2r_1d( ival, oval )
!-- convert double to real
  implicit none
  double precision, intent(in) :: ival(:)
  real, intent(out) :: oval(size(ival))
  integer :: ii, ni

  ni=size(ival)

  do ii=1,ni
     oval(ii)=real(ival(ii))
  end do

end subroutine conv_d2r_1d

subroutine conv_d2r_2d( ival, oval )
!-- convert double to real
  implicit none
  double precision, intent(in) :: ival(:,:)
  real, intent(out) :: oval(size(ival,1),size(ival,2))
  integer :: ii, jj, ni, nj

  ni=size(ival,1)
  nj=size(ival,2)

  do jj=1,nj
     do ii=1,ni
        oval(ii,jj)=real(ival(ii,jj))
     end do
  end do

end subroutine conv_d2r_2d

subroutine sum_1d( val, res, undef )
!-- calculation of sum for 1D variable
  implicit none
  double precision, intent(in) :: val(:)  ! input
  double precision, intent(out) :: res  ! output
  double precision, intent(in), optional :: undef
  integer :: ii, ni, icount

  ni=size(val)
  icount=0
  res=0.0d0

  if(present(undef))then
     do ii=1,ni
        if(val(ii)/=undef)then
           res=res+val(ii)
           icount=icount+1
        end if
     end do
     if(icount>0)then
        res=res/dble(icount)
     else
        res=undef
     end if
  else
     do ii=1,ni
        res=res+val(ii)
     end do
     res=res/dble(ni)
  end if

end subroutine sum_1d

subroutine add_2d( ioval, ival, undef )
!-- add ival
  implicit none
  double precision, intent(inout) :: ioval(:,:)
  double precision, intent(in) :: ival(size(ioval,1),size(ioval,2))
  double precision, intent(in), optional :: undef
  integer :: ii, jj, ni, nj

  ni=size(ioval,1)
  nj=size(ioval,2)

  if(present(undef))then
     do jj=1,nj
        do ii=1,ni
           if(ioval(ii,jj)/=undef.and.ival(ii,jj)/=undef)then
              ioval(ii,jj)=ioval(ii,jj)+ival(ii,jj)
           else
              ioval(ii,jj)=undef
           end if
        end do
     end do
  else
     do jj=1,nj
        do ii=1,ni
           ioval(ii,jj)=ioval(ii,jj)+ival(ii,jj)
        end do
     end do
  end if

end subroutine add_2d

subroutine subst_2d( ioval, ival, undef )
!-- subtract ival
  implicit none
  double precision, intent(inout) :: ioval(:,:)
  double precision, intent(in) :: ival(size(ioval,1),size(ioval,2))
  double precision, intent(in), optional :: undef
  integer :: ii, jj, ni, nj

  ni=size(ioval,1)
  nj=size(ioval,2)

  if(present(undef))then
     do jj=1,nj
        do ii=1,ni
           if(ioval(ii,jj)/=undef.and.ival(ii,jj)/=undef)then
              ioval(ii,jj)=ioval(ii,jj)-ival(ii,jj)
           else
              ioval(ii,jj)=undef
           end if
        end do
     end do
  else
     do jj=1,nj
        do ii=1,ni
           ioval(ii,jj)=ioval(ii,jj)-ival(ii,jj)
        end do
     end do
  end if

end subroutine subst_2d

subroutine subst_2d_r( ioval, ival, undef )
!-- subtract ival
  implicit none
  real, intent(inout) :: ioval(:,:)
  real, intent(in) :: ival(size(ioval,1),size(ioval,2))
  real, intent(in), optional :: undef
  integer :: ii, jj, ni, nj

  ni=size(ioval,1)
  nj=size(ioval,2)

  if(present(undef))then
     do jj=1,nj
        do ii=1,ni
           if(ioval(ii,jj)/=undef.and.ival(ii,jj)/=undef)then
              ioval(ii,jj)=ioval(ii,jj)-ival(ii,jj)
           else
              ioval(ii,jj)=undef
           end if
        end do
     end do
  else
     do jj=1,nj
        do ii=1,ni
           ioval(ii,jj)=ioval(ii,jj)-ival(ii,jj)
        end do
     end do
  end if

end subroutine subst_2d_r

subroutine rearrange_3d_2d( val3d, val2d )
!-- rearrange 3d variable to 2d variable (k,i,j -> i*j,k)
  implicit none
  double precision, intent(in) :: val3d(:,:,:)
  double precision, intent(out) :: val2d(size(val3d,2)*size(val3d,3),size(val3d,1))
  integer :: ii, jj, kk, ni, nj, nk

  nk=size(val3d,1)
  ni=size(val3d,2)
  nj=size(val3d,3)

  do kk=1,nk
     do jj=1,nj
        do ii=1,ni
           val2d(ni*(jj-1)+ii,kk)=val3d(kk,ii,jj)
        end do
     end do
  end do

end subroutine rearrange_3d_2d

subroutine rearrange_2d_1d( val2d, val1d )
!-- rearrange 2d variable to 1d variable (i,j -> i*j)
  implicit none
  double precision, intent(in) :: val2d(:,:)
  double precision, intent(out) :: val1d(size(val2d,1)*size(val2d,2))
  integer :: ii, jj, ni, nj

  ni=size(val2d,1)
  nj=size(val2d,2)

  do jj=1,nj
     do ii=1,ni
        val1d(ni*(jj-1)+ii)=val2d(ii,jj)
     end do
  end do

end subroutine rearrange_2d_1d

subroutine display_2valdiff_max( val1, val2, undef, cout )
!-- display the maximum of the difference between val1 and val2
  implicit none
  double precision, intent(inout) :: val1(:,:)
  double precision, intent(in) :: val2(size(val1,1),size(val1,2))
  double precision, intent(in), optional :: undef
  character(*), intent(out), optional :: cout
  integer :: ii, jj, ni, nj, maxi, maxj
  double precision :: maxv, dval

  ni=size(val1,1)
  nj=size(val1,2)
  maxv=0.0d0
  maxi=0
  maxj=0

  if(present(undef))then
     do jj=1,nj
        do ii=1,ni
           if(val1(ii,jj)/=undef.and.val2(ii,jj)/=undef)then
              dval=dabs(val1(ii,jj)-val2(ii,jj))
              if(maxv<dval)then
                 maxv=dval
                 maxi=ii
                 maxj=jj
              end if
           end if
        end do
     end do
  else
     do jj=1,nj
        do ii=1,ni
           dval=dabs(val1(ii,jj)-val2(ii,jj))
           if(maxv<dval)then
              maxv=dval
              maxi=ii
              maxj=jj
           end if
        end do
     end do
  end if

  write(*,'(a21,1PE16.8,a5,i4,a1,i4,a2)') "Maximum difference = ", maxv,  &
  &                           " at (", maxi, ",", maxj, ")."
  if(present(cout))then
     write(cout,'(1PE8.1)') maxv
  end if

end subroutine display_2valdiff_max

subroutine stdout( message, routine_name, mtype )
!-- standard output for message
  implicit none
  character(*), intent(in) :: message  ! output message
  character(*), intent(in) :: routine_name  ! called routine name
  integer, intent(in) :: mtype   ! message type
                                 ! 0: message, -1: error, 1: warning
  character(100) :: tname, forma
  character(10000) :: all_mess

  tname=''
  all_mess=''
  select case (mtype)
  case (0)  ! message
     tname(1:7)="MESSAGE"
  case (-1)  ! error
     tname(1:5)="ERROR"
  case (1)  ! warning
     tname(1:7)="WARNING"
  end select

  all_mess="*** "//trim(adjustl(tname))//"("//trim(adjustl(routine_name))  &
  &              //") *** : "//trim(adjustl(message))

  write(forma,*) len_trim(adjustl(all_mess))
  forma='(a'//trim(adjustl(forma))//')'
  write(6,trim(adjustl(forma))) trim(adjustl(all_mess))

end subroutine stdout

end module sub_mod
