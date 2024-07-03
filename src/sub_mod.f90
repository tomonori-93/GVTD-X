module GVTDX_sub
!! Sub module for GVTDX procedures.
  implicit none
  real, parameter :: pi=3.14159265e0   !! Pi for real
  real, parameter :: radius=6.371e6 !! polar radius of the Earth (m)
  complex, parameter :: img=(0.0,1.0)  !! Imaginary unit for real
  double precision, parameter :: pi_dp=3.14159265358979d0  !! Pi for double
  double precision, parameter :: radius_dp=6.371d6   !! polar radius of the Earth (m)
  double precision, parameter :: omega_dp=7.2918933286303412732d-5    !! rotational angular velocity of Earth (s-1)
  complex(kind(0d0)), parameter :: img_cdp=(0.0d0,1.0d0)   !! Imaginary unit for double

contains

!------------------------------------------

subroutine prod_vortex_structure( r, t, rmax, vmax, c1u, c2u,  &
  &                               Vt, Vr, Vt_pert, Vr_pert, Vt_pert_ang, Vr_pert_ang,  &
  &                               ropt, dopt, Vt_0, Vr_0, Uxm, Vym, flag_disp )
!! Producing vortex structure with rmax, vmax, and umax
  implicit none
  double precision, intent(in) :: r(:)  !! radius [m]
  double precision, intent(in) :: t(:)  !! azimuthal angle [rad]
  double precision, intent(in) :: rmax  !! radius of maximum tangential wind speed [m]
  double precision, intent(in) :: vmax  !! maximum tangential wind speed [m s-1]
  double precision, intent(in) :: c1u   !! coefficient 1 for radial wind [s-1]
  double precision, intent(in) :: c2u   !! coefficient 2 for radial wind [s-1]
  double precision, intent(out) :: Vt(size(r),size(t))  !! Profile of tangential wind
  double precision, intent(out) :: Vr(size(r),size(t))  !! Profile of radial wind
  double precision, intent(in), optional :: Vt_pert(:)  !! perturbations of tangential wind [m s-1]
  double precision, intent(in), optional :: Vr_pert(:)  !! perturbations of radial wind [m s-1]
  double precision, intent(in), optional :: Vt_pert_ang(:)  !! angles of tangential wind [rad]
  double precision, intent(in), optional :: Vr_pert_ang(:)  !! angles of radial wind [rad]
  logical, intent(in), optional :: ropt  !! option for radial variation of perturbation Vt and Vr
  logical, intent(in), optional :: dopt  !! option for divergent components of perturbation Vt and Vr
  double precision, intent(out), optional :: Vt_0(:)  !! Radial profile of axisymmetric Vt [m s-1]
  double precision, intent(out), optional :: Vr_0(:)  !! Radial profile of axisymmetric Vr [m s-1]
  double precision, intent(out), optional :: Uxm(2)  !! Azimuthal averaged X-wind of wavenumber-1 component [m s-1]
  double precision, intent(out), optional :: Vym(2)  !! Azimuthal averaged Y-wind of wavenumber-1 component [m s-1]
  logical, intent(in), optional :: flag_disp  ! Flag for displaying each amplitude of asymmetric winds (default: .false.)

  integer :: nr, nt, i, j, k, nvtp, nvrp, nvpmax
  double precision :: tmp_vtp1, tmp_vrp1, tmp_vtp2n, tmp_vrp2n, rad_coef, radp_coef, lin_coef, dr
  double precision :: tmp_vtp1_div, tmp_vrp1_div
  double precision :: umean(2), vmean(2)
  double precision :: r_inv(size(r))
  double precision, allocatable, dimension(:) :: Vtp_omax, Vrp_omax
  double precision, allocatable, dimension(:,:) :: zetap, divp
  double precision, allocatable, dimension(:,:,:) :: gkrr, dgkrr
  double precision, allocatable, dimension(:,:,:) :: gkrr_d, dgkrr_d
  logical rad_opt

  nr=size(r)
  nt=size(t)
  if(present(Vt_pert))then
     nvtp=size(Vt_pert)
  else
     nvtp=0
  end if
  if(present(Vr_pert))then
     nvrp=size(Vr_pert)
  else
     nvrp=0
  end if
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

  if(present(dopt))then
     if(nvpmax>0)then
        allocate(gkrr_d(nvpmax,nr+1,nr+1))
        allocate(dgkrr_d(nvpmax,nr+1,nr+1))
        allocate(divp(nvpmax,nr+1))
        gkrr_d=0.0d0
        dgkrr_d=0.0d0
        divp=0.0d0
        do k=1,nvpmax
           do j=1,nr
              do i=1,nr
                 if(r(i)-0.5d0*dr<0.0d0)then
                    gkrr_d(k,i,j)=green_func( 0.0d0, r(j), k )
                 else
                    gkrr_d(k,i,j)=green_func( r(i)-0.5d0*dr, r(j), k )
                 end if
              end do
           end do
           do j=1,nr
              gkrr_d(k,nr+1,j)=green_func( r(nr)+0.5d0*dr, r(j), k )
           end do
           do i=1,nr
              gkrr_d(k,i,nr+1)=green_func( r(i)+0.5d0*dr, r(nr)+dr, k )
           end do
           do j=1,nr
              do i=1,nr+1
                 dgkrr_d(k,i,j)=(gkrr_d(k,i,j+1)-gkrr_d(k,i,j))/dr
              end do
           end do
           do i=1,nr
              !ORGif(r(i)>=0.1d0*rmax.and.r(i)<2.0d0*rmax)then
              if(r(i)>=0.9d0*rmax.and.r(i)<2.0d0*rmax)then
                 divp(k,i)=abs(Vr_pert(k))/rmax
              end if
           end do
        end do
     end if
  end if

  do i=1,nr
     if(r(i)/=0.0d0)then
        r_inv(i)=1.0d0/r(i)
     else
        r_inv(i)=0.0d0
     end if
  end do

  do j=1,nt
     do i=1,nr
        tmp_vtp1=0.0d0
        tmp_vrp1=0.0d0
        tmp_vtp2n=0.0d0
        tmp_vrp2n=0.0d0
        tmp_vtp1_div=0.0d0
        tmp_vrp1_div=0.0d0
        if(present(Vt_pert))then
           if(rad_opt.eqv..true.)then
              if(nvtp>1)then
!                 do k=2,nvtp
                 do k=1,nvtp
                    lin_coef=line_integral( nr-1, r, dgkrr(k,1:nr,i), zetap(k,1:nr) )*dr
                    tmp_vtp2n=tmp_vtp2n+(-lin_coef*dcos(dble(k)*(t(j)+Vt_pert_ang(k))))
                 end do
              end if
           else
              do k=1,nvtp
                 tmp_vtp1=tmp_vtp1+Vt_pert(k)*dcos(dble(k)*(t(j)+Vt_pert_ang(k)))
              end do
           end if
           if(dopt.eqv..true.)then
              if(nvtp>1)then
                 k=1  ! only WN-1
                 lin_coef=line_integral( nr-1, r, gkrr_d(k,1:nr,i), divp(k,1:nr) )*dr
                 tmp_vtp2n=tmp_vtp2n+lin_coef*dsin(dble(k)*(t(j)+Vt_pert_ang(k)))*r_inv(i)
              end if
           end if
        end if

        if(present(Vr_pert))then
           if(rad_opt.eqv..true.)then
              if(nvrp>1)then
!                 do k=2,nvrp
                 do k=1,nvrp
                    lin_coef=line_integral( nr-1, r, gkrr(k,1:nr,i), zetap(k,1:nr) )*dr
                    tmp_vrp2n=tmp_vrp2n+(-dble(k)*lin_coef*dsin(dble(k)*(t(j)+Vt_pert_ang(k)))*r_inv(i))  ! same as Vt_pert_ang
                 end do
              end if
           else
              do k=1,nvrp
                 tmp_vrp1=tmp_vrp1+Vr_pert(k)*dcos(dble(k)*(t(j)+Vr_pert_ang(k)))
              end do
           end if

           if(dopt.eqv..true.)then
              if(nvrp>1)then
                 k=1  ! only WN-1
                 lin_coef=line_integral( nr-1, r, dgkrr_d(k,1:nr,i), divp(k,1:nr) )*dr
                 tmp_vrp2n=tmp_vrp2n+(-dble(k)*lin_coef*dcos(dble(k)*(t(j)+Vt_pert_ang(k))))  ! same as Vt_pert_ang
              end if
           end if
         end if

        if(r(i)<=rmax)then
           rad_coef=r(i)/rmax
           radp_coef=r(i)/rmax
           Vt(i,j)=vmax*rad_coef
           Vr(i,j)=c1u*dsqrt((rmax-r(i))*r(i))
           Vr(i,j)=Vr(i,j)*4.0*1.0e-3  ! 多分これが必要 [m] -> [km]
        else
           rad_coef=rmax*r_inv(i)
           if((r(i)>rmax).and.(r(i)<=1.5d0*rmax))then
              radp_coef=1.0d0
           else
              radp_coef=rmax*r_inv(i)
           end if
           Vt(i,j)=vmax*rad_coef
           Vr(i,j)=-c2u*dsqrt(r(i)-rmax)*(rmax*r_inv(i))
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

  if(present(Uxm))then
     umean(1)=0.0d0
     umean(2)=0.0d0
     do j=1,nt
        umean(1)=umean(1)+(Vr(1,j)*dcos(t(j))-Vt(1,j)*dsin(t(j)))
        umean(2)=umean(2)+(Vr(nr,j)*dcos(t(j))-Vt(nr,j)*dsin(t(j)))
     end do
     umean(1)=umean(1)/dble(nt)
     umean(2)=umean(2)/dble(nt)
     Uxm(1:2)=umean(1:2)
  end if
  if(present(Vym))then
     vmean(1)=0.0d0
     vmean(2)=0.0d0
     do j=1,nt
        vmean(1)=vmean(1)+(Vr(1,j)*dsin(t(j))+Vt(1,j)*dcos(t(j)))
        vmean(2)=vmean(2)+(Vr(nr,j)*dsin(t(j))+Vt(nr,j)*dcos(t(j)))
     end do
     vmean(1)=vmean(1)/dble(nt)
     vmean(2)=vmean(2)/dble(nt)
     Vym(1:2)=vmean(1:2)
  end if

  if(present(flag_disp))then  ! Only displaying values
     allocate(Vtp_omax(nvtp))
     allocate(Vrp_omax(nvrp))
     if((flag_disp.eqv..true.).and.(nvpmax>0))then
        if(present(Vt_pert))then
           if(rad_opt.eqv..true.)then
              do k=1,nvtp
                 do i=1,nr
                    Vtp_omax(k)=-line_integral( nr-1, r, dgkrr(k,1:nr,i), zetap(k,1:nr) )*dr
!                    write(*,'(a4,i2,a8,1PE16.8,a6)') "Vtp(", k ,",r_m) = ", Vtp_omax(k), "[m/s]."
                 end do
              end do
           else
              do k=1,nvtp
                 Vtp_omax(k)=Vt_pert(k)
              end do
           end if
           do k=1,nvtp
              write(*,'(a4,i2,a8,1PE16.8,a6)') "Vtp(", k ,",r_m) = ", Vtp_omax(k), "[m/s]."
           end do
        end if
        if(present(Vr_pert))then
           if(rad_opt.eqv..true.)then
              do k=1,nvrp
                 do i=1,nr
                    Vrp_omax(k)=-dble(k)*line_integral( nr-1, r, gkrr(k,1:nr,i), zetap(k,1:nr) )*dr*r_inv(i)
!                    write(*,'(a4,i2,a8,1PE16.8,a6)') "Vrp(", k ,",r_i) = ", Vrp_omax(k), "[m/s]."
                 end do
              end do
           else
              do k=1,nvrp
                 Vrp_omax(k)=Vr_pert(k)
              end do
           end if
           do k=1,nvrp
              write(*,'(a4,i2,a8,1PE16.8,a6)') "Vrp(", k ,",r_m) = ", Vrp_omax(k), "[m/s]."
           end do
        end if
     end if
     deallocate(Vtp_omax)
     deallocate(Vrp_omax)
  end if

  if(present(ropt))then
     if(nvpmax>0)then
        deallocate(gkrr)
        deallocate(dgkrr)
        deallocate(zetap)
     end if
  end if

  if(present(ropt))then
     if(nvpmax>0)then
        deallocate(gkrr_d)
        deallocate(dgkrr_d)
        deallocate(divp)
     end if
  end if

end subroutine prod_vortex_structure

!--------------------------------------------------
!--------------------------------------------------

subroutine prod_vortex_structure_L06( r, t, rmax, zmax, epsr, Vt_pert_ang, Vt, Vr )
!! Producing vortex structure with rmax, vmax, and umax in Lee et al. (2006)
  implicit none
  double precision, intent(in) :: r(:)  !! radius [m]
  double precision, intent(in) :: t(:)  !! azimuthal angle [rad]
  double precision, intent(in) :: rmax  !! radius of maximum tangential wind speed [m]
  double precision, intent(in) :: zmax  !! constant vorticity in the eye [s-1]
  double precision, intent(in) :: epsr  !! distance of epsilon for WN2 [m]
  double precision, intent(in) :: Vt_pert_ang  !! angles of tangential wind [rad]
  double precision, intent(out) :: Vt(size(r),size(t))  !! Profile of tangential wind
  double precision, intent(out) :: Vr(size(r),size(t))  !! Profile of radial wind

  integer :: nr, nt, i, j
  double precision :: R_ell

  nr=size(r)
  nt=size(t)

  do j=1,nt
     R_ell=rmax+epsr*dcos(2.0d0*t(j)+Vt_pert_ang)
     do i=1,nr
        if(r(i)<=R_ell)then
           Vt(i,j)=0.5d0*zmax*r(i)*(1.0d0+(epsr/rmax)*dcos(2.0d0*t(j)+Vt_pert_ang))
           Vr(i,j)=0.5d0*zmax*r(i)*((epsr/rmax)*dsin(2.0d0*t(j)+Vt_pert_ang))
        else
           Vt(i,j)=0.5d0*zmax*(rmax**2/r(i))*(1.0d0-epsr*(rmax/(r(i)**2))*dcos(2.0d0*t(j)+Vt_pert_ang))
           Vr(i,j)=0.5d0*zmax*(rmax**2/r(i))*(epsr*(rmax/(r(i)**2))*dsin(2.0d0*t(j)+Vt_pert_ang))
        end if
     end do
  end do

end subroutine prod_vortex_structure_L06

!--------------------------------------------------
!--------------------------------------------------

!subroutine prod_radar_along_vel()
!  implicit none
!
!end subroutine prod_radar_along_vel

!--------------------------------------------------
!--------------------------------------------------

subroutine conv_VtVr2VxVy_rt( r, t, Vt, Vr, Vx, Vy, undef )
!! Convert Vt and Vr to Vx and Vy on R-T coordinates
  implicit none
  double precision, intent(in) :: r(:)  !! R-coordinate [m]
  double precision, intent(in) :: t(:)  !! T-coordinate [rad]
  double precision, intent(in) :: Vt(size(r),size(t))   !! tangential wind component on R-T coordinates
  double precision, intent(in) :: Vr(size(r),size(t))   !! radial wind component on R-T coordinates
  double precision, intent(out) :: Vx(size(r),size(t))  !! X-component of wind on R-T coordinates
  double precision, intent(out) :: Vy(size(r),size(t))  !! Y-component of wind on R-T coordinates
  double precision, intent(in), optional :: undef  !! Undefined value
  integer :: nr, nt, i, j

  nr=size(r)
  nt=size(t)

  if(present(undef))then
     Vx=undef
     Vy=undef
     do j=1,nt
        do i=1,nr
           if(Vt(i,j)/=undef.and.Vr(i,j)/=undef)then
              Vx(i,j)=Vr(i,j)*dcos(t(j))-Vt(i,j)*dsin(t(j))
              Vy(i,j)=Vr(i,j)*dsin(t(j))+Vt(i,j)*dcos(t(j))
           end if
        end do
     end do
  else
     do j=1,nt
        do i=1,nr
           Vx(i,j)=Vr(i,j)*dcos(t(j))-Vt(i,j)*dsin(t(j))
           Vy(i,j)=Vr(i,j)*dsin(t(j))+Vt(i,j)*dcos(t(j))
        end do
     end do
  end if

end subroutine conv_VtVr2VxVy_rt

!--------------------------------------------------
!--------------------------------------------------

subroutine conv_VxVy2VtVr_rt( r, t, Vx, Vy, Vt, Vr, undef )
!! Convert Vx and Vy to Vr and Vt on R-T coordinates
  implicit none
  double precision, intent(in) :: r(:)  !! R-coordinate [m]
  double precision, intent(in) :: t(:)  !! T-coordinate [rad]
  double precision, intent(in) :: Vx(size(r),size(t))  !! X-component of wind on R-T coordinates
  double precision, intent(in) :: Vy(size(r),size(t))  !! Y-component of wind on R-T coordinates
  double precision, intent(out) :: Vt(size(r),size(t))   !! tangential wind component on R-T coordinates
  double precision, intent(out) :: Vr(size(r),size(t))   !! radial wind component on R-T coordinates
  double precision, intent(in), optional :: undef  !! Undefined value
  integer :: nr, nt, i, j

  nr=size(r)
  nt=size(t)

  if(present(undef))then
     Vr=undef
     Vt=undef
     do j=1,nt
        do i=1,nr
           if(Vx(i,j)/=undef.and.Vy(i,j)/=undef)then
              Vr(i,j)=Vx(i,j)*dcos(t(j))+Vy(i,j)*dsin(t(j))
              Vt(i,j)=-Vx(i,j)*dsin(t(j))+Vy(i,j)*dcos(t(j))
           end if
        end do
     end do
  else
     do j=1,nt
        do i=1,nr
           Vr(i,j)=Vx(i,j)*dcos(t(j))+Vy(i,j)*dsin(t(j))
           Vt(i,j)=-Vx(i,j)*dsin(t(j))+Vy(i,j)*dcos(t(j))
        end do
     end do
  end if

end subroutine conv_VxVy2VtVr_rt

!--------------------------------------------------
!--------------------------------------------------

subroutine conv_VxVy2VtVr_xy( x, y, xc, yc, Vx, Vy, Vt, Vr, undef )
!! Convert Vx and Vy to Vr and Vt on X-Y coordinates
  implicit none
  double precision, intent(in) :: x(:)  !! X-coordinate [m]
  double precision, intent(in) :: y(:)  !! Y-coordinate [m]
  double precision, intent(in) :: xc    !! X component of the center [m]
  double precision, intent(in) :: yc    !! Y component of the center [m]
  double precision, intent(in) :: Vx(size(x),size(y))  !! X-component of wind on X-Y coordinates
  double precision, intent(in) :: Vy(size(x),size(y))  !! Y-component of wind on X-Y coordinates
  double precision, intent(out) :: Vt(size(x),size(y))   !! tangential wind component on X-Y coordinates
  double precision, intent(out) :: Vr(size(x),size(y))   !! radial wind component on X-Y coordinates
  double precision, intent(in), optional :: undef  !! Undefined value
  integer :: nx, ny, i, j
  double precision :: radi, radi_i

  nx=size(x)
  ny=size(y)

  if(present(undef))then
     Vr=undef
     Vt=undef
     do j=1,ny
        do i=1,nx
           if(Vx(i,j)/=undef.and.Vy(i,j)/=undef)then
              radi=dsqrt(dabs((x(i)-xc)**2+(y(j)-yc)**2))
              if(radi/=0.0d0)then
                 radi_i=1.0d0/radi
                 Vr(i,j)=((x(i)-xc)*radi_i)*Vx(i,j)+((y(j)-yc)*radi_i)*Vy(i,j)
                 Vt(i,j)=((x(i)-xc)*radi_i)*Vy(i,j)-((y(j)-yc)*radi_i)*Vx(i,j)
              else
                 Vr(i,j)=0.0d0
                 Vt(i,j)=0.0d0
              end if
           end if
        end do
     end do
  else
     do j=1,ny
        do i=1,nx
           radi=dsqrt(dabs((x(i)-xc)**2+(y(j)-yc)**2))
           if(radi/=0.0d0)then
              radi_i=1.0d0/radi
              Vr(i,j)=((x(i)-xc)*radi_i)*Vx(i,j)+((y(j)-yc)*radi_i)*Vy(i,j)
              Vt(i,j)=((x(i)-xc)*radi_i)*Vy(i,j)-((y(j)-yc)*radi_i)*Vx(i,j)
           else
              Vr(i,j)=0.0d0
              Vt(i,j)=0.0d0
           end if
        end do
     end do
  end if

end subroutine conv_VxVy2VtVr_xy

!--------------------------------------------------
!--------------------------------------------------

subroutine proj_VxVy2Vraxy( x, y, rax, ray, Vx, Vy, Vraxy, undef )
!! Calculate Vx and Vy to Vd along with radar beams on X-Y coodinates
  implicit none
  double precision, intent(in) :: x(:)  !! X-coordinate
  double precision, intent(in) :: y(:)  !! Y-coordinate
  double precision, intent(in) :: rax   !! X-coodinate of radar location
  double precision, intent(in) :: ray   !! Y-coodinate of radar location
  double precision, intent(in) :: Vx(size(x),size(y))  !! X-component of wind on X-Y coordinates
  double precision, intent(in) :: Vy(size(x),size(y))  !! Y-component of wind on X-Y coordinates
  double precision, intent(out) :: Vraxy(size(x),size(y))  !! velocity along with beam on X-Y coordinates
  double precision, intent(in), optional :: undef  !! Undefined value
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

!--------------------------------------------------
!--------------------------------------------------

subroutine proj_VtVr2Vrart( r, t, td, Vt, Vr, Vra, undef )
!! Convert Vt and Vr to Vx and Vy on R-T coordinates
  implicit none
  double precision, intent(in) :: r(:)  !! R-coordinate [m]
  double precision, intent(in) :: t(:)  !! T-coordinate [rad]
  double precision, intent(in) :: td(size(r),size(t))   !! radar azimuthal angle on R-T coordinate [rad]
  double precision, intent(in) :: Vt(size(r),size(t))   !! tangential wind component on R-T coordinates
  double precision, intent(in) :: Vr(size(r),size(t))   !! radial wind component on R-T coordinates
  double precision, intent(out) :: Vra(size(r),size(t))  !! Velocity along beam on R-T coordinates
  double precision, intent(in), optional :: undef  !! undefined value
  integer :: nr, nt, i, j

  nr=size(r)
  nt=size(t)

  do j=1,nt
     do i=1,nr
        if(Vt(i,j)/=undef.and.Vr(i,j)/=undef.and.td(i,j)/=undef)then
           Vra(i,j)=-Vt(i,j)*dsin(t(j)-td(i,j))+Vr(i,j)*dcos(t(j)-td(i,j))
        else
           Vra(i,j)=undef
        end if
     end do
  end do

end subroutine proj_VtVr2Vrart

!--------------------------------------------------
!--------------------------------------------------

double precision function line_integral( nr, rdh, gkrr, div_r, undef )
!! Calculate a line integral (actually, sum for arguments)
  implicit none
  integer, intent(in) :: nr  !! Radial grid number
  double precision, intent(in) :: rdh(nr+1)  !! R-coordinate [m]
  double precision, intent(in) :: gkrr(nr+1)  !! Green function
  double precision, intent(in) :: div_r(nr+1)  !! Integral of gkrr
  double precision, intent(in), optional :: undef  !! Undefined value
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

!--------------------------------------------------
!--------------------------------------------------

double precision function green_func( rc, r, nval )
!! Calculation of the Green function: \(,\; \) <br>
!! \( G_n(r_c;r)=-(2^{-n})(r/r_c)^n, r<r_c \) <br>
!! \( G_n(r_c;r)=-(2^{-n})(r_c/r)^n, r\geq r_c \) <br>
!! \( n= \)nval
  implicit none
  double precision, intent(in) :: rc  !! Source radius
  double precision, intent(in) :: r  !! Non-source radius
  integer, intent(in) :: nval  !! Order number
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

!--------------------------------------------------
!--------------------------------------------------

subroutine div_curl_2d( r, t, ur, vt, divr, curl, undef )
!! Calculation of rotation and divergence from radial and tangential winds <br>
!! divr = \(\dfrac{\partial ru_r}{r\partial r} + \dfrac{\partial v_t}{r\partial \theta} \) <br>
!! curl = \(\dfrac{\partial rv_t}{r\partial r} - \dfrac{\partial u_r}{r\partial \theta} \)
  implicit none
  double precision, intent(in) :: r(:)  !! radius [m]
  double precision, intent(in) :: t(:)  !! angle [rad]
  double precision, intent(in) :: ur(size(r),size(t))  !! Ur [m/s]
  double precision, intent(in) :: vt(size(r),size(t))  !! Vt [m/s]
  double precision, intent(out) :: divr(size(r),size(t))  !! divergence [1/s]
  double precision, intent(out) :: curl(size(r),size(t))  !! rotation [1/s]
  double precision, intent(in), optional :: undef  !! Undefined value
  integer :: ii, jj, ni, nj
  double precision :: dr, dt
  logical :: undeflag(size(r),size(t))

  ni=size(r)
  nj=size(t)
  dr=r(2)-r(1)
  dt=t(2)-t(1)

  if(present(undef))then

     divr=undef
     curl=undef

     undeflag=.false.

     do jj=2,nj-1
        do ii=2,ni-1
           if(ur(ii,jj)==undef.or.vt(ii,jj)==undef)then
              undeflag(ii,jj)=.true.
              undeflag(ii-1,jj)=.true.
              undeflag(ii+1,jj)=.true.
              undeflag(ii,jj-1)=.true.
              undeflag(ii,jj+1)=.true.
           end if
        end do
     end do
     do jj=2,nj-1
        if(ur(1,jj)==undef.or.vt(1,jj)==undef)then
           undeflag(1,jj)=.true.
           undeflag(2,jj)=.true.
           undeflag(1,jj-1)=.true.
           undeflag(1,jj+1)=.true.
        end if
        if(ur(ni,jj)==undef.or.vt(ni,jj)==undef)then
           undeflag(ni,jj)=.true.
           undeflag(ni-1,jj)=.true.
           undeflag(ni,jj-1)=.true.
           undeflag(ni,jj+1)=.true.
        end if
     end do
     do ii=2,ni-1
        if(ur(ii,1)==undef.or.vt(ii,1)==undef)then
           undeflag(ii,1)=.true.
           undeflag(ii-1,1)=.true.
           undeflag(ii+1,1)=.true.
           undeflag(ii,2)=.true.
        end if
        if(ur(ii,nj)==undef.or.vt(ii,nj)==undef)then
           undeflag(ii,nj)=.true.
           undeflag(ii-1,nj)=.true.
           undeflag(ii+1,nj)=.true.
           undeflag(ii,nj-1)=.true.
        end if
     end do
     if(ur(1,1)==undef.or.vt(1,1)==undef)then
        undeflag(1,1)=.true.
        undeflag(2,1)=.true.
        undeflag(1,2)=.true.
     end if
     if(ur(ni,1)==undef.or.vt(ni,1)==undef)then
        undeflag(ni,1)=.true.
        undeflag(ni-1,1)=.true.
        undeflag(ni,2)=.true.
     end if
     if(ur(1,nj)==undef.or.vt(1,nj)==undef)then
        undeflag(1,nj)=.true.
        undeflag(2,nj)=.true.
        undeflag(1,nj-1)=.true.
     end if
     if(ur(ni,nj)==undef.or.vt(ni,nj)==undef)then
        undeflag(ni,nj)=.true.
        undeflag(ni-1,nj)=.true.
        undeflag(ni,nj-1)=.true.
     end if

     do jj=2,nj-1
        do ii=2,ni-1
           if(undeflag(ii,jj).eqv..false.)then
              divr(ii,jj)=0.5d0*(ur(ii+1,jj)-ur(ii-1,jj))/dr  &
  &                      +ur(ii,jj)/r(ii)  &
  &                      +0.5d0*(vt(ii,jj+1)-vt(ii,jj-1))/(r(ii)*dt)
              curl(ii,jj)=0.5d0*(vt(ii+1,jj)-vt(ii-1,jj))/dr  &
  &                      +vt(ii,jj)/r(ii)  &
  &                      -0.5d0*(ur(ii,jj+1)-ur(ii,jj-1))/(r(ii)*dt)
           end if
        end do
     end do

     if(r(1)>0.0d0)then
        do jj=2,nj-1
           if(undeflag(1,jj).eqv..false.)then
              divr(1,jj)=(ur(2,jj)-ur(1,jj))/dr  &
  &                      +ur(1,jj)/r(1)  &
  &                      +0.5d0*(vt(1,jj+1)-vt(1,jj-1))/(r(1)*dt)
              curl(1,jj)=(vt(2,jj)-vt(1,jj))/dr  &
  &                      +vt(1,jj)/r(1)  &
  &                      -0.d50*(ur(1,jj+1)-ur(1,jj-1))/(r(1)*dt)
           end if
        end do
     end if

     do jj=2,nj-1
        if(undeflag(ni,jj).eqv..false.)then
           divr(ni,jj)=(ur(ni,jj)-ur(ni-1,jj))/dr  &
  &                   +ur(ni,jj)/r(ni)  &
  &                   +0.5d0*(vt(ni,jj+1)-vt(ni,jj-1))/(r(ni)*dt)
           curl(ni,jj)=(vt(ni,jj)-vt(ni-1,jj))/dr  &
  &                   +vt(ni,jj)/r(ni)  &
  &                   -0.5d0*(ur(ni,jj+1)-ur(ni,jj-1))/(r(ni)*dt)
        end if
     end do

     do ii=2,ni-1
        if(undeflag(ii,1).eqv..false.)then
           divr(ii,1)=0.5d0*(ur(ii+1,1)-ur(ii-1,1))/dr  &
  &                   +ur(ii,1)/r(ii)  &
  &                   +(vt(ii,2)-vt(ii,1))/(r(ii)*dt)
           curl(ii,1)=0.5d0*(vt(ii+1,1)-vt(ii-1,1))/dr  &
  &                   +vt(ii,1)/r(ii)  &
  &                   -(ur(ii,2)-ur(ii,1))/(r(ii)*dt)
        end if
        if(undeflag(ii,nj).eqv..false.)then
           divr(ii,nj)=0.5d0*(ur(ii+1,nj)-ur(ii-1,nj))/dr  &
  &                   +ur(ii,nj)/r(ii)  &
  &                   +(vt(ii,nj)-vt(ii,nj-1))/(r(ii)*dt)
           curl(ii,nj)=0.5d0*(vt(ii+1,nj)-vt(ii-1,nj))/dr  &
  &                   +vt(ii,nj)/r(ii)  &
  &                   -(ur(ii,nj)-ur(ii,nj-1))/(r(ii)*dt)
        end if
     end do

     if(r(1)>0.0d0)then
        if(undeflag(1,1).eqv..false.)then
           divr(1,1)=(ur(2,1)-ur(1,1))/dr  &
  &                 +ur(1,1)/r(1)  &
  &                 +(vt(1,2)-vt(1,1))/(r(1)*dt)
           curl(1,1)=(vt(2,1)-vt(1,1))/dr  &
  &                 +vt(1,1)/r(1)  &
  &                 -(ur(1,2)-ur(1,1))/(r(1)*dt)
        end if
        if(undeflag(1,nj).eqv..false.)then
           divr(1,nj)=(ur(2,nj)-ur(1,nj))/dr  &
  &                 +ur(1,nj)/r(1)  &
  &                 +(vt(1,nj)-vt(1,nj-1))/(r(1)*dt)
           curl(1,nj)=(vt(2,nj)-vt(1,nj))/dr  &
  &                 +vt(1,nj)/r(1)  &
  &                 -(ur(1,nj)-ur(1,nj-1))/(r(1)*dt)
        end if
     end if

     if(undeflag(ni,1).eqv..false.)then
        divr(ni,1)=(ur(ni,1)-ur(ni-1,1))/dr  &
  &                +ur(ni,1)/r(ni)  &
  &                +(vt(ni,2)-vt(ni,1))/(r(ni)*dt)
        curl(ni,1)=(vt(ni,1)-vt(ni-1,1))/dr  &
  &                +vt(ni,1)/r(ni)  &
  &                -(ur(ni,2)-ur(ni,1))/(r(ni)*dt)
     end if
     if(undeflag(ni,nj).eqv..false.)then
        divr(ni,nj)=(ur(ni,nj)-ur(ni-1,nj))/dr  &
  &                +ur(ni,nj)/r(ni)  &
  &                +(vt(ni,nj)-vt(ni,nj-1))/(r(ni)*dt)
        curl(ni,nj)=(vt(ni,nj)-vt(ni-1,nj))/dr  &
  &                +vt(ni,nj)/r(ni)  &
  &                -(ur(ni,nj)-ur(ni,nj-1))/(r(ni)*dt)
     end if

  else

     divr=0.0d0
     curl=0.0d0

     do jj=2,nj-1
        do ii=2,ni-1
           divr(ii,jj)=0.5d0*(ur(ii+1,jj)-ur(ii-1,jj))/dr  &
  &                   +ur(ii,jj)/r(ii)  &
  &                   +0.5d0*(vt(ii,jj+1)-vt(ii,jj-1))/(r(ii)*dt)
           curl(ii,jj)=0.5d0*(vt(ii+1,jj)-vt(ii-1,jj))/dr  &
  &                   +vt(ii,jj)/r(ii)  &
  &                   -0.5d0*(ur(ii,jj+1)-ur(ii,jj-1))/(r(ii)*dt)
        end do
     end do

     if(r(1)>0.0d0)then
        do jj=2,nj-1
           divr(1,jj)=(ur(2,jj)-ur(1,jj))/dr  &
  &                   +ur(1,jj)/r(1)  &
  &                   +0.5d0*(vt(1,jj+1)-vt(1,jj-1))/(r(1)*dt)
           curl(1,jj)=(vt(2,jj)-vt(1,jj))/dr  &
  &                   +vt(1,jj)/r(1)  &
  &                   -0.d50*(ur(1,jj+1)-ur(1,jj-1))/(r(1)*dt)
           divr(ni,jj)=(ur(ni,jj)-ur(ni-1,jj))/dr  &
  &                   +ur(ni,jj)/r(ni)  &
  &                   +0.5d0*(vt(ni,jj+1)-vt(ni,jj-1))/(r(ni)*dt)
           curl(ni,jj)=(vt(ni,jj)-vt(ni-1,jj))/dr  &
  &                   +vt(ni,jj)/r(ni)  &
  &                   -0.5d0*(ur(ni,jj+1)-ur(ni,jj-1))/(r(ni)*dt)
        end do
     else
        do jj=2,nj-1
           divr(ni,jj)=(ur(ni,jj)-ur(ni-1,jj))/dr  &
  &                   +ur(ni,jj)/r(ni)  &
  &                   +0.5d0*(vt(ni,jj+1)-vt(ni,jj-1))/(r(ni)*dt)
           curl(ni,jj)=(vt(ni,jj)-vt(ni-1,jj))/dr  &
  &                   +vt(ni,jj)/r(ni)  &
  &                   -0.5d0*(ur(ni,jj+1)-ur(ni,jj-1))/(r(ni)*dt)
        end do
     end if

     do ii=2,ni-1
        divr(ii,1)=0.5d0*(ur(ii+1,1)-ur(ii-1,1))/dr  &
  &                +ur(ii,1)/r(ii)  &
  &                +(vt(ii,2)-vt(ii,1))/(r(ii)*dt)
        curl(ii,1)=0.5d0*(vt(ii+1,1)-vt(ii-1,1))/dr  &
  &                +vt(ii,1)/r(ii)  &
  &                -(ur(ii,2)-ur(ii,1))/(r(ii)*dt)
        divr(ii,nj)=0.5d0*(ur(ii+1,nj)-ur(ii-1,nj))/dr  &
  &                +ur(ii,nj)/r(ii)  &
  &                +(vt(ii,nj)-vt(ii,nj-1))/(r(ii)*dt)
        curl(ii,nj)=0.5d0*(vt(ii+1,nj)-vt(ii-1,nj))/dr  &
  &                +vt(ii,nj)/r(ii)  &
  &                -(ur(ii,nj)-ur(ii,nj-1))/(r(ii)*dt)
     end do

     if(r(1)>0.0d0)then
        divr(1,1)=(ur(2,1)-ur(1,1))/dr  &
  &              +ur(1,1)/r(1)  &
  &              +(vt(1,2)-vt(1,1))/(r(1)*dt)
        curl(1,1)=(vt(2,1)-vt(1,1))/dr  &
  &              +vt(1,1)/r(1)  &
  &              -(ur(1,2)-ur(1,1))/(r(1)*dt)
        divr(1,nj)=(ur(2,nj)-ur(1,nj))/dr  &
  &              +ur(1,nj)/r(1)  &
  &              +(vt(1,nj)-vt(1,nj-1))/(r(1)*dt)
        curl(1,nj)=(vt(2,nj)-vt(1,nj))/dr  &
  &              +vt(1,nj)/r(1)  &
  &              -(ur(1,nj)-ur(1,nj-1))/(r(1)*dt)
     end if

     divr(ni,1)=(ur(ni,1)-ur(ni-1,1))/dr  &
  &             +ur(ni,1)/r(ni)  &
  &             +(vt(ni,2)-vt(ni,1))/(r(ni)*dt)
     curl(ni,1)=(vt(ni,1)-vt(ni-1,1))/dr  &
  &             +vt(ni,1)/r(ni)  &
  &             -(ur(ni,2)-ur(ni,1))/(r(ni)*dt)
     divr(ni,nj)=(ur(ni,nj)-ur(ni-1,nj))/dr  &
  &             +ur(ni,nj)/r(ni)  &
  &             +(vt(ni,nj)-vt(ni,nj-1))/(r(ni)*dt)
     curl(ni,nj)=(vt(ni,nj)-vt(ni-1,nj))/dr  &
  &             +vt(ni,nj)/r(ni)  &
  &             -(ur(ni,nj)-ur(ni,nj-1))/(r(ni)*dt)
  end if

end subroutine div_curl_2d

!--------------------------------------------------
!--------------------------------------------------


subroutine rotate_thetad_tc( thetad_tc, Vx, Vy, undef )
!! Rotate horizontal winds in the east-west and north-south directions to the storm-relative direction. 
  implicit none
  double precision, intent(in) :: thetad_tc   !! Angle of the direction from the radar to the storm center [rad]
  double precision, intent(inout) :: Vx(:,:)  !! Horizontal wind component in the east-west direction [m/s]
  double precision, intent(inout) :: Vy(size(Vx,1),size(Vx,2))  !! Horizontal wind component in the north-south direction [m/s]
  double precision, intent(in), optional :: undef  !! Undefined value
  integer :: ii, jj, ni, nj
  double precision :: tmpu, tmpv

  ni=size(Vx,1)
  nj=size(Vx,2)

  if(present(undef))then

     do jj=1,nj
        do ii=1,ni
           if(Vx(ii,jj)/=undef.and.Vy(ii,jj)/=undef)then
              tmpu=Vx(ii,jj)*dcos(thetad_tc)+Vy(ii,jj)*dsin(thetad_tc)
              tmpv=-Vx(ii,jj)*dsin(thetad_tc)+Vy(ii,jj)*dcos(thetad_tc)
              Vx(ii,jj)=tmpu
              Vy(ii,jj)=tmpv
           end if
        end do
     end do

  else

     do jj=1,nj
        do ii=1,ni
           tmpu=Vx(ii,jj)*dcos(thetad_tc)+Vy(ii,jj)*dsin(thetad_tc)
           tmpv=-Vx(ii,jj)*dsin(thetad_tc)+Vy(ii,jj)*dcos(thetad_tc)
           Vx(ii,jj)=tmpu
           Vy(ii,jj)=tmpv
        end do
     end do

  end if

end subroutine rotate_thetad_tc

!--------------------------------------------------
!--------------------------------------------------


subroutine conv_d2r_1d( ival, oval )
!! Convert double to real
  implicit none
  double precision, intent(in) :: ival(:)  !! Input
  real, intent(out) :: oval(size(ival))  !! Output
  integer :: ii, ni

  ni=size(ival)

  do ii=1,ni
     oval(ii)=real(ival(ii))
  end do

end subroutine conv_d2r_1d

!--------------------------------------------------
!--------------------------------------------------

subroutine conv_d2r_2d( ival, oval )
!! Convert double to real
  implicit none
  double precision, intent(in) :: ival(:,:)  !! Input
  real, intent(out) :: oval(size(ival,1),size(ival,2))  !! Output
  integer :: ii, jj, ni, nj

  ni=size(ival,1)
  nj=size(ival,2)

  do jj=1,nj
     do ii=1,ni
        oval(ii,jj)=real(ival(ii,jj))
     end do
  end do

end subroutine conv_d2r_2d

!--------------------------------------------------
!--------------------------------------------------

subroutine conv_d2r_3d( ival, oval )
!! convert double to real
  implicit none
  double precision, intent(in) :: ival(:,:,:)  !! input array
  real, intent(out) :: oval(size(ival,1),size(ival,2),size(ival,3))  !! output array
  integer :: ii, jj, kk, ni, nj, nk  !! internal variables

  ni=size(ival,1)
  nj=size(ival,2)
  nk=size(ival,3)

  do kk=1,nk
     do jj=1,nj
        do ii=1,ni
!write(*,*) "checkii", ii, jj, kk, ival(ii,jj,kk)
if(dabs(ival(ii,jj,kk))>1.0d15)then
           oval(ii,jj,kk)=-1.0e3
else
           oval(ii,jj,kk)=real(ival(ii,jj,kk))
end if
        end do
     end do
  end do

end subroutine conv_d2r_3d

!--------------------------------------------------
!--------------------------------------------------

subroutine conv_r2d_2d( ival, oval )
!! convert real to double
  implicit none
  real, intent(in) :: ival(:,:)  !! input array
  double precision, intent(out) :: oval(size(ival,1),size(ival,2))  !! output array
  integer :: ii, jj, ni, nj  !! internal variables

  ni=size(ival,1)
  nj=size(ival,2)

  do jj=1,nj
     do ii=1,ni
        oval(ii,jj)=dble(ival(ii,jj))
     end do
  end do

end subroutine conv_r2d_2d

!--------------------------------------------------
!--------------------------------------------------

subroutine sum_1d( val, res, undef )
!! Calculation of sum for 1D variable
  implicit none
  double precision, intent(in) :: val(:)  !! input
  double precision, intent(out) :: res  !! output
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

!--------------------------------------------------
!--------------------------------------------------

subroutine add_2d( ioval, ival, undef )
!! add ival
  implicit none
  double precision, intent(inout) :: ioval(:,:)  !! Base value
  double precision, intent(in) :: ival(size(ioval,1),size(ioval,2))  !! added
  double precision, intent(in), optional :: undef  !! Undefined value
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

!--------------------------------------------------
!--------------------------------------------------

subroutine add_3d( ioval, ival, undef )
!! add ival
  implicit none
  double precision, intent(inout) :: ioval(:,:,:)  !! Base value
  double precision, intent(in) :: ival(size(ioval,1),size(ioval,2),size(ioval,3))  !! added
  double precision, intent(in), optional :: undef  !! Undefined value
  integer :: ii, jj, kk, ni, nj, nk

  ni=size(ioval,1)
  nj=size(ioval,2)
  nk=size(ioval,3)

  if(present(undef))then
     do kk=1,nk
        do jj=1,nj
           do ii=1,ni
              if(ioval(ii,jj,kk)/=undef.and.ival(ii,jj,kk)/=undef)then
                 ioval(ii,jj,kk)=ioval(ii,jj,kk)+ival(ii,jj,kk)
              else
                 ioval(ii,jj,kk)=undef
              end if
           end do
        end do
     end do
  else
     do kk=1,nk
        do jj=1,nj
           do ii=1,ni
              ioval(ii,jj,kk)=ioval(ii,jj,kk)+ival(ii,jj,kk)
           end do
        end do
     end do
  end if

end subroutine add_3d

!--------------------------------------------------
!--------------------------------------------------

subroutine subst_2d( ioval, ival, undef )
!! subtract ival
  implicit none
  double precision, intent(inout) :: ioval(:,:)  !! Base value
  double precision, intent(in) :: ival(size(ioval,1),size(ioval,2))  !! Subtracted
  double precision, intent(in), optional :: undef  !! Undefined value
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

!--------------------------------------------------
!--------------------------------------------------

subroutine subst_2d_r( ioval, ival, undef )
!! subtract ival
  implicit none
  real, intent(inout) :: ioval(:,:)  !! Base value
  real, intent(in) :: ival(size(ioval,1),size(ioval,2))  !! Subtracted value
  real, intent(in), optional :: undef  !! Undefined value
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

!--------------------------------------------------
!--------------------------------------------------

subroutine rearrange_3d_2d( val3d, val2d )
!! Rearrange 3d variable to 2d variable (k,i,j -> i*j,k)
  implicit none
  double precision, intent(in) :: val3d(:,:,:)  !! Input
  double precision, intent(out) :: val2d(size(val3d,2)*size(val3d,3),size(val3d,1))  !! Output
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

!--------------------------------------------------
!--------------------------------------------------

subroutine rearrange_2d_1d( val2d, val1d )
!! Rearrange 2d variable to 1d variable (i,j -> i*j)
  implicit none
  double precision, intent(in) :: val2d(:,:)  !! Input
  double precision, intent(out) :: val1d(size(val2d,1)*size(val2d,2))  !! Output
  integer :: ii, jj, ni, nj

  ni=size(val2d,1)
  nj=size(val2d,2)

  do jj=1,nj
     do ii=1,ni
        val1d(ni*(jj-1)+ii)=val2d(ii,jj)
     end do
  end do

end subroutine rearrange_2d_1d

!--------------------------------------------------
!--------------------------------------------------

subroutine display_1val_max( val, undef, cout, vout )
!! Display the maximum of the array val
  implicit none
  real, intent(in) :: val(:,:)  !! Input 1
  real, intent(in), optional :: undef  !! Undefined value
  character(*), intent(out), optional :: cout  !! Maximum value by character
  real, intent(out), optional :: vout  !! Maximum value by float
  integer :: ii, jj, ni, nj, maxi, maxj
  real :: maxv, dval

  ni=size(val,1)
  nj=size(val,2)
  maxv=0.0
  maxi=0
  maxj=0

  if(present(undef))then
     do jj=1,nj
        do ii=1,ni
           if(val(ii,jj)/=undef)then
              dval=abs(val(ii,jj))
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
           dval=abs(val(ii,jj))
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
  if(present(vout))then
     vout=maxv
  end if

end subroutine display_1val_max

!--------------------------------------------------
!--------------------------------------------------

subroutine display_2valdiff_max( val1, val2, undef, cout, vout )
!! Display the maximum of the difference between val1 and val2
  implicit none
  double precision, intent(in) :: val1(:,:)  !! Input 1
  double precision, intent(in) :: val2(size(val1,1),size(val1,2))  !! Input 2
  double precision, intent(in), optional :: undef  !! Undefined value
  character(*), intent(out), optional :: cout  !! Maximum value by character
  real, intent(out), optional :: vout  !! Maximum value by float
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
  if(present(vout))then
     vout=maxv
  end if

end subroutine display_2valdiff_max

!--------------------------------------------------
!--------------------------------------------------

subroutine stdout( message, routine_name, mtype )
!! Standard output for message
  implicit none
  character(*), intent(in) :: message  !! output message
  character(*), intent(in) :: routine_name  !! called routine name
  integer, intent(in) :: mtype   !! message type
                                 !! 0: message, -1: error, 1: warning
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

!------------------------------------------------------------!
! Reuse the subroutine from the STPK library (0.9.20.0)      !
! STPK library (LGPL2.1):                                    !
! https://www.gfd-dennou.org/library/davis/stpk/index.htm.en !
!------------------------------------------------------------!

subroutine fp_gauss( c, d, x )
!! Gauss-Jordan method with fully pivotting (from STPK)
  implicit none
  double precision, intent(in) :: d(:)  !! Vector
  double precision, intent(in) :: c(size(d),size(d))  !! Square matrix (array is the first elements)
  double precision, intent(inout) :: x(size(d))  !! Vector for unknown variables

!-- internal variables
  double precision :: b(size(d))  ! == d
  double precision :: a(size(d),size(d))  ! == c
  double precision :: s, pivotb  ! working variables for pivotting
  double precision :: pivot(size(d)+1), y(size(d))  ! working variables for pivotting
  integer :: i, j, k, nmax, pivi, pivj, ipv
  integer :: ipivot(size(d))  ! temporary store of the original values for replacement

  nmax=size(b)

  do k=1,nmax
     do j=1,nmax
        a(k,j)=c(j,k)   ! Replacement between the column and array
     end do
     b(k)=d(k)
     ipivot(k)=k
  end do

!-- Forward erasure ---
!-- Forward erasure for a(i,j) ---
  do k=1,nmax-1
!-- Start the pivotting procedure ---
!-- Determine the maximum value in the matrix elements ---
     pivi=k
     pivj=k
     do i=k,nmax
        do j=k,nmax
           if(dabs(a(i,j)).gt.dabs(a(pivi,pivj)))then
              pivi=i
              pivj=j
           end if
        end do
     end do
     ipv=ipivot(pivj)
     ipivot(pivj)=ipivot(k)
     ipivot(k)=ipv
     !-- Replacing the column with the maximum value to the current column ---
     do i=1,nmax
        pivot(i)=a(i,k)
        a(i,k)=a(i,pivj)
        a(i,pivj)=pivot(i)
     end do
     !-- Replacing the array with the maximum value to the current array ---
     do j=k,nmax
        pivot(j)=a(k,j)
        a(k,j)=a(pivi,j)
        a(pivi,j)=pivot(j)
     end do
     pivotb=b(k)
     b(k)=b(pivi)
     b(pivi)=pivotb
     if(dabs(a(k,k))<=1.0d-10)then
     write(*,*) "detect small", a(k,k:nmax)
     end if
!-- End the pivotting procedure ---
     do i=k+1,nmax
        a(k,i)=a(k,i)/a(k,k)
     end do
     b(k)=b(k)/a(k,k)
     a(k,k)=1.0d0

     do j=k+1,nmax
        do i=k+1,nmax
            a(j,i)=a(j,i)-a(k,i)*a(j,k)
        end do
        b(j)=b(j)-b(k)*a(j,k)
        a(j,k)=0.0d0
     end do
  end do

  b(nmax)=b(nmax)/a(nmax,nmax)
  a(nmax,nmax)=1.0d0

!-- Back substitution of x(i)
  y(nmax)=b(nmax)
  do i=nmax-1,1,-1
     s=b(i)
     do j=i+1,nmax
        s=s-a(i,j)*y(j)
     end do
     y(i)=s
  end do

  do i=1,nmax
     x(ipivot(i))=y(i)
  end do

end subroutine fp_gauss

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine fp_invert_mat( ax, xx )
!! Calculate the inverse "xx" for the matrix "ax" (from STPK)
  implicit none
  double precision, intent(in) :: ax(:,:)  !! Input matrix
  double precision, intent(inout) :: xx(size(ax,1),size(ax,2))  !! Inverse
  integer :: i, j, k
  double precision :: c(size(ax,1),size(ax,2))
  double precision :: d(size(ax,1),size(ax,2))
  integer :: nx

  nx=size(ax,1)

  c=0.0d0

  do i=1,nx
     c(i,i)=1.0d0
  end do

  d(1:nx,1:nx)=ax(1:nx,1:nx)

!$omp parallel default(shared)
!$omp do schedule(runtime) private(i)

  do i=1,nx
     call fp_gauss( d, c(1:nx,i), xx(1:nx,i) )
  end do

!$omp end do
!$omp end parallel

end subroutine fp_invert_mat

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine tangent_conv_scal( x, y, xc, yc, u, r, theta, v,  &
  &                           undef, undefg, stdopt )
  !! Convert the Cartesian grid to polar grid with the origin of
  !!  the storm center (from STPK)
  !! The procedure: 
  !! (1) Define the given polar grid (r-theta) on the Cartesian grid
  !! (2) Search the 4 nearest points on the Cartesian grid (x-y) for each polar grid point
  !! (3) Performing the bilinear interpolation of the 4 values defined on the
  !!     Cartesian grid to the polar grid. 
  implicit none
  double precision, intent(in) :: x(:)  !! X-coordinate on the Cartesian grid
  double precision, intent(in) :: y(:)  !! Y-coordinate on the Cartesian grid
  double precision, intent(in) :: u(size(x),size(y))  !! Values defined on the Cartesian grid
  double precision, intent(in) :: xc    !! X-component of the storm center
  double precision, intent(in) :: yc    !! Y-component of the storm center
  double precision, intent(in) :: r(:)  !! R-coordinate on the polar grid with the origin (xc, yc).
  double precision, intent(in) :: theta(:)  !! theta-coordinate of the polar grid with the origin (xc, yc) [rad]
  double precision, intent(out) :: v(size(r),size(theta))  !! Values converted to the polar grid
  double precision, intent(in), optional :: undef   !! Missing value for the outside of the polar grid area (default: -999.0)
  double precision, intent(in), optional :: undefg  !! Missing value for the inside of the polar grid area (default: -999.0)
  logical, intent(in), optional :: stdopt  !! Display debug messages.
                                           !! (default: .false. == No display)

  !-- internal variables
  integer :: i, j, nx, ny, nr, nt, i_undef
  double precision :: r_undef, r_undefg
  double precision :: work(size(r),size(theta))
  double precision :: point(size(r),size(theta),2)
  integer :: ip(size(r),size(theta),2)
  double precision :: tmpx(2), tmpy(2), tmpz(2,2), inter(2)
  double precision :: tmppointd1, tmppointd2
  logical :: ucf, stderr

  nx=size(x)
  ny=size(y)
  nr=size(r)
  nt=size(theta)

  i_undef=0

  if(present(undef))then
    r_undef=undef
  else
    r_undef=-999.0d0
  end if

  if(present(undefg))then
    r_undefg=undefg
  else
    r_undefg=-999.0d0
  end if

  if(present(stdopt))then
     stderr=stdopt
  else
     stderr=.false.
  end if

!-- Process (1) ---
  do j=1,nt
     do i=1,nr
        call rt_2_xy( r(i), theta(j), point(i,j,1), point(i,j,2) )
        point(i,j,1)=xc+point(i,j,1)
        point(i,j,2)=yc+point(i,j,2)
     end do
  end do

!-- Process (2) ---
  do j=1,nt
     do i=1,nr
        call interpo_search_2d( x, y, point(i,j,1), point(i,j,2),  &
      &                         ip(i,j,1), ip(i,j,2), undeff=i_undef,  &
  &                             stdopt=stderr )
     end do
  end do

!-- Process (3) ---
  do j=1,nt
     do i=1,nr
        if(ip(i,j,1)/=i_undef.and.ip(i,j,2)/=i_undef.and.  &
  &        ip(i,j,1)/=nx.and.ip(i,j,2)/=ny)then
           tmpx(1)=x(ip(i,j,1))
           tmpx(2)=x(ip(i,j,1)+1)
           tmpy(1)=y(ip(i,j,2))
           tmpy(2)=y(ip(i,j,2)+1)
           tmpz(1,1)=u(ip(i,j,1),ip(i,j,2))
           tmpz(2,1)=u(ip(i,j,1)+1,ip(i,j,2))
           tmpz(1,2)=u(ip(i,j,1),ip(i,j,2)+1)
           tmpz(2,2)=u(ip(i,j,1)+1,ip(i,j,2)+1)
           inter(1)=point(i,j,1)
           inter(2)=point(i,j,2)

           if(present(undefg))then
              ucf=undef_checker_2d( tmpz, undefg )
              if(ucf.eqv..false.)then
                 call interpolation_2d( tmpx, tmpy, tmpz, inter, work(i,j) )
              else
                 work(i,j)=r_undefg
              end if
           else
              call interpolation_2d( tmpx, tmpy, tmpz, inter, work(i,j) )
           end if
        else
           work(i,j)=r_undef
        end if
     end do
  end do

  do j=1,nt
     do i=1,nr
        v(i,j)=work(i,j)
     end do
  end do

end subroutine tangent_conv_scal

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine cart_conv_scal( r, theta, v, x, y, xc, yc, u,  &
  &                        undef, undefg, stdopt )
  !! Convert polar grid with the origin of the storm center to 
  !!  the Cartesian grid (from STPK)
  !! The procedure: 
  !! (1) Define the given Cartesian grid (x-y) on the polar grid
  !! (2) Search the 4 nearest points on the polar grid for each Cartesian grid point
  !! (3) Performing the bilinear interpolation of the 4 values defined on the
  !!     polar grid to the Cartesian grid. 
  implicit none
  double precision, intent(in) :: r(:)  !! R-coordinate on the polar grid with the origin (xc, yc).
  double precision, intent(in) :: theta(:)  !! theta-coordinate of the polar grid with the origin (xc, yc) [rad]
  double precision, intent(in) :: v(size(r),size(theta))  !! Values defined on the polar grid
  double precision, intent(in) :: x(:)  !! X-coordinate on the Cartesian grid
  double precision, intent(in) :: y(:)  !! Y-coordinate on the Cartesian grid
  double precision, intent(in) :: xc    !! X-component of the storm center
  double precision, intent(in) :: yc    !! Y-component of the storm center
  double precision, intent(out) :: u(size(x),size(y))  !! Values converted to the Cartesian grid
  double precision, intent(in), optional :: undef   !! Missing value for the outside of the Cartesian grid area (default: -999.0)
  double precision, intent(in), optional :: undefg  !! Missing value for the inside of the Cartesian grid area (default: -999.0)
  logical, intent(in), optional :: stdopt  !! Display debug messages.
                                           !! (default: .false. == No display)

  !-- internal variables
  integer :: i, j, nx, ny, nr, nt, i_undef
  double precision :: r_undef, r_undefg
  double precision :: work(size(x),size(y))
  double precision :: point(size(x),size(y),2)
  integer :: ip(size(x),size(y),2)
  double precision :: tmpx(2), tmpy(2), tmpz(2,2), inter(2)
  double precision :: tmppoint1, tmppoint2
  logical :: ucf, stderr

  nx=size(x)
  ny=size(y)
  nr=size(r)
  nt=size(theta)

  i_undef=0

  if(present(undef))then
    r_undef=undef
  else
    r_undef=-999.0d0
  end if

  if(present(undefg))then
    r_undefg=undefg
  else
    r_undefg=-999.0d0
  end if

  if(present(stdopt))then
     stderr=stdopt
  else
     stderr=.false.
  end if

!-- Process (1) ---
   do j=1,ny
      do i=1,nx
         call xy_2_rt( x(i), y(j), xc, yc, point(i,j,1), point(i,j,2) )
!         if(point(i,j,2)<0.0d0)then
!            point(i,j,2)=point(i,j,2)+2.0d0*pi_dp
!         end if
         if(point(i,j,2)<theta(1))then  ! +2pi
            do while(point(i,j,2)<theta(1))
               point(i,j,2)=point(i,j,2)+2.0d0*pi_dp
            end do
         else if(point(i,j,2)>theta(nt))then  ! -2pi
            do while(point(i,j,2)>theta(nt))
               point(i,j,2)=point(i,j,2)-2.0d0*pi_dp
            end do
         end if
      end do
   end do

!-- Process (2) ---
  do j=1,ny
     do i=1,nx
        call interpo_search_2d( r, theta, point(i,j,1), point(i,j,2),  &
      &                         ip(i,j,1), ip(i,j,2), undeff=i_undef,  &
  &                             stdopt=stderr )
     end do
  end do

!-- Process (3) ---
  do j=1,ny
     do i=1,nx
        if(ip(i,j,1)/=i_undef.and.ip(i,j,2)/=i_undef.and.  &
  &        ip(i,j,1)/=nr.and.ip(i,j,2)/=nt)then
           tmpx(1)=r(ip(i,j,1))
           tmpx(2)=r(ip(i,j,1)+1)
           tmpy(1)=theta(ip(i,j,2))
           tmpy(2)=theta(ip(i,j,2)+1)
           tmpz(1,1)=v(ip(i,j,1),ip(i,j,2))
           tmpz(2,1)=v(ip(i,j,1)+1,ip(i,j,2))
           tmpz(1,2)=v(ip(i,j,1),ip(i,j,2)+1)
           tmpz(2,2)=v(ip(i,j,1)+1,ip(i,j,2)+1)
           inter(1)=point(i,j,1)
           inter(2)=point(i,j,2)

           if(present(undefg))then
              ucf=undef_checker_2d( tmpz, undefg )
              if(ucf.eqv..false.)then
                 call interpolation_2d( tmpx, tmpy, tmpz, inter, work(i,j) )
              else
                 work(i,j)=r_undefg
              end if
           else
              call interpolation_2d( tmpx, tmpy, tmpz, inter, work(i,j) )
           end if
        else
           work(i,j)=r_undef
        end if
     end do
  end do

  do j=1,ny
     do i=1,nx
        u(i,j)=work(i,j)
     end do
  end do

end subroutine cart_conv_scal

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine Mean_1d( x, ave, error, nc )
!! Average x
  implicit none
  double precision, intent(in) :: x(:)    !! Input data
  double precision, intent(inout) :: ave  !! Output mean value
  double precision, intent(in), optional :: error  !! Missing value
  integer, intent(inout), optional :: nc  !! Number of sampling data without error
  integer :: i, nt
  integer :: nx  ! sampling number of x
  double precision :: summ

  summ=0.0d0
  nt=0
  nx=size(x)

  if(present(error))then
     do i=1,nx
        if(x(i)/=error)then
           summ=summ+x(i)
           nt=1+nt
        end if
     end do

     if(nt/=0)then
        ave=summ/dble(nt)
     else
        ave=error
     end if

     if(present(nc))then
        nc=nt
     end if

  else

     do i=1,nx
        summ=summ+x(i)
     end do

     ave=summ/dble(nx)

  end if

end subroutine Mean_1d

!--------------------------------------------------------------
!--------------------------------------------------------------

logical function undef_checker_2d( val, undef )
!! Check missing value in "val"
  implicit none
  double precision, dimension(:,:), intent(in) :: val  !! Input
  double precision, intent(in) :: undef  !! Undefined value
  integer :: i, nx
  logical :: checker

  nx=size(val,2)
  checker=.false.

  do i=1,nx
     checker=undef_checker_1d( val(:,i), undef )
     if(checker.eqv..true.)then
        exit
     end if
  end do

  undef_checker_2d=checker

  return
end function undef_checker_2d

!--------------------------------------------------------------
!--------------------------------------------------------------

logical function undef_checker_1d( val, undef )
!! Check missing value in "val"
  implicit none
  double precision, dimension(:), intent(in) :: val  !! Input
  double precision, intent(in) :: undef  !! Undefined value
  integer :: i, nx
  logical :: checker

  nx=size(val)
  checker=.false.

  do i=1,nx
     if(val(i)==undef)then
        checker=.true.
        exit
     end if
  end do

  undef_checker_1d=checker

  return
end function undef_checker_1d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine interpo_search_2d( x, y, pointx, pointy, i, j, undeff, stdopt )
  !! Floor function for the real grid points (from STPK)
  implicit none
  double precision, intent(in) :: x(:)  !! X-coordinate
  double precision, intent(in) :: y(:)  !! Y-coordinate
  double precision, intent(in) :: pointx  !! The X point in real
  double precision, intent(in) :: pointy  !! The Y point in real
  integer, intent(out) :: i  !! floor(pointx)
  integer, intent(out) :: j  !! floor(pointy)
  integer, intent(in), optional :: undeff  !! In case of (x(1)>pointx or y(1)>pointy), the value returned to i and j
                                           !! (default = 0)
  logical, intent(in), optional :: stdopt  !! Display debug messages
                                           !! (default = .false. = Not display)

  !-- internal variables
  integer :: just
  logical :: stderr

  if(present(stdopt))then
     stderr=stdopt
  else
     stderr=.false.
  end if

  if(present(undeff))then
     just=undeff
     call interpo_search_1d( x, pointx, i, just, stdopt=stderr )
     call interpo_search_1d( y, pointy, j, just, stdopt=stderr )
  else
     call interpo_search_1d( x, pointx, i, stdopt=stderr )
     call interpo_search_1d( y, pointy, j, stdopt=stderr )
  end if

end subroutine interpo_search_2d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine interpo_search_1d( x, point, i, undeff, stdopt )
  !! Floor function for the real grid points (from STPK)
  implicit none
  double precision, intent(in) :: x(:)  !! X-coordinate
  double precision, intent(in) :: point  !! The X point in real
  integer, intent(out) :: i  !! floor(pointx)
  integer, intent(in), optional :: undeff  !! In case of (x(1)>pointx or y(1)>pointy), the value returned to i and j
                                           !! (default = 0)
  logical, intent(in), optional :: stdopt  !! Display debug messages
                                           !! (default = .false. = Not display)

  !-- internal variables
  integer :: nx, j
  integer :: just
  logical :: stderr

  nx=size(x)
  if(present(undeff))then
     just=undeff
  else
     just=0
  end if

  if(present(stdopt))then
     stderr=stdopt
  else
     stderr=.false.
  end if

  if(x(1)>point)then

     if(stderr.eqv..false.)then
        write(*,*) "****** WARNING ******"
        write(*,*) "searching point was not found :", x(1), point
        write(*,*) "Abort. Exit.!!!"
     end if
     i=just

  else

     do j=1,nx
        if(x(j)<=point)then
           i=j
        else
           exit
        end if
     end do

  end if

end subroutine interpo_search_1d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine xy_2_rt( x, y, xc, yc, r, t )
!! Convert the Cartesian (x-y) grid to the polar (r-t) grid (from STPK)
  implicit none
  double precision, intent(in) :: x  !! X-coordinate [m]
  double precision, intent(in) :: y  !! Y-coordinate [m]
  double precision, intent(in) :: xc !! X-coordinate of the center on the polar grid
  double precision, intent(in) :: yc !! Y-coordinate of the center on the polar grid
  double precision, intent(out) :: r  !! Radius [m]
  double precision, intent(out) :: t  !! Angle [rad]
  double precision :: rx, ry

  rx=x-xc
  ry=y-yc

  r=dsqrt(rx**2+ry**2)

  if(rx==0.0d0.and.ry==0.0d0)then
     t=0.0d0
  else if(rx==0.0d0.and.ry/=0.0d0)then
     if(ry>0.0d0)then
        t=0.5d0*pi_dp
     else
        t=1.5d0*pi_dp
     end if
  else if(rx/=0.0d0.and.ry==0.0d0)then
     if(rx>0.0d0)then
        t=0.0d0
     else
        t=pi_dp
     end if
  else
     if(rx>0.0d0.and.ry>0.0d0)then
        t=datan(ry/rx)
     else if(rx<0.0d0.and.ry>0.0d0)then
        t=pi_dp-datan(ry/dabs(rx))
     else if(rx>0.0d0.and.ry<0.0d0)then
        t=2.0d0*pi_dp-datan(dabs(ry)/rx)
     else if(rx<0.0d0.and.ry<0.0d0)then
        t=pi_dp+datan(ry/rx)
     end if
  end if

end subroutine xy_2_rt

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine rt_2_xy( r, t, x, y )
!! Convert the polar grid (r-t) to the Cartesian (x-y) grid (from STPK)
  implicit none
  double precision, intent(in) :: r  !! Radius [m]
  double precision, intent(in) :: t  !! Angle [rad]
  double precision, intent(out) :: x  !! X-coordinate [m]
  double precision, intent(out) :: y  !! Y-coordinate [m]

  x=r*dcos(t)
  y=r*dsin(t)

end subroutine rt_2_xy

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine interpolation_2d( x, y, z, point, val )
  !! Perform bilinear interpolation to the "point" on the Cartesian (x-y) grid (from STPK)
  implicit none
  double precision, intent(in) :: x(2)  !! The nearest west and east points for "point"
  double precision, intent(in) :: y(2)  !! The nearest south and north points for "point"
  double precision, intent(in) :: z(2,2)  !! Values defined at (x,y).
                                          !! z(1,1) at x(1), y(1)
                                          !! z(1,2) at x(1), y(2)
                                          !! z(2,1) at x(2), y(1)
                                          !! z(2,2) at x(2), y(2)
  double precision, intent(in) :: point(2)  !! The target point for the interpolation
  double precision, intent(out) :: val  !! Interpolated value

  ! internal variables
  double precision :: valx(2)

  call interpolation_1d( x, (/z(1,1), z(2,1)/), point(1), valx(1) )
  call interpolation_1d( x, (/z(1,2), z(2,2)/), point(1), valx(2) )
  call interpolation_1d( y, valx, point(2), val )

end subroutine interpolation_2d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine interpolation_1d( x, y, point, val )
  !! Perform linear interpolation to the "point" on the Cartesian (x) grid (from STPK)
  implicit none
  double precision, intent(in) :: x(2)  !! The nearest west and east points for "point"
  double precision, intent(in) :: y(2)  !! Values defined at x.
                                        !! y(1) at x(1)
                                        !! y(2) at x(2)
  double precision, intent(in) :: point !! The target point for the interpolation
  double precision, intent(out) :: val  !! Interpolated value

  ! internal variables
  double precision :: fd, dt
  double precision :: tmin
  double precision :: tmax
  double precision :: xmin
  double precision :: xmax

  tmin=x(1)
  tmax=x(2)

  xmin=y(1)
  xmax=y(2)

  dt=point-tmin

  fd=(xmax-xmin)/(tmax-tmin)

  val=xmin+dt*fd

end subroutine interpolation_1d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine auto_interpolation_2d( x, y, r, q, u, v, undef, stdopt )
  !! Automatic interpolation for 2d data (continuous running of interpolation_2d)
  implicit none
  double precision, intent(in) :: x(:)  !! reference grid 1
  double precision, intent(in) :: y(:)  !! reference grid 2
  double precision, intent(in) :: r(:)  !! target grid 1
  double precision, intent(in) :: q(:)  !! target grid 2
  double precision, intent(in) :: u(size(x),size(y))  !! reference data
  double precision, intent(inout) :: v(size(r),size(q))  !! interpolated data
  double precision, intent(in), optional :: undef  !! undefined value
  logical, intent(in), optional :: stdopt  !! Display debug messages.
                                           !! (default: .false. == No display)
  integer :: ir(size(r)), iq(size(q))
  integer :: i, j, nx, ny, nr, nq
  double precision :: defun
  logical :: stderr

  nx=size(x)
  ny=size(y)
  nr=size(r)
  nq=size(q)

  if(present(undef))then
     defun=undef
  else
     defun=-999.0d0
  end if

  if(present(stdopt))then
     stderr=stdopt
  else
     stderr=.false.
  end if

  call auto_interpo_search_2d( x, y, r, q, ir, iq, undeff=0, stdopt=stderr )

  do j=1, nq
     do i=1, nr
        if(ir(i)/=0.and.iq(j)/=0)then
           if(u(ir(i),iq(j))/=defun)then
              if(ir(i)<nx.and.iq(j)<ny)then
                 if(u(ir(i),iq(j)+1)/=defun.and.  &
  &                 u(ir(i)+1,iq(j))/=defun.and.  &
  &                 u(ir(i)+1,iq(j)+1)/=defun)then
                    call interpolation_2d( x(ir(i):ir(i)+1),  &
  &                                        y(iq(j):iq(j)+1),  &
  &                                        u(ir(i):ir(i)+1,iq(j):iq(j)+1),  &
  &                                        (/r(i), q(j)/), v(i,j) )
                 else
                    v(i,j)=defun
                 end if

              else if(x(nx)==r(i).and.y(ny)==q(j))then
                 v(i,j)=u(nx,ny)

              else if(x(nx)==r(i).and.iq(j)<ny)then
                 if(u(nx,iq(j)+1)/=defun)then
                    call interpolation_1d( y(iq(j):iq(j)+1),  &
  &                                        u(nx,iq(j):iq(j)+1),  &
  &                                        q(j), v(i,j) )
                 else
                    v(i,j)=defun
                 end if

              else if(y(ny)==q(j).and.ir(i)<nx)then
                 if(u(ir(i)+1,ny)/=defun)then
                    call interpolation_1d( x(ir(i):ir(i)+1),  &
  &                                        u(ir(i):ir(i)+1,ny),  &
  &                                        r(i), v(i,j) )
                 else
                    v(i,j)=defun
                 end if
              else
                 v(i,j)=defun
              end if
           else
              v(i,j)=defun
           end if
        else
           v(i,j)=defun
        end if
     end do
  end do

end subroutine auto_interpolation_2d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine auto_interpo_search_1d( x, point, i, undeff, stdopt )
  !! continuous running of interpo_search_1d
  implicit none
  double precision, intent(in) :: x(:)  !! gradual increasing array
  double precision, intent(in) :: point(:)  !! searching points
  integer, intent(inout) :: i(size(point)) !! floor for each "point"
  integer, intent(in), optional :: undeff  !! In case of (x(1)>pointx), the value returned to i
                                           !! (default = 0)
  logical, intent(in), optional :: stdopt  !! Display debug messages.
                                           !! (default: .false. == No display)
  integer :: nx, ni, j, icount, jcount
  integer :: just
  logical :: stderr

  nx=size(x)
  ni=size(point)

  if(present(undeff))then
     just=undeff
  else
     just=0
  end if

  if(present(stdopt))then
     stderr=stdopt
  else
     stderr=.false.
  end if

  jcount=1

  do j=1,ni
     if(x(1)>point(j))then
        if(stderr.eqv..false.)then
           write(*,*) "****** WARNING ******"
           write(*,*) "searching point was not found :", x(1), point(j)
           write(*,*) "Abort. Exit.!!!"
        end if
        i(j)=just
     else
        jcount=j
        exit
     end if
  end do

  icount=1

  do j=jcount,ni
     if(x(icount)<=point(j))then
        do while(x(icount)<=point(j))
           i(j)=icount
           icount=icount+1

           if(icount>nx)then
              i(j:ni)=nx
              exit
           end if
        end do
        icount=icount-1
     else
        icount=icount+1
     end if
  end do

end subroutine auto_interpo_search_1d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine auto_interpo_search_2d( x, y, pointx, pointy, i, j, undeff, stdopt )
  !! continuous running of auto_interpo_search_1d
  implicit none
  double precision, intent(in) :: x(:)  !! gradual increasing array 1
  double precision, intent(in) :: y(:)  !! gradual increasing array 2
  double precision, intent(in) :: pointx(:)  !! searching points for x
  double precision, intent(in) :: pointy(:)  !! searching points for y
  integer, intent(inout) :: i(size(pointx))  !! floor for pointx
  integer, intent(inout) :: j(size(pointy))  !! floor for pointy
  integer, intent(in), optional :: undeff  !! In case of (x(1)>pointx or y(1)>pointy), the value returned to i and j
                                           !! (default = 0)
  logical, intent(in), optional :: stdopt  !! 探索範囲が見つからない旨の標準出力を表示させないようにする.
  integer :: just
  logical :: stderr

  if(present(stdopt))then
     stderr=stdopt
  else
     stderr=.false.
  end if

  if(present(undeff))then
     just=undeff
     call auto_interpo_search_1d( x, pointx, i, just, stdopt=stderr )
     call auto_interpo_search_1d( y, pointy, j, just, stdopt=stderr )
  else
     call auto_interpo_search_1d( x, pointx, i, stdopt=stderr )
     call auto_interpo_search_1d( y, pointy, j, stdopt=stderr )
  end if

end subroutine auto_interpo_search_2d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine max_val_1d(var, mamv, undef)
  !! Get the max value (from STPK)
  implicit none
  double precision, intent(in) :: var(:)  !! Searched array
  double precision, intent(inout) :: mamv  !! Max value in var
  double precision, intent(in), optional :: undef  !! undefined value
  integer :: nx
  integer :: i
  logical :: undeflag

  nx=size(var)
  undeflag=.true.

  if(present(undef))then

     do i=1,nx
        if(var(i)/=undef)then
           if(undeflag.eqv..true.)then
              undeflag=.false.
              mamv=dabs(var(i))
           else
              if(dabs(var(i))>mamv)then
                 mamv=dabs(var(i))
              end if
           end if
        end if
     end do

     if(undeflag.eqv..true.)then
        mamv=undef
     end if

  else

     mamv=dabs(var(1))

     do i=2,nx
        if(dabs(var(i))>mamv)then
           mamv=dabs(var(i))
        end if
     end do
  end if

end subroutine max_val_1d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine stand_devi( x, true_val, anor, undef )
  !! Calculate RMSE of x for the "true_val" (from STPK) <br>
  !! Definition: RMSE \(= \sqrt{\sum^{N}_{i=1}{(1/N)(x(i)-x_t)^2}}, \) <br>
  !!             \(x_t=\mathrm{true\_val} \)
  implicit none
  double precision, intent(in) :: x(:)  !! sampling data
  double precision, intent(in) :: true_val  !! reference data
  double precision, intent(out) :: anor  !! RMSE
  double precision, intent(in), optional :: undef  !! missing value
  integer :: i
  integer :: nx  ! data number
  integer :: nt
  double precision :: anorval

  nx=size(x)
  anorval=0.0d0

  if(present(undef))then
     nt=0
     do i=1,nx
        if(x(i)/=undef)then
           anorval=anorval+(x(i)-true_val)**2
           nt=nt+1
        end if
     end do
     if(anorval/=undef.and.nt/=0)then
        anorval=dsqrt(anorval/dble(nt))
     end if
  else
     do i=1,nx
        anorval=anorval+(x(i)-true_val)**2
     end do
     anorval=dsqrt(anorval/dble(nx))
  end if

  anor=anorval

end subroutine stand_devi

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine ll2rt( lon0, lat0, lon1, lat1, r, theta )
  !! calculate radial and azimuthal location at (lon1,lat1) on the 
  !! polar coordinate with the center of (lon0,lat0)
  implicit none
  double precision, intent(in) :: lon0      !! the center longitude (rad)
  double precision, intent(in) :: lat0      !! the center latitude (rad)
  double precision, intent(in) :: lon1      !! target longitude (rad)
  double precision, intent(in) :: lat1      !! target latitude (rad)
  double precision, intent(inout) :: r      !! radial distance (m)
  double precision, intent(inout) :: theta  !! azimuthal angle (rad)
  double precision :: tmpcos, tmpsin, tmptan

  r=ll2radi( lon0, lat0, lon1, lat1 )

  if(r>0.0d0)then
     tmpcos=dcos(lat1)*dsin(lon1-lon0)
     tmpsin=dsin(lat1)*dcos(lat0)-dcos(lat1)*dsin(lat0)*dcos(lon1-lon0)
     if(tmpcos==0.0d0.and.tmpsin==0.0d0)then
        theta=0.0d0
     else if(tmpcos==0.0d0.and.tmpsin>0.0d0)then
        theta=0.5d0*pi_dp
     else if(tmpcos==0.0d0.and.tmpsin<0.0d0)then
        theta=1.5d0*pi_dp
     else if(tmpcos>0.0d0.and.tmpsin==0.0d0)then
        theta=0.0d0
     else if(tmpcos<0.0d0.and.tmpsin==0.0d0)then
        theta=pi_dp
     else
        if(tmpcos>0.0d0.and.tmpsin>0.0d0)then
           tmptan=tmpsin/tmpcos
           theta=datan(tmptan)
        else if(tmpcos<0.0d0.and.tmpsin>0.0d0)then
           tmptan=tmpsin/dabs(tmpcos)
           theta=pi_dp-datan(tmptan)
        else if(tmpcos>0.0d0.and.tmpsin<0.0d0)then
           tmptan=dabs(tmpsin)/tmpcos
           theta=2.0d0*pi_dp-datan(tmptan)
        else if(tmpcos<0.0d0.and.tmpsin<0.0d0)then
           tmptan=tmpsin/tmpcos
           theta=pi_dp+datan(tmptan)
        end if
     end if
  else
     theta=0.0d0
  end if

end subroutine ll2rt

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine rt2ll( r, theta, lon0, lat0, lon, lat )
  !! calculate (lon,lat) from radial and azimuthal grid (r,theta) 
  !! on the polar coordinate with the origin of (lon0,lat0)
  implicit none
  double precision, intent(in) :: r       !! radial distance from (lon0,lat0) (m)
  double precision, intent(in) :: theta   !! azimuthal angle from (lon0,lat0) (rad)
  double precision, intent(in) :: lon0    !! longitude of the center on the polar coordinate (rad)
  double precision, intent(in) :: lat0    !! latitude of the center on the polar coordinate (rad)
  double precision, intent(inout) :: lon  !! target longitude (rad)
  double precision, intent(inout) :: lat  !! target latitude (rad)
  double precision :: thetad, lond, latd, tmplon, tmplat, rratio

  thetad=180.0d0*theta/pi_dp
  lond=180.0d0*lon0/pi_dp
  latd=180.0d0*lat0/pi_dp
  rratio=r/radius_dp

  do while(thetad>360.0d0)
     thetad=thetad-360.0d0
  end do

  if(thetad==-90.0d0.or.thetad==270.0d0)then
     lon=lon0
     lat=lat0-rratio
  else if(thetad==90.0d0)then
     lon=lon0
     lat=lat0+rratio
  else if((-90.0d0<thetad.and.90.0d0>thetad).or.  &
  &  (270.0d0<thetad.and.360.0d0>=thetad))then
     tmplat=dcos(lat0)*dsin(rratio)*dsin(theta)+dsin(lat0)*dcos(rratio)
     lat=dasin(tmplat)
     tmplon=dsin(rratio)*dcos(theta)/dcos(dasin(tmplat))
     lon=lon0+dasin(tmplon)
  else if((90.0d0<thetad.and.270.0d0>thetad).or.  &
  &       (-180.0d0<=thetad.and.-90.0d0>thetad))then
     tmplat=dcos(lat0)*dsin(rratio)*dsin(theta)+dsin(lat0)*dcos(rratio)
     lat=dasin(tmplat)
     tmplon=-dsin(rratio)*dcos(theta)/dcos(dasin(tmplat))
     lon=lon0-dasin(tmplon)
  else
     write(*,*) "### ERROR : (rt2ll:Map_Function)"
     write(*,*) "argument 'theta' is not valid : ", theta
     write(*,*) "STOP."
     stop
  end if

end subroutine rt2ll

!--------------------------------------------------------------
!--------------------------------------------------------------

real function ll2radi( lon1, lat1, lon2, lat2, forcef )
  !! calculate arc distance between two points on the sphere
  implicit none
  double precision, intent(in) :: lon1    !! longitude1 (rad)
  double precision, intent(in) :: lat1    !! latitude1 (rad)
  double precision, intent(in) :: lon2    !! longitude2 (rad)
  double precision, intent(in) :: lat2    !! latitude2 (rad)
  logical, intent(in), optional :: forcef !! truncating flag for numerical error, 
                                          !! default = .false. (no truncating)
  double precision :: lond1, lond2, latd1, latd2, tmp
  logical :: fflag

  lond1=lon1
  lond2=lon2
  latd1=lat1
  latd2=lat2

  if(present(forcef))then
     fflag=forcef
  else
     fflag=.false.
  end if

  if(lond1==lond2.and.latd1==latd2)then
     ll2radi=0.0d0
  else
     tmp=dsin(latd1)*dsin(latd2)+dcos(latd1)*dcos(latd2)*dcos(lond2-lond1)
     if(tmp<-1.0d0.or.tmp>1.0d0)then
        if(fflag.eqv..true.)then
           write(*,*) "*** WARGNING (ll2radi) *** : Detect over 1",  &
  &                   tmp, latd1, latd2, lond1, lond2
           write(*,*) "tmp value is forced to 1. "
           if(tmp<-1.0d0)then
              tmp=-1.0d0
           else
              tmp=1.0d0
           end if
        else
           write(*,*) "*** ERROR (ll2radi) *** : Detect error",  &
  &                   tmp, latd1, latd2, lond1, lond2
           stop
        end if
     end if
     ll2radi=dacos(tmp)*radius_dp
  end if

  return
end function ll2radi

!--------------------------------------------------------------
!--------------------------------------------------------------

integer function line_number_counter( fname, funit )
  !! count the line in the fname
  implicit none
  character(*), intent(in) :: fname  !! counting the file name
  integer, intent(in), optional :: funit   !! file unit (default: 11)
!  integer, intent(inout) :: res
  integer :: i, err, unitn
  character(1) :: dummy

  if(present(funit))then
     unitn=funit
  else
     unitn=11
  end if

  i=0
  open(unit=unitn,file=trim(fname),iostat=err,status='old')
  do while(err==0)
     read(unitn,*,iostat=err) dummy
     i=i+1
  end do
  close(unit=unitn)

  line_number_counter=i-1
  return

end function line_number_counter

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine read_file_text( fname, nx, ny, val, skip, forma, funit )
  !! read ASCII file
  implicit none
  character(*), intent(in) :: fname  !! file name in reading
  integer, intent(in) :: nx  !! column number for fname
  integer, intent(in) :: ny  !! line number for fname
  character(*), intent(out) :: val(nx,ny)  !! read data
  integer, intent(in), optional :: skip  !! line number for skipping from the head
  character(*), intent(in), optional :: forma  !! Fortran format in reading
  integer, intent(in), optional :: funit   !! file unit
  integer :: i, j, unitn
  character(1) :: dummy

  if(present(funit))then
     unitn=funit
  else
     unitn=11
  end if

  open(unit=unitn, file=trim(adjustl(fname)), status='old')
  if(present(skip))then
     do i=1,skip
        read(unitn,*) dummy
     end do
  end if

  if(present(forma))then
     do j=1,ny
        read(unitn,forma) (val(i,j),i=1,nx)
     end do
  else
     do j=1,ny
        read(unitn,*) (val(i,j),i=1,nx)
     end do
  end if
  close(unit=unitn)

end subroutine

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine read_file_2d( file_name, nx, ny, rec_num, var, offset, funit )
  !! read float data from 4-byte unformatted binary
  implicit none
  integer, intent(in) :: nx  !! data number in x
  integer, intent(in) :: ny  !! data number in y
  integer, intent(in) :: rec_num  !! record number for reading data
  character(*), intent(in) :: file_name  !! file name
  real, intent(out) :: var(nx,ny)  ! output data
  integer, intent(in), optional :: offset  !! offset in reading
  integer, intent(in), optional :: funit   !! file unit
  integer :: i, j, l, err, unitn  ! working variables
  integer, parameter :: bnum=4

  if(present(funit))then
     unitn=funit
  else
     unitn=11
  end if

  err=0
  if(present(offset))then
     open(unit=unitn, file=trim(adjustl(file_name)), access='direct',  &
  &       recl=bnum, status='old', iostat=err)
        if(err/=0)then
           call stdout( "File Not Found : "//trim(adjustl(file_name)), "read_file_2d", 1 )
        end if
        l=offset
        do j=1,ny
           do i=1,nx
              l=l+1
              read(unitn,rec=l,iostat=err) var(i,j)
              if(err/=0)then
                 call stdout( "Can not read : "//trim(adjustl(file_name)), "read_file_2d", 1 )
              end if
           end do
        end do
     close(unit=unitn)
  else
     open(unit=unitn, file=trim(adjustl(file_name)), access='direct',  &
  &       recl=bnum*nx*ny, status='old', iostat=err)
        if(err/=0)then
           call stdout( "File Not Found : "//trim(adjustl(file_name)), "read_file_2d", 1 )
        end if
        read(unitn,rec=rec_num,iostat=err) ((var(i,j),i=1,nx),j=1,ny)
        if(err/=0)then
           call stdout( "Can not read : "//trim(adjustl(file_name)), "read_file_2d", 1 )
        end if
     close(unit=unitn)
  end if

end subroutine read_file_2d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine read_file_3d( file_name, nx, ny, nz, rec_num, var, offset, funit )
  !! read float data from 4-byte unformatted binary
  implicit none
  integer, intent(in) :: nx  !! data number in x
  integer, intent(in) :: ny  !! data number in y
  integer, intent(in) :: nz  !! data number in z
  integer, intent(in) :: rec_num  !! record number for reading data
  character(*), intent(in) :: file_name  !! file name
  real, intent(out) :: var(nx,ny,nz)  ! output data
  integer, intent(in), optional :: offset  !! offset in reading
  integer, intent(in), optional :: funit   !! file unit
  integer :: i, j, k, l, err, unitn  ! working variables
  integer, parameter :: bnum=4

  if(present(funit))then
     unitn=funit
  else
     unitn=11
  end if

  err=0
  if(present(offset))then
     open(unit=unitn, file=trim(adjustl(file_name)), access='direct',  &
  &       recl=bnum, status='old', iostat=err)
        if(err/=0)then
           call stdout( "File Not Found : "//trim(adjustl(file_name)), "read_file_3d", 1 )
        end if
        l=offset
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 l=l+1
                 read(unitn,rec=l,iostat=err) var(i,j,k)
                 if(err/=0)then
                    call stdout( "Can not read : "//trim(adjustl(file_name)), "read_file_3d", 1 )
                 end if
              end do
           end do
        end do
     close(unit=unitn)
  else
     open(unit=unitn, file=trim(adjustl(file_name)), access='direct',  &
  &       recl=bnum*nx*ny, status='old', iostat=err)
        if(err/=0)then
           call stdout( "File Not Found : "//trim(adjustl(file_name)), "read_file_3d", 1 )
        end if
        do k=1,nz
           read(unitn,rec=rec_num+k-1,iostat=err) ((var(i,j,k),i=1,nx),j=1,ny)
           if(err/=0)then
              call stdout( "Can not read : "//trim(adjustl(file_name)), "read_file_3d", 1 )
           end if
        end do
     close(unit=unitn)
  end if

end subroutine read_file_3d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine write_file_3d( file_name, nx, ny, nz, rec_num, var, mode, funit )
  !! write float data from 4-byte unformatted binary
  implicit none
  integer, intent(in) :: nx  !! data number in x
  integer, intent(in) :: ny  !! data number in y
  integer, intent(in) :: nz  !! data number in z
  integer, intent(in) :: rec_num  !! record number for reading data
  character(*), intent(in) :: file_name  !! file name
  real, intent(in) :: var(nx,ny,nz)  !! output data
  character(*), optional, intent(in) :: mode  !! option for output
  integer, intent(in), optional :: funit   !! file unit
  integer :: i, j, k, unitn
  character(10) :: cmode
  integer, parameter :: bnum=4

  cmode=''

  if(present(funit))then
     unitn=funit
  else
     unitn=11
  end if

  if(present(mode))then
     cmode=mode
  else
     cmode='unknown'
  end if

  open(unit=unitn, file=trim(adjustl(file_name)), access='direct',  &
  &    recl=bnum*nx*ny, status=trim(cmode))
     do k=1,nz
        write(unitn,rec=rec_num+k-1) ((var(i,j,k),i=1,nx),j=1,ny)
     end do
  close(unit=unitn)

end subroutine write_file_3d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine write_file_2d( file_name, nx, ny, rec_num, var, mode, funit )
  !! write float data from 4-byte unformatted binary
  implicit none
  integer, intent(in) :: nx  !! data number in x
  integer, intent(in) :: ny  !! data number in y
  integer, intent(in) :: rec_num  !! record number for reading data
  character(*), intent(in) :: file_name  !! file name
  real, intent(in) :: var(nx,ny)  !! output data
  character(*), optional, intent(in) :: mode  !! option for output
  integer, intent(in), optional :: funit   !! file unit
  integer :: i, j, unitn
  character(10) :: cmode
  integer, parameter :: bnum=4

  cmode=''

  if(present(funit))then
     unitn=funit
  else
     unitn=11
  end if

  if(present(mode))then
     cmode=mode
  else
     cmode='unknown'
  end if

  open(unit=unitn, file=trim(adjustl(file_name)), access='direct',  &
  &    recl=bnum*nx*ny, status=trim(cmode))
     write(unitn,rec=rec_num) ((var(i,j),i=1,nx),j=1,ny)
  close(unit=unitn)

end subroutine write_file_2d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine write_file_text_add( iunit, char_in )
  !! write text in the file with iunit
  implicit none
  integer, intent(in) :: iunit  !! unit number for output file
  character(*), intent(in) :: char_in  !! strings for output
  character(100) :: forma

  forma='(a'//trim(adjustl(i2c_convert(len_trim(adjustl(char_in)))))//')'

  write(iunit,forma(1:len_trim(adjustl(forma)))) trim(adjustl(char_in))

end subroutine write_file_text_add

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine grad_1d( x, u, dudx, undef )
!! Calculate gradient of "u" in one dimension based on the 2nd order central differential approximation:<br>
!! \(\frac{\partial u}{\partial x}\approx \frac{u_{i+1}-u_{i-1}}{2dx} \)
  implicit none
  double precision, intent(in) :: x(:)  !! Axis point
  double precision, intent(in) :: u(size(x))  !! Target variable
  double precision, intent(out) :: dudx(size(x))  !! du/dx
  double precision, intent(in) :: undef  !! undefined dudxue
  integer :: i  !! iteration index
  integer :: nx  !! grid number

  nx=size(x)
  dudx=undef

  do i=2,nx-1
     if(u(i+1)/=undef.and.u(i-1)/=undef)then
        dudx(i)=(u(i+1)-u(i-1))/(x(i+1)-x(i-1))
     end if
  end do

  if(u(1)/=undef.and.u(2)/=undef)then
     dudx(1)=(u(2)-u(1))/(x(2)-x(1))
  end if
  if(u(nx)/=undef.and.u(nx-1)/=undef)then
     dudx(nx)=(u(nx)-u(nx-1))/(x(nx)-x(nx-1))
  end if

end subroutine grad_1d

!--------------------------------------------------------------
!--------------------------------------------------------------

subroutine grad_2d( x, y, u, dudx, dudy, undef )
!! Calculate gradient of "u" in two dimensions based on the 2nd order central differential approximation:<br>
!! \(\frac{\partial u}{\partial x}\approx \frac{u_{i+1}-u_{i-1}}{2dx} \)
!! \(\nabla u\approx \left(\frac{p_{i+1,j}-p_{i-1,j}}{2dx} ,\; \frac{p_{i,j+1}-p_{i,j-1}}{2dy} \right) \)
  implicit none
  double precision, intent(in) :: x(:)  !! Axis point 1
  double precision, intent(in) :: y(:)  !! Axis point 2
  double precision, intent(in) :: u(size(x),size(y))  ! Target variable
  double precision, intent(out) :: dudx(size(x),size(y))  ! du/dx
  double precision, intent(out) :: dudy(size(x),size(y))  ! du/dy
  double precision, intent(in), optional :: undef
  integer :: i, j    ! iteration index
  integer :: nx, ny  ! grid points for x and y

  nx=size(x)
  ny=size(y)

  do i=1,ny
     call grad_1d(x, u(:,i), dudx(:,i), undef)
  end do

  do i=1,nx
     call grad_1d(y, u(i,:), dudy(i,:), undef)
  end do

end subroutine grad_2d

!--------------------------------------------------------------
!--------------------------------------------------------------

character(100) function r2c_convert( rval, forma )
  !! convert float to char
  implicit none
  real, intent(in) :: rval  !! float
  character(*), intent(in), optional :: forma  !! format
  character(100) :: tmp

  if(present(forma))then
     write(tmp,trim(forma)) rval
  else
     write(tmp,*) rval
  end if

  r2c_convert=tmp

  return
end function

!--------------------------------------------------------------
!--------------------------------------------------------------

character(100) function i2c_convert( ival, forma )
  !! convert int to char
  implicit none
  integer, intent(in) :: ival  !! int
  character(*), intent(in), optional :: forma  !! format
  character(100) :: tmp

  if(present(forma))then
     write(tmp,trim(forma)) ival
  else
     write(tmp,*) ival
  end if

  i2c_convert=tmp

  return
end function

!--------------------------------------------------------------
!--------------------------------------------------------------

real function c2r_convert( cval )
  !! convert char to float
  implicit none
  character(*), intent(in) :: cval  !! char

  read(cval,*) c2r_convert

  return
end function

!--------------------------------------------------------------
!--------------------------------------------------------------

end module GVTDX_sub
