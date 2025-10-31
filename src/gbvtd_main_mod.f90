module GBVTD_main
!! The main module of GBVTD based on Lee et al. (1999, MWR)

  use GVTDX_sub

  implicit none

  public :: Retrieve_velocity_GBVTD

  private :: calc_fkj
  private :: calc_fkj2akp
  private :: calc_fkjVd2bk
  private :: set_xk2variables
  private :: calc_AB2VT
  private :: calc_Vn2Vtot
  private :: check_zero
  private :: check_undef_grid
  private :: set_undef_value

contains

subroutine Retrieve_velocity_GBVTD( nasym, r, t, td, Vd, RadTC,  &
  &                                 VT, VR, VT0, VR0, VTSn, VTCn, undef,  &
  &                                 flag_datagap )
!!  Solve unknown variables and return wind velocity on R-T coordinates
!!  based on the GBVTD technique. <br>
!!------------------------------------------------------ <br>
!!  [relationship between r and rh] -- <br>
!!------------------------------------------------------ <br>
!!    i-1    i    i+1 <br>
!!  ...|-- --|-- --|... : r(1:size(r)) = velocity radii <br>
!!------------------------------------------------------
  implicit none
  !-- input/output
  integer, intent(in) :: nasym  !! wave number for asymmetric tangential wind
  double precision, intent(in) :: r(:)   !! radial coordinate on which Vd is defined [m]
  double precision, intent(in) :: t(:)   !! azimuthal coordinate on which Vd is defined [rad]
  double precision, intent(in) :: td(size(r),size(t))  !! radar azimuthal angle defined at Vd(r,t) [rad]
  double precision, intent(inout) :: Vd(size(r),size(t))  !! Doppler velocity defined on r-t [m s-1]
  double precision, intent(in) :: RadTC                !! Distance from radar to TC center [m]
  double precision, intent(out) :: VT(size(r),size(t))  !! retrieved total tangential wind [m s-1]
  double precision, intent(out) :: VR(size(r),size(t))  !! retrieved total radial wind [m s-1]
  double precision, intent(out) :: VT0(size(r),size(t))  !! retrieved axisymmetric radial component of rotating wind [m s-1]
  double precision, intent(out) :: VR0(size(r),size(t))  !! retrieved axisymmetric tangential component of divergent wind [m s-1]
  double precision, intent(out) :: VTSn(nasym,size(r),size(t))  !! retrieved tangential component of rotating wind [m s-1]
  double precision, intent(out) :: VTCn(nasym,size(r),size(t))  !! retrieved radial component of rotating wind [m s-1]
  double precision, intent(in), optional :: undef  !! undefined value for Vd
  logical, intent(in), optional :: flag_datagap  !! Optimize the truncating wavenumber at each radius,  according to Lee et al. (2000) [Default: .false. (not optimize)]

  !-- internal variables
  integer :: i, j, k, p, cstat  ! dummy indexes
  integer :: nr, nt  ! array numbers for r and t, respectively
  integer :: nk      ! array number of a_k
  integer :: nr_out  ! outermost radius for GBVTD limit radius
  integer :: na      ! optimized nassym at each radius for flag_datagap
  integer :: nktmp   ! optimized nk at each radius for flag_datagap
  integer :: gap_nmax(size(r))  ! the continuous data gap for flag_datagap
  double precision, allocatable, dimension(:,:) :: x_k    ! unknown vector for retrieved coefficients
  double precision, allocatable, dimension(:,:) :: b_k    ! known vector given by observed values
  double precision, allocatable, dimension(:,:,:) :: a_kp ! coefficient matrix for x_k
  double precision, allocatable, dimension(:,:,:) :: f_kj ! a_kp = sum_{j}(f_kj * f_pj)
  double precision, allocatable, dimension(:) :: Vd_A0    ! Results for wavenumber-0 of Vd
  double precision, allocatable, dimension(:,:) :: Vd_AC  ! Results for cosine components of Vd
  double precision, allocatable, dimension(:,:) :: Vd_BS  ! Results for sine components of Vd
  double precision, allocatable, dimension(:,:) :: psid   ! nonlinear angle
  double precision, allocatable, dimension(:) :: td_max   ! Max of td
  double precision :: dundef, vmax
  double precision, dimension(size(r)) :: r_n             ! Nondimensional r
  double precision :: rtc_n                               ! Nondimensional RadTC
  logical, allocatable, dimension(:,:) :: undeflag ! Flag for Vd grid with undef
  logical :: gap_flag

!-- OpenMP variables
!$ integer :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS
  integer :: ompnum, omppe

  call stdout( "Enter procedure.", "Retrieve_velocity_GBVTD", 0 )

  nr=size(r)
  nt=size(t)
  vmax=50.0d0

  if(present(undef))then
     dundef=undef
  else
     dundef=-1.0d35
  end if

  if(present(flag_datagap))then
     gap_flag=flag_datagap
  else
     gap_flag=.false.
  end if

  VT=dundef
  VR=dundef
  VT0=dundef
  VR0=dundef
  VTSn=dundef
  VTCn=dundef

!-- Check the largest data gap
  if(gap_flag.eqv..true.)then
     call check_datagap( nr, nt, dundef, Vd, gap_nmax )
  end if

!-- Check retrieved asymmetric wave number
  if(nasym<0)then
     call stdout( "nasym is greater equal to 0. stop.", "Retrieve_velocity_GBVTD", -1 )
     stop
  end if

  nk=1+2*(nasym+1)

!-- Normalized r
  r_n=r/r(nr)
  rtc_n=RadTC/r(nr)

  nr_out=nr
  do i=1,nr
     if(r_n(i)>=rtc_n)then
        nr_out=i-1
        exit
     end if
  end do

!-- Allocate and initialize arrays
  ompnum=1
  omppe=1
!$   ompnum=OMP_GET_MAX_THREADS()
  allocate(Vd_A0(ompnum),stat=cstat)
  allocate(Vd_AC(nasym+1,ompnum),stat=cstat)
  allocate(Vd_BS(nasym+1,ompnum),stat=cstat)
  allocate(x_k(nk,ompnum),stat=cstat)
  allocate(b_k(nk,ompnum),stat=cstat)
  allocate(a_kp(nk,nk,ompnum),stat=cstat)
  allocate(f_kj(nk,nt,ompnum),stat=cstat)
  allocate(psid(nt,ompnum),stat=cstat)
  allocate(td_max(ompnum),stat=cstat)
  allocate(undeflag(nr,nt),stat=cstat)

  undeflag=.false.

  call check_undef_grid( Vd, dundef, undeflag )

  if(cstat/=0)then
     call stdout( "Failed to allocate variables. stop.", "Retrieve_velocity_GBVTD", -1 )
     stop
  end if

!$omp parallel default(shared)
!$omp do schedule(runtime) private(i,na,nktmp,omppe)

  do i=1,nr_out

!$   omppe=OMP_GET_THREAD_NUM()+1

     if(gap_flag.eqv..true.)then
        if(dble(gap_nmax(i))/dble(nt)<=30.0d0/360.0d0)then
           na=3
        else
           if(dble(gap_nmax(i))/dble(nt)<=60.0d0/360.0d0)then
              na=2
           else
              if(dble(gap_nmax(i))/dble(nt)<=90.0d0/360.0d0)then
                 na=1
              else
                 if(dble(gap_nmax(i))/dble(nt)<=180.0d0/360.0d0)then
                    na=0
                 else
                    na=-1  ! Not retrieval at this radius
                 end if
              end if
           end if
        end if
        if(na<nasym)then
           VTSn(na+1:nasym,i,1:nt)=0.0d0
           VTCn(na+1:nasym,i,1:nt)=0.0d0
        end if
     else
        na=nasym
     end if

     if(na>=0)then
        nktmp=1+2*(na+1)

        x_k(1:nktmp,omppe)=0.0d0
        b_k(1:nktmp,omppe)=0.0d0
        a_kp(1:nktmp,1:nktmp,omppe)=0.0d0
        f_kj(1:nktmp,1:nt,omppe)=0.0d0
        Vd_A0(omppe)=0.0d0
        Vd_AC(1:na+1,omppe)=0.0d0
        Vd_BS(1:na+1,omppe)=0.0d0
        psid(1:nt,omppe)=0.0d0

        call check_max( td(i,1:nt), td_max(omppe) )

!-- Calculate the nonlinear angle psid
        call calc_psid( rtc_n, r_n(i), t, td(i,1:nt), psid(1:nt,omppe) )

!-- Calculate f_kj
        call calc_fkj( na, nktmp, psid(1:nt,omppe),  &
  &                    f_kj(1:nktmp,1:nt,omppe), undeflag(i,1:nt) )

!-- Calculate b_k
        call calc_fkjVd2bk( vmax, f_kj(1:nktmp,1:nt,omppe), Vd(i,1:nt),  &
  &                         b_k(1:nktmp,omppe), undeflag(i,1:nt) )

!-- Calculate a_kp
        call calc_fkj2akp( f_kj(1:nktmp,1:nt,omppe), a_kp(1:nktmp,1:nktmp,omppe), undeflag(i,1:nt) )

        call check_zero( a_kp(1:nktmp,1:nktmp,omppe) )

!-- Solve x_k
!     call tri_gauss( a_kp, b_k, x_k )
!     call gausss( a_kp, b_k, x_k )
        call fp_gauss( a_kp(1:nktmp,1:nktmp,omppe), b_k(1:nktmp,omppe), x_k(1:nktmp,omppe) )

!-- Set each unknown variable from x_k
        call set_xk2variables( na, nktmp, x_k(1:nktmp,omppe), Vd_A0(omppe),  &
  &                            Vd_AC(1:na+1,omppe), Vd_BS(1:na+1,omppe),  &
  &                            undef=dundef )

!-- Calculate Vr and Vt components of rotating wind
        call calc_AB2VT( na, vmax, r_n(i), td_max(omppe), psid(1:nt,omppe), rtc_n,  &
  &                      Vd_A0(omppe), Vd_AC(1:na+1,omppe), Vd_BS(1:na+1,omppe),  &
  &                      VT0(i,1:nt), VR0(i,1:nt), VTSn(1:na,i,1:nt), VTCn(1:na,i,1:nt),  &
  &                      undef=dundef )
     end if
  end do

!$omp end do
!$omp end parallel

!-- Calculate total retrieved Vr and Vt
  call calc_Vn2Vtot( nasym, VT0, VTSn, VTCn, VT, undef=dundef )
  VR=VR0

!-- Set undef in each output variable at undefined grids
  call set_undef_value( undeflag, dundef, VT )
  call set_undef_value( undeflag, dundef, VR )
  if(nasym>0)then
     do k=1,nasym
        call set_undef_value( undeflag, dundef, VTSn(k,1:nr,1:nt) )
        call set_undef_value( undeflag, dundef, VTCn(k,1:nr,1:nt) )
     end do
  end if

  call stdout( "Finish procedure.", "Retrieve_velocity_GBVTD", 0 )

end subroutine Retrieve_velocity_GBVTD


subroutine calc_fkj( nasym, nnk, psid, fkj, undeflag )
!! Calculate the coefficient matrix \(f_{kj}\)
  implicit none
  integer, intent(in) :: nasym  !! Maximum wavenumber for asymmetric components
  integer, intent(in) :: nnk  !! Matrix dimension for fkj
  double precision, intent(in) :: psid(:)  !! Nonlinear angle \(\psi _d\) [rad]
  double precision, intent(out) :: fkj(nnk,size(psid))  !! Coefficient matrix
  logical, intent(in) :: undeflag(size(psid))  !! Undefined flag at each sampling point

  !-- internal variables
  integer :: nmax, nnt, jj, kk, cstat
  double precision, dimension(nasym+1,size(psid)) :: sinen, cosinen

  call stdout( "Enter procedure.", "calc_fkj", 0 )

  nnt=size(psid)
  fkj=0.0d0
  nmax=nasym+1

  sinen=0.0d0
  cosinen=0.0d0

  if(nnk/=1+2*(nasym+1))then
     call stdout( "nnk is not 1+2(nasym+1). stop.", "calc_fkj", -1 )
     stop
  end if

!-- Set fixed variables for R-T, in advance

  do jj=1,nnt
     do kk=1,nmax
        sinen(kk,jj)=dsin(dble(kk)*psid(jj))
        cosinen(kk,jj)=dcos(dble(kk)*psid(jj))
     end do
  end do

!-- Set coefficients for A0 at each (jj)

  fkj(1,1:nnt)=1.0d0

!-- Set coefficients for A1 to An and B1 to Bn at each (jj)

  do jj=1,nnt
     do kk=1,nmax
        fkj(1+kk,jj)=cosinen(kk,jj)
        fkj(1+nmax+kk,jj)=sinen(kk,jj)
     end do
  end do

  call stdout( "Finish procedure.", "calc_fkj", 0 )

end subroutine calc_fkj


subroutine calc_fkj2akp( fkj, akp, undeflag )
!! Calculate a_kp from f_kj
  implicit none
  double precision, intent(in) :: fkj(:,:)  !! Coefficient matrix
  double precision, intent(out) :: akp(size(fkj,1),size(fkj,1))  !! Coefficient matrix in LSM
  logical, intent(in) :: undeflag(size(fkj,2))  !! Undefined flag at each sampling point
  integer :: nnk, nnj, jj, kk, ll, cstat

  call stdout( "Enter procedure.", "calc_fkj2akp", 0 )

  nnk=size(fkj,1)
  nnj=size(fkj,2)

  do ll=1,nnk
     do kk=ll,nnk
        akp(kk,ll)=matrix_sum( fkj(kk,1:nnj), fkj(ll,1:nnj),  &
  &                            undeflag(1:nnj) )
        akp(ll,kk)=akp(kk,ll)
     end do
  end do

  call stdout( "Finish procedure.", "calc_fkj2akp", 0 )

end subroutine calc_fkj2akp


subroutine calc_fkjVd2bk( vmax, fkj, Vdl, bk, undeflag )
!! Calculate \(b_k\) from \(f_{k,j}\) and \(V_d\)
  implicit none
  double precision, intent(in) :: vmax  !! Scaling factor for velocity [m/s]
  double precision, intent(in) :: fkj(:,:)  !! Coefficient matrix
  double precision, intent(in) :: Vdl(size(fkj,2))  !! Doppler velocity [m/s]
  double precision, intent(out) :: bk(size(fkj,1))  !! Vector \(\textbf{b} \)
  logical, intent(in) :: undeflag(size(fkj,2))  !! Undefined flag at each sampling point
  integer :: nnk, nnj, jj, kk, ll, cstat
  double precision :: dVdl(size(fkj,2))

  call stdout( "Enter procedure.", "calc_fkjVd2bk", 0 )

  nnk=size(fkj,1)
  nnj=size(fkj,2)

  dVdl(1:nnj)=Vdl(1:nnj)

  do kk=1,nnk
     bk(kk)=matrix_sum( dVdl(1:nnj), fkj(kk,1:nnj),  &
  &                     undeflag(1:nnj) )/vmax
  end do

  call stdout( "Finish procedure.", "calc_fkjVd2bk", 0 )

end subroutine calc_fkjVd2bk


subroutine set_xk2variables( nasym, nnk, xk, A0, ACn, BSn, undef )
!! Set each unknown variable from \(x_k\)
  implicit none
  integer, intent(in) :: nasym  !! Maximum wavenumber for asymmetric components
  integer, intent(in) :: nnk  !! Matrix dimension for coefficient matrix A
  double precision, intent(in) :: xk(nnk)  !! solved unknown variable vector
  double precision, intent(out) :: A0  !! Wavenumber-0 for Doppler velocity
  double precision, intent(out) :: ACn(nasym+1)  !! Cosine conponents of Doppler velocity
  double precision, intent(out) :: BSn(nasym+1)  !! Sine components of Doppler velocity
  double precision, intent(in), optional :: undef  !! Undefined value

  integer :: kk

  call stdout( "Enter procedure.", "set_xk2variables", 0 )

  A0=xk(1)

  do kk=1,nasym+1
     ACn(kk)=xk(1+kk)
     BSn(kk)=xk(1+nasym+1+kk)
  end do

  call stdout( "Finish procedure.", "set_xk2variables", 0 )

end subroutine set_xk2variables


subroutine calc_AB2VT( nasym, vmax, rd, thetad_max, psid, rtc, A0, An, Bn,  &
  &                    VT0_rt, VR0_rt, VTSn_rt, VTCn_rt, undef )
!! Calculate tangential components of retrieved wind
  implicit none
  integer, intent(in) :: nasym  !! Maximum wavenumber for asymmetric components
  double precision, intent(in) :: vmax  !! Scaling factor for velocity [m/s]
  double precision, intent(in) :: rd  !! Normalized radius [1]
  double precision, intent(in) :: thetad_max  !! Normalized radius
  double precision, intent(in) :: psid(:)  !! Nonlinear angle \(\psi _d\)
  double precision, intent(in) :: rtc !! Normalized distance between the radar to vortex center [1]
  double precision, intent(in) :: A0  !! Wanvenumber-0 of Doppler velocity
  double precision, intent(in) :: An(nasym+1)  !! Cosine components of Doppler velocity
  double precision, intent(in) :: Bn(nasym+1)  !! Sine components of Doppler velocity
  double precision, intent(out) :: VT0_rt(size(psid))  !! Wavenumber-0 tangential wind
  double precision, intent(out) :: VR0_rt(size(psid))  !! Wavenumber-0 radial wind
  double precision, intent(out) :: VTSn_rt(nasym,size(psid))  !! Sine components of tangential wind [m/s]
  double precision, intent(out) :: VTCn_rt(nasym,size(psid))  !! Cosine components of tangential wind [m/s]
  double precision, intent(in), optional :: undef  !! No use

  integer :: jj, kk, nnt, cstat
  double precision, dimension(nasym,size(psid)) :: cosinen, sinen
  double precision, dimension(nasym) :: VTSn_r, VTCn_r
  double precision :: tmp_VT0, tmp_VR0

  call stdout( "Enter procedure.", "calc_AB2VT", 0 )

  nnt=size(psid)

  VT0_rt=0.0d0
  VR0_rt=0.0d0
  VTSn_rt=0.0d0
  VTCn_rt=0.0d0

  do jj=1,nnt
     do kk=1,nasym
        cosinen(kk,jj)=dcos(dble(kk)*psid(jj))
        sinen(kk,jj)=dsin(dble(kk)*psid(jj))
     end do
  end do

  tmp_VT0=0.0d0
  tmp_VR0=A0
  do kk=1,nasym+1
     if(mod(kk,2)==1)then  ! only odd number
        tmp_VT0=tmp_VT0-Bn(kk)
     end if
     tmp_VR0=tmp_VR0+An(kk)
  end do
  VT0_rt(1:nnt)=tmp_VT0*vmax
  VR0_rt(1:nnt)=tmp_VR0/(1.0d0+rd/rtc)*vmax

  if(nasym>=1)then
     VTSn_r(1)=An(2)-A0+An(4)+(A0+An(2)+An(4))*dcos(thetad_max)
     VTCn_r(1)=-2.0d0*(Bn(2)+Bn(4))

     do kk=2,nasym
        VTSn_r(kk)=2.0d0*An(kk+1)
        VTCn_r(kk)=-2.0d0*Bn(kk+1)
     end do

     do jj=1,nnt
        do kk=1,nasym
           VTSn_rt(kk,jj)=VTSn_r(kk)*sinen(kk,jj)*vmax
           VTCn_rt(kk,jj)=VTCn_r(kk)*cosinen(kk,jj)*vmax
        end do
     end do
  end if

  call stdout( "Finish procedure.", "calc_AB2VT", 0 )

end subroutine calc_AB2VT


subroutine calc_Vn2Vtot( nasym, V0, VSn, VCn, Vtot, undef )
!! Calculate total wind from all wavenumbers
  implicit none
  integer, intent(in) :: nasym  !! Maximum wavenumber for asymmetric components
  double precision, intent(in) :: V0(:,:)  !! Wavenumber-0 wind [m/s]
  double precision, intent(in) :: VSn(nasym,size(V0,1),size(V0,2))  !! Sine components of wnd [m/s]
  double precision, intent(in) :: VCn(nasym,size(V0,1),size(V0,2))  !! Cosine components of wind [m/s]
  double precision, intent(inout) :: Vtot(size(V0,1),size(V0,2))  !! Total wind [m/s]
  double precision, intent(in), optional :: undef  !! Undefined value
  integer :: ii, jj, kk, nnr, nnt

  call stdout( "Enter procedure.", "calc_Vn2Vtot", 0 )

  nnr=size(V0,1)
  nnt=size(V0,2)
  Vtot=V0

  if(present(undef))then
     do jj=1,nnt
        do ii=1,nnr
           do kk=1,nasym
              if(VSn(kk,ii,jj)/=undef)then
                 Vtot(ii,jj)=Vtot(ii,jj)+VSn(kk,ii,jj)
              end if
              if(VCn(kk,ii,jj)/=undef)then
                 Vtot(ii,jj)=Vtot(ii,jj)+VCn(kk,ii,jj)
              end if
           end do
        end do
     end do
  else
     do jj=1,nnt
        do ii=1,nnr
           do kk=1,nasym
              Vtot(ii,jj)=Vtot(ii,jj)+VSn(kk,ii,jj)+VCn(kk,ii,jj)
           end do
        end do
     end do
  end if

  call stdout( "Finish procedure.", "calc_VSn2Vtot", 0 )

end subroutine calc_Vn2Vtot


subroutine calc_psid( rtc, r, theta, thetad, psid )
!! Calculate the nonlinear angle psid at a centain radius <br>
!! From the geometry, <br>
!! \(\sin{\psi_d} = (R_{TC}/r) \sin{\theta _d}.\) --(1) <br>
!! \(R^2_{TC} = r^2 + D^2 - 2rD\cos{\psi_d} , D=r\dfrac{\sin{\theta}}{\sin{\theta _d}}.\) --(2) <br>
!! From Eq. (2), <br>
!! \(\cos{\psi_d} = (r^2 + D^2 - R_{TC}^2) / (2rD).\) --(3) <br>
!! From Eqs. (1) and (3), <br>
!! \(\tan{\psi_d} = 2\rho \dfrac{\sin{\theta}}{\rho ^2+a\rho ^2-1}, \) <br>
!!  \(\rho =r/R_{TC}, a=\dfrac{\sin{\theta}}{\sin{\theta _d}} . \)
  implicit none
  double precision, intent(in) :: rtc  !! Distance from the radar to TC center [m]
  double precision, intent(in) :: r  !! Radius [m]
  double precision, intent(in) :: theta(:)  !! azimuthal angle [deg]
  double precision, intent(in) :: thetad(size(theta))  !! Azimuthal angle for radar [rad]
  double precision, intent(out) :: psid(size(theta))  !! Nonlinear angle \(\psi _d\) [rad]
  integer :: jj, nnt
  double precision :: x, y, rpp

  call stdout( "Enter procedure.", "calc_psid", 0 )

  nnt=size(theta)

  rpp=r/rtc

  do jj=1,nnt
     if(dsin(thetad(jj))==0.0d0)then  ! In this case, theta = psid
        psid(jj)=theta(jj)
     else
        x=(rpp**2)*(1.0d0+(dsin(theta(jj))/dsin(thetad(jj)))**2)-1.0d0
        y=2.0d0*rpp*dsin(theta(jj))
        psid(jj)=datan2( y, x )
     end if
  end do

  call stdout( "Finish procedure.", "calc_psid", 0 )

end subroutine calc_psid


double precision function matrix_sum( aij, akj, undeflag )
!! Calculate product for a component in a matrix
  implicit none
  double precision, intent(in) :: aij(:)  !! Matrix A1
  double precision, intent(in) :: akj(size(aij))  !! Matrix A2
  logical, intent(in), optional :: undeflag(size(aij))  !! Undefined flag at each sampling point
  integer :: jj, nj
  double precision :: res

  nj=size(aij)

  res=0.0d0

  if(present(undeflag))then
     do jj=1,nj
        if(undeflag(jj).eqv..false.)then
           res=res+aij(jj)*akj(jj)
        end if
     end do
  else
     do jj=1,nj
        res=res+aij(jj)*akj(jj)
     end do
  end if

  matrix_sum=res

  return

end function matrix_sum


subroutine check_max( ival, mx_val )
!! Check maximum value in each element of the matrix "a"
  implicit none
  double precision, intent(in) :: ival(:)  !! Matrix A
  double precision, intent(out) :: mx_val  !! Max value in A
  integer :: ii, nni
  double precision :: res

  nni=size(ival)

  res=ival(1)

  do ii=2,nni
     res=max(ival(ii),res)
  end do

  mx_val=res

end subroutine check_max


subroutine check_zero( a )
!! Check zero components in the matrix "a"
  implicit none
  double precision, intent(in) :: a(:,:)  !! Matrix A
  integer :: ii, jj, nni, nnj
  logical :: res

  nni=size(a,1)
  nnj=size(a,2)

  do jj=1,nnj
     res=.false.
     do ii=1,nni
        if(a(ii,jj)/=0.0d0)then
           res=.true.
           exit
        end if
     end do
     if(res.eqv..false.)then
        write(*,*) "Detect all zero", jj
     end if
  end do

end subroutine check_zero


subroutine check_undef_grid( vval, undefv, undeflag )
!! Check undefined grids
  implicit none
  double precision, intent(in) :: vval(:,:)  !! Grid value
  double precision, intent(in) :: undefv  !! Undefined value
  logical, intent(out) :: undeflag(size(vval,1),size(vval,2))  ! Undefined flag
  integer :: ii, jj, nni, nnj

  nni=size(vval,1)
  nnj=size(vval,2)
  undeflag=.false.

  do jj=1,nnj
     do ii=1,nni
        if(vval(ii,jj)==undefv)then
           undeflag(ii,jj)=.true.
        end if
     end do
  end do

end subroutine check_undef_grid


subroutine set_undef_value( undeflag, undefv, vval )
!! Set undef value (="undefv") to val if undeflag == true.
  implicit none
  logical, intent(in) :: undeflag(:,:)  !! Undefined flag at each sampling point
  double precision, intent(in) :: undefv  !! Undefined value
  double precision, intent(inout) :: vval(size(undeflag,1),size(undeflag,2))  !! Original value at each sampling point
  integer :: ii, jj, nni, nnj

  nni=size(undeflag,1)
  nnj=size(undeflag,2)

  do jj=1,nnj
     do ii=1,nni
        if(undeflag(ii,jj).eqv..true.)then
           vval(ii,jj)=undefv
        end if
     end do
  end do

end subroutine set_undef_value

end module GBVTD_main
