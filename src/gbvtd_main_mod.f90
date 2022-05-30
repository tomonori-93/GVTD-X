module GBVTD_main

  use ToRMHOWe_sub

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

subroutine Retrieve_velocity_GBVTD( nasym, r, t, td, Vd, Un, Vn, RadTC,  &
  &                                 VT, VR, VT0, VR0, VTSn, VTCn, undef )
!-- solve unknown variables and return wind velocity on R-T coordinates
!-- based on the GBVTD technique.
!-------------------------------------------------------
!-- [relationship between r and rh] --
!-------------------------------------------------------
!--   i-1    i    i+1
!-- ...|-- --|-- --|... : r(1:size(r)) = velocity radii
!-------------------------------------------------------
  implicit none
  !-- input/output
  integer, intent(in) :: nasym  ! wave number for asymmetric tangential wind
  double precision, intent(in) :: r(:)   ! radial coordinate on which Vd is defined [m]
  double precision, intent(in) :: t(:)   ! azimuthal coordinate on which Vd is defined [rad]
  double precision, intent(in) :: td(size(r),size(t))  ! radar azimuthal angle defined at Vd(r,t) [rad]
  double precision, intent(inout) :: Vd(size(r),size(t))  ! Doppler velocity defined on r-t [m s-1]
  double precision, intent(in) :: Un(2)                ! Parallel component to radar in environmental wind, defined on r-t [m s-1]
  double precision, intent(in) :: Vn(2)                ! Normal component to radar in environmental wind, defined on r-t [m s-1]
  double precision, intent(in) :: RadTC                ! Distance from radar to TC center [m]
  double precision, intent(out) :: VT(size(r),size(t))  ! retrieved total tangential wind [m s-1]
  double precision, intent(out) :: VR(size(r),size(t))  ! retrieved total radial wind [m s-1]
  double precision, intent(out) :: VT0(size(r),size(t))  ! retrieved axisymmetric radial component of rotating wind [m s-1]
  double precision, intent(out) :: VR0(size(r),size(t))  ! retrieved axisymmetric tangential component of divergent wind [m s-1]
  double precision, intent(out) :: VTSn(nasym,size(r),size(t))  ! retrieved tangential component of rotating wind [m s-1]
  double precision, intent(out) :: VTCn(nasym,size(r),size(t))  ! retrieved radial component of rotating wind [m s-1]
  double precision, intent(in), optional :: undef  ! undefined value for Vd

  !-- internal variables
  integer :: i, j, k, p, cstat  ! dummy indexes
  integer :: nr, nt  ! array numbers for r and t, respectively
  integer :: nk      ! array number of a_k
  integer :: nr_out  ! outermost radius for GBVTD limit radius
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

  VT=dundef
  VR=dundef
  VT0=dundef
  VR0=dundef
  VTSn=dundef
  VTCn=dundef

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
!$omp do schedule(runtime) private(i,omppe)

  do i=1,nr_out

!$   omppe=OMP_GET_THREAD_NUM()+1

     x_k(1:nk,omppe)=0.0d0
     b_k(1:nk,omppe)=0.0d0
     a_kp(1:nk,1:nk,omppe)=0.0d0
     f_kj(1:nk,1:nt,omppe)=0.0d0
     Vd_A0(omppe)=0.0d0
     Vd_AC(1:nasym+1,omppe)=0.0d0
     Vd_BS(1:nasym+1,omppe)=0.0d0
     psid(1:nt,omppe)=0.0d0

     call check_max( td(i,1:nt), td_max(omppe) )

!-- Calculate the nonlinear angle psid
     call calc_psid( rtc_n, r_n(i), t, td(i,1:nt), psid(1:nt,omppe) )

!-- Calculate f_kj
     call calc_fkj( nasym, nk, psid(1:nt,omppe),  &
  &                 f_kj(1:nk,1:nt,omppe), undeflag(i,1:nt) )

!-- Calculate b_k
     call calc_fkjVd2bk( vmax, f_kj(1:nk,1:nt,omppe), Vd(i,1:nt),  &
  &                      b_k(1:nk,omppe), undeflag(i,1:nt) )

!-- Calculate a_kp
     call calc_fkj2akp( f_kj(1:nk,1:nt,omppe), a_kp(1:nk,1:nk,omppe), undeflag(i,1:nt) )

     call check_zero( a_kp(1:nk,1:nk,omppe) )

!-- Solve x_k
!  call tri_gauss( a_kp, b_k, x_k )
!  call gausss( a_kp, b_k, x_k )
     call fp_gauss( a_kp(1:nk,1:nk,omppe), b_k(1:nk,omppe), x_k(1:nk,omppe) )

!-- Set each unknown variable from x_k
     call set_xk2variables( nasym, nk, x_k(1:nk,omppe), Vd_A0(omppe),  &
  &                         Vd_AC(1:nasym+1,omppe), Vd_BS(1:nasym+1,omppe),  &
  &                         undef=dundef )

!-- Calculate Vr and Vt components of rotating wind
     call calc_AB2VT( nasym, vmax, r_n(i), td_max(omppe), Vn(1), psid(1:nt,omppe), rtc_n,  &
  &                   Vd_A0(omppe), Vd_AC(1:nasym+1,omppe), Vd_BS(1:nasym+1,omppe),  &
  &                   VT0(i,1:nt), VR0(i,1:nt), VTSn(1:nasym,i,1:nt), VTCn(1:nasym,i,1:nt),  &
  &                   undef=dundef )
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

!--------------------------------------------------
!-- calculate f_kj
!--------------------------------------------------

subroutine calc_fkj( nasym, nnk, psid, fkj, undeflag )
  implicit none
  integer, intent(in) :: nasym
  integer, intent(in) :: nnk
  double precision, intent(in) :: psid(:)
  double precision, intent(out) :: fkj(nnk,size(psid))
  logical, intent(in) :: undeflag(size(psid))

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

!--------------------------------------------------
!-- calculate a_kp from f_kj
!--------------------------------------------------

subroutine calc_fkj2akp( fkj, akp, undeflag )
  implicit none
  double precision, intent(in) :: fkj(:,:)
  double precision, intent(out) :: akp(size(fkj,1),size(fkj,1))
  logical, intent(in) :: undeflag(size(fkj,2))
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

!--------------------------------------------------
!-- calculate b_k from f_kj and Vd
!--------------------------------------------------

subroutine calc_fkjVd2bk( vmax, fkj, Vdl, bk, undeflag )
  implicit none
  double precision, intent(in) :: vmax
  double precision, intent(in) :: fkj(:,:)
  double precision, intent(in) :: Vdl(size(fkj,2))
  double precision, intent(out) :: bk(size(fkj,1))
  logical, intent(in) :: undeflag(size(fkj,2))
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

!--------------------------------------------------
!-- Set each unknown variable from x_k
!--------------------------------------------------

subroutine set_xk2variables( nasym, nnk, xk, A0, ACn, BSn, undef )
  implicit none
  integer, intent(in) :: nasym
  integer, intent(in) :: nnk
  double precision, intent(in) :: xk(nnk)  ! solved unknown variable vector
  double precision, intent(out) :: A0
  double precision, intent(out) :: ACn(nasym+1)
  double precision, intent(out) :: BSn(nasym+1)
  double precision, intent(in), optional :: undef

  integer :: kk

  call stdout( "Enter procedure.", "set_xk2variables", 0 )

  A0=xk(1)

  do kk=1,nasym+1
     ACn(kk)=xk(1+kk)
     BSn(kk)=xk(1+nasym+1+kk)
  end do

  call stdout( "Finish procedure.", "set_xk2variables", 0 )

end subroutine set_xk2variables

!--------------------------------------------------
!-- Calculate Vt components of retrieved wind
!--------------------------------------------------

subroutine calc_AB2VT( nasym, vmax, rd, thetad_max, Vn, psid, rtc, A0, An, Bn,  &
  &                    VT0_rt, VR0_rt, VTSn_rt, VTCn_rt, undef )
  implicit none
  integer, intent(in) :: nasym
  double precision, intent(in) :: vmax
  double precision, intent(in) :: rd  ! Normalized radius
  double precision, intent(in) :: thetad_max  ! Normalized radius
  double precision, intent(in) :: Vn  ! Normal component of environmental [ms-1]
  double precision, intent(in) :: psid(:)
  double precision, intent(in) :: rtc ! Normalized radius
  double precision, intent(in) :: A0
  double precision, intent(in) :: An(nasym+1)
  double precision, intent(in) :: Bn(nasym+1)
  double precision, intent(out) :: VT0_rt(size(psid))
  double precision, intent(out) :: VR0_rt(size(psid))
  double precision, intent(out) :: VTSn_rt(nasym,size(psid))
  double precision, intent(out) :: VTCn_rt(nasym,size(psid))
  double precision, intent(in), optional :: undef  ! No use

  integer :: jj, kk, nnt, cstat
  double precision, dimension(nasym,size(psid)) :: cosinen, sinen
  double precision, dimension(nasym) :: VTSn_r, VTCn_r

  call stdout( "Enter procedure.", "calc_AB2VT", 0 )

  nnt=size(psid)

  do jj=1,nnt
     do kk=1,nasym
        cosinen(kk,jj)=dcos(dble(kk)*psid(jj))
        sinen(kk,jj)=dsin(dble(kk)*psid(jj))
     end do
  end do

  VT0_rt(1:nnt)=-(Bn(1)+Bn(3))*vmax-Vn*dsin(thetad_max)
  VR0_rt(1:nnt)=(An(1)+An(3))*vmax

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

  call stdout( "Finish procedure.", "calc_AB2VT", 0 )

end subroutine calc_AB2VT

!--------------------------------------------------
!-- Calculate total wind from all wavenumbers
!--------------------------------------------------

subroutine calc_Vn2Vtot( nasym, V0, VSn, VCn, Vtot, undef )
  implicit none
  integer, intent(in) :: nasym
  double precision, intent(in) :: V0(:,:)
  double precision, intent(in) :: VSn(nasym,size(V0,1),size(V0,2))
  double precision, intent(in) :: VCn(nasym,size(V0,1),size(V0,2))
  double precision, intent(inout) :: Vtot(size(V0,1),size(V0,2))
  double precision, intent(in), optional :: undef
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

!--------------------------------------------------
!-- Calculate the nonlinear angle psid at a centain radius
!--------------------------------------------------

subroutine calc_psid( rtc, r, theta, thetad, psid )
!-- From the geometry, 
!-- sin(psid) = (rtc/r) * sin(thetad). --(1)
!-- rtc**2 = r**2 + D**2 - 2*r*D*cos(psid), D=r*(sin(theta)/sin(thetad)) --(2)
!-- From Eq. (2),
!-- cos(psid) = (r**2 + D**2 - rtc**2) / (2*r*D). --(3)
!-- From Eqs. (1) and (3), 
!-- tan(psid) = 2*rpp*sin(theta) / (rpp**2+(rpp**2)*a-1),
!--   rpp=r/rtc, a=sin(theta)/sin(thetad). 
  implicit none
  double precision, intent(in) :: rtc  ! Distance from the radar to TC center
  double precision, intent(in) :: r  ! Radius
  double precision, intent(in) :: theta(:)  ! azimuthal angle [deg]
  double precision, intent(in) :: thetad(size(theta))
  double precision, intent(out) :: psid(size(theta))
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

!--------------------------------------------------
!-- calculate product for a component in a matrix
!--------------------------------------------------

double precision function matrix_sum( aij, akj, undeflag )
  implicit none
  double precision, intent(in) :: aij(:)
  double precision, intent(in) :: akj(size(aij))
  logical, intent(in), optional :: undeflag(size(aij))
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

!--------------------------------------------------
!--------------------------------------------------

subroutine check_max( ival, mx_val )
  implicit none
  double precision, intent(in) :: ival(:)
  double precision, intent(out) :: mx_val
  integer :: ii, nni
  double precision :: res

  nni=size(ival)

  res=ival(1)

  do ii=2,nni
     res=max(ival(ii),res)
  end do

  mx_val=res

end subroutine check_max

!--------------------------------------------------
!--------------------------------------------------

subroutine check_zero( a )
  implicit none
  double precision, intent(in) :: a(:,:)
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

!--------------------------------------------------
!--------------------------------------------------

subroutine check_undef_grid( vval, undefv, undeflag )
  implicit none
  double precision, intent(in) :: vval(:,:)
  double precision, intent(in) :: undefv
  logical, intent(out) :: undeflag(size(vval,1),size(vval,2))
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

!--------------------------------------------------
!--------------------------------------------------

subroutine set_undef_value( undeflag, undefv, vval )
  implicit none
  logical, intent(in) :: undeflag(:,:)
  double precision, intent(in) :: undefv
  double precision, intent(inout) :: vval(size(undeflag,1),size(undeflag,2))
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
