module ToRMHOWe_main

  use ToRMHOWe_sub

  implicit none

  public :: Retrieve_velocity

  private :: calc_fkij
  private :: calc_fkij2akp
  private :: calc_fkijVd2bk
  private :: set_xk2variables
  private :: calc_phi2Vrot
  private :: calc_D2Vdiv
  private :: calc_Vn2Vtot
  private :: check_zero
  private :: check_undef_grid
  private :: set_undef_value
  private :: calc_Phi2Phin
  private :: calc_Phi2Zetan
  private :: calc_pseudo_GVTD0
  private :: calc_phi2sc

contains

subroutine Retrieve_velocity( nrot, ndiv, r, t, rh, td, rdiv, Vd, Un, Vn, RadTC,  &
  &                           VT, VR, VRT0, VDR0, VRTn, VRRn, VDTm, VDRm,  &
  &                           undef, phin, zetan, VRT0_GVTD, VDR0_GVTD,  &
  &                           VRTns, VRTnc, VRRns, VRRnc )
!-- solve unknown variables and return wind velocity on R-T coordinates.
!-------------------------------------------------------
!-- [relationship between r and rh] --
!-------------------------------------------------------
!--   i-1    i    i+1
!-- ...|-- --|-- --|... : r(1:size(r)) = velocity radii
!-- |-- --|-- --|-- --| : rh(1,size(r)+1) = potential radii
!--i-1    i    i+1   i+2
!-------------------------------------------------------
  implicit none
  !-- input/output
  integer, intent(in) :: nrot  ! wave number for rotating wind
  integer, intent(in) :: ndiv  ! wave number for divergent wind
  double precision, intent(in) :: r(:)   ! radial coordinate on which Vd is defined [m]
  double precision, intent(in) :: t(:)   ! azimuthal coordinate on which Vd is defined [rad]
  double precision, intent(in) :: rh(size(r)+1)  ! radial coordinate on which Phi (staggered for Vd) is defined [m]
  double precision, intent(in) :: td(size(r),size(t))  ! radar azimuthal angle defined at Vd(r,t) [rad]
  double precision, intent(in) :: rdiv(:)  ! radial coordinate on which Dc (staggered for Vd) is defined [m]
  double precision, intent(inout) :: Vd(size(r),size(t))  ! Doppler velocity defined on r-t [m s-1]
  double precision, intent(in) :: Un(2)                ! Parallel component to radar in environmental wind, defined on r-t [m s-1]
  double precision, intent(in) :: Vn(2)                ! Normal component to radar in environmental wind, defined on r-t [m s-1]
  double precision, intent(in) :: RadTC                ! Distance from radar to TC center [m]
  double precision, intent(out) :: VT(size(r),size(t))  ! retrieved total tangential wind [m s-1]
  double precision, intent(out) :: VR(size(r),size(t))  ! retrieved total radial wind [m s-1]
  double precision, intent(out) :: VRT0(size(r),size(t))  ! retrieved axisymmetric radial component of rotating wind [m s-1]
  double precision, intent(out) :: VDR0(size(r),size(t))  ! retrieved axisymmetric tangential component of divergent wind [m s-1]
  double precision, intent(out) :: VRTn(nrot,size(r),size(t))  ! retrieved tangential component of rotating wind [m s-1]
  double precision, intent(out) :: VRRn(nrot,size(r),size(t))  ! retrieved radial component of rotating wind [m s-1]
  double precision, intent(out) :: VDTm(ndiv,size(r),size(t))  ! retrieved tangential component of divergent wind [m s-1]
  double precision, intent(out) :: VDRm(ndiv,size(r),size(t))  ! retrieved radial component of divergent wind [m s-1]
  double precision, intent(in), optional :: undef  ! undefined value for Vd
  double precision, intent(out), optional :: phin(nrot,size(r),size(t))   ! retrieved stream function [m2 s-1]
  double precision, intent(out), optional :: zetan(nrot,size(r),size(t))  ! retrieved vorticity [s-1]
  double precision, intent(out), optional :: VRT0_GVTD(size(r),size(t))  ! retrieved axisymmetric radial component of pseudo-GVTD tangential wind [m s-1]
  double precision, intent(out), optional :: VDR0_GVTD(size(r),size(t))  ! retrieved axisymmetric tangential component of pseudo-GVTD tangential wind [m s-1]
  double precision, intent(out), optional :: VRTns(nrot,size(r),size(t))  ! Sine component of retrieved asymmetric radial wind [m s-1]
  double precision, intent(out), optional :: VRTnc(nrot,size(r),size(t))  ! Cosine component of retrieved asymmetric radial wind [m s-1]
  double precision, intent(out), optional :: VRRns(nrot,size(r),size(t))  ! Sine component of retrieved asymmetric tangential wind [m s-1]
  double precision, intent(out), optional :: VRRnc(nrot,size(r),size(t))  ! Cosine component of retrieved asymmetric tangential wind [m s-1]

  !-- internal variables
  integer :: i, j, k, p, irad, cstat  ! dummy indexes
  integer :: nr, nt  ! array numbers for r and t, respectively
  integer :: nk      ! array number of a_k
  integer :: nrdiv   ! array number for rdiv
  integer :: nrdiv2  ! array number for rdiv_n
  integer :: nbound  ! element number for variables related to the outermost radius (i.e., 2*nrot-1)
  double precision, allocatable, dimension(:) :: Vdivr_r    ! axisymmetric divergent wind (VDR0(r))
  double precision, allocatable, dimension(:) :: Vrott_r    ! axisymmetric rotating wind (VRT0(r))
  double precision, allocatable, dimension(:,:) :: phis_nr  ! asymmetric (sine) stream function (Phi_S(n,r))
  double precision, allocatable, dimension(:,:) :: phic_nr  ! asymmetric (cosine) stream function (Phi_C(n,r))
!  double precision, allocatable, dimension(:,:) :: divs_mr  ! asymmetric (sine) stream function (D_S(n,r))
  double precision, allocatable, dimension(:,:) :: divc_mr  ! asymmetric (cosine) stream function (D_C(n,r))
  double precision, allocatable, dimension(:) :: GVTDU_r    ! axisymmetric radial wind for pseudo-GVTD
  double precision, allocatable, dimension(:) :: GVTDV_r    ! axisymmetric tangential wind for pseudo-GVTD
  double precision, allocatable, dimension(:,:) :: VRTns_r  ! sine component of tangential wind
  double precision, allocatable, dimension(:,:) :: VRTnc_r  ! cosine component of tangential wind
  double precision, allocatable, dimension(:,:) :: VRRns_r  ! sine component of radial wind
  double precision, allocatable, dimension(:,:) :: VRRnc_r  ! cosine component of radial wind
  double precision, allocatable, dimension(:) :: x_k        ! unknown vector for retrieved coefficients
  double precision, allocatable, dimension(:) :: b_k        ! known vector given by observed values
  double precision, allocatable, dimension(:,:) :: a_kp     ! coefficient matrix for x_k
  double precision, allocatable, dimension(:,:,:) :: f_kij  ! a_kp = sum_{i,j}(f_kij * f_pij)
  double precision :: dundef, vmax, tmprho
  double precision, dimension(size(r)) :: r_n               ! Nondimensional r
  double precision, dimension(size(rh)) :: rh_n             ! Nondimensional rh
  double precision, dimension(size(rdiv)*2) :: rdiv_n       ! Nondimensional rdiv (internal variable)
  double precision :: rtc_n                                 ! Nondimensional RadTC
  double precision, dimension(size(r),size(t)) :: delta     ! delta_ij
  logical, allocatable, dimension(:,:) :: undeflag ! Flag for Vd grid with undef

  call stdout( "Enter procedure.", "Retrieve_velocity", 0 )

  nr=size(r)
  nt=size(t)
  nrdiv=size(rdiv)
  nrdiv2=size(rdiv)*2
  vmax=50.0d0

  if(present(undef))then
     dundef=undef
  else
     dundef=-1.0d35
  end if

  VT=dundef
  VR=dundef
  VRT0=dundef
  VDR0=dundef
  VRTn=dundef
  VRRn=dundef
  VDTm=dundef
  VDRm=dundef
  delta=undef

  if(present(phin))then
     phin=dundef
  end if
  if(present(zetan))then
     zetan=dundef
  end if
  if(present(VRT0_GVTD))then
     VRT0_GVTD=dundef
     allocate(GVTDV_r(nr),stat=cstat)
  end if
  if(present(VDR0_GVTD))then
     VDR0_GVTD=dundef
     allocate(GVTDU_r(nr),stat=cstat)
  end if
  if(present(VRTns))then
     VRTns=dundef
     VRTnc=dundef
     VRRns=dundef
     VRRnc=dundef
     allocate(VRTns_r(nrot,nr),stat=cstat)
     allocate(VRTnc_r(nrot,nr),stat=cstat)
     allocate(VRRns_r(nrot,nr),stat=cstat)
     allocate(VRRnc_r(nrot,nr),stat=cstat)
  end if

!-- Check retrieved asymmetric wave number
  if(nrot<0)then
     call stdout( "nrot is greater equal to 0. stop.", "Retrieve_velocity", -1 )
     stop
  end if
  if(ndiv<0)then
     call stdout( "ndiv is greater equal to 0. stop.", "Retrieve_velocity", -1 )
     stop
  end if

!-- Normalized r and rh
  r_n=r/rh(nr+1)
  rh_n=rh/rh(nr+1)
!  rdiv_n=rdiv/rh(nr+1)
  rtc_n=RadTC/rh(nr+1)

!-- Check and search divergent radii
  do i=1,nrdiv
     call interpo_search_1d( rh, rdiv(i), irad )
     if((irad==nr+1).and.(rh(nr+1)<rdiv(i)))then
        if(ndiv>0)then
           call stdout( "Detect out of range. stop.", "Retrieve_velocity", -1 )
           stop
        else  ! Not use of rdiv.
           call stdout( "Detect out of range.", "Retrieve_velocity", 1 )
        end if
     end if
     rdiv_n(2*i-1)=rh(irad)/rh(nr+1)
     rdiv_n(2*i)=rh(irad+1)/rh(nr+1)
  end do

!-- Calculate delta_ij
!$omp parallel default(shared)
!$omp do schedule(runtime) private(i,j,tmprho)
  do j=1,nt
     do i=1,nr
        if(r_n(i)>0.0d0)then
           tmprho=rtc_n/r_n(i)
           delta(i,j)=dsqrt(tmprho**2+2.0d0*tmprho*dcos(t(j))+1.0d0)
        end if
     end do
  end do
!$omp end do
!$omp end parallel

!-- Set total number for unknown variables in a_k
  if(nrot>0)then
     !-- nk = free variable + boundary variable (=nbound)
     !-- all arrays and matrices are composed of nk + nbound
     !-- The last "nbound" is asigned for "lambda" (i.e., additional constraints)
     nbound=1
     nk=(2+2*nrot)*(nr-1)+2+2+ndiv*nrdiv+nbound
  else
     nbound=0
     nk=2*(nr-1)+2+ndiv*nrdiv
  end if

!-- Allocate and initialize arrays
  allocate(Vdivr_r(nr),stat=cstat)
  allocate(Vrott_r(nr),stat=cstat)
  allocate(phis_nr(nrot,nr+1),stat=cstat)
  allocate(phic_nr(nrot,nr+1),stat=cstat)
!  allocate(divs_mr(ndiv,nr+1),stat=cstat)
  allocate(divc_mr(ndiv,nrdiv),stat=cstat)
  allocate(x_k(nk+nbound),stat=cstat)
  allocate(b_k(nk+nbound),stat=cstat)
  allocate(a_kp(nk+nbound,nk+nbound),stat=cstat)
  allocate(f_kij(nk,nr,nt),stat=cstat)
  allocate(undeflag(nr,nt),stat=cstat)

  undeflag=.false.

  call check_undef_grid( Vd, dundef, undeflag )

  if(cstat/=0)then
     call stdout( "Failed to allocate variables. stop.", "Retrieve_velocity", -1 )
     stop
  end if

  Vdivr_r=0.0d0
  Vrott_r=0.0d0
  phis_nr=0.0d0
  phic_nr=0.0d0
!  divs_mr=0.0d0
  divc_mr=0.0d0
  x_k=0.0d0
  b_k=0.0d0
  a_kp=0.0d0
  f_kij=0.0d0

!-- Calculate f_kij
!-- ** normalized radius is used in r_n **
  call calc_fkij( nrot, ndiv, nk, Un, Vn, rtc_n, r_n, t, rh_n, td, rdiv_n, f_kij, Vd, undeflag )
!do j=1,nt
!do i=1,nr
!write(*,'(2i3,1P100E10.2)') i, j, f_kij(1:nk,i,j), Vd(i,j)
!end do
!end do

!-- Calculate b_k
  call calc_fkijVd2bk( vmax, f_kij, Vd, delta, b_k(1:nk), undeflag )

!-- Calculate a_kp
  call calc_fkij2akp( f_kij, a_kp(1:nk,1:nk), undeflag )

  call check_zero( a_kp(1:nk,1:nk) )

!-- Set elements for additional constraints
  if(nrot>0)then
     do k=1,nbound
        a_kp(nk+k,nk-nbound+k)=1.0d0
        a_kp(nk-nbound+k,nk+k)=1.0d0
     end do
     b_k(nk+1)=-rh_n(nr)*(Vn(2)-Vn(1))/vmax
  end if

!-- Solve x_k
!do k=1,nk
!write(*,'(i3,1P200E10.2)') k, a_kp(:,k), b_k(k)
!end do
!  call tri_gauss( a_kp, b_k, x_k )
!  call gausss( a_kp, b_k, x_k )
  call fp_gauss( a_kp, b_k, x_k )
!  call SOR_Gau_Sei( a_kp, b_k, 1.0d-5, 1.0d0, x_k )

!-- Set each unknown variable from x_k
  call set_xk2variables( nrot, ndiv, nrdiv, Un, Vn, vmax,  &
  &                      x_k(1:nk), Vrott_r, Vdivr_r,  &
  &                      phis_nr, phic_nr, divc_mr, undef=dundef )
!  &                      phis_nr, phic_nr, divs_mr, divc_mr, undef=dundef )

!-- Calculate Vr and Vt components of rotating wind
  call calc_phi2Vrot( nrot, Un, Vn, vmax, r_n, rh_n, t, Vrott_r, VRT0, VRTn, VRRn, phis_nr, phic_nr, undef=dundef )
!  call calc_phi2Vrot( nrot, Un, Vn, vmax, r(nr), r_n, rh_n, t, Vrott_r, VRT0, VRTn, VRRn, phis_nr, phic_nr, undef=dundef )

!-- Calculate Vr and Vt components of divergent wind
  call calc_D2Vdiv( ndiv, vmax, r_n, rh_n, t, rdiv_n, Vdivr_r, VDR0, VDTm, VDRm, divc_mr, undef=dundef )
!  call calc_D2Vdiv( ndiv, vmax, r(nr), r_n, rh_n, t, Vdivr_r, VDR0, VDTm, VDRm, divc_mr, undef=dundef )
!  call calc_D2Vdiv( ndiv, vmax, r(nr), r_n, rh_n, t, Vdivr_r, VDR0, VDTm, VDRm, divs_mr, divc_mr, undef=dundef )

!-- Calculate total retrieved Vr and Vt
  call calc_Vn2Vtot( nrot, ndiv, VRT0, VRTn, VDTm, VT )
  call calc_Vn2Vtot( nrot, ndiv, VDR0, VRRn, VDRm, VR )

!-- Set undef in each output variable at undefined grids
!  call set_undef_value( undeflag, dundef, VRT0 )
!  call set_undef_value( undeflag, dundef, VDR0 )
  call set_undef_value( undeflag, dundef, VT )
  call set_undef_value( undeflag, dundef, VR )
  if(nrot>0)then
     do k=1,nrot
        call set_undef_value( undeflag, dundef, VRTn(k,1:nr,1:nt) )
        call set_undef_value( undeflag, dundef, VRRn(k,1:nr,1:nt) )
     end do
  end if
  if(ndiv>0)then
     do k=1,ndiv
        call set_undef_value( undeflag, dundef, VDTm(k,1:nr,1:nt) )
        call set_undef_value( undeflag, dundef, VDRm(k,1:nr,1:nt) )
     end do
  end if

!-- monitor variables
  if((present(phin)).and.(nrot>0))then
     call calc_Phi2Phin( nrot, vmax, rh(nr+1), r_n, rh_n, t, phis_nr, phic_nr, phin )
  end if

  if((present(zetan)).and.(nrot>0))then
     call calc_Phi2Zetan( nrot, vmax, rh(nr+1), r_n, rh_n, t, phis_nr, phic_nr, zetan )
  end if

  if(present(VRT0_GVTD).and.(nrot>0))then
     call calc_pseudo_GVTD0( nrot, vmax, rtc_n, r_n, rh_n, VRT0(1:nr,1), VDR0(1:nr,1),  &
  &                          phis_nr, phic_nr, GVTDV_r(1:nr), GVTDU_r(1:nr) )
     do i=1,nr
        VRT0_GVTD(i,1:nt)=GVTDV_r(i)
        VDR0_GVTD(i,1:nt)=GVTDU_r(i)
     end do
  end if

  if(present(VRTns).and.(nrot>0))then
     call calc_phi2sc( nrot, vmax, r_n, rh_n, phis_nr, phic_nr,  &
  &                    VRTns_r(1:nrot,1:nr), VRTnc_r(1:nrot,1:nr),  &
  &                    VRRns_r(1:nrot,1:nr), VRRnc_r(1:nrot,1:nr) )
     do k=1,nrot
        do i=1,nr
           VRTns(k,i,1:nt)=VRTns_r(k,i)
           VRTnc(k,i,1:nt)=VRTnc_r(k,i)
           VRRns(k,i,1:nt)=VRRns_r(k,i)
           VRRnc(k,i,1:nt)=VRRnc_r(k,i)
        end do
     end do
  end if

  call stdout( "Finish procedure.", "Retrieve_velocity", 0 )

end subroutine Retrieve_velocity

!--------------------------------------------------
!-- calculate f_kij
!--------------------------------------------------

subroutine calc_fkij( nrot, ndiv, nnk, Usrn, Vsrn, rtc, rd, theta, rdh, thetad, rddiv,  &
  &                   fkij, Vdij, undeflag )
  implicit none
  integer, intent(in) :: nrot
  integer, intent(in) :: ndiv
  integer, intent(in) :: nnk
  double precision, intent(in) :: Usrn(2)
  double precision, intent(in) :: Vsrn(2)
  double precision, intent(in) :: rtc
  double precision, intent(in) :: rd(:)  ! Normalized radius
  double precision, intent(in) :: theta(:)
  double precision, intent(in) :: rdh(size(rd)+1)  ! Normalized radius
  double precision, intent(in) :: thetad(size(rd),size(theta))
  double precision, intent(in) :: rddiv(:)  ! Normalized radius
  double precision, intent(out) :: fkij(nnk,size(rd),size(theta))
  double precision, intent(inout) :: Vdij(size(rd),size(theta))
  logical, intent(in) :: undeflag(size(rd),size(theta))

  !-- internal variables
  integer :: nnr, nnt, nnrdiv, nnrdiv2, ii, jj, kk, pp, nmax, cstat, ncyc
  double precision :: r1_out_coef  !MOD, r_infty
  double precision, dimension(size(rd)) :: dr, dr_inv, alp
!  double precision, dimension(size(rddiv)) :: vareps
  double precision, dimension(size(rd)) :: r_inv
  double precision, dimension(size(rd),size(theta)) :: sines, cosines
  double precision, allocatable, dimension(:,:) :: sinen, cosinen
  double precision, allocatable, dimension(:,:,:) :: gkrr

  call stdout( "Enter procedure.", "calc_fkij", 0 )

  nnr=size(rd)
  nnt=size(theta)
  nnrdiv2=size(rddiv)
  nnrdiv=nnrdiv2/2
  ncyc=2+2*nrot  ! unknown variable number at a certain radius
  nmax=max(max(0,nrot),ndiv)   ! maximum wave number for rotating and divergent components
  fkij=0.0d0

  if(nmax>0)then
     allocate(sinen(nmax,nnt),stat=cstat)
     allocate(cosinen(nmax,nnt),stat=cstat)
     allocate(gkrr(nmax,nnrdiv2,nnr+1),stat=cstat)  ! Gk(r_p,r), r_p at rdh, r at rd
     if(cstat/=0)then
        call stdout( "Failed to allocate variables. stop.", "calc_fkij", -1 )
        stop
     end if
     sinen=0.0d0
     cosinen=0.0d0
     gkrr=0.0d0
  end if

  if(nrot>0)then
     if(nnk/=ncyc*(nnr-1)+2+2+ndiv*nnrdiv+1)then
        call stdout( "nnk is not identical to (2+2N)(m-1)+2+2+M*(mm-1). stop.", "calc_fkij", -1 )
        stop
     end if
  else
     if(nnk/=ncyc*(nnr-1)+2+ndiv*nnrdiv)then
        call stdout( "nnk is not identical to (2+2N)(m-1)+2+M*(mm-1). stop.", "calc_fkij", -1 )
        stop
     end if
  end if

  do ii=1,nnr
     dr(ii)=rdh(ii+1)-rdh(ii)
     dr_inv(ii)=1.0d0/dr(ii)
     r_inv(ii)=1.0d0/rd(ii)
     alp(ii)=(rd(ii)-rdh(ii))/(rdh(ii+1)-rdh(ii))
  end do

!$omp parallel default(shared)

!-- Set fixed variables for R-T, in advance
!$omp do schedule(runtime) private(ii,jj)
  do jj=1,nnt
     do ii=1,nnr
!ORG(thetad)        sines(ii,jj)=dsin(theta(jj)-thetad(ii,jj))
!ORG(thetad)        cosines(ii,jj)=dcos(theta(jj)-thetad(ii,jj))
        sines(ii,jj)=rtc*r_inv(ii)*dsin(theta(jj))  ! MOD = delta x sin(theta-thetad)
        cosines(ii,jj)=1.0d0+rtc*r_inv(ii)*dcos(theta(jj))  ! MOD = delta x cos(theta-thetad)
     end do
  end do
!$omp end do

!$omp barrier

  if(nmax>0)then

!$omp do schedule(runtime) private(kk,jj)
     do jj=1,nnt
        do kk=1,nmax
           sinen(kk,jj)=dsin(dble(kk)*theta(jj))
           cosinen(kk,jj)=dcos(dble(kk)*theta(jj))
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnr  ! For r
        do ii=1,nnrdiv2  ! For rdiv
           do kk=1,nmax
              gkrr(kk,ii,jj)=green_func( rddiv(ii), rd(jj), kk )
           end do
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii)
     do ii=1,nnrdiv2  ! For rddiv
        do kk=1,nmax
           gkrr(kk,ii,nnr+1)=green_func( rddiv(ii), rd(nnr)+dr(nnr), kk )
        end do
     end do
!$omp end do

  end if

!$omp barrier

!-- Set coefficients for VRT0 at each (ii,jj)
!$omp do schedule(runtime) private(ii,jj)
  do jj=1,nnt
     do ii=1,nnr
        fkij(1+ncyc*(ii-1),ii,jj)=-sines(ii,jj)
     end do
  end do
!$omp end do

!$omp barrier

!-- Set coefficients for VDR0 at each (ii,jj)
!$omp do schedule(runtime) private(ii,jj)
  do jj=1,nnt
     do ii=1,nnr
        fkij(2+ncyc*(ii-1),ii,jj)=cosines(ii,jj)
     end do
  end do
!$omp end do

!$omp barrier

  if(nrot>0)then

!-- Set coefficients for Phi_s and Phi_c at each (ii,jj) for wavenumber 1
!$omp do schedule(runtime) private(ii,jj)
     do jj=1,nnt
        do ii=1,nnr-2
           !-- Phi(s)*delta(s,i)
           fkij(2+1+ncyc*(ii-1),ii,jj)  &
  &       =(dr_inv(ii)*sinen(1,jj))  &
  &        *(-sines(ii,jj))  &
  &       +((1.0d0-alp(ii))*dble(1)*cosinen(1,jj))  &
  &        *(r_inv(ii)*cosines(ii,jj))

           !-- Phi(c)*delta(s,i)
           fkij(2+nrot+1+ncyc*(ii-1),ii,jj)  &
  &       =(dr_inv(ii)*cosinen(1,jj))  &
  &        *(-sines(ii,jj))  &
  &       -((1.0d0-alp(ii))*dble(1)*sinen(1,jj))  &
  &        *(r_inv(ii)*cosines(ii,jj))

           !-- Phi(s)*delta(s,i+1)
           fkij(2+1+ncyc*(ii),ii,jj)  &
  &       =(dr_inv(ii)*sinen(1,jj))  &
  &        *(sines(ii,jj))  &
  &       +(alp(ii)*dble(1)*cosinen(1,jj))  &
  &        *(r_inv(ii)*cosines(ii,jj))

           !-- Phi(c)*delta(s,i+1)
           fkij(2+nrot+1+ncyc*(ii),ii,jj)  &
  &       =(dr_inv(ii)*cosinen(1,jj))  &
  &        *(sines(ii,jj))  &
  &       -(alp(ii)*dble(1)*sinen(1,jj))  &
  &        *(r_inv(ii)*cosines(ii,jj))
        end do
     end do
!$omp end do

!$omp barrier

!-- Set coefficients for Phi_s and Phi_c 
!--     at one inner radius from the outermost (nnr-1,jj) for wavenumber 1
!$omp do schedule(runtime) private(jj)
     do jj=1,nnt

        !-- Phi(s)*delta(s,nnr-2)
        fkij(2+1+ncyc*(nnr-2),nnr-1,jj)  &
  &    =(dr_inv(nnr-1)*sinen(1,jj))*(-sines(nnr-1,jj))  &
  &    +((1.0d0-alp(nnr-1))*cosinen(1,jj))  &
  &     *(r_inv(nnr-1)*cosines(nnr-1,jj))

        !-- Phi(c)*delta(s,nnr-2)
        fkij(2+nrot+1+ncyc*(nnr-2),nnr-1,jj)  &
  &    =(dr_inv(nnr-1)*cosinen(1,jj))*(-sines(nnr-1,jj))  &
  &    -((1.0d0-alp(nnr-1))*dble(1)*sinen(1,jj))  &
  &     *(r_inv(nnr-1)*cosines(nnr-1,jj))

        !-- Phi(s)*delta(s,nnr-1)
        fkij(2+1+ncyc*(nnr-1),nnr-1,jj)  &
  &    =(dr_inv(nnr-1)*sinen(1,jj))*(sines(nnr-1,jj))  &
  &    +(alp(nnr-1)*cosinen(1,jj))  &
  &     *(r_inv(nnr-1)*cosines(nnr-1,jj))

        !-- Phi(c)*delta(s,nnr-1) (related to the outermost radius)
        fkij(2+2+ndiv*nnrdiv+1+ncyc*(nnr-1),nnr-1,jj)  &
  &    =(dr_inv(nnr-1)*cosinen(1,jj))*(sines(nnr-1,jj))  &
  &    -(alp(nnr-1)*dble(1)*sinen(1,jj))  &
  &     *(r_inv(nnr-1)*cosines(nnr-1,jj))

!        if(undeflag(nnr-1,jj).eqv..false.)then  ! Remove WN-1 Vm at one inner radius
!write(*,*) "before Vd at one inner", Vdij(nnr-1,jj), jj
!           Vdij(nnr-1,jj)  &
!  &       =Vdij(nnr-1,jj)  &
!  &       +rdh(nnr)*(Vsrn(2)-Vsrn(1))  &
!  &        *(dr_inv(nnr-1)*cosinen(1,jj)*sines(nnr-1,jj)  &
!  &         -alp(nnr-1)*r_inv(nnr-1)*sinen(1,jj)*cosines(nnr-1,jj))
!write(*,*) "after Vd at one inner", Vdij(nnr-1,jj), jj
!        end if

     end do
!$omp end do

!$omp barrier

!-- Set coefficients for Phi_s and Phi_c 
!--     at the outermost (nnr,jj) for wavenumber 1
!$omp do schedule(runtime) private(jj)
     do jj=1,nnt

        !-- Phi(s)*delta(s,nnr-1)
        fkij(2+1+ncyc*(nnr-1),nnr,jj)  &
  &    =(dr_inv(nnr)*sinen(1,jj))*(-sines(nnr,jj))  &
  &    +((1.0d0-alp(nnr))*cosinen(1,jj))  &
  &     *(r_inv(nnr)*cosines(nnr,jj))

        !-- Phi(s)*delta(s,nnr)
        fkij(2+2+ncyc*(nnr-1),nnr,jj)  &
  &    =(dr_inv(nnr)*sinen(1,jj))*(sines(nnr,jj))  &
  &    +(alp(nnr)*cosinen(1,jj))  &
  &     *(r_inv(nnr)*cosines(nnr,jj))

        if(undeflag(nnr,jj).eqv..false.)then  ! Remove WN-1 Vm at the outermost radius
write(*,*) "before Vd at the outer", Vdij(nnr,jj), jj
           Vdij(nnr,jj)  &
  &       =Vdij(nnr,jj)  &
  &       -(Vsrn(2)-Vsrn(1))*(-cosinen(1,jj)*sines(nnr,jj)  &
  &                           +sinen(1,jj)*cosines(nnr,jj))
write(*,*) "after Vd at the outer", Vdij(nnr,jj), jj
        end if

     end do
!$omp end do

!$omp barrier

!-- 2. For wavenumber >= 2
     if(nrot>1)then

!-- Set coefficients for Phi_s and Phi_c at each (ii,jj)
!$omp do schedule(runtime) private(kk,ii,jj)
        do jj=1,nnt
           do ii=1,nnr-2
              do kk=2,nrot
                 !-- Phi(s)*delta(s,i)
                 fkij(2+kk+ncyc*(ii-1),ii,jj)  &
  &             =(dr_inv(ii)*sinen(kk,jj))  &
  &              *(-sines(ii,jj))  &
  &             +((1.0d0-alp(ii))*dble(kk)*cosinen(kk,jj))  &
  &              *(r_inv(ii)*cosines(ii,jj))

                 !-- Phi(c)*delta(s,i)
                 fkij(2+nrot+kk+ncyc*(ii-1),ii,jj)  &
  &             =(dr_inv(ii)*cosinen(kk,jj))  &
  &              *(-sines(ii,jj))  &
  &             -((1.0d0-alp(ii))*dble(kk)*sinen(kk,jj))  &
  &              *(r_inv(ii)*cosines(ii,jj))

                 !-- Phi(s)*delta(s,i+1)
                 fkij(2+kk+ncyc*(ii),ii,jj)  &
  &             =(dr_inv(ii)*sinen(kk,jj))  &
  &              *(sines(ii,jj))  &
  &             +(alp(ii)*dble(kk)*cosinen(kk,jj))  &
  &              *(r_inv(ii)*cosines(ii,jj))

                 !-- Phi(c)*delta(s,i+1)
                 fkij(2+nrot+kk+ncyc*(ii),ii,jj)  &
  &             =(dr_inv(ii)*cosinen(kk,jj))  &
  &              *(sines(ii,jj))  &
  &             -(alp(ii)*dble(kk)*sinen(kk,jj))  &
  &              *(r_inv(ii)*cosines(ii,jj))
              end do
           end do
        end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,jj)
!-- Set coefficients for Phi_s and Phi_c at one inner radius from the outermost (nnr-1,jj)
        do jj=1,nnt
           do kk=2,nrot
              !-- Phi(s)*delta(s,nnr-1)
              fkij(2+kk+ncyc*(nnr-2),nnr-1,jj)  &
  &          =-dr_inv(nnr-1)*sinen(kk,jj)*sines(nnr-1,jj)  &
  &           +(1.0d0-alp(nnr-1))*dble(kk)*cosinen(kk,jj)  &
  &            *r_inv(nnr-1)*cosines(nnr-1,jj)

              !-- Phi(c)*delta(s,nnr-1)
              fkij(2+nrot+kk+ncyc*(nnr-2),nnr-1,jj)  &
  &          =-dr_inv(nnr-1)*cosinen(kk,jj)*sines(nnr-1,jj)  &
  &           -(1.0d0-alp(nnr-1))*dble(kk)*sinen(kk,jj)  &
  &            *r_inv(nnr-1)*cosines(nnr-1,jj)
           end do
        end do
!$omp end do
     end if

  end if

!$omp barrier

  if(ndiv>0)then

!-- Set coefficients for D_s and D_c at each (ii,jj)
!$omp do schedule(runtime) private(kk,pp,ii,jj)
     do jj=1,nnt
        do ii=1,nnr
           do pp=1,nnrdiv
              do kk=1,ndiv
                 fkij(ncyc*(nnr-1)+2+2+kk+ndiv*(pp-1),ii,jj)  &
!  &             =vareps(pp)*rdh(pp)  &  ! For Ds
!  &                       *(dble(kk)*dr(ii)*r_inv(ii)*gkrr(kk,pp,ii)  &
!  &                       *cosinen(kk,jj)*sines(ii,jj)  &
!  &                      -(gkrr(kk,pp,ii+1)-gkrr(kk,pp,ii))  &
!  &                       *sinen(kk,jj)*cosines(ii,jj))
!
!                 fkij(2+2*nrot+ndiv+kk+ncyc*(pp-2),ii,jj)  &
!  &             =-vareps(pp)*rddiv(pp)  &  ! For Dc
  &             =-(r_inv(ii)*dble(kk)*0.5d0*(rddiv(2*pp)-rddiv(2*pp-1))  &  ! For Dc
  &                *(rddiv(2*pp)*gkrr(kk,2*pp,ii)+rddiv(2*pp-1)*gkrr(kk,2*pp-1,ii))  &
  &                *sinen(kk,jj)*sines(ii,jj)  &
  &               +0.5d0*(rddiv(2*pp)-rddiv(2*pp-1))*dr_inv(ii)  &
  &                *(rddiv(2*pp)*(gkrr(kk,2*pp,ii+1)-gkrr(kk,2*pp,ii))  &
  &                 +rddiv(2*pp-1)*(gkrr(kk,2*pp-1,ii+1)-gkrr(kk,2*pp-1,ii)))  &
  &                *cosinen(kk,jj)*cosines(ii,jj))
              end do
           end do
        end do
     end do
!$omp end do

!$omp barrier

!!-- Set coefficients for D_s and D_c at the outermost (nnr,jj)
!!$omp do schedule(runtime) private(kk,ii,jj)
!     do jj=1,nnt
!        do ii=1,nnr
!           do kk=1,ndiv
!              fkij(2+2*nrot+kk+ncyc*(nnr-2),ii,jj)  &
!!  &          =vareps(nnr)*rdh(nnr)  &  ! For Ds
!!  &                    *(dble(kk)*dr(ii)*r_inv(ii)*gkrr(kk,nnr,ii)  &
!!  &                    *cosinen(kk,jj)*sines(ii,jj)  &
!!  &                   -(gkrr(kk,nnr,ii+1)-gkrr(kk,nnr,ii))  &
!!  &                    *sinen(kk,jj)*cosines(ii,jj))
!!
!!              fkij(2+2*nrot+ndiv+kk+ncyc*(nnr-2),ii,jj)  &
!  &          =-vareps(nnr)*rdh(nnr)  &  ! For Dc
!  &                   *(dble(kk)*dr(ii)*r_inv(ii)*gkrr(kk,nnr,ii)  &
!  &                    *sinen(kk,jj)*sines(ii,jj)  &
!  &                   +(gkrr(kk,nnr,ii+1)-gkrr(kk,nnr,ii))  &
!  &                    *cosinen(kk,jj)*cosines(ii,jj))
!           end do
!        end do
!     end do
!!$omp end do

!!$omp barrier

!tmpORG!-- Set coefficients for D_s and D_c at the outermost radius for D (nnr+1,jj)
!tmpORG!$omp do schedule(runtime) private(kk,ii,jj)
!tmpORG     do jj=1,nnt
!tmpORG        do ii=1,nnr
!tmpORG           do kk=1,ndiv
!tmpORG              fkij(2+kk+ncyc*(nnr-2)+ncyc,ii,jj)  &
!tmpORG!  &          =vareps(nnr+1)*rdh(nnr+1)  &  ! For Ds
!tmpORG!  &                    *(dble(kk)*dr(ii)*r_inv(ii)*gkrr(kk,nnr+1,ii)  &
!tmpORG!  &                    *cosinen(kk,jj)*sines(ii,jj)  &
!tmpORG!  &                   -(gkrr(kk,nnr+1,ii+1)-gkrr(kk,nnr+1,ii))  &
!tmpORG!  &                    *sinen(kk,jj)*cosines(ii,jj))
!tmpORG!
!tmpORG!              fkij(2+ndiv+kk+ncyc*(nnr-2)+ncyc,ii,jj)  &
!tmpORG  &          =-vareps(nnr+1)*rdh(nnr+1)  &  ! For Dc
!tmpORG  &                   *(dble(kk)*dr(ii)*r_inv(ii)*gkrr(kk,nnr+1,ii)  &
!tmpORG  &                    *sinen(kk,jj)*sines(ii,jj)  &
!tmpORG  &                   +(gkrr(kk,nnr+1,ii+1)-gkrr(kk,nnr+1,ii))  &
!tmpORG  &                    *cosinen(kk,jj)*cosines(ii,jj))
!tmpORG           end do
!tmpORG        end do
!tmpORG     end do
!tmpORG!$omp end do
!tmpORG
!tmpORG!$omp barrier

!-- Set coefficients for D_s and D_c at the innermost radius (without center)
!tmptmpORG     if(rdh(1)/=0.0d0)then
!tmptmpORG!$omp do schedule(runtime) private(kk,pp,ii,jj)
!tmptmpORG        do jj=1,nnt
!tmptmpORG           do ii=1,nnr
!tmptmpORG              do kk=1,ndiv
!tmptmpORG                 fkij(2+ncyc*(nnr-2)+ncyc+kk,ii,jj)  &
!tmpORG                 fkij(2+ndiv+ncyc*(nnr-2)+ncyc+kk,ii,jj)  &
!  &             =vareps(1)*rdh(1)  &  ! For Ds
!  &                       *(dble(kk)*dr(ii)*r_inv(ii)*gkrr(kk,1,ii)  &
!  &                       *cosinen(kk,jj)*sines(ii,jj)  &
!  &                      -(gkrr(kk,1,ii+1)-gkrr(kk,1,ii))  &
!  &                       *sinen(kk,jj)*cosines(ii,jj))
!
!                 fkij(2+2*ndiv+ncyc*(nnr-2)+ncyc+ndiv+kk,ii,jj)  &
!tmptmpORG  &             =-vareps(1)*rdh(1)  &  ! For Dc
!tmptmpORG  &                      *(dble(kk)*dr(ii)*r_inv(ii)*gkrr(kk,1,ii)  &
!tmptmpORG  &                       *sinen(kk,jj)*sines(ii,jj)  &
!tmptmpORG  &                      +(gkrr(kk,1,ii+1)-gkrr(kk,1,ii))  &
!tmptmpORG  &                       *cosinen(kk,jj)*cosines(ii,jj))
!tmptmpORG              end do
!tmptmpORG           end do
!tmptmpORG        end do
!tmptmpORG!$omp end do
!tmptmpORG     end if

  end if

!$omp end parallel

  if(nmax>0)then
     deallocate(sinen)
     deallocate(cosinen)
     deallocate(gkrr)
  end if

  call stdout( "Finish procedure.", "calc_fkij", 0 )

end subroutine calc_fkij

!--------------------------------------------------
!-- calculate a_kp from f_kij
!--------------------------------------------------

subroutine calc_fkij2akp( fkij, akp, undeflag )
  implicit none
  double precision, intent(in) :: fkij(:,:,:)
  double precision, intent(out) :: akp(size(fkij,1),size(fkij,1))
  logical, intent(in) :: undeflag(size(fkij,2),size(fkij,3))
  integer :: nnk, nni, nnj, ii, jj, kk, ll, cstat
  double precision, allocatable, dimension(:,:,:) :: fkl, fpl

!-- OpenMP variables
!$ integer :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS
  integer :: ompnum, omppe

  call stdout( "Enter procedure.", "calc_fkij2akp", 0 )

  nnk=size(fkij,1)
  nni=size(fkij,2)
  nnj=size(fkij,3)

  ompnum=1
  omppe=1
!$   ompnum=OMP_GET_MAX_THREADS()
  allocate(fkl(nni,nnj,ompnum),stat=cstat)
  allocate(fpl(nni,nnj,ompnum),stat=cstat)

  if(cstat/=0)then
     call stdout( "Failed to allocate variables. stop.", "calc_fkij2akp", -1 )
     stop
  end if

  do ll=1,nnk
!$omp parallel default(shared)
!$omp do schedule(dynamic) private(kk,omppe)
     do kk=ll,nnk
!$      omppe=OMP_GET_THREAD_NUM()+1
        fkl(1:nni,1:nnj,omppe)=fkij(kk,1:nni,1:nnj)
        fpl(1:nni,1:nnj,omppe)=fkij(ll,1:nni,1:nnj)
        akp(kk,ll)=matrix_sum( fkl(1:nni,1:nnj,omppe), fpl(1:nni,1:nnj,omppe),  &
  &                            undeflag(1:nni,1:nnj) )
        akp(ll,kk)=akp(kk,ll)
     end do
!$omp end do
!$omp end parallel
  end do

  deallocate(fkl)
  deallocate(fpl)

  call stdout( "Finish procedure.", "calc_fkij2akp", 0 )

end subroutine calc_fkij2akp

!--------------------------------------------------
!-- calculate b_k from f_kij and Vd
!--------------------------------------------------

subroutine calc_fkijVd2bk( vmax, fkij, Vdl, deltaij, bk, undeflag )
  implicit none
  double precision, intent(in) :: vmax
  double precision, intent(in) :: fkij(:,:,:)
  double precision, intent(in) :: Vdl(size(fkij,2),size(fkij,3))
  double precision, intent(in) :: deltaij(size(fkij,2),size(fkij,3))
  double precision, intent(out) :: bk(size(fkij,1))
  logical, intent(in) :: undeflag(size(fkij,2),size(fkij,3))
  integer :: nnk, nni, nnj, ii, jj, kk, ll, cstat
  double precision, allocatable, dimension(:,:,:) :: fkl
  double precision :: dVdl(size(fkij,2),size(fkij,3))

!-- OpenMP variables
!$ integer :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS
  integer :: ompnum, omppe

  call stdout( "Enter procedure.", "calc_fkijVd2bk", 0 )

  nnk=size(fkij,1)
  nni=size(fkij,2)
  nnj=size(fkij,3)

  ompnum=1
  omppe=1
!$   ompnum=OMP_GET_MAX_THREADS()

  allocate(fkl(nni,nnj,ompnum),stat=cstat)
  if(cstat/=0)then
     call stdout( "Failed to allocate variables. stop.", "calc_fkijVd2bk", -1 )
     stop
  end if

!$omp parallel default(shared)
!$omp do schedule(runtime) private(ii,jj)
  do jj=1,nnj
     do ii=1,nni
        if(undeflag(ii,jj).eqv..true.)then
           dVdl(ii,jj)=Vdl(ii,jj)
        else
           dVdl(ii,jj)=deltaij(ii,jj)*Vdl(ii,jj)
        end if
     end do
  end do
!$omp end do
!$omp barrier
!$omp do schedule(runtime) private(kk,omppe)
  do kk=1,nnk
!$   omppe=OMP_GET_THREAD_NUM()+1
     fkl(1:nni,1:nnj,omppe)=fkij(kk,1:nni,1:nnj)
     bk(kk)=matrix_sum( dVdl(1:nni,1:nnj), fkl(1:nni,1:nnj,omppe),  &
  &                     undeflag(1:nni,1:nnj) )/vmax
  end do
!$omp end do
!$omp end parallel

  deallocate(fkl)

  call stdout( "Finish procedure.", "calc_fkijVd2bk", 0 )

end subroutine calc_fkijVd2bk

!--------------------------------------------------
!-- Set each unknown variable from x_k
!--------------------------------------------------

subroutine set_xk2variables( nrot, ndiv, nnrdiv, Usrn, Vsrn, vmax,  &
  &                          xk, VRT0, VDR0,  &
!  &                          phis_n, phic_n, Ds_m, Dc_m, undef )
  &                          phis_n, phic_n, Ds_m, undef )
  implicit none
  integer, intent(in) :: nrot
  integer, intent(in) :: ndiv
  integer, intent(in) :: nnrdiv
  double precision, intent(in) :: Usrn(2)
  double precision, intent(in) :: Vsrn(2)
  double precision, intent(in) :: vmax
  double precision, intent(in) :: xk(:)  ! solved unknown variable vector
  double precision, intent(out) :: VRT0(:)
  double precision, intent(out) :: VDR0(size(VRT0))
  double precision, intent(out), optional :: phis_n(nrot,size(VRT0)+1)
  double precision, intent(out), optional :: phic_n(nrot,size(VRT0)+1)
  double precision, intent(out), optional :: Ds_m(ndiv,nnrdiv)
!  double precision, intent(out), optional :: Dc_m(ndiv,size(VRT0)+1)
  double precision, intent(in), optional :: undef

  integer :: ii, kk, nnk, nnr, ncyc
  double precision :: Usrn_n(2), Vsrn_n(2)

  call stdout( "Enter procedure.", "set_xk2variables", 0 )

  nnk=size(xk)
  nnr=size(VRT0)
  ncyc=2+2*nrot
  Usrn_n(1:2)=Usrn(1:2)/vmax
  Vsrn_n(1:2)=Vsrn(1:2)/vmax

!-- Set VRT0 and VDR0
  do ii=1,nnr-1
     VRT0(ii)=xk(1+ncyc*(ii-1))
     VDR0(ii)=xk(2+ncyc*(ii-1))
write(*,*) "check [VRT0, VDR0] = ", VRT0(ii), VDR0(ii), ii
  end do
  VRT0(nnr)=xk(1+ncyc*(nnr-1))
  VDR0(nnr)=xk(2+ncyc*(nnr-1))
write(*,*) "check [VRT0, VDR0] = ", VRT0(nnr), VDR0(nnr), nnr

!-- Set Phi_s and Phi_c
  if(nrot>0)then
!$omp parallel default(shared)

!!$omp do schedule(runtime) private(ii)
!!-- For wn1
!     do ii=1,nnr-1
!        phis_n(1,ii)=xk(2+1+ncyc*(ii-1))
!        phic_n(1,ii)=xk(2+nrot+1+ncyc*(ii-1))
!     end do
!!$omp end do
!
!!$omp barrier
!
!     if(nrot>1)then  !-- For over wn2

!$omp do schedule(runtime) private(kk,ii)
        do ii=1,nnr-1
           do kk=1,nrot
              phis_n(kk,ii)=xk(2+kk+ncyc*(ii-1))
              phic_n(kk,ii)=xk(2+nrot+kk+ncyc*(ii-1))
           end do
        end do
!$omp end do
!$omp end parallel

        phis_n(1,nnr)=xk(2+1+ncyc*(nnr-1))
        phis_n(1,nnr+1)=xk(2+2+ncyc*(nnr-1))

!!$omp barrier
!!$omp do schedule(runtime) private(kk)
!        do kk=2,nrot
!           phis_n(kk,nnr-1)=xk(2+(kk-1)+ncyc*(nnr-2))
!           phic_n(kk,nnr-1)=xk(2+nrot+(kk-1)+ncyc*(nnr-2))
!        end do
!!$omp end do
!     end if

  end if

!-- Set D_s and D_c
  if(ndiv>0)then

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,ii)
     do ii=1,nnrdiv
        do kk=1,ndiv
           Ds_m(kk,ii)=xk(ncyc*(nnr-1)+2+2+kk+ndiv*(ii-1))
!           Ds_m(kk,ii+1)=xk(2+2*nrot+kk+ncyc*(ii-1))
!           Dc_m(kk,ii+1)=xk(2+2*nrot+ndiv+kk+ncyc*(ii-1))
!           Dc_m(kk,ii+1)=xk(2+2*nrot+kk+ncyc*(ii-1))
        end do
     end do
!$omp end do

!!$omp barrier
!!$omp do schedule(runtime) private(kk)
!     do kk=1,ndiv  ! at ii=nnr-1
!        Ds_m(kk,nnr)=xk(2+2*nrot+kk+ncyc*(nnr-2))
!     end do
!!$omp end do
!!$omp barrier
!!$omp do schedule(runtime) private(kk)
!     do kk=1,ndiv  ! at ii=nnr
!        Ds_m(kk,nnr+1)=0.0d0
!tmpORG        Ds_m(kk,nnr+1)=xk(2+kk+ncyc*(nnr-2)+ncyc)
!     end do
!!$omp end do
!!$omp barrier
!     if(size(xk)>ncyc*(nnr-1)+2)then  ! At innermost radius (without center)
!        Ds_m(1:ndiv,1)=0.0d0
!tmpORG     if(size(xk)>ncyc*(nnr-1)+2+ndiv)then  ! At innermost radius (without center)
!tmptmpORG!$omp do schedule(runtime) private(kk)
!tmptmpORG        do kk=1,ndiv
!tmptmpORG           Ds_m(kk,1)=xk(2+kk+ncyc*(nnr-2)+ncyc)  ! nnr = 1
!tmpORG           Ds_m(kk,1)=xk(2+ndiv+kk+ncyc*(nnr-2)+ncyc)  ! nnr = 1
!           Dc_m(kk,1)=xk(ndiv+kk+ncyc*nnr)
!           Dc_m(kk,1)=xk(kk+ncyc*nnr)  ! nnr = m - 1
!tmptmpORG        end do
!tmptmpORG!$omp end do
!     else  ! At innermost radius (with center)
!        Ds_m(1:ndiv,1)=0.0d0
!        Dc_m(1:ndiv,1)=0.0d0
!     end if

!$omp end parallel

do ii=1,nnrdiv
write(*,*) "check [Dc_m] = ", Ds_m(1:ndiv,ii), ii
!write(*,*) "check [Ds_m, Dc_m] = ", Ds_m(1:ndiv,ii), Dc_m(1:ndiv,ii), ii
end do
  end if

  call stdout( "Finish procedure.", "set_xk2variables", 0 )

end subroutine set_xk2variables

!--------------------------------------------------
!-- Calculate Vr and Vt components of rotating wind
!--------------------------------------------------

subroutine calc_phi2Vrot( nrot, Usrn, Vsrn, vmax, rd, rdh, theta, VRT0_r,  &
!subroutine calc_phi2Vrot( nrot, Usrn, Vsrn, vmax, rmax, rd, rdh, theta, VRT0_r,  &
  &                       VRT0_rt, VRT_nrt, VRR_nrt,  &
  &                       phis_nr, phic_nr, undef )
  implicit none
  integer, intent(in) :: nrot
  double precision, intent(in) :: Usrn(2)
  double precision, intent(in) :: Vsrn(2)
  double precision, intent(in) :: vmax
!  double precision, intent(in) :: rmax
  double precision, intent(in) :: rd(:)  ! Normalized radius
  double precision, intent(in) :: rdh(size(rd)+1)
  double precision, intent(in) :: theta(:)
  double precision, intent(in) :: VRT0_r(size(rd))
  double precision, intent(out) :: VRT0_rt(size(rd),size(theta))
  double precision, intent(out), optional :: VRT_nrt(nrot,size(rd),size(theta))
  double precision, intent(out), optional :: VRR_nrt(nrot,size(rd),size(theta))
  double precision, intent(inout), optional :: phis_nr(nrot,size(rd)+1)
  double precision, intent(inout), optional :: phic_nr(nrot,size(rd)+1)
  double precision, intent(in), optional :: undef  ! No use

  integer :: ii, jj, kk, nnr, nnt, cstat
  double precision :: Usrn_n(2), Vsrn_n(2)
  double precision, dimension(size(rd)) :: dr, dr_inv, r_inv, alp
  double precision, allocatable, dimension(:,:) :: cosinen, sinen

  call stdout( "Enter procedure.", "calc_phi2Vrot", 0 )

  nnr=size(rd)
  nnt=size(theta)

  if(nrot>0)then
     allocate(cosinen(nrot,nnt),stat=cstat)
     allocate(sinen(nrot,nnt),stat=cstat)
     if(cstat/=0)then
        call stdout( "Failed to allocate variables. stop.", "calc_phi2Vrot", -1 )
        stop
     end if
!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,jj)
     do jj=1,nnt
        do kk=1,nrot
           cosinen(kk,jj)=dcos(dble(kk)*theta(jj))
           sinen(kk,jj)=dsin(dble(kk)*theta(jj))
        end do
     end do
!$omp end do
!$omp end parallel
  end if

  do ii=1,nnr
     dr(ii)=rdh(ii+1)-rdh(ii)
     dr_inv(ii)=1.0d0/dr(ii)
     alp(ii)=(rd(ii)-rdh(ii))/(rdh(ii+1)-rdh(ii))
  end do
  r_inv(1:nnr)=1.0d0/rd(1:nnr)
  Usrn_n(1:2)=Usrn(1:2)/vmax
  Vsrn_n(1:2)=Vsrn(1:2)/vmax

  do ii=1,nnr
     VRT0_rt(ii,1:nnt)=VRT0_r(ii)*vmax
  end do

  if(nrot>0)then

     VRT_nrt=0.0d0
     VRR_nrt=0.0d0

     !-- set the outermost boundary for wavenumber 1
     phic_nr(1,nnr+1)=-rdh(nnr+1)*(Vsrn_n(2)-Vsrn_n(1))
     phic_nr(1,nnr)=-rdh(nnr)*(Vsrn_n(2)-Vsrn_n(1))

     !-- set the outermost boundary for wavenumber 2
     if(nrot>1)then
        do kk=2,nrot
           phis_nr(kk,nnr+1)=0.0d0
           phic_nr(kk,nnr+1)=0.0d0
           phis_nr(kk,nnr)=0.0d0
           phic_nr(kk,nnr)=0.0d0
        end do
     end if
do ii=1,nnr+1
write(*,*) "check [phis_n, phic_n] = ", phis_nr(1:nrot,ii), phic_nr(1:nrot,ii), ii
end do

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnt
        do ii=1,nnr
           do kk=1,nrot
              VRT_nrt(kk,ii,jj)=-dr_inv(ii)*((phis_nr(kk,ii+1)-phis_nr(kk,ii))*sinen(kk,jj)  &
  &                                           +(phic_nr(kk,ii+1)-phic_nr(kk,ii))*cosinen(kk,jj))
              VRR_nrt(kk,ii,jj)=dble(kk)*r_inv(ii)  &
  &                            *((alp(ii)*phis_nr(kk,ii+1)+(1.0d0-alp(ii))*phis_nr(kk,ii))*cosinen(kk,jj)  &
  &                             -(alp(ii)*phic_nr(kk,ii+1)+(1.0d0-alp(ii))*phic_nr(kk,ii))*sinen(kk,jj))
              VRT_nrt(kk,ii,jj)=VRT_nrt(kk,ii,jj)*vmax
              VRR_nrt(kk,ii,jj)=VRR_nrt(kk,ii,jj)*vmax
           end do
        end do
     end do
!$omp end do
!$omp end parallel

  end if

  if(nrot>0)then
     deallocate(cosinen)
     deallocate(sinen)
  end if

  call stdout( "Finish procedure.", "calc_phi2Vrot", 0 )

end subroutine calc_phi2Vrot

!--------------------------------------------------
!-- Calculate Vr and Vt components of divergent wind
!--------------------------------------------------

!subroutine calc_D2Vdiv( ndiv, vmax, rmax, rd, rdh, theta, VDR0_r,  &
subroutine calc_D2Vdiv( ndiv, vmax, rd, rdh, theta, rddiv, VDR0_r,  &
  &                     VDR0_rt, VDT_mrt, VDR_mrt, Ds_mr, undef )
!  &                     VDR0_rt, VDT_mrt, VDR_mrt, Ds_mr, Dc_mr, undef )
  implicit none
  integer, intent(in) :: ndiv
  double precision, intent(in) :: vmax
!  double precision, intent(in) :: rmax
  double precision, intent(in) :: rd(:)
  double precision, intent(in) :: rdh(size(rd)+1)
  double precision, intent(in) :: theta(:)
  double precision, intent(in) :: rddiv(:)
  double precision, intent(in) :: VDR0_r(size(rd))
  double precision, intent(out) :: VDR0_rt(size(rd),size(theta))
  double precision, intent(out), optional :: VDT_mrt(ndiv,size(rd),size(theta))
  double precision, intent(out), optional :: VDR_mrt(ndiv,size(rd),size(theta))
  double precision, intent(in), optional :: Ds_mr(ndiv,size(rddiv))
!  double precision, intent(in), optional :: Dc_mr(ndiv,size(rd)+1)
  double precision, intent(in), optional :: undef

  integer :: ii, jj, kk, nnr, nnt, nnrdiv, nnrdiv2, cstat, irad
!  double precision :: rmax_inv
  double precision, dimension(size(rd)) :: dr_inv, dr, r_inv
  double precision, allocatable, dimension(:,:) :: cosinen, sinen
!  double precision, allocatable, dimension(:,:) :: gkrrhDs, gkrrhDc, dgkrrDs, dgkrrDc
  double precision, allocatable, dimension(:,:,:) :: gkrr, gkrrh, dgkrr
  double precision :: tmp_Ds_mr(ndiv,size(rd)+1)

  call stdout( "Enter procedure.", "calc_D2Vdiv", 0 )

  nnr=size(rd)
  nnt=size(theta)
  nnrdiv2=size(rddiv)
  nnrdiv=nnrdiv2/2

  if(ndiv>0)then
     allocate(cosinen(ndiv,nnt),stat=cstat)
     allocate(sinen(ndiv,nnt),stat=cstat)
     allocate(gkrr(ndiv,nnr+1,nnr+1),stat=cstat)  ! Gk(r_p,r), r_p at rdh, r at rdh
     allocate(gkrrh(ndiv,nnr+1,nnr+1),stat=cstat)  ! Gk(r_p,r), r_p at rdh, r at rdh
     allocate(dgkrr(ndiv,nnr+1,nnr+1),stat=cstat)  ! Gk(r_p,r+1)-Gk(r_p,r), r_p at rdh, r at rdh
     if(cstat/=0)then
        call stdout( "Failed to allocate variables. stop.", "calc_D2Vdiv", -1 )
        stop
     end if
     cosinen=0.0d0
     sinen=0.0d0
     gkrr=0.0d0
     gkrrh=0.0d0
     dgkrr=0.0d0

     tmp_Ds_mr=0.0d0
     do kk=1,ndiv
        do ii=1,nnrdiv
           call interpo_search_1d( rdh, rddiv(2*ii-1), irad )
           tmp_Ds_mr(kk,irad)=Ds_mr(kk,ii)
           call interpo_search_1d( rdh, rddiv(2*ii), irad )
           tmp_Ds_mr(kk,irad)=Ds_mr(kk,ii)
        end do
     end do
  end if

  do ii=1,nnr
     dr(ii)=rdh(ii+1)-rdh(ii)
     dr_inv(ii)=1.0d0/dr(ii)
  end do
  r_inv(1:nnr)=1.0d0/rd(1:nnr)

!  rmax_inv=1.0d0/rmax

  do ii=1,nnr
     VDR0_rt(ii,1:nnt)=VDR0_r(ii)*vmax
  end do

  if(ndiv>0)then

     VDT_mrt=0.0d0
     VDR_mrt=0.0d0

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,jj)
     do jj=1,nnt
        do kk=1,ndiv
           cosinen(kk,jj)=dcos(dble(kk)*theta(jj))
           sinen(kk,jj)=dsin(dble(kk)*theta(jj))
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnr  ! For r
        do ii=1,nnr+1  ! For r_p
           do kk=1,ndiv
              gkrr(kk,ii,jj)=green_func( rdh(ii), rd(jj), kk )
           end do
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii)
     ! At the outer boundary for r
     do ii=1,nnr+1  ! For r_p
        do kk=1,ndiv
           gkrr(kk,ii,nnr+1)=green_func( rdh(ii), rdh(nnr+1), kk )
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnr+1  ! For r
        do ii=1,nnr+1  ! For r_p
           do kk=1,ndiv
              gkrrh(kk,ii,jj)=green_func( rdh(ii), rdh(jj), kk )
           end do
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnr  ! For r
        do ii=1,nnr+1  ! For r_p
           do kk=1,ndiv
              dgkrr(kk,ii,jj)=gkrr(kk,ii,jj+1)-gkrr(kk,ii,jj)
           end do
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnt
        do ii=1,nnr
           do kk=1,ndiv
              VDT_mrt(kk,ii,jj)=-(dr(ii)*dble(kk)*r_inv(ii)*vmax)  &
!  &                              *(line_integral( nnr, rdh(1:nnr+1), gkrr(kk,1:nnr+1,ii), Ds_mr(kk,1:nnr+1) )  &
!  &                                *cosinen(kk,jj)) ! &
  &                               *(-line_integral( nnr, rdh(1:nnr+1), gkrr(kk,1:nnr+1,ii), tmp_Ds_mr(kk,1:nnr+1) )  &
  &                                 *sinen(kk,jj))
!  &                               *(-line_integral( nnr, rdh(1:nnr+1), gkrr(kk,1:nnr+1,ii), Dc_mr(kk,1:nnr+1) )  &
!  &                                 *sinen(kk,jj))

              VDR_mrt(kk,ii,jj)=-(vmax)  &
!  &                              *(line_integral( nnr, rdh(1:nnr+1), dgkrr(kk,1:nnr+1,ii), Ds_mr(kk,1:nnr+1) )  &
!  &                               *sinen(kk,jj)) ! &
  &                              *(+line_integral( nnr, rdh(1:nnr+1), dgkrr(kk,1:nnr+1,ii), tmp_Ds_mr(kk,1:nnr+1) )  &
  &                                 *cosinen(kk,jj))
!  &                              *(+line_integral( nnr, rdh(1:nnr+1), dgkrr(kk,1:nnr+1,ii), Dc_mr(kk,1:nnr+1) )  &
!  &                                 *cosinen(kk,jj))
           end do
        end do
     end do
!$omp end do
!$omp end parallel

  end if

  if(ndiv>0)then
     deallocate(cosinen)
     deallocate(sinen)
     deallocate(gkrr)
     deallocate(gkrrh)
     deallocate(dgkrr)
  end if

  call stdout( "Finish procedure.", "calc_D2Vdiv", 0 )

end subroutine calc_D2Vdiv

!--------------------------------------------------
!-- Calculate total wind from all wavenumbers
!--------------------------------------------------

subroutine calc_Vn2Vtot( nrot, ndiv, V0, Vn, Vm, Vtot, undef )
  implicit none
  integer, intent(in) :: nrot
  integer, intent(in) :: ndiv
  double precision, intent(in) :: V0(:,:)
  double precision, intent(in) :: Vn(nrot,size(V0,1),size(V0,2))
  double precision, intent(in) :: Vm(ndiv,size(V0,1),size(V0,2))
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
           do kk=1,nrot
              if(Vn(kk,ii,jj)/=undef)then
                 Vtot(ii,jj)=Vtot(ii,jj)+Vn(kk,ii,jj)
              end if
           end do
           do kk=1,ndiv
              if(Vn(kk,ii,jj)/=undef)then
                 Vtot(ii,jj)=Vtot(ii,jj)+Vm(kk,ii,jj)
              end if
           end do
        end do
     end do
  else
     do jj=1,nnt
        do ii=1,nnr
           do kk=1,nrot
              Vtot(ii,jj)=Vtot(ii,jj)+Vn(kk,ii,jj)
           end do
           do kk=1,ndiv
              Vtot(ii,jj)=Vtot(ii,jj)+Vm(kk,ii,jj)
           end do
        end do
     end do
  end if

  call stdout( "Finish procedure.", "calc_Vn2Vtot", 0 )

end subroutine calc_Vn2Vtot

!--------------------------------------------------
!-- calculate inner product of two vectors
!--------------------------------------------------

double precision function dot_prod( v1, v2 )
  implicit none
  double precision, intent(in) :: v1(:)
  double precision, intent(in) :: v2(size(v1))
  integer :: ii, ni
  double precision :: res

  ni=size(v1)

  res=0.0d0

  do ii=1,ni
     res=res+v1(ii)*v2(ii)
  end do

  dot_prod=res

  return

end function dot_prod

!--------------------------------------------------
!-- calculate product for a component in a matrix
!--------------------------------------------------

double precision function matrix_sum( aij, akj, undeflag )
  implicit none
  double precision, intent(in) :: aij(:,:)
  double precision, intent(in) :: akj(size(aij,1),size(aij,2))
  logical, intent(in), optional :: undeflag(size(aij,1),size(aij,2))
  integer :: ii, jj, ni, nj
  double precision :: res

  ni=size(aij,1)
  nj=size(aij,2)

  res=0.0d0

  if(present(undeflag))then
     do jj=1,nj
        do ii=1,ni
           if(undeflag(ii,jj).eqv..false.)then
              res=res+aij(ii,jj)*akj(ii,jj)
           end if
        end do
     end do
  else
     do jj=1,nj
        do ii=1,ni
           res=res+aij(ii,jj)*akj(ii,jj)
        end do
     end do
  end if

  matrix_sum=res

  return

end function matrix_sum

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

!--------------------------------------------------
! Calculate streamfunction for each wavenumber
!--------------------------------------------------

subroutine calc_Phi2Phin( nrot, vmax, rmax, rd, rdh, theta, phis_nr, phic_nr, phi_nr )
  implicit none
  integer, intent(in) :: nrot
  double precision, intent(in) :: vmax
  double precision, intent(in) :: rmax
  double precision, intent(in) :: rd(:)  ! Normalized radius
  double precision, intent(in) :: rdh(size(rd)+1)
  double precision, intent(in) :: theta(:)
  double precision, intent(in) :: phis_nr(nrot,size(rd)+1)
  double precision, intent(in) :: phic_nr(nrot,size(rd)+1)
  double precision, intent(out) :: phi_nr(nrot,size(rd),size(theta))

  integer :: ii, jj, kk, nnr, nnt, cstat
  double precision, dimension(size(rd)) :: alp
  double precision, dimension(nrot,size(theta)) :: sinen, cosinen
  double precision :: phisi, phici

  call stdout( "Enter procedure.", "calc_Phi2Phin", 0 )

  nnr=size(rd)
  nnt=size(theta)

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,ii)

  do jj=1,nnt
     do kk=1,nrot
        sinen(kk,jj)=dsin(dble(kk)*theta(jj))
        cosinen(kk,jj)=dcos(dble(kk)*theta(jj))
     end do
  end do

!$omp end do
!$omp end parallel

  do ii=1,nnr
     alp(ii)=(rd(ii)-rdh(ii))/(rdh(ii+1)-rdh(ii))
  end do

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,ii,jj,phisi,phici)

  do jj=1,nnt
     do ii=1,nnr
        do kk=1,nrot
           phisi=alp(ii)*phis_nr(kk,ii+1)+(1.0d0-alp(ii))*phis_nr(kk,ii)
           phici=alp(ii)*phic_nr(kk,ii+1)+(1.0d0-alp(ii))*phic_nr(kk,ii)
           phi_nr(kk,ii,jj)=(phisi*sinen(kk,jj)+phici*cosinen(kk,jj))*vmax*rmax
        end do
     end do
  end do

!$omp end do
!$omp end parallel

  call stdout( "Finish procedure.", "calc_Phi2Phin", 0 )

end subroutine calc_Phi2Phin

!--------------------------------------------------
! Calculate vorticity for each wavenumber
!--------------------------------------------------

subroutine calc_Phi2Zetan( nrot, vmax, rmax, rd, rdh, theta, phis_nr, phic_nr, zeta_nr )
  implicit none
  integer, intent(in) :: nrot
  double precision, intent(in) :: vmax
  double precision, intent(in) :: rmax
  double precision, intent(in) :: rd(:)  ! Normalized radius
  double precision, intent(in) :: rdh(size(rd)+1)
  double precision, intent(in) :: theta(:)
  double precision, intent(in) :: phis_nr(nrot,size(rd)+1)
  double precision, intent(in) :: phic_nr(nrot,size(rd)+1)
  double precision, intent(out) :: zeta_nr(nrot,size(rd),size(theta))

  integer :: ii, jj, kk, nnr, nnt, cstat
  double precision, dimension(size(rd)) :: alp
  double precision, dimension(nrot,size(rd)+1) :: d2psdr2, d2pcdr2, dpsdr, dpcdr, psinv, pcinv
  double precision, dimension(nrot,size(theta)) :: sinen, cosinen
  double precision, dimension(nrot,0:size(rd)+2) :: tmpphis, tmpphic
  double precision, dimension(size(rd)+1) :: drc_inv, drf_inv, drb_inv, rh_inv, rh2_inv
  double precision, dimension(size(rd)) :: r_inv
  double precision :: dpfs, dpfc, dpbs, dpbc, rmax_inv
  double precision :: d2psdr2i, dpsdri, d2pcdr2i, dpcdri, phisi, phici

  call stdout( "Enter procedure.", "calc_Phi2Zetan", 0 )

  if(nrot<1)then
     call stdout( "nrot is greater than 0. stop.", "calc_Phi2Zetan", -1 )
     stop
  end if

  nnr=size(rd)
  nnt=size(theta)

  drc_inv(1)=1.0d0/(rdh(2)-rdh(1))
  drc_inv(nnr+1)=1.0d0/(rdh(nnr+1)-rdh(nnr))
  drb_inv(1)=1.0d0/(rdh(2)-rdh(1))
  drb_inv(nnr+1)=1.0d0/(rdh(nnr+1)-rdh(nnr))
  drf_inv(1)=1.0d0/(rdh(2)-rdh(1))
  drf_inv(nnr+1)=1.0d0/(rdh(nnr+1)-rdh(nnr))
  rh_inv(nnr+1)=1.0/rdh(nnr+1)
  if(rdh(1)==0.0d0)then
     rh_inv(1)=0.0d0
  end if
  if(rd(1)==0.0d0)then
     r_inv(1)=0.0d0
  end if
  do ii=2,nnr
     drc_inv(ii)=1.0d0/(rdh(ii+1)-rdh(ii-1))
     drb_inv(ii)=1.0d0/(rdh(ii)-rdh(ii-1))
     drf_inv(ii)=1.0d0/(rdh(ii+1)-rdh(ii))
     rh_inv(ii)=1.0d0/rdh(ii)
     r_inv(ii)=1.0d0/rd(ii)
  end do

  rmax_inv=1.0d0/rmax
  rh2_inv=rh_inv**2

  tmpphis(1:nrot,1:nnr+1)=phis_nr(1:nrot,1:nnr+1)
  tmpphic(1:nrot,1:nnr+1)=phic_nr(1:nrot,1:nnr+1)
  tmpphis(1:nrot,0)=phis_nr(1:nrot,1)          ! dummy
  tmpphis(1:nrot,nnr+2)=phis_nr(1:nrot,nnr+1)  ! dummy
  tmpphic(1:nrot,0)=phic_nr(1:nrot,1)          ! dummy
  tmpphic(1:nrot,nnr+2)=phic_nr(1:nrot,nnr+1)  ! dummy

  zeta_nr=0.0d0

  do ii=1,nnr
     alp(ii)=(rd(ii)-rdh(ii))/(rdh(ii+1)-rdh(ii))
  end do

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,ii)

  do jj=1,nnt
     do kk=1,nrot
        sinen(kk,jj)=dsin(dble(kk)*theta(jj))
        cosinen(kk,jj)=dcos(dble(kk)*theta(jj))
     end do
  end do

!$omp end do
!$omp barrier
!$omp do schedule(runtime) private(kk,ii)

  do ii=1,nnr+1
     do kk=1,nrot
        d2psdr2(kk,ii)=2.0d0*drc_inv(ii)  &
  &                         *((tmpphis(kk,ii+1)-tmpphis(kk,ii))*drf_inv(ii)  &
  &                          +(tmpphis(kk,ii-1)-tmpphis(kk,ii))*drb_inv(ii))
        d2pcdr2(kk,ii)=2.0d0*drc_inv(ii)  &
  &                         *((tmpphic(kk,ii+1)-tmpphic(kk,ii))*drf_inv(ii)  &
  &                          +(tmpphic(kk,ii-1)-tmpphic(kk,ii))*drb_inv(ii))
        dpsdr(kk,ii)=0.5d0*((tmpphis(kk,ii+1)-tmpphis(kk,ii))*drf_inv(ii)  &
  &                        -(tmpphis(kk,ii-1)-tmpphis(kk,ii))*drb_inv(ii))
        dpcdr(kk,ii)=0.5d0*((tmpphic(kk,ii+1)-tmpphic(kk,ii))*drf_inv(ii)  &
  &                        -(tmpphic(kk,ii-1)-tmpphic(kk,ii))*drb_inv(ii))
     end do
  end do

!$omp end do
!$omp barrier
!$omp do schedule(runtime) private(kk,ii,jj,d2psdr2i,dpsdri,d2pcdr2i,dpcdri,phisi,phici)

  do jj=1,nnt
     do ii=1,nnr
        do kk=1,nrot
           phisi=alp(ii)*phis_nr(kk,ii+1)+(1.0d0-alp(ii))*phis_nr(kk,ii)
           phici=alp(ii)*phic_nr(kk,ii+1)+(1.0d0-alp(ii))*phic_nr(kk,ii)
           d2psdr2i=alp(ii)*d2psdr2(kk,ii+1)+(1.0d0-alp(ii))*d2psdr2(kk,ii)
           dpsdri=alp(ii)*dpsdr(kk,ii+1)+(1.0d0-alp(ii))*dpsdr(kk,ii)
           d2pcdr2i=alp(ii)*d2pcdr2(kk,ii+1)+(1.0d0-alp(ii))*d2pcdr2(kk,ii)
           dpcdri=alp(ii)*dpcdr(kk,ii+1)+(1.0d0-alp(ii))*dpcdr(kk,ii)
           zeta_nr(kk,ii,jj)=((d2psdr2i+dpsdri*r_inv(ii)  &
  &                           -((dble(kk)*r_inv(ii))**2)*phisi)  &
  &                           *sinen(kk,jj)  &
  &                          +(d2pcdr2i+dpcdri*r_inv(ii)  &
  &                           -((dble(kk)*r_inv(ii))**2)*phici)  &
  &                           *cosinen(kk,jj))  &
  &                         *vmax*rmax_inv
        end do
     end do
  end do

!$omp end do
!$omp end parallel

  call stdout( "Finish procedure.", "calc_Phi2Zetan", 0 )

end subroutine calc_Phi2Zetan

!--------------------------------------------------
! Calculate pseudo retrieval of VT and VR for GVTD
!--------------------------------------------------

subroutine calc_pseudo_GVTD0( nrot, vmax, rtc, rd, rdh, VRT0_r, VDR0_r,  &
  &                           phis_nr, phic_nr, VRT0_GVTD_r, VDR0_GVTD_r )
  implicit none
  integer, intent(in) :: nrot
  double precision, intent(in) :: vmax
  double precision, intent(in) :: rtc  ! Normalized radius
  double precision, intent(in) :: rd(:)  ! Normalized radius
  double precision, intent(in) :: rdh(size(rd)+1)  ! Normalized radius
  double precision, intent(in) :: VRT0_r(size(rd))
  double precision, intent(in) :: VDR0_r(size(rd))
  double precision, intent(in) :: phis_nr(nrot,size(rd)+1)
  double precision, intent(in) :: phic_nr(nrot,size(rd)+1)
  double precision, intent(out) :: VRT0_GVTD_r(size(rd))
  double precision, intent(out) :: VDR0_GVTD_r(size(rd))

  integer :: ii, jj, kk, nnr, cstat
  double precision, dimension(size(rd)) :: dr, dr_inv, r_inv, alp
  double precision, dimension(size(rd)) :: tmp_VRT0_r, tmp_VDR0_r

  call stdout( "Enter procedure.", "calc_pseudo_GVTD0", 0 )

  nnr=size(rd)

  do ii=1,nnr
     dr(ii)=rdh(ii+1)-rdh(ii)
     dr_inv(ii)=1.0d0/dr(ii)
     alp(ii)=(rd(ii)-rdh(ii))/(rdh(ii+1)-rdh(ii))
  end do
  r_inv(1:nnr)=1.0d0/rd(1:nnr)

  tmp_VRT0_r(1:nnr)=VRT0_r(1:nnr)
  tmp_VDR0_r(1:nnr)=VDR0_r(1:nnr)

  if(nrot>0)then

!$omp parallel default(shared)

!-- For wavenumber 1
!$omp do schedule(runtime) private(ii)
     do ii=1,nnr
        tmp_VRT0_r(ii)=tmp_VRT0_r(ii)  &
  &                    +((alp(ii)*phic_nr(1,ii+1)+(1.0d0-alp(ii))*phic_nr(1,ii))/rtc)*vmax
        tmp_VDR0_r(ii)=tmp_VDR0_r(ii)  &
  &                    +r_inv(ii)*(alp(ii)*phis_nr(1,ii+1)+(1.0d0-alp(ii))*phis_nr(1,ii))*vmax
     end do
!$omp end do

!$omp barrier

!-- For wavenumber 2
     if(nrot>1)then
!$omp do schedule(runtime) private(ii)
        do ii=1,nnr
           tmp_VRT0_r(ii)=tmp_VRT0_r(ii)  &
  &                       +2.0d0*r_inv(ii)*(alp(ii)*phic_nr(2,ii+1)+(1.0d0-alp(ii))*phic_nr(2,ii))*vmax
           tmp_VDR0_r(ii)=tmp_VDR0_r(ii)  &
  &                       +2.0d0*r_inv(ii)*(alp(ii)*phis_nr(2,ii+1)+(1.0d0-alp(ii))*phis_nr(2,ii))*vmax
        end do
!$omp end do
     end if

!$omp barrier

!-- For wavenumber 3
     if(nrot>2)then
!$omp do schedule(runtime) private(ii)
        do ii=1,nnr
           tmp_VRT0_r(ii)=tmp_VRT0_r(ii)  &
  &                       +3.0d0*((alp(ii)*phic_nr(3,ii+1)+(1.0d0-alp(ii))*phic_nr(3,ii))/rtc)*vmax
           tmp_VDR0_r(ii)=tmp_VDR0_r(ii)  &
  &                       +3.0d0*r_inv(ii)*(alp(ii)*phis_nr(3,ii+1)+(1.0d0-alp(ii))*phis_nr(3,ii))*vmax
        end do
!$omp end do
     end if
!$omp end parallel

     VRT0_GVTD_r(1:nnr)=tmp_VRT0_r(1:nnr)
     VDR0_GVTD_r(1:nnr)=tmp_VDR0_r(1:nnr)

  end if

  call stdout( "Finish procedure.", "calc_pseudo_GVTD0", 0 )

end subroutine calc_pseudo_GVTD0

!--------------------------------------------------
! Calculate sine and cosine components of Vt and VR for rot
!--------------------------------------------------

subroutine calc_phi2sc( nrot, vmax, rd, rdh, phis_nr, phic_nr,  &
  &                     VRTns_r, VRTnc_r, VRRns_r, VRRnc_r )
  implicit none
  integer, intent(in) :: nrot
  double precision, intent(in) :: vmax
  double precision, intent(in) :: rd(:)  ! Normalized radius
  double precision, intent(in) :: rdh(size(rd)+1)  ! Normalized radius
  double precision, intent(in) :: phis_nr(nrot,size(rd)+1)
  double precision, intent(in) :: phic_nr(nrot,size(rd)+1)
  double precision, intent(out) :: VRTns_r(nrot,size(rd))
  double precision, intent(out) :: VRTnc_r(nrot,size(rd))
  double precision, intent(out) :: VRRns_r(nrot,size(rd))
  double precision, intent(out) :: VRRnc_r(nrot,size(rd))

  integer :: ii, jj, kk, nnr, cstat
  double precision, dimension(size(rd)) :: dr, dr_inv, r_inv, alp

  call stdout( "Enter procedure.", "calc_phi2sc", 0 )

  nnr=size(rd)

  do ii=1,nnr
     dr(ii)=rdh(ii+1)-rdh(ii)
     dr_inv(ii)=1.0d0/dr(ii)
     alp(ii)=(rd(ii)-rdh(ii))/(rdh(ii+1)-rdh(ii))
  end do
  r_inv(1:nnr)=1.0d0/rd(1:nnr)

  if(nrot>0)then

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,ii)

     do ii=1,nnr
        do kk=1,nrot
        VRTns_r(kk,ii)=-(phis_nr(kk,ii+1)-phis_nr(kk,ii))*dr_inv(ii)*vmax
        VRTnc_r(kk,ii)=-(phic_nr(kk,ii+1)-phic_nr(kk,ii))*dr_inv(ii)*vmax
        VRRns_r(kk,ii)=-dble(kk)*(alp(ii)*phic_nr(kk,ii+1)+(1.0d0-alp(ii))*phic_nr(kk,ii))*r_inv(ii)*vmax
        VRRnc_r(kk,ii)=dble(kk)*(alp(ii)*phis_nr(kk,ii+1)+(1.0d0-alp(ii))*phis_nr(kk,ii))*r_inv(ii)*vmax
        end do
     end do

!$omp end do
!$omp end parallel

  end if

  call stdout( "Finish procedure.", "calc_phi2sc", 0 )

end subroutine calc_phi2sc

end module ToRMHOWe_main
