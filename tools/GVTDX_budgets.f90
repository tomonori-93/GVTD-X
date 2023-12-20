program GVTDX_budgets

!! Perform budget analysis with GVTD-X-retrieved variables <br>
!! Buget equations: Absolute angular momentum (AAM), vorticity, and enstrophy <br>
!! Input data: GVTD-X-retrieved variables <br>
!! Data format: 4-byte direct access binary <br>
!! Output data: 4-byte binary (nr x nz) at one time for one file <br>
!! Data grid coordinate: polar with the origin of the vortex center <br>
!! <br>
!! [USAGE]: ./GVTDX_budgets < GVTDX_budgets.nml

  use GVTDX_sub
  use tools_sub

  implicit none

  !-- namelist variables
  integer :: nr                !! the radial grid number of the retrieved data
  integer :: nt                !! the azimuthal grid number of the retrieved data
  integer :: nz                !! the vertical grid number of the retrieved data
  integer :: nrot              !! the rotating maximum wavenumber used in the retrieval
  integer :: ndiv              !! the divergent maximum wavenumber used in the retrieval
  real :: undefobs             !! undefined value for the original Doppler radar data
  double precision :: dpoint_x !! longitudinal position of the radar (degree)
  double precision :: dpoint_y !! latitudinal position of the radar (degree)
  double precision :: rmin     !! the innermost radius where Doppler velocity is defined in the original data (m)
  double precision :: dr       !! the radial grid spacing where Doppler velocity is defined (m)
  double precision :: tmin     !! the start of the azimuthal angle where Doppler velocity is defined in the original data (deg)
  double precision :: dt       !! the azimuthal angle interval where Doppler velocity is defined (deg)
  double precision :: zmin     !! the bottom height where Doppler velocity is defined in the original data (m)
  double precision :: dz       !! the vertical grid spacing where Doppler velocity is defined (m)
  character(1000) :: listname  !! ASCII file name listing file name and other information

  !-- internal variables
  integer :: i, j, k, id, it, m, stat, nl, irec, tccol
  integer :: nrotmin, ndivmin
  double precision, parameter :: undef=-999.0d0
  real, allocatable, dimension(:) :: ttime
  real, allocatable, dimension(:,:) :: rval
  double precision :: fc, d2r, y_tc
  double precision, allocatable, dimension(:) :: theta_t, r_t, rh_t, z_v
  double precision, allocatable, dimension(:,:) :: dval
  double precision, allocatable, dimension(:,:) :: VRT0_r, VDR0_r, DIV0_r, ZETA0_r
  double precision, allocatable, dimension(:,:) :: dVRT0_rdt, dZETA0_rdt
  double precision, allocatable, dimension(:,:) :: w0_r
  double precision, allocatable, dimension(:,:,:,:) :: VRTn, VRRn, VDTm, VDRm
  double precision, allocatable, dimension(:,:,:) :: Vn_0
  double precision, allocatable, dimension(:,:,:) :: VRTns_r, VRTnc_r, VRRns_r, VRRnc_r, zetans_r, zetanc_r
  double precision, allocatable, dimension(:,:) :: VRT0_rt_t, VDR0_rt_t
  double precision, allocatable, dimension(:,:,:) :: VDTmc_r, VDRmc_r
  double precision, allocatable, dimension(:,:,:) :: VRTns_rt_t, VRTnc_rt_t, VRRns_rt_t, VRRnc_rt_t
  double precision, allocatable, dimension(:,:,:) :: VRTn_rt_t, VRRn_rt_t, VDTm_rt_t, VDRm_rt_t
  double precision, allocatable, dimension(:,:,:) :: zetan_rt_t
  !-- Budget terms
  double precision, allocatable, dimension(:,:) :: HADVAX_AAM, VADVAX_AAM, HADVAS_AAM
  double precision, allocatable, dimension(:,:,:) :: HADVASn_AAM
  double precision, allocatable, dimension(:,:) :: HADVAX_vort, VADVAX_vort, HADVAS_vort, STRAX_vort, TILAX_vort

  character(200) :: input_fname, output_fname
!  character(1), dimension(3) :: dimname, unitname
!  character(12), dimension(3) :: lname
  character(200), allocatable, dimension(:,:) :: cval
!  character(50) :: valc, vall, valu
!  character(50), dimension(2) :: vecc, vecl, vecu
  logical :: stdflag
!  logical :: vec_flag, stdflag
!  type(GT_HISTORY) :: vr_hst, vt_hst, w_hst
  type file_ord  ! reading order type for variables
     integer :: VRTs  ! (sine) rotational component of tangential wind
     integer :: VRTc  ! (cosine) rotational component of tangential wind
     integer :: VRRs  ! (sine) rotational component of radial wind
     integer :: VRRc  ! (cosine) rotational component of radial wind
     integer :: zetas ! (sine) vorticity
     integer :: zetac ! (cosine) vorticity
  end type file_ord
  type(file_ord) :: iordn(10)  ! reading order for asymmetric component
  type(file_ord) :: iord0      ! reading order for axisymmetric component

!-- read namelist file
  namelist /iocheck /listname, nr, nt, nz, rmin, dr, tmin, dt, zmin, dz
  namelist /ret_opt /nrot, ndiv, undefobs
  read(5,nml=iocheck)
  read(5,nml=ret_opt)

  if(undefobs==0.0e0)then
     write(*,*) "*** WARNING (main) *** : namelist (undefobs) is not set."
  end if

!-- initialize and allocate

  d2r=pi_dp/180.0d0

  stdflag=.true.

  if(stdflag.eqv..true.)then
     write(*,*) "### MESSAGE (main) ### : "
     write(*,*) "In interpolation routines, stdopt is set as true."
     write(*,*) "This means that error is not output."
  end if

  !-- setting reading order (these orders are the same as the output in GVTDX_Dradar.)
  iord0%VRTc=1  ! wavenumber-0 rotational component of tangential wind
  iord0%zetac=0  ! wavenumber-0 vorticity
  iord0%VRRc=2  ! wavenumber-0 divergent component of radial wind
  iord0%zetas=0  ! wavenumber-0 divergence
  do k=1,nrot
     iordn(k)%zetas=4+6*(k-1)+1
     iordn(k)%zetac=4+6*(k-1)+2
     iordn(k)%VRTs=4+6*(k-1)+3
     iordn(k)%VRTc=4+6*(k-1)+4
     iordn(k)%VRRs=4+6*(k-1)+5
     iordn(k)%VRRc=4+6*(k-1)+6
  end do

!  dimname=(/'r', 'z', 't'/)
!  lname=(/'Radius', 'Height', 'Time  '/)
!  unitname=(/'m', 'm', 's'/)

  allocate(r_t(nr))
  allocate(rh_t(nr+1))
  allocate(theta_t(nt))
  allocate(z_v(nz))

  if(nrot==0)then
     nrotmin=0
  else
     nrotmin=1
  end if
  if(ndiv==0)then
     ndivmin=0
  else
     ndivmin=1
  end if

!-- Define r-theta coordinate

  do j=1, nr
     r_t(j)=rmin+dr*dble(j-1)
  end do

  do j=1, nr-1
     rh_t(j+1)=0.5d0*(r_t(j+1)+r_t(j))
  end do
  rh_t(1)=r_t(1)-0.5*(r_t(2)-r_t(1))
  if(rh_t(1)<0.0d0)then
     rh_t(1)=0.0d0
  end if
  rh_t(nr+1)=r_t(nr)+0.5*(r_t(nr)-r_t(nr-1))

  do j=1, nt
     theta_t(j)=tmin+dt*dble(j-1)
  end do

  do j=1, nz
     z_v(j)=zmin+dz*dble(j-1)
  end do

  allocate(VRT0_r(nr,nz))
  allocate(VDR0_r(nr,nz))
  allocate(DIV0_r(nr,nz))
  allocate(ZETA0_r(nr,nz))
  allocate(w0_r(nr,nz))
  allocate(dVRT0_rdt(nr,nz))
  allocate(dZETA0_rdt(nr,nz))
  allocate(VRTn(nrotmin:nrot,nr,nt,nz))
  allocate(VRRn(nrotmin:nrot,nr,nt,nz))
  allocate(VDTm(ndivmin:ndiv,nr,nt,nz))
  allocate(VDRm(ndivmin:ndiv,nr,nt,nz))
  allocate(zetans_r(nr,nz,nrotmin:nrot))
  allocate(zetanc_r(nr,nz,nrotmin:nrot))
  allocate(VRTns_r(nr,nz,nrotmin:nrot))
  allocate(VRTnc_r(nr,nz,nrotmin:nrot))
  allocate(VRRns_r(nr,nz,nrotmin:nrot))
  allocate(VRRnc_r(nr,nz,nrotmin:nrot))
  allocate(VDTmc_r(nr,nz,ndivmin:ndiv))
  allocate(VDRmc_r(nr,nz,ndivmin:ndiv))
!  allocate(Vn_0(nr,nt,nz))
  allocate(VRT0_rt_t(nr,nt))
  allocate(VDR0_rt_t(nr,nt))
  allocate(VRTn_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRRn_rt_t(nrotmin:nrot,nr,nt))
  allocate(VDTm_rt_t(ndivmin:ndiv,nr,nt))
  allocate(VDRm_rt_t(ndivmin:ndiv,nr,nt))
  allocate(zetan_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRTns_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRTnc_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRRns_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRRnc_rt_t(nrotmin:nrot,nr,nt))
  allocate(HADVAX_AAM(nr,nz))
  allocate(VADVAX_AAM(nr,nz))
  allocate(HADVAS_AAM(nr,nz))
  allocate(HADVASn_AAM(nr,nz,nrotmin:nrot))
  allocate(HADVAX_vort(nr,nz))
  allocate(VADVAX_vort(nr,nz))
  allocate(HADVAS_vort(nr,nz))
!  allocate(HADVASn_vort(nr,nz,nrotmin:nrot))
  allocate(STRAX_vort(nr,nz))
  allocate(TILAX_vort(nr,nz))

!  allocate(Vn_0_rt_t(nr,nt))
  allocate(rval(nr,nz))

!-- Read vortex center, storm motion, and mean wind

  nl=line_number_counter( trim(adjustl(listname)) )-2
  tccol=6+2*nz  ! fname, time, TC-lon, TC-lat, Uxs, Vxs, U1m, V1m,... Unzm, Vnzm

  allocate(cval(tccol,nl))
  allocate(ttime(nl))

  call read_file_text( trim(adjustl(listname)), tccol, nl, cval, skip=2 )
  do i=1,nl
     ttime(i)=c2r_convert( trim(adjustl(cval(2,i))) )
  end do

!-- ASCII output for normal component to the direction of the radar to the vortex center of storm-relative mean wind
  open(unit=100,file=trim(adjustl(listname))//'.srVM',status='unknown')
  write(100,'(a32)') "'Time'          'Vm-SR'         "
  write(100,'(a32)') "'s'             'ms-1'          "

!-- 以下から編集していく (上は見ていない)
!-- Loop for time

  do i=1,nl
     input_fname=trim(adjustl(cval(1,i)))
     output_fname=trim(adjustl(input_fname))//'.budget'
     write(*,*) trim(adjustl(input_fname))

     !-- Set temporary variables
     VRT0_r=undef
     VDR0_r=undef
     w0_r=0.0d0
     VRTn=undef
     VRRn=undef
     VDTm=undef
     VDRm=undef
     zetans_r=undef
     zetanc_r=undef
     VRTns_r=undef
     VRTnc_r=undef
     VRRns_r=undef
     VRRnc_r=undef

     !-- Read the vortex center position on lon-lat
     y_tc=dble( c2r_convert( trim(adjustl(cval(4,i))) ) )
     fc=2.0d0*omega_dp*dsin(d2r*y_tc)

     !-- Read GVTD-X-retrieved variables
     call read_file_2d( trim(adjustl(input_fname)), nr, nz, (iord0%VRTc-1)+1, rval )
     call conv_r2d_2d( rval(1:nr,1:nz), VRT0_r(1:nr,1:nz) )

     call read_file_2d( trim(adjustl(input_fname)), nr, nz, (iord0%VRRc-1)+1, rval )
     call conv_r2d_2d( rval(1:nr,1:nz), VDR0_r(1:nr,1:nz) )

     if(nrot>0)then
        do k=1,nrot
           call read_file_2d( trim(adjustl(input_fname)), nr, nz, (iordn(k)%VRTs-1)+1, rval )
           call conv_r2d_2d( rval(1:nr,1:nz), VRTns_r(1:nr,1:nz,k) )

           call read_file_2d( trim(adjustl(input_fname)), nr, nz, (iordn(k)%VRTc-1)+1, rval )
           call conv_r2d_2d( rval(1:nr,1:nz), VRTnc_r(1:nr,1:nz,k) )

           call read_file_2d( trim(adjustl(input_fname)), nr, nz, (iordn(k)%VRRs-1)+1, rval )
           call conv_r2d_2d( rval(1:nr,1:nz), VRRns_r(1:nr,1:nz,k) )

           call read_file_2d( trim(adjustl(input_fname)), nr, nz, (iordn(k)%VRRc-1)+1, rval )
           call conv_r2d_2d( rval(1:nr,1:nz), VRRnc_r(1:nr,1:nz,k) )

           call read_file_2d( trim(adjustl(input_fname)), nr, nz, (iordn(k)%zetas-1)+1, rval )
           call conv_r2d_2d( rval(1:nr,1:nz), zetans_r(1:nr,1:nz,k) )

           call read_file_2d( trim(adjustl(input_fname)), nr, nz, (iordn(k)%zetac-1)+1, rval )
           call conv_r2d_2d( rval(1:nr,1:nz), zetanc_r(1:nr,1:nz,k) )
        end do
     end if

     if(ndiv>0)then
        do k=1,ndiv
           call read_file_2d( trim(adjustl(input_fname)), nr, nz, (iordn(k)%VRTc-1)+1, rval )
           call conv_r2d_2d( rval(1:nr,1:nz), VDTmc_r(1:nr,1:nz,k) )

           call read_file_2d( trim(adjustl(input_fname)), nr, nz, (iordn(k)%VRRc-1)+1, rval )
           call conv_r2d_2d( rval(1:nr,1:nz), VDRmc_r(1:nr,1:nz,k) )
        end do
     end if

     !-- 0. Calculate axisymmetric components of vorticity and divergence
     call calc_rotdiv( nr, nz, r_t, VDR0_r, VRT0_r, undef, DIV0_r, ZETA0_r )

     !-- 1. Calculate AAM budget
     call calc_AAM_bud( nrot, fc, r_t, z_v, VDR0_r, VRT0_r, w0_r, ZETA0_r,  & ! in
  &                     VRRns_r, VRRnc_r, zetans_r, zetanc_r, undef,  & ! in
  &                     dVRT0_rdt, HADVAX_AAM, HADVAS_AAM, HADVASn_AAM, VADVAX_AAM )  ! out

     !-- 2. Calculate vorticity budget
     call calc_vort_bud( nrot, fc, r_t, z_v, VDR0_r, VRT0_r, w0_r,  & ! in
  &                      DIV0_r, ZETA0_r, VRRns_r, VRRnc_r, VRTns_r, VRTnc_r,  & ! in
  &                      zetans_r, zetanc_r, undef,  & ! in
  &                      dZETA0_rdt, HADVAX_vort, HADVAS_vort,  & !out
  &                      VADVAX_vort, STRAX_vort, TILAX_vort )  ! out

!-- 以下は作成しただけで未チェック
!     call calc_vortn_bud( nrot, r_t, z_v, VDR0, VRT0, w0, DIV0, ZETA0,  & ! in
!  &                       VRRns, VRRnc, VRTns, VRTnc,  & !in
!  &                       zetans, zetanc, undef, undeflag,  & ! in
!  &                       dzetancdt, HADVAXnc_vort, HADVASnc_vort,  & ! in
!  &                       VADVAXnc_vort, STRAXnc_vort, TILAXnc_vort,  &  ! out
!  &                       dzetansdt, HADVAXns_vort, HADVASns_vort,  & ! in
!  &                       VADVAXns_vort, STRAXns_vort, TILAXns_vort )  ! out

!     !-- 5. output to binary file (float)
     irec=1
     call conv_d2r_2d( ZETA0_r(1:nr,1:nz), rval(1:nr,1:nz) )
     call write_file_2d( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='replace' )

     irec=irec+1
     call conv_d2r_2d( DIV0_r(1:nr,1:nz), rval(1:nr,1:nz) )
     call write_file_2d( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='old' )

     irec=irec+1
     call conv_d2r_2d( dVRT0_rdt(1:nr,1:nz), rval(1:nr,1:nz) )
     call write_file_2d( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='old' )

     irec=irec+1
     call conv_d2r_2d( HADVAX_AAM(1:nr,1:nz), rval(1:nr,1:nz) )
     call write_file_2d( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='old' )

     irec=irec+1
     call conv_d2r_2d( HADVAS_AAM(1:nr,1:nz), rval(1:nr,1:nz) )
     call write_file_2d( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='old' )

     do k=1,nrot
        irec=irec+1
        call conv_d2r_2d( HADVASn_AAM(1:nr,1:nz,k), rval(1:nr,1:nz) )
        call write_file_2d( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='old' )
     end do

     irec=irec+1
     call conv_d2r_2d( VADVAX_AAM(1:nr,1:nz), rval(1:nr,1:nz) )
     call write_file_2d( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='old' )

!     if(ndiv>0)then
!        do k=1,ndiv
!           irec=irec+nz
!           call conv_d2r_2d( VDTm(k,1:nr,1:nt,nz), rval(1:nr,1:nz) )
!           call write_file_3d( trim(adjustl(output_fname)), nr, nt, nz, irec,  &
!  &                            rval(1:nr,1:nz), mode='old' )

!           irec=irec+nz
!           call conv_d2r_2d( VDRm(k,1:nr,1:nt,nz), rval(1:nr,1:nt,nz) )
!           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
!  &                            rval(1:nr,1:nz), mode='old' )
!        end do
!     end if

!!-- optional output variables
!     irec=irec+ntz
!     call conv_d2r_2d( Vn_0(1:nr,1:nt,nz), rval(1:nr,1:nt,nz) )
!     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
!  &                      rval(1:nr,1:nt,nz), mode='old' )

!!-- NetCDF output
!
!     call HistoryPut( trim(adjustl(valc)), varbar(1:nr,nz) )

     write(*,*) "Writing the data at "//trim(adjustl(output_fname))

  end do

  close(unit=100)

!  call HistoryClose

contains

subroutine calc_rotdiv( nr, nz, r, UD0, VR0, undef,  &  ! in
  &                     DIV0, ZETA0 )  ! out
!! Calculate axisymmetric components of vorticity and divergence
  integer, intent(in) :: nr  !! radial grid number
  integer, intent(in) :: nz  !! vertical grid number
  double precision, intent(in) :: r(nr)  !! Radius at which velocity is defined (m)
  double precision, intent(in) :: UD0(nr,nz)   !! Divergent zero wind component (m/s)
  double precision, intent(in) :: VR0(nr,nz)   !! Rotating zero wind component (m/s)
  double precision, intent(in) :: undef  !! Undefined value
  double precision, intent(out) :: DIV0(nr,nz)   !! Axisymmetric divergence (1/s)
  double precision, intent(out) :: ZETA0(nr,nz)   !! Axisymmetric vorticity (1/s)

  !-- internal variables
  integer :: ii, jj, kk
  double precision :: r_inv(nr)
  double precision :: dV0dr(nr,nz), dU0dr(nr,nz)

  r_inv=undef
  ZETA0=undef
  DIV0=undef
  do ii=1,nr
     if(r(ii)/=0.0d0)then
        r_inv(ii)=1.0d0/r(ii)
     end if
  end do

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk)

  !-- calculate dV0/dr, dU0/dr
  do kk=1,nz
     call grad_1d( r, VR0(1:nr,kk), dV0dr(1:nr,kk), undef )
     call grad_1d( r, UD0(1:nr,kk), dU0dr(1:nr,kk), undef )
  end do

!$omp end do
!$omp barrier
!$omp do schedule(dynamic) private(ii,kk)

  do kk=1,nz
     do ii=1,nr
        if(dV0dr(ii,kk)/=undef.and.dU0dr(ii,kk)/=undef.and.r_inv(ii)/=undef)then
           ZETA0(ii,kk)=dV0dr(ii,kk)+VR0(ii,kk)*r_inv(ii)
           DIV0(ii,kk)=dU0dr(ii,kk)+UD0(ii,kk)*r_inv(ii)
        end if
     end do
  end do

!$omp end do
!$omp end parallel

end subroutine calc_rotdiv

!--------------------------------
!--------------------------------

subroutine calc_AAM_bud( nw, f0, r, z, UD0, VR0, w0, ZETA0,  & ! in
  &                      Uns, Unc, zetans, zetanc, undef,  & ! in
  &                      dVR0dt, HADVAX, HADVAS, HADVASn, VADVAX )  ! out
!! Perform budget analysis of V0
  integer, intent(in) :: nw  ! wavenumber for rotating component
  double precision, intent(in) :: f0    !! Coriolis parameter (1/s)
  double precision, intent(in) :: r(:)  !! Radius at which velocity is defined (m)
  double precision, intent(in) :: z(:)  !! Height at which velocity is defined (m)
  double precision, intent(in) :: UD0(size(r),size(z))   !! Divergent zero wind component (m/s)
  double precision, intent(in) :: VR0(size(r),size(z))   !! Rotating zero wind component (m/s)
  double precision, intent(in) :: w0(size(r),size(z))    !! Vertical zero wind component (m/s)
  double precision, intent(in) :: ZETA0(size(r),size(z))   !! Axisymmetric vorticity (1/s)
  double precision, intent(in) :: Uns(size(r),size(z),nw)   !! Sine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: Unc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: zetans(size(r),size(z),nw)   !! Sine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: zetanc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: undef  !! Undefined value
  double precision, intent(out) :: dVR0dt(size(r),size(z))   !! Sum of the budget equation (m/s2)
  double precision, intent(out) :: HADVAX(size(r),size(z))   !! Horizontal advection for axisymmetric flows (m/s2)
  double precision, intent(out) :: HADVAS(size(r),size(z))  !! Total horizontal advection for asymmetric flows (m/s2)
  double precision, intent(out) :: HADVASn(size(r),size(z),nw)  !! Horizontal advection for asymmetric flows (m/s2)
  double precision, intent(out) :: VADVAX(size(r),size(z))   !! Vertical advection for axisymmetric flows (m/s2)

  !-- internal variables
  integer :: ii, jj, kk, nnr, nnz
  double precision, dimension(size(r),size(z)) :: tmp_dVR0dt, tmp_HADVAX, tmp_HADVAS, tmp_VADVAX
  double precision, dimension(size(r),size(z)) :: dv0dz
  double precision :: tmp_HADVASn(size(r),size(z),nw)

  nnr=size(r)
  nnz=size(z)

  tmp_dVR0dt=undef
  tmp_HADVAX=undef
  tmp_HADVAS=undef
  tmp_HADVASn=undef
  tmp_VADVAX=undef

!$omp parallel default(shared)
!$omp do schedule(runtime) private(ii)

  !-- calculate dV0/dz
  do ii=1,nnr
     call grad_1d( z, VR0(ii,1:nnz), dv0dz(ii,1:nnz), undef )
  end do

!$omp end do
!$omp barrier


!$omp do schedule(runtime) private(ii,kk,jj)

  do jj=1,nw
     do kk=1,nnz
        do ii=1,nnr
           if(Unc(ii,kk,jj)/=undef.and.Uns(ii,kk,jj)/=undef.and.  &
  &           zetanc(ii,kk,jj)/=undef.and.zetans(ii,kk,jj)/=undef)then
              tmp_HADVASn(ii,kk,jj)=-0.5d0*(Unc(ii,kk,jj)*zetanc(ii,kk,jj)  &
  &                                        +Uns(ii,kk,jj)*zetans(ii,kk,jj))
           end if
        end do
     end do
  end do

!$omp end do
!$omp barrier
!$omp do schedule(runtime) private(ii,kk)

  do kk=1,nnz
     do ii=1,nnr
        if(UD0(ii,kk)/=undef.and.ZETA0(ii,kk)/=undef.and.  &
  &        w0(ii,kk)/=undef.and.dv0dz(ii,kk)/=undef)then
           tmp_HADVAX(ii,kk)=-UD0(ii,kk)*(ZETA0(ii,kk)+f0)
           tmp_VADVAX(ii,kk)=-w0(ii,kk)*dv0dz(ii,kk)
        end if
     end do
  end do

!$omp end do
!$omp end parallel

!-- Return each argument

  tmp_HADVAS=tmp_HADVASn(1:nnr,1:nnz,1)
  if(nw>1)then
     do jj=2,nw
        call add_2d( tmp_HADVAS, tmp_HADVASn(1:nnr,1:nnz,jj), undef=undef )
     end do
  end if

  HADVAX=tmp_HADVAX
  HADVAS=tmp_HADVAS
  HADVASn=tmp_HADVASn
  VADVAX=tmp_VADVAX

  tmp_dVR0dt=tmp_HADVAX

  call add_2d( tmp_dVR0dt, tmp_HADVAS, undef=undef )
  call add_2d( tmp_dVR0dt, tmp_VADVAX, undef=undef )

  dVR0dt=tmp_dVR0dt

end subroutine calc_AAM_bud

!--------------------------------
!--------------------------------

subroutine calc_vort_bud( nw, f0, r, z, UD0, VR0, w0, DIV0, ZETA0,  & ! in
  &                       Uns, Unc, Vns, Vnc, zetans, zetanc, undef,  & ! in
  &                       dZETA0dt, HADVAX, HADVAS, VADVAX, STRAX, TILAX )  ! out
!! Perform budget analysis of ZETA0
  integer, intent(in) :: nw  ! wavenumber for rotating component
  double precision, intent(in) :: f0    !! Coriolis parameter (1/s)
  double precision, intent(in) :: r(:)  !! Radius at which velocity is defined (m)
  double precision, intent(in) :: z(:)  !! Height at which velocity is defined (m)
  double precision, intent(in) :: UD0(size(r),size(z))   !! Divergent zero wind component (m/s)
  double precision, intent(in) :: VR0(size(r),size(z))   !! Rotating zero wind component (m/s)
  double precision, intent(in) :: w0(size(r),size(z))    !! Vertical zero wind component (m/s)
  double precision, intent(in) :: DIV0(size(r),size(z))   !! Axisymmetric divergence (1/s)
  double precision, intent(in) :: ZETA0(size(r),size(z))   !! Axisymmetric vorticity (1/s)
  double precision, intent(in) :: Uns(size(r),size(z),nw)   !! Sine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: Unc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: Vns(size(r),size(z),nw)   !! Sine coefficient for asymmetric tangential winds (m/s)
  double precision, intent(in) :: Vnc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric tangential winds (m/s)
  double precision, intent(in) :: zetans(size(r),size(z),nw)   !! Sine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: zetanc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: undef  !! Undefined value
  double precision, intent(out) :: dZETA0dt(size(r),size(z))   !! Sum of the budget equation (1/s2)
  double precision, intent(out) :: HADVAX(size(r),size(z))   !! Horizontal advection for axisymmetric flows (1/s2)
  double precision, intent(out) :: HADVAS(size(r),size(z),nw)  !! Horizontal advection for asymmetric flows (1/s2)
  double precision, intent(out) :: VADVAX(size(r),size(z))   !! Vertical advection for axisymmetric flows (1/s2)
  double precision, intent(out) :: STRAX(size(r),size(z))   !! Stretching by axisymmetric flows (1/s2)
  double precision, intent(out) :: TILAX(size(r),size(z))   !! Tilting by axisymmetric flows (1/s2)

  !-- internal variables
  integer :: ii, jj, kk, nnr, nnz
  double precision :: r_inv(size(r))
  double precision, dimension(size(r),size(z)) :: tmp_dZETA0dt, tmp_HADVAX, tmp_VADVAX, tmp_STRAX, tmp_TILAX
  double precision, dimension(size(r),size(z)) :: dv0dz, dw0dr, dzeta0dz, dzeta0dr
  double precision, dimension(size(r),size(z),nw) :: dzetancdr, dzetansdr, tmp_HADVAS

  nnr=size(r)
  nnz=size(z)

  tmp_dZETA0dt=undef
  tmp_HADVAX=undef
  tmp_HADVAS=undef
  tmp_VADVAX=undef
  tmp_STRAX=undef
  tmp_TILAX=undef

  r_inv=undef
  do ii=1,nnr
     if(r(ii)/=0.0d0)then
        r_inv(ii)=1.0d0/r(ii)
     end if
  end do

  !-- calculate dZETA0/dr, dZETA0/dz
  call grad_2d( r, z, ZETA0, dzeta0dr, dzeta0dz, undef )

!$omp parallel default(shared)
!$omp do schedule(runtime) private(ii)

  !-- calculate dV0/dz
  do ii=1,nnr
     call grad_1d( z, VR0(ii,1:nnz), dv0dz(ii,1:nnz), undef )
  end do

!$omp end do
!$omp barrier
!$omp do schedule(runtime) private(kk)

  !-- calculate dw0/dr
  do kk=1,nnz
     call grad_1d( r, w0(1:nnr,kk), dw0dr(1:nnr,kk), undef )
  end do

!$omp end do
!$omp barrier
!$omp do schedule(runtime) private(jj,kk)

  !-- calculate dzetan/dr
  do jj=1,nw
     do kk=1,nnz
        call grad_1d( r, zetans(1:nnr,kk,jj), dzetansdr(1:nnr,kk,jj), undef )
        call grad_1d( r, zetanc(1:nnr,kk,jj), dzetancdr(1:nnr,kk,jj), undef )
     end do
  end do

!$omp end do
!$omp barrier

  do jj=1,nw

!$omp do schedule(runtime) private(ii,kk)
     do kk=1,nnz
        do ii=1,nnr
           if(Unc(ii,kk,jj)/=undef.and.Uns(ii,kk,jj)/=undef.and.  &
  &           Vnc(ii,kk,jj)/=undef.and.Vns(ii,kk,jj)/=undef.and.  &
  &           dzetancdr(ii,kk,jj)/=undef.and.dzetansdr(ii,kk,jj)/=undef.and.  &
  &           zetanc(ii,kk,jj)/=undef.and.zetans(ii,kk,jj)/=undef)then
              tmp_HADVAS(ii,kk,jj)=-0.5d0*(Unc(ii,kk,jj)*dzetancdr(ii,kk,jj)  &
  &                                       +Uns(ii,kk,jj)*dzetansdr(ii,kk,jj)  &
  &                                       +(Vnc(ii,kk,jj)*zetans(ii,kk,jj)  &
  &                                        +Vns(ii,kk,jj)*zetanc(ii,kk,jj))  &
  &                                        *dble(jj)*r_inv(ii))
           end if
        end do
     end do
!$omp end do
!$omp barrier

  end do

!$omp do schedule(runtime) private(ii,kk)

  do kk=1,nnz
     do ii=1,nnr
        if(UD0(ii,kk)/=undef.and.w0(ii,kk)/=undef.and.  &
  &        ZETA0(ii,kk)/=undef.and.DIV0(ii,kk)/=undef.and.  &
  &        dzeta0dr(ii,kk)/=undef.and.dzeta0dz(ii,kk)/=undef.and.  &
  &        dv0dz(ii,kk)/=undef.and.dw0dr(ii,kk)/=undef)then
           tmp_HADVAX(ii,kk)=-UD0(ii,kk)*dzeta0dr(ii,kk)
           tmp_VADVAX(ii,kk)=-w0(ii,kk)*dzeta0dz(ii,kk)
           tmp_STRAX(ii,kk)=-(f0+ZETA0(ii,kk))*DIV0(ii,kk)
           tmp_TILAX(ii,kk)=-dw0dr(ii,kk)*dv0dz(ii,kk)
        end if
     end do
  end do

!$omp end do
!$omp end parallel

!-- Return each argument

  HADVAX=tmp_HADVAX
  HADVAS=tmp_HADVAS
  VADVAX=tmp_VADVAX
  STRAX=tmp_STRAX
  TILAX=tmp_TILAX

  tmp_dZETA0dt=tmp_HADVAX

  call add_2d( tmp_dZETA0dt, tmp_VADVAX, undef=undef )
  call add_2d( tmp_dZETA0dt, tmp_STRAX, undef=undef )
  call add_2d( tmp_dZETA0dt, tmp_TILAX, undef=undef )
  do jj=1,nw
     call add_2d( tmp_dZETA0dt, tmp_HADVAS(1:nnr,1:nnz,jj), undef=undef )
  end do

  dZETA0dt=tmp_dZETA0dt

end subroutine calc_vort_bud

!--------------------------------
!--------------------------------

subroutine calc_vortn_bud( nw, r, z, UD0, VR0, w0, DIV0, ZETA0,  & ! in
  &                        Uns, Unc, Vns, Vnc, zetans, zetanc, undef,  & ! in
  &                        dzetandt, HADVAXn, HADVASn, VADVAXn, STRAXn, TILAXn )  ! out
!! Perform budget analysis of wavenumber-n vorticity
  integer, intent(in) :: nw  ! wavenumber for rotating component
  double precision, intent(in) :: r(:)  !! Radius at which velocity is defined (m)
  double precision, intent(in) :: z(:)  !! Height at which velocity is defined (m)
  double precision, intent(in) :: UD0(size(r),size(z))   !! Divergent zero wind component (m/s)
  double precision, intent(in) :: VR0(size(r),size(z))   !! Rotating zero wind component (m/s)
  double precision, intent(in) :: w0(size(r),size(z))    !! Vertical zero wind component (m/s)
  double precision, intent(in) :: DIV0(size(r),size(z))   !! Axisymmetric divergence (1/s)
  double precision, intent(in) :: ZETA0(size(r),size(z))   !! Axisymmetric vorticity (1/s)
  double precision, intent(in) :: Uns(size(r),size(z),nw)   !! Sine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: Unc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: Vns(size(r),size(z),nw)   !! Sine coefficient for asymmetric tangential winds (m/s)
  double precision, intent(in) :: Vnc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric tangential winds (m/s)
  double precision, intent(in) :: zetans(size(r),size(z),nw)   !! Sine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: zetanc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: undef  !! Undefined value
  double precision, intent(out) :: dzetandt(size(r),size(z),nw)  !! Amplitude of Sum of the budget equation (1/s2)
  double precision, intent(out) :: HADVAXn(size(r),size(z),nw)   !! Amplitude of horizontal advection for axisymmetric flows (1/s2)
  double precision, intent(out) :: HADVASn(size(r),size(z),nw)   !! Amplitude of horizontal advection for asymmetric flows (1/s2)
  double precision, intent(out) :: VADVAXn(size(r),size(z),nw)   !! Amplitude of vertical advection for axisymmetric flows (1/s2)
  double precision, intent(out) :: STRAXn(size(r),size(z),nw)    !! Amplitude of stretching by axisymmetric flows (1/s2)
  double precision, intent(out) :: TILAXn(size(r),size(z),nw)    !! Amplitude of tilting by axisymmetric flows (1/s2)

  !-- internal variables
  integer :: ii, jj, kk, nnr, nnz
  double precision :: r_inv(size(r))
  double precision, dimension(size(r),size(z),nw) :: tmp_dzetandt, tmp_HADVAXn, tmp_HADVASn, tmp_VADVAXn, tmp_STRAXn, tmp_TILAXn
  double precision, dimension(size(r),size(z)) :: dv0dz, dw0dr, dzeta0dz, dzeta0dr
  double precision, dimension(size(r),size(z),nw) :: dzetansdr, dzetancdr, dzetansdz, dzetancdz
  double precision, dimension(size(r),size(z),nw) :: dVnsdz, dVncdz

  nnr=size(r)
  nnz=size(z)

  tmp_dzetandt=undef
  tmp_HADVAXn=undef
  tmp_HADVASn=undef
  tmp_VADVAXn=undef
  tmp_STRAXn=undef
  tmp_TILAXn=undef

  r_inv=undef
  do ii=1,nnr
     if(r(ii)/=0.0d0)then
        r_inv(ii)=1.0d0/r(ii)
     end if
  end do

  call grad_2d( r, z, ZETA0, dzeta0dr, dzeta0dz, undef )

!$omp parallel default(shared)
!$omp do schedule(runtime) private(jj)

  !-- calculate dzetan{s,c}/dr, dzetan{s,c}/dz
  do jj=1,nw
     call grad_2d( r, z, zetans(1:nnr,1:nnz,jj),  &
  &                dzetansdr(1:nnr,1:nnz,jj),  &
  &                dzetansdz(1:nnr,1:nnz,jj),  &
  &                undef )

     call grad_2d( r, z, zetanc(1:nnr,1:nnz,jj),  &
  &                dzetancdr(1:nnr,1:nnz,jj),  &
  &                dzetancdz(1:nnr,1:nnz,jj),  &
  &                undef )
  end do

!$omp end do
!$omp barrier

  !-- calculate dVn{s,c}/dz
  do jj=1,nw

!$omp do schedule(runtime) private(ii)
     do ii=1,nnr
        call grad_1d( z, Vns(ii,1:nnz,jj), dVnsdz(ii,1:nnz,jj), undef )

        call grad_1d( z, Vnc(ii,1:nnz,jj), dVncdz(ii,1:nnz,jj), undef )
     end do
!$omp end do
!$omp barrier

  end do

!$omp do schedule(runtime) private(kk)

  !-- calculate dw0/dr
  do kk=1,nnz
     call grad_1d( r, w0(1:nnr,kk), dw0dr(1:nnr,kk), undef )
  end do

!$omp end do
!$omp barrier
!$omp do schedule(runtime) private(jj,kk)

  !-- calculate dzetan/dr
  do jj=1,nw
     do kk=1,nnz
        call grad_1d( r, zetans(1:nnr,kk,jj), dzetansdr(1:nnr,kk,jj),  &
  &                   undef )
        call grad_1d( r, zetanc(1:nnr,kk,jj), dzetancdr(1:nnr,kk,jj),  &
  &                   undef )
     end do
  end do

!$omp end do
!$omp barrier

  do jj=1,nw

!$omp do schedule(runtime) private(ii,kk)
     do kk=1,nnz
        do ii=1,nnr
           if(Unc(ii,kk,jj)/=undef.and.Uns(ii,kk,jj)/=undef.and.  &
  &           Vnc(ii,kk,jj)/=undef.and.Vns(ii,kk,jj)/=undef.and.  &
  &           zetanc(ii,kk,jj)/=undef.and.zetans(ii,kk,jj)/=undef.and.  &
  &           dzetancdr(ii,kk,jj)/=undef.and.dzetansdr(ii,kk,jj)/=undef.and.  &
  &           r_inv(ii)/=undef)then
              tmp_HADVASn(ii,kk,jj)=-0.5d0*(Unc(ii,kk,jj)*dzetancdr(ii,kk,jj)  &
  &                                        +Uns(ii,kk,jj)*dzetansdr(ii,kk,jj)  &
  &                                        +(Vnc(ii,kk,jj)*zetans(ii,kk,jj)  &
  &                                         +Vns(ii,kk,jj)*zetanc(ii,kk,jj))  &
  &                                         *dble(jj)*r_inv(ii))
           end if
        end do
     end do
!$omp end do
!$omp barrier

  end do

  do jj=1,nw

!$omp do schedule(runtime) private(ii,kk)
     do kk=1,nnz
        do ii=1,nnr
           if(UD0(ii,kk)/=undef.and.VR0(ii,kk)/=undef.and.  &
  &           w0(ii,kk)/=undef.and.DIV0(ii,kk)/=undef.and.ZETA0(ii,kk)/=undef.and.  &
  &           dw0dr(ii,kk)/=undef.and.dv0dz(ii,kk)/=undef.and.  &
  &           zetanc(ii,kk,jj)/=undef.and.zetans(ii,kk,jj)/=undef.and.  &
  &           dzetancdr(ii,kk,jj)/=undef.and.dzetansdr(ii,kk,jj)/=undef)then
              tmp_HADVAXn(ii,kk,jj)=-UD0(ii,kk)*dzetansdr(ii,kk,jj)  &
  &                                 +VR0(ii,kk)*zetanc(ii,kk,jj)*dble(jj)*r_inv(ii)
              tmp_VADVAXn(ii,kk,jj)=-w0(ii,kk)*dzeta0dz(ii,kk)
              tmp_STRAXn(ii,kk,jj)=0.0d0 !-(zetanc(ii,kk))*DIV0(ii,kk,jj)
              tmp_TILAXn(ii,kk,jj)=0.0d0 !-dw0dr(ii,kk)*dv0dz(ii,kk,jj)

           end if
        end do
     end do
!$omp end do
!$omp barrier

  end do

!$omp end parallel

!-- Return each argument

  HADVAXn=tmp_HADVAXn
  HADVASn=tmp_HADVASn
  VADVAXn=tmp_VADVAXn
  STRAXn=tmp_STRAXn
  TILAXn=tmp_TILAXn

  tmp_dzetandt=tmp_HADVAXn

  call add_3d( tmp_dzetandt, tmp_VADVAXn, undef=undef )
  call add_3d( tmp_dzetandt, tmp_STRAXn, undef=undef )
  call add_3d( tmp_dzetandt, tmp_HADVASn, undef=undef )

  dzetandt=tmp_dzetandt

end subroutine calc_vortn_bud

!--------------------------------
!--------------------------------

end program
