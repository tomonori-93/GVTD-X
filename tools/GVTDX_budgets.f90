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
  integer :: nr_org            !! the radial grid number of the original data
  integer :: nt_org            !! the azimuthal grid number of the original data
  integer :: nz                !! the vertical grid number of the original data
  integer :: nrot              !! the rotating maximum wavenumber used in the retrieval
  integer :: ndiv              !! the divergent maximum wavenumber used in the retrieval
  integer :: nrdiv             !! radial grid number where the divergence is defined
  integer :: nthres_undef(2)   !! thresholds of the azimuthal sampling number to determine the (1) innermost and (2) outermost radii, respectively
  integer :: smooth_r          !! resampled radial grid number from the original data
  integer :: smooth_t          !! resampled azimuthal grid number from the original data
  integer :: nnz(2)            !! calculating vertical grid levels
  integer :: skip_min_t        !! threshold of the azimuthal sampling number to determine unused radii
  integer :: flag_GVTDX        !! 1: GVTD-X, 2: GVTD, 3: GBVTD
  real :: undefobs             !! undefined value for the original Doppler radar data
  double precision, dimension(nrdivmax) :: rdiv_t  !! radial grids where the divergence is defined
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
  integer :: i, j, k, id, it, m, stat, nr, ntz, nt, nl, irec, tccol
  integer :: nrotmin, ndivmin
  integer :: nr_in, nr_out, ntin_c, ntout_c
  double precision, parameter :: undef=-999.0d0
  real, allocatable, dimension(:) :: ttime
  real, allocatable, dimension(:,:,:) :: rval, rval_org
  double precision, allocatable, dimension(:) :: theta_t, r_t, rh_t
  double precision, allocatable, dimension(:,:) :: dval
  double precision, allocatable, dimension(:,:,:) :: VTtot, VRtot, VRT0, VDR0
  double precision, allocatable, dimension(:,:,:,:) :: VRTn, VRRn, VDTm, VDRm, phin, zetan
  double precision, allocatable, dimension(:,:,:) :: VRT0_GVTD, VDR0_GVTD, Vn_0
  double precision, allocatable, dimension(:,:,:,:) :: VRTns, VRTnc, VRRns, VRRnc
  double precision, allocatable, dimension(:,:) :: VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t
  double precision, allocatable, dimension(:,:,:) :: VRTns_rt_t, VRTnc_rt_t, VRRns_rt_t, VRRnc_rt_t
  double precision, allocatable, dimension(:,:,:) :: VRTn_rt_t, VRRn_rt_t, VDTm_rt_t, VDRm_rt_t
  double precision, allocatable, dimension(:,:,:) :: phin_rt_t, zetan_rt_t

  character(200) :: input_fname, output_fname
!  character(1), dimension(3) :: dimname, unitname
!  character(12), dimension(3) :: lname
  character(200), allocatable, dimension(:,:) :: cval
!  character(50) :: valc, vall, valu
!  character(50), dimension(2) :: vecc, vecl, vecu
  logical :: stdflag
  logical, allocatable, dimension(:,:) :: undef_grid
!  logical :: vec_flag, stdflag
!  type(GT_HISTORY) :: vr_hst, vt_hst, w_hst

!-- read namelist file
  namelist /iocheck /listname, nr_org, nt_org, nz, rmin, dr, tmin, dt, zmin, dz
  namelist /ret_opt /nrot, ndiv, nrdiv, rdiv_t, undefobs, nthres_undef,  &
  &                  smooth_r, smooth_t, nnz, skip_min_t, flag_GVTDX
  namelist /rad_opt /dpoint_x, dpoint_y
  read(5,nml=iocheck)
  read(5,nml=ret_opt)
  read(5,nml=rad_opt)

  if(undefobs==0.0e0)then
     write(*,*) "*** WARNING (main) *** : namelist (undefobs) is not set."
  end if

!-- initialize and allocate

  vds=0.0d0
  vdm=0.0d0

  d2r=pi_dp/180.0d0

  stdflag=.true.

  if(stdflag.eqv..true.)then
     write(*,*) "### MESSAGE (main) ### : "
     write(*,*) "In interpolation routines, stdopt is set as true."
     write(*,*) "This means that error is not output."
  end if

!  dimname=(/'r', 'z', 't'/)
!  lname=(/'Radius', 'Height', 'Time  '/)
!  unitname=(/'m', 'm', 's'/)

  nr=smooth_r
  nt=smooth_t

  ntz=nnz(2)-nnz(1)+1

  allocate(r_t_org(nr_org))
  allocate(r_t(nr))
  allocate(rh_t(nr+1))
  allocate(theta_ref_t_org(nt_org))
  allocate(theta_ref_t(nt))
  allocate(theta_t(nt))
  allocate(thetad_t(nr,nt))
  allocate(lonr(nr,nt))
  allocate(latr(nr,nt))
  allocate(projVs(nr,nt))
  allocate(projVm(nr,nt))
  allocate(dval(nr_org,nt_org))
  allocate(rval_org(nr_org,nt_org,nz))

  allocate(nr_grid_skp(nr))
  allocate(r_skp_t(nr))
  allocate(rh_skp_t(nr+1))
  allocate(thetad_skp_t(nr,nt))
  allocate(Vra_skp_rt_t(nr,nt))

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

  r_t_org=(/((rmin+dr*real(i-1)),i=1,nr_org)/)
  theta_ref_t_org=(/((tmin+dt*real(i-1)),i=1,nt_org)/)
  theta_ref_t_org=theta_ref_t_org*d2r
  do j=1, nr
     r_t(j)=r_t_org(1)+(r_t_org(nr_org)-r_t_org(1))*(dble(j-1)/dble(nr-1))
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
     theta_ref_t(j)=theta_ref_t_org(1)  &
  &               +(theta_ref_t_org(nt_org)-theta_ref_t_org(1))*(dble(j-1)/dble(nt-1))
  end do

  allocate(umd(nz))
  allocate(vmd(nz))
  allocate(VTtot(nr,nt,nnz(1):nnz(2)))
  allocate(VRtot(nr,nt,nnz(1):nnz(2)))
  allocate(VRT0(nr,nt,nnz(1):nnz(2)))
  allocate(VDR0(nr,nt,nnz(1):nnz(2)))
  allocate(VRTn(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VRRn(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VDTm(ndivmin:ndiv,nr,nt,nnz(1):nnz(2)))
  allocate(VDRm(ndivmin:ndiv,nr,nt,nnz(1):nnz(2)))
  allocate(phin(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(zetan(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VRT0_GVTD(nr,nt,nnz(1):nnz(2)))
  allocate(VDR0_GVTD(nr,nt,nnz(1):nnz(2)))
  allocate(VRTns(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VRTnc(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VRRns(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VRRnc(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(Vn_0(nr,nt,nnz(1):nnz(2)))
  allocate(Vra(nr,nt,nnz(1):nnz(2)))
  allocate(Vra_ret(nr,nt,nnz(1):nnz(2)))
  allocate(Vra_rt_t(nr,nt))
  allocate(VTtot_rt_t(nr,nt))
  allocate(VRtot_rt_t(nr,nt))
  allocate(VRT0_rt_t(nr,nt))
  allocate(VDR0_rt_t(nr,nt))
  allocate(VRTn_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRRn_rt_t(nrotmin:nrot,nr,nt))
  allocate(VDTm_rt_t(ndivmin:ndiv,nr,nt))
  allocate(VDRm_rt_t(ndivmin:ndiv,nr,nt))
  allocate(phin_rt_t(nrotmin:nrot,nr,nt))
  allocate(zetan_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRT0_GVTD_rt_t(nr,nt))
  allocate(VDR0_GVTD_rt_t(nr,nt))
  allocate(VRTns_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRTnc_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRRns_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRRnc_rt_t(nrotmin:nrot,nr,nt))
  allocate(Vn_0_rt_t(nr,nt))
  allocate(rval(nr,nt,nz))
  allocate(undef_grid(nr,nt))

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
     VTtot=undef
     VRtot=undef
     VRT0=undef
     VDR0=undef
     VRTn=undef
     VRRn=undef
     VDTm=undef
     VDRm=undef
     phin=undef
     zetan=undef
     VRTns=undef
     VRTnc=undef
     VRRns=undef
     VRRnc=undef

     !-- Read the vortex center position on lon-lat
     y_tc=dble( c2r_convert( trim(adjustl(cval(4,i))) ) )
     fc=2.0d0*omega_dp*dsin(d2r*y_tc)

     !-- Read GVTD-X-retrieved variables
     call read_file( trim(adjustl(input_fname)), nr, nz, iord_VRT0, rval )
     call conv_d2r_3d( rval(1:nr,1:nz), VRT0(1:nr,1:nz) )

     call read_file( trim(adjustl(input_fname)), nr, nz, iord_VDR0, rval )
     call conv_d2r_3d( rval(1:nr,1:nz), VDR0(1:nr,1:nz) )

     if(nrot>0)then
        do k=1,nrot
           call read_file( trim(adjustl(input_fname)), nr, nz, iord_VRTns(k), rval )
           call conv_d2r_3d( rval(1:nr,1:nz), VRTns(1:nr,1:nz,k) )

           call read_file( trim(adjustl(input_fname)), nr, nz, iord_VRTnc(k), rval )
           call conv_d2r_3d( rval(1:nr,1:nz), VRTnc(1:nr,1:nz,k) )

           call read_file( trim(adjustl(input_fname)), nr, nz, iord_VRRns(k), rval )
           call conv_d2r_3d( rval(1:nr,1:nz), VRRns(1:nr,1:nz,k) )

           call read_file( trim(adjustl(input_fname)), nr, nz, iord_VRRnc(k), rval )
           call conv_d2r_3d( rval(1:nr,1:nz), VRRnc(1:nr,1:nz,k) )

           call read_file( trim(adjustl(input_fname)), nr, nz, iord_zetans(k), rval )
           call conv_d2r_3d( rval(1:nr,1:nz), zetans(1:nr,1:nz,k) )

           call read_file( trim(adjustl(input_fname)), nr, nz, iord_zetanc(k), rval )
           call conv_d2r_3d( rval(1:nr,1:nz), zetanc(1:nr,1:nz,k) )
        end do
     end if

     if(ndiv>0)then
        do k=1,ndiv
           call read_file( trim(adjustl(input_fname)), nr, nz, iord_VDTmc(k), rval )
           call conv_d2r_3d( rval(1:nr,1:nz), VDTmc(1:nr,1:nz,k) )

           call read_file( trim(adjustl(input_fname)), nr, nz, iord_VDRmc(k), rval )
           call conv_d2r_3d( rval(1:nr,1:nz), VDRmc(1:nr,1:nz,k) )
        end do
     end if

     !-- 0. Calculate axisymmetric components of vorticity and divergence
     call calc_rotdiv( nrot, r_t, z, VDR0, VRT0, undef, DIV0, VORT0 )

     !-- 1. Calculate AAM budget (ここから作成)
     call calc_AAM_bud( nrot, fc, r_t, rh_t, z_v, VDR0, VRT0, w0, VORT0,  & ! in
  &                     VRRns, VRRnc, zetans, zetanc, undef, undeflag,  & ! in
  &                     dVRT0dt, HADVAX_AAM, HADVAS_AAM, VADVAX_AAM )  ! out

     !-- 2. Calculate vorticity budget
     call calc_vort_bud( nrot, fc, r_t, rh_t, z_v, VDR0, VRT0, w0,  & ! in
  &                      DIV0, VORT0, VRRns, VRRnc, VRTns, VRTnc,  & ! in
  &                      zetans, zetanc, undef, undeflag,  & ! in
  &                      dVORT0dt, HADVAX_vort, HADVAS_vort,  & !out
  &                      VADVAX_vort, STRAX_vort, TILAX_vort )  ! out

     call calc_vortn_bud( nrot, r_t, z_v, VDR0, VRT0, w0, DIV0, VORT0,  & ! in
  &                       VRRns, VRRnc, VRTns, VRTnc,  & !in
  &                       zetans, zetanc, undef, undeflag,  & ! in
  &                       dzetancdt, HADVAXnc_vort, HADVASnc_vort,  & ! in
  &                       VADVAXnc_vort, STRAXnc_vort, TILAXnc_vort,  &  ! out
  &                       dzetansdt, HADVAXns_vort, HADVASns_vort,  & ! in
  &                       VADVAXns_vort, STRAXns_vort, TILAXns_vort )  ! out

     !-- 5. output to binary file (float)
     irec=1
     call conv_d2r_3d( dVRT0dt(1:nr,1:nz), rval(1:nr,1:nz) )
     call write_file( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='replace' )

     irec=irec+1
     call conv_d2r_3d( HADVAX_AAM(1:nr,1:nz), rval(1:nr,1:nz) )
     call write_file( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='old' )

     irec=irec+1
     call conv_d2r_3d( HADVAS_AAM(1:nr,1:nz), rval(1:nr,1:nz) )
     call write_file( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='old' )

     irec=irec+1
     call conv_d2r_3d( VADVAX_AAM(1:nr,1:nz), rval(1:nr,1:nz) )
     call write_file( trim(adjustl(output_fname)), nr, nz, irec, rval(1:nr,1:nz), mode='old' )


     if(ndiv>0)then
        do k=1,ndiv
           irec=irec+ntz
           call conv_d2r_3d( VDTm(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )

           irec=irec+ntz
           call conv_d2r_3d( VDRm(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
        end do
     end if

!-- optional output variables
     irec=irec+ntz
     call conv_d2r_3d( Vn_0(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )

!!-- NetCDF output
!
!     call HistoryPut( trim(adjustl(valc)), varbar(1:nr,nnz(1):nnz(2)) )

     write(*,*) "Writing the data at "//trim(adjustl(output_fname))

  end do

  close(unit=100)

!  call HistoryClose

contains

subroutine calc_rotdiv( r, z, UD0, VR0, undef, undeflag,  &  ! in
  &                     DIV0, VORT0 )  ! out
!! Calculate axisymmetric components of vorticity and divergence
  double precision, intent(in) :: r(:)  !! Radius at which velocity is defined (m)
  double precision, intent(in) :: z(:)  !! Height at which velocity is defined (m)
  double precision, intent(in) :: UD0(size(r),size(z))   !! Divergent zero wind component (m/s)
  double precision, intent(in) :: VR0(size(r),size(z))   !! Rotating zero wind component (m/s)
  double precision, intent(in) :: undef  !! Undefined value
  logical, intent(inout) :: undeflag(size(r),size(z))  ! Undefined grid flag (true)
  double precision, intent(out) :: DIV0(size(r),size(z))   !! Axisymmetric divergence (1/s)
  double precision, intent(out) :: VORT0(size(r),size(z))   !! Axisymmetric vorticity (1/s)

  !-- internal variables
  integer :: ii, jj, kk, nnr, nnz
  double precision :: r_inv(size(r))
  double precision :: dV0dr(size(r),size(z)), dU0dr(size(r),size(z))

  nnr=size(r)
  nnz=size(z)

  r_inv=0.0d0
  do ii=1,nnr
     if(r(ii)/=0.0d0)then
        r_inv(ii)=1.0d0/r(ii)
     else
        undeflag(ii,1:nnz)=.true.
     end if
  end do

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk)

  !-- calculate dV0/dr, dU0/dr
  do kk=1,nnz
     call grad_1d( r, VR0(1:nnr,kk), dV0dr(1:nnr,kk), undeflag=undeflag(1:nnr,kk) )
     call grad_1d( r, UD0(1:nnr,kk), dU0dr(1:nnr,kk), undeflag=undeflag(1:nnr,kk) )
  end do

!$omp end do
!$omp barrier
!$omp do schedule(dynamic) private(ii,kk)

  do kk=1,nnz
     do ii=1,nnr
        if(undeflag(ii,kk).eqv..false.)then
           VORT0(ii,kk)=dV0dr(ii,kk)+VR0(ii,kk)*r_inv(ii)
           DIV0(ii,kk)=dU0dr(ii,kk)+UD0(ii,kk)*r_inv(ii)
        end if
     end do
  end do

!$omp end do
!$omp end parallel

end subroutine calc_rotdiv

!--------------------------------
!--------------------------------

subroutine calc_AAM_bud( nw, f0, r, rh, z, UD0, VR0, w0, VORT0,  & ! in
  &                      Uns, Unc, zetans, zetanc, undef, undeflag,  & ! in
  &                      dVR0dt, HADVAX, HADVAS, VADVAX )  ! out
!! Perform budget analysis of V0
  integer, intent(in) :: nw  ! wavenumber for rotating component
  double precision, intent(in) :: f0    !! Coriolis parameter (1/s)
  double precision, intent(in) :: r(:)  !! Radius at which velocity is defined (m)
  double precision, intent(in) :: rh(size(r)+1)  !! Staggered radius at which scalar is defined (m)
  double precision, intent(in) :: z(:)  !! Height at which velocity is defined (m)
  double precision, intent(in) :: UD0(size(r),size(z))   !! Divergent zero wind component (m/s)
  double precision, intent(in) :: VR0(size(r),size(z))   !! Rotating zero wind component (m/s)
  double precision, intent(in) :: w0(size(r),size(z))    !! Vertical zero wind component (m/s)
  double precision, intent(in) :: VORT0(size(r),size(z))   !! Axisymmetric vorticity (1/s)
  double precision, intent(in) :: Uns(size(r),size(z),nw)   !! Sine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: Unc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: zetans(size(r),size(z),nw)   !! Sine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: zetanc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: undef  !! Undefined value
  logical, intent(inout) :: undeflag(size(r),size(z))  ! Undefined grid flag (true)
  double precision, intent(out) :: dVR0dt(size(r),size(z))   !! Sum of the budget equation (m/s2)
  double precision, intent(out) :: HADVAX(size(r),size(z))   !! Horizontal advection for axisymmetric flows (m/s2)
  double precision, intent(out) :: HADVAS(size(r),size(z),nw)  !! Horizontal advection for asymmetric flows (m/s2)
  double precision, intent(out) :: VADVAX(size(r),size(z))   !! Vertical advection for axisymmetric flows (m/s2)

  !-- internal variables
  integer :: ii, jj, kk, nnr, nnz
  double precision, dimension(size(r),size(z)) :: tmp_dVR0dt, tmp_HADVAX, tmp_VADVAX
  double precision, dimension(size(r),size(z)) :: dv0dz
  double precision :: tmp_HADVAS(size(r),size(z),nw)

  nnr=size(r)
  nnz=size(z)

  tmp_dVR0dt=undef
  tmp_HADVAX=undef
  tmp_HADVAS=undef
  tmp_VADVAX=undef

!$omp parallel default(shared)
!$omp do schedule(runtime) private(ii)

  !-- calculate dV0/dz
  do ii=1,nnr
     call grad_1d( z, VR0(ii,1:nnz), dv0dz(ii,1:nnz), undeflag=undeflag(ii,1:nnz) )
  end do

!$omp end do
!$omp barrier

  do jj=1,nw

!$omp do schedule(runtime) private(ii,kk)
     do kk=1,nnz
        do ii=1,nnr
           if(undeflag(ii,kk).eqv..false.)then
              tmp_HADVAS(ii,kk,jj)=-0.5d0*(Unc(ii,kk,jj)*zetanc(ii,kk,jj)  &
  &                                       +Uns(ii,kk,jj)*zetans(ii,kk,jj))
           end if
        end do
     end do
!$omp end do
!$omp barrier

  end do

!$omp do schedule(runtime) private(ii,kk)

  do kk=1,nnz
     do ii=1,nnr
        if(undeflag(ii,kk).eqv..false.)then
           tmp_HADVAX(ii,kk)=-UD0(ii,kk)*(VORT0(ii,kk)+f0)
           tmp_VADVAX(ii,kk)=-w0(ii,kk)*dv0dz(ii,kk)
        end if
     end do
  end do

!$omp end do
!$omp end parallel

!-- Return each argument

  HADVAX=tmp_HADVAX
  HADVAS=tmp_HADVAS
  VADVAX=tmp_VADVAX

  tmp_dVR0dt=tmp_HADVAX

  call add_2dd( tmp_dVR0dt, tmp_VADVAX, undef=undef )
  do jj=1,nw
     call add_2dd( tmp_dVR0dt, tmp_HADVAS(1:nnr,1:nnz,jj), undef=undef )
  end do

  dVR0dt=tmp_dVR0dt

end subroutine calc_AAM_bud

!--------------------------------
!--------------------------------

subroutine calc_vort_bud( nw, f0, r, rh, z, UD0, VR0, w0, DIV0, VORT0,  & ! in
  &                       Uns, Unc, Vns, Vnc, zetans, zetanc, undef, undeflag,  & ! in
  &                       dVORT0dt, HADVAX, HADVAS, VADVAX, STRAX, TILAX )  ! out
!! Perform budget analysis of VORT0
  integer, intent(in) :: nw  ! wavenumber for rotating component
  double precision, intent(in) :: f0    !! Coriolis parameter (1/s)
  double precision, intent(in) :: r(:)  !! Radius at which velocity is defined (m)
  double precision, intent(in) :: rh(size(r)+1)  !! Staggered radius at which scalar is defined (m)
  double precision, intent(in) :: z(:)  !! Height at which velocity is defined (m)
  double precision, intent(in) :: UD0(size(r),size(z))   !! Divergent zero wind component (m/s)
  double precision, intent(in) :: VR0(size(r),size(z))   !! Rotating zero wind component (m/s)
  double precision, intent(in) :: w0(size(r),size(z))    !! Vertical zero wind component (m/s)
  double precision, intent(in) :: DIV0(size(r),size(z))   !! Axisymmetric divergence (1/s)
  double precision, intent(in) :: VORT0(size(r),size(z))   !! Axisymmetric vorticity (1/s)
  double precision, intent(in) :: Uns(size(r),size(z),nw)   !! Sine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: Unc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: Vns(size(r),size(z),nw)   !! Sine coefficient for asymmetric tangential winds (m/s)
  double precision, intent(in) :: Vnc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric tangential winds (m/s)
  double precision, intent(in) :: zetans(size(r),size(z),nw)   !! Sine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: zetanc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: undef  !! Undefined value
  logical, intent(inout) :: undeflag(size(r),size(z))  ! Undefined grid flag (true)
  double precision, intent(out) :: dVORT0dt(size(r),size(z))   !! Sum of the budget equation (1/s2)
  double precision, intent(out) :: HADVAX(size(r),size(z))   !! Horizontal advection for axisymmetric flows (1/s2)
  double precision, intent(out) :: HADVAS(size(r),size(z),nw)  !! Horizontal advection for asymmetric flows (1/s2)
  double precision, intent(out) :: VADVAX(size(r),size(z))   !! Vertical advection for axisymmetric flows (1/s2)
  double precision, intent(out) :: STRAX(size(r),size(z))   !! Stretching by axisymmetric flows (1/s2)
  double precision, intent(out) :: TILAX(size(r),size(z))   !! Tilting by axisymmetric flows (1/s2)

  !-- internal variables
  integer :: ii, jj, kk, nnr, nnz
  double precision :: r_inv(size(r))
  double precision, dimension(size(r),size(z)) :: tmp_dVORT0dt, tmp_HADVAX, tmp_VADVAX, tmp_STRAX, tmp_TILAX
  double precision, dimension(size(r),size(z)) :: dv0dz, dw0dr, dzeta0dz, dzeta0dr
  double precision, dimension(size(r),size(z),nw) :: dzetancdr, dzetansdr, tmp_HADVAS

  nnr=size(r)
  nnz=size(z)

  tmp_dVORT0dt=undef
  tmp_HADVAX=undef
  tmp_HADVAS=undef
  tmp_VADVAX=undef
  tmp_STRAX=undef
  tmp_TILAX=undef

  r_inv=0.0d0
  do ii=1,nnr
     if(r(ii)/=0.0d0)then
        r_inv(ii)=1.0d0/r(ii)
     else
        undeflag(ii,1:nnz)=.true.
     end if
  end do

  !-- calculate dVORT0/dr, dVORT0/dz
  call grad_2d( r, z, VORT0, dzeta0dr, dzeta0dz, undeflag=undeflag )

!$omp parallel default(shared)
!$omp do schedule(runtime) private(ii)

  !-- calculate dV0/dz
  do ii=1,nnr
     call grad_1d( z, VR0(ii,1:nnz), dv0dz(ii,1:nnz), undeflag=undeflag(ii,1:nnz) )
  end do

!$omp end do
!$omp barrier
!$omp do schedule(runtime) private(kk)

  !-- calculate dw0/dr
  do kk=1,nnz
     call grad_1d( r, w0(1:nnr,kk), dw0dr(1:nnr,kk), undeflag=undeflag(1:nnr,kk) )
  end do

!$omp end do
!$omp barrier
!$omp do schedule(runtime) private(jj,kk)

  !-- calculate dzetan/dr
  do jj=1,nw
     do kk=1,nnz
        call grad_1d( r, zetans(1:nnr,kk,jj), dzetansdr(1:nnr,kk,jj), undeflag=undef(1:nnr,kk) )
        call grad_1d( r, zetanc(1:nnr,kk,jj), dzetancdr(1:nnr,kk,jj), undeflag=undef(1:nnr,kk) )
     end do
  end do

!$omp end do
!$omp barrier

  do jj=1,nw

!$omp do schedule(runtime) private(ii,kk)
     do kk=1,nnz
        do ii=1,nnr
           if(undeflag(ii,kk).eqv..false.)then
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
        if(undeflag(ii,kk).eqv..false.)then
           tmp_HADVAX(ii,kk)=-UD0(ii,kk)*dzeta0dr(ii,kk)
           tmp_VADVAX(ii,kk)=-w0(ii,kk)*dzeta0dz(ii,kk)
           tmp_STRAX(ii,kk)=-(f0+VORT0(ii,kk))*DIV0(ii,kk)
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

  tmp_dVORT0dt=tmp_HADVAX

  call add_2dd( tmp_dVORT0dt, tmp_VADVAX, undef=undef )
  call add_2dd( tmp_dVORT0dt, tmp_STRAX, undef=undef )
  call add_2dd( tmp_dVORT0dt, tmp_TILAX, undef=undef )
  do jj=1,nw
     call add_2dd( tmp_dVORT0dt, tmp_HADVAS(1:nnr,1:nnz,jj), undef=undef )
  end do

  dVORT0dt=tmp_dVORT0dt

end subroutine calc_vort_bud

!--------------------------------
!--------------------------------

subroutine calc_vortn_bud( nw, r, z, UD0, VR0, w0, DIV0, VORT0,  & ! in
  &                        Uns, Unc, Vns, Vnc, zetans, zetanc, undef, undeflag,  & ! in
  &                        dzetancdt, HADVAXnc, HADVASnc, VADVAXnc, STRAXnc, TILAXnc,  &  ! out
  &                        dzetansdt, HADVAXns, HADVASns, VADVAXns, STRAXns, TILAXns )  ! out
!! Perform budget analysis of wavenumber-n vorticity
  integer, intent(in) :: nw  ! wavenumber for rotating component
  double precision, intent(in) :: r(:)  !! Radius at which velocity is defined (m)
  double precision, intent(in) :: z(:)  !! Height at which velocity is defined (m)
  double precision, intent(in) :: UD0(size(r),size(z))   !! Divergent zero wind component (m/s)
  double precision, intent(in) :: VR0(size(r),size(z))   !! Rotating zero wind component (m/s)
  double precision, intent(in) :: w0(size(r),size(z))    !! Vertical zero wind component (m/s)
  double precision, intent(in) :: DIV0(size(r),size(z))   !! Axisymmetric divergence (1/s)
  double precision, intent(in) :: VORT0(size(r),size(z))   !! Axisymmetric vorticity (1/s)
  double precision, intent(in) :: Uns(size(r),size(z),nw)   !! Sine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: Unc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric radial winds (m/s)
  double precision, intent(in) :: Vns(size(r),size(z),nw)   !! Sine coefficient for asymmetric tangential winds (m/s)
  double precision, intent(in) :: Vnc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric tangential winds (m/s)
  double precision, intent(in) :: zetans(size(r),size(z),nw)   !! Sine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: zetanc(size(r),size(z),nw)   !! Cosine coefficient for asymmetric vorticity (1/s)
  double precision, intent(in) :: undef  !! Undefined value
  logical, intent(inout) :: undeflag(size(r),size(z))  ! Undefined grid flag (true)
  double precision, intent(out) :: dzetansdt(size(r),size(z),nw)   !! Sine amplitude of Sum of the budget equation (1/s2)
  double precision, intent(out) :: HADVAXns(size(r),size(z),nw)   !! Sine amplitude of horizontal advection for axisymmetric flows (1/s2)
  double precision, intent(out) :: HADVASns(size(r),size(z),nw)  !! Sine amplitude of horizontal advection for asymmetric flows (1/s2)
  double precision, intent(out) :: VADVAXns(size(r),size(z),nw)   !! Sine amplitude of vertical advection for axisymmetric flows (1/s2)
  double precision, intent(out) :: STRAXns(size(r),size(z),nw)   !! Sine amplitude of stretching by axisymmetric flows (1/s2)
  double precision, intent(out) :: TILAXns(size(r),size(z),nw)   !! Sine amplitude of tilting by axisymmetric flows (1/s2)
  double precision, intent(out) :: dzetancdt(size(r),size(z),nw)   !! Cosine amplitude of Sum of the budget equation (1/s2)
  double precision, intent(out) :: HADVAXnc(size(r),size(z),nw)   !! Cosine amplitude of horizontal advection for axisymmetric flows (1/s2)
  double precision, intent(out) :: HADVASnc(size(r),size(z),nw)  !! Cosine amplitude of horizontal advection for asymmetric flows (1/s2)
  double precision, intent(out) :: VADVAXnc(size(r),size(z),nw)   !! Cosine amplitude of vertical advection for axisymmetric flows (1/s2)
  double precision, intent(out) :: STRAXnc(size(r),size(z),nw)   !! Cosine amplitude of stretching by axisymmetric flows (1/s2)
  double precision, intent(out) :: TILAXnc(size(r),size(z),nw)   !! Cosine amplitude of tilting by axisymmetric flows (1/s2)

  !-- internal variables
  integer :: ii, jj, kk, nnr, nnz
  double precision :: r_inv(size(r))
  double precision, dimension(size(r),size(z),nw) :: tmp_dzetansdt, tmp_HADVAXns, tmp_HADVASns, tmp_VADVAXns, tmp_STRAXns, tmp_TILAXns
  double precision, dimension(size(r),size(z),nw) :: tmp_dzetancdt, tmp_HADVAXnc, tmp_HADVASnc, tmp_VADVAXnc, tmp_STRAXnc, tmp_TILAXnc
  double precision, dimension(size(r),size(z)) :: dv0dz, dw0dr, dzeta0dz, dzeta0dr
  double precision, dimension(size(r),size(z),nw) :: dzetansdr, dzetancdr, dzetansdz, dzetancdz
  double precision, dimension(size(r),size(z),nw) :: dVnsdz, dVncdz

  nnr=size(r)
  nnz=size(z)

  tmp_dzetansdt=undef
  tmp_HADVAXns=undef
  tmp_HADVASns=undef
  tmp_VADVAXns=undef
  tmp_STRAXns=undef
  tmp_TILAXns=undef
  tmp_dzetancdt=undef
  tmp_HADVAXnc=undef
  tmp_HADVASnc=undef
  tmp_VADVAXnc=undef
  tmp_STRAXnc=undef
  tmp_TILAXnc=undef

  r_inv=0.0d0
  do ii=1,nnr
     if(r(ii)/=0.0d0)then
        r_inv(ii)=1.0d0/r(ii)
     else
        undeflag(ii,1:nnz)=.true.
     end if
  end do

  call grad_2d( r, z, VORT0, dzeta0dr, dzeta0dz, undeflag=undeflag )

!$omp parallel default(shared)
!$omp do schedule(runtime) private(jj)

  !-- calculate dzetan{s,c}/dr, dzetan{s,c}/dz
  do jj=1,nw
     call grad_2d( r, z, zetans(1:nnr,1:nnz,jj),  &
  &                dzetansdr(1:nnr,1:nnz,jj),  &
  &                dzetansdz(1:nnr,1:nnz,jj),  &
  &                undeflag=undeflag )

     call grad_2d( r, z, zetanc(1:nnr,1:nnz,omppe),  &
  &                dzetancdr(1:nnr,1:nnz,jj),  &
  &                dzetancdz(1:nnr,1:nnz,jj),  &
  &                undeflag=undeflag )
  end do

!$omp end do
!$omp barrier

  !-- calculate dVn{s,c}/dz
  do jj=1,nw

!$omp do schedule(runtime) private(ii)
     do ii=1,nnr
        call grad_1d( z, Vns(ii,1:nnz,jj), dVnsdz(ii,1:nnz,jj),  &
  &                   undeflag=undeflag(ii,1:nnz) )

        call grad_1d( z, Vnc(ii,1:nnz,jj), dVncdz(ii,1:nnz,jj),  &
  &                   undeflag=undeflag(ii,1:nnz) )
     end do
!$omp end do
!$omp barrier

  end do

!$omp do schedule(runtime) private(kk)

  !-- calculate dw0/dr
  do kk=1,nnz
     call grad_1d( r, w0(1:nnr,kk), dw0dr(1:nnr,kk), undeflag=undeflag(1:nnr,kk) )
  end do

!$omp end do
!$omp barrier
!$omp do schedule(runtime) private(jj,kk)

  !-- calculate dzetan/dr
  do jj=1,nw
     do kk=1,nnz
        call grad_1d( r, zetans(1:nnr,kk,jj), dzetansdr(1:nnr,kk,jj),  &
  &                   undeflag=undeflag(1:nnr,kk) )
        call grad_1d( r, zetanc(1:nnr,kk,jj), dzetancdr(1:nnr,kk,jj),  &
  &                   undeflag=undeflag(1:nnr,kk) )
     end do
  end do

!$omp end do
!$omp barrier

  do jj=1,nw

!$omp do schedule(runtime) private(ii,kk)
     do kk=1,nnz
        do ii=1,nnr
           if(undeflag(ii,kk).eqv..false.)then
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

  do jj=1,nw

!$omp do schedule(runtime) private(ii,kk)
     do kk=1,nnz
        do ii=1,nnr
           if(undeflag(ii,kk,jj).eqv..false.)then
              tmp_HADVAXns(ii,kk,jj)=-UD0(ii,kk)*dzetansdr(ii,kk,jj)  &
  &                                  +VR0(ii,kk)*zetanc(ii,kk,jj)*dble(jj)*r_inv(ii)
              tmp_VADVAXns(ii,kk,jj)=-w0(ii,kk)*dzeta0dz(ii,kk,jj)
              tmp_STRAXns(ii,kk,jj)=-(f0+VORT0(ii,kk))*DIV0(ii,kk,jj)
              tmp_TILAXns(ii,kk,jj)=-dw0dr(ii,kk)*dv0dz(ii,kk,jj)

           end if
        end do
     end do
!$omp end do
!$omp barrier

  end do

!$omp end parallel

!-- Return each argument

  HADVAX=tmp_HADVAX
  HADVAS=tmp_HADVAS
  VADVAX=tmp_VADVAX
  STRAX=tmp_STRAX
  TILAX=tmp_TILAX

  tmp_dVORT0dt=tmp_HADVAX

  call add_2dd( tmp_dVORT0dt, tmp_VADVAX, undef=undef )
  call add_2dd( tmp_dVORT0dt, tmp_STRAX, undef=undef )
  call add_2dd( tmp_dVORT0dt, tmp_TILAX, undef=undef )
  do jj=1,nw
     call add_2dd( tmp_dVORT0dt, tmp_HADVAS(jj,1:nnr,1:nnz), undef=undef )
  end do

  dVORT0dt=tmp_dVORT0dt

end subroutine calc_vort_bud

!--------------------------------
!--------------------------------

end program
