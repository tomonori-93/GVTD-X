program GVTDX_Dradar

!! Perform GVTD-X (you can choose the retrieval method by flag_GVTDX) <br>
!! Input data: Doppler velocity based CAPPI <br>
!! Data format: 4-byte binary (nr_org x nt_org x nz) at one time for one file <br>
!! Output data: 4-byte binary (nr x nt x nzo) at one time for one file <br>
!! Data grid coordinate: polar with the origin of the vortex center <br>
!! You can resample the input data from the original grid in GVTD-X retrieval. <br>
!! <br>
!! [USAGE]: ./GVTDX_Dradar < GVTDX_Dradar.nml

  use GVTDX_sub
  use GVTDX_main, only : Retrieve_velocity_GVTDX
  use GVTD_main, only : Retrieve_velocity_GVTD
  use GBVTD_main, only : Retrieve_velocity_GBVTD
  use tools_sub

  implicit none

  !-- parameters
  integer, parameter :: nrdivmax=1000  !! the maximum number of radial grids for the divergent
  integer, parameter :: out_fnum(2)=(/100, 101/)
                        !! unit numbers for output files <br>
                        !! 1: .srVM file (storm-relative mean wind)
                        !! 2: GVTDX.ctl file (GrADS control file)

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
  logical :: out2d_flag        !! Flag for output of 2d (r-z) data
  logical :: flag_datagap=.false. !! Flag for use of optimal wavenumber from data gap (Lee et al. 2000)

  !-- internal variables
  integer :: i, j, k, id, it, m, stat, nr, ntz, nt, nl, irec, tccol, nval
  integer :: nrotmin, ndivmin
  integer :: nr_in, nr_out, ntin_c, ntout_c
  double precision :: usp, vsp, thetad_tc, thetaM, thetaS
  double precision :: x_tc, y_tc, d2r, r2d, r_tmp, RdTc
  double precision :: vdm, vds, dvm
  double precision, parameter :: undef=-999.0d0
  real, allocatable, dimension(:) :: ttime
  real, allocatable, dimension(:,:) :: rval2d
  real, allocatable, dimension(:,:,:) :: rval, rval_org
  double precision, allocatable, dimension(:) :: umd, vmd
  double precision, allocatable, dimension(:) :: theta_ref_t
  double precision, allocatable, dimension(:) :: theta_ref_t_org, r_t_org
  double precision, allocatable, dimension(:) :: theta_t, r_t, rh_t
  double precision, allocatable, dimension(:,:) :: dval
  double precision, allocatable, dimension(:,:) :: thetad_t
  double precision, allocatable, dimension(:,:) :: lonr, latr, lond, latd
  double precision, allocatable, dimension(:,:) :: lonr_sph, latr_sph, lond_sph, latd_sph
  double precision, allocatable, dimension(:,:) :: projVs, projVm
  double precision, allocatable, dimension(:,:) :: projVRs_rt_t, projVTs_rt_t
  double precision, allocatable, dimension(:,:) :: projVRm_rt_t, projVTm_rt_t
  double precision, allocatable, dimension(:,:,:) :: VTtot, VRtot, VRT0, VDR0
  double precision, allocatable, dimension(:,:,:) :: zeta0, zetatot
  double precision, allocatable, dimension(:,:,:,:) :: VRTn, VRRn, VDTm, VDRm, phin, zetan
  double precision, allocatable, dimension(:,:,:) :: VTtot_Er, VRtot_Er
  double precision, allocatable, dimension(:,:,:) :: Uxtot_Er, Vytot_Er
  double precision, allocatable, dimension(:,:,:) :: Wstot_Er
  double precision, allocatable, dimension(:,:,:) :: Vra, Vra_ret
  double precision, allocatable, dimension(:,:,:) :: Vra_Er, Vra_Er_ret
  double precision, allocatable, dimension(:,:,:) :: VRT0_GVTD, VDR0_GVTD, Vn_0
  double precision, allocatable, dimension(:,:) :: VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t
  double precision, allocatable, dimension(:,:) :: VRT0_GVTD_rt_t, VDR0_GVTD_rt_t, Vn_0_rt_t
  double precision, allocatable, dimension(:,:,:) :: VRTns_2d, VRTnc_2d, VRRns_2d, VRRnc_2d
  double precision, allocatable, dimension(:,:) :: Vra_rt_t
  double precision, allocatable, dimension(:,:,:) :: VRTn_rt_t, VRRn_rt_t, VDTm_rt_t, VDRm_rt_t
  double precision, allocatable, dimension(:,:,:) :: phin_rt_t, zetan_rt_t
!-- Variables for amplitude of each component
!  double precision, allocatable, dimension(:,:) :: VTtot_2d, VRtot_2d, VRT0_2d, VDR0_2d
!  double precision, allocatable, dimension(:,:) :: VRT0_GVTD_2d, VDR0_GVTD_2d, Vn_0_2d
  double precision, allocatable, dimension(:,:,:) :: VRTn_2d, VRRn_2d, VDTm_2d, VDRm_2d
!  double precision, allocatable, dimension(:,:,:) :: phin_2d
  double precision, allocatable, dimension(:,:,:) :: zetans_2d, zetanc_2d

  integer :: nr_out_skp
  integer, allocatable, dimension(:) :: nr_grid_skp
  double precision, allocatable, dimension(:) :: r_skp_t, rh_skp_t
  double precision, allocatable, dimension(:,:) :: thetad_skp_t, Vra_skp_rt_t

  character(200) :: input_fname, output_fname, output2d_fname
!  character(1), dimension(3) :: dimname, unitname
!  character(12), dimension(3) :: lname
  character(200), allocatable, dimension(:,:) :: cval
  character(10000) :: tmpchar
!  character(50) :: valc, vall, valu
!  character(50), dimension(2) :: vecc, vecl, vecu
  logical :: stdflag
  logical, allocatable, dimension(:,:) :: undef_grid, undef_grid_2d
!  logical :: vec_flag, stdflag
!  type(GT_HISTORY) :: vr_hst, vt_hst, w_hst

!-- read namelist file
  namelist /iocheck /listname, out2d_flag, nr_org, nt_org, nz,  &
  &                  rmin, dr, tmin, dt, zmin, dz
  namelist /ret_opt /nrot, ndiv, nrdiv, rdiv_t, undefobs, nthres_undef,  &
  &                  smooth_r, smooth_t, nnz, skip_min_t, flag_GVTDX,  &
  &                  flag_datagap
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
  r2d=180.0d0/pi_dp

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
  allocate(lond(nr,nt))
  allocate(latd(nr,nt))
  allocate(lond_sph(nr,nt))
  allocate(latd_sph(nr,nt))
  allocate(lonr_sph(nr,nt))
  allocate(latr_sph(nr,nt))
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
  allocate(VTtot_Er(nr,nt,nnz(1):nnz(2)))
  allocate(VRtot_Er(nr,nt,nnz(1):nnz(2)))
  allocate(Uxtot_Er(nr,nt,nnz(1):nnz(2)))
  allocate(Vytot_Er(nr,nt,nnz(1):nnz(2)))
  allocate(Wstot_Er(nr,nt,nnz(1):nnz(2)))
  allocate(VRT0(nr,nt,nnz(1):nnz(2)))
  allocate(VDR0(nr,nt,nnz(1):nnz(2)))
  allocate(zetatot(nr,nt,nnz(1):nnz(2)))
  allocate(zeta0(nr,nt,nnz(1):nnz(2)))
  allocate(VRTn(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VRRn(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VDTm(ndivmin:ndiv,nr,nt,nnz(1):nnz(2)))
  allocate(VDRm(ndivmin:ndiv,nr,nt,nnz(1):nnz(2)))
  allocate(phin(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(zetan(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VRT0_GVTD(nr,nt,nnz(1):nnz(2)))
  allocate(VDR0_GVTD(nr,nt,nnz(1):nnz(2)))
  allocate(Vn_0(nr,nt,nnz(1):nnz(2)))
  allocate(Vra(nr,nt,nnz(1):nnz(2)))
  allocate(Vra_ret(nr,nt,nnz(1):nnz(2)))
  allocate(Vra_rt_t(nr,nt))
  allocate(Vra_Er(nr,nt,nnz(1):nnz(2)))
  allocate(Vra_Er_ret(nr,nt,nnz(1):nnz(2)))
  allocate(projVRs_rt_t(nr,nt))
  allocate(projVTs_rt_t(nr,nt))
  allocate(projVRm_rt_t(nr,nt))
  allocate(projVTm_rt_t(nr,nt))
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
  allocate(VRTns_2d(nrotmin:nrot,nr,nz))
  allocate(VRTnc_2d(nrotmin:nrot,nr,nz))
  allocate(VRRns_2d(nrotmin:nrot,nr,nz))
  allocate(VRRnc_2d(nrotmin:nrot,nr,nz))
  allocate(Vn_0_rt_t(nr,nt))
  allocate(rval(nr,nt,nz))
  allocate(rval2d(nr,nz))
  allocate(undef_grid(nr,nt))

  allocate(undef_grid_2d(nr,nz))
!  allocate(VTtot_2d(nr,nz))
!  allocate(VRtot_2d(nr,nz))
!  allocate(VRT0_2d(nr,nz))
!  allocate(VDR0_2d(nr,nz))
  allocate(VRTn_2d(nrotmin:nrot,nr,nz))
  allocate(VRRn_2d(nrotmin:nrot,nr,nz))
  allocate(VDTm_2d(ndivmin:ndiv,nr,nz))
  allocate(VDRm_2d(ndivmin:ndiv,nr,nz))
!  allocate(VRT0_GVTD_2d(nr,nz))
!  allocate(VDR0_GVTD_2d(nr,nz))
!  allocate(Vn_0_2d(nr,nz))
!  allocate(phin_2d(nrotmin:nrot,nr,nz))
  allocate(zetans_2d(nrotmin:nrot,nr,nz))
  allocate(zetanc_2d(nrotmin:nrot,nr,nz))

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
  open(unit=out_fnum(1),file=trim(adjustl(listname))//'.srVM',status='unknown')
  write(out_fnum(1),'(a32)') "'Time'          'Vm-SR'         "
  write(out_fnum(1),'(a32)') "'s'             'ms-1'          "

!-- Output GrADS control file for *.GVTDX files
  open(unit=out_fnum(2),file='GVTDX.ctl',status='unknown')
  call write_file_text_add( out_fnum(2), "title GVTDX output file (please edit filenames and time)" )
  call write_file_text_add( out_fnum(2), "undef "//trim(adjustl(r2c_convert(real(undef)))) )
  call write_file_text_add( out_fnum(2), "options big_endian template" )
  call write_file_text_add( out_fnum(2), "xdef "//trim(adjustl(i2c_convert(nr)))  &
  &                         //" LINEAR "//trim(adjustl(r2c_convert(real(rmin))))//" "  &
  &                         //trim(adjustl(r2c_convert(real(dr)*real(nr_org)/real(smooth_r)))) )
  call write_file_text_add( out_fnum(2), "ydef "//trim(adjustl(i2c_convert(nt)))  &
  &                         //" LINEAR "//trim(adjustl(r2c_convert(real(tmin))))//" "  &
  &                         //trim(adjustl(r2c_convert(real(dt)*real(nt_org)/real(smooth_t)))) )
  call write_file_text_add( out_fnum(2), "zdef "//trim(adjustl(i2c_convert(ntz)))  &
  &                         //" LINEAR "//trim(adjustl(r2c_convert(real(zmin+dz*real(nnz(1)-1)))))//" "  &
  &                         //trim(adjustl(r2c_convert(real(dz)))) )
  call write_file_text_add( out_fnum(2), "tdef "//trim(adjustl(i2c_convert(nl)))  &
  &                         //" LINEAR 00Z00JAN0000 10mn" )
  call write_file_text_add( out_fnum(2), "* vars XX (replace XX with the line after 'endvars'" )

!-- Loop for time

  do i=1,nl
     input_fname=trim(adjustl(cval(1,i)))
     output_fname=trim(adjustl(input_fname))//'.GVTDX'
     write(*,*) trim(adjustl(input_fname))

     !-- Read Doppler velocity
     call read_file_3d( trim(adjustl(input_fname)), nr_org, nt_org, nz, 1, rval_org )

     !-- Set temporary variables
     VTtot=undef
     VRtot=undef
     VTtot_Er=undef
     VRtot_Er=undef
     Uxtot_Er=undef
     Vytot_Er=undef
     Wstot_Er=undef
     zetatot=undef
     zeta0=undef
     VRT0=undef
     VDR0=undef
     VRTn=undef
     VRRn=undef
     VDTm=undef
     VDRm=undef
     phin=undef
     zetan=undef
     Vra=undef
     Vra_ret=undef
     Vra_Er=undef
     Vra_Er_ret=undef
     VRT0_GVTD=undef
     VDR0_GVTD=undef
     Vn_0=undef
     VRTns_2d=undef
     VRTnc_2d=undef
     VRRns_2d=undef
     VRRnc_2d=undef
     zetans_2d=undef
     zetanc_2d=undef

     !-- Read the vortex center position on lon-lat
     x_tc=dble( c2r_convert( trim(adjustl(cval(3,i))) ) )
     y_tc=dble( c2r_convert( trim(adjustl(cval(4,i))) ) )

     !-- Read the motion vector of the vortex
     usp=dble(c2r_convert( trim(adjustl(cval(5,i))) ))
     vsp=dble(c2r_convert( trim(adjustl(cval(6,i))) ))

     !-- Read the mean wind at each height
     do k=1,nz
        umd(k)=dble(c2r_convert( trim(adjustl(cval(6+2*(k-1)+1,i))) ))
        vmd(k)=dble(c2r_convert( trim(adjustl(cval(6+2*k,i))) ))
     end do

     !-- Define thetad
     !-- 1. calculate thetad from the radar's lat-lon (dpoint_x,dpoint_y)
     !--    (i.e., the east direction is thetad = 0 at the radar's lat-lon)
     !-- 1.1 calculate lat-lon at each Doppler-velocity point from radius-azimuth and vortex center in the original data
     !-- 1.2 calculate thetad from the radar's lat-lon and the Doppler-velocity lat-lon

     do j=1,nt
        do id=1,nr
           call rt2ll( r_t(id), theta_ref_t(j), x_tc*d2r, y_tc*d2r,  &
  &                    lonr(id,j), latr(id,j) )
           call ll2rt( dpoint_x*d2r, dpoint_y*d2r, lonr(id,j), latr(id,j),  &
  &                    r_tmp, thetad_t(id,j) )
           lond(id,j)=lonr(id,j)*r2d
           latd(id,j)=latr(id,j)*r2d
           call sph_rt2ll( r_t(id), theta_ref_t(j), x_tc*d2r, y_tc*d2r,  &
  &                        lonr_sph(id,j), latr_sph(id,j) )
           lond_sph(id,j)=lonr_sph(id,j)*r2d
           latd_sph(id,j)=latr_sph(id,j)*r2d
        end do
     end do

     !-- 2. subtract thetad_tc from each thetad_t.

     call ll2rt( dpoint_x*d2r, dpoint_y*d2r, x_tc*d2r, y_tc*d2r, r_tmp, thetad_tc )
     do j=1,nt
        do id=1,nr
           thetad_t(id,j)=thetad_t(id,j)-thetad_tc
        end do
     end do

     !-- 3. calculate the distance from the radar to the vortex center
     RdTc=ll2radi( dpoint_x*d2r, dpoint_y*d2r, x_tc*d2r, y_tc*d2r )

     !-- 4. subtract thetad_tc from theta_t
     theta_t=theta_ref_t-thetad_tc

     !-- 5.1. project the storm motion to the line-of-sight direction of each radar beam
     call proj_Vs( dpoint_x*d2r, dpoint_y*d2r, lonr, latr, usp, vsp, projVs, undef )
     !-- (opt) project the storm motion to the R-T coordinate
     call proj_rtVs( r_t, theta_ref_t, usp, vsp, projVRs_rt_t, projVTs_rt_t, undef )

     !-- Loop for altitude
     do k=nnz(1),nnz(2)
        !-- 5.2. project the mean wind to the line-of-sight direction of each radar beam
        call proj_Vs( dpoint_x*d2r, dpoint_y*d2r, lonr, latr, umd(k), vmd(k), projVm, undef )
        !-- (opt) project the mean wind to the R-T coordinate
        call proj_rtVs( r_t, theta_ref_t, umd(k), vmd(k), projVRm_rt_t, projVTm_rt_t, undef )
        !-- A. convert real to double
        call conv_r2d_2d( rval_org(1:nr_org,1:nt_org,k), dval(1:nr_org,1:nt_org) )
        call replace_val_2d( dval(1:nr_org,1:nt_org), dble(undefobs), undef )
        undef_grid=.false.
        VTtot_rt_t(1:nr,1:nt)=undef
        VRtot_rt_t(1:nr,1:nt)=undef
        VRT0_rt_t(1:nr,1:nt)=undef
        VDR0_rt_t(1:nr,1:nt)=undef
        VRTn_rt_t(nrotmin:nrot,1:nr,1:nt)=undef
        VRRn_rt_t(nrotmin:nrot,1:nr,1:nt)=undef
        VDTm_rt_t(ndivmin:ndiv,1:nr,1:nt)=undef
        VDRm_rt_t(ndivmin:ndiv,1:nr,1:nt)=undef
        phin_rt_t(nrotmin:nrot,1:nr,1:nt)=undef
        zetan_rt_t(nrotmin:nrot,1:nr,1:nt)=undef
        VRT0_GVTD_rt_t(1:nr,1:nt)=undef
        VDR0_GVTD_rt_t(1:nr,1:nt)=undef
        Vn_0_rt_t(1:nr,1:nt)=undef

        !-- B. do interpolation from nr_org, nt_org to smooth_r, smooth_t = (nr,nt)
        call auto_interpolation_2d( r_t_org, theta_ref_t_org,  &
  &                                 r_t, theta_ref_t, dval, Vra_rt_t,  &
  &                                 undef=undef, stdopt=stdflag )

        !-- C. VD - projVs
        select case(flag_GVTDX)
        case (1)  ! GVTDX
           call subst_2d( Vra_rt_t(1:nr,1:nt), projVs(1:nr,1:nt), undef )
           call subst_2d( projVm(1:nr,1:nt), projVs(1:nr,1:nt), undef )
        case (2)  ! GVTD
           call subst_2d( Vra_rt_t(1:nr,1:nt), projVm(1:nr,1:nt), undef )
        end select

        !-- D. determine the innermost and outermost radii for retrieval using nthres_undef
        !-- D.1. check the innermost radius index
        nr_in=check_data_fulfill( Vra_rt_t(1:nr,1:nt), undef,  &
  &                               nt_count=nthres_undef(1), dir="i2o",  &
  &                               ncount=ntin_c )
        if(nr_in/=0)then  ! if there is no radius with sufficient sampling, the retrieval is not performed.

           !-- check the positive value of r_t(nr_in)
           nr_in=inner_radius_check( nr_in, nr, r_t(nr_in:nr) )
!write(*,*) "nr", nr_in, ntin_c
           !-- D.2. check the outermost radius index
           nr_out=check_data_fulfill( Vra_rt_t(1:nr,1:nt), undef, &
  &                                   nt_count=nthres_undef(2), dir="o2i",  &
  &                                   ncount=ntout_c )
!write(*,*) "nr", nr_out, ntout_c

           !-- (opt): activate the undef flag for each grid with undef
           call check_undef_grid( Vra_rt_t, undef, undef_grid )
!        call replace_undef( Vra_rt_t, undef_grid, undef )
!write(*,*) "Vra check", Vra_rt_t(nr_in:nr_out,1:nt)

           !-- D.3. rearrange radial grids with skipping unused radii
           if(skip_min_t>0)then
              !-- _skp_: the original variables before the rearrangement
              nr_out_skp=nr_out
              r_skp_t(1:nr)=r_t(1:nr)
              rh_skp_t(1:nr+1)=rh_t(1:nr+1)
              thetad_skp_t(1:nr,1:nt)=thetad_t(1:nr,1:nt)
              Vra_skp_rt_t(1:nr,1:nt)=Vra_rt_t(1:nr,1:nt)
              call rearrange_undef_rad( skip_min_t, nr_in, nr_out_skp, nt,  &
  &                                     undef_grid(nr_in:nr_out_skp,1:nt),  &
  &                                     r_skp_t(nr_in:nr_out_skp),  &
  &                                     rh_skp_t(nr_in:nr_out_skp+1),  &
  &                                     thetad_skp_t(nr_in:nr_out_skp,1:nt),  &
  &                                     Vra_skp_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                     nr_out, nr_grid_skp(nr_in:nr_out_skp),  &  ! 調整後
  &                                     r_t(nr_in:nr_out_skp),  &
  &                                     rh_t(nr_in:nr_out_skp+1),  &
  &                                     thetad_t(nr_in:nr_out_skp,1:nt),  &
  &                                     Vra_rt_t(nr_in:nr_out_skp,1:nt) )
             write(*,*) "rearrange (nr_out): ", nr_out_skp, nr_out, nr_grid_skp
           end if

           select case (flag_GVTDX)
           !-- E. Retrieval (for storm relative azimuth)
           case (1)  ! Run GVTDX
              thetaS=datan2( vsp, usp )
              vds=dsqrt(usp**2+vsp**2)*dsin(thetaS-thetad_tc)  ! Vs x sin(thetaS-thetaT)
              thetaM=datan2( vmd(k), umd(k) )
              vdm=dsqrt(umd(k)**2+vmd(k)**2)*dsin(thetaM-thetad_tc)  ! VM x sin(thetaM-thetaS)
              dvm=vdm-vds
              write(out_fnum(1),'(1P2E16.8)') real(i), dvm  ! Output storm-relative mean wind

              if(nr_in<nr_out)then
              call Retrieve_velocity_GVTDX( nrot, ndiv, r_t(nr_in:nr_out), theta_t,  &
  &                                         rh_t(nr_in:nr_out+1),  &
  &                                         thetad_t(nr_in:nr_out,1:nt), rdiv_t(1:nrdiv),  &
  &                                         Vra_rt_t(nr_in:nr_out,1:nt), dvm, RdTc,  &
  &                                         VTtot_rt_t(nr_in:nr_out,1:nt),  &
  &                                         VRtot_rt_t(nr_in:nr_out,1:nt),  &
!  &                                         Vra_rt_t, (/Vsrn,0.0d0/), VTtot_rt_t, VRtot_rt_t,  &
  &                                         VRT0_rt_t(nr_in:nr_out,1:nt),  &
  &                                         VDR0_rt_t(nr_in:nr_out,1:nt),  &
  &                                         VRTn_rt_t(nrotmin:nrot,nr_in:nr_out,1:nt),  &
  &                                         VRRn_rt_t(nrotmin:nrot,nr_in:nr_out,1:nt),  &
  &                                         VDTm_rt_t(ndivmin:ndiv,nr_in:nr_out,1:nt),  &
  &                                         VDRm_rt_t(ndivmin:ndiv,nr_in:nr_out,1:nt),  &
  &                                         undef,  &
  &                                         phin=phin_rt_t(nrotmin:nrot,nr_in:nr_out,1:nt),  &
  &                                         zetan=zetan_rt_t(nrotmin:nrot,nr_in:nr_out,1:nt),  &
  &                                         VRT0_GVTD=VRT0_GVTD_rt_t(nr_in:nr_out,1:nt),  &
  &                                         VDR0_GVTD=VDR0_GVTD_rt_t(nr_in:nr_out,1:nt),  &
  &                                         Vn_0=Vn_0_rt_t(nr_in:nr_out,1:nt),  &
  &                                         VRTns_r=VRTns_2d(nrotmin:nrot,nr_in:nr_out,k),  &
  &                                         VRTnc_r=VRTnc_2d(nrotmin:nrot,nr_in:nr_out,k),  &
  &                                         VRRns_r=VRRns_2d(nrotmin:nrot,nr_in:nr_out,k),  &
  &                                         VRRnc_r=VRRnc_2d(nrotmin:nrot,nr_in:nr_out,k),  &
  &                                         zetans_r=zetans_2d(nrotmin:nrot,nr_in:nr_out,k),  &
  &                                         zetanc_r=zetanc_2d(nrotmin:nrot,nr_in:nr_out,k) )
              end if

           case (2)  ! Run GVTD
              call Retrieve_velocity_GVTD( nrot, r_t(nr_in:nr_out), theta_t,  &
  &                                        thetad_t(nr_in:nr_out,1:nt),  &
  &                                        Vra_rt_t(nr_in:nr_out,1:nt),  &
  &                                        RdTc,  &
  &                                        VTtot_rt_t(nr_in:nr_out,1:nt),  &
  &                                        VRtot_rt_t(nr_in:nr_out,1:nt),  &
  &                                        VRT0_rt_t(nr_in:nr_out,1:nt),  &
  &                                        VDR0_rt_t(nr_in:nr_out,1:nt),  &
  &                                        VRTn_rt_t(nrotmin:nrot,nr_in:nr_out,1:nt),  &
  &                                        VRRn_rt_t(nrotmin:nrot,nr_in:nr_out,1:nt),  &
  &                                        undef, flag_datagap=flag_datagap )
           case (3)  ! Run GBVTD
              thetaM=datan2( vsp, usp )
              call Retrieve_velocity_GBVTD( nrot, r_t(nr_in:nr_out), theta_t,  &
  &                                         thetad_t(nr_in:nr_out,1:nt),  &
  &                                         Vra_rt_t(nr_in:nr_out,1:nt),  &
  &                                         RdTc,  &
  &                                         VTtot_rt_t(nr_in:nr_out,1:nt),  &
  &                                         VRtot_rt_t(nr_in:nr_out,1:nt),  &
  &                                         VRT0_rt_t(nr_in:nr_out,1:nt),  &
  &                                         VDR0_rt_t(nr_in:nr_out,1:nt),  &
  &                                         VRTn_rt_t(nrotmin:nrot,nr_in:nr_out,1:nt),  &
  &                                         VRRn_rt_t(nrotmin:nrot,nr_in:nr_out,1:nt),  &
  &                                         undef, flag_datagap=flag_datagap )
           end select

           !-- F. recover retrieved data on the rearranged radii to the original radii
           if(skip_min_t>0)then
              !-- _skp_: the original variables before the rearrangement
              !-- _rt_t, intent(inout): replace the index in the rearranged with the original index
              !-- From the above reasons, the element numbers are given from nr_in to nr_out_skp (not nr_out)
              call recover_undef_rad( nrotmin, nrot, ndivmin, ndiv,  &
  &                                   nr_in, nr_out, nr_out_skp, nt,  &
  &                                   undef, undef_grid(nr_in:nr_out_skp,1:nt),  &
  &                                   nr_grid_skp(nr_in:nr_out_skp),  &
  &                                   VTtot_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   VRtot_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   VRT0_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   VDR0_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   VRTn_rt_t(nrotmin:nrot,nr_in:nr_out_skp,1:nt),  &
  &                                   VRRn_rt_t(nrotmin:nrot,nr_in:nr_out_skp,1:nt),  &
  &                                   VDTm_rt_t(ndivmin:ndiv,nr_in:nr_out_skp,1:nt),  &
  &                                   VDRm_rt_t(ndivmin:ndiv,nr_in:nr_out_skp,1:nt),  &
  &                                   VRT0_GVTD_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   VDR0_GVTD_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   VRTns_2d(nrotmin:nrot,nr_in:nr_out_skp,k),  &
  &                                   VRTnc_2d(nrotmin:nrot,nr_in:nr_out_skp,k),  &
  &                                   VRRns_2d(nrotmin:nrot,nr_in:nr_out_skp,k),  &
  &                                   VRRnc_2d(nrotmin:nrot,nr_in:nr_out_skp,k),  &
  &                                   Vn_0_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   phin_rt_t(nrotmin:nrot,nr_in:nr_out_skp,1:nt),  &
  &                                   zetan_rt_t(nrotmin:nrot,nr_in:nr_out_skp,1:nt),  &
  &                                   zetans_2d(nrotmin:nrot,nr_in:nr_out_skp,k),  &
  &                                   zetanc_2d(nrotmin:nrot,nr_in:nr_out_skp,k) )
              nr_out=nr_out_skp
              r_t(nr_in:nr_out_skp)=r_skp_t(nr_in:nr_out_skp)
              rh_t(nr_in:nr_out_skp+1)=rh_skp_t(nr_in:nr_out_skp+1)
              thetad_t(nr_in:nr_out_skp,1:nt)=thetad_skp_t(nr_in:nr_out_skp,1:nt)
              Vra_rt_t(nr_in:nr_out_skp,1:nt)=Vra_skp_rt_t(nr_in:nr_out_skp,1:nt)
           end if

        end if

        !-- F. set the retrieval results to output variables
        VTtot(1:nr,1:nt,k)=VTtot_rt_t(1:nr,1:nt)
        VRtot(1:nr,1:nt,k)=VRtot_rt_t(1:nr,1:nt)
        VRT0(1:nr,1:nt,k)=VRT0_rt_t(1:nr,1:nt)
        VDR0(1:nr,1:nt,k)=VDR0_rt_t(1:nr,1:nt)
        VRT0_GVTD(1:nr,1:nt,k)=VRT0_GVTD_rt_t(1:nr,1:nt)
        VDR0_GVTD(1:nr,1:nt,k)=VDR0_GVTD_rt_t(1:nr,1:nt)
        Vn_0(1:nr,1:nt,k)=Vn_0_rt_t(1:nr,1:nt)
        VRTn(nrotmin:nrot,1:nr,1:nt,k)=VRTn_rt_t(nrotmin:nrot,1:nr,1:nt)
        VRRn(nrotmin:nrot,1:nr,1:nt,k)=VRRn_rt_t(nrotmin:nrot,1:nr,1:nt)
        VDTm(ndivmin:ndiv,1:nr,1:nt,k)=VDTm_rt_t(ndivmin:ndiv,1:nr,1:nt)
        VDRm(ndivmin:ndiv,1:nr,1:nt,k)=VDRm_rt_t(ndivmin:ndiv,1:nr,1:nt)
        phin(nrotmin:nrot,1:nr,1:nt,k)=phin_rt_t(nrotmin:nrot,1:nr,1:nt)
        zetan(nrotmin:nrot,1:nr,1:nt,k)=zetan_rt_t(nrotmin:nrot,1:nr,1:nt)
        Vra(1:nr,1:nt,k)=Vra_rt_t(1:nr,1:nt)

        !-- calculate storm-relative Doppler velocity from retrieved winds
        call proj_VtVr2Vrart( r_t, theta_t, thetad_t, VTtot_rt_t, VRtot_rt_t,  &
  &                           Vra_ret(1:nr,1:nt,k), undef )

        Vra_Er(1:nr,1:nt,k)=Vra(1:nr,1:nt,k)
        Vra_Er_ret(1:nr,1:nt,k)=Vra_ret(1:nr,1:nt,k)
        call add_2d( Vra_Er(1:nr,1:nt,k), projVs(1:nr,1:nt), undef )
        call add_2d( Vra_Er_ret(1:nr,1:nt,k), projVs(1:nr,1:nt), undef )
        VTtot_Er(1:nr,1:nt,k)=VTtot(1:nr,1:nt,k)
        VRtot_Er(1:nr,1:nt,k)=VRtot(1:nr,1:nt,k)
        call add_2d( VTtot_Er(1:nr,1:nt,k), projVTs_rt_t(1:nr,1:nt), undef )
        call add_2d( VRtot_Er(1:nr,1:nt,k), projVRs_rt_t(1:nr,1:nt), undef )

        call conv_V_rt2ll( x_tc*d2r, y_tc*d2r, r_t(1:nr), theta_ref_t(1:nt),  &
  &                        lonr_sph(1:nr,1:nt), latr_sph(1:nr,1:nt),  &
  &                        VRtot_Er(1:nr,1:nt,k), VTtot_Er(1:nr,1:nt,k),  &
  &                        Uxtot_Er(1:nr,1:nt,k), Vytot_Er(1:nr,1:nt,k), undef )

        call abs_2d( VRtot_Er(1:nr,1:nt,k), VTtot_Er(1:nr,1:nt,k),  &
  &                  Wstot_Er(1:nr,1:nt,k), undef )

        !-- calculate additional variables for analyses
        call calc_zeta_ax( nrot, r_t(1:nr), theta_t(1:nt), VRT0(1:nr,1:nt,k),  &
  &                        zeta0(1:nr,1:nt,k), zetan(nrotmin:nrot,1:nr,1:nt,k),  &
  &                        zetatot(1:nr,1:nt,k), undef )

     end do

     !-- 5. output to binary file (float)
     irec=1
     nval=0
     call conv_d2r_3d( VTtot(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='replace' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "VTtot "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved total tangential wind [m s-1]" )
     
     call conv_d2r_3d( VRtot(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "VRtot "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved total radial wind [m s-1]" )

     call conv_d2r_3d( VRT0(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "VRT0 "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved axisymmetric tangential wind [m s-1]" )

     call conv_d2r_3d( VDR0(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "VDR0 "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved axisymmetric radial wind [m s-1]" )

     call conv_d2r_3d( Vra(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "Vra "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"input Doppler velocity [m s-1]" )

     call conv_d2r_3d( Vra_ret(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "Vrar "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved Doppler velocity [m s-1]" )

     call conv_d2r_3d( VRT0_GVTD(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "VRT0g "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"axisymmetric tangential wind reconstructed from GVTD [m s-1]" )

     call conv_d2r_3d( VDR0_GVTD(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "VDR0g "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"axisymmetric radial wind reconstructed from GVTD [m s-1]" )

     if(nrot>0)then
        do k=1,nrot
           call conv_d2r_3d( VRTn(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "VRT"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" rotational-tangential wind [m s-1]" )

           call conv_d2r_3d( VRRn(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "VRR"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" rotational-radial wind [m s-1]" )

           call conv_d2r_3d( phin(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "phi"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" streamfunction [m2 s-1]" )

           call conv_d2r_3d( zetan(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "zeta"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" vorticity [s-1]" )
        end do
     end if

     if(ndiv>0)then
        do k=1,ndiv
           call conv_d2r_3d( VDTm(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "VDT"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" divergent-tangential wind [m s-1]" )

           call conv_d2r_3d( VDRm(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "VDR"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" divergent-radial wind [m s-1]" )
        end do
     end if

!-- optional output variables
     call conv_d2r_3d( Vn_0(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Vn0 "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"storm-relative mean wind normal to line of sight [m s-1]" )
     call conv_d2r_3d( Vra_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Vrae "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative Doppler velocity [m s-1]" )
     call conv_d2r_3d( Vra_Er_ret(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Vrare "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved Doppler velocity [m s-1]" )
     call conv_d2r_3d( VTtot_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "VTtote "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved total tangential wind [m s-1]" )
     call conv_d2r_3d( VRtot_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "VRtote "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved total radial wind [m s-1]" )
     call conv_d2r_3d( Uxtot_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Uxtote "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved total zonal wind [m s-1]" )
     call conv_d2r_3d( Vytot_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Vytote "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved total meridional wind [m s-1]" )
     call conv_d2r_3d( Wstot_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Wstote "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved total wind speed [m s-1]" )
!-- 2d monitor (lat/lon)
     call conv_d2r_2d( latd_sph(1:nr,1:nt), rval(1:nr,1:nt,nnz(1)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, 1, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(1)), mode='old' )
     irec=irec+1
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "latd 0 99 latitude [degree]" )
     call conv_d2r_2d( lond_sph(1:nr,1:nt), rval(1:nr,1:nt,nnz(1)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, 1, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(1)), mode='old' )
     irec=irec+1
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "lond 0 99 longitude [degree]" )

!-- additional output variables for analyses
     call conv_d2r_3d( zeta0(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Zeta0 "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"retrieved axisymmetric vorticity [s-1]" )
     call conv_d2r_3d( zetatot(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Zetatot "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"retrieved total vorticity [s-1]" )

!!-- NetCDF output
!
!     call HistoryPut( trim(adjustl(valc)), varbar(1:nr,nnz(1):nnz(2)) )

     write(*,*) "Writing the data at "//trim(adjustl(output_fname))

     !-- (5.1) output to 2d binary file (float)
     if(out2d_flag.eqv..true.)then
        output2d_fname=trim(adjustl(output_fname))//'.2d'
        irec=1
        call conv_d2r_2d( VRT0(1:nr,1,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
        call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                         rval2d(1:nr,nnz(1):nnz(2)), mode='replace' )
        irec=irec+1
        call conv_d2r_2d( VDR0(1:nr,1,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
        call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                         rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
        irec=irec+1
        call conv_d2r_2d( VRT0_GVTD(1:nr,1,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
        call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                         rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
        irec=irec+1
        call conv_d2r_2d( VDR0_GVTD(1:nr,1,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
        call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                         rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
        irec=irec+1

        if(nrot>0)then
           do k=1,nrot
              call conv_d2r_2d( zetans_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
              call conv_d2r_2d( zetanc_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
              call conv_d2r_2d( VRTns_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
              call conv_d2r_2d( VRTnc_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
              call conv_d2r_2d( VRRns_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
              call conv_d2r_2d( VRRnc_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
           end do
        end if

        write(*,*) "Writing the data at "//trim(adjustl(output2d_fname))

     end if

  end do

  call write_file_text_add( out_fnum(2), "vars "//trim(adjustl(i2c_convert(nval))))

  write(*,*) "Output CTL file: GVTDX.ctl"

  close(unit=out_fnum(1))
  close(unit=out_fnum(2))

!  call HistoryClose

end program
