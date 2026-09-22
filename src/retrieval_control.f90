module Retrieval_control

!! Perform GVTD-X (you can choose the retrieval method by flag_GVTDX) <br>
!! Input data: Doppler velocity based CAPPI <br>
!! Data format: 4-byte binary (nr x nt x nz) at one time for one file <br>
!! Output data: 4-byte binary (nr x nt x nz) at one time for one file <br>
!! Data grid coordinate: polar with the origin of the vortex center <br>
!! You can resample the input data from the original grid in GVTD-X retrieval. <br>
!! <br>
!! [USAGE]: ./GVTDX_Dradar < GVTDX_Dradar.nml

  use GVTDX_sub
  use GVTDX_main, only : Retrieve_velocity_GVTDX
  use GVTD_main, only : Retrieve_velocity_GVTD
  use GBVTD_main, only : Retrieve_velocity_GBVTD
  use tools_sub

contains

subroutine Retrieval_velocity( nrot, ndiv, nz, r_t, theta_ref_t, lon_tc, lat_tc,  &
  &                  usp, vsp, nthres_undef, skip_min_t, flag_GVTDX, missing_value,  &
  &                  lon_rdr, lat_rdr, Vra_in,  &
  &                  VTtot, VRtot, VRT0, VDR0, Vra, Vra_ret, VRTn, VRRn, phin, zetan,  &
  &                  VDTm, VDRm, Vn_0, Vra_Er, Vra_Er_ret, VTtot_Er, VRtot_Er, Uxtot_Er, Vytot_Er,  &
  &                  lond, latd, zeta0, zetatot, flag_datagap, umd, vmd, nrdiv, rdiv_t )

  implicit none

  !-- namelist variables
  integer, intent(in) :: nrot              !! the rotating maximum wavenumber used in the retrieval
  integer, intent(in) :: ndiv              !! the divergent maximum wavenumber used in the retrieval
  integer, intent(in) :: nz                !! vertical grid number for the input Doppler velocity
  double precision, intent(in) :: r_t(:)     !! radial coordinate on which Vra_in is defined [m]
  double precision, intent(in) :: theta_ref_t(:)     !! azimuthal coordinate on which Vra_in is defined [rad]
  double precision, intent(in) :: lon_tc   !! longitude of the TC center [degree]
  double precision, intent(in) :: lat_tc   !! latitude of the TC center [degree]
  double precision, intent(in) :: usp      !! zonal component of the moving velocity of the TC [m/s]
  double precision, intent(in) :: vsp      !! meridional component of the moving velocity of the TC [m/s]
  integer, intent(in) :: nthres_undef(2)   !! thresholds of the azimuthal sampling number to determine the (1) innermost and (2) outermost radii, respectively
  integer, intent(in) :: skip_min_t        !! threshold of the azimuthal sampling number to determine unused radii
  integer, intent(in) :: flag_GVTDX        !! 1: GVTD-X, 2: GVTD, 3: GBVTD
  real, intent(in) :: missing_value             !! undefined value for the original Doppler radar data
  double precision, intent(in) :: lon_rdr !! longitudinal position of the radar (degree)
  double precision, intent(in) :: lat_rdr !! latitudinal position of the radar (degree)
  real, dimension(size(r_t),size(theta_ref_t),nz), intent(in) :: Vra_in  !! input Doppler velocity [m/s]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: VTtot  !! retrieved total tangential wind [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: VRtot  !! retrieved total radial wind [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: VRT0  !! retrieved axisymmetric tangential wind [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: VDR0  !! retrieved axisymmetric radial wind [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: Vra      !! storm-relative input Doppler velocity [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: Vra_ret  !! storm-relative retrieved Doppler velocity [m s-1]
  double precision, dimension(nrot,size(r_t),size(theta_ref_t),nz), intent(out) :: VRTn  !! retrieved wavenumber-N rotational-tangential wind [m s-1]
  double precision, dimension(nrot,size(r_t),size(theta_ref_t),nz), intent(out) :: VRRn  !! retrieved wavenumber-N rotational-radial wind [m s-1]
  double precision, dimension(nrot,size(r_t),size(theta_ref_t),nz), intent(out) :: phin  !! retrieved wavenumber-N streamfunction [m2 s-1]
  double precision, dimension(nrot,size(r_t),size(theta_ref_t),nz), intent(out) :: zetan  !! retrieved wavenumber-N vorticity [s-1]
  double precision, dimension(ndiv,size(r_t),size(theta_ref_t),nz), intent(out) :: VDTm  !! retrieved wavenumber-M divergent-tangential wind [m s-1]
  double precision, dimension(ndiv,size(r_t),size(theta_ref_t),nz), intent(out) :: VDRm  !! retrieved wavenumber-M divergent-radial wind [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: Vn_0  !! storm-relative mean wind normal to line of sight [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: Vra_Er  !! earth-relative Doppler velocity [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: Vra_Er_ret  !! earth-relative retrieved Doppler velocity [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: VTtot_Er  !! earth-relative retrieved total tangential wind [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: VRtot_Er  !! earth-relative retrieved total radial wind [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: Uxtot_Er  !! earth-relative retrieved total zonal wind [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: Vytot_Er  !! earth-relative retrieved total meridional wind [m s-1]
  double precision, dimension(size(r_t),size(theta_ref_t)), intent(out) :: lond  !! Longitude [degree]
  double precision, dimension(size(r_t),size(theta_ref_t)), intent(out) :: latd  !! Latitude [degree]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: zeta0  !! retrieved axisymmetric vorticity [s-1]
  double precision, dimension(size(r_t),size(theta_ref_t),nz), intent(out) :: zetatot  !! retrieved total vorticity [s-1]
  logical, intent(in), optional :: flag_datagap !! Flag for use of optimal wavenumber from data gap (Lee et al. 2000)
  double precision, dimension(nz), intent(in), optional :: umd  !! zonal component of mean wind [m s-1]
  double precision, dimension(nz), intent(in), optional :: vmd  !! meridional component of mean wind [m s-1]
  integer, intent(in), optional :: nrdiv             !! radial grid number where the divergence is defined
  double precision, intent(in), dimension(nrdiv), optional :: rdiv_t  !! radial grids where the divergence is defined

  !-- internal variables
  integer :: i, j, k, id, it, m, stat, nl
  integer :: nrotmin, ndivmin
  integer :: nr_in, nr_out, ntin_c, ntout_c
  double precision :: thetad_tc, thetaM, thetaS
  double precision :: d2r, r2d, r_tmp, RdTc
  double precision :: vdm, vds, dvm
  double precision, parameter :: missing_intern=-999.0d0
  double precision :: rh_t(size(r_t)+1)  !! radial coordinate on which Phi (staggered for Vd) is defined [m]
  real, dimension(size(r_t),size(theta_ref_t)) :: rval2d
  real, dimension(size(r_t),size(theta_ref_t),nz) :: rval
  double precision, dimension(size(theta_ref_t)) :: theta_t
  double precision, dimension(size(r_t),size(theta_ref_t)) :: dval
  double precision, dimension(size(r_t),size(theta_ref_t)) :: thetad_t
  double precision, dimension(size(r_t),size(theta_ref_t)) :: lonr, latr
  double precision, dimension(size(r_t),size(theta_ref_t)) :: lonr_sph, latr_sph, lond_sph, latd_sph
  double precision, dimension(size(r_t),size(theta_ref_t)) :: projVs, projVm
  double precision, dimension(size(r_t),size(theta_ref_t)) :: projVRs_rt_t, projVTs_rt_t
  double precision, dimension(size(r_t),size(theta_ref_t)) :: projVRm_rt_t, projVTm_rt_t
!  double precision, dimension(:,:,:) :: Wstot_Er
  double precision, dimension(size(r_t),size(theta_ref_t)) :: VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t
  double precision, dimension(size(r_t),size(theta_ref_t)) :: Vn_0_rt_t
  double precision, dimension(size(r_t),size(theta_ref_t)) :: dummy_d
!  double precision, dimension(:,:,:) :: VRTns_2d, VRTnc_2d, VRRns_2d, VRRnc_2d
  double precision, dimension(size(r_t),size(theta_ref_t)) :: Vra_rt_t
  double precision, dimension(nrot,size(r_t),size(theta_ref_t)) :: VRTn_rt_t, VRRn_rt_t, phin_rt_t, zetan_rt_t
  double precision, dimension(ndiv,size(r_t),size(theta_ref_t)) :: VDTm_rt_t, VDRm_rt_t
  logical, dimension(size(r_t),size(theta_ref_t)) :: undef_grid
  logical, dimension(size(r_t),nz) :: undef_grid_2d
  logical :: flag_datagap_in

  integer :: nr_out_skp
  integer, dimension(size(r_t)) :: nr_grid_skp
  double precision, dimension(size(r_t)) :: r_skp_t
  double precision, dimension(size(r_t)+1) :: rh_skp_t
  double precision, dimension(size(r_t),size(theta_ref_t)) :: thetad_skp_t, Vra_skp_rt_t

  !-- Variables for amplitude of each component
!  double precision, allocatable, dimension(:,:) :: VTtot_2d, VRtot_2d, VRT0_2d, VDR0_2d
  double precision, allocatable, dimension(:,:,:) :: VRTn_2d, VRRn_2d, VDTm_2d, VDRm_2d
!  double precision, allocatable, dimension(:,:,:) :: phin_2d
  double precision, allocatable, dimension(:,:,:) :: zetans_2d, zetanc_2d

  if(missing_value==0.0e0)then
     write(*,*) "*** WARNING (main) *** : namelist (missing_value) is not set."
  end if

  if(present(flag_datagap))then
     flag_datagap_in=flag_datagap
  else
     flag_datagap_in=.false.
  end if

!-- initialize and allocate

  vds=0.0d0
  vdm=0.0d0

  d2r=pi_dp/180.0d0
  r2d=180.0d0/pi_dp

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

  if(present(umd))then
     umd=0.0d0
  end if
  if(present(vmd))then
     vmd=0.0d0
  end if

  !-- Define the staggerred radial grids

  do j=1, nr-1
     rh_t(j+1)=0.5d0*(r_t(j+1)+r_t(j))
  end do
  rh_t(1)=r_t(1)-0.5*(r_t(2)-r_t(1))
  if(rh_t(1)<0.0d0)then
     rh_t(1)=0.0d0
  end if
  rh_t(nr+1)=r_t(nr)+0.5d0*(r_t(nr)-r_t(nr-1))

!  allocate(VRTns_2d(nrotmin:nrot,nr,nz))
!  allocate(VRTnc_2d(nrotmin:nrot,nr,nz))
!  allocate(VRRns_2d(nrotmin:nrot,nr,nz))
!  allocate(VRRnc_2d(nrotmin:nrot,nr,nz))

!  allocate(VTtot_2d(nr,nz))
!  allocate(VRtot_2d(nr,nz))
!  allocate(VRT0_2d(nr,nz))
!  allocate(VDR0_2d(nr,nz))
!  allocate(VRTn_2d(nrotmin:nrot,nr,nz))
!  allocate(VRRn_2d(nrotmin:nrot,nr,nz))
!  allocate(VDTm_2d(ndivmin:ndiv,nr,nz))
!  allocate(VDRm_2d(ndivmin:ndiv,nr,nz))
!  allocate(Vn_0_2d(nr,nz))
!  allocate(phin_2d(nrotmin:nrot,nr,nz))
!  allocate(zetans_2d(nrotmin:nrot,nr,nz))
!  allocate(zetanc_2d(nrotmin:nrot,nr,nz))

     !-- Set temporary variables
     VTtot=missing_intern
     VRtot=missing_intern
     VTtot_Er=missing_intern
     VRtot_Er=missing_intern
     Uxtot_Er=missing_intern
     Vytot_Er=missing_intern
!     Wstot_Er=missing_intern
     zetatot=missing_intern
     zeta0=missing_intern
     VRT0=missing_intern
     VDR0=missing_intern
     VRTn=missing_intern
     VRRn=missing_intern
     VDTm=missing_intern
     VDRm=missing_intern
     phin=missing_intern
     zetan=missing_intern
     Vra=missing_intern
     Vra_ret=missing_intern
     Vra_Er=missing_intern
     Vra_Er_ret=missing_intern
     Vn_0=missing_intern
!     VRTns_2d=missing_intern
!     VRTnc_2d=missing_intern
!     VRRns_2d=missing_intern
!     VRRnc_2d=missing_intern
!     zetans_2d=missing_intern
!     zetanc_2d=missing_intern

     !-- Define thetad
     !-- 1. calculate thetad from the radar's lat-lon (lon_rdr,lat_rdr)
     !--    (i.e., the east direction is thetad = 0 at the radar's lat-lon)
     !-- 1.1 calculate lat-lon at each Doppler-velocity point from radius-azimuth and vortex center in the original data
     !-- 1.2 calculate thetad from the radar's lat-lon and the Doppler-velocity lat-lon

     do j=1,nt
        do id=1,nr
           call rt2ll( r_t(id), theta_ref_t(j), lon_tc*d2r, lat_tc*d2r,  &
  &                    lonr(id,j), latr(id,j) )
           call ll2rt( lon_rdr*d2r, lat_rdr*d2r, lonr(id,j), latr(id,j),  &
  &                    r_tmp, thetad_t(id,j) )
           lond(id,j)=lonr(id,j)*r2d
           latd(id,j)=latr(id,j)*r2d
           call sph_rt2ll( r_t(id), theta_ref_t(j), lon_tc*d2r, lat_tc*d2r,  &
  &                        lonr_sph(id,j), latr_sph(id,j) )
           lond_sph(id,j)=lonr_sph(id,j)*r2d
           latd_sph(id,j)=latr_sph(id,j)*r2d
        end do
     end do

     !-- 2. subtract thetad_tc from each thetad_t.

     call ll2rt( lon_rdr*d2r, lat_rdr*d2r, lon_tc*d2r, lat_tc*d2r, r_tmp, thetad_tc )
     do j=1,nt
        do id=1,nr
           thetad_t(id,j)=thetad_t(id,j)-thetad_tc
        end do
     end do

     !-- 3. calculate the distance from the radar to the vortex center
     RdTc=ll2radi( lon_rdr*d2r, lat_rdr*d2r, lon_tc*d2r, lat_tc*d2r )

     !-- 4. subtract thetad_tc from theta_t
     theta_t=theta_ref_t-thetad_tc

     !-- 5.1. project the storm motion to the line-of-sight direction of each radar beam
     call proj_Vs( lon_rdr*d2r, lat_rdr*d2r, lonr, latr, usp, vsp, projVs, missing_intern )
     !-- (opt) project the storm motion to the R-T coordinate
     call proj_rtVs( r_t, theta_ref_t, usp, vsp, projVRs_rt_t, projVTs_rt_t, missing_intern )

     !-- Loop for altitude
     do k=1,nz
        !-- 5.2. project the mean wind to the line-of-sight direction of each radar beam
        call proj_Vs( lon_rdr*d2r, lat_rdr*d2r, lonr, latr, umd(k), vmd(k), projVm, missing_intern )
        !-- (opt) project the mean wind to the R-T coordinate
        call proj_rtVs( r_t, theta_ref_t, umd(k), vmd(k), projVRm_rt_t, projVTm_rt_t, missing_intern )
        !-- A. convert real to double
        call conv_r2d_2d( Vra_in(1:nr_org,1:nt_org,k), dval(1:nr_org,1:nt_org) )
        call replace_val_2d( dval(1:nr_org,1:nt_org), dble(missing_value), missing_intern )
        undef_grid=.false.
        VTtot_rt_t(1:nr,1:nt)=missing_intern
        VRtot_rt_t(1:nr,1:nt)=missing_intern
        VRT0_rt_t(1:nr,1:nt)=missing_intern
        VDR0_rt_t(1:nr,1:nt)=missing_intern
        VRTn_rt_t(nrotmin:nrot,1:nr,1:nt)=missing_intern
        VRRn_rt_t(nrotmin:nrot,1:nr,1:nt)=missing_intern
        VDTm_rt_t(ndivmin:ndiv,1:nr,1:nt)=missing_intern
        VDRm_rt_t(ndivmin:ndiv,1:nr,1:nt)=missing_intern
        phin_rt_t(nrotmin:nrot,1:nr,1:nt)=missing_intern
        zetan_rt_t(nrotmin:nrot,1:nr,1:nt)=missing_intern
        Vn_0_rt_t(1:nr,1:nt)=missing_intern

        !-- C. VD - projVs
        select case(flag_GVTDX)
        case (1)  ! GVTDX
           call subst_2d( Vra_rt_t(1:nr,1:nt), projVs(1:nr,1:nt), missing_intern )
           call subst_2d( projVm(1:nr,1:nt), projVs(1:nr,1:nt), missing_intern )
        case (2)  ! GVTD
           call subst_2d( Vra_rt_t(1:nr,1:nt), projVm(1:nr,1:nt), missing_intern )
        end select

        !-- D. determine the innermost and outermost radii for retrieval using nthres_undef
        !-- D.1. check the innermost radius index
        nr_in=check_data_fulfill( Vra_rt_t(1:nr,1:nt), missing_intern,  &
  &                               nt_count=nthres_undef(1), dir="i2o",  &
  &                               ncount=ntin_c )
        if(nr_in/=0)then  ! if there is no radius with sufficient sampling, the retrieval is not performed.

           !-- check the positive value of r_t(nr_in)
           nr_in=inner_radius_check( nr_in, nr, r_t(nr_in:nr) )
!write(*,*) "nr", nr_in, ntin_c
           !-- D.2. check the outermost radius index
           nr_out=check_data_fulfill( Vra_rt_t(1:nr,1:nt), missing_intern, &
  &                                   nt_count=nthres_undef(2), dir="o2i",  &
  &                                   ncount=ntout_c )
!write(*,*) "nr", nr_out, ntout_c

           !-- (opt): activate the undef flag for each grid with missing_intern
           call check_undef_grid( Vra_rt_t, missing_intern, undef_grid )
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
  &                                         missing_intern,  &
  &                                         phin=phin_rt_t(nrotmin:nrot,nr_in:nr_out,1:nt),  &
  &                                         zetan=zetan_rt_t(nrotmin:nrot,nr_in:nr_out,1:nt),  &
  &                                         Vn_0=Vn_0_rt_t(nr_in:nr_out,1:nt) )  !,  &
!  &                                         VRTns_r=VRTns_2d(nrotmin:nrot,nr_in:nr_out,k),  &
!  &                                         VRTnc_r=VRTnc_2d(nrotmin:nrot,nr_in:nr_out,k),  &
!  &                                         VRRns_r=VRRns_2d(nrotmin:nrot,nr_in:nr_out,k),  &
!  &                                         VRRnc_r=VRRnc_2d(nrotmin:nrot,nr_in:nr_out,k),  &
!  &                                         zetans_r=zetans_2d(nrotmin:nrot,nr_in:nr_out,k),  &
!  &                                         zetanc_r=zetanc_2d(nrotmin:nrot,nr_in:nr_out,k) )
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
  &                                        missing_intern, flag_datagap=flag_datagap_in )
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
  &                                         missing_intern, flag_datagap=flag_datagap_in )
           end select

           !-- F. recover retrieved data on the rearranged radii to the original radii
           if(skip_min_t>0)then
              !-- _skp_: the original variables before the rearrangement
              !-- _rt_t, intent(inout): replace the index in the rearranged with the original index
              !-- From the above reasons, the element numbers are given from nr_in to nr_out_skp (not nr_out)
              call recover_undef_rad( nrotmin, nrot, ndivmin, ndiv,  &
  &                                   nr_in, nr_out, nr_out_skp, nt,  &
  &                                   missing_intern, undef_grid(nr_in:nr_out_skp,1:nt),  &
  &                                   nr_grid_skp(nr_in:nr_out_skp),  &
  &                                   VTtot_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   VRtot_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   VRT0_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   VDR0_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   VRTn_rt_t(nrotmin:nrot,nr_in:nr_out_skp,1:nt),  &
  &                                   VRRn_rt_t(nrotmin:nrot,nr_in:nr_out_skp,1:nt),  &
  &                                   VDTm_rt_t(ndivmin:ndiv,nr_in:nr_out_skp,1:nt),  &
  &                                   VDRm_rt_t(ndivmin:ndiv,nr_in:nr_out_skp,1:nt),  &
  &                                   dummy_d(1,nr_in:nr_out_skp,1:nt),  &  !VRT0_GVTD_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   dummy_d(1,nr_in:nr_out_skp,1:nt),  &  !VDR0_GVTD_rt_t(nr_in:nr_out_skp,1:nt),  &
  &                                   dummy_d(1:nrot,nr_in:nr_out_skp,1:nt),  &  !VRTns_2d(nrotmin:nrot,nr_in:nr_out_skp,k),  &
  &                                   dummy_d(1:nrot,nr_in:nr_out_skp,1:nt),  &  !VRTnc_2d(nrotmin:nrot,nr_in:nr_out_skp,k),  &
  &                                   dummy_d(1:nrot,nr_in:nr_out_skp,1:nt),  &  !VRRns_2d(nrotmin:nrot,nr_in:nr_out_skp,k),  &
  &                                   dummy_d(1:nrot,nr_in:nr_out_skp,1:nt),  &  !VRRnc_2d(nrotmin:nrot,nr_in:nr_out_skp,k),  &
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
!        VRT0_GVTD(1:nr,1:nt,k)=VRT0_GVTD_rt_t(1:nr,1:nt)
!        VDR0_GVTD(1:nr,1:nt,k)=VDR0_GVTD_rt_t(1:nr,1:nt)
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
  &                           Vra_ret(1:nr,1:nt,k), missing_intern )

        Vra_Er(1:nr,1:nt,k)=Vra(1:nr,1:nt,k)
        Vra_Er_ret(1:nr,1:nt,k)=Vra_ret(1:nr,1:nt,k)
        call add_2d( Vra_Er(1:nr,1:nt,k), projVs(1:nr,1:nt), missing_intern )
        call add_2d( Vra_Er_ret(1:nr,1:nt,k), projVs(1:nr,1:nt), missing_intern )
        VTtot_Er(1:nr,1:nt,k)=VTtot(1:nr,1:nt,k)
        VRtot_Er(1:nr,1:nt,k)=VRtot(1:nr,1:nt,k)
        call add_2d( VTtot_Er(1:nr,1:nt,k), projVTs_rt_t(1:nr,1:nt), missing_intern )
        call add_2d( VRtot_Er(1:nr,1:nt,k), projVRs_rt_t(1:nr,1:nt), missing_intern )

        call conv_V_rt2ll( lon_tc*d2r, lat_tc*d2r, r_t(1:nr), theta_ref_t(1:nt),  &
  &                        lonr_sph(1:nr,1:nt), latr_sph(1:nr,1:nt),  &
  &                        VRtot_Er(1:nr,1:nt,k), VTtot_Er(1:nr,1:nt,k),  &
  &                        Uxtot_Er(1:nr,1:nt,k), Vytot_Er(1:nr,1:nt,k), missing_intern )

!        call abs_2d( VRtot_Er(1:nr,1:nt,k), VTtot_Er(1:nr,1:nt,k),  &
!  &                  Wstot_Er(1:nr,1:nt,k), missing_intern )

        !-- calculate additional variables for analyses
        call calc_zeta_ax( nrot, r_t(1:nr), theta_t(1:nt), VRT0(1:nr,1:nt,k),  &
  &                        zeta0(1:nr,1:nt,k), zetan(nrotmin:nrot,1:nr,1:nt,k),  &
  &                        zetatot(1:nr,1:nt,k), missing_intern )

     end do

  end do

!  call HistoryClose

end subroutine Retrieval_velocity

end module Retrieval_control