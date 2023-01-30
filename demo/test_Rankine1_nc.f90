program test_Rankine1
!-- A retrieval program for a 2-dim analytical vortex

  use gtool_history
  use GVTDX_sub
  use GVTDX_main
  use GVTD_main
  use GBVTD_main
!  use GVTDX_main2

  implicit none

  integer, parameter :: nvp_max=100
  integer, parameter :: nrdiv_max=100

!-- namelist
  integer :: flag_GVTDX
  integer :: nvp, nup, nxd, nyd, nr_d, nr_t, nt_d, nt_t
  integer :: nrot, ndiv, nrdiv
  integer :: IWS, tone_grid, cmap
  integer :: contour_num, contour_num2, contour_num3
  integer :: shade_num, min_tab, max_tab
  integer, dimension(80) :: fix_col
  integer, dimension(20) :: fixc_idx, fixc_typ, fixc_idx2, fixc_typ2, fixc_idx3, fixc_typ3
  real, dimension(80) :: fix_val
  real, dimension(20) :: fixc_val, fixc_val2, fixc_val3
  double precision :: us, vs
  double precision :: xdmin, xdmax, ydmin, ydmax
  double precision :: r_dmin, r_dmax, t_dmin, t_dmax
  double precision :: r_tmin, r_tmax, t_tmin, t_tmax
  double precision :: undef, xax_fact, yax_fact
  double precision :: tc_xd, tc_yd, ra_xd, ra_yd, tc_ra_r, tc_ra_t
  double precision :: rvmax, vmax, c1u, c2u
  double precision :: vp(nvp_max), up(nvp_max), vpa(nvp_max), upa(nvp_max)
  double precision, dimension(nrdiv_max) :: rdiv
  character(20) :: form_typec, form_typec2, form_typec3, form_types
  logical :: col_rev, ropt

!-- internal
  integer :: i, j, k, cstat
  double precision :: d2r, r2d, rad_tc
  double precision :: Usrn(2), Vsrn(2), Vra1d, thetad_tc
  double precision, dimension(2) :: vx_new, vy_new
  double precision :: dxd, dyd, dr_d, dr_t, dt_d, dt_t
  double precision, allocatable, dimension(:) :: xd, yd, r_d, r_t, rh_t, t_d, t_t, t_ref_t
  double precision, allocatable, dimension(:,:) :: tdr_t
  double precision, allocatable, dimension(:,:) :: Vd_rt_d, Ut_xyd, Vt_xyd, Vra_xyd, Vsra_xyd, Vratot_xyd
  double precision, allocatable, dimension(:,:) :: Utott_xyd, Vtott_xyd, Vra_rt_t, Vratot_rt_t, Vsra_rt_t
  double precision, allocatable, dimension(:,:) :: Ut_rht_t, Vt_rht_t
  double precision, allocatable, dimension(:,:) :: Vst_rht_t, Usr_rht_t
  double precision, allocatable, dimension(:,:) :: Vx_rht_t, Vy_rht_t
  double precision, allocatable, dimension(:,:) :: Vx_xyd_t, Vy_xyd_t
  double precision, allocatable, dimension(:,:) :: VRT0_rt_t, VDR0_rt_t, VTtot_rt_t, VRtot_rt_t
  double precision, allocatable, dimension(:,:) :: us0, vs0, us0_rht_t, vs0_rht_t
  double precision, allocatable, dimension(:,:) :: div_rht_t, rot_rht_t, div_xyd, rot_xyd
  double precision, allocatable, dimension(:,:) :: zeta0_rt_t, zeta_xyd, dummy_rt_t
  double precision, allocatable, dimension(:,:,:) :: phin_rt_t, phin_xyd, zetan_rt_t
  double precision, allocatable, dimension(:,:,:) :: VRTn_rt_t, VRRn_rt_t, VDTm_rt_t, VDRm_rt_t

  real :: dundef
  real, allocatable, dimension(:) :: draw_xd, draw_yd
  real, allocatable, dimension(:,:) :: draw_Vt, draw_Vr, draw_Vt_ret, draw_Vr_ret
  real, allocatable, dimension(:,:) :: draw_Vra, draw_Vra_ret, draw_rot, draw_div
  real, allocatable, dimension(:,:) :: draw_dVt, draw_dVr, draw_dVra, draw_zeta
  real, allocatable, dimension(:,:,:) :: draw_phin
  character(20) :: cvtmax, cvrmax, cvamax
  character(1) :: tmpk
  type(GT_HISTORY) :: val_hst

  namelist /input /nvp, nup, undef, rvmax, vmax, c1u, c2u, vp, up, vpa, upa,  &
  &                us, vs, nrot, ndiv, ropt, nrdiv, rdiv, flag_GVTDX
  namelist /domain /nxd, nyd, nr_d, nr_t, nt_d, nt_t,  &
  &                 xdmin, xdmax, ydmin, ydmax,  &
  &                 r_dmin, r_dmax, t_dmin, t_dmax,  &
  &                 r_tmin, r_tmax, t_tmin, t_tmax
  namelist /pos_info /tc_xd, tc_yd, ra_xd, ra_yd
  namelist /nc_info /ncfile

  read(5,nml=input)
  read(5,nml=domain)
  read(5,nml=pos_info)
  read(5,nml=nc_info)

  d2r=pi/180.0d0
  r2d=180.0d0/pi

  if(flag_GVTDX/=1)then
     nrot=3
  end if

!-- Allocate and assign variables for coordinates
  allocate(xd(nxd),stat=cstat)  ! Drawing area for x on X-Y coordinate
  allocate(yd(nyd),stat=cstat)  ! Drawing area for y on X-Y coordinate
  allocate(r_d(nr_d+1),stat=cstat)  ! Whole range of vortex on radar R-T coordinate
  allocate(r_t(nr_t+1),stat=cstat)  ! Radius on TC R-T coordinate
  allocate(t_d(nt_d),stat=cstat)  ! Whole azimuthal angle on radar R-T coordinate
  allocate(t_t(nt_t),stat=cstat)  ! Azimuthal angle on TC R-T coordinate
  allocate(t_ref_t(nt_t),stat=cstat)  ! Reference angle on TC R-T coordinate
  allocate(Vd_rt_d(nr_d,nt_d),stat=cstat)  ! Velocity along beam on radar R-T coodinate
  allocate(tdr_t(nr_t,nt_t),stat=cstat)  ! Radar azimuthal angle on TC R-T coordinate
  allocate(rh_t(nr_t),stat=cstat)  ! (staggered) Radius on TC R-T coordinate
  allocate(VTtot_rt_t(nr_t,nt_t),stat=cstat)  ! Total tangential wind on TC R-T coordinate
  allocate(VRtot_rt_t(nr_t,nt_t),stat=cstat)  ! Total radial wind on TC R-T coordinate
  allocate(VRT0_rt_t(nr_t,nt_t),stat=cstat)  ! Axisymmetric tangential wind on TC R-T coordinate
  allocate(VDR0_rt_t(nr_t,nt_t),stat=cstat)  ! Axisymmetric radial wind on TC R-T coordinate
  allocate(VRTn_rt_t(nrot,nr_t,nt_t),stat=cstat)  ! Asymmetric rotating wind on TC R-T coordinate
  allocate(VRRn_rt_t(nrot,nr_t,nt_t),stat=cstat)  ! Asymmetric rotating wind on TC R-T coordinate
  allocate(VDTm_rt_t(ndiv,nr_t,nt_t),stat=cstat)  ! Asymmetric divergent wind on TC R-T coordinate
  allocate(VDRm_rt_t(ndiv,nr_t,nt_t),stat=cstat)  ! Asymmetric divergent wind on TC R-T coordinate
  allocate(Ut_rht_t(nr_t,nt_t),stat=cstat)  ! Radial wind on TC R-T coordinate
  allocate(Vt_rht_t(nr_t,nt_t),stat=cstat)  ! Tangential wind on TC R-T coordinate
  allocate(Usr_rht_t(nr_t,nt_t),stat=cstat)  ! Radial component of environmental wind on TC R-T coordinate
  allocate(Vst_rht_t(nr_t,nt_t),stat=cstat)  ! Tangential component of environmental wind on TC R-T coordinate
  allocate(Vra_rt_t(nr_t,nt_t),stat=cstat)  ! Velocity along beam on TC R-T coordinate
  allocate(Vsra_rt_t(nr_t,nt_t),stat=cstat)  ! Environmental wind velocity along beam on TC R-T coordinate
  allocate(Vratot_rt_t(nr_t,nt_t),stat=cstat)  ! Velocity along beam on TC R-T coordinate
  allocate(Ut_xyd(nxd,nyd),stat=cstat)  ! Radial wind on X-Y coordinate
  allocate(Vt_xyd(nxd,nyd),stat=cstat)  ! Tangential wind on X-Y coordinate
  allocate(Utott_xyd(nxd,nyd),stat=cstat)  ! Total radial wind on X-Y coordinate
  allocate(Vtott_xyd(nxd,nyd),stat=cstat)  ! Total tangential wind on X-Y coordinate
  allocate(Vx_rht_t(nr_t,nt_t),stat=cstat)  ! X component of wind on TC R-T coordinate
  allocate(Vy_rht_t(nr_t,nt_t),stat=cstat)  ! Y component of wind on TC R-T coordinate
  allocate(Vx_xyd_t(nxd,nyd),stat=cstat)  ! X component of wind on X-Y coordinate
  allocate(Vy_xyd_t(nxd,nyd),stat=cstat)  ! Y component of wind on X-Y coordinate
  allocate(Vra_xyd(nxd,nyd),stat=cstat)  ! Velocity along beam on X-Y coordinate
  allocate(Vsra_xyd(nxd,nyd),stat=cstat)  ! Environmental wind velocity along beam on X-Y coordinate
  allocate(Vratot_xyd(nxd,nyd),stat=cstat)  ! Retrieved velocity along beam on X-Y coordinate
  allocate(us0(nxd,nyd),stat=cstat)  ! X-component of homogeneous wind on X-Y coordinate
  allocate(vs0(nxd,nyd),stat=cstat)  ! Y-component of homogeneous wind on X-Y coordinate
  allocate(us0_rht_t(nr_t,nt_t),stat=cstat)  ! X-component of homogeneous wind on R-T coordinate
  allocate(vs0_rht_t(nr_t,nt_t),stat=cstat)  ! Y-component of homogeneous wind on R-T coordinate
  allocate(div_rht_t(nr_t,nt_t),stat=cstat)  ! divergence on R-T coordinate
  allocate(rot_rht_t(nr_t,nt_t),stat=cstat)  ! rotation on R-T coordinate
  allocate(phin_rt_t(nrot,nr_t,nt_t),stat=cstat)  ! phin on R-T coordinate
  allocate(zeta0_rt_t(nr_t,nt_t),stat=cstat)  ! zeta0 on R-T coordinate
  allocate(dummy_rt_t(nr_t,nt_t),stat=cstat)  ! dummy array
  allocate(zetan_rt_t(nrot,nr_t,nt_t),stat=cstat)  ! zetan on R-T coordinate
  allocate(div_xyd(nxd,nyd),stat=cstat)  ! divergence on X-Y coordinate
  allocate(rot_xyd(nxd,nyd),stat=cstat)  ! rotation on X-Y coordinate
  allocate(phin_xyd(nrot,nxd,nyd),stat=cstat)  ! phin on X-Y coordinate
  allocate(zeta_xyd(nxd,nyd),stat=cstat)  ! zeta_tot on X-Y coordinate

  !-- For drawing variables
  allocate(draw_xd(nxd),stat=cstat)
  allocate(draw_yd(nyd),stat=cstat)
  allocate(draw_Vt(nxd,nyd),stat=cstat)
  allocate(draw_Vr(nxd,nyd),stat=cstat)
  allocate(draw_Vt_ret(nxd,nyd),stat=cstat)
  allocate(draw_Vr_ret(nxd,nyd),stat=cstat)
  allocate(draw_dVt(nxd,nyd),stat=cstat)
  allocate(draw_dVr(nxd,nyd),stat=cstat)
  allocate(draw_dVra(nxd,nyd),stat=cstat)
  allocate(draw_Vra(nxd,nyd),stat=cstat)
  allocate(draw_Vra_ret(nxd,nyd),stat=cstat)
  allocate(draw_div(nxd,nyd),stat=cstat)
  allocate(draw_rot(nxd,nyd),stat=cstat)
  allocate(draw_phin(nrot,nxd,nyd),stat=cstat)
  allocate(draw_zeta(nxd,nyd),stat=cstat)

  if(cstat/=0)then
     call stdout( "Failed to allocate variables. stop.", "main", -1 )
     stop
  else
     call stdout( "Allocated variables.", "main", 0 )
  end if

  dxd=(xdmax-xdmin)/dble(nxd-1)
  dyd=(ydmax-ydmin)/dble(nyd-1)
  dr_d=(r_dmax-r_dmin)/dble(nr_d-1)
  dr_t=(r_tmax-r_tmin)/dble(nr_t-1)
  dt_d=(t_dmax-t_dmin)/dble(nt_d-1)
  dt_t=(t_tmax-t_tmin)/dble(nt_t-1)

  xd=(/((xdmin+dxd*dble(i-1)),i=1,nxd)/)
  yd=(/((ydmin+dyd*dble(i-1)),i=1,nyd)/)
  r_d=(/((r_dmin+dr_d*dble(i-1)),i=1,nr_d+1)/)
  r_t=(/((r_tmin+dr_t*dble(i-1)),i=1,nr_t+1)/)
  t_d=(/((t_dmin+dt_d*dble(i-1)),i=1,nt_d)/)
  t_t=(/((t_tmin+dt_t*dble(i-1)),i=1,nt_t)/)

  t_d=t_d*d2r
  t_t=t_t*d2r
  t_ref_t=t_t

  rh_t(1:nr_t)=r_t(1:nr_t)+0.5d0*dr_t
  do j=1,nt_t
     do i=1,nr_t
        tdr_t(i,j)=datan2(((tc_yd-ra_yd)+rh_t(i)*dsin(t_t(j))),((tc_xd-ra_xd)+rh_t(i)*dcos(t_t(j))))
     end do
  end do

!-- Making relative angle to the storm center (tdr_r - thetad_tc)
  rad_tc=dsqrt((tc_xd-ra_xd)**2+(tc_yd-ra_yd)**2)
  thetad_tc=datan2((tc_yd-ra_yd),(tc_xd-ra_xd))
  write(*,*) "thetad_tc is ", thetad_tc
  do j=1,nt_t
     do i=1,nr_t
        tdr_t(i,j)=tdr_t(i,j)-thetad_tc
     end do
  end do
  t_t=t_ref_t-thetad_tc

!-- producing vortex profiles at vector points
  call prod_vortex_structure( rh_t, t_ref_t, rvmax, vmax, c1u, c2u,  &
  &                           Vt_rht_t, Ut_rht_t, vp(1:nvp), up(1:nup),  &
  &                           vpa(1:nvp)*d2r, upa(1:nup)*d2r, ropt=ropt,  &
  &                           Uxm=Usrn, Vym=Vsrn )

!-- Environmental wind (Us, Vs) -> Vra(r_t,t_t), Vrn(r_t,t_t)
  us0=us
  vs0=vs
  call proj_VxVy2Vraxy( xd, yd, ra_xd, ra_yd, us0, vs0, Vsra_xyd, undef=undef )
  call tangent_conv_scal( xd, yd, tc_xd, tc_yd, Vsra_xyd, rh_t, t_ref_t, Vsra_rt_t,  &
  &                       undef=undef, undefg=undef, stdopt=.true. )
  Usrn=0.0d0
  Vsrn=0.0d0

!-- Environmental wind (Us, Vs) -> Vsr(r_t,t_ref_t), Vst(r_t,t_ref_t) for only drawing
  ! (Us, Vs)(xd,yd) -> (Us, Vs)(r_t,t_ref_t) -> (Vsr,Vst)(r_t,t_ref_t)
  call tangent_conv_scal( xd, yd, tc_xd, tc_yd, us0, rh_t, t_ref_t, us0_rht_t,  &
  &                       undef=undef, undefg=undef, stdopt=.true. )
  call tangent_conv_scal( xd, yd, tc_xd, tc_yd, vs0, rh_t, t_ref_t, vs0_rht_t,  &
  &                       undef=undef, undefg=undef, stdopt=.true. )
  call conv_VxVy2VtVr_rt( rh_t, t_ref_t, us0_rht_t, vs0_rht_t, Vst_rht_t, Usr_rht_t )

!-- converting (Vr,Vt)(r_t,t_ref_t) -> (Vx,Vy)(r_t,t_ref_t)
  call conv_VtVr2VxVy_rt( rh_t, t_ref_t, Vt_rht_t, Ut_rht_t, Vx_rht_t, Vy_rht_t )
  call cart_conv_scal( rh_t, t_ref_t, Vx_rht_t, xd, yd, tc_xd, tc_yd, Vx_xyd_t, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  call cart_conv_scal( rh_t, t_ref_t, Vy_rht_t, xd, yd, tc_xd, tc_yd, Vy_xyd_t, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  call proj_VxVy2Vraxy( xd, yd, ra_xd, ra_yd, Vx_xyd_t, Vy_xyd_t, Vra_xyd, undef=undef )
  call proj_VtVr2Vrart( rh_t, t_t, tdr_t, Vt_rht_t, Ut_rht_t, Vra_rt_t, undef=undef )
  call subst_2d( Vra_xyd, Vsra_xyd, undef=undef )

  call stdout( "Projected winds.", "main", 0 )

!-- converting (r_t,t_t) -> (xd,yd)
  call subst_2d( Vt_rht_t, Vst_rht_t, undef=undef )
  call subst_2d( Ut_rht_t, Usr_rht_t, undef=undef )
  call cart_conv_scal( rh_t, t_ref_t, Vt_rht_t, xd, yd, tc_xd, tc_yd, Vt_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  call cart_conv_scal( rh_t, t_ref_t, Ut_rht_t, xd, yd, tc_xd, tc_yd, Ut_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )

  call stdout( "Converted r-t -> x-y.", "main", 0 )

!-- Retrieving all components of horizontal winds (VRT, VRR, VDT, and VDR) from Vd
  call subst_2d( Vra_rt_t, Vsra_rt_t, undef=undef )  ! Vd - proj(Vs)
  call cart_conv_scal( rh_t, t_ref_t, Vra_rt_t, xd, yd, tc_xd, tc_yd, Vra_xyd,  &
  &                    undef=undef, undefg=undef, stdopt=.true. )
  call sum_1d( Vra_rt_t(1,1:nt_t), Vra1d, undef )  ! calc. mean Vra
write(*,*) "val check", Vra1d

  select case (flag_GVTDX)
  case (1)  ! GVTDX
     call Retrieve_velocity_GVTDX( nrot, ndiv, rh_t, t_t, r_t, tdr_t, rdiv(1:nrdiv),  &
  &                                Vra_rt_t, Usrn, Vsrn, rad_tc,  &
  &                                VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                                VRTn_rt_t, VRRn_rt_t, VDTm_rt_t, VDRm_rt_t,  &
  &                                undef, phin=phin_rt_t, zetan=zetan_rt_t )
  case (2)  ! GVTD
     call Retrieve_velocity_GVTD( nrot, rh_t, t_t, tdr_t, Vra_rt_t,  &
  &                               Usrn, Vsrn, rad_tc,  &
  &                               VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                               VRTn_rt_t, VRRn_rt_t, undef )
  case (3)  ! GBVTD
     call Retrieve_velocity_GBVTD( nrot, rh_t, t_t, tdr_t, Vra_rt_t,  &
  &                                Usrn, Vsrn, rad_tc,  &
  &                                VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                                VRTn_rt_t, VRRn_rt_t, undef )
  end select
  call stdout( "Retrieved velocity.", "main", 0 )

!-- converting (r_t,t_ref_t) -> (xd,yd)
  call proj_VtVr2Vrart( rh_t, t_t, tdr_t, VTtot_rt_t, VRtot_rt_t, Vratot_rt_t, undef=undef )
  call cart_conv_scal( rh_t, t_ref_t, VTtot_rt_t, xd, yd, tc_xd, tc_yd, Vtott_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  call cart_conv_scal( rh_t, t_ref_t, VRtot_rt_t, xd, yd, tc_xd, tc_yd, Utott_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  call cart_conv_scal( rh_t, t_ref_t, Vratot_rt_t, xd, yd, tc_xd, tc_yd, Vratot_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )

!-- calculate divergence and rotation for the retrieved VR and VT
  call div_curl_2d( rh_t, t_ref_t, Ut_rht_t, Vt_rht_t, div_rht_t, rot_rht_t, undef=undef )
  call cart_conv_scal( rh_t, t_ref_t, div_rht_t, xd, yd, tc_xd, tc_yd, div_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  call cart_conv_scal( rh_t, t_ref_t, rot_rht_t, xd, yd, tc_xd, tc_yd, rot_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )

  call div_curl_2d( rh_t, t_t, VDR0_rt_t, VRT0_rt_t,  &
  &                 dummy_rt_t(1:nr_t,1:nt_t), zeta0_rt_t(1:nr_t,1:nt_t) )
  if(nrot>0)then
     do k=1,nrot
        call cart_conv_scal( rh_t, t_ref_t, phin_rt_t(k,1:nr_t,1:nt_t),  &
  &                          xd, yd, tc_xd, tc_yd,  &
  &                          phin_xyd(k,1:nxd,1:nyd), undef=undef,  &
  &                          undefg=undef, stdopt=.true. )
        call add_2d( zeta0_rt_t(1:nr_t,1:nt_t),  &
  &                  zetan_rt_t(k,1:nr_t,1:nt_t), undef=undef )
     end do
  else
     call cart_conv_scal( rh_t, t_ref_t, phin_rt_t(nrot,1:nr_t,1:nt_t),  &
  &                       xd, yd, tc_xd, tc_yd,  &
  &                       phin_xyd(nrot,1:nxd,1:nyd), undef=undef,  &
  &                       undefg=undef, stdopt=.true. )
  end if
  call cart_conv_scal( rh_t, t_ref_t, zeta0_rt_t(1:nr_t,1:nt_t),  &
  &                    xd, yd, tc_xd, tc_yd,  &
  &                    zeta_xyd(1:nxd,1:nyd), undef=undef,  &
  &                    undefg=undef, stdopt=.true. )

!-- calculate the maximum difference between analysis and retrieval
  call display_2valdiff_max( Vt_rht_t, VTtot_rt_t, undef=undef, cout=cvtmax )
  call display_2valdiff_max( Ut_rht_t, VRtot_rt_t, undef=undef, cout=cvrmax )
  call display_2valdiff_max( Vra_rt_t, Vratot_rt_t, undef=undef, cout=cvamax )

!-- DCL drawing
  call conv_d2r_1d( xd, draw_xd )
  call conv_d2r_1d( yd, draw_yd )
  draw_xd=draw_xd*real(xax_fact)
  draw_yd=draw_yd*real(yax_fact)
  call conv_d2r_2d( Vt_xyd, draw_Vt )
  call conv_d2r_2d( Ut_xyd, draw_Vr )
  call conv_d2r_2d( Vtott_xyd, draw_Vt_ret )
  call conv_d2r_2d( Utott_xyd, draw_Vr_ret )
  call conv_d2r_2d( Vra_xyd, draw_Vra )
  call conv_d2r_2d( Vratot_xyd, draw_Vra_ret )
  call conv_d2r_2d( div_xyd, draw_div )
  call conv_d2r_2d( rot_xyd, draw_rot )
  draw_dVt=draw_Vt_ret
  draw_dVr=draw_Vr_ret
  draw_dVra=draw_Vra_ret
  call subst_2d_r( draw_dVt, draw_Vt, undef=real(undef) )
  call subst_2d_r( draw_dVr, draw_Vr, undef=real(undef) )
  call subst_2d_r( draw_dVra, draw_Vra, undef=real(undef) )
  if(nrot>0)then
     do k=1,nrot
        call conv_d2r_2d( phin_xyd(k,1:nxd,1:nyd), draw_phin(k,1:nxd,1:nyd) )
     end do
  else
     call conv_d2r_2d( phin_xyd(nrot,1:nxd,1:nyd), draw_phin(nrot,1:nxd,1:nyd) )
  end if
  call conv_d2r_2d( zeta_xyd(1:nxd,1:nyd), draw_zeta(1:nxd,1:nyd) )

!-- Output NetCDF

  call HistoryCreate( &
  &    file=trim(ncfile),  &
  &    title='Test procedure of GVTDX', &
  &    source='test_Rankine1_nc',   &
  &    institution='Meteorological Research Institute / Japan Meteorological Agency',       &
  &    dims=dimname,  &
  &    dimsizes=dimsize,  &
  &    longnames=longname,  &
  &    units=units, history=vt_hst )

  
  call HistoryAddVariable( &                   ! 変数定義
  &    varname='vt',  &
  &    dims=(/dimname(1), dimname(2), dimname(3) /),  &
  &    longname=vall(1:len_trim(vall)),  &
  &    units=valu(1:len_trim(valu)), xtype='float', history=vt_hst )

     call HistoryAddAttr( 'vt', 'missing_value', undef, history=vt_hst )

     call HistoryPut( dimname(1), r, history=vt_hst )                 ! 次元変数出力
     call HistoryPut( dimname(2), z(nnz(1):nnz(2)), history=vt_hst )  ! 次元変数出力

end program
