program test_Rankine
!-- Test the dependency of errors in vortex center estimation in an analytical vortex

  use dcl
  use Dcl_Automatic
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
  double precision :: pseudo_tc_xd, pseudo_tc_yd
  double precision :: rvmax, vmax, c1u, c2u
  double precision :: vp(nvp_max), up(nvp_max), vpa(nvp_max), upa(nvp_max)
  double precision, dimension(nrdiv_max) :: rdiv
  character(20) :: form_typec, form_typec2, form_typec3, form_types
  logical :: col_rev, ropt, dopt, flag_fout

!-- internal
  integer :: i, j, k, cstat, ivmax
  double precision :: d2r, r2d, rad_tc, pseudo_rad_tc, maxv
  double precision :: Usrn(2), Vsrn(2), Vra1d, thetad_tc, pseudo_thetad_tc
  double precision, dimension(2) :: vx_new, vy_new
  double precision :: dxd, dyd, dr_d, dr_t, dt_d, dt_t
  double precision, allocatable, dimension(:) :: xd, yd, r_d, r_t, rh_t, t_d, t_t, t_ref_t, t_ref_d
  double precision, allocatable, dimension(:) :: Vt_r_d, Ut_r_d, VtP_r_d, UtP_r_d
  double precision, allocatable, dimension(:,:) :: tdr_t, tdr_d
  double precision, allocatable, dimension(:,:) :: Ut_xyd, Vt_xyd, Vra_xyd, Vsra_xyd, Vratot_xyd
  double precision, allocatable, dimension(:,:) :: UtP_xyd, VtP_xyd
  double precision, allocatable, dimension(:,:) :: Utott_xyd, Vtott_xyd, Vra_rt_d, Vratot_rt_t
  double precision, allocatable, dimension(:,:) :: Vst_rt_d, Usr_rt_d
  double precision, allocatable, dimension(:,:) :: Vx_rt_d, Vy_rt_d
  double precision, allocatable, dimension(:,:) :: VtP_rt_d, UtP_rt_d
  double precision, allocatable, dimension(:,:) :: Vx_xyd, Vy_xyd
  double precision, allocatable, dimension(:,:) :: Vtotx_xyd, Vtoty_xyd
  double precision, allocatable, dimension(:,:) :: Vxtot_rt_t, Vytot_rt_t
  double precision, allocatable, dimension(:,:) :: dVx_xyd, dVy_xyd
  double precision, allocatable, dimension(:,:) :: Ut_rt_d, Vt_rt_d, VraP_rt_t
  double precision, allocatable, dimension(:,:) :: VRT0_rt_t, VDR0_rt_t, VTtot_rt_t, VRtot_rt_t
  double precision, allocatable, dimension(:,:) :: us0, vs0, us0_rt_d, vs0_rt_d
  double precision, allocatable, dimension(:,:) :: div_rt_d, rot_rt_d, div_xyd, rot_xyd
  double precision, allocatable, dimension(:,:,:) :: phin_rt_t, phin_xyd
  double precision, allocatable, dimension(:,:,:) :: VRTn_rt_t, VRRn_rt_t, VDTm_rt_t, VDRm_rt_t

  real :: dundef
  real, allocatable, dimension(:) :: draw_xd, draw_yd
  real, allocatable, dimension(:,:) :: draw_Vt, draw_Vr, draw_Vt_ret, draw_Vr_ret
  real, allocatable, dimension(:,:) :: draw_Vra, draw_Vra_ret, draw_rot, draw_div
  real, allocatable, dimension(:,:) :: draw_dVt, draw_dVr, draw_dVra, draw_phi1
  real, allocatable, dimension(:,:) :: draw_Vx, draw_Vy, draw_Vx_ret, draw_Vy_ret
  real, allocatable, dimension(:,:) :: draw_dVx, draw_dVy
  character(20) :: cvtmax, cvrmax, cvamax, cvxmax, cvymax

  namelist /input /nvp, nup, undef, rvmax, vmax, c1u, c2u, vp, up, vpa, upa,  &
  &                us, vs, nrot, ndiv, ropt, dopt, nrdiv, rdiv, flag_GVTDX
  namelist /domain /nxd, nyd, nr_d, nr_t, nt_d, nt_t,  &
  &                 xdmin, xdmax, ydmin, ydmax,  &
  &                 r_dmin, r_dmax, t_dmin, t_dmax,  &
  &                 r_tmin, r_tmax, t_tmin, t_tmax
  namelist /pos_info /tc_xd, tc_yd, ra_xd, ra_yd, pseudo_tc_xd, pseudo_tc_yd, flag_fout
  namelist /draw_info /IWS, tone_grid, cmap, col_rev,  &
  &                    contour_num, fixc_val, fixc_idx, fixc_typ, form_typec,  &
  &                    contour_num2, fixc_val2, fixc_idx2, fixc_typ2, form_typec2,  &
  &                    contour_num3, fixc_val3, fixc_idx3, fixc_typ3, form_typec3,  &
  &                    shade_num, min_tab, max_tab, fix_val, fix_col, form_types,  &
  &                    xax_fact, yax_fact

  read(5,nml=input)
  read(5,nml=domain)
  read(5,nml=pos_info)
  read(5,nml=draw_info)

  d2r=pi/180.0d0
  r2d=180.0d0/pi

  if(flag_GVTDX/=1)then
     nrot=3
  end if

!-- Allocate and assign variables for coordinates
  allocate(xd(nxd),stat=cstat)  ! Drawing area for x on X-Y coordinate
  allocate(yd(nyd),stat=cstat)  ! Drawing area for y on X-Y coordinate
  allocate(r_d(nr_d),stat=cstat)  ! Whole range of vortex on radar R-T coordinate
  allocate(r_t(nr_t+1),stat=cstat)  ! Radius on TC R-T coordinate
  allocate(t_d(nt_d),stat=cstat)  ! Whole azimuthal angle on radar R-T coordinate
  allocate(t_t(nt_t),stat=cstat)  ! Azimuthal angle on TC R-T coordinate
  allocate(t_ref_t(nt_t),stat=cstat)  ! Reference angle on TC R-T coordinate
  allocate(t_ref_d(nt_d),stat=cstat)  ! Reference angle on TC R-T coordinate
  allocate(tdr_t(nr_t,nt_t),stat=cstat)  ! Radar azimuthal angle on TC R-T coordinate
  allocate(tdr_d(nr_d,nt_d),stat=cstat)  ! Radar azimuthal angle on TC R-T coordinate
  allocate(rh_t(nr_t),stat=cstat)  ! (staggered) Radius on TC R-T coordinate
  allocate(VTtot_rt_t(nr_t,nt_t),stat=cstat)  ! Total tangential wind on TC R-T coordinate
  allocate(VRtot_rt_t(nr_t,nt_t),stat=cstat)  ! Total radial wind on TC R-T coordinate
  allocate(VRT0_rt_t(nr_t,nt_t),stat=cstat)  ! Axisymmetric tangential wind on TC R-T coordinate
  allocate(VDR0_rt_t(nr_t,nt_t),stat=cstat)  ! Axisymmetric radial wind on TC R-T coordinate
  allocate(VRTn_rt_t(nrot,nr_t,nt_t),stat=cstat)  ! Asymmetric rotating wind on TC R-T coordinate
  allocate(VRRn_rt_t(nrot,nr_t,nt_t),stat=cstat)  ! Asymmetric rotating wind on TC R-T coordinate
  allocate(VDTm_rt_t(ndiv,nr_t,nt_t),stat=cstat)  ! Asymmetric divergent wind on TC R-T coordinate
  allocate(VDRm_rt_t(ndiv,nr_t,nt_t),stat=cstat)  ! Asymmetric divergent wind on TC R-T coordinate
  allocate(Usr_rt_d(nr_d,nt_d),stat=cstat)  ! Radial component of environmental wind on TC R-T coordinate
  allocate(Vst_rt_d(nr_d,nt_d),stat=cstat)  ! Tangential component of environmental wind on TC R-T coordinate
  allocate(Vra_rt_d(nr_d,nt_d),stat=cstat)  ! Velocity along beam on TC R-T coordinate
  allocate(VraP_rt_t(nr_t,nt_t),stat=cstat)  ! Velocity along beam on TC R-T coordinate
  allocate(Vratot_rt_t(nr_t,nt_t),stat=cstat)  ! Velocity along beam on TC R-T coordinate
  allocate(Vxtot_rt_t(nr_t,nt_t),stat=cstat)  ! X component of wind on TC R-T coordinate
  allocate(Vytot_rt_t(nr_t,nt_t),stat=cstat)  ! Y component of wind on TC R-T coordinate
  allocate(Ut_rt_d(nr_d,nt_d),stat=cstat)  ! Radial wind on TC R-T coordinate
  allocate(Vt_rt_d(nr_d,nt_d),stat=cstat)  ! Tangential wind on TC R-T coordinate
  allocate(Ut_xyd(nxd,nyd),stat=cstat)  ! Radial wind on X-Y coordinate
  allocate(Vt_xyd(nxd,nyd),stat=cstat)  ! Tangential wind on X-Y coordinate
  allocate(UtP_xyd(nxd,nyd),stat=cstat)  ! Radial wind on X-Y coordinate
  allocate(VtP_xyd(nxd,nyd),stat=cstat)  ! Tangential wind on X-Y coordinate
  allocate(Utott_xyd(nxd,nyd),stat=cstat)  ! Total radial wind on X-Y coordinate
  allocate(Vtott_xyd(nxd,nyd),stat=cstat)  ! Total tangential wind on X-Y coordinate
  allocate(Vx_rt_d(nr_d,nt_d),stat=cstat)  ! X component of wind on TC R-T coordinate
  allocate(Vy_rt_d(nr_d,nt_d),stat=cstat)  ! Y component of wind on TC R-T coordinate
  allocate(VtP_rt_d(nr_d,nt_d),stat=cstat)  ! Radial wind on TC R-T coordinate
  allocate(UtP_rt_d(nr_d,nt_d),stat=cstat)  ! Tangential wind on TC R-T coordinate
  allocate(Vx_xyd(nxd,nyd),stat=cstat)  ! X component of wind on X-Y coordinate
  allocate(Vy_xyd(nxd,nyd),stat=cstat)  ! Y component of wind on X-Y coordinate
  allocate(Vtotx_xyd(nxd,nyd),stat=cstat)  ! Retrieved X component of wind on X-Y coordinate
  allocate(Vtoty_xyd(nxd,nyd),stat=cstat)  ! Retrieved Y component of wind on X-Y coordinate
  allocate(dVx_xyd(nxd,nyd),stat=cstat)  ! Vx_xyd - Vtotx_xyd
  allocate(dVy_xyd(nxd,nyd),stat=cstat)  ! Vy_xyd - Vtoty_xyd
  allocate(Vra_xyd(nxd,nyd),stat=cstat)  ! Velocity along beam on X-Y coordinate
  allocate(Vsra_xyd(nxd,nyd),stat=cstat)  ! Environmental wind velocity along beam on X-Y coordinate
  allocate(Vratot_xyd(nxd,nyd),stat=cstat)  ! Retrieved velocity along beam on X-Y coordinate
  allocate(us0(nxd,nyd),stat=cstat)  ! X-component of homogeneous wind on X-Y coordinate
  allocate(vs0(nxd,nyd),stat=cstat)  ! Y-component of homogeneous wind on X-Y coordinate
  allocate(us0_rt_d(nr_d,nt_d),stat=cstat)  ! X-component of homogeneous wind on R-T coordinate
  allocate(vs0_rt_d(nr_d,nt_d),stat=cstat)  ! Y-component of homogeneous wind on R-T coordinate
  allocate(div_rt_d(nr_d,nt_d),stat=cstat)  ! divergence on R-T coordinate
  allocate(rot_rt_d(nr_d,nt_d),stat=cstat)  ! rotation on R-T coordinate
  allocate(phin_rt_t(nrot,nr_t,nt_t),stat=cstat)  ! phin on R-T coordinate
  allocate(div_xyd(nxd,nyd),stat=cstat)  ! divergence on X-Y coordinate
  allocate(rot_xyd(nxd,nyd),stat=cstat)  ! rotation on X-Y coordinate
  allocate(phin_xyd(nrot,nxd,nyd),stat=cstat)  ! phin on X-Y coordinate
  allocate(Vt_r_d(nr_d),stat=cstat)  ! Tangential wind on R coordinate
  allocate(Ut_r_d(nr_d),stat=cstat)  ! Radial wind on R coordinate
  allocate(VtP_r_d(nr_d),stat=cstat)  ! Tangential wind on R coordinate
  allocate(UtP_r_d(nr_d),stat=cstat)  ! Radial wind on R coordinate

  !-- For drawing variables
  allocate(draw_xd(nxd),stat=cstat)
  allocate(draw_yd(nyd),stat=cstat)
  allocate(draw_Vt(nxd,nyd),stat=cstat)
  allocate(draw_Vr(nxd,nyd),stat=cstat)
  allocate(draw_Vx(nxd,nyd),stat=cstat)
  allocate(draw_Vy(nxd,nyd),stat=cstat)
  allocate(draw_Vt_ret(nxd,nyd),stat=cstat)
  allocate(draw_Vr_ret(nxd,nyd),stat=cstat)
  allocate(draw_Vx_ret(nxd,nyd),stat=cstat)
  allocate(draw_Vy_ret(nxd,nyd),stat=cstat)
  allocate(draw_dVt(nxd,nyd),stat=cstat)
  allocate(draw_dVr(nxd,nyd),stat=cstat)
  allocate(draw_dVra(nxd,nyd),stat=cstat)
  allocate(draw_dVx(nxd,nyd),stat=cstat)
  allocate(draw_dVy(nxd,nyd),stat=cstat)
  allocate(draw_Vra(nxd,nyd),stat=cstat)
  allocate(draw_Vra_ret(nxd,nyd),stat=cstat)
  allocate(draw_div(nxd,nyd),stat=cstat)
  allocate(draw_rot(nxd,nyd),stat=cstat)
  allocate(draw_phi1(nxd,nyd),stat=cstat)

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
  r_d=(/((r_dmin+dr_d*dble(i-1)),i=1,nr_d)/)
  r_t=(/((r_tmin+dr_t*dble(i-1)),i=1,nr_t+1)/)
  t_d=(/((t_dmin+dt_d*dble(i-1)),i=1,nt_d)/)
  t_t=(/((t_tmin+dt_t*dble(i-1)),i=1,nt_t)/)

  t_d=t_d*d2r
  t_t=t_t*d2r
  t_ref_d=t_d
  t_ref_t=t_t

  rh_t(1:nr_t)=r_t(1:nr_t)+0.5d0*dr_t

  !-- Note: tdr_d is located with the true center
  do j=1,nt_d
     do i=1,nr_d
        tdr_d(i,j)=datan2(((tc_yd-ra_yd)+r_d(i)*dsin(t_d(j))),  &
  &                       ((tc_xd-ra_xd)+r_d(i)*dcos(t_d(j))))
     end do
  end do
  !-- Note: tdr_t is located with the pseudo-center
  do j=1,nt_t
     do i=1,nr_t
        tdr_t(i,j)=datan2(((pseudo_tc_yd-ra_yd)+rh_t(i)*dsin(t_t(j))),  &
  &                       ((pseudo_tc_xd-ra_xd)+rh_t(i)*dcos(t_t(j))))
     end do
  end do

!-- Making relative angle to the storm center (tdr_r - thetad_tc)
  pseudo_rad_tc=dsqrt((pseudo_tc_xd-ra_xd)**2+(pseudo_tc_yd-ra_yd)**2)
  pseudo_thetad_tc=datan2((pseudo_tc_yd-ra_yd),(pseudo_tc_xd-ra_xd))
  rad_tc=dsqrt((tc_xd-ra_xd)**2+(tc_yd-ra_yd)**2)
  thetad_tc=datan2((tc_yd-ra_yd),(tc_xd-ra_xd))
  write(*,*) "thetad_tc is ", thetad_tc, pseudo_thetad_tc
  do j=1,nt_d
     do i=1,nr_d
        tdr_d(i,j)=tdr_d(i,j)-thetad_tc
     end do
  end do
  do j=1,nt_t
     do i=1,nr_t
        tdr_t(i,j)=tdr_t(i,j)-pseudo_thetad_tc
     end do
  end do
  t_d=t_ref_d-thetad_tc
  t_t=t_ref_t-pseudo_thetad_tc

!-- producing vortex profiles at vector points
  call prod_vortex_structure( r_d, t_d, rvmax, vmax, c1u, c2u,  &
  &                           Vt_rt_d, Ut_rt_d, vp(1:nvp), up(1:nup),  &
  &                           vpa(1:nvp)*d2r, upa(1:nup)*d2r, ropt=ropt, dopt=dopt,  &
  &                           Uxm=Usrn, Vym=Vsrn )  ! For retrieval

!-- Environmental wind (Us, Vs) -> Vsra(x,y)
  us0=us
  vs0=vs
  call proj_VxVy2Vraxy( xd, yd, ra_xd, ra_yd, us0, vs0, Vsra_xyd, undef=undef )
!ORG  tc_ra_r=dsqrt((tc_xd-ra_xd)**2+(tc_yd-ra_yd)**2)
!ORG  tc_ra_t=datan2((tc_yd-ra_yd),(tc_xd-ra_xd))
!MOD  Vsrn=vs*dcos(tc_ra_t)-us*dsin(tc_ra_t)
!ORG  Vsrn=vs*dcos(tc_ra_t+dasin(rh_t(1)/tc_ra_r))-us*dsin(tc_ra_t+dasin(rh_t(1)/tc_ra_r))
!  Vsrn=0.0d0

!-- Environmental wind (Us, Vs) -> Vsr(r_t,t_ref_t), Vst(r_t,t_ref_t) for only drawing
  ! (Us, Vs)(xd,yd) -> (Us, Vs)(r_t,t_ref_t) -> (Vsr,Vst)(r_t,t_ref_t)
  call tangent_conv_scal( xd, yd, tc_xd, tc_yd, us0, r_d, t_ref_d, us0_rt_d,  &
  &                       undef=undef, undefg=undef, stdopt=.true. )
  call tangent_conv_scal( xd, yd, tc_xd, tc_yd, vs0, r_d, t_ref_d, vs0_rt_d,  &
  &                       undef=undef, undefg=undef, stdopt=.true. )
  call conv_VxVy2VtVr_rt( r_d, t_ref_d, us0_rt_d, vs0_rt_d, Vst_rt_d, Usr_rt_d, undef=undef )

!-- converting (Vr,Vt)(r_t,t_ref_t) -> (Vx,Vy)(r_t,t_ref_t)
  call conv_VtVr2VxVy_rt( r_d, t_ref_d, Vt_rt_d, Ut_rt_d, Vx_rt_d, Vy_rt_d, undef=undef )
  call cart_conv_scal( r_d, t_ref_d, Vx_rt_d, xd, yd, tc_xd, tc_yd, Vx_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  call cart_conv_scal( r_d, t_ref_d, Vy_rt_d, xd, yd, tc_xd, tc_yd, Vy_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  call proj_VxVy2Vraxy( xd, yd, ra_xd, ra_yd, Vx_xyd, Vy_xyd, Vra_xyd, undef=undef )
  call subst_2d( Vra_xyd, Vsra_xyd, undef=undef )
!  call proj_VxVy2Vra( xd, yd, ra_xd, ra_yd, Um_xyd, Vm_xyd, Vmra_xyd )
!  Vra_xyd_t=Vra_xyd_t!+Vmra_xyd

  call stdout( "Projected winds.", "main", 0 )

!-- converting (r_d,t_d) -> (xd,yd)
  call subst_2d( Vt_rt_d, Vst_rt_d, undef=undef )
  call subst_2d( Ut_rt_d, Usr_rt_d, undef=undef )
  call cart_conv_scal( r_d, t_ref_d, Vt_rt_d, xd, yd, tc_xd, tc_yd, Vt_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  call cart_conv_scal( r_d, t_ref_d, Ut_rt_d, xd, yd, tc_xd, tc_yd, Ut_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )

  call stdout( "Converted r-t -> x-y.", "main", 0 )

!-- Retrieving all components of horizontal winds (VRT, VRR, VDT, and VDR) from Vd
  call proj_VtVr2Vrart( r_d, t_d, tdr_d, Vt_rt_d, Ut_rt_d, Vra_rt_d,  &
  &                     undef=undef )  ! Vra_rt = Vd - proj(Vs)
  call cart_conv_scal( r_d, t_ref_d, Vra_rt_d, xd, yd, tc_xd, tc_yd, Vra_xyd,  &
  &                    undef=undef, undefg=undef, stdopt=.true. )
  call sum_1d( Vra_rt_d(1,1:nt_d), Vra1d, undef )  ! calc. mean Vra
write(*,*) "val check", Vra1d

!-- Reallocate Vra with the true center to pseudo-center
  call tangent_conv_scal( xd, yd, pseudo_tc_xd, pseudo_tc_yd, Vra_xyd, rh_t, t_ref_t, VraP_rt_t,  &
  &                       undef=undef, undefg=undef, stdopt=.true. )

  select case (flag_GVTDX)
  case (1)  ! GVTDX
     call Retrieve_velocity_GVTDX( nrot, ndiv, rh_t, t_t, r_t, tdr_t, rdiv(1:nrdiv),  &
  &                          VraP_rt_t, Vsrn(2), pseudo_rad_tc,  &
  &                          VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                          VRTn_rt_t, VRRn_rt_t,  &
  &                          VDTm_rt_t, VDRm_rt_t, undef, phin=phin_rt_t )
  case (2)  ! GVTD
     call Retrieve_velocity_GVTD( nrot, rh_t, t_t, tdr_t, VraP_rt_T,  &
  &                               pseudo_rad_tc,  &
  &                               VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                               VRTn_rt_t, VRRn_rt_t, undef )
  case (3)  ! GBVTD
     call Retrieve_velocity_GBVTD( nrot, rh_t, t_t, tdr_t, VraP_rt_T,  &
  &                                pseudo_rad_tc,  &
  &                                VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                                VRTn_rt_t, VRRn_rt_t, undef )
  end select
  call stdout( "Retrieved velocity.", "main", 0 )

!do i=1,nr_t
!call interpo_search_1d( r_d, rh_t(i), ivmax )
!write(*,'(a6,i3,1P3E22.15)') "check ", ivmax, rh_t(i), Vt_rt_d(ivmax,1), VRT0_rt_t(i,1)
!end do
  call interpo_search_1d( rh_t, rvmax, ivmax )
!-- converting (r_t,t_ref_t) -> (xd,yd)
  call proj_VtVr2Vrart( rh_t, t_t, tdr_t, VTtot_rt_t, VRtot_rt_t, Vratot_rt_t, undef=undef )
  call cart_conv_scal( rh_t, t_ref_t, VTtot_rt_t, xd, yd, pseudo_tc_xd, pseudo_tc_yd, Vtott_xyd,  &
  &                    undef=undef, undefg=undef, stdopt=.true. )
  call cart_conv_scal( rh_t, t_ref_t, VRtot_rt_t, xd, yd, pseudo_tc_xd, pseudo_tc_yd, Utott_xyd,  &
  &                    undef=undef, undefg=undef, stdopt=.true. )
  call cart_conv_scal( rh_t, t_ref_t, Vratot_rt_t, xd, yd, pseudo_tc_xd, pseudo_tc_yd, Vratot_xyd,  &
  &                    undef=undef, undefg=undef, stdopt=.true. )

!-- converting (r_t,t_ref_t) -> (xd,yd)
  call conv_VtVr2VxVy_rt( rh_t, t_ref_t, VTtot_rt_t, VRtot_rt_t, Vxtot_rt_t, Vytot_rt_t, undef=undef )
  call cart_conv_scal( rh_t, t_ref_t, Vxtot_rt_t, xd, yd, pseudo_tc_xd, pseudo_tc_yd, Vtotx_xyd,  &
  &                    undef=undef, undefg=undef, stdopt=.true. )
  call cart_conv_scal( rh_t, t_ref_t, Vytot_rt_t, xd, yd, pseudo_tc_xd, pseudo_tc_yd, Vtoty_xyd,  &
  &                    undef=undef, undefg=undef, stdopt=.true. )

!-- calculate divergence and rotation for the retrieved VR and VT
  call div_curl_2d( r_d, t_ref_d, Ut_rt_d, Vt_rt_d, div_rt_d, rot_rt_d, undef=undef )
!  call div_curl_2d( rh_t, t_t, VRtot_rt_t, VTtot_rt_t, div_rht_t, rot_rht_t )
  call cart_conv_scal( r_d, t_ref_d, div_rt_d, xd, yd, tc_xd, tc_yd, div_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  call cart_conv_scal( r_d, t_ref_d, rot_rt_d, xd, yd, tc_xd, tc_yd, rot_xyd, undef=undef,  &
  &                    undefg=undef, stdopt=.true. )
  if(nrot>0)then
     do k=1,nrot
        call cart_conv_scal( rh_t, t_ref_t, phin_rt_t(k,1:nr_t,1:nt_t),  &
  &                          xd, yd, tc_xd, tc_yd,  &
  &                          phin_xyd(k,1:nxd,1:nyd), undef=undef,  &
  &                          undefg=undef, stdopt=.true. )
     end do
  else
     call cart_conv_scal( rh_t, t_ref_t, phin_rt_t(nrot,1:nr_t,1:nt_t),  &
  &                       xd, yd, tc_xd, tc_yd,  &
  &                       phin_xyd(nrot,1:nxd,1:nyd), undef=undef,  &
  &                       undefg=undef, stdopt=.true. )
  end if

  !-- convert E-W / N-S directions to storm-relative angle
  call rotate_thetad_tc( pseudo_thetad_tc, Vx_xyd, Vy_xyd, undef=undef )
  call rotate_thetad_tc( pseudo_thetad_tc, Vtotx_xyd, Vtoty_xyd, undef=undef )

!-- calculate the maximum difference between analysis and retrieval
  call display_2valdiff_max( Vt_xyd, Vtott_xyd, undef=undef, cout=cvtmax )
  call display_2valdiff_max( Ut_xyd, Utott_xyd, undef=undef, cout=cvrmax )
  call display_2valdiff_max( Vra_xyd, Vratot_xyd, undef=undef, cout=cvamax )
  call display_2valdiff_max( Vx_xyd, Vtotx_xyd, undef=undef, cout=cvxmax )
  call display_2valdiff_max( Vy_xyd, Vtoty_xyd, undef=undef, cout=cvymax )

!-- Check max value of VT0 and VR0
  call stand_devi( VTtot_rt_t(ivmax,1:nt_t), vmax, maxv, undef=undef )
!  call stand_devi( VRtot_rt_t(ivmax,1:nt_t), 0.0d0, maxv, undef=undef )
  write(*,'(a22,1P3E16.8,a5)') "RMSE of VTmax at RMW: ", real(maxv),  &
  &                            real(VRT0_rt_t(ivmax,1)), real(rh_t(ivmax)), "(m/s)"

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
  call conv_d2r_2d( Vx_xyd, draw_Vx )
  call conv_d2r_2d( Vy_xyd, draw_Vy )
  call conv_d2r_2d( Vtotx_xyd, draw_Vx_ret )
  call conv_d2r_2d( Vtoty_xyd, draw_Vy_ret )
  call conv_d2r_2d( div_xyd, draw_div )
  call conv_d2r_2d( rot_xyd, draw_rot )
  draw_dVt=draw_Vt_ret
  draw_dVr=draw_Vr_ret
  draw_dVra=draw_Vra_ret
  draw_dVx=draw_Vx_ret
  draw_dVy=draw_Vy_ret
  call subst_2d_r( draw_dVt, draw_Vt, undef=real(undef) )
  call subst_2d_r( draw_dVr, draw_Vr, undef=real(undef) )
  call subst_2d_r( draw_dVra, draw_Vra, undef=real(undef) )
  call subst_2d_r( draw_dVx, draw_Vx, undef=real(undef) )
  call subst_2d_r( draw_dVy, draw_Vy, undef=real(undef) )
  if(nrot>0)then
     call conv_d2r_2d( phin_xyd(1,1:nxd,1:nyd), draw_phi1 )
  else
     call conv_d2r_2d( phin_xyd(nrot,1:nxd,1:nyd), draw_phi1 )
  end if

  if(flag_fout.eqv..true.)then
     call conv_VxVy2VtVr_xy( xd, yd, pseudo_tc_xd, pseudo_tc_yd, Vx_xyd, Vy_xyd,  &
  &                          VtP_xyd, UtP_xyd, undef=undef )
     call tangent_conv_scal( xd, yd, pseudo_tc_xd, pseudo_tc_yd, VtP_xyd,  &
  &                          r_d, t_ref_d, VtP_rt_d,  &
  &                          undef=undef, undefg=undef, stdopt=.true. )
     call tangent_conv_scal( xd, yd, pseudo_tc_xd, pseudo_tc_yd, UtP_xyd,  &
  &                          r_d, t_ref_d, UtP_rt_d,  &
  &                          undef=undef, undefg=undef, stdopt=.true. )
     open(unit=100,file='test_Rankine3_out.dat',status='unknown')
     write(100,'(a80)') 'Radius          True-center-Vt  True-center-Ur  Pseu-center-Vt  Pseu-center-Ur'
     write(100,'(a80)') 'm               ms-1            ms-1            ms-1            ms-1          '
     do i=1,nr_d
        call Mean_1d( Vt_rt_d(i,1:nt_d), Vt_r_d(i), error=undef )
        call Mean_1d( Ut_rt_d(i,1:nt_d), Ut_r_d(i), error=undef )
        call Mean_1d( VtP_rt_d(i,1:nt_d), VtP_r_d(i), error=undef )
        call Mean_1d( UtP_rt_d(i,1:nt_d), UtP_r_d(i), error=undef )
        write(100,'(1P5E16.8)') r_d(i), Vt_r_d(i), Ut_r_d(i), VtP_r_d(i), UtP_r_d(i)
     end do
     close(100)
     call stdout( "Output test test_Rankine3_out.dat", "main", 0 )
  end if

write(*,*) "checkVt0", VRT0_rt_t(:,1)
write(*,*) "checkUt0", VDR0_rt_t(:,1)
!write(*,*) "Vt", draw_Vt
!write(*,*) "Vt", draw_Vt_ret
  call SGISET( 'IFONT', 1 )
  call SWLSET( 'LSYSFNT', .true. )
  CALL GLLSET( 'LMISS', .TRUE. )
  CALL GLRSET( 'RMISS', real(undef) )
  call UZFACT( 0.9 )
  call DclSetParm( 'ENABLE_CONTOUR_MESSAGE', .false. )
!  call DclSetTextIndex( 3 )
  call DclSetTextHeight( 0.03*0.75 )
!  call UDLSET( 'LABEL', .false. )  ! Drawing major label of contour

  call DclOpenGraphics(IWS)

!  CALL SWSLFT("")
  CALL SWCSET('FONTNAME', 'Nimbus Sans 12')

!-- Draw Vt from TC center

  call contourl_setting( contour_num, val_spec=fixc_val(1:contour_num+1),  &
  &                      idx_spec=fixc_idx(1:contour_num),  &
  &                      typ_spec=fixc_typ(1:contour_num),  &
  &                      formc=trim(adjustl(form_typec)) )

  call color_setting( 1, (/0.0, 1.0/), min_tab=10999,  &
  &                   max_tab=89999, col_min=10999, col_max=89999,  &
  &                   col_tab=14, reverse=col_rev,  &
  &                   val_spec=(/-1000.0,1000.0/),  &
  &                   col_spec=(/55999/) )

  call Dcl_2D_cont_shade( 'True V',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vt(1:nxd,1:nyd),  &
  &       draw_Vt(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num, shade_num/),  &
  &       no_tone=.true. )

!  do j=1,lfnum
!     call DclSetLineIndex( lidx(j) )
!     call DclDrawMarker( (/1.0/), (/1.0/) )
!     call DclDrawLine( xg(1:ngl(j),j), yg(1:ngl(j),j),  &
!  &                    index=lidx(j), type=ltyp(j) )
!  end do

!  call DclSetParm( "GRAPH:LCLIP", .false. )
!  call DclDrawMarker( (/1.0/), (/1.0/) )
!  CALL SGQVPT( vx_new(1), vx_new(2), vy_new(1), vy_new(2) )
!  call tone_bar( shade_num, (/0.0d0, 1.0/),  &
!  &              (/vx_new(2)+0.0d025, vx_new(2)+0.0d05/), vy_new,  &
!  &              trim(form_types), col_mem_num=tone_grid,  &
!  &              trigle='a', col_spec=fix_val(1:shade_num+1),  &
!  &              val_spec=fix_col(1:shade_num) )

  call DclSetParm( "GRAPH:LCLIP", .true. )

  call contourl_setting( contour_num, val_spec=fixc_val(1:contour_num+1),  &
  &                      idx_spec=fixc_idx(1:contour_num),  &
  &                      typ_spec=fixc_typ(1:contour_num),  &
  &                      formc=trim(adjustl(form_typec)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Retrieved V',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vt_ret(1:nxd,1:nyd),  &
  &       draw_dVt(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawTextNormalized( 0.82, 0.75, 'Max Diff.', centering=-1 )
  call DclDrawTextNormalized( 0.82, 0.7, trim(adjustl(cvtmax))//'(m/s)', centering=-1 )
  call tone_bar( shade_num, (/0.0, 1.0/), (/0.825,0.85/),  &
  &              (/0.2,0.5/), trim(form_types),  &
!  &              col_mem_num=tone_grid,  &
  &              col_spec=fix_val(1:shade_num+1),  &
  &              val_spec=fix_col(1:shade_num),  &
  &              dir='t', trigle='a' )
  call DclDrawTextNormalized( 0.825, 0.525, 'ΔV (m/s)', centering=-1 )
  call DclSetParm( "GRAPH:LCLIP", .true. )

!-- Draw Vr from TC center

  call contourl_setting( contour_num2, val_spec=fixc_val2(1:contour_num2+1),  &
  &                      idx_spec=fixc_idx2(1:contour_num2),  &
  &                      typ_spec=fixc_typ2(1:contour_num2),  &
  &                      formc=trim(adjustl(form_typec2)) )

  call color_setting( 1, (/0.0, 1.0/), min_tab=10999,  &
  &                   max_tab=89999, col_min=10999, col_max=89999,  &
  &                   col_tab=14, reverse=col_rev,  &
  &                   val_spec=(/-100.0,100.0/),  &
  &                   col_spec=(/55999/) )

  call Dcl_2D_cont_shade( 'True U',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vr(1:nxd,1:nyd),  &
  &       draw_Vr(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec2, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num2, shade_num/),  &
  &       no_tone=.true. )

!  do j=1,lfnum
!     call DclSetLineIndex( lidx(j) )
!     call DclDrawMarker( (/1.0/), (/1.0/) )
!     call DclDrawLine( xg(1:ngl(j),j), yg(1:ngl(j),j),  &
!  &                    index=lidx(j), type=ltyp(j) )
!  end do

!  call DclSetParm( "GRAPH:LCLIP", .false. )
!  call DclDrawMarker( (/1.0/), (/1.0/) )
!  CALL SGQVPT( vx_new(1), vx_new(2), vy_new(1), vy_new(2) )
!  call tone_bar( shade_num, (/0.0d0, 1.0/),  &
!  &              (/vx_new(2)+0.0d025, vx_new(2)+0.0d05/), vy_new,  &
!  &              trim(form_types), col_mem_num=tone_grid,  &
!  &              trigle='a', col_spec=fix_val(1:shade_num+1),  &
!  &              val_spec=fix_col(1:shade_num) )

  call DclSetParm( "GRAPH:LCLIP", .true. )

  call contourl_setting( contour_num2, val_spec=fixc_val2(1:contour_num2+1),  &
  &                      idx_spec=fixc_idx2(1:contour_num2),  &
  &                      typ_spec=fixc_typ2(1:contour_num2),  &
  &                      formc=trim(adjustl(form_typec2)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Retrieved U',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vr_ret(1:nxd,1:nyd),  &
  &       draw_dVr(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec2, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num2, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawTextNormalized( 0.82, 0.75, 'Max Diff.', centering=-1 )
  call DclDrawTextNormalized( 0.82, 0.7, trim(adjustl(cvrmax))//'(m/s)', centering=-1 )
  call tone_bar( shade_num, (/0.0, 1.0/), (/0.825,0.85/),  &
  &              (/0.2,0.5/), trim(form_types),  &
!  &              col_mem_num=tone_grid,  &
  &              col_spec=fix_val(1:shade_num+1),  &
  &              val_spec=fix_col(1:shade_num),  &
  &              dir='t', trigle='a' )
  call DclDrawTextNormalized( 0.825, 0.525, 'ΔV (m/s)', centering=-1 )
  call DclSetParm( "GRAPH:LCLIP", .true. )

!-- Draw Vt from radar

  call contourl_setting( contour_num3, val_spec=fixc_val3(1:contour_num3+1),  &
  &                      idx_spec=fixc_idx3(1:contour_num3),  &
  &                      typ_spec=fixc_typ3(1:contour_num3),  &
  &                      formc=trim(adjustl(form_typec3)) )

  call color_setting( 1, (/0.0, 1.0/), min_tab=10999,  &
  &                   max_tab=89999, col_min=10999, col_max=89999,  &
  &                   col_tab=14, reverse=col_rev,  &
  &                   val_spec=(/-1000.0,1000.0/),  &
  &                   col_spec=(/55999/) )

  call Dcl_2D_cont_shade( 'True velocity along beam',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vra(1:nxd,1:nyd),  &
  &       draw_Vra(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

!  do j=1,lfnum
!     call DclSetLineIndex( lidx(j) )
!     call DclDrawMarker( (/1.0/), (/1.0/) )
!     call DclDrawLine( xg(1:ngl(j),j), yg(1:ngl(j),j),  &
!  &                    index=lidx(j), type=ltyp(j) )
!  end do

!  call DclSetParm( "GRAPH:LCLIP", .false. )
!  call DclDrawMarker( (/1.0/), (/1.0/) )
!  CALL SGQVPT( vx_new(1), vx_new(2), vy_new(1), vy_new(2) )
!  call tone_bar( shade_num, (/0.0d0, 1.0/),  &
!  &              (/vx_new(2)+0.0d025, vx_new(2)+0.0d05/), vy_new,  &
!  &              trim(form_types), col_mem_num=tone_grid,  &
!  &              trigle='a', col_spec=fix_val(1:shade_num+1),  &
!  &              val_spec=fix_col(1:shade_num) )

  call DclSetParm( "GRAPH:LCLIP", .true. )

  call contourl_setting( contour_num3, val_spec=fixc_val3(1:contour_num3+1),  &
  &                      idx_spec=fixc_idx3(1:contour_num3),  &
  &                      typ_spec=fixc_typ3(1:contour_num3),  &
  &                      formc=trim(adjustl(form_typec3)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Retrieved velocity along beam',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vra_ret(1:nxd,1:nyd),  &
  &       draw_dVra(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawTextNormalized( 0.82, 0.75, 'Max Diff.', centering=-1 )
  call DclDrawTextNormalized( 0.82, 0.7, trim(adjustl(cvamax))//'(m/s)', centering=-1 )
  call tone_bar( shade_num, (/0.0, 1.0/), (/0.825,0.85/),  &
  &              (/0.2,0.5/), trim(form_types),  &
!  &              col_mem_num=tone_grid,  &
  &              col_spec=fix_val(1:shade_num+1),  &
  &              val_spec=fix_col(1:shade_num),  &
  &              dir='t', trigle='a' )
  call DclDrawTextNormalized( 0.825, 0.525, 'ΔV (m/s)', centering=-1 )
  call DclSetParm( "GRAPH:LCLIP", .true. )

!-- Draw Vx

  call contourl_setting( contour_num3, val_spec=fixc_val3(1:contour_num3+1),  &
  &                      idx_spec=fixc_idx3(1:contour_num3),  &
  &                      typ_spec=fixc_typ3(1:contour_num3),  &
  &                      formc=trim(adjustl(form_typec3)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Retrieved u and Δu',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vx_ret(1:nxd,1:nyd),  &
  &       draw_dVx(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawTextNormalized( 0.82, 0.75, 'Max Diff.', centering=-1 )
  call DclDrawTextNormalized( 0.82, 0.7, trim(adjustl(cvxmax))//'(m/s)', centering=-1 )
  call tone_bar( shade_num, (/0.0, 1.0/), (/0.825,0.85/),  &
  &              (/0.2,0.5/), trim(form_types),  &
!  &              col_mem_num=tone_grid,  &
  &              col_spec=fix_val(1:shade_num+1),  &
  &              val_spec=fix_col(1:shade_num),  &
  &              dir='t', trigle='a' )
  call DclDrawTextNormalized( 0.825, 0.525, 'ΔV (m/s)', centering=-1 )
  call DclSetParm( "GRAPH:LCLIP", .true. )

!-- Draw Vy

  call contourl_setting( contour_num3, val_spec=fixc_val3(1:contour_num3+1),  &
  &                      idx_spec=fixc_idx3(1:contour_num3),  &
  &                      typ_spec=fixc_typ3(1:contour_num3),  &
  &                      formc=trim(adjustl(form_typec3)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Retrieved v and Δv',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vy_ret(1:nxd,1:nyd),  &
  &       draw_dVy(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawTextNormalized( 0.82, 0.75, 'Max Diff.', centering=-1 )
  call DclDrawTextNormalized( 0.82, 0.7, trim(adjustl(cvymax))//'(m/s)', centering=-1 )
  call tone_bar( shade_num, (/0.0, 1.0/), (/0.825,0.85/),  &
  &              (/0.2,0.5/), trim(form_types),  &
!  &              col_mem_num=tone_grid,  &
  &              col_spec=fix_val(1:shade_num+1),  &
  &              val_spec=fix_col(1:shade_num),  &
  &              dir='t', trigle='a' )
  call DclDrawTextNormalized( 0.825, 0.525, 'ΔV (m/s)', centering=-1 )
  call DclSetParm( "GRAPH:LCLIP", .true. )

  contour_num3=1
  fixc_val3(1:contour_num3+1)=(/-998.0,-997.0/)
  fixc_idx3(1:1)=(/133/)
  fixc_typ3(1:1)=(/1/)
  form_typec3='(f3.0)'
  shade_num=9
  fix_val(1:shade_num+1)=(/-1.0, -0.5, -0.2, -0.1, -0.03, 0.03, 0.1, 0.2, 0.5, 1.0/)*1.0e-3
  fix_col(1:shade_num)=(/15999, 25999, 35999, 45999, 55999, 65999, 75999, 85999, 90999/)
  call contourl_setting( contour_num3, val_spec=fixc_val3(1:contour_num3+1),  &
  &                      idx_spec=fixc_idx3(1:contour_num3),  &
  &                      typ_spec=fixc_typ3(1:contour_num3),  &
  &                      formc=trim(adjustl(form_typec3)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Divergence of retrieved wind',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_div(1:nxd,1:nyd),  &
  &       draw_div(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .true. )

  call Dcl_2D_cont_shade( 'Rotation of retrieved wind',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_rot(1:nxd,1:nyd),  &
  &       draw_rot(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

!  write(*,*) "rot", draw_rot
!  write(*,*) "div", draw_div

  call DclSetParm( "GRAPH:LCLIP", .true. )

!write(*,*) "draw_phi", draw_phi1(1:nxd,nyd/2)
  contour_num3=9
  fixc_val3(1:contour_num3)=(/-20.0,-10.0,-5.0,-2.5,0.0,2.5,5.0,10.0,20.0/)*1.e4
  fixc_idx3(1:contour_num3)=(/13,13,13,13,13,13,13,13,13,13/)
  fixc_typ3(1:contour_num3)=(/2,2,2,2,1,1,1,1,1/)
  form_typec3='(1PE8.1)'
  shade_num=1
  fix_val(1:shade_num+1)=(/-1000.0, 1000.0/)*1.0e9
  fix_col(1:shade_num)=(/55999/)
  call contourl_setting( contour_num3, val_spec=fixc_val3(1:contour_num3+1),  &
  &                      idx_spec=fixc_idx3(1:contour_num3),  &
  &                      typ_spec=fixc_typ3(1:contour_num3),  &
  &                      formc=trim(adjustl(form_typec3)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Φ\_{1} in retrieved wind',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_phi1(1:nxd,1:nyd),  &
  &       draw_phi1(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'x (km)', 'y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .true. )

  call DclCloseGraphics

end program
