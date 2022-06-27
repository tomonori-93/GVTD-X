program test_Rankine
!-- A retrieval program for a 2-dim analytical vortex

  use dcl
  use Dcl_Automatic
  use Math_Const
  use typhoon_analy
  use ToRMHOWe_sub
  use ToRMHOWe_main
!  use ToRMHOWe_main2

  implicit none

  integer, parameter :: nvp_max=100

!-- namelist
  integer :: nvp, nup, nxd, nyd, nr_d, nr_t, nt_d, nt_t
  integer :: nrot, ndiv
  integer :: nx_d, ny_d, nx_t, ny_t
  integer :: IWS, tone_grid, cmap
  integer :: contour_num, contour_num2, contour_num3
  integer :: shade_num, min_tab, max_tab
  integer, dimension(80) :: fix_col
  integer, dimension(20) :: fixc_idx, fixc_typ, fixc_idx2, fixc_typ2, fixc_idx3, fixc_typ3
  real, dimension(80) :: fix_val
  real, dimension(20) :: fixc_val, fixc_val2, fixc_val3
  double precision :: us, vs
  double precision :: xdmin, xdmax, ydmin, ydmax
  double precision :: x_dmin, x_dmax, y_dmin, y_dmax
  double precision :: x_tmin, x_tmax, y_tmin, y_tmax
  double precision :: r_dmin, r_dmax, t_dmin, t_dmax
  double precision :: r_tmin, r_tmax, t_tmin, t_tmax
  double precision :: undef, xax_fact, yax_fact
  double precision :: tc_xd, tc_yd, ra_xd, ra_yd, tc_ra_r, tc_ra_t
  double precision :: rvmax, vmax, c1u, c2u
  double precision :: vp(nvp_max), up(nvp_max), vpa(nvp_max), upa(nvp_max)
  character(20) :: form_typec, form_typec2, form_typec3, form_types
  logical :: col_rev, ropt

!-- internal
  integer :: i, j, k, cstat
  double precision :: d2r, r2d, rad_tc
  double precision :: Vsrn, Vra1d, thetad_tc
  double precision, dimension(2) :: vx_new, vy_new
  double precision :: dxd, dyd, dr_d, dr_t, dt_d, dt_t, dx_d, dy_d, dx_t, dy_t
  double precision, allocatable, dimension(:) :: xd, yd, r_d, r_t, rh_t, t_d, t_t, t_ref_t, x_d, y_d, x_t, y_t
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
  double precision, allocatable, dimension(:,:) :: phi1_rt_t, phi1_xyd
  double precision, allocatable, dimension(:,:,:) :: VRTn_rt_t, VRRn_rt_t, VDTm_rt_t, VDRm_rt_t

  real :: dundef
  real, allocatable, dimension(:) :: draw_xd, draw_yd
  real, allocatable, dimension(:,:) :: draw_Vt, draw_Vr, draw_Vt_ret, draw_Vr_ret
  real, allocatable, dimension(:,:) :: draw_Vra, draw_Vra_ret, draw_rot, draw_div
  real, allocatable, dimension(:,:) :: draw_dVt, draw_dVr, draw_dVra, draw_phi1
  character(20) :: cvtmax, cvrmax, cvamax

  namelist /input /nvp, nup, undef, rvmax, vmax, c1u, c2u, vp, up, vpa, upa,  &
  &                us, vs, nrot, ndiv, ropt
  namelist /domain /nxd, nyd, nr_d, nr_t, nt_d, nt_t,  &
  &                 nx_d, ny_d, nx_t, ny_t,  &
  &                 xdmin, xdmax, ydmin, ydmax,  &
  &                 x_dmin, x_dmax, y_dmin, y_dmax,  &
  &                 x_tmin, x_tmax, y_tmin, y_tmax,  &
  &                 r_dmin, r_dmax, t_dmin, t_dmax,  &
  &                 r_tmin, r_tmax, t_tmin, t_tmax
  namelist /pos_info /tc_xd, tc_yd, ra_xd, ra_yd
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

!-- Allocate and assign variables for coordinates
  allocate(xd(nxd),stat=cstat)  ! Drawing area for x on X-Y coordinate
  allocate(yd(nyd),stat=cstat)  ! Drawing area for y on X-Y coordinate
  allocate(r_d(nr_d+1),stat=cstat)  ! Radar range on radar R-T coordinate
  allocate(r_t(nr_t+1),stat=cstat)  ! Radius on TC R-T coordinate
  allocate(t_d(nt_d),stat=cstat)  ! Radar azimuthal angle on radar R-T coordinate
  allocate(t_t(nt_t),stat=cstat)  ! Azimuthal angle on TC R-T coordinate
  allocate(t_ref_t(nt_t),stat=cstat)  ! Reference angle on TC R-T coordinate
  allocate(x_d(nx_d),stat=cstat)  ! X coordinate for each (r_d, t_d)
  allocate(y_d(ny_d),stat=cstat)  ! Y coordinate for each (r_d, t_d)
  allocate(x_t(nx_t),stat=cstat)  ! X coordinate for each (r_t, t_t)
  allocate(y_t(ny_t),stat=cstat)  ! Y coordinate for each (r_t, t_t)
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
  allocate(phi1_rt_t(nr_t+1,nt_t),stat=cstat)  ! phi1 on R-T coordinate
  allocate(div_xyd(nxd,nyd),stat=cstat)  ! divergence on X-Y coordinate
  allocate(rot_xyd(nxd,nyd),stat=cstat)  ! rotation on X-Y coordinate
  allocate(phi1_xyd(nxd,nyd),stat=cstat)  ! phi1 on X-Y coordinate

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
!  dx_d=(x_dmax-x_dmin)/dble(nx_d-1)
!  dy_d=(y_dmax-y_dmin)/dble(ny_d-1)
!  dx_t=(x_tmax-x_tmin)/dble(nx_t-1)
!  dy_t=(y_tmax-y_tmin)/dble(ny_t-1)

  xd=(/((xdmin+dxd*dble(i-1)),i=1,nxd)/)
  yd=(/((ydmin+dyd*dble(i-1)),i=1,nyd)/)
  r_d=(/((r_dmin+dr_d*dble(i-1)),i=1,nr_d+1)/)
  r_t=(/((r_tmin+dr_t*dble(i-1)),i=1,nr_t+1)/)
  t_d=(/((t_dmin+dt_d*dble(i-1)),i=1,nt_d)/)
  t_t=(/((t_tmin+dt_t*dble(i-1)),i=1,nt_t)/)
!  x_d=(/((x_dmin+dx_d*dble(i-1)),i=1,nx_d)/)
!  y_d=(/((y_dmin+dy_d*dble(i-1)),i=1,ny_d)/)
!  x_t=(/((x_tmin+dx_t*dble(i-1)),i=1,nx_t)/)
!  y_t=(/((y_tmin+dy_t*dble(i-1)),i=1,ny_t)/)

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
  &                           vpa(1:nvp)*d2r, upa(1:nup)*d2r, ropt=ropt )

!-- Environmental wind (Us, Vs) -> Vra(r_t,t_t), Vrn(r_t,t_t)
  us0=us
  vs0=vs
  call proj_VxVy2Vraxy( xd, yd, ra_xd, ra_yd, us0, vs0, Vsra_xyd )
  call tangent_conv_scal( xd, yd, tc_xd, tc_yd, Vsra_xyd, rh_t, t_ref_t, Vsra_rt_t,  &
  &                       undef=undef, undefg=undef, undefgc='inc',  &
  &                       stdopt=.true., axis='xy' )
!ORG  tc_ra_r=dsqrt((tc_xd-ra_xd)**2+(tc_yd-ra_yd)**2)
!ORG  tc_ra_t=datan2((tc_yd-ra_yd),(tc_xd-ra_xd))
!MOD  Vsrn=vs*dcos(tc_ra_t)-us*dsin(tc_ra_t)
!ORG  Vsrn=vs*dcos(tc_ra_t+dasin(rh_t(1)/tc_ra_r))-us*dsin(tc_ra_t+dasin(rh_t(1)/tc_ra_r))
  Vsrn=0.0d0

!-- Environmental wind (Us, Vs) -> Vsr(r_t,t_ref_t), Vst(r_t,t_ref_t) for only drawing
  ! (Us, Vs)(xd,yd) -> (Us, Vs)(r_t,t_ref_t) -> (Vsr,Vst)(r_t,t_ref_t)
  call tangent_conv_scal( xd, yd, tc_xd, tc_yd, us0, rh_t, t_ref_t, us0_rht_t,  &
  &                       undef=undef, undefg=undef, undefgc='inc',  &
  &                       stdopt=.true., axis='xy' )
  call tangent_conv_scal( xd, yd, tc_xd, tc_yd, vs0, rh_t, t_ref_t, vs0_rht_t,  &
  &                       undef=undef, undefg=undef, undefgc='inc',  &
  &                       stdopt=.true., axis='xy' )
  call conv_VxVy2VtVr( rh_t, t_ref_t, us0_rht_t, vs0_rht_t, Vst_rht_t, Usr_rht_t )

!-- converting (Vr,Vt)(r_t,t_ref_t) -> (Vx,Vy)(r_t,t_ref_t)
  call conv_VtVr2VxVy( rh_t, t_ref_t, Vt_rht_t, Ut_rht_t, Vx_rht_t, Vy_rht_t )
  call Cart_conv_scal( rh_t, t_ref_t, Vx_rht_t, xd, yd, tc_xd, tc_yd, Vx_xyd_t, undef=undef,  &
  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )
  call Cart_conv_scal( rh_t, t_ref_t, Vy_rht_t, xd, yd, tc_xd, tc_yd, Vy_xyd_t, undef=undef,  &
  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )
  call proj_VxVy2Vraxy( xd, yd, ra_xd, ra_yd, Vx_xyd_t, Vy_xyd_t, Vra_xyd, undef=undef )
  call proj_VtVr2Vrart( rh_t, t_t, tdr_t, Vt_rht_t, Ut_rht_t, Vra_rt_t, undef=undef )
  call subst_2d( Vra_xyd, Vsra_xyd, undef=undef )
!  call proj_VxVy2Vra( xd, yd, ra_xd, ra_yd, Um_xyd, Vm_xyd, Vmra_xyd )
!  Vra_xyd_t=Vra_xyd_t!+Vmra_xyd

  call stdout( "Projected winds.", "main", 0 )

!-- converting (r_t,t_t) -> (xd,yd)
  call subst_2d( Vt_rht_t, Vst_rht_t, undef=undef )
  call subst_2d( Ut_rht_t, Usr_rht_t, undef=undef )
  call Cart_conv_scal( rh_t, t_ref_t, Vt_rht_t, xd, yd, tc_xd, tc_yd, Vt_xyd, undef=undef,  &
  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )
  call Cart_conv_scal( rh_t, t_ref_t, Ut_rht_t, xd, yd, tc_xd, tc_yd, Ut_xyd, undef=undef,  &
  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )

  call stdout( "Converted r-t -> x-y.", "main", 0 )

!-- Retrieving all components of horizontal winds (VRT, VRR, VDT, and VDR) from Vd
  call subst_2d( Vra_rt_t, Vsra_rt_t, undef=undef )  ! Vd - proj(Vs)
  call sum_1d( Vra_rt_t(1,1:nt_t), Vra1d, undef )  ! calc. mean Vra
write(*,*) "val check", Vra1d
  !call Retrieve_velocity2( nrot, ndiv, rh_t, t_t, r_t, tdr_t, Vra_rt_t,  &
  call Retrieve_velocity( nrot, ndiv, rh_t, t_t, r_t, tdr_t, Vra_rt_t,  &
  &                       (/Vsrn,0.0d0/), (/0.0d0,0.0d0/), rad_tc,  &
  &                       VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t, VRTn_rt_t, VRRn_rt_t,  &
  &                       VDTm_rt_t, VDRm_rt_t, undef, phi1=phi1_rt_t )
  call stdout( "Retrieved velocity.", "main", 0 )

do i=1,nr_t
write(*,'(a6,1P3E22.15)') "check ", rh_t(i), Vt_rht_t(i,1), VRT0_rt_t(i,1)
end do
!-- converting (r_t,t_ref_t) -> (xd,yd)
  call proj_VtVr2Vrart( rh_t, t_t, tdr_t, VTtot_rt_t, VRtot_rt_t, Vratot_rt_t, undef=undef )
  call Cart_conv_scal( rh_t, t_ref_t, VTtot_rt_t, xd, yd, tc_xd, tc_yd, Vtott_xyd, undef=undef,  &
  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )
  call Cart_conv_scal( rh_t, t_ref_t, VRtot_rt_t, xd, yd, tc_xd, tc_yd, Utott_xyd, undef=undef,  &
  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )
  call Cart_conv_scal( rh_t, t_ref_t, Vratot_rt_t, xd, yd, tc_xd, tc_yd, Vratot_xyd, undef=undef,  &
  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )

!-- calculate divergence and rotation for the retrieved VR and VT
  call div_curl_2d( rh_t, t_ref_t, Ut_rht_t, Vt_rht_t, div_rht_t, rot_rht_t )
!  call div_curl_2d( rh_t, t_t, VRtot_rt_t, VTtot_rt_t, div_rht_t, rot_rht_t )
  call Cart_conv_scal( rh_t, t_ref_t, div_rht_t, xd, yd, tc_xd, tc_yd, div_xyd, undef=undef,  &
  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )
  call Cart_conv_scal( rh_t, t_ref_t, rot_rht_t, xd, yd, tc_xd, tc_yd, rot_xyd, undef=undef,  &
  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )
  call Cart_conv_scal( r_t, t_ref_t, phi1_rt_t, xd, yd, tc_xd, tc_yd, phi1_xyd, undef=undef,  &
  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )

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
  call conv_d2r_2d( phi1_xyd, draw_phi1 )

write(*,*) "checkVt0", VRT0_rt_t(:,1)
write(*,*) "checkUt0", VDR0_rt_t(:,1)
!write(*,*) "Vt", draw_Vt
!write(*,*) "Vt", draw_Vt_ret
  call SGISET( 'IFONT', 1 )
  call SWLSET( 'LSYSFNT', .true. )
  CALL GLLSET( 'LMISS', .TRUE. )
  CALL GLRSET( 'RMISS', real(undef) )
  call UZFACT(0.9)
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

  call Dcl_2D_cont_shade( 'Vt for Analysis',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vt(1:nxd,1:nyd),  &
  &       draw_Vt(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
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

  call Dcl_2D_cont_shade( 'Vt for Retrieval',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vt_ret(1:nxd,1:nyd),  &
  &       draw_dVt(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
  &       (/form_typec, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawTextNormalized( 0.82, 0.75, 'Max Diff.', centering=-1 )
  call DclDrawTextNormalized( 0.82, 0.7, trim(adjustl(cvtmax))//'[m/s]', centering=-1 )
  call tone_bar( shade_num, (/0.0, 1.0/), (/0.825,0.85/),  &
  &              (/0.2,0.5/), trim(form_types),  &
!  &              col_mem_num=tone_grid,  &
  &              col_spec=fix_val(1:shade_num+1),  &
  &              val_spec=fix_col(1:shade_num),  &
  &              dir='t', trigle='a' )
  call DclDrawTextNormalized( 0.825, 0.525, 'ΔV [m/s]', centering=-1 )
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

  call Dcl_2D_cont_shade( 'Vr for Analysis',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vr(1:nxd,1:nyd),  &
  &       draw_Vr(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
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

  call Dcl_2D_cont_shade( 'Vr for Retrieval',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vr_ret(1:nxd,1:nyd),  &
  &       draw_dVr(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
  &       (/form_typec2, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num2, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawTextNormalized( 0.82, 0.75, 'Max Diff.', centering=-1 )
  call DclDrawTextNormalized( 0.82, 0.7, trim(adjustl(cvrmax))//'[m/s]', centering=-1 )
  call tone_bar( shade_num, (/0.0, 1.0/), (/0.825,0.85/),  &
  &              (/0.2,0.5/), trim(form_types),  &
!  &              col_mem_num=tone_grid,  &
  &              col_spec=fix_val(1:shade_num+1),  &
  &              val_spec=fix_col(1:shade_num),  &
  &              dir='t', trigle='a' )
  call DclDrawTextNormalized( 0.825, 0.525, 'ΔV [m/s]', centering=-1 )
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

  call Dcl_2D_cont_shade( 'Velocity along beam for Analysis',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vra(1:nxd,1:nyd),  &
  &       draw_Vra(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
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

  call Dcl_2D_cont_shade( 'Velocity along beam for Retrieval',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_Vra_ret(1:nxd,1:nyd),  &
  &       draw_dVra(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawTextNormalized( 0.82, 0.75, 'Max Diff.', centering=-1 )
  call DclDrawTextNormalized( 0.82, 0.7, trim(adjustl(cvamax))//'[m/s]', centering=-1 )
  call tone_bar( shade_num, (/0.0, 1.0/), (/0.825,0.85/),  &
  &              (/0.2,0.5/), trim(form_types),  &
!  &              col_mem_num=tone_grid,  &
  &              col_spec=fix_val(1:shade_num+1),  &
  &              val_spec=fix_col(1:shade_num),  &
  &              dir='t', trigle='a' )
  call DclDrawTextNormalized( 0.825, 0.525, 'ΔV [m/s]', centering=-1 )
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
  &       (/'X (km)', 'Y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .true. )

  call Dcl_2D_cont_shade( 'Rotation of retrieved wind',  &
  &       draw_xd(1:nxd), draw_yd(1:nyd),  &
  &       draw_rot(1:nxd,1:nyd),  &
  &       draw_rot(1:nxd,1:nyd),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
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
  &       (/'X (km)', 'Y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .true. )

  call DclCloseGraphics

end program
