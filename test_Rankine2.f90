program test_Rankine
!-- Lee et al. (1999) で与えられる解析的な台風渦分布を可視化するプログラム

  use dcl
  use Dcl_Automatic
  use Math_Const
  use typhoon_analy
  use sub_mod
  use main_mod

  implicit none

  integer, parameter :: nvp_max=100

!-- namelist
  integer :: nvp, nup, nr_d, nr_t, nt_d, nt_t
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
  double precision :: d2r, r2d
  double precision :: Vsrn
  double precision, dimension(2) :: vx_new, vy_new
  double precision :: dxd, dyd, dr_d, dr_t, dt_d, dt_t, dx_d, dy_d, dx_t, dy_t
  double precision, allocatable, dimension(:) :: xd, yd, r_d, r_t, rh_t, t_d, t_t, x_d, y_d, x_t, y_t
  double precision, allocatable, dimension(:,:) :: tdr_t
  double precision, allocatable, dimension(:,:) :: Vd_rt_d
  double precision, allocatable, dimension(:,:) :: Vra_rt_t, Vratot_rt_t, Vsra_rt_t
  double precision, allocatable, dimension(:,:) :: Ut_rht_t, Vt_rht_t
  double precision, allocatable, dimension(:,:) :: Vx_rht_t, Vy_rht_t
  double precision, allocatable, dimension(:,:) :: VRT0_rt_t, VDR0_rt_t, VTtot_rt_t, VRtot_rt_t
  double precision, allocatable, dimension(:,:) :: us0, vs0, Vsra_xyd
  double precision, allocatable, dimension(:,:,:) :: VRTn_rt_t, VRRn_rt_t, VDTm_rt_t, VDRm_rt_t

  real :: dundef
  real, allocatable, dimension(:) :: draw_rd, draw_td
  real, allocatable, dimension(:,:) :: draw_Vt, draw_Vr, draw_Vt_ret, draw_Vr_ret

  namelist /input /nvp, nup, undef, rvmax, vmax, c1u, c2u, vp, up, vpa, upa,  &
  &                us, vs, nrot, ndiv, ropt
  namelist /domain /nr_d, nr_t, nt_d, nt_t,  &
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
  allocate(r_d(nr_d+1),stat=cstat)  ! Radar range on radar R-T coordinate
  allocate(r_t(nr_t+1),stat=cstat)  ! Radius on TC R-T coordinate
  allocate(t_d(nt_d),stat=cstat)  ! Radar azimuthal angle on radar R-T coordinate
  allocate(t_t(nt_t),stat=cstat)  ! Azimuthal angle on TC R-T coordinate
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
  allocate(Vra_rt_t(nr_t,nt_t),stat=cstat)  ! Velocity along beam on TC R-T coordinate
  allocate(Vsra_rt_t(nr_t,nt_t),stat=cstat)  ! Environmental wind velocity along beam on TC R-T coordinate
  allocate(Vratot_rt_t(nr_t,nt_t),stat=cstat)  ! Velocity along beam on TC R-T coordinate
  allocate(us0(nxd,nyd),stat=cstat)  ! X-component of homogeneous wind on X-Y coordinate
  allocate(vs0(nxd,nyd),stat=cstat)  ! Y-component of homogeneous wind on X-Y coordinate
  allocate(Vsra_xyd(nxd,nyd),stat=cstat)  ! Environmental wind velocity along beam on X-Y coordinate
  allocate(Vx_rht_t(nr_t,nt_t),stat=cstat)  ! X component of wind on TC R-T coordinate
  allocate(Vy_rht_t(nr_t,nt_t),stat=cstat)  ! Y component of wind on TC R-T coordinate

  !-- For drawing variables
  allocate(draw_rd(nr_t),stat=cstat)
  allocate(draw_td(nt_t),stat=cstat)
  allocate(draw_Vt(nr_t,nt_t),stat=cstat)
  allocate(draw_Vr(nr_t,nt_t),stat=cstat)
  allocate(draw_Vt_ret(nr_t,nt_t),stat=cstat)
  allocate(draw_Vr_ret(nr_t,nt_t),stat=cstat)

  if(cstat/=0)then
     call stdout( "Failed to allocate variables. stop.", "main", -1 )
     stop
  else
     call stdout( "Allocated variables.", "main", 0 )
  end if


!  dxd=(xdmax-xdmin)/dble(nxd-1)
!  dyd=(ydmax-ydmin)/dble(nyd-1)
  dr_d=(r_dmax-r_dmin)/dble(nr_d-1)
  dr_t=(r_tmax-r_tmin)/dble(nr_t-1)
  dt_d=(t_dmax-t_dmin)/dble(nt_d-1)
  dt_t=(t_tmax-t_tmin)/dble(nt_t-1)
!  dx_d=(x_dmax-x_dmin)/dble(nx_d-1)
!  dy_d=(y_dmax-y_dmin)/dble(ny_d-1)
!  dx_t=(x_tmax-x_tmin)/dble(nx_t-1)
!  dy_t=(y_tmax-y_tmin)/dble(ny_t-1)

!  xd=(/((xdmin+dxd*dble(i-1)),i=1,nxd)/)
!  yd=(/((ydmin+dyd*dble(i-1)),i=1,nyd)/)
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

  rh_t(1:nr_t)=r_t(1:nr_t)+0.5d0*dr_t
  do j=1,nt_t
     do i=1,nr_t
        tdr_t(i,j)=datan2(((tc_yd-ra_yd)+rh_t(i)*dsin(t_t(j))),((tc_xd-ra_xd)+rh_t(i)*dcos(t_t(j))))
     end do
  end do

  do k=1,nt_t
!-- producing vortex profiles at vector points 
     call prod_vortex_structure( rh_t, t_t, rvmax, vmax, c1u, c2u,  &
  &                              Vt_rht_t, Ut_rht_t, vp(1:nvp), up(1:nup),  &
  &                              vpa(1:nvp)*d2r+t_t(k),  &
  &                              upa(1:nup)*d2r+t_t(k), ropt=ropt )

!-- Environmental wind
  us0=us
  vs0=vs
  call proj_VxVy2Vraxy( xd, yd, ra_xd, ra_yd, us0, vs0, Vsra_xyd )
  call tangent_conv_scal( xd, yd, tc_xd, tc_yd, Vsra_xyd, rh_t, t_t, Vsra_rt_t,  &
  &                       undef=undef, undefg=undef, undefgc='inc',  &
  &                       stdopt=.true., axis='xy' )
!ORG  tc_ra_r=dsqrt((tc_xd-ra_xd)**2+(tc_yd-ra_yd)**2)
!ORG  tc_ra_t=datan2((tc_yd-ra_yd),(tc_xd-ra_xd))
!MOD  Vsrn=vs*dcos(tc_ra_t)-us*dsin(tc_ra_t)
!ORG  Vsrn=vs*dcos(tc_ra_t+dasin(rh_t(1)/tc_ra_r))-us*dsin(tc_ra_t+dasin(rh_t(1)/tc_ra_r))
  Vsrn=0.0d0

!-- converting (Vr,Vt)(r_t,t_t) -> (Vx,Vy)(r_t,t_t)
!  call conv_VtVr2VxVy( rh_t, t_t, Vt_rht_t, Ut_rht_t, Vx_rht_t, Vy_rht_t )
!  call Cart_conv_scal( rh_t, t_t, Vx_rht_t, xd, yd, tc_xd, tc_yd, Vx_xyd_t, undef=undef,  &
!  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )
!  call Cart_conv_scal( rh_t, t_t, Vy_rht_t, xd, yd, tc_xd, tc_yd, Vy_xyd_t, undef=undef,  &
!  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )
!  call proj_VxVy2Vraxy( xd, yd, ra_xd, ra_yd, Vx_xyd_t, Vy_xyd_t, Vra_xyd, undef=undef )
     call proj_VtVr2Vrart( rh_t, t_t, tdr_t, Vt_rht_t, Ut_rht_t, Vra_rt_t, undef=undef )
!  call proj_VxVy2Vra( xd, yd, ra_xd, ra_yd, Um_xyd, Vm_xyd, Vmra_xyd )
!  Vra_xyd_t=Vra_xyd_t!+Vmra_xyd

     call stdout( "Projected winds.", "main", 0 )

!!-- converting (r_t,t_t) -> (xd,yd)
!  call Cart_conv_scal( rh_t, t_t, Vt_rht_t, xd, yd, tc_xd, tc_yd, Vt_xyd, undef=undef,  &
!  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )
!  call Cart_conv_scal( rh_t, t_t, Ut_rht_t, xd, yd, tc_xd, tc_yd, Ut_xyd, undef=undef,  &
!  &                    undefg=undef, undefgc='inc', stdopt=.true., axis='xy' )

!  call stdout( "Converted r-t -> x-y.", "main", 0 )

!-- Retrieving all components of horizontal winds (VRT, VRR, VDT, and VDR) from Vd
     call subst_2d( Vra_rt_t, Vsra_rt_t, undef=undef )  ! Vd - proj(Vs)
     call Retrieve_velocity( nrot, ndiv, rh_t, t_t, r_t, tdr_t, Vra_rt_t, Vsrn,  &
  &                          VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                          VRTn_rt_t, VRRn_rt_t,  &
  &                          VDTm_rt_t, VDRm_rt_t, undef )
     call stdout( "Retrieved velocity.", "main", 0 )

     call conv_d2r_2d( VRT0_rt_t(1:nr_t,1:1), draw_Vt(1:nr_t,k:k) )
     call conv_d2r_2d( VDR0_rt_t(1:nr_t,1:1), draw_Vr(1:nr_t,k:k) )
     call conv_d2r_2d( VRT0_rt_t(1:nr_t,1:1), draw_Vt_ret(1:nr_t,k:k) )
     call conv_d2r_2d( VDR0_rt_t(1:nr_t,1:1), draw_Vr_ret(1:nr_t,k:k) )

  end do

!-- DCL drawing
  call conv_d2r_1d( rh_t, draw_rd )
  call conv_d2r_1d( t_t, draw_td )
  draw_rd=draw_rd*real(xax_fact)
  draw_td=draw_td*real(yax_fact)

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

  CALL SWCSET('FONTNAME', 'Nimbus Sans L 12')

!-- Draw Vt from TC center

  call contourl_setting( contour_num, val_spec=fixc_val(1:contour_num+1),  &
  &                      idx_spec=fixc_idx(1:contour_num),  &
  &                      typ_spec=fixc_typ(1:contour_num),  &
  &                      formc=trim(adjustl(form_typec)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Vt for Analysis',  &
  &       draw_rd(1:nr_t), draw_td(1:nt_t),  &
  &       draw_Vt(1:nr_t,1:nt_t),  &
  &       draw_Vt(1:nr_t,1:nt_t),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'R (km)   ', 'θ (\^{o})'/),  &
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
  &       draw_rd(1:nr_t), draw_td(1:nt_t),  &
  &       draw_Vt_ret(1:nr_t,1:nt_t),  &
  &       draw_Vt_ret(1:nr_t,1:nt_t),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'R (km)   ', 'θ (\^{o})'/),  &
  &       (/form_typec, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num, shade_num/),  &
  &       no_tone=.true. )

!-- Draw Vr from TC center

  call contourl_setting( contour_num2, val_spec=fixc_val2(1:contour_num2+1),  &
  &                      idx_spec=fixc_idx2(1:contour_num2),  &
  &                      typ_spec=fixc_typ2(1:contour_num2),  &
  &                      formc=trim(adjustl(form_typec2)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Vr for Analysis',  &
  &       draw_rd(1:nr_t), draw_td(1:nt_t),  &
  &       draw_Vr(1:nr_t,1:nt_t),  &
  &       draw_Vr(1:nr_t,1:nt_t),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'R (km)   ', 'θ (\^{o})'/),  &
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
  &       draw_rd(1:nr_t), draw_td(1:nt_t),  &
  &       draw_Vr_ret(1:nr_t,1:nt_t),  &
  &       draw_Vr_ret(1:nr_t,1:nt_t),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'R (km)   ', 'θ (\^{o})'/),  &
  &       (/form_typec2, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num2, shade_num/),  &
  &       no_tone=.true. )

  call DclCloseGraphics

end program
