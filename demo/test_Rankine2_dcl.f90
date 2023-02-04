program test_Rankine
!-- Test the dependency of azimuthal phase for asymmetric components in an analytical vortex

  use dcl
  use Dcl_Automatic
  use GVTDX_sub
  use GVTDX_main
  use GVTD_main
  use GBVTD_main

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
  double precision, allocatable, dimension(:,:) :: Vt_0, Vr_0
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
  real, allocatable, dimension(:,:) :: draw_dVt, draw_dVr, draw_Vt, draw_Vr, draw_Vt_ret, draw_Vr_ret
  character(20) :: cvtmax, cvrmax, cvamax

  namelist /input /nvp, nup, undef, rvmax, vmax, c1u, c2u, vp, up, vpa, upa,  &
  &                us, vs, nrot, ndiv, ropt, nrdiv, rdiv, flag_GVTDX
  namelist /domain /nxd, nyd, nr_d, nr_t, nt_d, nt_t,  &
  &                 xdmin, xdmax, ydmin, ydmax,  &
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
  allocate(Vra_rt_t(nr_t,nt_t),stat=cstat)  ! Velocity along beam on TC R-T coordinate
  allocate(Vsra_rt_t(nr_t,nt_t),stat=cstat)  ! Environmental wind velocity along beam on TC R-T coordinate
  allocate(Vratot_rt_t(nr_t,nt_t),stat=cstat)  ! Velocity along beam on TC R-T coordinate
  allocate(us0(nxd,nyd),stat=cstat)  ! X-component of homogeneous wind on X-Y coordinate
  allocate(vs0(nxd,nyd),stat=cstat)  ! Y-component of homogeneous wind on X-Y coordinate
  allocate(Vsra_xyd(nxd,nyd),stat=cstat)  ! Environmental wind velocity along beam on X-Y coordinate
  allocate(Vx_rht_t(nr_t,nt_t),stat=cstat)  ! X component of wind on TC R-T coordinate
  allocate(Vy_rht_t(nr_t,nt_t),stat=cstat)  ! Y component of wind on TC R-T coordinate
  allocate(Vt_0(nr_t,1),stat=cstat)  ! Axisymmetric component of Vt
  allocate(Vr_0(nr_t,1),stat=cstat)  ! Axisymmetric component of Vr

  !-- For drawing variables
  allocate(draw_rd(nr_t),stat=cstat)
  allocate(draw_td(nt_t),stat=cstat)
  allocate(draw_dVt(nr_t,nt_t),stat=cstat)
  allocate(draw_dVr(nr_t,nt_t),stat=cstat)
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

  do k=1,nt_t
!-- producing vortex profiles at vector points 
     call prod_vortex_structure( rh_t, t_ref_t, rvmax, vmax, c1u, c2u,  &
  &                              Vt_rht_t, Ut_rht_t, vp(1:nvp), up(1:nup),  &
  &                              vpa(1:nvp)*d2r+t_t(k),  &
  &                              upa(1:nup)*d2r+t_t(k), ropt=ropt,  &
  &                              Vt_0=Vt_0(1:nr_t,1), Vr_0=Vr_0(1:nr_t,1) )

!-- Environmental wind
     us0=us
     vs0=vs
     call proj_VxVy2Vraxy( xd, yd, ra_xd, ra_yd, us0, vs0, Vsra_xyd, undef=undef )
     call tangent_conv_scal( xd, yd, tc_xd, tc_yd, Vsra_xyd, rh_t, t_ref_t, Vsra_rt_t,  &
  &                          undef=undef, undefg=undef,  &
  &                          stdopt=.true. )
     Vsrn=0.0d0

!-- converting (Vr,Vt)(r_t,t_t) -> (Vx,Vy)(r_t,t_t)
     call proj_VtVr2Vrart( rh_t, t_t, tdr_t, Vt_rht_t, Ut_rht_t, Vra_rt_t, undef=undef )

     call stdout( "Projected winds.", "main", 0 )

!-- Retrieving all components of horizontal winds (VRT, VRR, VDT, and VDR) from Vd
     call subst_2d( Vra_rt_t, Vsra_rt_t, undef=undef )  ! Vd - proj(Vs)

     select case (flag_GVTDX)
     case (1)  ! GVTDX
        call Retrieve_velocity_GVTDX( nrot, ndiv, rh_t, t_t, r_t, tdr_t,  &
  &                                   rdiv(1:nrdiv), Vra_rt_t,  &
  &                                   0.0d0, rad_tc,  &
  &                                   VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                                   VRTn_rt_t, VRRn_rt_t,  &
  &                                   VDTm_rt_t, VDRm_rt_t, undef )
     case (2)  ! GVTD
        call Retrieve_velocity_GVTD( nrot, rh_t, t_t, tdr_t, Vra_rt_t,  &
  &                                  rad_tc,  &
  &                                  VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                                  VRTn_rt_t, VRRn_rt_t, undef )
     case (3)  ! GBVTD
        call Retrieve_velocity_GBVTD( nrot, rh_t, t_t, tdr_t, Vra_rt_t,  &
  &                                   rad_tc,  &
  &                                   VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                                   VRTn_rt_t, VRRn_rt_t, undef )
     end select
     call stdout( "Retrieved velocity.", "main", 0 )

     call conv_d2r_2d( VRT0_rt_t(1:nr_t,1:1), draw_Vt_ret(1:nr_t,k:k) )
     call conv_d2r_2d( VDR0_rt_t(1:nr_t,1:1), draw_Vr_ret(1:nr_t,k:k) )
     call conv_d2r_2d( Vt_0(1:nr_t,1:1), draw_Vt(1:nr_t,k:k) )
     call conv_d2r_2d( Vr_0(1:nr_t,1:1), draw_Vr(1:nr_t,k:k) )
     draw_dVt(1:nr_t,k:k)=draw_Vt_ret(1:nr_t,k:k)
     draw_dVr(1:nr_t,k:k)=draw_Vr_ret(1:nr_t,k:k)
     call subst_2d_r( draw_dVt(1:nr_t,k:k), draw_Vt(1:nr_t,1:1), undef=real(undef) )  ! draw_Vt_ret - draw_Vt
     call subst_2d_r( draw_dVr(1:nr_t,k:k), draw_Vr(1:nr_t,1:1), undef=real(undef) )  ! draw_Vr_ret - draw_Vr

  end do

!-- calculate the maximum difference between analysis and retrieval
  call display_1val_max( draw_dVt, undef=real(undef), cout=cvtmax )
  call display_1val_max( draw_dVr, undef=real(undef), cout=cvrmax )

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

  CALL SWCSET('FONTNAME', 'Nimbus Sans 12')

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

  call Dcl_2D_cont_shade( 'Retrieved V\_{0}',  &
  &       draw_rd(1:nr_t), draw_td(1:nt_t),  &
  &       draw_Vt_ret(1:nr_t,1:nt_t),  &
  &       draw_dVt(1:nr_t,1:nt_t),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'r (km)         ', 'θ\_{0} (\^{o})'/),  &
  &       (/form_typec, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num, shade_num/),  &
  &       no_tone=.true. )

!  do j=1,lfnum
!     call DclSetLineIndex( lidx(j) )
!     call DclDrawMarker( (/1.0/), (/1.0/) )
!     call DclDrawLine( xg(1:ngl(j),j), yg(1:ngl(j),j),  &
!  &                    index=lidx(j), type=ltyp(j) )
!  end do

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawTextNormalized( 0.82, 0.75, 'Max Diff.', centering=-1 )
  call DclDrawTextNormalized( 0.82, 0.7, trim(adjustl(cvtmax))//'(m/s)', centering=-1 )
  call tone_bar( shade_num, (/0.0, 1.0/), (/0.825,0.85/),  &
  &              (/0.2,0.5/), trim(form_types),  &
!  &              col_mem_num=tone_grid,  &
  &              col_spec=fix_val(1:shade_num+1),  &
  &              val_spec=fix_col(1:shade_num),  &
  &              dir='t', trigle='a' )
  call DclDrawTextNormalized( 0.825, 0.525, 'ΔV\_{0} (m/s)', centering=-1 )

  call DclSetParm( "GRAPH:LCLIP", .true. )

!  call contourl_setting( contour_num2, val_spec=fixc_val2(1:contour_num+1),  &
!  &                      idx_spec=fixc_idx2(1:contour_num),  &
!  &                      typ_spec=fixc_typ2(1:contour_num),  &
!  &                      formc=trim(adjustl(form_typec2)) )

!  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
!  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
!  &                   col_tab=cmap, reverse=col_rev,  &
!  &                   val_spec=fix_val(1:shade_num+1),  &
!  &                   col_spec=fix_col(1:shade_num) )

!  call Dcl_2D_cont_shade( 'ΔV\_{0} (retrieval - truth) ',  &
!  &       draw_rd(1:nr_t), draw_td(1:nt_t),  &
!  &       draw_dVt(1:nr_t,1:nt_t),  &
!  &       draw_dVt(1:nr_t,1:nt_t),  &
!  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
!  &       (/'r (km)   ', 'θ (\^{o})'/),  &
!  &       (/form_typec, form_types/), (/0.2, 0.8/),  &
!  &       (/0.2, 0.8/), c_num=(/contour_num2, shade_num/),  &
!  &       no_tone=.true. )

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

  call Dcl_2D_cont_shade( 'Retrieved U\_{0}',  &
  &       draw_rd(1:nr_t), draw_td(1:nt_t),  &
  &       draw_Vr_ret(1:nr_t,1:nt_t),  &
  &       draw_dVr(1:nr_t,1:nt_t),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'r (km)    ', 'θ (\^{o})'/),  &
  &       (/form_typec2, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num2, shade_num/),  &
  &       no_tone=.true. )

!  do j=1,lfnum
!     call DclSetLineIndex( lidx(j) )
!     call DclDrawMarker( (/1.0/), (/1.0/) )
!     call DclDrawLine( xg(1:ngl(j),j), yg(1:ngl(j),j),  &
!  &                    index=lidx(j), type=ltyp(j) )
!  end do

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawTextNormalized( 0.82, 0.75, 'Max Diff.', centering=-1 )
  call DclDrawTextNormalized( 0.82, 0.7, trim(adjustl(cvrmax))//'(m/s)', centering=-1 )
  call tone_bar( shade_num, (/0.0, 1.0/), (/0.825,0.85/),  &
  &              (/0.2,0.5/), trim(form_types),  &
!  &              col_mem_num=tone_grid,  &
  &              col_spec=fix_val(1:shade_num+1),  &
  &              val_spec=fix_col(1:shade_num),  &
  &              dir='t', trigle='a' )
  call DclDrawTextNormalized( 0.825, 0.525, 'ΔU\_{0} (m/s)', centering=-1 )
  call DclSetParm( "GRAPH:LCLIP", .true. )

!  call contourl_setting( contour_num2, val_spec=fixc_val2(1:contour_num2+1),  &
!  &                      idx_spec=fixc_idx2(1:contour_num2),  &
!  &                      typ_spec=fixc_typ2(1:contour_num2),  &
!  &                      formc=trim(adjustl(form_typec2)) )

!  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
!  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
!  &                   col_tab=cmap, reverse=col_rev,  &
!  &                   val_spec=fix_val(1:shade_num+1),  &
!  &                   col_spec=fix_col(1:shade_num) )

!  call Dcl_2D_cont_shade( 'ΔU (retrieval - truth) ',  &
!  &       draw_rd(1:nr_t), draw_td(1:nt_t),  &
!  &       draw_dVr(1:nr_t,1:nt_t),  &
!  &       draw_dVr(1:nr_t,1:nt_t),  &
!  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
!  &       (/'r (km)   ', 'θ (\^{o})'/),  &
!  &       (/form_typec2, form_types/), (/0.2, 0.8/),  &
!  &       (/0.2, 0.8/), c_num=(/contour_num2, shade_num/),  &
!  &       no_tone=.true. )

  call DclCloseGraphics

end program
