program test_draw
  use dcl
  use Dcl_Automatic
  use Math_Const
  use max_min

  implicit none

!-- namelist
  integer :: nx, ny, nr, nt
  real :: dx, dy, xmin, ymin, dr, dt, rmin, tmin

  integer :: IWS, tone_grid, cmap, phi_opt
  integer :: contour_num, shade_num, min_tab, max_tab
  integer :: contour_num2, contour_num3, contour_num4
  integer, dimension(80) :: fix_col
  integer, dimension(20) :: fixc_idx, fixc_typ, fixc_idx2, fixc_typ2, fixc_idx3, fixc_typ3, fixc_idx4, fixc_typ4
  real :: undef, xax_fact, yax_fact, vfact, tcx, tcy, phi1s, phi1c
  real :: us, vs, vmax, rmwv, phi0, xphi0, yphi0
  real, dimension(80) :: fix_val
  real, dimension(20) :: fixc_val, fixc_val2, fixc_val3, fixc_val4
  character(20) :: form_typec, form_typec2, form_typec3, form_typec4, form_types
  logical :: col_rev

  integer :: iphivmx, iphivmy, iphimx, iphimy
  integer :: i, j, k, l
  real :: tmpr
  real, allocatable, dimension(:) :: xd, yd, rd, td, draw_xd, draw_yd
  real, allocatable, dimension(:,:) :: phis, phi1, phiv, phi, ur, vt, rdxy, tdxy

  namelist /input /nx, ny, dx, dy, xmin, ymin,  &
  &                nr, nt, dr, dt, rmin, tmin, tcx, tcy
  namelist /draw_info /IWS, tone_grid, cmap, col_rev,  &
  &                    contour_num, fixc_val, fixc_idx, fixc_typ, form_typec,  &
  &                    contour_num2, fixc_val2, fixc_idx2, fixc_typ2, form_typec2,  &
  &                    contour_num3, fixc_val3, fixc_idx3, fixc_typ3, form_typec3,  &
  &                    contour_num4, fixc_val4, fixc_idx4, fixc_typ4, form_typec4,  &
  &                    shade_num, min_tab, max_tab, fix_val, fix_col, form_types,  &
  &                    xax_fact, yax_fact, vfact
  namelist /vortex /us, vs, vmax, rmwv,  &
  &                 phi0, xphi0, yphi0, phi1s, phi1c, phi_opt, undef
  read(5,nml=input)
  read(5,nml=draw_info)
  read(5,nml=vortex)

!-- Allocate variables

  allocate(xd(nx))
  allocate(yd(ny))
  allocate(draw_xd(nx))
  allocate(draw_yd(ny))
  allocate(rd(nr))
  allocate(td(nt))
  allocate(rdxy(nx,ny))
  allocate(tdxy(nx,ny))
  allocate(ur(nx,ny))
  allocate(vt(nx,ny))
  allocate(phis(nx,ny))
  allocate(phi1(nx,ny))
  allocate(phiv(nx,ny))
  allocate(phi(nx,ny))

!-- Define coordinates

  xd=(/((xmin+dx*real(i-1)),i=1,nx)/)
  yd=(/((ymin+dy*real(i-1)),i=1,ny)/)
  rd=(/((rmin+dr*real(i-1)),i=1,nr)/)
  td=(/((tmin+dt*real(i-1)),i=1,nt)/)
  draw_xd=xd*xax_fact
  draw_yd=yd*yax_fact

  do j=1,ny
     do i=1,nx
        rdxy(i,j)=sqrt((xd(i)-tcx)**2+(yd(j)-tcy)**2)
        tdxy(i,j)=atan2((yd(j)-tcy),(xd(i)-tcx))
     end do
  end do

!-- Define Vt, Ur, Phi
!--   Calculate Phis (us=dphi/dy, vs=-dphi/dx)
  do j=1,ny
     do i=1,nx
        phis(i,j)=phi0+us*(yd(j)-yphi0)-vs*(xd(i)-xphi0)
     end do
  end do

!--   Calculate Phi1 (specified from namelist)
  select case (phi_opt)
  case (1)  ! ~r
     do j=1,ny
        do i=1,nx
           phi1(i,j)=(phi1s*sin(tdxy(i,j))+phi1c*cos(tdxy(i,j)))*rdxy(i,j)
        end do
     end do

  case (2)  ! ~r^2/logr
     do j=1,ny
        do i=1,nx
           if(rdxy(i,j)<=rmwv)then
              phi1(i,j)=(phi1s*sin(tdxy(i,j))+phi1c*cos(tdxy(i,j)))*(rdxy(i,j)**2)
           else
              phi1(i,j)=(phi1s*sin(tdxy(i,j))+phi1c*cos(tdxy(i,j)))*(rmwv**2)  &
  &                    +(phi1s*sin(tdxy(i,j))+phi1c*cos(tdxy(i,j)))*(log(rdxy(i,j)/rmwv))
           end if
        end do
     end do
  end select

!--   Calculate Phiv (us=dphi/dy, vs=-dphi/dx)
  do j=1,ny
     do i=1,nx
        if(rdxy(i,j)<=rmwv)then
           vt(i,j)=(rdxy(i,j)/rmwv)*vmax
           phiv(i,j)=-0.5*(rdxy(i,j)**2/rmwv)*vmax
        else
           phiv(i,j)=-0.5*rmwv*vmax-rmwv*vmax*log(rdxy(i,j)/rmwv)
           vt(i,j)=(rmwv/rdxy(i,j))*vmax
        end if
     end do
  end do

!--   Calculate Phi
  do j=1,ny
     do i=1,nx
        phi(i,j)=phis(i,j)+phi1(i,j)+phiv(i,j)
     end do
  end do

  phis=phis*vfact
  phi1=phi1*vfact
  phiv=phiv*vfact
  phi=phi*vfact

!-- Calculate Max of phi
  call max_val_2d( phiv, iphivmx, iphivmy, tmpr )
  call max_val_2d( phi, iphimx, iphimy, tmpr )

!-- DCL

  call SGISET( 'IFONT', 1 )
  call SWLSET( 'LSYSFNT', .true. )
  CALL GLLSET( 'LMISS', .TRUE. )
  CALL GLRSET( 'RMISS', real(undef) )
  call UZFACT(0.9)
  call DclSetParm( 'ENABLE_CONTOUR_MESSAGE', .false. )
!  call DclSetTextIndex( 3 )
  call DclSetTextHeight( 0.03*0.75 )
  call UDLSET( 'LABEL', .false. )  ! Drawing major label of contour

  call DclOpenGraphics(IWS)

!  CALL SWSLFT("")
  CALL SWCSET('FONTNAME', 'Nimbus Sans 12')

!-- phis

  call contourl_setting( contour_num, val_spec=fixc_val(1:contour_num+1),  &
  &                      idx_spec=fixc_idx(1:contour_num),  &
  &                      typ_spec=fixc_typ(1:contour_num),  &
  &                      formc=trim(adjustl(form_typec)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Φ\_{s}',  &
  &       draw_xd(1:nx), draw_yd(1:ny), phis(1:nx,1:ny), phis(1:nx,1:ny),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
  &       (/form_typec, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num, shade_num/),  &
  &       no_tone=.true. )

  call DclDrawMarker( (/tcx*xax_fact/), (/tcy*yax_fact/),  &
  &                   index=24, type=16, height=0.015 )

  call DclSetParm( "GRAPH:LCLIP", .true. )

!-- phi1

  call contourl_setting( contour_num2, val_spec=fixc_val2(1:contour_num2+1),  &
  &                      idx_spec=fixc_idx2(1:contour_num2),  &
  &                      typ_spec=fixc_typ2(1:contour_num2),  &
  &                      formc=trim(adjustl(form_typec2)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Φ\_{1}',  &
  &       draw_xd(1:nx), draw_yd(1:ny), phi1(1:nx,1:ny), phi1(1:nx,1:ny),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
  &       (/form_typec2, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num2, shade_num/),  &
  &       no_tone=.true. )

  call DclDrawMarker( (/tcx*xax_fact/), (/tcy*yax_fact/),  &
  &                   index=24, type=16, height=0.015 )

  call DclSetParm( "GRAPH:LCLIP", .true. )

!-- phiv

  call contourl_setting( contour_num3, val_spec=fixc_val3(1:contour_num3+1),  &
  &                      idx_spec=fixc_idx3(1:contour_num3),  &
  &                      typ_spec=fixc_typ3(1:contour_num3),  &
  &                      formc=trim(adjustl(form_typec3)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Φ\_{v}',  &
  &       draw_xd(1:nx), draw_yd(1:ny), phiv(1:nx,1:ny), phiv(1:nx,1:ny),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
  &       (/form_typec3, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num3, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawMarker( draw_xd(iphivmx:iphivmx), draw_yd(iphivmy:iphivmy),  &
  &                   index=44, type=1, height=0.03 )
  call DclDrawMarker( (/tcx*xax_fact/), (/tcy*yax_fact/),  &
  &                   index=24, type=16, height=0.015 )
  call DclSetParm( "GRAPH:LCLIP", .true. )

!-- phi

  call contourl_setting( contour_num4, val_spec=fixc_val4(1:contour_num4+1),  &
  &                      idx_spec=fixc_idx4(1:contour_num4),  &
  &                      typ_spec=fixc_typ4(1:contour_num4),  &
  &                      formc=trim(adjustl(form_typec4)) )

  call color_setting( shade_num, (/0.0, 1.0/), min_tab=min_tab,  &
  &                   max_tab=max_tab, col_min=10999, col_max=89999,  &
  &                   col_tab=cmap, reverse=col_rev,  &
  &                   val_spec=fix_val(1:shade_num+1),  &
  &                   col_spec=fix_col(1:shade_num) )

  call Dcl_2D_cont_shade( 'Φ\_{v}+Φ\_{s}+Φ\_{1}',  &
  &       draw_xd(1:nx), draw_yd(1:ny), phi(1:nx,1:ny), phi(1:nx,1:ny),  &
  &       (/0.0, 1.0/), (/0.0, 1.0/),  &
  &       (/'X (km)', 'Y (km)'/),  &
  &       (/form_typec4, form_types/), (/0.2, 0.8/),  &
  &       (/0.2, 0.8/), c_num=(/contour_num4, shade_num/),  &
  &       no_tone=.true. )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  call DclDrawMarker( draw_xd(iphimx:iphimx), draw_yd(iphimy:iphimy),  &
  &                   index=44, type=1, height=0.03 )
  call DclDrawMarker( (/tcx*xax_fact/), (/tcy*yax_fact/),  &
  &                   index=24, type=16, height=0.015 )
  call DclSetParm( "GRAPH:LCLIP", .true. )

  call DclCloseGraphics

end program test_draw
