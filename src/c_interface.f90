!-----------------------------------------------------------------------
!     Copyright (C) 2026-2026 Satoki Tsujino. All rights reserved.
!-----------------------------------------------------------------------
開発中

module c_interface
  use iso_c_binding
  use GVTDX_sub
  use GVTDX_main
  use GVTD_main
  use GBVTD_main

  implicit none

contains

  subroutine c_Retrieve_velocity_GVTDX(n, m, lon_pix, lat_pix, h_pix,  &
  &              lon_cor, lat_cor, re, rp, hsat, psat, lsat, missing_value)  &
  &          bind(C, name="c_Retrieve_velocity_GVTDX")
subroutine Retrieve_velocity_GVTDX( n, m, nrot, ndiv, r, t, rh, td, rdiv, Vd, Vn, RadTC,  &
  &                                 VT, VR, VRT0, VDR0, VRTn, VRRn, VDTm, VDRm,  &
  &                                 missing_value, phin, zetan, VRT0_GVTD, VDR0_GVTD, Vn_0,  &
  &                                 VRTns_r, VRTnc_r, VRRns_r, VRRnc_r,  &
  &                                 zetans_r, zetanc_r )
    integer(c_int), value :: n, m, nrot, ndiv
    real(c_double)        :: lon_pix(n, m)
    real(c_double)        :: lat_pix(n, m)
    real(c_double)        :: h_pix(n, m)
    real(c_double)        :: lon_cor(n, m)
    real(c_double)        :: lat_cor(n, m)
    real(c_double), value :: missing_value
    real(c_double)        :: r(:)   !! radial coordinate on which Vd is defined [m]
    real(c_double)        :: t(:)   !! azimuthal coordinate on which Vd is defined [rad]
    real(c_double)        :: rh(size(r)+1)  !! radial coordinate on which Phi (staggered for Vd) is defined [m]
    real(c_double)        :: td(size(r),size(t))  !! radar azimuthal angle defined at Vd(r,t) [rad]
    real(c_double)        :: rdiv(:)  !! radial coordinate on which Dc (staggered for Vd) is defined [m]
    real(c_double)        :: Vd(size(r),size(t))  !! Doppler velocity defined on r-t [m s-1]
    real(c_double)        :: Vn                   !! y component of storm-relative mean wind on Cartesian coordinate (x,y) which is defined in the direction of the radar to TC ccenter [m s-1]
    real(c_double)        :: RadTC                !! Distance from radar to TC center [m]
    real(c_double)        :: VT(size(r),size(t))  !! retrieved total tangential wind [m s-1]
    real(c_double)        :: VR(size(r),size(t))  !! retrieved total radial wind [m s-1]
    real(c_double)        :: VRT0(size(r),size(t))  !! retrieved axisymmetric radial component of rotating wind [m s-1]
    real(c_double)        :: VDR0(size(r),size(t))  !! retrieved axisymmetric tangential component of divergent wind [m s-1]
    real(c_double)        :: VRTn(nrot,size(r),size(t))  !! retrieved tangential component of rotating wind [m s-1]
    real(c_double)        :: VRRn(nrot,size(r),size(t))  !! retrieved radial component of rotating wind [m s-1]
    real(c_double)        :: VDTm(ndiv,size(r),size(t))  !! retrieved tangential component of divergent wind [m s-1]
    real(c_double)        :: VDRm(ndiv,size(r),size(t))  !! retrieved radial component of divergent wind [m s-1]
    real(c_double)       , optional :: undef  !! undefined value for Vd
    real(c_double)       , optional :: phin(nrot,size(r),size(t))   !! retrieved stream function [m2 s-1]
    real(c_double)       , optional :: zetan(nrot,size(r),size(t))  !! retrieved vorticity [s-1]
    real(c_double)       , optional :: VRT0_GVTD(size(r),size(t))  !! retrieved axisymmetric radial component of pseudo-GVTD tangential wind [m s-1]
    real(c_double)       , optional :: VDR0_GVTD(size(r),size(t))  !! retrieved axisymmetric tangential component of pseudo-GVTD tangential wind [m s-1]
    real(c_double)       , optional :: Vn_0(size(r),size(t))  !! Aliased component to asymmetric tangential wind from (storm-relative) mean wind [m s-1]
    real(c_double)       , optional :: VRTns_r(nrot,size(r))  !! Sine component of retrieved asymmetric radial wind [m s-1]
    real(c_double)       , optional :: VRTnc_r(nrot,size(r))  !! Cosine component of retrieved asymmetric radial wind [m s-1]
    real(c_double)       , optional :: VRRns_r(nrot,size(r))  !! Sine component of retrieved asymmetric tangential wind [m s-1]
    real(c_double)       , optional :: VRRnc_r(nrot,size(r))  !! Cosine component of retrieved asymmetric tangential wind [m s-1]
    real(c_double)       , optional :: zetans_r(nrot,size(r))  !! Sine amplitude of retrieved vorticity [s-1]
    real(c_double)       , optional :: zetanc_r(nrot,size(r))  !! Cosine amplitude of retrieved vorticity [s-1]

    call Parallax_Correct( lon_pix, lat_pix, h_pix, lon_cor, lat_cor,  &
  &                        re, rp, hsat, psat, lsat, missing_value )

  end subroutine c_parallax_correct

  subroutine c_tri_interpolation_2d(n, m, l, k, x_in, y_in, iv, ivad,  &
  &                x_out, y_out, ov, ovad, missing_value, jflag )  &
  &          bind(C, name="c_tri_interpolation_2d")
    integer(c_int), value :: n, m, l, k
    real(c_double)        :: x_in(n, m)
    real(c_double)        :: y_in(n, m)
    real(c_double)        :: iv(n, m)
    real(c_double)        :: ivad(n, m)
    real(c_double)        :: x_out(l)
    real(c_double)        :: y_out(k)
    real(c_double)        :: ov(l, k)
    real(c_double)        :: ovad(l, k)
    real(c_double), value :: missing_value
    character(1), value :: jflag

    call tri_interpolation_2d( x_in, y_in, iv, ivad,  &
  &                            x_out, y_out, ov, ovad, missing_value, jflag )

  end subroutine c_tri_interpolation_2d


  subroutine c_tri_interpolation( x, y, val, point, oval )  &
  &          bind(C, name="c_tri_interpolation")
    real(c_double)       :: x(3)
    real(c_double)       :: y(3)
    real(c_double)       :: val(3)
    real(c_double)       :: point(2)
    real(c_double)       :: oval

    call tri_interpolation( x, y, val, point, oval )

  end subroutine c_tri_interpolation


  subroutine c_check_square_intersect( x, y, inum, selopt )  &
  &          bind(C, name="c_check_square_intersect")
    real(c_double)       :: x(4)
    real(c_double)       :: y(4)
    integer(c_int)       :: inum(2)
    character(1), value :: selopt

    call check_square_intersect( x, y, inum, selopt )

  end subroutine c_check_square_intersect


  logical function c_check_intersect( x1, y1, x2, y2 )  &
  &          bind(C, name="c_check_intersect")
    real(c_double)       :: x1(2)
    real(c_double)       :: y1(2)
    real(c_double)       :: x2(2)
    real(c_double)       :: y2(2)

    c_check_intersect=check_intersect( x1, y1, x2, y2 )

    return

  end function c_check_intersect


  logical function c_check_triclose( xposi, yposi, ival )  &
  &          bind(C, name="c_check_triclose")
    real(c_double)       :: xposi(3)
    real(c_double)       :: yposi(3)
    real(c_double)       :: ival(2)

    c_check_triclose=check_triclose( xposi, yposi, ival )

    return

  end function c_check_triclose


  subroutine c_convert_Tbb2Zph(n, m, l, tval, zval, t1d, z1d, missing_value)  &
  &          bind(C, name="c_convert_Tbb2Zph")
    integer(c_int), value :: n, m, l
    real(c_double)        :: tval(n, m)
    real(c_double)        :: zval(n, m)
    real(c_double)        :: t1d(l)
    real(c_double)        :: z1d(l)
    real(c_double), value :: missing_value

    call convert_Tbb2Zph( tval, zval, t1d, z1d, missing_value )

  end subroutine c_convert_Tbb2Zph



end module c_interface
