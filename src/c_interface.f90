!-----------------------------------------------------------------------
!     Copyright (C) 2026-2026 Satoki Tsujino. All rights reserved.
!-----------------------------------------------------------------------
開発中

module c_interface
  use iso_c_binding
  use parallax_core
  implicit none

contains

  subroutine c_parallax_correct(n, m, lon_pix, lat_pix, h_pix,  &
  &              lon_cor, lat_cor, re, rp, hsat, psat, lsat, missing_value)  &
  &          bind(C, name="c_parallax_correct")
    integer(c_int), value :: n, m
    real(c_double)        :: lon_pix(n, m)
    real(c_double)        :: lat_pix(n, m)
    real(c_double)        :: h_pix(n, m)
    real(c_double)        :: lon_cor(n, m)
    real(c_double)        :: lat_cor(n, m)
    real(c_double), value :: re
    real(c_double), value :: rp
    real(c_double), value :: hsat
    real(c_double), value :: psat
    real(c_double), value :: lsat
    real(c_double), value :: missing_value

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
