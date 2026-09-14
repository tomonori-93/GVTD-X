!-----------------------------------------------------------------------
!     Copyright (C) 2026-2026 Satoki Tsujino. All rights reserved.
!-----------------------------------------------------------------------

module c_interface
  use iso_c_binding
  use GVTDX_sub
  use GVTDX_main
  use GVTD_main
  use GBVTD_main

  implicit none

contains

  subroutine c_Retrieve_velocity_GVTDX(  &
  &            n, m, l, nrot, ndiv, r, t, rh, td, rdiv, Vd, Vn, RadTC,  &
  &            VT, VR, VRT0, VDR0, VRTn, VRRn, VDTm, VDRm,  &
  &            missing_value, phin, zetan, VRT0_GVTD, VDR0_GVTD, Vn_0,  &
  &            VRTns_r, VRTnc_r, VRRns_r, VRRnc_r,  &
  &            zetans_r, zetanc_r)  &
  &          bind(C, name="c_Retrieve_velocity_GVTDX")
    integer(c_int), value :: n, m, l, nrot, ndiv
    real(c_double), value :: missing_value
    real(c_double)        :: r(n)   !! radial coordinate on which Vd is defined [m]
    real(c_double)        :: t(m)   !! azimuthal coordinate on which Vd is defined [rad]
    real(c_double)        :: rh(n+1)  !! radial coordinate on which Phi (staggered for Vd) is defined [m]
    real(c_double)        :: td(n,m)  !! radar azimuthal angle defined at Vd(r,t) [rad]
    real(c_double)        :: rdiv(l)  !! radial coordinate on which Dc (staggered for Vd) is defined [m]
    real(c_double)        :: Vd(n,m)  !! Doppler velocity defined on r-t [m s-1]
    real(c_double), value :: Vn       !! y component of storm-relative mean wind on Cartesian coordinate (x,y) which is defined in the direction of the radar to TC ccenter [m s-1]
    real(c_double), value :: RadTC    !! Distance from radar to TC center [m]
    real(c_double)        :: VT(n,m)  !! retrieved total tangential wind [m s-1]
    real(c_double)        :: VR(n,m)  !! retrieved total radial wind [m s-1]
    real(c_double)        :: VRT0(n,m)  !! retrieved axisymmetric radial component of rotating wind [m s-1]
    real(c_double)        :: VDR0(n,m)  !! retrieved axisymmetric tangential component of divergent wind [m s-1]
    real(c_double)        :: VRTn(nrot,n,m)  !! retrieved tangential component of rotating wind [m s-1]
    real(c_double)        :: VRRn(nrot,n,m)  !! retrieved radial component of rotating wind [m s-1]
    real(c_double)        :: VDTm(ndiv,n,m)  !! retrieved tangential component of divergent wind [m s-1]
    real(c_double)        :: VDRm(ndiv,n,m)  !! retrieved radial component of divergent wind [m s-1]
    real(c_double), value :: missing_value  !! missing value for Vd
    real(c_double)       , optional :: phin(nrot,n,m)   !! retrieved stream function [m2 s-1]
    real(c_double)       , optional :: zetan(nrot,n,m)  !! retrieved vorticity [s-1]
    real(c_double)       , optional :: VRT0_GVTD(n,m)  !! retrieved axisymmetric radial component of pseudo-GVTD tangential wind [m s-1]
    real(c_double)       , optional :: VDR0_GVTD(n,m)  !! retrieved axisymmetric tangential component of pseudo-GVTD tangential wind [m s-1]
    real(c_double)       , optional :: Vn_0(n,m)  !! Aliased component to asymmetric tangential wind from (storm-relative) mean wind [m s-1]
    real(c_double)       , optional :: VRTns_r(nrot,n)  !! Sine component of retrieved asymmetric radial wind [m s-1]
    real(c_double)       , optional :: VRTnc_r(nrot,n)  !! Cosine component of retrieved asymmetric radial wind [m s-1]
    real(c_double)       , optional :: VRRns_r(nrot,n)  !! Sine component of retrieved asymmetric tangential wind [m s-1]
    real(c_double)       , optional :: VRRnc_r(nrot,n)  !! Cosine component of retrieved asymmetric tangential wind [m s-1]
    real(c_double)       , optional :: zetans_r(nrot,n)  !! Sine amplitude of retrieved vorticity [s-1]
    real(c_double)       , optional :: zetanc_r(nrot,n)  !! Cosine amplitude of retrieved vorticity [s-1]

    call Retrieve_velocity_GVTDX( nrot, ndiv, r, t, rh, td, rdiv, Vd, Vn, RadTC,  &
  &                               VT, VR, VRT0, VDR0, VRTn, VRRn, VDTm, VDRm,  &
  &                               missing_value, phin, zetan, VRT0_GVTD, VDR0_GVTD, Vn_0,  &
  &                               VRTns_r, VRTnc_r, VRRns_r, VRRnc_r,  &
  &                               zetans_r, zetanc_r )

  end subroutine c_Retrieve_velocity_GVTDX


  subroutine c_Retrieve_velocity_GVTD(  &
               n, m, nasym, r, t, td, Vd, RadTC,  &
  &            VT, VR, VT0, VR0, VTSn, VTCn, missing_value,  &
  &            flag_datagap )  &
  &          bind(C, name="c_Retrieve_velocity_GVTD")
    integer(c_int), value :: n, m, nasym
    real(c_double)        :: r(n)   !! radial coordinate on which Vd is defined [m]
    real(c_double)        :: t(m)   !! azimuthal coordinate on which Vd is defined [rad]
    real(c_double)        :: td(n,m)  !! radar azimuthal angle defined at Vd(r,t) [rad]
    real(c_double)        :: Vd(n,m)  !! Doppler velocity defined on r-t [m s-1]
    real(c_double), value :: RadTC                !! Distance from radar to TC center [m]
    real(c_double)        :: VT(n,m)  !! retrieved total tangential wind [m s-1]
    real(c_double)        :: VR(n,m)  !! retrieved total radial wind [m s-1]
    real(c_double)        :: VT0(n,m)  !! retrieved axisymmetric radial component of rotating wind [m s-1]
    real(c_double)        :: VR0(n,m)  !! retrieved axisymmetric tangential component of divergent wind [m s-1]
    real(c_double)        :: VTSn(nasym,n,m)  !! retrieved tangential component of rotating wind [m s-1]
    real(c_double)        :: VTCn(nasym,n,m)  !! retrieved radial component of rotating wind [m s-1]
    real(c_double), value :: missing_value  !! missing value for Vd
    logical,       , optional :: flag_datagap  !! Optimize the truncating wavenumber at each radius,  according to Lee et al. (2000) [Default: .false. (not optimize)]

    call Retrieve_velocity_GVTD( nasym, r, t, td, Vd, RadTC,  &
  &                              VT, VR, VT0, VR0, VTSn, VTCn, missing_value,  &
  &                              flag_datagap )  &

  end subroutine c_Retrieve_velocity_GVTD


  subroutine c_Retrieve_velocity_GBVTD(  &
               n, m, nasym, r, t, td, Vd, RadTC,  &
  &            VT, VR, VT0, VR0, VTSn, VTCn, missing_value,  &
  &            flag_datagap )  &
  &          bind(C, name="c_Retrieve_velocity_GBVTD")
    integer(c_int), value :: n, m, nasym
    real(c_double)        :: r(n)   !! radial coordinate on which Vd is defined [m]
    real(c_double)        :: t(m)   !! azimuthal coordinate on which Vd is defined [rad]
    real(c_double)        :: td(n,m)  !! radar azimuthal angle defined at Vd(r,t) [rad]
    real(c_double)        :: Vd(n,m)  !! Doppler velocity defined on r-t [m s-1]
    real(c_double), value :: RadTC                !! Distance from radar to TC center [m]
    real(c_double)        :: VT(n,m)  !! retrieved total tangential wind [m s-1]
    real(c_double)        :: VR(n,m)  !! retrieved total radial wind [m s-1]
    real(c_double)        :: VT0(n,m)  !! retrieved axisymmetric radial component of rotating wind [m s-1]
    real(c_double)        :: VR0(n,m)  !! retrieved axisymmetric tangential component of divergent wind [m s-1]
    real(c_double)        :: VTSn(nasym,n,m)  !! retrieved tangential component of rotating wind [m s-1]
    real(c_double)        :: VTCn(nasym,n,m)  !! retrieved radial component of rotating wind [m s-1]
    real(c_double), value :: missing_value  !! missing value for Vd
    logical,       , optional :: flag_datagap  !! Optimize the truncating wavenumber at each radius,  according to Lee et al. (2000) [Default: .false. (not optimize)]

    call Retrieve_velocity_GBVTD( nasym, r, t, td, Vd, RadTC,  &
  &                               VT, VR, VT0, VR0, VTSn, VTCn, missing_value,  &
  &                               flag_datagap )  &

  end subroutine c_Retrieve_velocity_GBVTD


end module c_interface
