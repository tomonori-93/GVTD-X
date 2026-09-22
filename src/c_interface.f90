!-----------------------------------------------------------------------
!     Copyright (C) 2026-2026 Satoki Tsujino. All rights reserved.
!-----------------------------------------------------------------------

module c_interface
  use iso_c_binding
  use Retrieval_control
  use GVTDX_sub
  use GVTDX_main
  use GVTD_main
  use GBVTD_main

  implicit none

contains

  subroutine c_Retrieval_control(  &
  &            n, m, nrot, ndiv, nz, r_t, theta_ref_t, lon_tc, lat_tc,  &
  &            usp, vsp, nthres_undef, skip_min_t, flag_GVTDX, missing_value,  &
  &            lon_rdr, lat_rdr, Vra_in,  &
  &            VTtot, VRtot, VRT0, VDR0, Vra, Vra_ret, VRTn, VRRn, phin, zetan,  &
  &            VDTm, VDRm, Vn_0, Vra_Er, Vra_Er_ret, VTtot_Er, VRtot_Er, Uxtot_Er, Vytot_Er,  &
  &            lond, latd, zeta0, zetatot, flag_datagap, umd, vmd, l, rdiv_t)  &
  &          bind(C, name="c_Retrieval_control")
    integer(c_int), value :: n, m
    integer(c_int), value :: nrot              !! the rotating maximum wavenumber used in the retrieval
    integer(c_int), value :: ndiv              !! the divergent maximum wavenumber used in the retrieval
    integer(c_int), value :: nz                !! vertical grid number for the input Doppler velocity
    real(c_double)        :: r_t(n)     !! radial coordinate on which Vra_in is defined [m]
    real(c_double)        :: theta_ref_t(m)     !! azimuthal coordinate on which Vra_in is defined [rad]
    real(c_double), value :: lon_tc   !! longitude of the TC center [degree]
    real(c_double), value :: lat_tc   !! latitude of the TC center [degree]
    real(c_double), value :: usp      !! zonal component of the moving velocity of the TC [m/s]
    real(c_double), value :: vsp      !! meridional component of the moving velocity of the TC [m/s]
    integer(c_int)        :: nthres_undef(2)   !! thresholds of the azimuthal sampling number to determine the (1) innermost and (2) outermost radii, respectively
    integer(c_int), value :: skip_min_t        !! threshold of the azimuthal sampling number to determine unused radii
    integer(c_int), value :: flag_GVTDX        !! 1: GVTD-X, 2: GVTD, 3: GBVTD
    real,           value :: missing_value             !! undefined value for the original Doppler radar data
    real(c_double), value :: lon_rdr !! longitudinal position of the radar (degree)
    real(c_double), value :: lat_rdr !! latitudinal position of the radar (degree)
    real                  :: Vra_in(n,m,nz)  !! input Doppler velocity [m/s]
    real(c_double)        :: VTtot(n,m,nz)  !! retrieved total tangential wind [m s-1]
    real(c_double)        :: VRtot(n,m,nz)  !! retrieved total radial wind [m s-1]
    real(c_double)        :: VRT0(n,m,nz)  !! retrieved axisymmetric tangential wind [m s-1]
    real(c_double)        :: VDR0(n,m,nz)  !! retrieved axisymmetric radial wind [m s-1]
    real(c_double)        :: Vra(n,m,nz)      !! storm-relative input Doppler velocity [m s-1]
    real(c_double)        :: Vra_ret(n,m,nz)  !! storm-relative retrieved Doppler velocity [m s-1]
    real(c_double)        :: VRTn(nrot,n,m,nz)  !! retrieved wavenumber-N rotational-tangential wind [m s-1]
    real(c_double)        :: VRRn(nrot,n,m,nz)  !! retrieved wavenumber-N rotational-radial wind [m s-1]
    real(c_double)        :: phin(nrot,n,m,nz)  !! retrieved wavenumber-N streamfunction [m2 s-1]
    real(c_double)        :: zetan(nrot,n,m,nz)  !! retrieved wavenumber-N vorticity [s-1]
    real(c_double)        :: VDTm(ndiv,n,m,nz)  !! retrieved wavenumber-M divergent-tangential wind [m s-1]
    real(c_double)        :: VDRm(ndiv,n,m,nz)  !! retrieved wavenumber-M divergent-radial wind [m s-1]
    real(c_double)        :: Vn_0(n,m,nz)  !! storm-relative mean wind normal to line of sight [m s-1]
    real(c_double)        :: Vra_Er(n,m,nz)  !! earth-relative Doppler velocity [m s-1]
    real(c_double)        :: Vra_Er_ret(n,m,nz)  !! earth-relative retrieved Doppler velocity [m s-1]
    real(c_double)        :: VTtot_Er(n,m,nz)  !! earth-relative retrieved total tangential wind [m s-1]
    real(c_double)        :: VRtot_Er(n,m,nz)  !! earth-relative retrieved total radial wind [m s-1]
    real(c_double)        :: Uxtot_Er(n,m,nz)  !! earth-relative retrieved total zonal wind [m s-1]
    real(c_double)        :: Vytot_Er(n,m,nz)  !! earth-relative retrieved total meridional wind [m s-1]
    real(c_double)        :: lond(n,m)  !! Longitude [degree]
    real(c_double)        :: latd(n,m)  !! Latitude [degree]
    real(c_double)        :: zeta0(n,m,nz)  !! retrieved axisymmetric vorticity [s-1]
    real(c_double)        :: zetatot(n,m,nz)  !! retrieved total vorticity [s-1]
    logical(c_bool),       optional :: flag_datagap !! Flag for use of optimal wavenumber from data gap (Lee et al. 2000)
    real(c_double),        optional :: umd(nz)  !! zonal component of mean wind [m s-1]
    real(c_double),        optional :: vmd(nz)  !! meridional component of mean wind [m s-1]
    integer(c_int), value, optional :: nrdiv             !! radial grid number where the divergence is defined
    real(c_double),        optional :: rdiv_t(nrdiv)  !! radial grids where the divergence is defined

    !-- internal arrays for working
    real(c_double) :: tmp_umd(nz)
    real(c_double) :: tmp_vmd(nz)
    real(c_double) :: tmp_rdiv_t(nrdiv)
    logical(c_bool) :: tmp_flag_datagap

    tmp_umd = 0.0d0
    tmp_umd = 0.0d0
    tmp_flag_datagap = .false.

    if(present(umd)) tmp_umd = umd
    if(present(vmd)) tmp_vmd = vmd
    if(present(flag_datagap)) tmp_flag_datagap = flag_datagap

    if(present(nrdiv))then
       call Retrieve_velocity( nrot, ndiv, nz, r_t, theta_ref_t, lon_tc, lat_tc,  &
  &                  usp, vsp, nthres_undef, skip_min_t, flag_GVTDX, missing_value,  &
  &                  lon_rdr, lat_rdr, Vra_in,  &
  &                  VTtot, VRtot, VRT0, VDR0, Vra, Vra_ret, VRTn, VRRn, phin, zetan,  &
  &                  VDTm, VDRm, Vn_0, Vra_Er, Vra_Er_ret, VTtot_Er, VRtot_Er, Uxtot_Er, Vytot_Er,  &
  &                  lond, latd, zeta0, zetatot, flag_datagap=tmp_flag_datagap,  &
  &                  umd=tmp_umd, vmd=tmp_vmd, nrdiv=nrdiv, rdiv_t=rdiv_t )
    else
       call Retrieve_velocity( nrot, ndiv, nz, r_t, theta_ref_t, lon_tc, lat_tc,  &
  &                  usp, vsp, nthres_undef, skip_min_t, flag_GVTDX, missing_value,  &
  &                  lon_rdr, lat_rdr, Vra_in,  &
  &                  VTtot, VRtot, VRT0, VDR0, Vra, Vra_ret, VRTn, VRRn, phin, zetan,  &
  &                  VDTm, VDRm, Vn_0, Vra_Er, Vra_Er_ret, VTtot_Er, VRtot_Er, Uxtot_Er, Vytot_Er,  &
  &                  lond, latd, zeta0, zetatot, flag_datagap=tmp_flag_datagap,  &
  &                  umd=tmp_umd, vmd=tmp_vmd )
    end if

  end subroutine c_Retrieval_control


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
    real(c_double),        optional :: phin(nrot,n,m)   !! retrieved stream function [m2 s-1]
    real(c_double),        optional :: zetan(nrot,n,m)  !! retrieved vorticity [s-1]
    real(c_double),        optional :: VRT0_GVTD(n,m)  !! retrieved axisymmetric radial component of pseudo-GVTD tangential wind [m s-1]
    real(c_double),        optional :: VDR0_GVTD(n,m)  !! retrieved axisymmetric tangential component of pseudo-GVTD tangential wind [m s-1]
    real(c_double),        optional :: Vn_0(n,m)  !! Aliased component to asymmetric tangential wind from (storm-relative) mean wind [m s-1]
    real(c_double),        optional :: VRTns_r(nrot,n)  !! Sine component of retrieved asymmetric radial wind [m s-1]
    real(c_double),        optional :: VRTnc_r(nrot,n)  !! Cosine component of retrieved asymmetric radial wind [m s-1]
    real(c_double),        optional :: VRRns_r(nrot,n)  !! Sine component of retrieved asymmetric tangential wind [m s-1]
    real(c_double),        optional :: VRRnc_r(nrot,n)  !! Cosine component of retrieved asymmetric tangential wind [m s-1]
    real(c_double),        optional :: zetans_r(nrot,n)  !! Sine amplitude of retrieved vorticity [s-1]
    real(c_double),        optional :: zetanc_r(nrot,n)  !! Cosine amplitude of retrieved vorticity [s-1]

    !-- internal arrays for working
    real(c_double) :: tmp_phin(nrot,n,m)
    real(c_double) :: tmp_zetan(nrot,n,m)
    real(c_double) :: tmp_VRT0_GVTD(n,m)
    real(c_double) :: tmp_VDR0_GVTD(n,m)
    real(c_double) :: tmp_Vn_0(n,m)
    real(c_double) :: tmp_VRTns_r(nrot,n)
    real(c_double) :: tmp_VRTnc_r(nrot,n)
    real(c_double) :: tmp_VRRns_r(nrot,n)
    real(c_double) :: tmp_VRRnc_r(nrot,n)
    real(c_double) :: tmp_zetans_r(nrot,n)
    real(c_double) :: tmp_zetanc_r(nrot,n)

    tmp_phin      = missing_value
    tmp_zetan     = missing_value
    tmp_VRT0_GVTD = missing_value
    tmp_VDR0_GVTD = missing_value
    tmp_Vn_0      = missing_value
    tmp_VRTns_r   = missing_value
    tmp_VRTnc_r   = missing_value
    tmp_VRRns_r   = missing_value
    tmp_VRRnc_r   = missing_value
    tmp_zetans_r  = missing_value
    tmp_zetanc_r  = missing_value

    call Retrieve_velocity_GVTDX( nrot, ndiv, r, t, rh, td, rdiv, Vd, Vn, RadTC,  &
  &                               VT, VR, VRT0, VDR0, VRTn, VRRn, VDTm, VDRm,  &
  &                               missing_value, phin, zetan, VRT0_GVTD, VDR0_GVTD, Vn_0,  &
  &                               VRTns_r, VRTnc_r, VRRns_r, VRRnc_r,  &
  &                               zetans_r, zetanc_r )

    if(present(phin))      phin      = tmp_phin
    if(present(zetan))     zetan     = tmp_zetan
    if(present(VRT0_GVTD)) VRT0_GVTD = tmp_VRT0_GVTD
    if(present(VDR0_GVTD)) VDR0_GVTD = tmp_VDR0_GVTD
    if(present(Vn_0))      Vn_0      = tmp_Vn_0
    if(present(VRTns_r))   VRTns_r   = tmp_VRTns_r
    if(present(VRTnc_r))   VRTnc_r   = tmp_VRTnc_r
    if(present(VRRns_r))   VRRns_r   = tmp_VRRns_r
    if(present(VRRnc_r))   VRRnc_r   = tmp_VRRnc_r
    if(present(zetans_r))  zetans_r  = tmp_zetans_r
    if(present(zetanc_r))  zetanc_r  = tmp_zetanc_r

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
    real(c_double), value :: RadTC    !! Distance from radar to TC center [m]
    real(c_double)        :: VT(n,m)  !! retrieved total tangential wind [m s-1]
    real(c_double)        :: VR(n,m)  !! retrieved total radial wind [m s-1]
    real(c_double)        :: VT0(n,m)  !! retrieved axisymmetric radial component of rotating wind [m s-1]
    real(c_double)        :: VR0(n,m)  !! retrieved axisymmetric tangential component of divergent wind [m s-1]
    real(c_double)        :: VTSn(nasym,n,m)  !! retrieved tangential component of rotating wind [m s-1]
    real(c_double)        :: VTCn(nasym,n,m)  !! retrieved radial component of rotating wind [m s-1]
    real(c_double), value :: missing_value  !! missing value for Vd
    logical(c_bool), value, optional :: flag_datagap  !! Optimize the truncating wavenumber at each radius,  according to Lee et al. (2000) [Default: .false. (not optimize)]

    if(present(flag_datagap))then
       call Retrieve_velocity_GVTD( nasym, r, t, td, Vd, RadTC,  &
  &                                 VT, VR, VT0, VR0, VTSn, VTCn, missing_value,  &
  &                                 flag_datagap )
    else
       call Retrieve_velocity_GVTD( nasym, r, t, td, Vd, RadTC,  &
  &                                 VT, VR, VT0, VR0, VTSn, VTCn, missing_value )
    end if

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
    real(c_double), value :: RadTC    !! Distance from radar to TC center [m]
    real(c_double)        :: VT(n,m)  !! retrieved total tangential wind [m s-1]
    real(c_double)        :: VR(n,m)  !! retrieved total radial wind [m s-1]
    real(c_double)        :: VT0(n,m)  !! retrieved axisymmetric radial component of rotating wind [m s-1]
    real(c_double)        :: VR0(n,m)  !! retrieved axisymmetric tangential component of divergent wind [m s-1]
    real(c_double)        :: VTSn(nasym,n,m)  !! retrieved tangential component of rotating wind [m s-1]
    real(c_double)        :: VTCn(nasym,n,m)  !! retrieved radial component of rotating wind [m s-1]
    real(c_double), value :: missing_value  !! missing value for Vd
    logical(c_bool), value, optional :: flag_datagap  !! Optimize the truncating wavenumber at each radius,  according to Lee et al. (2000) [Default: .false. (not optimize)]

    if(present(flag_datagap))then
       call Retrieve_velocity_GBVTD( nasym, r, t, td, Vd, RadTC,  &
  &                                  VT, VR, VT0, VR0, VTSn, VTCn, missing_value,  &
  &                                  flag_datagap )
    else
       call Retrieve_velocity_GBVTD( nasym, r, t, td, Vd, RadTC,  &
  &                                  VT, VR, VT0, VR0, VTSn, VTCn, missing_value )
    end if

  end subroutine c_Retrieve_velocity_GBVTD


end module c_interface
