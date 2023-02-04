module tools_sub
!! Module for rearrangement of the radial grid and some operations for undef data in tools/ programs

contains

subroutine conv_d2r_3d( ival, oval )
!! convert double to real
  implicit none
  double precision, intent(in) :: ival(:,:,:)  !! input array
  real, intent(out) :: oval(size(ival,1),size(ival,2),size(ival,3))  !! output array
  integer :: ii, jj, kk, ni, nj, nk  !! internal variables

  ni=size(ival,1)
  nj=size(ival,2)
  nk=size(ival,3)

  do kk=1,nk
     do jj=1,nj
        do ii=1,ni
!write(*,*) "checkii", ii, jj, kk, ival(ii,jj,kk)
if(dabs(ival(ii,jj,kk))>1.0d15)then
           oval(ii,jj,kk)=-1.0e3
else
           oval(ii,jj,kk)=real(ival(ii,jj,kk))
end if
        end do
     end do
  end do

end subroutine conv_d2r_3d

!--------------------------------------------------
!--------------------------------------------------

subroutine conv_r2d_2d( ival, oval )
!! convert real to double
  implicit none
  real, intent(in) :: ival(:,:)  !! input array
  double precision, intent(out) :: oval(size(ival,1),size(ival,2))  !! output array
  integer :: ii, jj, ni, nj  !! internal variables

  ni=size(ival,1)
  nj=size(ival,2)

  do jj=1,nj
     do ii=1,ni
        oval(ii,jj)=dble(ival(ii,jj))
     end do
  end do

end subroutine conv_r2d_2d

!--------------------------------------------------
!--------------------------------------------------

subroutine add_2dd( ioval, sval, undef )
!! add sval to ioval
  implicit none
  double precision, intent(inout) :: ioval(:,:)  !! base
  double precision, intent(in) :: sval(size(ioval,1),size(ioval,2))  !! increment
  double precision, intent(in) :: undef  !! undefined value
  integer :: ii, jj, ni, nj  !! internal variables

  ni=size(ioval,1)
  nj=size(ioval,2)

  do jj=1,nj
     do ii=1,ni
        if(ioval(ii,jj)/=undef.and.sval(ii,jj)/=undef)then
           ioval(ii,jj)=ioval(ii,jj)+sval(ii,jj)
        end if
     end do
  end do

end subroutine add_2dd

!--------------------------------------------------
!--------------------------------------------------

subroutine sub_2dd( ioval, sval, undef )
!! subtract sval from ioval
  implicit none
  double precision, intent(inout) :: ioval(:,:)  !! base
  double precision, intent(in) :: sval(size(ioval,1),size(ioval,2))  !! increment
  double precision, intent(in) :: undef  !! undefined value
  integer :: ii, jj, ni, nj  !! internal variables

  ni=size(ioval,1)
  nj=size(ioval,2)

  do jj=1,nj
     do ii=1,ni
        if(ioval(ii,jj)/=undef.and.sval(ii,jj)/=undef)then
           ioval(ii,jj)=ioval(ii,jj)-sval(ii,jj)
        end if
     end do
  end do

end subroutine sub_2dd

!--------------------------------------------------
!--------------------------------------------------

subroutine replace_val_2d( ival2d, orgval, repval )
!! replace orgval with repval in ival2d array
  implicit none
  double precision, intent(inout) :: ival2d(:,:)  !! base
  double precision, intent(in) :: orgval  !! replace from this
  double precision, intent(in) :: repval  !! replace to this
  integer :: ii, jj, ix, jy  !! internal variables

  ix=size(ival2d,1)
  jy=size(ival2d,2)

  do jj=1,jy
     do ii=1,ix
        if(ival2d(ii,jj)==orgval)then
           ival2d(ii,jj)=repval
        end if
     end do
  end do

end subroutine replace_val_2d

!--------------------------------------------------
!--------------------------------------------------

subroutine replace_undef( ioval2d, undeflag, undefv )
!! replace ioval2d with undefv at the point undeflag == .true.
  implicit none
  double precision, intent(inout) :: ioval2d(:,:)  !! base
  logical, intent(in) :: undeflag(size(ioval2d,1),size(ioval2d,2))  !! flag for undefined point
  double precision, intent(in) :: undefv  !! undefined value
  integer :: ii, jj, ix, jy  !! internal variables

  ix=size(ioval2d,1)
  jy=size(ioval2d,2)

  do jj=1,jy
     do ii=1,ix
        if(undeflag(ii,jj).eqv..true.)then
           ioval2d(ii,jj)=undefv
        end if
     end do
  end do

end subroutine replace_undef

!--------------------------------------------------
!--------------------------------------------------

subroutine proj_Vs( xcent, ycent, xx, yy, ux, vy, projV, undef )
!! projection of a uniform wind [(ux,vy) on Cartesian grids] to the line-of-sight component along the radar beam
  implicit none
  double precision, intent(in) :: xcent  !! Radar location (x or lon)
  double precision, intent(in) :: ycent  !! Radar location (y or lat)
  double precision, intent(in) :: xx(:,:)  !! x coordinate (x or lon)
  double precision, intent(in) :: yy(size(xx,1),size(xx,2))  !! y coordinate (y or lat)
  double precision, intent(in) :: ux  !! X component of the uniform wind (m/s)
  double precision, intent(in) :: vy  !! Y component of the uniform wind (m/s)
  double precision, intent(out) :: projV(size(xx,1),size(xx,2))  !! line-of-sight component on (xx,yy)
  double precision, intent(in) :: undef  !! undefined value
  integer :: ii, jj, ix, jy
  double precision :: r_inv, rpx, rpy

  ix=size(xx,1)
  jy=size(xx,2)
  projV=undef

!$omp parallel default(shared)
!$omp do schedule(runtime) private(ii,jj,r_inv,rpx,rpy)

  do jj=1,jy
     do ii=1,ix
        rpx=xx(ii,jj)-xcent
        rpy=yy(ii,jj)-ycent
        if(rpx/=0.0d0.or.rpy/=0.0d0)then
           r_inv=1.0d0/dsqrt(rpx**2+rpy**2)
           projV(ii,jj)=rpx*r_inv*ux+rpy*r_inv*vy
        end if
     end do
  end do

!$omp end do
!$omp end parallel

end subroutine proj_Vs

!--------------------------------------------------
!--------------------------------------------------

subroutine proj_rtVs( radi, thet, ux, vy, projVR, projVT, undef )
!! projection of a uniform wind [(ux,vy) on Cartesian grids] to the radial and tangential components on the polar coordinate
  implicit none
  double precision, intent(in) :: radi(:)  !! radial grid (m)
  double precision, intent(in) :: thet(:)  !! azimuthal grid (rad)
  double precision, intent(in) :: ux       !! X-component of a uniform wind (m/s)
  double precision, intent(in) :: vy       !! Y-component of a uniform wind (m/s)
  double precision, intent(out) :: projVR(size(radi),size(thet))  !! radial wind
  double precision, intent(out) :: projVT(size(radi),size(thet))  !! tangential wind
  double precision, intent(in) :: undef  !! undefined value
  integer :: ii, jj, ix, jy

  ix=size(radi)
  jy=size(thet)
  projVR=undef
  projVT=undef

!$omp parallel default(shared)
!$omp do schedule(runtime) private(ii,jj)

  do jj=1,jy
     do ii=1,ix
        projVR(ii,jj)=ux*dcos(thet(jj))+vy*dsin(thet(jj))
        projVT(ii,jj)=-ux*dsin(thet(jj))+vy*dcos(thet(jj))
     end do
  end do

!$omp end do
!$omp end parallel

end subroutine proj_rtVs

!--------------------------------------------------
!--------------------------------------------------

subroutine decomp_Vd2rdpn( Vd, thetad, u_decomp, v_decomp, undef )
!! decomposition of VD to the components parallel and normal 
!!  to the direction of thetad = 0
  implicit none
  double precision, intent(in) :: Vd(:,:)  !! Doppler velocity (m/s)
  double precision, intent(in) :: thetad(size(Vd,1),size(Vd,2))  !! azimuthal angle from the line of radar to TC center (rad)
  double precision, intent(out) :: u_decomp(size(Vd,1),size(Vd,2))  !! parallel component 
  double precision, intent(out) :: v_decomp(size(Vd,1),size(Vd,2))  !! normal component
  double precision, intent(in) :: undef  !! undefined value
  integer :: ii, jj, ix, jy

  ix=size(Vd,1)
  jy=size(Vd,2)
  u_decomp=undef
  v_decomp=undef

!$omp parallel default(shared)
!$omp do schedule(runtime) private(ii,jj)

  do jj=1,jy
     do ii=1,ix
        if(Vd(ii,jj)/=undef)then
           u_decomp(ii,jj)=Vd(ii,jj)*dcos(thetad(ii,jj))
           v_decomp(ii,jj)=Vd(ii,jj)*dsin(thetad(ii,jj))
        end if
     end do
  end do

!$omp end do
!$omp end parallel

end subroutine decomp_Vd2rdpn

!--------------------------------------------------
!--------------------------------------------------

integer function check_data_fulfill( val, undef, nt_count, dir, ncount )
!! Check the innermost radius of data with no undef
!! If the innermost radius has not been found, 
!! the value of zero is returned.
  implicit none
  double precision, intent(in) :: val(:,:)  !! original data
  double precision, intent(in) :: undef     !! missing value
  integer, intent(in), optional :: nt_count !! minimum of data gridpoints
                                            !! default = size(val,2)
  character(3), intent(in), optional :: dir !! direction for searching
                                            !! 'i2o': inner to outer radii
                                            !!        (default)
                                            !! 'o2i': outer to inner radii
  integer, intent(out), optional :: ncount  !! available sampling number
  integer :: ifirst, ntc, ntcmax
  integer :: ii, jj, ix, jy, ixs, ixe, ixi
  logical :: i2o_flag

  ix=size(val,1)
  jy=size(val,2)
  ifirst=0

  if(present(nt_count))then
     ntcmax=nt_count
  else
     ntcmax=jy
  end if

  if(present(dir))then
     if(dir(1:3)=="i2o")then
        i2o_flag=.true.
     else if(dir(1:3)=="o2i")then
        i2o_flag=.false.
     else
        write(*,*) "*** ERROR (main:check_data_fulfill) ***: dir is invalid."
        stop
     end if
  else
     i2o_flag=.true.
  end if

  if(i2o_flag.eqv..true.)then  ! inner to outer
     ixs=1
     ixe=ix
     ixi=1
  else  ! outer to inner
     ixs=ix
     ixe=1
     ixi=-1
  end if

  do ii=ixs,ixe,ixi
     ntc=0
     do jj=1,jy
        if(val(ii,jj)/=undef)then
           ntc=ntc+1
        end if
     end do
     if(ntc>=ntcmax)then
        ifirst=ii
        if(present(ncount))then
           ncount=ntc
        end if
        exit
     end if
write(*,*) "ntcheck", ntc, ntcmax
  end do

  check_data_fulfill=ifirst

  return

end function check_data_fulfill

subroutine rearrange_undef_rad( skip_thres, nr_in, nr_out_org, nt, undef_grid,  &
  &                             r_org, rh_org, thetad_org, Vra_org,  &
  &                             nr_out, nn_grid, r, rh, thetad, Vra )
!! rearrange the radial grids:
!! 1. skip radii with few azimuthal sampling (less than "skip_thres")
!! 2. rearrange the radial coordinate.
!! [Note1]: r, rh, thetad, and Vra must be given by the same as
!!          the array number in the original.
!!          (that is, size(r_org) == size(r).)
!! [Note2]: nr_out_org >= nr_out. 
!!          Undef value is given in r, rh, thetad, and Vra outside nr_out. 
  implicit none
  integer, intent(in) :: skip_thres  !! threshold for unused radius
  integer, intent(in) :: nr_in       !! innermost array number in radius
  integer, intent(in) :: nr_out_org  !! outermost array number in radius (orig)
  integer, intent(in) :: nt          !! azimuthal array number
  logical, dimension(nr_in:nr_out_org,1:nt), intent(in) :: undef_grid
                                     !! flag of undefined grid (.true. is undef)
  double precision, dimension(nr_in:nr_out_org), intent(in) :: r_org
                                     !! velocity radius (original) [m]
  double precision, dimension(nr_in:nr_out_org+1), intent(in) :: rh_org
                                     !! potential radius (original) [m]
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(in) :: thetad_org
                                     !! azimuthal angle from radar (original) [rad]
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(in) :: Vra_org
                                     !! Doppler velocity (original) [m/s]
  integer, intent(out) :: nr_out     !! outermost array number in radius (skip)
  integer, dimension(nr_in:nr_out_org), intent(out) :: nn_grid
                                     !! grid number in rearranged each radius
  double precision, dimension(nr_in:nr_out_org), intent(out) :: r
                                     !! velocity radius (skip) [m]
  double precision, dimension(nr_in:nr_out_org+1), intent(out) :: rh
                                     !! potential radius (skip) [m]
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(out) :: thetad
                                     !! azimuthal angle from radar (skip) [rad]
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(out) :: Vra
                                     !! Doppler velocity (skip) [m/s]

  integer :: rcounter, tcounter, ii, jj

  rcounter=nr_in

  r(nr_in)=r_org(nr_in)
  rh(nr_in)=rh_org(nr_in)
  thetad(nr_in,1:nt)=thetad_org(nr_in,1:nt)
  Vra(nr_in,1:nt)=Vra_org(nr_in,1:nt)
  nn_grid=0
  nn_grid(nr_in)=nr_in

  do jj=nr_in+1,nr_out_org
     tcounter=0
     do ii=1,nt
        if(undef_grid(jj,ii).eqv..false.)then
           tcounter=tcounter+1
           if(tcounter>=skip_thres)then
              rcounter=rcounter+1
              nn_grid(rcounter)=jj
              exit
           end if
        end if
     end do

     r(rcounter)=r_org(nn_grid(rcounter))
     thetad(rcounter,1:nt)=thetad_org(nn_grid(rcounter),1:nt)
     Vra(rcounter,1:nt)=Vra_org(nn_grid(rcounter),1:nt)
  end do

  nr_out=rcounter

  do jj=nr_in+1,nr_out
     !-- [Note]: r is rearranged.
     rh(jj)=0.5d0*(r(jj)+r(jj-1))
  end do
  rh(nr_out+1)=rh_org(nn_grid(nr_out)+1)

end subroutine rearrange_undef_rad

!--------------------------------------------------
!--------------------------------------------------

subroutine recover_undef_rad( nrotmin, nrot, ndivmin, ndiv,  &
  &                           nr_in, nr_out, nr_out_org, nt,  &
  &                           undef, undef_grid, nn_grid,  &
  &                           VTtot_rt_t, VRtot_rt_t, VRT0_rt_t, VDR0_rt_t,  &
  &                           VRTn_rt_t, VRRn_rt_t, VDTm_rt_t, VDRm_rt_t,  &
  &                           VRT0_GVTD_rt_t, VDR0_GVTD_rt_t,  &
  &                           VRTns_rt_t, VRTnc_rt_t, VRRns_rt_t, VRRnc_rt_t,  &
  &                           Vn_0_rt_t, phin_rt_t, zetan_rt_t )
!! recover the retrieved values from the rearranged to original radii.
  implicit none
  integer, intent(in) :: nrotmin     !! minimum wavenumber for rotational components
  integer, intent(in) :: nrot        !! maximum wavenumber for rotational components
  integer, intent(in) :: ndivmin     !! minimum wavenumber for divergent components
  integer, intent(in) :: ndiv        !! maximum wavenumber for divergent components
  integer, intent(in) :: nr_in       !! innermost array number in radius
  integer, intent(in) :: nr_out      !! outermost array number in radius (skip)
  integer, intent(in) :: nr_out_org  !! outermost array number in radius (orig)
  integer, intent(in) :: nt          !! azimuthal array number
  double precision, intent(in) :: undef  !! undefined value
  logical, dimension(nr_in:nr_out_org,1:nt), intent(in) :: undef_grid
                                     !! flag of undefined grid (.true. is undef)
  integer, dimension(nr_in:nr_out_org), intent(in) :: nn_grid
                                     !! grid number in rearranged each radius
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(inout) :: VTtot_rt_t
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(inout) :: VRtot_rt_t
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(inout) :: VRT0_rt_t
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(inout) :: VDR0_rt_t
  double precision, dimension(nrotmin:nrot,nr_in:nr_out_org,1:nt), intent(inout) :: VRTn_rt_t
  double precision, dimension(nrotmin:nrot,nr_in:nr_out_org,1:nt), intent(inout) :: VRRn_rt_t
  double precision, dimension(ndivmin:ndiv,nr_in:nr_out_org,1:nt), intent(inout) :: VDTm_rt_t
  double precision, dimension(ndivmin:ndiv,nr_in:nr_out_org,1:nt), intent(inout) :: VDRm_rt_t
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(inout) :: VRT0_GVTD_rt_t
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(inout) :: VDR0_GVTD_rt_t
  double precision, dimension(nrotmin:nrot,nr_in:nr_out_org,1:nt), intent(inout) :: VRTns_rt_t
  double precision, dimension(nrotmin:nrot,nr_in:nr_out_org,1:nt), intent(inout) :: VRTnc_rt_t
  double precision, dimension(nrotmin:nrot,nr_in:nr_out_org,1:nt), intent(inout) :: VRRns_rt_t
  double precision, dimension(nrotmin:nrot,nr_in:nr_out_org,1:nt), intent(inout) :: VRRnc_rt_t
  double precision, dimension(nr_in:nr_out_org,1:nt), intent(inout) :: Vn_0_rt_t
  double precision, dimension(nrotmin:nrot,nr_in:nr_out_org,1:nt), intent(inout) :: phin_rt_t
  double precision, dimension(nrotmin:nrot,nr_in:nr_out_org,1:nt), intent(inout) :: zetan_rt_t

  integer :: rcounter, tcounter, ii, jj
  double precision, dimension(nr_in:nr_out_org,1:nt) :: VTtot_skp_rt_t, VRtot_skp_rt_t, VRT0_skp_rt_t, VDR0_skp_rt_t, VRT0_GVTD_skp_rt_t, VDR0_GVTD_skp_rt_t, Vn_0_skp_rt_t
  double precision, dimension(nrotmin:nrot,nr_in:nr_out_org,1:nt) :: VRTn_skp_rt_t, VRRn_skp_rt_t, VRTns_skp_rt_t, VRTnc_skp_rt_t, VRRns_skp_rt_t, VRRnc_skp_rt_t
  double precision, dimension(nrotmin:nrot,nr_in:nr_out_org,1:nt) :: phin_skp_rt_t, zetan_skp_rt_t
  double precision, dimension(ndivmin:ndiv,nr_in:nr_out_org,1:nt) :: VDTm_skp_rt_t, VDRm_skp_rt_t

  VTtot_skp_rt_t(nr_in:nr_out_org,1:nt)=VTtot_rt_t(nr_in:nr_out_org,1:nt)
  VRtot_skp_rt_t(nr_in:nr_out_org,1:nt)=VRtot_rt_t(nr_in:nr_out_org,1:nt)
  VRT0_skp_rt_t(nr_in:nr_out_org,1:nt)=VRT0_rt_t(nr_in:nr_out_org,1:nt)
  VDR0_skp_rt_t(nr_in:nr_out_org,1:nt)=VDR0_rt_t(nr_in:nr_out_org,1:nt)
  VRTn_skp_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=VRTn_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)
  VRRn_skp_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=VRRn_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)
  VDTm_skp_rt_t(ndivmin:ndiv,nr_in:nr_out_org,1:nt)=VDTm_rt_t(ndivmin:ndiv,nr_in:nr_out_org,1:nt)
  VDRm_skp_rt_t(ndivmin:ndiv,nr_in:nr_out_org,1:nt)=VDRm_rt_t(ndivmin:ndiv,nr_in:nr_out_org,1:nt)
  VRT0_GVTD_skp_rt_t(nr_in:nr_out_org,1:nt)=VRT0_GVTD_rt_t(nr_in:nr_out_org,1:nt)
  VDR0_GVTD_skp_rt_t(nr_in:nr_out_org,1:nt)=VDR0_GVTD_rt_t(nr_in:nr_out_org,1:nt)
  VRTns_skp_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=VRTns_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)
  VRTnc_skp_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=VRTnc_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)
  VRRns_skp_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=VRRns_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)
  VRRnc_skp_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=VRRnc_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)
  Vn_0_skp_rt_t(nr_in:nr_out_org,1:nt)=Vn_0_rt_t(nr_in:nr_out_org,1:nt)
  phin_skp_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=phin_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)
  zetan_skp_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=zetan_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)

  VTtot_rt_t(nr_in:nr_out_org,1:nt)=undef
  VRtot_rt_t(nr_in:nr_out_org,1:nt)=undef
  VRT0_rt_t(nr_in:nr_out_org,1:nt)=undef
  VDR0_rt_t(nr_in:nr_out_org,1:nt)=undef
  VRTn_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=undef
  VRRn_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=undef
  VDTm_rt_t(ndivmin:ndiv,nr_in:nr_out_org,1:nt)=undef
  VDRm_rt_t(ndivmin:ndiv,nr_in:nr_out_org,1:nt)=undef
  VRT0_GVTD_rt_t(nr_in:nr_out_org,1:nt)=undef
  VDR0_GVTD_rt_t(nr_in:nr_out_org,1:nt)=undef
  VRTns_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=undef
  VRTnc_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=undef
  VRRns_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=undef
  VRRnc_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=undef
  Vn_0_rt_t(nr_in:nr_out_org,1:nt)=undef
  phin_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=undef
  zetan_rt_t(nrotmin:nrot,nr_in:nr_out_org,1:nt)=undef

  do jj=nr_out,nr_in,-1
     VRT0_rt_t(nn_grid(jj),1:nt)=VRT0_skp_rt_t(jj,1:nt)
     VDR0_rt_t(nn_grid(jj),1:nt)=VDR0_skp_rt_t(jj,1:nt)
     VRT0_GVTD_rt_t(nn_grid(jj),1:nt)=VRT0_GVTD_skp_rt_t(jj,1:nt)
     VDR0_GVTD_rt_t(nn_grid(jj),1:nt)=VDR0_GVTD_skp_rt_t(jj,1:nt)
     do ii=1,nt
        if(undef_grid(nn_grid(jj),ii).eqv..false.)then
           VTtot_rt_t(nn_grid(jj),ii)=VTtot_skp_rt_t(jj,ii)
           VRtot_rt_t(nn_grid(jj),ii)=VRtot_skp_rt_t(jj,ii)
        end if
     end do
  end do

  if(nrotmin==1)then
     do jj=nr_out,nr_in,-1
        do ii=1,nt
           if(undef_grid(nn_grid(jj),ii).eqv..false.)then
              VRTn_rt_t(nrotmin:nrot,nn_grid(jj),ii)=VRTn_skp_rt_t(nrotmin:nrot,jj,ii)
              VRRn_rt_t(nrotmin:nrot,nn_grid(jj),ii)=VRRn_skp_rt_t(nrotmin:nrot,jj,ii)
           end if
        end do
        VRTns_rt_t(nrotmin:nrot,nn_grid(jj),1:nt)=VRTns_skp_rt_t(nrotmin:nrot,jj,1:nt)
        VRTnc_rt_t(nrotmin:nrot,nn_grid(jj),1:nt)=VRTnc_skp_rt_t(nrotmin:nrot,jj,1:nt)
        VRRns_rt_t(nrotmin:nrot,nn_grid(jj),1:nt)=VRRns_skp_rt_t(nrotmin:nrot,jj,1:nt)
        VRRnc_rt_t(nrotmin:nrot,nn_grid(jj),1:nt)=VRRnc_skp_rt_t(nrotmin:nrot,jj,1:nt)
        Vn_0_rt_t(nn_grid(jj),1:nt)=Vn_0_skp_rt_t(jj,1:nt)
        phin_rt_t(nrotmin:nrot,nn_grid(jj),1:nt)=phin_skp_rt_t(nrotmin:nrot,jj,1:nt)
        zetan_rt_t(nrotmin:nrot,nn_grid(jj),1:nt)=zetan_skp_rt_t(nrotmin:nrot,jj,1:nt)
     end do
  end if

  if(ndivmin==1)then
     do jj=nr_out,nr_in,-1
        do ii=1,nt
           if(undef_grid(nn_grid(jj),ii).eqv..false.)then
              VDTm_rt_t(ndivmin:ndiv,nn_grid(jj),ii)=VDTm_skp_rt_t(ndivmin:ndiv,jj,ii)
              VDRm_rt_t(ndivmin:ndiv,nn_grid(jj),ii)=VDRm_skp_rt_t(ndivmin:ndiv,jj,ii)
           end if
        end do
     end do
  end if

end subroutine recover_undef_rad

!--------------------------------------------------
!--------------------------------------------------

subroutine check_undef_grid( vval, undefv, undeflag )
!! check missing grids
  implicit none
  double precision, intent(in) :: vval(:,:)  !! input data
  double precision, intent(in) :: undefv  !! undefined value
  logical, intent(out) :: undeflag(size(vval,1),size(vval,2))  !! undefined flag (.true.)
  integer :: ii, jj, nni, nnj

  nni=size(vval,1)
  nnj=size(vval,2)
  undeflag=.false.

  do jj=1,nnj
     do ii=1,nni
        if(vval(ii,jj)==undefv)then
           undeflag(ii,jj)=.true.
        end if
     end do
  end do

end subroutine check_undef_grid

!--------------------------------------------------
!--------------------------------------------------

integer function inner_radius_check( nrin, nrout, radi )
!! Check positive value of the innermost radius. 
!! If the innermost radius is negative, 
!! the element number located at the innermost positive radius is returned.
  implicit none
  integer, intent(in) :: nrin     !! A given element of the innermost radius
  integer, intent(in) :: nrout    !! A given element of the outermost radius
  double precision, intent(in) :: radi(nrin:nrout)   !! Original radial grids
  integer :: ii

  if(radi(nrin)<=0.0d0)then
     do ii=nrin+1,nrout
        if(radi(ii)>0.0d0)then
           inner_radius_check=ii
           exit
        end if
     end do
  else
     inner_radius_check=nrin
  end if

  return

end function inner_radius_check

!--------------------------------------------------
! 
!--------------------------------------------------

end module tools_sub
