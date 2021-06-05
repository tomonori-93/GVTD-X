module sub_mod

contains

subroutine prod_vortex_structure( r, t, rmax, vmax, c1u, c2u,  &
  &                               Vt, Vr, Vt_pert, Vr_pert, Vt_pert_ang, Vr_pert_ang,  &
  &                               ropt, Vt_0, Vr_0 )
!-- Producing vortex structure with rmax, vmax, and umax
  implicit none
  double precision, intent(in) :: r(:)  ! radius [m]
  double precision, intent(in) :: t(:)  ! azimuthal angle [rad]
  double precision, intent(in) :: rmax  ! radius of maximum tangential wind speed [m]
  double precision, intent(in) :: vmax  ! maximum tangential wind speed [m s-1]
  double precision, intent(in) :: c1u   ! coefficient 1 for radial wind [s-1]
  double precision, intent(in) :: c2u   ! coefficient 2 for radial wind [s-1]
  double precision, intent(out) :: Vt(size(r),size(t))  ! Profile of tangential wind
  double precision, intent(out) :: Vr(size(r),size(t))  ! Profile of radial wind
  double precision, intent(in), optional :: Vt_pert(:)  ! perturbations of tangential wind [m s-1]
  double precision, intent(in), optional :: Vr_pert(:)  ! perturbations of radial wind [m s-1]
  double precision, intent(in), optional :: Vt_pert_ang(size(Vt_pert))  ! angles of tangential wind [rad]
  double precision, intent(in), optional :: Vr_pert_ang(size(Vr_pert))  ! angles of radial wind [rad]
  logical, intent(in), optional :: ropt  ! option for radial variation of perturbation Vt and Vr
  double precision, intent(out), optional :: Vt_0(size(r))  ! Radial profile of axisymmetric Vt [m s-1]
  double precision, intent(out), optional :: Vr_0(size(r))  ! Radial profile of axisymmetric Vr [m s-1]

  integer :: nr, nt, i, j, k, nvtp, nvrp
  double precision :: tmp_vtp, tmp_vrp, rad_coef, radp_coef
  logical rad_opt

  nr=size(r)
  nt=size(t)
  nvtp=size(Vt_pert)
  nvrp=size(Vr_pert)

  rad_opt=.false.
  if(present(ropt))then
     rad_opt=ropt
  end if

  do j=1,nt
     tmp_vtp=0.0d0
     tmp_vrp=0.0d0
     if(present(Vt_pert))then
        do k=1,nvtp
           tmp_vtp=tmp_vtp+Vt_pert(k)*dcos(dble(k)*(t(j)+Vt_pert_ang(k)))
        end do
     end if
     if(present(Vr_pert))then
        do k=1,nvrp
           tmp_vrp=tmp_vrp+Vr_pert(k)*dcos(dble(k)*(t(j)+Vr_pert_ang(k)))
        end do
     end if
     do i=1,nr
        if(r(i)<=rmax)then
           rad_coef=r(i)/rmax
           radp_coef=r(i)/rmax
           Vt(i,j)=vmax*rad_coef
           Vr(i,j)=c1u*dsqrt((rmax-r(i))*r(i))
           Vr(i,j)=Vr(i,j)*4.0*1.0e-3  ! 多分これが必要 [m] -> [km]
        else
           rad_coef=rmax/r(i)
           if((r(i)>rmax).and.(r(i)<=1.5d0*rmax))then
              radp_coef=1.0d0
           else
              radp_coef=rmax/r(i)
           end if
           Vt(i,j)=vmax*rad_coef
           Vr(i,j)=-c2u*dsqrt(r(i)-rmax)*(rmax/r(i))
           Vr(i,j)=Vr(i,j)/dsqrt(1000.0d0)  ! 多分これが必要 [m] -> [km]
        end if
        if(j==1)then
           if(present(Vt_0))then
              Vt_0(i)=Vt(i,j)
           end if
           if(present(Vr_0))then
              Vr_0(i)=Vr(i,j)
           end if
        end if
        if(rad_opt.eqv..true.)then
           Vt(i,j)=Vt(i,j)+tmp_vtp*radp_coef
           Vr(i,j)=Vr(i,j)+tmp_vrp*radp_coef
        else
           Vt(i,j)=Vt(i,j)+tmp_vtp
           Vr(i,j)=Vr(i,j)+tmp_vrp
        end if
     end do
  end do

end subroutine prod_vortex_structure

subroutine prod_radar_along_vel()
  implicit none

end subroutine prod_radar_along_vel

subroutine conv_VtVr2VxVy( r, t, Vt, Vr, Vx, Vy )
!-- convert Vt and Vr to Vx and Vy on R-T coordinates
  implicit none
  double precision, intent(in) :: r(:)  ! R-coordinate [m]
  double precision, intent(in) :: t(:)  ! T-coordinate [rad]
  double precision, intent(in) :: Vt(size(r),size(t))   ! tangential wind component on R-T coordinates
  double precision, intent(in) :: Vr(size(r),size(t))   ! radial wind component on R-T coordinates
  double precision, intent(out) :: Vx(size(r),size(t))  ! X-component of wind on R-T coordinates
  double precision, intent(out) :: Vy(size(r),size(t))  ! Y-component of wind on R-T coordinates
  integer :: nr, nt, i, j

  nr=size(r)
  nt=size(t)

  do j=1,nt
     do i=1,nr
        Vx(i,j)=Vr(i,j)*dcos(t(j))-Vt(i,j)*dsin(t(j))
        Vy(i,j)=Vr(i,j)*dsin(t(j))+Vt(i,j)*dcos(t(j))
     end do
  end do

end subroutine conv_VtVr2VxVy

subroutine conv_VxVy2VtVr( r, t, Vx, Vy, Vt, Vr )
!-- convert Vx and Vy to Vr and Vt on R-T coordinates
  implicit none
  double precision, intent(in) :: r(:)  ! R-coordinate [m]
  double precision, intent(in) :: t(:)  ! T-coordinate [rad]
  double precision, intent(in) :: Vx(size(r),size(t))  ! X-component of wind on R-T coordinates
  double precision, intent(in) :: Vy(size(r),size(t))  ! Y-component of wind on R-T coordinates
  double precision, intent(out) :: Vt(size(r),size(t))   ! tangential wind component on R-T coordinates
  double precision, intent(out) :: Vr(size(r),size(t))   ! radial wind component on R-T coordinates
  integer :: nr, nt, i, j

  nr=size(r)
  nt=size(t)

  do j=1,nt
     do i=1,nr
        Vr(i,j)=Vx(i,j)*dcos(t(j))+Vy(i,j)*dsin(t(j))
        Vt(i,j)=-Vx(i,j)*dsin(t(j))+Vy(i,j)*dcos(t(j))
     end do
  end do

end subroutine conv_VxVy2VtVr

subroutine proj_VxVy2Vraxy( x, y, rax, ray, Vx, Vy, Vraxy, undef )
!-- calculate Vx and Vy to Vd along with radar beams on X-Y coodinates
  implicit none
  double precision, intent(in) :: x(:)  ! X-coordinate
  double precision, intent(in) :: y(:)  ! Y-coordinate
  double precision, intent(in) :: rax   ! X-coodinate of radar location
  double precision, intent(in) :: ray   ! Y-coodinate of radar location
  double precision, intent(in) :: Vx(size(x),size(y))  ! X-component of wind on X-Y coordinates
  double precision, intent(in) :: Vy(size(x),size(y))  ! Y-component of wind on X-Y coordinates
  double precision, intent(out) :: Vraxy(size(x),size(y))  ! velocity along with beam on X-Y coordinates
  double precision, intent(in), optional :: undef
  integer :: nx, ny, i, j
  double precision :: rad

  nx=size(x)
  ny=size(y)

  if(present(undef))then
     Vraxy=undef
     do j=1,ny
        do i=1,nx
           rad=dsqrt((x(i)-rax)**2+(y(j)-ray)**2)
           if(rad>0.0d0)then
              if(Vx(i,j)/=undef.and.Vy(i,j)/=undef)then
                 Vraxy(i,j)=((x(i)-rax)/rad)*Vx(i,j)+((y(j)-ray)/rad)*Vy(i,j)
              end if
           end if
        end do
     end do
  else
     Vraxy=0.0d0
     do j=1,ny
        do i=1,nx
           rad=dsqrt((x(i)-rax)**2+(y(j)-ray)**2)
           if(rad>0.0d0)then
              Vraxy(i,j)=((x(i)-rax)/rad)*Vx(i,j)+((y(j)-ray)/rad)*Vy(i,j)
           end if
        end do
     end do
  end if

end subroutine proj_VxVy2Vraxy

subroutine proj_VtVr2Vrart( r, t, td, Vt, Vr, Vra, undef )
!-- convert Vt and Vr to Vx and Vy on R-T coordinates
  implicit none
  double precision, intent(in) :: r(:)  ! R-coordinate [m]
  double precision, intent(in) :: t(:)  ! T-coordinate [rad]
  double precision, intent(in) :: td(size(r),size(t))   ! radar azimuthal angle on R-T coordinate [rad]
  double precision, intent(in) :: Vt(size(r),size(t))   ! tangential wind component on R-T coordinates
  double precision, intent(in) :: Vr(size(r),size(t))   ! radial wind component on R-T coordinates
  double precision, intent(out) :: Vra(size(r),size(t))  ! Velocity along beam on R-T coordinates
  double precision, intent(in), optional :: undef  ! undefined value
  integer :: nr, nt, i, j

  nr=size(r)
  nt=size(t)

  do j=1,nt
     do i=1,nr
        Vra(i,j)=-Vt(i,j)*dsin(t(j)-td(i,j))+Vr(i,j)*dcos(t(j)-td(i,j))
     end do
  end do

end subroutine proj_VtVr2Vrart

double precision function line_integral( nr, rdh, gkrr, div_r, undef )
!-- calculate a line integral (actually, sum for arguments)
  implicit none
  integer, intent(in) :: nr
  double precision, intent(in) :: rdh(nr+1)
  double precision, intent(in) :: gkrr(nr+1)
  double precision, intent(in) :: div_r(nr+1)
  double precision, intent(in), optional :: undef
  integer :: ii, jj
  double precision :: tmpval
  double precision :: vareps(nr+1)

  tmpval=0.0d0
  vareps=1.0d0
  vareps(1)=0.5d0
  vareps(nr+1)=0.5d0

  do ii=1,nr+1
     tmpval=tmpval+vareps(ii)*rdh(ii)*gkrr(ii)*div_r(ii)
  end do

  line_integral=tmpval

  return

end function line_integral

!subroutine prod_VM( x, y, us, vs, Usx, Vsy )
!!-- produce environmental wind on X-Y coordinates
!  implicit none
!  double precision, intent(in) :: x(:)  ! X-coordinate
!  double precision, intent(in) :: y(:)  ! Y-coordinate
!  double precision, intent(in) :: um0   ! coefficient for X-component of environmental wind
!  double precision, intent(in) :: vm0   ! coefficient for Y-component of environmental wind
!  double precision, intent(in) :: um1   ! coefficient 1 of environmental wind
!  double precision, intent(in) :: um2   ! coefficient 2 of environmental wind
!  double precision, intent(in) :: vm1   ! coefficient 3 of environmental wind
!  double precision, intent(out) :: Um(size(x),size(y))  ! X-component of environmental wind
!  double precision, intent(out) :: Vm(size(x),size(y))  ! Y-component of environmental wind
!  integer :: nx, ny, i, j
!
!  nx=size(x)
!  ny=size(y)
!
!  do j=1,ny
!     do i=1,nx
!        Um(i,j)=um0+um1*x(i)+um2*y(j)
!        Vm(i,j)=vm0+vm1*x(i)-um1*y(j)
!     end do
!  end do
!
!end subroutine prod_VM

!subroutine norm_VxVy2Vrnxy( x, y, rax, ray, Vx, Vy, Vrnxy, undef )
!!-- calculate Vx and Vy to Vn normal to radar beams on X-Y coodinates
!  implicit none
!  double precision, intent(in) :: x(:)  ! X-coordinate
!  double precision, intent(in) :: y(:)  ! Y-coordinate
!  double precision, intent(in) :: rax   ! X-coodinate of radar location
!  double precision, intent(in) :: ray   ! Y-coodinate of radar location
!  double precision, intent(in) :: Vx(size(x),size(y))  ! X-component of wind on X-Y coordinates
!  double precision, intent(in) :: Vy(size(x),size(y))  ! Y-component of wind on X-Y coordinates
!  double precision, intent(out) :: Vrnxy(size(x),size(y))  ! velocity normal to beam on X-Y coordinates
!  double precision, intent(in), optional :: undef
!  integer :: nx, ny, i, j
!  double precision :: rad
!
!  nx=size(x)
!  ny=size(y)
!
!  if(present(undef))then
!     Vrnxy=undef
!     do j=1,ny
!        do i=1,nx
!           rad=dsqrt((x(i)-rax)**2+(y(j)-ray)**2)
!           if(rad>0.0d0)then
!              if(Vx(i,j)/=undef.and.Vy(i,j)/=undef)then
!                 Vrnxy(i,j)=((x(i)-rax)/rad)*Vy(i,j)-((y(j)-ray)/rad)*Vx(i,j)
!              end if
!           end if
!        end do
!     end do
!  else
!     Vrnxy=0.0d0
!     do j=1,ny
!        do i=1,nx
!           rad=dsqrt((x(i)-rax)**2+(y(j)-ray)**2)
!           if(rad>0.0d0)then
!              Vrnxy(i,j)=((x(i)-rax)/rad)*Vy(i,j)-((y(j)-ray)/rad)*Vx(i,j)
!           end if
!        end do
!     end do
!  end if
!
!end subroutine norm_VxVy2Vrnxy

double precision function green_func( rc, r, nval )
!-- def: -(1/(2*nval))*(r/rc)^nval, r<rc
!-- def: -(1/(2*nval))*(rc/r)^nval, r>=rc
  implicit none
  double precision, intent(in) :: rc
  double precision, intent(in) :: r
  integer, intent(in) :: nval
  double precision :: res, nr

  nr=dble(nval)
  if(rc<0.0d0.or.r<0.0d0)then
     call stdout( "rc and r must not be negative", "green_func", 1 )
     write(*,*) "rc and r = ", rc, r
  end if
  if(r==rc)then
     res=-(0.5d0/nr)
  else
     if(r<rc)then
        res=-(0.5d0/nr)*((r/rc)**nval)
     else  ! (r/rc)**nval == exp(nval*(log(r/rc)))
        if(rc==0.0d0)then
           res=0.0d0
        else
           res=-(0.5d0/nr)*exp(dble(nval)*(log(rc)-log(r)))
        end if
     end if
  end if

  green_func=res

  return

end function green_func

subroutine conv_d2r_1d( ival, oval )
!-- convert double to real
  implicit none
  double precision, intent(in) :: ival(:)
  real, intent(out) :: oval(size(ival))
  integer :: ii, ni

  ni=size(ival)

  do ii=1,ni
     oval(ii)=real(ival(ii))
  end do

end subroutine conv_d2r_1d

subroutine conv_d2r_2d( ival, oval )
!-- convert double to real
  implicit none
  double precision, intent(in) :: ival(:,:)
  real, intent(out) :: oval(size(ival,1),size(ival,2))
  integer :: ii, jj, ni, nj

  ni=size(ival,1)
  nj=size(ival,2)

  do jj=1,nj
     do ii=1,ni
        oval(ii,jj)=real(ival(ii,jj))
     end do
  end do

end subroutine conv_d2r_2d

subroutine subst_2d( ioval, ival, undef )
!-- subtract ival
  implicit none
  double precision, intent(inout) :: ioval(:,:)
  double precision, intent(in) :: ival(size(ioval,1),size(ioval,2))
  double precision, intent(in), optional :: undef
  integer :: ii, jj, ni, nj

  ni=size(ioval,1)
  nj=size(ioval,2)

  if(present(undef))then
     do jj=1,nj
        do ii=1,ni
           if(ioval(ii,jj)/=undef.and.ival(ii,jj)/=undef)then
              ioval(ii,jj)=ioval(ii,jj)-real(ival(ii,jj))
           else
              ioval(ii,jj)=undef
           end if
        end do
     end do
  else
     do jj=1,nj
        do ii=1,ni
           ioval(ii,jj)=ioval(ii,jj)-ival(ii,jj)
        end do
     end do
  end if

end subroutine subst_2d

subroutine subst_2d_r( ioval, ival, undef )
!-- subtract ival
  implicit none
  real, intent(inout) :: ioval(:,:)
  real, intent(in) :: ival(size(ioval,1),size(ioval,2))
  real, intent(in), optional :: undef
  integer :: ii, jj, ni, nj

  ni=size(ioval,1)
  nj=size(ioval,2)

  if(present(undef))then
     do jj=1,nj
        do ii=1,ni
           if(ioval(ii,jj)/=undef.and.ival(ii,jj)/=undef)then
              ioval(ii,jj)=ioval(ii,jj)-real(ival(ii,jj))
           else
              ioval(ii,jj)=undef
           end if
        end do
     end do
  else
     do jj=1,nj
        do ii=1,ni
           ioval(ii,jj)=ioval(ii,jj)-ival(ii,jj)
        end do
     end do
  end if

end subroutine subst_2d_r

subroutine rearrange_3d_2d( val3d, val2d )
!-- rearrange 3d variable to 2d variable (k,i,j -> i*j,k)
  implicit none
  double precision, intent(in) :: val3d(:,:,:)
  double precision, intent(out) :: val2d(size(val3d,2)*size(val3d,3),size(val3d,1))
  integer :: ii, jj, kk, ni, nj, nk

  nk=size(val3d,1)
  ni=size(val3d,2)
  nj=size(val3d,3)

  do kk=1,nk
     do jj=1,nj
        do ii=1,ni
           val2d(ni*(jj-1)+ii,kk)=val3d(kk,ii,jj)
        end do
     end do
  end do

end subroutine rearrange_3d_2d

subroutine rearrange_2d_1d( val2d, val1d )
!-- rearrange 2d variable to 1d variable (i,j -> i*j)
  implicit none
  double precision, intent(in) :: val2d(:,:)
  double precision, intent(out) :: val1d(size(val2d,1)*size(val2d,2))
  integer :: ii, jj, ni, nj

  ni=size(val2d,1)
  nj=size(val2d,2)

  do jj=1,nj
     do ii=1,ni
        val1d(ni*(jj-1)+ii)=val2d(ii,jj)
     end do
  end do

end subroutine rearrange_2d_1d

subroutine display_2valdiff_max( val1, val2, undef, cout )
!-- display the maximum of the difference between val1 and val2
  implicit none
  double precision, intent(inout) :: val1(:,:)
  double precision, intent(in) :: val2(size(val1,1),size(val1,2))
  double precision, intent(in), optional :: undef
  character(*), intent(out), optional :: cout
  integer :: ii, jj, ni, nj, maxi, maxj
  double precision :: maxv, dval

  ni=size(val1,1)
  nj=size(val1,2)
  maxv=0.0d0
  maxi=0
  maxj=0

  if(present(undef))then
     do jj=1,nj
        do ii=1,ni
           if(val1(ii,jj)/=undef.and.val2(ii,jj)/=undef)then
              dval=dabs(val1(ii,jj)-val2(ii,jj))
              if(maxv<dval)then
                 maxv=dval
                 maxi=ii
                 maxj=jj
              end if
           end if
        end do
     end do
  else
     do jj=1,nj
        do ii=1,ni
           dval=dabs(val1(ii,jj)-val2(ii,jj))
           if(maxv<dval)then
              maxv=dval
              maxi=ii
              maxj=jj
           end if
        end do
     end do
  end if

  write(*,'(a21,1PE16.8,a5,i4,a1,i4,a2)') "Maximum difference = ", maxv,  &
  &                           " at (", maxi, ",", maxj, ")."
  if(present(cout))then
     write(cout,'(1PE8.1)') maxv
  end if

end subroutine display_2valdiff_max

subroutine stdout( message, routine_name, mtype )
!-- standard output for message
  implicit none
  character(*), intent(in) :: message  ! output message
  character(*), intent(in) :: routine_name  ! called routine name
  integer, intent(in) :: mtype   ! message type
                                 ! 0: message, -1: error, 1: warning
  character(100) :: tname, forma
  character(10000) :: all_mess

  tname=''
  all_mess=''
  select case (mtype)
  case (0)  ! message
     tname(1:7)="MESSAGE"
  case (-1)  ! error
     tname(1:5)="ERROR"
  case (1)  ! warning
     tname(1:7)="WARNING"
  end select

  all_mess="*** "//trim(adjustl(tname))//"("//trim(adjustl(routine_name))  &
  &              //") *** : "//trim(adjustl(message))

  write(forma,*) len_trim(adjustl(all_mess))
  forma='(a'//trim(adjustl(forma))//')'
  write(6,trim(adjustl(forma))) trim(adjustl(all_mess))

end subroutine stdout

end module sub_mod
