program vtg2p
!-- テキスト形式の接線風速分布から, 同じ半径の気圧を
!-- 傾度風バランスで求める.
!-- ファイルフォーマット
!-- 1 列目= 半径, 2 列目= 接線風速

  use basis
  use Math_Const
  use Phys_Const
  use file_operate
  use algebra
  use map_function
  use statistics

  implicit none

  real, parameter :: rhoc=1.15  ! From Shimada et al. (2016; MWR)
!-- namelist variables
  real :: tclon, tclat  ! 台風中心(lon,lat)
  real :: obslon, obslat, obsp  ! アメダス観測点 (lon,lat), アメダス海面気圧 (hPa)
  real :: undef  ! 未定義値
  character(1) :: int_opt  ! 未定義半径の内挿方法
                           ! r: 剛体回転, s: 3 次スプライン, n: その範囲は計算しない
  character(1000) :: fname  ! アスキーテキストファイル名
  character(1000) :: outfname  ! 出力ファイル名

  integer :: i, nl, iref, int_point
  real :: d2r, fc, dp, refr
  real, allocatable, dimension(:) :: r, vtg, pres, fv
  character(20), allocatable, dimension(:,:) :: cval

  namelist /input /tclon, tclat, obslon, obslat, obsp, undef, fname, outfname,  &
  &                int_opt
  read(5,nml=input)

  d2r=pi/180.0
  obsp=obsp*100.0

  nl=line_number_counter( trim(adjustl(fname)) )
  allocate(cval(2,nl))
  allocate(r(nl))
  allocate(vtg(nl))
  allocate(pres(nl))
  allocate(fv(nl))
  call read_file_text( trim(adjustl(fname)), 2, nl, cval )

  do i=1,nl
     r(i)=c2r_convert( trim(adjustl(cval(1,i))) )
     vtg(i)=c2r_convert( trim(adjustl(cval(2,i))) )
  end do

  fc=2.0*omega*sin(tclat*d2r)
  refr=ll2radi( tclon*d2r, tclat*d2r, obslon*d2r, obslat*d2r )
  call interpo_search_1d( r, refr, iref )

  !-- 未定義域の内挿
  select case (int_opt)
  case ('r')  ! 剛体回転仮定
     write(*,*) "Interpolating with rigid-body rotation"
     int_point=iref
     do i=iref,1,-1
        if(vtg(i)==undef)then
           if(r(i)==0.0)then
              vtg(i)=0.0
           else
              vtg(i)=vtg(int_point)*(r(i)/r(int_point))
           end if
        else
           int_point=i
        end if
     end do
  case default
     write(*,*) "No interpolation"
  end select

  do i=1,nl
     if(r(i)>0.0)then
        fv(i)=rhoc*(fc*vtg(i)+vtg(i)**2/r(i))
     else
        fv(i)=0.0
     end if
  end do
  do i=iref,1,-1
     call rectangle_int( r, fv, r(i), refr, dp, undeff=undef )
     pres(i)=obsp-dp
  end do
  do i=iref+1,nl
     call rectangle_int( r, fv, refr, r(i), dp, undeff=undef )
     pres(i)=obsp+dp
  end do

  open(unit=100,file=trim(adjustl(outfname)),status='unknown')
  write(100,'(a48)') "'Radius'        'Velocity'      'pressure'      "
  write(100,'(a48)') "'m'             'ms-1'          'hPa'           "
  do i=1,nl
     write(100,'(1P3E16.8)') r(i), vtg(i), pres(i)*0.01
  end do
  close(unit=100)

  write(*,*) "Finish: output "//trim(adjustl(outfname))
  write(*,*) "Distance between OBS to TCcenter: "  &
  &          //trim(adjustl( r2c_convert(refr*0.001, forma='(1PE16.8)') ))//" km."

end program
