program aliasing_test

  use dcl
  use Dcl_Automatic
  use statistics
  use max_min

  implicit none
  integer, parameter :: nx=512  ! サンプリング数
  integer, parameter :: nl=3  ! 描画用のラインの本数
  integer, parameter :: nwidth=10  ! コヒーレント分布を検出する幅
  real, parameter :: pi=3.141592
  real, parameter :: Vdmax=80.0  ! オリジナルドップラー速度の振幅
  real, parameter :: Vd_thres=35.0  ! 折り返し速度
  real, parameter :: fact_Nq_thres=0.8  ! Vd_thres の周辺判定の閾値
  real, parameter :: undef=-1000.0  ! 欠損値
  real, parameter :: vmax=120.0  ! 非現実的な風速の閾値
  integer :: i, j
  integer :: idslope_max, itmp
  integer, dimension(nl) :: lidx, ltyp
  real :: dslope, dslope_max, Vdmmax, Vdmmin, dinterp
  real, dimension(nx) :: Vdo, Vdm, Vdt, theta
  real, dimension(nx,nl) :: xline, yline

  lidx=(/14,24,44/)
  ltyp=(/1,1,2/)

  theta=(/((2.0*pi*(real(i-1)/real(nx))),i=1,nx)/)
  Vdt=(/((Vdmax*sin(theta(i))),i=1,nx)/)
  Vdo=Vdt

  do i=1,nx
     if(Vdo(i)<-Vd_thres)then
        Vdo(i)=Vdt(i)+2.0*Vd_thres
     end if
     if(Vdo(i)>Vd_thres)then
        Vdo(i)=Vdt(i)-2.0*Vd_thres
     end if
  end do

  !-- クラッターノイズ or 欠損
  Vdo(nx/5:nx/3)=0.1*sin(100.0*theta(nx/5:nx/3))
  !Vdo(nx/5:nx/3)=undef

  Vdm=Vdo

  !-- 不連続角度の検出 and 基準ドップラー速度となる角度の検出

  dslope=0.0
  dslope_max=0.0
  idslope_max=0
  do i=nwidth+1,nx-nwidth
     !-- コヒーレントなドップラー速度変化を検出する.
     !-- 1. 2*nwidth の範囲で, 両端が欠損していないデータセクターを判定
     if(Vdm(i-nwidth)/=undef.and.Vdm(i+nwidth)/=undef)then
        !-- 2*nwidth の中に fact_Nq_thres 以上の値をもつ折り返しの
        !-- 可能性があるデータがないかチェック
        call max_val_1d( Vdm(i-nwidth:i+nwidth), itmp, Vdmmax, undef=undef )
        call min_val_1d( Vdm(i-nwidth:i+nwidth), itmp, Vdmmin, undef=undef )
        if((Vdmmax>=fact_Nq_thres*Vd_thres).and.(Vdmmin<=-fact_Nq_thres*Vd_thres))then
           cycle
        end if
        !-- 両端が異符号の判定 (ゼロをまたいでいるか)
        if(Vdm(i-nwidth)*Vdm(i+nwidth)<0.0)then
           !-- 2*nwidth の範囲で線形の傾きを計算
           call LSM_1df( theta(i-nwidth:i+nwidth), Vdm(i-nwidth:i+nwidth),  &
                   &     dslope, dinterp, undef=undef )
           !-- 計算した傾きの絶対値が過去より大きければ更新
           if(dslope_max<abs(dslope))then
              dslope_max=abs(dslope)
              idslope_max=i
           end if
        end if
     end if
  end do

  write(*,*) "idcheck", idslope_max
  write(*,*) "check", Vdm(idslope_max-nwidth:idslope_max+nwidth)
  !stop
  !-- idslope_max から前方にサーベイ
  do i=idslope_max,nx-1
     if(Vdm(i)/=undef.and.Vdm(i+1)/=undef)then
        if((Vdm(i)<-fact_Nq_thres*Vd_thres).and.  &
  &        (abs(Vdm(i+1)-Vdm(i))>2.0*fact_Nq_thres*Vd_thres))then  ! i+1 を -2Vd_thres
           Vdm(i+1)=Vdm(i+1)-2.0*Vd_thres
        else if((Vdm(i)>fact_Nq_thres*Vd_thres).and.  &
  &        (abs(Vdm(i+1)-Vdm(i))>2.0*fact_Nq_thres*Vd_thres))then  ! i+1 を +2Vd_thres
           Vdm(i+1)=Vdm(i+1)+2.0*Vd_thres
        end if
     end if
  end do

  !-- idslope_max から後方にサーベイ
  do i=idslope_max,2,-1
     if(Vdm(i)/=undef.and.Vdm(i-1)/=undef)then
        if((Vdm(i)<-fact_Nq_thres*Vd_thres).and.  &
  &        (abs(Vdm(i)-Vdm(i-1))>2.0*fact_Nq_thres*Vd_thres))then  ! i-1 を -2Vd_thres
           Vdm(i-1)=Vdm(i-1)-2.0*Vd_thres
        else if((Vdm(i)>fact_Nq_thres*Vd_thres).and.  &
  &        (abs(Vdm(i)-Vdm(i-1))>2.0*fact_Nq_thres*Vd_thres))then  ! i-1 を +2Vd_thres
           Vdm(i-1)=Vdm(i-1)+2.0*Vd_thres
        end if
     end if
  end do

  !-- 最大最小値を計算し, 非現実的な強風速が出た場合, 全体を弱い方向にオフセット
  call max_val_1d(Vdm, itmp, Vdmmax, undef=undef)
  call min_val_1d(Vdm, itmp, Vdmmin, undef=undef)
  if(Vdmmax>vmax)then
     call offset( Vdm, -2.0*Vd_thres, undef=undef )
  else if(Vdmmin<-vmax)then
     call offset( Vdm, 2.0*Vd_thres, undef=undef )
  end if

  xline(1:nx,1)=theta(1:nx)*180.0/pi
  xline(1:nx,2)=xline(1:nx,1)
  xline(1:nx,3)=xline(1:nx,1)
  yline(1:nx,1)=Vdt(1:nx)
  yline(1:nx,2)=Vdm(1:nx)
  yline(1:nx,3)=Vdo(1:nx)

!-- dcl 各種設定
!-- 高品位フォント設定 ---
  call SGISET('IFONT', 1 )
  call SWLSET( 'LSYSFNT', .true. )
  call SWLSET( 'LCMCH', .true. )
!  call UZFACT( 0.9 )
  call DclSetTextHeight( 0.025 )
  call DclSetParm( 'ENABLE_CONTOUR_MESSAGE', .false. )
  call GLLSET( 'LMISS', .true. )
  call GLRSET( 'RMISS', undef )

  call DclOpenGraphics(1)

  CALL SWCSET('FONTNAME', 'Nimbus Sans 12')

  call Dcl_PL( 'l', '', xline, yline, xline, yline,  &
  &            (/'Angle (degree)    ', 'V\_{d} (m s\^{-1})'/),  &
  &            mxitv=(/60.0, 30.0/), myitv=(/10.0, 5.0/),  &
  &            l_idx=lidx, l_typ=ltyp )

  call DclDrawTextNormalized( 0.82, 0.8, 'V\_{d}\^{obs}', height=0.02,  &
          &                   centering=-1, index=44 )
  call DclDrawTextNormalized( 0.82, 0.77, 'V\_{d}\^{corr}', height=0.02,  &
          &                   centering=-1, index=24 )
  call DclDrawTextNormalized( 0.82, 0.74, 'V\_{d}\^{true}', height=0.02,  &
          &                   centering=-1, index=14 )

  call DclCloseGraphics

contains

subroutine offset( ioval, off, undef )
  implicit none
  real, intent(inout) :: ioval(:)
  real, intent(in) :: off
  real, intent(in), optional :: undef
  real :: defun
  integer :: ii, ix

  ix=size(ioval)

  if(present(undef))then
     defun=undef
  else
     defun=-1000.0
  end if

  do ii=1,ix
     if(ioval(ii)/=defun)then
        ioval(ii)=ioval(ii)+off
     end if
  end do

end subroutine offset

end program aliasing_test
