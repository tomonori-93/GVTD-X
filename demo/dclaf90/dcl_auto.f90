module Dcl_Automatic  ! Dclf90 の描画を自動で行うモジュール

use dcl

  type dclatime  ! 開始日の日付
     integer :: year_d  ! 西暦
     integer :: month_d  ! 月
     integer :: day_d  ! 日
     integer :: hour_d  ! 時
     integer :: min_d  ! 分
     integer :: sec_d  ! 秒
  end type dclatime

contains

subroutine Dcl_2D_cont_shade( outname,  &
  &  x, y, contour, shade, cont_int, shade_int,  &
  &  axis_title, form_type, viewx_int, viewy_int, c_num,  &
!  &  viewy_min, viewy_max, color_num, cont_num, nongrid,  &
  &  xg, yg, zg, xp, yp, zp, mxitv, myitv,  &
  &  mono, mono_val, mono_lev, trigleg, no_tone, no_frame,  &
  &  l_idx, l_typ, p_idx, p_typ, p_siz, xlogf, ylogf, tonbf,  &
  &  axlbl, axtck, cxlbl, aylbl, aytck, cylbl )
  ! 2 次元で 2 変数を等値線とカラーシェードで描画する.
  use dcl
  implicit none
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! x 方向の格子点座標
  real, intent(in) :: y(:)  ! y 方向の格子点座標
  real, intent(inout) :: contour(size(x),size(y))  ! 等値線に描く配列
  real, intent(inout) :: shade(size(x),size(y))  ! カラーシェードに描く配列
  real, intent(in) :: cont_int(2)  ! 等値線の上下端 
                                 ! [cont_int(1)=cont_min, cont_int(2)=cont_max]
  real, intent(in) :: shade_int(2)  ! 等値線の上下端 [shade_int(1)=shade_min,
                                 ! shade_int(2)=shade_max]
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                                 ! axis_title(1)=x_title, axis_title(2)=y_title
  character(*), intent(in) :: form_type(2)  ! フォーマット
                           ! form_type(1)=form_typec, form_type(2)=form_types
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  integer, intent(in), optional :: c_num(2)  ! コンター・シェードの数
                           ! c_num(1)=cont_num, c_num(2)=color_num
!  character(2), intent(in), optional :: nongrid  ! 不等間隔格子にするか.
                                        ! nongrid = 'ox' で判断.
                                        ! 1 文字目が横軸, 2 文字目が縦軸.
                                        ! o = 不等間隔, x = 等間隔.
                                        ! デフォルトでは 'xx'.
  real, intent(in), optional :: xg(:,:)  ! x 軸に入れるグリッド線の座標
  real, intent(in), optional :: yg(:,:)  ! y 軸に入れるグリッド線の座標
                    ! 第一要素が線の位置データで, 複数本描く場合は,
                    ! 第二要素を 2 個以上にして描く.
                    ! 配列に入れるデータ次第で直線ではなく, 曲線グリッドを
                    ! 描くことも可能.
                    ! 3 本の線を描く場合は, xg(:,1) と yg(:,1) で 1 本の
                    ! 線を表すように指定すること.
  real, intent(in), optional :: zg(:,:)  ! グリッド線が値をもっていればその値.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラー折れ線を描くモードに移行する.
  real, intent(in), optional :: xp(:,:)  ! x 方向にマーカーを入れる x 座標
  real, intent(in), optional :: yp(:,:)  ! y 方向にマーカーを入れる y 座標
  real, intent(in), optional :: zp(:,:)  ! マーカーが値をもっていればその値.
                                         ! これらの指定方法は線と同じ.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラーマーカーを描くモードに移行する.
  real, intent(in), optional :: mxitv(2)   ! 横軸の目盛り間隔 (mxitv(1):大目盛り)
  real, intent(in), optional :: myitv(2)   ! 縦軸の目盛り間隔 (myitv(1):大目盛り)
  logical, intent(in), optional :: mono  ! モノトーンの階調にする [.true.]
                                         ! デフォルトは .false.
  real, intent(in), optional :: mono_val(:)  ! 階調の境界値.
                    ! mono=.true. のときに必ず設定しないとエラーを返す.
                    ! 値は mono_lev + 1 成分存在しなければならない.
  integer, intent(in), optional :: mono_lev(:)  ! トーンマップ番号. dcl の 3 桁
                    ! mono=.true. のときに設定しないとエラーを返す.
  character(1), intent(in), optional :: trigleg  ! トーンバーの三角形オプション.
                ! オプションの値は, tone_bar ルーチンの trigle と同じ.
  logical, intent(in), optional :: no_tone  ! トーンバーを作成しないオプション
                                   ! .false. = 作成する. .true. = 作成しない.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(:)  ! xg, yg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: l_typ(:)  ! xg, yg で描く線のタイプ.
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: p_idx(:)  ! xp, yp で描くマーカーのインデックス
  integer, intent(in), optional :: p_typ(:)  ! xg, yg で描くマーカーのタイプ.
  real, intent(in), optional :: p_siz(:)  ! xg, yg で描くマーカーのサイズ.
                                                      ! デフォルトは 0.01.
  logical, intent(in), optional :: xlogf  ! X 座標対数フラグ, デフォルトは false.
  logical, intent(in), optional :: ylogf  ! Y 座標対数フラグ, デフォルトは false.
  character(1), intent(in), optional :: tonbf  ! UETONE FLAG (a,b,e,f,..)
                                          ! default 'f' = UETONF
  real, intent(in), optional :: axlbl(:)  ! X 座標のラベル描画での座標値
  real, intent(in), optional :: axtck(:)  ! X 座標の小目盛での座標値
  character(*), intent(in), optional :: cxlbl(:)  ! X 座標のラベル描画
  real, intent(in), optional :: aylbl(:)  ! Y 座標のラベル描画での座標値
  real, intent(in), optional :: aytck(:)  ! Y 座標の小目盛での座標値
  character(*), intent(in), optional :: cylbl(:)  ! Y 座標のラベル描画
!-- 以上, 引数
  real :: cont_min  ! 等値線を描く最小値
  real :: cont_max  ! 等値線を描く最大値
  real :: shade_min  ! シェードを描く最小値
  real :: shade_max  ! シェードを描く最大値
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  character(10) :: form_typec  ! contour 用のフォーマット
  character(10) :: form_types  ! shade 用のフォーマット
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
  integer :: cont_num  ! 等値線の数
  integer :: color_num  ! カラーの数
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k  ! 作業用添字
  integer :: nx, ny, trans_num
  real :: undef, RMISS
  character(20) :: fmt_mxitv, fmt_myitv
  logical :: monoto, no_tone_flag, no_frame_flag, ax_log_flag(2)
  logical :: mxitv_flag, myitv_flag, cxlbl_flag, cylbl_flag

  nx=size(x)
  ny=size(y)

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(c_num))then
     cont_num=c_num(1)
     color_num=c_num(2)
  else
     cont_num=10
     color_num=56
  end if

  if(present(no_tone))then
     no_tone_flag=no_tone
  else
     no_tone_flag=.false.
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  if(present(xlogf))then
     ax_log_flag(1)=xlogf
  else
     ax_log_flag(1)=.false.
  end if

  if(present(ylogf))then
     ax_log_flag(2)=ylogf
  else
     ax_log_flag(2)=.false.
  end if

  if(ax_log_flag(1).eqv..true.)then
     if(ax_log_flag(2).eqv..true.)then
        trans_num=4
     else
        trans_num=3
     end if
  else
     if(ax_log_flag(2).eqv..true.)then
        trans_num=2
     else
        trans_num=1
     end if
  end if

  mxitv_flag=.false.
  myitv_flag=.false.
  if(present(mxitv))then
     if(mxitv(1)/=0.0.and.mxitv(2)/=0.0)then
        mxitv_flag=.true.
     end if
  end if
  if(present(myitv))then
     if(myitv(1)/=0.0.and.myitv(2)/=0.0)then
        myitv_flag=.true.
     end if
  end if

  cxlbl_flag=.false.
  cylbl_flag=.false.
  if(present(cxlbl))then
     if(len_trim(cxlbl(1))>0)then
        cxlbl_flag=.true.
        mxitv_flag=.false.
     end if
  end if
  if(present(cylbl))then
     if(len_trim(cylbl(1))>0)then
        cylbl_flag=.true.
        myitv_flag=.false.
     end if
  end if

!-- 引数の置き換え用変数に置き換え
  cont_min=cont_int(1)
  cont_max=cont_int(2)
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))
  form_typec=trim(form_type(1))
  form_types=trim(form_type(2))

  if(present(mono))then
     if(present(mono_val).and.present(mono_lev))then
        if(size(mono_val)-1==size(mono_lev))then
           monoto=mono
        else
           write(*,*) "*** ERROR ***"
           write(*,*) "[array number] : mono_val = mono_lev + 1"
           write(*,*) "STOP"
           stop
        end if
     else
        write(*,*) "*** ERROR ***"
        write(*,*) "When option MONO is true, MONO_VAL and MONO_LEV must be specified."
        write(*,*) "STOP"
        stop
     end if
  else
     monoto=.false.
  end if

!-- 処理ここまで ---

  call undef_CReSS2Dcl( nx, ny, 1, contour)
  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  call DclSetWindow( x(1), x(nx), y(1), y(ny) )

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 's', xg, yg, zg, color_num,  &
  &                      (/shade_min, shade_max/), subsubidx='l' )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              call DclScalingPoint( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  if(present(zp))then
     call color_line( 's', xp, yp, zp, color_num, (/shade_min, shade_max/),  &
  &                   subsubidx='p' )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           call DclScalingPoint( xp(:,i), yp(:,i) )
        end do
     end if
  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )

  call DclSetTransNumber( trans_num )
  call DclSetTransFunction

  if(monoto.eqv..true.)then
     call SGLSET( 'LSOFTF', .true. )
     call DclClearShadeLevel
     call DclSetShadeLevel( mono_val, mono_lev )
  else
     call SGLSET( 'LSOFTF', .false. )
!     call DclClearShadeLevel
!     call DclSetShadeLevel( shade_min, shade_max,   &
! &                          (shade_max-shade_min)/s_num )
  end if

!  if(present(nongrid))then
!     if(nongrid(1:1)=='o')then
        call DclSetXGrid( x )
!     end if
!     if(nongrid(2:2)=='o')then
        call DclSetYgrid( y )
!     end if
!  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(monoto.eqv..true.)then
     call DclShadeContour( shade )
  else
     if(present(tonbf))then
        select case (tonbf(1:1))
        case ('b')
           call UETONB( shade, nx, nx, ny )
        case ('c')
           call UETONC( shade, nx, nx, ny )
        case ('e')
           call UETONE( shade, nx, nx, ny )
        case ('f')
           call DclShadeContourEx( shade )
        end select
     else
        call DclShadeContourEx( shade )
     end if
  end if

  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )
  if(mxitv_flag.eqv..true.)then  ! automatic label format less than 1.0
     call auto_label_fmt( mxitv(1), x(nx), fmt_mxitv, x(1) )
     CALL USCSET( 'CXFMT', trim(adjustl(fmt_mxitv) ))
  end if
  if(myitv_flag.eqv..true.)then  ! automatic label format less than 1.0
     call auto_label_fmt( myitv(1), y(ny), fmt_myitv, y(1) )
     CALL USCSET( 'CYFMT', trim(adjustl(fmt_myitv) ))
  end if
  if(cxlbl_flag.eqv..true.)then
     call DclDrawAxisSpecify( 'b', axlbl, tick_pos=axtck, label=cxlbl )
     call DclDrawAxisSpecify( 't', axlbl, tick_pos=axtck )
  else if(mxitv_flag.eqv..true.)then
     call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
  else
     call DclDrawScaledAxis( 'bt' )
  end if
  if(cylbl_flag.eqv..true.)then
     call DclDrawAxisSpecify( 'l', aylbl, tick_pos=aytck, label=cylbl )
     call DclDrawAxisSpecify( 'r', aylbl, tick_pos=aytck )
  else if(myitv_flag.eqv..true.)then
     call DclDrawAxis( 'rl', myitv(1), myitv(2) )
  else
     call DclDrawScaledAxis( 'rl' )
  end if

  call DclDrawTitle( 'b', trim(x_title), 0.0 )
  call DclDrawTitle( 'l', trim(y_title), 0.0 )
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

  if(DclGetContourLevelNumber()==0)then
     call DclSetContourLabelFormat(trim(form_typec))
     call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
  end if

  call DclDrawContour( contour )

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 'l', xg, yg, zg, color_num, (/shade_min, shade_max/) )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              if(present(l_idx))then
                 call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
              end if
              if(present(l_typ))then
                 call DclSetLineType( l_typ(i) )
              end if
              call DclDrawLine( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(present(zp))then
     call color_line( 'p', xp, yp, zp, color_num, (/shade_min, shade_max/) )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           if(present(p_idx))then
              call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
           end if
           if(present(p_typ))then
              call DclSetMarkerType( p_typ(i) )
           end if
           if(present(p_siz))then
              call DclSetMarkerSize( p_siz(i) )
           end if
           call DclDrawMarker( xp(:,i), yp(:,i) )
        end do
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(no_tone_flag.eqv..false.)then
     if(present(trigleg))then
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto,  &
  &                  trigle=trigleg )
     else
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto )
     end if
  end if

  call DclClearContourLevel()

end subroutine Dcl_2D_cont_shade

!---------------------------------------------------------

subroutine Dcl_2D_cont_shade_MapPro( map_pro, outname,  &
  &  x, y, contour, shade, cont_int, shade_int,  &
  &  axis_title, form_type, viewx_int, viewy_int, c_num,  &
!  &  viewy_max, color_num, cont_num, nongrid,  &
  &  xg, yg, zg, long, latg, xp, yp, zp, mxitv, myitv, lonp, latp,  &
  &  mono, mono_val, mono_lev, trigleg,  &
  &  mlitv, mlidx, coast, border, blidx, bltyp,  &
  &  no_tone, no_frame, l_idx, l_typ, m_idx, m_typ,  &
  &  p_idx, p_typ, p_siz, mp_idx, mp_typ, mp_siz,  &
  &  t_posi, lon_wnd, lat_wnd, tonbf )
  ! 2 次元で 2 変数を等値線とカラーシェードで描画する.
  ! 引数 map_pro で地図番号を選択し, 地図投影モードに切り替える.
  use dcl
  implicit none
  integer, intent(in) :: map_pro  ! DCL の地図変換関数番号
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! x 方向の格子点座標 [deg]
  real, intent(in) :: y(:)  ! y 方向の格子点座標 [deg]
  real, intent(inout) :: contour(size(x),size(y))  ! 等値線に描く配列
  real, intent(inout) :: shade(size(x),size(y))  ! カラーシェードに描く配列
  real, intent(in) :: cont_int(2)  ! 等値線の上下端 
                                 ! [cont_int(1)=cont_min, cont_int(2)=cont_max]
  real, intent(in) :: shade_int(2)  ! 等値線の上下端 [shade_int(1)=shade_min,
                                 ! shade_int(2)=shade_max]
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                                 ! axis_title(1)=x_title, axis_title(2)=y_title
  character(*), intent(in) :: form_type(2)  ! フォーマット
                           ! form_type(1)=form_typec, form_type(2)=form_types
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  integer, intent(in), optional :: c_num(2)  ! コンター・シェードの数
                           ! c_num(1)=cont_num, c_num(2)=color_num
!  character(2), intent(in), optional :: nongrid  ! 不等間隔格子にするか.
                                        ! nongrid = 'ox' で判断.
                                        ! 1 文字目が横軸, 2 文字目が縦軸.
                                        ! o = 不等間隔, x = 等間隔.
                                        ! デフォルトでは 'xx'.
  real, intent(in), optional :: xg(:,:)  ! x 軸に入れるグリッド線の座標
  real, intent(in), optional :: yg(:,:)  ! y 軸に入れるグリッド線の座標
                    ! 第一要素が線の位置データで, 複数本描く場合は,
                    ! 第二要素を 2 個以上にして描く.
                    ! 配列に入れるデータ次第で直線ではなく, 曲線グリッドを
                    ! 描くことも可能.
                    ! 3 本の線を描く場合は, xg(:,1) と yg(:,1) で 1 本の
                    ! 線を表すように指定すること.
  real, intent(in), optional :: zg(:,:)  ! グリッド線が値をもっていればその値.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラー折れ線を描くモードに移行する.
  real, intent(in), optional :: long(:,:)  ! lon 座標で入れるグリッド線
  real, intent(in), optional :: latg(:,:)  ! lat 座標で入れるグリッド線
  real, intent(in), optional :: xp(:,:)  ! x 方向にマーカーを入れる x 座標
  real, intent(in), optional :: yp(:,:)  ! y 方向にマーカーを入れる y 座標
  real, intent(in), optional :: zp(:,:)  ! マーカーが値をもっていればその値.
                                         ! これらの指定方法は線と同じ.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラーマーカーを描くモードに移行する.
  real, intent(in), optional :: mxitv(2)   ! 横軸の目盛り間隔 (mxitv(1):大目盛り)
  real, intent(in), optional :: myitv(2)   ! 縦軸の目盛り間隔 (myitv(1):大目盛り)
  logical, intent(in), optional :: mono  ! モノトーンの階調にする [.true.]
                                         ! デフォルトは .false.
  real, intent(in), optional :: lonp(:,:)  ! lon 座標で入れるマーカー
  real, intent(in), optional :: latp(:,:)  ! lat 座標で入れるマーカー
  real, intent(in), optional :: mono_val(:)  ! 階調の境界値.
                    ! mono=.true. のときに必ず設定しないとエラーを返す.
                    ! 値は mono_lev + 1 成分存在しなければならない.
  integer, intent(in), optional :: mono_lev(:)  ! トーンマップ番号. dcl の 3 桁
                    ! mono=.true. のときに設定しないとエラーを返す.
  character(1), intent(in), optional :: trigleg  ! トーンバーの三角形オプション.
                ! オプションの値は, tone_bar ルーチンの trigle と同じ.
  real, intent(in), optional :: mlitv  ! メジャーラインの表示間隔 [degree]. デフォルトは 1 degree.
  integer, intent(in), optional :: mlidx  ! メジャーライン, 目盛のインデックス.
                                          ! デフォルトは 1.
  character(5), intent(in), optional :: coast  ! 海岸線選択引数
                                   ! ['japan'] = 日本域詳細版
                                   ! ['world'] = 全球版
                                   ! default = 'world'
  character(5), intent(in), optional :: border  ! 国, 州, 県境描画
                                   ! ['japan'] = 日本県境
                                   ! ['world'] = 世界国境
                                   ! ['state'] = 米国州境
                                   ! default = 描画しない.
  integer, intent(in), optional :: blidx  ! 海岸線, 国境のインデックス.
                                          ! デフォルトは 3.
  integer, intent(in), optional :: bltyp  ! 海岸線, 国境のタイプ.
                                          ! デフォルトは 1.
  logical, intent(in), optional :: no_tone  ! トーンバーを作成しないオプション
                                   ! .false. = 作成する. .true. = 作成しない.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(:)  ! xg, yg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: l_typ(:)  ! xg, yg で描く線のタイプ.
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: m_idx(:)  ! long, latg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: m_typ(:)  ! long, latg で描く線のタイプ.
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: p_idx(:)  ! xp, yp で描くマーカーのインデックス
  integer, intent(in), optional :: p_typ(:)  ! xg, yg で描くマーカーのタイプ.
                                                      ! デフォルトは 1.
  real, intent(in), optional :: p_siz(:)  ! xg, yg で描くマーカーの大きさ.
                                                      ! デフォルトは 0.01.
  integer, intent(in), optional :: mp_idx(:)  ! lonp, latp で描くマーカーのインデックス
  integer, intent(in), optional :: mp_typ(:)  ! lonp, latp で描くマーカーのインデックス
  real, intent(in), optional :: mp_siz(:)  ! lonp, latp で描くマーカーのインデックス
  real, intent(in), optional :: t_posi(3)  ! map optiona がランベルトの場合
                                           ! t_posi=(/lat1, lat2, lon1/) で設定.
                                           ! 単位は degree
  real, intent(in), optional :: lon_wnd(:,:)  ! メルカトル系以外で矩形領域を設定
                   ! する場合の各座標系経緯度. この値が設定されるとき, 
                   ! x, y には, デカルト系での距離を与えておくこと.
  real, intent(in), optional :: lat_wnd(:,:)  ! メルカトル系以外で矩形領域を設定
                   ! する場合の各座標系経緯度. この値が設定されるとき, 
                   ! x, y には, デカルト系での距離を与えておくこと.
  character(1), intent(in), optional :: tonbf  ! UETONE FLAG (a,b,e,f,..)
                                          ! default 'f' = UETONF
!-- 以上, 引数
  real :: cont_min  ! 等値線を描く最小値
  real :: cont_max  ! 等値線を描く最大値
  real :: shade_min  ! シェードを描く最小値
  real :: shade_max  ! シェードを描く最大値
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  character(10) :: form_typec  ! contour 用のフォーマット
  character(10) :: form_types  ! shade 用のフォーマット
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
  integer :: cont_num  ! 等値線の数
  integer :: color_num  ! カラーの数
!-- 以上, 引数の置き換え用変数
  real, parameter :: pi=3.14159265
  real, parameter :: radius=6.38e6
  integer :: i, j, k  ! 作業用添字
  integer :: nx, ny
  real :: uratio
  real :: undef, RMIS
  real :: map_lat_min, map_lat_max, map_lon_min, map_lon_max
  real :: lat_min, lat_max, lon_min, lon_max
  real :: mlat2dis_min, mlat2dis_max, mlon2dis_min, mlon2dis_max, mditv, mid_p
  integer :: mdidx, bdidx, bdtyp
  real, dimension(2) :: vx_new, vy_new
  character(20) :: coast_sel
  character(20) :: border_sel
  character(20) :: fmt_mxitv, fmt_myitv
  logical :: monoto, no_tone_flag, no_frame_flag, bord_flag, mlitv_flag
  logical :: mxitv_flag, myitv_flag

  nx=size(x)
  ny=size(y)
  coast_sel=''
  border_sel=''
  bord_flag=.false.
  mlitv_flag=.false.
  mxitv_flag=.false.
  myitv_flag=.false.

!-- 引数を rad 単位に変換
  map_lon_min=x(1)*pi/180.0
  map_lon_max=x(nx)*pi/180.0
  map_lat_min=y(1)*pi/180.0
  map_lat_max=y(ny)*pi/180.0

  select case (map_pro)
  case (10)
     mlon2dis_min=map_lon_min
     mlon2dis_max=map_lon_max
     mlat2dis_min=map_lat_min
     mlat2dis_max=map_lat_max
  case (11)
     mlon2dis_min=map_lon_min
     mlon2dis_max=map_lon_max
     mlat2dis_min=log(tan(0.25*pi+0.5*map_lat_min))
     mlat2dis_max=log(tan(0.25*pi+0.5*map_lat_max))
  case (22)
     mlon2dis_min=x(1)
     mlon2dis_max=x(nx)
     mlat2dis_min=y(1)
     mlat2dis_max=y(ny)
  end select

!-- C 座標系の計算
  if(present(lon_wnd))then
     lon_min=lon_wnd(1,1)
     lon_max=lon_wnd(nx,1)
     lat_min=lat_wnd(1,1)
     lat_max=lat_wnd(1,ny)
  end if

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(c_num))then
     cont_num=c_num(1)
     color_num=c_num(2)
  else
     cont_num=10
     color_num=56
  end if

  if(present(no_tone))then
     no_tone_flag=no_tone
  else
     no_tone_flag=.false.
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  if(map_pro==22)then
     if(.not.present(t_posi))then
        write(*,*) "*** ERROR (dcl_auto) *** : In case of map_pro = 22,"
        write(*,*) "                           option 't_posi' must be set."
        stop
     end if
  end if

!-- 引数の置き換え用変数に置き換え
  cont_min=cont_int(1)
  cont_max=cont_int(2)
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))
  form_typec=trim(form_type(1))
  form_types=trim(form_type(2))

!-- 地図独自のオプション ---
!-- MapFit ルーチンを用いると, 地図の vp が強制的に変更されるので,
!-- その修正を行う.
!-- u 座標系でのアスペクト比をとり, 長さの長い方の vp を基準にして,
!-- 短い方の vp を修正する.
  uratio=(mlat2dis_max-mlat2dis_min)/(mlon2dis_max-mlon2dis_min)  ! u 座標系での ratio
  if( uratio>1.0 )then
  ! y 軸の方が長いので, vratio で vxmin, vxmax を中点基準に修正.
  ! 修正公式は以下のとおり (mid は中点座標) : 
  ! vxmax+vxmin=2.0*mid, vxmax-vxmin=(vymax-vymin)/uratio
  ! これをそれぞれ解くと, vymax, vymin は基準系なので引数のものを使用し,
  ! vxmax=mid+0.5*(vymax-vymin)/uratio
  ! vxmin=mid-0.5*(vymax-vymin)/uratio
     mid_p=0.5*(vx_min+vx_max)
     vx_max=mid_p+0.5*(vy_max-vy_min)/uratio
     vx_min=mid_p-0.5*(vy_max-vy_min)/uratio
  else
  ! x 軸の方が長いので, vratio で vymin, vymax を中点基準に修正.
  ! 修正公式は以下のとおり (mid は中点座標) : 
  ! vymax+vymin=2.0*mid, vymax-vymin=uratio*(vxmax-vxmin)
  ! これをそれぞれ解くと, vxmax, vxmin は基準系なので引数のものを使用し,
  ! vymax=mid+0.5*(uratio*(vxmax-vxmin)
  ! vymin=mid-0.5*(uratio*(vxmax-vxmin)
     mid_p=0.5*(vy_min+vy_max)
     vy_max=mid_p+0.5*uratio*(vx_max-vx_min)
     vy_min=mid_p-0.5*uratio*(vx_max-vx_min)
  end if

  if(present(mono))then
     if(present(mono_val).and.present(mono_lev))then
        if(size(mono_val)-1==size(mono_lev))then
           monoto=mono
        else
           write(*,*) "*** ERROR ***"
           write(*,*) "[array number] : mono_val = mono_lev + 1"
           write(*,*) "STOP"
           stop
        end if
     else
        write(*,*) "*** ERROR ***"
        write(*,*) "When option MONO is true, MONO_VAL and MONO_LEV must be specified."
        write(*,*) "STOP"
        stop
     end if
  else
     monoto=.false.
  end if

  if(present(mlitv))then
     mlitv_flag=.true.
     mditv=mlitv
  else
     mditv=1.0
  end if

  if(present(mlidx))then
     mdidx=mlidx
  else
     mdidx=1
  end if

  if(present(mxitv))then
     if(mxitv(1)/=0.0.and.mxitv(2)/=0.0)then
        mxitv_flag=.true.
     end if
  end if
  if(present(myitv))then
     if(myitv(1)/=0.0.and.myitv(2)/=0.0)then
        myitv_flag=.true.
     end if
  end if

  if(present(coast))then
     coast_sel='coast_'//coast
  else
     coast_sel='coast_world'
  end if

  if(present(border))then
     select case (trim(border))
     case ('japan')
        bord_flag=.true.
        border_sel='pref_japan'
     case ('world')
        bord_flag=.true.
        border_sel='border_world'
     case ('state')
        bord_flag=.true.
        border_sel='state_usa'
     end select
  end if

  if(present(blidx))then
     bdidx=blidx
  else
     bdidx=3
  end if

  if(present(bltyp))then
     bdtyp=bltyp
  else
     bdtyp=1
  end if

  if(present(lon_wnd))then
     call udlset('LMSG',.false.)
  end if

!-- 処理ここまで ---

!-- contour を axis の前に描くので, 下に contour interval が表示されない
!-- ようにするルーチン. contour interval は別途設定.
  call undef_CReSS2Dcl( nx, ny, 1, contour)
  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

!ORG  call DclSetParm( 'MAP:LGRIDMN', .false. )
!ORG  call DclSetParm( 'MAP:INDEXMJ', mdidx )
!ORG  call DclSetParm( 'MAP:dgridmj', mditv )
!ORG  call DclSetParm( 'MAP:INDEXBND', bdidx )
!ORG  call DclSetParm( 'MAP:INDEXOUT', bdidx )
!ORG  call DclSetParm( 'MAP:ITYPEOUT', bdtyp )

  write(*,*) "window set", x(1), x(nx), y(1), y(ny)
  call DclSetWindow( x(1), x(nx), y(1), y(ny) )

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 's', xg, yg, zg, color_num,  &
  &                      (/shade_min, shade_max/), subsubidx='l' )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              call DclScalingPoint( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  if(present(zp))then
     call color_line( 's', xp, yp, zp, color_num, (/shade_min, shade_max/),  &
  &                   subsubidx='p' )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           call DclScalingPoint( xp(:,i), yp(:,i) )
        end do
     end if
  end if

  write(*,*) "viewport set", vx_min, vx_max, vy_min, vy_max
  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  if(present(lon_wnd))then
     call DclSetTransNumber( 1 )
  else
     call DclSetTransNumber( map_pro )
     call DclFitMapParm
  end if
  call DclSetTransFunction

  if(monoto.eqv..true.)then
     call SGLSET( 'LSOFTF', .true. )
     call DclClearShadeLevel
     call DclSetShadeLevel( mono_val, mono_lev )
  else
     call SGLSET( 'LSOFTF', .false. )
!     call DclClearShadeLevel
!     call DclSetShadeLevel( shade_min, shade_max,   &
! &                          (shade_max-shade_min)/s_num )
  end if

  if(map_pro==11.or.map_pro==10)then
     call DclSetParm( "GRAPH:LCLIP", .true. )
  end if
!     call DclDrawViewPortFrame( 1 )
!  if(present(nongrid))then
!     if(nongrid(1:1)=='o')then
        call DclSetXGrid( x )
!     end if
!     if(nongrid(2:2)=='o')then
        call DclSetYgrid( y )
!     end if
!  end if

  if(monoto.eqv..true.)then
     call DclShadeContour( shade )
  else
     if(present(tonbf))then
        select case (tonbf(1:1))
        case ('b')
           call UETONB( shade, nx, nx, ny )
        case ('c')
           call UETONC( shade, nx, nx, ny )
        case ('e')
           call UETONE( shade, nx, nx, ny )
        case ('f')
           call DclShadeContourEx( shade )
        end select
     else
        call DclShadeContourEx( shade )
     end if
  end if

  if(present(lon_wnd))then
!     call DclDrawAxis( 'b', mditv, 0.5*mditv )
!     call DclDrawAxis( 'l', mditv, 0.5*mditv )
     call Dcl_Special_Axis( 'bl', map_pro, mditv, (/vx_min, vx_max/),  &
  &                         (/vy_min, vy_max/), t_posi, lon_wnd, lat_wnd,  &
  &                         (/trim(x_title), trim(y_title)/) )
!     call DclDrawTitle( 'b', trim(x_title), 0.0 )
!     call DclDrawTitle( 'l', trim(y_title), 0.0 )
     call DclDrawTitle( 't', trim(outname), 0.0, 2 )

     if(DclGetContourLevelNumber()==0)then
        call DclSetContourLabelFormat(trim(form_typec))
        call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
     end if

     call DclDrawContour( contour )

     if(map_pro/=11.and.map_pro/=10)then
        if(present(zg))then
           if(size(xg,1)>1)then
              call color_line( 'l', xg, yg, zg, color_num, (/shade_min, shade_max/) )
           end if
        else
           if(present(xg))then
              if(size(xg,1)>1)then
                 do i=1,size(xg,2)
                    if(present(l_idx))then
                       call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
                    end if
                    if(present(l_typ))then
                       call DclSetLineType( l_typ(i) )
                    end if
                    call DclDrawLine( xg(:,i), yg(:,i) )
                 end do
              end if
           end if
        end if

        call DclSetParm( "GRAPH:LCLIP", .true. )

        if(present(zp))then
           call color_line( 'p', xp, yp, zp, color_num, (/shade_min, shade_max/) )
        else
           if(present(xp))then
              do i=1,size(xp,2)
                 if(present(p_idx))then
                    call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
                 end if
                 if(present(p_typ))then
                    call DclSetMarkerType( p_typ(i) )
                 end if
                 if(present(p_siz))then
                    call DclSetMarkerSize( p_siz(i) )
                 end if
                 call DclDrawMarker( xp(:,i), yp(:,i) )
              end do
           end if
        end if

        call DclSetParm( "GRAPH:LCLIP", .true. )

     end if

     call DclNewFig

!     call g2qctm( lon_min, lon_max, lat_min, lat_max )
     if(present(t_posi))then
        call SGRSET( 'STLAT1', t_posi(1) )
        call SGRSET( 'STLAT2', t_posi(2) )
        call UMSCNT( t_posi(3), t_posi(1), 0.0 )
     end if

write(*,*) "window set", lon_min, lon_max, lat_min, lat_max
write(*,*) "viewport set", vx_min, vx_max, vy_min, vy_max
     CALL UMSPNT( 4, (/lon_wnd(1,1), lon_wnd(nx,1), lon_wnd(1,ny), lon_wnd(nx,ny)/),  &
  &               (/lat_wnd(1,1), lat_wnd(nx,1), lat_wnd(1,ny), lat_wnd(nx,ny)/) )
!     call DclSetWindow( lon_min, lon_max, lat_min, lat_max )

     if(present(long))then
        if(size(long,1)>1)then
           do i=1,size(long,2)
              call DclScalingPoint( long(:,i), latg(:,i) )
           end do
        end if
     end if

     if(present(lonp))then
        do i=1,size(lonp,2)
           call DclScalingPoint( lonp(:,i), latp(:,i) )
        end do
     end if

     call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
     call DclSetTransNumber( map_pro )
     call DclFitMapParm
     call DclSetTransFunction
  end if

  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )

  if(present(lon_wnd))then
!     call DclDrawAxis( 'b', mditv, 0.5*mditv )
!     call DclDrawAxis( 'l', mditv, 0.5*mditv )
     write(*,*) "dummy"
  else
     if(mxitv_flag.eqv..true.)then
        call auto_label_fmt( mxitv(1), x(nx), fmt_mxitv, x(1) )
        CALL USCSET( 'CXFMT', trim(adjustl(fmt_mxitv) ))
     end if
     if(myitv_flag.eqv..true.)then
        call auto_label_fmt( myitv(1), y(ny), fmt_myitv, y(1) )
        CALL USCSET( 'CYFMT', trim(adjustl(fmt_myitv) ))
     end if
     if((mxitv_flag.eqv..true.).and.(myitv_flag.eqv..true.))then
        call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
        call DclDrawAxis( 'rl', myitv(1), myitv(2) )
     else
        if(mxitv_flag.eqv..true.)then
           call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
           call DclDrawAxis( 'rl', mditv, 0.5*mditv )
        else if(myitv_flag.eqv..true.)then
           call DclDrawAxis( 'rl', myitv(1), myitv(2) )
           call DclDrawAxis( 'bt', mditv, 0.5*mditv )
        else
           call DclDrawScaledAxis
        end if
     end if
!     call DclDrawAxis( 'bt', mditv, 0.5*mditv )
!     call DclDrawAxis( 'rl', mditv, 0.5*mditv )
!     call DclDrawScaledAxis
     call DclDrawTitle( 'b', trim(x_title), 0.0 )
     call DclDrawTitle( 'l', trim(y_title), 0.0 )
     call DclDrawTitle( 't', trim(outname), 0.0, 2 )
  end if

  call DclSetParm( 'MAP:LGRIDMN', .false. )
  call DclSetParm( 'MAP:INDEXMJ', mdidx )
  call DclSetParm( 'MAP:dgridmj', mditv )
  call DclSetParm( 'MAP:INDEXBND', bdidx )
  call DclSetParm( 'MAP:INDEXOUT', bdidx )
  call DclSetParm( 'MAP:ITYPEOUT', bdtyp )

  call DclDrawMap( trim(coast_sel) )
  if(present(border))then
     if(bord_flag.eqv..true.)then
        call DclDrawMap( border_sel(1:len_trim(border_sel)) )
     else
        write(*,*) "*** MESSAGE (Dcl_2D_cont_shade_MapPro) ***"
        write(*,*) "'border' argument is invalid."
     end if
  end if
  call DclDrawGlobe()

  if(map_pro==11.or.map_pro==10)then
     if(present(zg))then
        if(size(xg,1)>1)then
           call color_line( 'l', xg, yg, zg, color_num, (/shade_min, shade_max/) )
        end if
     else
        if(present(xg))then
           if(size(xg,1)>1)then
              do i=1,size(xg,2)
                 if(present(l_idx))then
                    call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
                 end if
                 if(present(l_typ))then
                    call DclSetLineType( l_typ(i) )
                 end if
                 call DclDrawLine( xg(:,i), yg(:,i) )
              end do
           end if
        end if
     end if

     call DclSetParm( "GRAPH:LCLIP", .true. )

     if(present(zp))then
        call color_line( 'p', xp, yp, zp, color_num, (/shade_min, shade_max/) )
     else
        if(present(xp))then
           do i=1,size(xp,2)
              if(present(p_idx))then
                 call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
              end if
              if(present(p_typ))then
                 call DclSetMarkerType( p_typ(i) )
              end if
              if(present(p_siz))then
                 call DclSetMarkerSize( p_siz(i) )
              end if
              call DclDrawMarker( xp(:,i), yp(:,i) )
           end do
        end if
     end if

     call DclSetParm( "GRAPH:LCLIP", .true. )

  else

     if(present(long))then
        if(size(long,1)>1)then
           do i=1,size(long,2)
              if(present(m_idx))then
                 call DclSetLineIndex( m_idx(i) )  ! この設定, 後まで引きずるかも
              end if
              if(present(m_typ))then
                 call DclSetLineType( m_typ(i) )
              end if
              call DclDrawLine( long(:,i), latg(:,i) )
           end do
        end if
     end if

     call DclSetParm( "GRAPH:LCLIP", .true. )

     if(present(lonp))then
        do i=1,size(lonp,2)
           if(present(mp_idx))then
              call DclSetMarkerIndex( mp_idx(i) )  ! この設定, 後まで引きずるかも
           end if
           if(present(mp_typ))then
              call DclSetMarkerType( mp_typ(i) )
           end if
           if(present(mp_siz))then
              call DclSetMarkerSize( mp_siz(i) )
           end if
           call DclDrawMarker( lonp(:,i), latp(:,i) )
        end do
     end if

     call DclSetParm( "GRAPH:LCLIP", .true. )

  end if

  if(present(lon_wnd))then
     write(*,*) "contour interval already is written before."
  else
     if(DclGetContourLevelNumber()==0)then
        call DclSetContourLabelFormat(trim(form_typec))
        call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
     end if
     call DclDrawContour( contour )
  end if

  CALL SGQVPT( vx_new(1), vx_new(2), vy_new(1), vy_new(2) )

  if(no_tone_flag.eqv..false.)then
     if(present(trigleg))then
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_new(2)+0.05, vx_new(2)+0.075/),   &
  &                  vy_new, trim(form_types), mono_log=monoto,  &
  &                  trigle=trigleg )
     else
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_new(2)+0.05, vx_new(2)+0.075/),   &
  &                  vy_new, trim(form_types), mono_log=monoto )
     end if
  end if

end subroutine Dcl_2D_cont_shade_MapPro

!---------------------------------------------------------

subroutine Dcl_2D_cont_shade_MapPro_vec( map_pro, outname,  &
  &  x, y, contour, shade, vecx, vecy, vn, cont_int, shade_int,  &
  &  axis_title, form_type, viewx_int, viewy_int, c_num,  &
!  &  viewy_max, color_num, cont_num, nongrid,  &
  &  xg, yg, zg, long, latg, xp, yp, zp, mxitv, myitv, lonp, latp,  &
  &  mono, mono_val, mono_lev, trigleg,  &
  &  unitv, vfact, unit_fact_sign, &
  &  unit_fact, unit_title, unit_posi,  &
  &  mlitv, mlidx, coast, border, blidx, bltyp,  &
  &  no_tone, no_frame, l_idx, l_typ, m_idx, m_typ,  &
  &  p_idx, p_typ, p_siz, mp_idx, mp_typ, mp_siz,  &
  &  t_posi, lon_wnd, lat_wnd, tonbf )
  ! 2 次元で 3 変数を等値線, カラーシェード, ベクトルで描画する.
  ! 最大 4 変数同時描画が可能となる.
  ! 基本的に右にカラーバーがつくので, ユニットベクトルは
  ! コンターインターバルの下に文字で表示される.
  ! 引数 map_pro で地図番号を選択し, 地図投影モードに切り替える.
  use dcl
  implicit none
  integer, intent(in) :: map_pro  ! DCL の地図変換関数番号
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! x 方向の格子点座標 [deg]
  real, intent(in) :: y(:)  ! y 方向の格子点座標 [deg]
  real, intent(inout) :: contour(size(x),size(y))  ! 等値線に描く配列
  real, intent(inout) :: shade(size(x),size(y))  ! カラーシェードに描く配列
  real, intent(inout) :: vecx(size(x),size(y))  ! x 方向のベクトル
  real, intent(inout) :: vecy(size(x),size(y))  ! y 方向のベクトル
  integer, intent(in) :: vn(2)  ! ベクトル格子点 (間引き使用)
                                ! vn(1)=vnx, vn(2)=vny
  real, intent(in) :: cont_int(2)  ! 等値線の上下端 
                                 ! [cont_int(1)=cont_min, cont_int(2)=cont_max]
  real, intent(in) :: shade_int(2)  ! 等値線の上下端 [shade_int(1)=shade_min,
                                 ! shade_int(2)=shade_max]
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                                 ! axis_title(1)=x_title, axis_title(2)=y_title
  character(*), intent(in) :: form_type(2)  ! フォーマット
                           ! form_type(1)=form_typec, form_type(2)=form_types
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  integer, intent(in), optional :: c_num(2)  ! コンター・シェードの数
                           ! c_num(1)=cont_num, c_num(2)=color_num
!  character(2), intent(in), optional :: nongrid  ! 不等間隔格子にするか.
                                        ! nongrid = 'ox' で判断.
                                        ! 1 文字目が横軸, 2 文字目が縦軸.
                                        ! o = 不等間隔, x = 等間隔.
                                        ! デフォルトでは 'xx'.
  real, intent(in), optional :: xg(:,:)  ! x 軸に入れるグリッド線の座標
  real, intent(in), optional :: yg(:,:)  ! y 軸に入れるグリッド線の座標
                    ! 第一要素が線の位置データで, 複数本描く場合は,
                    ! 第二要素を 2 個以上にして描く.
                    ! 配列に入れるデータ次第で直線ではなく, 曲線グリッドを
                    ! 描くことも可能.
                    ! 3 本の線を描く場合は, xg(:,1) と yg(:,1) で 1 本の
                    ! 線を表すように指定すること.
  real, intent(in), optional :: zg(:,:)  ! グリッド線が値をもっていればその値.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラー折れ線を描くモードに移行する.
  real, intent(in), optional :: long(:,:)  ! lon 座標で入れるグリッド線
  real, intent(in), optional :: latg(:,:)  ! lat 座標で入れるグリッド線
  real, intent(in), optional :: xp(:,:)  ! x 方向にマーカーを入れる x 座標
  real, intent(in), optional :: yp(:,:)  ! y 方向にマーカーを入れる y 座標
  real, intent(in), optional :: zp(:,:)  ! マーカーが値をもっていればその値.
                                         ! これらの指定方法は線と同じ.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラーマーカーを描くモードに移行する.
  real, intent(in), optional :: mxitv(2)   ! 横軸の目盛り間隔 (mxitv(1):大目盛り)
  real, intent(in), optional :: myitv(2)   ! 縦軸の目盛り間隔 (myitv(1):大目盛り)
  logical, intent(in), optional :: mono  ! モノトーンの階調にする [.true.]
                                         ! デフォルトは .false.
  real, intent(in), optional :: lonp(:,:)  ! lon 座標で入れるマーカー
  real, intent(in), optional :: latp(:,:)  ! lat 座標で入れるマーカー
  real, intent(in), optional :: mono_val(:)  ! 階調の境界値.
                    ! mono=.true. のときに必ず設定しないとエラーを返す.
                    ! 値は mono_lev + 1 成分存在しなければならない.
  integer, intent(in), optional :: mono_lev(:)  ! トーンマップ番号. dcl の 3 桁
                    ! mono=.true. のときに設定しないとエラーを返す.
  character(1), intent(in), optional :: trigleg  ! トーンバーの三角形オプション.
                ! オプションの値は, tone_bar ルーチンの trigle と同じ.
  logical, intent(in), optional :: unitv  ! 単位ベクトルを描くかどうか. default = .true.
  real, intent(in), optional :: vfact(2)  ! x,y 方向のスケーリングファクター
                    ! この値を指定すると, 内部的に決められないので, ベクトルが格子以上に
                    ! 伸びる可能性がある. 
                    ! 設定しない場合は, x, y の水平スケールと V 系のアスペクト比を考慮
                    ! して, vfact と一致させるようにする.
  logical, intent(in), optional :: unit_fact_sign  ! unitv = .true. のとき,
                    ! .true. = u, v の U 座標系での値を unit_fact に与えると, 
                    ! unit_fact はその値を単位ベクトルの単位として表示する.
                    ! unit の V 座標系の値は u, v の大きい方を 0.1 として表示する.
  real, intent(in), optional :: unit_fact(2)  ! x,y の単位ベクトルの v 座標系での長さ
                                              ! default = (0.1,0.1)
  character(*), intent(in), optional :: unit_title(2)  ! x,y の単位ベクトルのタイトル
                    ! default = 描かない.
  real, intent(in), optional :: unit_posi(2)  ! 単位ベクトルを描き始める原点座標 (V 系)
                    ! default = カラーバーの左端と同じで, 図の右下端から開始.
                    ! カラーバーはこれにぶつからないように自動的に短くする.
  real, intent(in), optional :: mlitv  ! メジャーライン, 目盛の表示間隔 [degree]. デフォルトは 1 degree.
  integer, intent(in), optional :: mlidx  ! メジャーライン, 目盛のインデックス.
                                          ! デフォルトは 1.
  character(5), intent(in), optional :: coast  ! 海岸線選択引数
                                   ! ['japan'] = 日本域詳細版
                                   ! ['world'] = 全球版
                                   ! default = 'world'
  character(5), intent(in), optional :: border  ! 国, 州, 県境描画
                                   ! ['japan'] = 日本県境
                                   ! ['world'] = 世界国境
                                   ! ['state'] = 米国州境
                                   ! default = 描画しない.
  integer, intent(in), optional :: blidx  ! 海岸線, 国境のインデックス.
                                          ! デフォルトは 3.
  integer, intent(in), optional :: bltyp  ! 海岸線, 国境のタイプ.
                                          ! デフォルトは 1.
  logical, intent(in), optional :: no_tone  ! トーンバーを作成しないオプション
                                   ! .false. = 作成する. .true. = 作成しない.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(:)  ! xg, yg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: l_typ(:)  ! xg, yg で描く線のタイプ.
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: m_idx(:)  ! long, latg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: m_typ(:)  ! long, latg で描く線のタイプ.
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: p_idx(:)  ! xp, yp で描くマーカーのインデックス
  integer, intent(in), optional :: p_typ(:)  ! xg, yg で描くマーカーのタイプ.
                                                      ! デフォルトは 1.
  real, intent(in), optional :: p_siz(:)  ! xg, yg で描くマーカーの大きさ.
                                                      ! デフォルトは 0.01.
  integer, intent(in), optional :: mp_idx(:)  ! lonp, latp で描くマーカーのインデックス
  integer, intent(in), optional :: mp_typ(:)  ! lonp, latp で描くマーカーのインデックス
  real, intent(in), optional :: mp_siz(:)  ! lonp, latp で描くマーカーのインデックス
  real, intent(in), optional :: t_posi(3)  ! map optiona がランベルトの場合
                                           ! t_posi=(/lat1, lat2, lon1/) で設定.
                                           ! 単位は degree
  real, intent(in), optional :: lon_wnd(:,:)  ! メルカトル系以外で矩形領域を設定
                   ! する場合の各座標系経緯度. この値が設定されるとき, 
                   ! x, y には, デカルト系での距離を与えておくこと.
  real, intent(in), optional :: lat_wnd(:,:)  ! メルカトル系以外で矩形領域を設定
                   ! する場合の各座標系経緯度. この値が設定されるとき, 
                   ! x, y には, デカルト系での距離を与えておくこと.
  character(1), intent(in), optional :: tonbf  ! UETONE FLAG (a,b,e,f,..)
                                          ! default 'f' = UETONF
!-- 以上, 引数
  integer :: vnx  ! x 方向のベクトル格子点 (間引き使用)
  integer :: vny  ! y 方向のベクトル格子点 (間引き使用)
  real :: cont_min  ! 等値線を描く最小値
  real :: cont_max  ! 等値線を描く最大値
  real :: shade_min  ! シェードを描く最小値
  real :: shade_max  ! シェードを描く最大値
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  character(10) :: form_typec  ! contour 用のフォーマット
  character(10) :: form_types  ! shade 用のフォーマット
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
  integer :: cont_num  ! 等値線の数
  integer :: color_num  ! カラーの数
!-- 以上, 引数の置き換え用変数
  real, parameter :: pi=3.14159265
  real, parameter :: radius=6.38e6
  integer :: i, j, k  ! 作業用添字
  integer :: nx, ny
  real :: factx, facty
  real, dimension(size(x),size(y)) :: um, vm  ! ベクトル間引き後の値を代入
  real :: vvx_min, vvx_max, vvy_min, vvy_max
  real :: unitvp(2), unitvl(2), unit_auto_fact(2)
  real :: uratio
  real :: undef, RMIS
  real :: map_lat_min, map_lat_max, map_lon_min, map_lon_max
  real :: lat_min, lat_max, lon_min, lon_max
  real :: mlat2dis_min, mlat2dis_max, mlon2dis_min, mlon2dis_max, mditv, mid_p
  integer :: mdidx, bdidx, bdtyp
  real, dimension(2) :: vx_new, vy_new
  character(20) :: coast_sel
  character(20) :: border_sel
  character(20) :: fmt_mxitv, fmt_myitv
  intrinsic :: nint
  logical :: monoto, unitvs, no_tone_flag, no_frame_flag, bord_flag, mlitv_flag
  logical :: mxitv_flag, myitv_flag

  nx=size(x)
  ny=size(y)
  coast_sel=''
  border_sel=''
  bord_flag=.false.
  mlitv_flag=.false.
  mxitv_flag=.false.
  myitv_flag=.false.

!-- 引数を rad 単位に変換
  map_lon_min=x(1)*pi/180.0
  map_lon_max=x(nx)*pi/180.0
  map_lat_min=y(1)*pi/180.0
  map_lat_max=y(ny)*pi/180.0

  select case (map_pro)
  case (10)
     mlon2dis_min=map_lon_min
     mlon2dis_max=map_lon_max
     mlat2dis_min=map_lat_min
     mlat2dis_max=map_lat_max
  case (11)
     mlon2dis_min=map_lon_min
     mlon2dis_max=map_lon_max
     mlat2dis_min=log(tan(0.25*pi+0.5*map_lat_min))
     mlat2dis_max=log(tan(0.25*pi+0.5*map_lat_max))
  case (22)
     mlon2dis_min=x(1)
     mlon2dis_max=x(nx)
     mlat2dis_min=y(1)
     mlat2dis_max=y(ny)
  end select

!-- C 座標系の計算
  if(present(lon_wnd))then
     lon_min=lon_wnd(1,1)
     lon_max=lon_wnd(nx,1)
     lat_min=lat_wnd(1,1)
     lat_max=lat_wnd(1,ny)
  end if

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(c_num))then
     cont_num=c_num(1)
     color_num=c_num(2)
  else
     cont_num=10
     color_num=56
  end if

  if(present(no_tone))then
     no_tone_flag=no_tone
  else
     no_tone_flag=.false.
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  if(map_pro==22)then
     if(.not.present(t_posi))then
        write(*,*) "*** ERROR (dcl_auto) *** : In case of map_pro = 22,"
        write(*,*) "                           option 't_posi' must be set."
        stop
     end if
  end if

!-- 引数の置き換え用変数に置き換え
  vnx=vn(1)
  vny=vn(2)
  cont_min=cont_int(1)
  cont_max=cont_int(2)
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))
  form_typec=trim(form_type(1))
  form_types=trim(form_type(2))

!-- エラー処理
  if(nx<vnx.or.ny<vny)then
     write(*,*) "*****ERROR***** : vnx > nx or vny > ny."
     stop
  end if
  if(nx<2.or.vnx<2.or.ny<2.or.vny<2)then
     write(*,*) "*****ERROR***** : nx or ny or vnx or vny is less than 2."
     stop
  end if

!-- 警告
  if(mod((nx-1),(vnx-1))/=0.and.mod((ny-1),(vny-1))/=0)then
     write(*,*) "****WARNING**** : vnx or vny is not the factor of nx and ny."
  else
     if(mod((nx-1),(vnx-1))/=0.or.mod((ny-1),(vny-1))/=0)then
        if(mod((nx-1),(vnx-1))/=0)then
           write(*,*) "****WARNING**** : vnx is not the factor of nx."
        else
           write(*,*) "****WARNING**** : vny is not the factor of ny."
        end if
     end if
  end if

!-- ベクトル場の間引き
  factx=real(nx-1)/real(vnx-1)
  facty=real(ny-1)/real(vny-1)

  um=0.0
  vm=0.0

!-- 起点を 1 から始める
  um(1,1)=vecx(1,1)
  vm(1,1)=vecy(1,1)

  do i=2,vnx
     um(1+nint(factx*(i-1)),1)=vecx(1+nint(factx*(i-1)),1)
     vm(1+nint(factx*(i-1)),1)=vecy(1+nint(factx*(i-1)),1)
  end do

  do j=2,vny
     um(1,1+nint((j-1)*facty))=vecx(1,1+nint((j-1)*facty))
     vm(1,1+nint((j-1)*facty))=vecy(1,1+nint((j-1)*facty))
  end do

  do j=2,vny
     do i=2,vnx
        um(1+nint(factx*(i-1)),1+nint(facty*(j-1)))  &
  &     =vecx(1+nint(factx*(i-1)),1+nint(facty*(j-1)))
        vm(1+nint(factx*(i-1)),1+nint(facty*(j-1)))  &
  &     =vecy(1+nint(factx*(i-1)),1+nint(facty*(j-1)))
     end do
  end do

!-- 地図独自のオプション ---
!-- MapFit ルーチンを用いると, 地図の vp が強制的に変更されるので,
!-- その修正を行う.
!-- u 座標系でのアスペクト比をとり, 長さの長い方の vp を基準にして,
!-- 短い方の vp を修正する.
  uratio=(mlat2dis_max-mlat2dis_min)/(mlon2dis_max-mlon2dis_min)  ! u 座標系での ratio
  if( uratio>1.0 )then
  ! y 軸の方が長いので, vratio で vxmin, vxmax を中点基準に修正.
  ! 修正公式は以下のとおり (mid は中点座標) : 
  ! vxmax+vxmin=2.0*mid, vxmax-vxmin=(vymax-vymin)/uratio
  ! これをそれぞれ解くと, vymax, vymin は基準系なので引数のものを使用し,
  ! vxmax=mid+0.5*(vymax-vymin)/uratio
  ! vxmin=mid-0.5*(vymax-vymin)/uratio
     mid_p=0.5*(vx_min+vx_max)
     vx_max=mid_p+0.5*(vy_max-vy_min)/uratio
     vx_min=mid_p-0.5*(vy_max-vy_min)/uratio
  else
  ! x 軸の方が長いので, vratio で vymin, vymax を中点基準に修正.
  ! 修正公式は以下のとおり (mid は中点座標) : 
  ! vymax+vymin=2.0*mid, vymax-vymin=uratio*(vxmax-vxmin)
  ! これをそれぞれ解くと, vxmax, vxmin は基準系なので引数のものを使用し,
  ! vymax=mid+0.5*(uratio*(vxmax-vxmin)
  ! vymin=mid-0.5*(uratio*(vxmax-vxmin)
     mid_p=0.5*(vy_min+vy_max)
     vy_max=mid_p+0.5*uratio*(vx_max-vx_min)
     vy_min=mid_p-0.5*uratio*(vx_max-vx_min)
  end if

  if(present(mono))then
     if(present(mono_val).and.present(mono_lev))then
        if(size(mono_val)-1==size(mono_lev))then
           monoto=mono
        else
           write(*,*) "*** ERROR ***"
           write(*,*) "[array number] : mono_val = mono_lev + 1"
           write(*,*) "STOP"
           stop
        end if
     else
        write(*,*) "*** ERROR ***"
        write(*,*) "When option MONO is true, MONO_VAL and MONO_LEV must be specified."
        write(*,*) "STOP"
        stop
     end if
  else
     monoto=.false.
  end if

  if(present(mlitv))then
     mditv=mlitv
     mlitv_flag=.true.
  else
     mditv=1.0
  end if

  if(present(mlidx))then
     mdidx=mlidx
  else
     mdidx=1
  end if

  if(present(mxitv))then
     if(mxitv(1)/=0.0.and.mxitv(2)/=0.0)then
        mxitv_flag=.true.
     end if
  end if
  if(present(myitv))then
     if(myitv(1)/=0.0.and.myitv(2)/=0.0)then
        myitv_flag=.true.
     end if
  end if

  if(present(coast))then
     coast_sel='coast_'//coast
  else
     coast_sel='coast_world'
  end if

  if(present(border))then
     select case (trim(border))
     case ('japan')
        bord_flag=.true.
        border_sel='pref_japan'
     case ('world')
        bord_flag=.true.
        border_sel='border_world'
     case ('state')
        bord_flag=.true.
        border_sel='state_usa'
     end select
  end if

  if(present(blidx))then
     bdidx=blidx
  else
     bdidx=3
  end if

  if(present(bltyp))then
     bdtyp=bltyp
  else
     bdtyp=1
  end if

  if(present(lon_wnd))then
     call udlset('LMSG',.false.)
  end if

!-- 処理ここまで ---

!-- contour を axis の前に描くので, 下に contour interval が表示されない
!-- ようにするルーチン. contour interval は別途設定.
  call undef_CReSS2Dcl( nx, ny, 1, contour)
  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

!ORG  call DclSetParm( 'MAP:LGRIDMN', .false. )
!ORG  call DclSetParm( 'MAP:INDEXMJ', mdidx )
!ORG  call DclSetParm( 'MAP:dgridmj', mditv )
!ORG  call DclSetParm( 'MAP:INDEXBND', bdidx )
!ORG  call DclSetParm( 'MAP:INDEXOUT', bdidx )
!ORG  call DclSetParm( 'MAP:ITYPEOUT', bdtyp )

  write(*,*) "window set", x(1), x(nx), y(1), y(ny)
  call DclSetWindow( x(1), x(nx), y(1), y(ny) )

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 's', xg, yg, zg, color_num,  &
  &                      (/shade_min, shade_max/), subsubidx='l' )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              call DclScalingPoint( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  if(present(zp))then
     call color_line( 's', xp, yp, zp, color_num, (/shade_min, shade_max/),  &
  &                   subsubidx='p' )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           call DclScalingPoint( xp(:,i), yp(:,i) )
        end do
     end if
  end if

!-- ベクトルスケールについての設定
  if(present(unit_fact_sign))then
     if(unit_fact_sign.eqv..true.)then
        if(present(unit_fact))then
           unit_auto_fact(1)=unit_fact(1)
           unit_auto_fact(2)=unit_fact(2)
        else
           write(*,*) "### ERROR ### : unit_fact_sign is .true. then,"
           write(*,*) "                unit_fact must configure."
           write(*,*) "STOP."
           stop
        end if
     else
        unit_auto_fact(1)=1.0
        unit_auto_fact(2)=1.0
     end if
  else
     unit_auto_fact(1)=1.0
     unit_auto_fact(2)=1.0
  end if

  if(present(vfact))then
     call DclSetParm( 'VECTOR:LNRMAL', .false. )
     call DclSetParm( 'VECTOR:XFACT1', vfact(1) )
     call DclSetParm( 'VECTOR:YFACT1', vfact(2) )
     unit_auto_fact(1)=unit_auto_fact(1)*vfact(1)
     unit_auto_fact(2)=unit_auto_fact(2)*vfact(2)
  else
     call DclSetParm( 'VECTOR:LNRMAL', .true.)
     call DclSetParm( 'VECTOR:XFACT1', unitvl(1) )
     call DclSetParm( 'VECTOR:YFACT1', unitvl(2) )
     unit_auto_fact(1)=unit_auto_fact(1)*unitvl(1)
     unit_auto_fact(2)=unit_auto_fact(2)*unitvl(2)
     unitvl=0.0
  end if

!-- ユニットベクトルについての設定
  if(present(unitv))then
     unitvs=unitv
  else
     unitvs=.true.
  end if

  if(unitvs.eqv..true.)then

     call DclSetParm( 'VECTOR:LUNIT', unitvs )

     !-- 単位ベクトルの長さ
     if(present(unit_fact))then
        if(present(unit_fact_sign))then
           if(unit_fact_sign.eqv..true.)then
              unitvl(:)=unit_auto_fact(:)
           else
              unitvl(:)=unit_fact(:)
           end if
        else
           unitvl(:)=unit_fact(:)
        end if
     else
        unitvl=(/0.1, 0.1/)
     end if

     !-- 単位ベクトルの書き始めの位置
     if(present(unit_posi))then
        vvx_min=unit_posi(1)
        vvy_min=unit_posi(2)
     else
        vvx_min=vx_max+0.05
        vvy_min=vy_min
     end if

     vvy_max=vvy_min+unitvl(2)+0.05

     call DclSetParm( "GRAPH:LCLIP", .false. )
     call DclSetParm( 'VECTOR:VXUNIT', unitvl(1) )
     call DclSetParm( 'VECTOR:VYUNIT', unitvl(2) )
     call DclSetParm( 'VECTOR:VXULOC', vvx_min )
     call DclSetParm( 'VECTOR:VYULOC', vvy_min )

     !-- タイトルを書くかどうか
     if(present(unit_title))then
        !-- ランベルトでの座標ラベルを個別に書くので,
        !-- ここで 'Y' を後に書くと, その設定を引きずって
        !-- 後ろのランベルト座標ラベルが 'Y' 向きに記載される.
        !-- それをしないためここでは 'Y' から描く. 
        if(len_trim(unit_title(2))>0)then
           call DclSetUnitVectorTitle( 'Y', trim(unit_title(2)) )
        end if
        if(len_trim(unit_title(1))>0)then
           call DclSetUnitVectorTitle( 'X', trim(unit_title(1)) )
        end if
        call DclSetParm( 'VECTOR:LUMSG', .false. )
     else  ! タイトルを書かないなら, グラフの下部にスケーリングファクターを明記
        call DclSetParm( 'VECTOR:LUMSG', .true. )
     end if

  else
     call DclSetParm( 'VECTOR:LUNIT', unitvs )
     vvx_min=0.0
     vvx_max=0.0
     vvy_min=0.0
     vvy_max=vy_min
  end if

  write(*,*) "viewport set", vx_min, vx_max, vy_min, vy_max
  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )

  if(present(lon_wnd))then
     call DclSetTransNumber( 1 )
  else
     call DclSetTransNumber( map_pro )
     call DclFitMapParm
  end if
  call DclSetTransFunction

  if(monoto.eqv..true.)then
     call SGLSET( 'LSOFTF', .true. )
     call DclClearShadeLevel
     call DclSetShadeLevel( mono_val, mono_lev )
  else
     call SGLSET( 'LSOFTF', .false. )
!     call DclClearShadeLevel
!     call DclSetShadeLevel( shade_min, shade_max,   &
! &                          (shade_max-shade_min)/s_num )
  end if

  if(map_pro==11.or.map_pro==10)then
     call DclSetParm( "GRAPH:LCLIP", .true. )
  end if
!     call DclDrawViewPortFrame( 1 )
!  if(present(nongrid))then
!     if(nongrid(1:1)=='o')then
        call DclSetXGrid( x )
!     end if
!     if(nongrid(2:2)=='o')then
        call DclSetYgrid( y )
!     end if
!  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(monoto.eqv..true.)then
     call DclShadeContour( shade )
  else
     if(present(tonbf))then
        select case (tonbf(1:1))
        case ('b')
           call UETONB( shade, nx, nx, ny )
        case ('c')
           call UETONC( shade, nx, nx, ny )
        case ('e')
           call UETONE( shade, nx, nx, ny )
        case ('f')
           call DclShadeContourEx( shade )
        end select
     else
        call DclShadeContourEx( shade )
     end if
  end if

  if(present(lon_wnd))then
!     call DclDrawAxis( 'b', mditv, 0.5*mditv )
!     call DclDrawAxis( 'l', mditv, 0.5*mditv )
     call Dcl_Special_Axis( 'bl', map_pro, mditv, (/vx_min, vx_max/),  &
  &                         (/vy_min, vy_max/), t_posi, lon_wnd, lat_wnd,  &
  &                         (/trim(x_title), trim(y_title)/) )
!     call DclDrawTitle( 'b', trim(x_title), 0.0 )
!     call DclDrawTitle( 'l', trim(y_title), 0.0 )
     call DclDrawTitle( 't', trim(outname), 0.0, 2 )

     if(DclGetContourLevelNumber()==0)then
        call DclSetContourLabelFormat(trim(form_typec))
        call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
     end if

     call DclDrawContour( contour )

     call DclSetParm( "GRAPH:LCLIP", .false. )
     call DclDrawVectors( um, vm )

     if(map_pro/=11.and.map_pro/=10)then
        if(present(zg))then
           if(size(xg,1)>1)then
              call color_line( 'l', xg, yg, zg, color_num, (/shade_min, shade_max/) )
           end if
        else
           if(present(xg))then
              if(size(xg,1)>1)then
                 do i=1,size(xg,2)
                    if(present(l_idx))then
                       call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
                    end if
                    if(present(l_typ))then
                       call DclSetLineType( l_typ(i) )
                    end if
                    call DclDrawLine( xg(:,i), yg(:,i) )
                 end do
              end if
           end if
        end if

        call DclSetParm( "GRAPH:LCLIP", .false. )

        if(present(zp))then
           call color_line( 'p', xp, yp, zp, color_num, (/shade_min, shade_max/) )
        else
           if(present(xp))then
              do i=1,size(xp,2)
                 if(present(p_idx))then
                    call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
                 end if
                 if(present(p_typ))then
                    call DclSetMarkerType( p_typ(i) )
                 end if
                 if(present(p_siz))then
                    call DclSetMarkerSize( p_siz(i) )
                 end if
                 call DclDrawMarker( xp(:,i), yp(:,i) )
              end do
           end if
        end if

        call DclSetParm( "GRAPH:LCLIP", .true. )

     end if

     call DclNewFig

!     call g2qctm( lon_min, lon_max, lat_min, lat_max )
     if(present(t_posi))then
        call SGRSET( 'STLAT1', t_posi(1) )
        call SGRSET( 'STLAT2', t_posi(2) )
        call UMSCNT( t_posi(3), t_posi(1), 0.0 )
     end if

write(*,*) "window set", lon_min, lon_max, lat_min, lat_max
write(*,*) "viewport set", vx_min, vx_max, vy_min, vy_max
     CALL UMSPNT( 4, (/lon_wnd(1,1), lon_wnd(nx,1), lon_wnd(1,ny), lon_wnd(nx,ny)/),  &
  &               (/lat_wnd(1,1), lat_wnd(nx,1), lat_wnd(1,ny), lat_wnd(nx,ny)/) )
!     call DclSetWindow( lon_min, lon_max, lat_min, lat_max )

     if(present(long))then
        if(size(long,1)>1)then
           do i=1,size(long,2)
              call DclScalingPoint( long(:,i), latg(:,i) )
           end do
        end if
     end if

     if(present(lonp))then
        do i=1,size(lonp,2)
           call DclScalingPoint( lonp(:,i), latp(:,i) )
        end do
     end if

     call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
     call DclSetTransNumber( map_pro )
     call DclFitMapParm
     call DclSetTransFunction
  end if

  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )

  if(present(lon_wnd))then
!     call DclDrawAxis( 'b', mditv, 0.5*mditv )
!     call DclDrawAxis( 'l', mditv, 0.5*mditv )
     write(*,*) "dummy"
  else
     if(mxitv_flag.eqv..true.)then
        call auto_label_fmt( mxitv(1), x(nx), fmt_mxitv, x(1) )
        CALL USCSET( 'CXFMT', trim(adjustl(fmt_mxitv) ))
     end if
     if(myitv_flag.eqv..true.)then
        call auto_label_fmt( myitv(1), y(ny), fmt_myitv, y(1) )
        CALL USCSET( 'CYFMT', trim(adjustl(fmt_myitv) ))
     end if
     if((mxitv_flag.eqv..true.).and.(myitv_flag.eqv..true.))then
        call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
        call DclDrawAxis( 'rl', myitv(1), myitv(2) )
     else
        if(mxitv_flag.eqv..true.)then
           call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
           call DclDrawAxis( 'rl', mditv, 0.5*mditv )
        else if(myitv_flag.eqv..true.)then
           call DclDrawAxis( 'rl', myitv(1), myitv(2) )
           call DclDrawAxis( 'bt', mditv, 0.5*mditv )
        else
           call DclDrawScaledAxis
        end if
     end if
!     call DclDrawAxis( 'bt', mditv, 0.5*mditv )
!     call DclDrawAxis( 'rl', mditv, 0.5*mditv )
!     call DclDrawScaledAxis
     call DclDrawTitle( 'b', trim(x_title), 0.0 )
     call DclDrawTitle( 'l', trim(y_title), 0.0 )
     call DclDrawTitle( 't', trim(outname), 0.0, 2 )
  end if

  call DclSetParm( 'MAP:LGRIDMN', .false. )
  call DclSetParm( 'MAP:INDEXMJ', mdidx )
  call DclSetParm( 'MAP:dgridmj', mditv )
  call DclSetParm( 'MAP:INDEXBND', bdidx )
  call DclSetParm( 'MAP:INDEXOUT', bdidx )
  call DclSetParm( 'MAP:ITYPEOUT', bdtyp )

  call DclDrawMap( trim(coast_sel) )
  if(present(border))then
     if(bord_flag.eqv..true.)then
        call DclDrawMap( border_sel(1:len_trim(border_sel)) )
     else
        write(*,*) "*** MESSAGE (Dcl_2D_cont_shade_MapPro) ***"
        write(*,*) "'border' argument is invalid."
     end if
  end if
  call DclDrawGlobe()

  if(map_pro==11.or.map_pro==10)then
     if(present(zg))then
        if(size(xg,1)>1)then
           call color_line( 'l', xg, yg, zg, color_num, (/shade_min, shade_max/) )
        end if
     else
        if(present(xg))then
           if(size(xg,1)>1)then
              do i=1,size(xg,2)
                 if(present(l_idx))then
                    call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
                 end if
                 if(present(l_typ))then
                    call DclSetLineType( l_typ(i) )
                 end if
                 call DclDrawLine( xg(:,i), yg(:,i) )
              end do
           end if
        end if
     end if

     call DclSetParm( "GRAPH:LCLIP", .false. )

     if(present(zp))then
        call color_line( 'p', xp, yp, zp, color_num, (/shade_min, shade_max/) )
     else
        if(present(xp))then
           do i=1,size(xp,2)
              if(present(p_idx))then
                 call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
              end if
              if(present(p_typ))then
                 call DclSetMarkerType( p_typ(i) )
              end if
              if(present(p_siz))then
                 call DclSetMarkerSize( p_siz(i) )
              end if
              call DclDrawMarker( xp(:,i), yp(:,i) )
           end do
        end if
     end if

     call DclSetParm( "GRAPH:LCLIP", .true. )

  else

     if(present(long))then
        if(size(long,1)>1)then
           do i=1,size(long,2)
              if(present(m_idx))then
                 call DclSetLineIndex( m_idx(i) )  ! この設定, 後まで引きずるかも
              end if
              if(present(m_typ))then
                 call DclSetLineType( m_typ(i) )
              end if
              call DclDrawLine( long(:,i), latg(:,i) )
           end do
        end if
     end if

     call DclSetParm( "GRAPH:LCLIP", .false. )

     if(present(lonp))then
        do i=1,size(lonp,2)
           if(present(mp_idx))then
              call DclSetMarkerIndex( mp_idx(i) )  ! この設定, 後まで引きずるかも
           end if
           if(present(mp_typ))then
              call DclSetMarkerType( mp_typ(i) )
           end if
           if(present(mp_siz))then
              call DclSetMarkerSize( mp_siz(i) )
           end if
           call DclDrawMarker( lonp(:,i), latp(:,i) )
        end do
     end if

     call DclSetParm( "GRAPH:LCLIP", .true. )

  end if

  if(present(lon_wnd))then
     write(*,*) "contour interval already is written before."
  else
     if(DclGetContourLevelNumber()==0)then
        call DclSetContourLabelFormat(trim(form_typec))
        call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
     end if
     call DclDrawContour( contour )

     call DclSetParm( "GRAPH:LCLIP", .false. )
     call DclDrawVectors( um, vm )
  end if

  CALL SGQVPT( vx_new(1), vx_new(2), vy_new(1), vy_new(2) )

  if(no_tone_flag.eqv..false.)then
     if(present(trigleg))then
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_new(2)+0.05, vx_new(2)+0.075/),   &
  &                  (/vvy_max, vy_max/), trim(form_types), mono_log=monoto,  &
  &                  trigle=trigleg )
     else
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_new(2)+0.05, vx_new(2)+0.075/),   &
  &                  (/vvy_max, vy_max/), trim(form_types), mono_log=monoto )
     end if
  end if

end subroutine Dcl_2D_cont_shade_MapPro_vec

!---------------------------------------------------------

subroutine Dcl_2D_cont_shade_terrain( outname,  &
  &  x, y, grid_point, contour, shade, cont_int, shade_int,  &
  &  axis_title, form_type, viewx_int, viewy_int, c_num,  &
!  &  viewy_min, viewy_max, color_num, cont_num, nongrid,  &
  &  xg, yg, zg, xp, yp, zp, mxitv, myitv,  &
  &  mono, mono_val, mono_lev, trigleg, trn_paint, trn_col,  &
  &  layer_line, no_tone, no_frame,  &
  &  l_idx, l_typ, p_idx, p_typ, p_siz, tonbf )
  ! 2 次元で 2 変数を等値線とカラーシェードで描画する.
  ! terrain following 版
  use dcl
  implicit none
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! x 方向の格子点座標
  real, intent(in) :: y(:)  ! y 方向の格子点座標
  real, intent(inout) :: grid_point(size(x),size(y))  ! terrain following 座標
  real, intent(inout) :: contour(size(x),size(y))  ! 等値線に描く配列
  real, intent(inout) :: shade(size(x),size(y))  ! カラーシェードに描く配列
  real, intent(in) :: cont_int(2)  ! 等値線の上下端 
                                 ! [cont_int(1)=cont_min, cont_int(2)=cont_max]
  real, intent(in) :: shade_int(2)  ! 等値線の上下端 [shade_int(1)=shade_min,
                                 ! shade_int(2)=shade_max]
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                                 ! axis_title(1)=x_title, axis_title(2)=y_title
  character(*), intent(in) :: form_type(2)  ! フォーマット
                           ! form_type(1)=form_typec, form_type(2)=form_types
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  integer, intent(in), optional :: c_num(2)  ! コンター・シェードの数
                           ! c_num(1)=cont_num, c_num(2)=color_num
!  character(2), intent(in), optional :: nongrid  ! 不等間隔格子にするか.
                                        ! nongrid = 'ox' で判断.
                                        ! 1 文字目が横軸, 2 文字目が縦軸.
                                        ! o = 不等間隔, x = 等間隔.
                                        ! デフォルトでは 'xx'.
  real, intent(in), optional :: xg(:,:)  ! x 軸に入れるグリッド線の座標
  real, intent(in), optional :: yg(:,:)  ! y 軸に入れるグリッド線の座標
                    ! 第一要素が線の位置データで, 複数本描く場合は,
                    ! 第二要素を 2 個以上にして描く.
                    ! 配列に入れるデータ次第で直線ではなく, 曲線グリッドを
                    ! 描くことも可能.
                    ! 3 本の線を描く場合は, xg(:,1) と yg(:,1) で 1 本の
                    ! 線を表すように指定すること.
  real, intent(in), optional :: zg(:,:)  ! グリッド線が値をもっていればその値.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラー折れ線を描くモードに移行する.
  real, intent(in), optional :: xp(:,:)  ! x 方向にマーカーを入れる x 座標
  real, intent(in), optional :: yp(:,:)  ! y 方向にマーカーを入れる y 座標
  real, intent(in), optional :: zp(:,:)  ! マーカーが値をもっていればその値.
                                         ! これらの指定方法は線と同じ.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラーマーカーを描くモードに移行する.
  real, intent(in), optional :: mxitv(2)   ! 横軸の目盛り間隔 (mxitv(1):大目盛り)
  real, intent(in), optional :: myitv(2)   ! 横軸の目盛り間隔 (myitv(1):大目盛り)
  logical, intent(in), optional :: mono  ! モノトーンの階調にする [.true.]
                                         ! デフォルトは .false.
  real, intent(in), optional :: mono_val(:)  ! 階調の境界値.
                    ! mono=.true. のときに必ず設定しないとエラーを返す.
                    ! 値は mono_lev + 1 成分存在しなければならない.
  integer, intent(in), optional :: mono_lev(:)  ! トーンマップ番号. dcl の 3 桁
                    ! mono=.true. のときに設定しないとエラーを返す.
  character(1), intent(in), optional :: trigleg  ! トーンバーの三角形オプション.
                ! オプションの値は, tone_bar ルーチンの trigle と同じ.
  logical, intent(in), optional :: trn_paint  ! 地形に色を塗るか. [def:.false.]
  integer, intent(in), optional :: trn_col  ! 地形に塗る色のカラー番号
  logical, intent(in), optional :: layer_line  ! 各層の格子線を表示する. [def:.false.]
  logical, intent(in), optional :: no_tone  ! トーンバーを作成しないオプション
                                   ! .false. = 作成する. .true. = 作成しない.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(:)  ! xg, yg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: l_typ(:)  ! xg, yg で描く線のタイプ
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: p_idx(:)  ! xp, yp で描くマーカーのインデックス
  integer, intent(in), optional :: p_typ(:)  ! xp, yp で描くマーカーのタイプ
  real, intent(in), optional :: p_siz(:)  ! xp, yp で描くマーカーの大きさ
  character(1), intent(in), optional :: tonbf  ! UETONE FLAG (a,b,e,f,..)
                                          ! default 'f' = UETONF
!-- 以上, 引数
  real :: cont_min  ! 等値線を描く最小値
  real :: cont_max  ! 等値線を描く最大値
  real :: shade_min  ! シェードを描く最小値
  real :: shade_max  ! シェードを描く最大値
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  character(10) :: form_typec  ! contour 用のフォーマット
  character(10) :: form_types  ! shade 用のフォーマット
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
  integer :: cont_num  ! 等値線の数
  integer :: color_num  ! カラーの数
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k  ! 作業用添字
  integer :: nx, ny
  real :: undef, RMISS, interc
  logical :: monoto, no_tone_flag, no_frame_flag
  real :: cx(size(x),size(y)), cy(size(x),size(y))
  real :: trn(size(x)+2), trn_x(size(x)+2)
  real :: cxmax, cxmin, cymax, cymin
  character(10) :: val_c
  character(20) :: fmt_mxitv, fmt_myitv
  integer :: maxcy, maxcx, trn_color
  logical :: mxitv_flag, myitv_flag

  nx=size(x)
  ny=size(y)

!-- c 座標系への変換
  do j=1,ny
     do i=1,nx
        cx(i,j)=x(i)
        cy(i,j)=grid_point(i,j)
     end do
  end do

!-- c 座標系極値の計算

  cxmin=x(1)
  cxmax=x(nx)
  cymin=cy(1,1)
  cymax=cy(1,ny)
  do i=2,nx
     if(cymin>cy(i,1))then
        cymin=cy(i,1)
     end if
     if(cymax<cy(i,ny))then
        cymax=cy(i,ny)
     end if
  end do

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(c_num))then
     cont_num=c_num(1)
     color_num=c_num(2)
  else
     cont_num=10
     color_num=56
  end if

  if(present(no_tone))then
     no_tone_flag=no_tone
  else
     no_tone_flag=.false.
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  mxitv_flag=.false.
  myitv_flag=.false.
  if(present(mxitv))then
     if(mxitv(1)/=0.0.and.mxitv(2)/=0.0)then
        mxitv_flag=.true.
     end if
  end if
  if(present(myitv))then
     if(myitv(1)/=0.0.and.myitv(2)/=0.0)then
        myitv_flag=.true.
     end if
  end if

!-- 引数の置き換え用変数に置き換え
  cont_min=cont_int(1)
  cont_max=cont_int(2)
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))
  form_typec=trim(form_type(1))
  form_types=trim(form_type(2))

  if(present(mono))then
     if(present(mono_val).and.present(mono_lev))then
        if(size(mono_val)-1==size(mono_lev))then
           monoto=mono
        else
           write(*,*) "*** ERROR ***"
           write(*,*) "[array number] : mono_val = mono_lev + 1"
           write(*,*) "STOP"
           stop
        end if
     else
        write(*,*) "*** ERROR ***"
        write(*,*) "When option MONO is true, MONO_VAL and MONO_LEV must be specified."
        write(*,*) "STOP"
        stop
     end if
  else
     monoto=.false.
  end if

!-- 処理ここまで ---

!-- contour を axis の前に描くので, 下に contour interval が表示されない
!-- ようにするルーチン. contour interval は別途設定.
  call udlset('LMSG',.false.)

  call undef_CReSS2Dcl( nx, ny, 1, contour)
  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  call DclSetWindow( x(1), x(nx), y(1), y(ny) )

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 's', xg, yg, zg, color_num,  &
  &                      (/shade_min, shade_max/), subsubidx='l' )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              call DclScalingPoint( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  if(present(zp))then
     call color_line( 's', xp, yp, zp, color_num,  &
  &                   (/shade_min, shade_max/), subsubidx='p' )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           call DclScalingPoint( xp(:,i), yp(:,i) )
        end do
     end if
  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  call DclSetTransNumber(51)
  call g2sctr(nx, ny, x, y, cx, cy )
  call DclSetTransFunction

  if(monoto.eqv..true.)then
     call SGLSET( 'LSOFTF', .true. )
     call DclClearShadeLevel
     call DclSetShadeLevel( mono_val, mono_lev )
  else
     call SGLSET( 'LSOFTF', .false. )
!     call SGLSET( 'LSOFTF', .true. )
!     call DclClearShadeLevel
!     call DclSetShadeLevel( shade_min, shade_max,   &
! &                          (shade_max-shade_min)/s_num )
  end if

  call uelset('ltone',.true.)

!  if(present(nongrid))then
!     if(nongrid(1:1)=='o')then
        call DclSetXGrid( x )
!     end if
!     if(nongrid(2:2)=='o')then
        call DclSetYgrid( y )
!     end if
!  end if

!  if(monoto.eqv..true.)then
!     call DclShadeContour( shade )
!  else
     call DclShadeContour( shade )
!  end if

  if(DclGetContourLevelNumber()==0)then
     call DclSetContourLabelFormat(trim(form_typec))
     call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
  end if

  call DclDrawContour( contour )

  call g2qctm( cxmin, cxmax, cymin, cymax )
  call DclSetWindow( cxmin, cxmax, cymin, cymax )
  call DclSetTransNumber(1)
  call DclSetTransFunction

  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )
  if(mxitv_flag.eqv..true.)then  ! automatic label format less than 1.0
     call auto_label_fmt( mxitv(1), x(nx), fmt_mxitv, x(1) )
     CALL USCSET( 'CXFMT', trim(adjustl(fmt_mxitv) ))
  end if
  if(myitv_flag.eqv..true.)then  ! automatic label format less than 1.0
     call auto_label_fmt( myitv(1), y(ny), fmt_myitv, y(1) )
     CALL USCSET( 'CYFMT', trim(adjustl(fmt_myitv) ))
  end if
  if((mxitv_flag.eqv..true.).and.(myitv_flag.eqv..true.))then
     call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
     call DclDrawAxis( 'rl', myitv(1), myitv(2) )
  else
     if(mxitv_flag.eqv..true.)then
        call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
        call DclDrawScaledAxis( 'rl' )
     else if(myitv_flag.eqv..true.)then
        call DclDrawAxis( 'rl', myitv(1), myitv(2) )
        call DclDrawScaledAxis( 'bt' )
     else
        call DclDrawScaledAxis
     end if
  end if
  call DclDrawTitle( 'b', trim(x_title), 0.0 )
  call DclDrawTitle( 'l', trim(y_title), 0.0 )
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

!-- 地形領域に色塗り
  if(present(trn_paint))then
     if(trn_paint.eqv..true.)then
        if(present(trn_col))then
           trn_color=trn_col
        else
           trn_color=1999
        end if
        do i=1,nx
           trn(i)=grid_point(i,1)
           trn_x(i)=x(i)
!        if(bot(i)==trn(i))then
!           call DclShadeRegion( )
!        end if
        end do
        trn(nx+1)=cymin
        trn(nx+2)=cymin
        trn_x(nx+1)=x(nx)
        trn_x(nx+2)=x(1)

        call DclShadeRegion( trn_x(1:nx+2), trn(1:nx+2), trn_color)
     end if
  end if

!  call DclSetContourLabelFormat(form_typec)
!  call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/c_num )
!  call DclDrawContour( contour )

  interc=DclGetContourInterval(1)
  write(*,*) interc
  write(val_c,'(E10.3)') interc

  call DclDrawTitle('b','_CONTOUR INTERVAL ='//val_c//'"',0.0,1)

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 'l', xg, yg, zg, color_num, (/shade_min, shade_max/) )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              if(present(l_idx))then
                 call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
              end if
              if(present(l_typ))then
                 call DclSetLineType( l_typ(i) )
              end if
              call DclDrawLine( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )

  if(present(zp))then
     call color_line( 'p', xp, yp, zp, color_num, (/shade_min, shade_max/) )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           if(present(p_idx))then
              call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
           end if
           if(present(p_typ))then
              call DclSetMarkerType( p_typ(i) )
           end if
           if(present(p_siz))then
              call DclSetMarkerSize( p_siz(i) )
           end if
           call DclDrawMarker( xp(:,i), yp(:,i) )
        end do
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(present(layer_line))then
     if(layer_line.eqv..true.)then
        do i=1,ny
           call DclDrawLine( x, grid_point(:,i) )
        end do
     end if
  end if

  if(no_tone_flag.eqv..false.)then
     if(present(trigleg))then
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto,  &
  &                  trigle=trigleg )
     else
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto )
     end if
  end if

end subroutine Dcl_2D_cont_shade_terrain

!---------------------------------------------------------

subroutine Dcl_2D_cont_shade_calendar( outname,  &
  &  x, y, contour, shade, cont_int, shade_int,  &
  &  axis_title, date, days, form_type, viewx_int, viewy_int, c_num,  &
!  &  viewy_min, viewy_max, color_num, cont_num, nongrid,  &
  &  xg, yg, zg, xp, yp, zp,  &
  &  mono, mono_val, mono_lev, trigleg, no_tone, no_frame,  &
  &  l_idx, l_typ, p_idx, p_typ, p_siz, tonbf, calmod,  &
  &  date_strt, date_end )
  ! 2 次元で 2 変数を等値線とカラーシェードで描画する. calender 対応
  use dcl
  implicit none
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! x 方向の格子点座標
  real, intent(in) :: y(:)  ! y 方向の格子点座標
  real, intent(inout) :: contour(size(x),size(y))  ! 等値線に描く配列
  real, intent(inout) :: shade(size(x),size(y))  ! カラーシェードに描く配列
  real, intent(in) :: cont_int(2)  ! 等値線の上下端 
                                 ! [cont_int(1)=cont_min, cont_int(2)=cont_max]
  real, intent(in) :: shade_int(2)  ! 等値線の上下端 [shade_int(1)=shade_min,
                                 ! shade_int(2)=shade_max]
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                                 ! axis_title(1)=x_title, axis_title(2)=y_title
  type(dcl_date), intent(in) :: date  ! 開始日付 [yyyy:mm:dd]
  integer, intent(in) :: days  ! 描画日数 [day]
  character(*), intent(in) :: form_type(2)  ! フォーマット
                           ! form_type(1)=form_typec, form_type(2)=form_types
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  integer, intent(in), optional :: c_num(2)  ! コンター・シェードの数
                           ! c_num(1)=cont_num, c_num(2)=color_num
!  character(2), intent(in), optional :: nongrid  ! 不等間隔格子にするか.
                                        ! nongrid = 'ox' で判断.
                                        ! 1 文字目が横軸, 2 文字目が縦軸.
                                        ! o = 不等間隔, x = 等間隔.
                                        ! デフォルトでは 'xx'.
  real, intent(in), optional :: xg(:,:)  ! x 軸に入れるグリッド線の座標
  real, intent(in), optional :: yg(:,:)  ! y 軸に入れるグリッド線の座標
                    ! 第一要素が線の位置データで, 複数本描く場合は,
                    ! 第二要素を 2 個以上にして描く.
                    ! 配列に入れるデータ次第で直線ではなく, 曲線グリッドを
                    ! 描くことも可能.
                    ! 3 本の線を描く場合は, xg(:,1) と yg(:,1) で 1 本の
                    ! 線を表すように指定すること.
  real, intent(in), optional :: zg(:,:)  ! グリッド線が値をもっていればその値.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラー折れ線を描くモードに移行する.
  real, intent(in), optional :: xp(:,:)  ! x 方向にマーカーを入れる x 座標
  real, intent(in), optional :: yp(:,:)  ! y 方向にマーカーを入れる y 座標
  real, intent(in), optional :: zp(:,:)  ! マーカーが値をもっていればその値.
                                         ! これらの指定方法は線と同じ.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラーマーカーを描くモードに移行する.
  logical, intent(in), optional :: mono  ! モノトーンの階調にする [.true.]
                                         ! デフォルトは .false.
  real, intent(in), optional :: mono_val(:)  ! 階調の境界値.
                    ! mono=.true. のときに必ず設定しないとエラーを返す.
                    ! 値は mono_lev + 1 成分存在しなければならない.
  integer, intent(in), optional :: mono_lev(:)  ! トーンマップ番号. dcl の 3 桁
                    ! mono=.true. のときに設定しないとエラーを返す.
  character(1), intent(in), optional :: trigleg  ! トーンバーの三角形オプション.
                ! オプションの値は, tone_bar ルーチンの trigle と同じ.
  logical, intent(in), optional :: no_tone  ! トーンバーを作成しないオプション
                                   ! .false. = 作成する. .true. = 作成しない.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(:)  ! xg, yg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: l_typ(:)  ! xg, yg で描く線のタイプ
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: p_idx(:)  ! xp, yp で描くマーカーのインデックス
  integer, intent(in), optional :: p_typ(:)  ! xp, yp で描くマーカーのタイプ
  real, intent(in), optional :: p_siz(:)  ! xp, yp で描くマーカーの大きさ
  character(1), intent(in), optional :: tonbf  ! UETONE FLAG (a,b,e,f,..)
                                          ! default 'f' = UETONF
  character(1), intent(in), optional :: calmod  ! Calendar direction (x or y)
                                          ! default 'x' = x direction
  integer, intent(in), optional :: date_strt  ! 開始日付 [hour]
  integer, intent(in), optional :: date_end   ! 終了日付 [hour]

!-- 以上, 引数
  real :: cont_min  ! 等値線を描く最小値
  real :: cont_max  ! 等値線を描く最大値
  real :: shade_min  ! シェードを描く最小値
  real :: shade_max  ! シェードを描く最大値
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  character(10) :: form_typec  ! contour 用のフォーマット
  character(10) :: form_types  ! shade 用のフォーマット
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
  integer :: cont_num  ! 等値線の数
  integer :: color_num  ! カラーの数
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k  ! 作業用添字
  integer :: nx, ny
  real :: undef, RMISS
  character(1) :: cal_mode
  logical :: monoto, no_tone_flag, no_frame_flag

  nx=size(x)
  ny=size(y)

!-- 日付が与えられているかを表示
  write(*,*) "start day is", date%year, date%month, date%day

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(c_num))then
     cont_num=c_num(1)
     color_num=c_num(2)
  else
     cont_num=10
     color_num=56
  end if

  if(present(no_tone))then
     no_tone_flag=no_tone
  else
     no_tone_flag=.false.
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  if(present(calmod))then
     cal_mode=calmod
  else
     cal_mode='x'
  end if

!-- 引数の置き換え用変数に置き換え
  cont_min=cont_int(1)
  cont_max=cont_int(2)
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))
  form_typec=trim(form_type(1))
  form_types=trim(form_type(2))

  if(present(mono))then
     if(present(mono_val).and.present(mono_lev))then
        if(size(mono_val)-1==size(mono_lev))then
           monoto=mono
        else
           write(*,*) "*** ERROR ***"
           write(*,*) "[array number] : mono_val = mono_lev + 1"
           write(*,*) "STOP"
           stop
        end if
     else
        write(*,*) "*** ERROR ***"
        write(*,*) "When option MONO is true, MONO_VAL and MONO_LEV must be specified."
        write(*,*) "STOP"
        stop
     end if
  else
     monoto=.false.
  end if

!-- 処理ここまで ---

  call undef_CReSS2Dcl( nx, ny, 1, contour)
  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  call DclSetWindow( x(1), x(nx), y(1), y(ny) )
!  if(cal_mode(1:1)=='x')then   ! CAL is x direction (default)
!     call DclSetWindow( 0.0, real(days), y(1), y(ny) )
!  else   ! CAL is y direction
!     call DclSetWindow( x(1), x(nx), 0.0, real(days) )
!  end if

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 's', xg, yg, zg, color_num,  &
  &                      (/shade_min, shade_max/), subsubidx='l' )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              call DclScalingPoint( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  if(present(zp))then
     call color_line( 's', xp, yp, zp, color_num, (/shade_min, shade_max/),  &
  &                   subsubidx='p' )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           call DclScalingPoint( xp(:,i), yp(:,i) )
        end do
     end if
  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  call DclSetTransFunction

  if(monoto.eqv..true.)then
     call SGLSET( 'LSOFTF', .true. )
     call DclClearShadeLevel
     call DclSetShadeLevel( mono_val, mono_lev )
  else
     call SGLSET( 'LSOFTF', .false. )
!     call DclClearShadeLevel
!     call DclSetShadeLevel( shade_min, shade_max,   &
! &                          (shade_max-shade_min)/s_num )
  end if

!  if(present(nongrid))then
!     if(nongrid(1:1)=='o')then
        call DclSetXGrid( x )
!     end if
!     if(nongrid(2:2)=='o')then
        call DclSetYgrid( y )
!     end if
!  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(monoto.eqv..true.)then
     call DclShadeContour( shade )
  else
     if(present(tonbf))then
        select case (tonbf(1:1))
        case ('b')
           call UETONB( shade, nx, nx, ny )
        case ('c')
           call UETONC( shade, nx, nx, ny )
        case ('e')
           call UETONE( shade, nx, nx, ny )
        case ('f')
           call DclShadeContourEx( shade )
        end select
     else
        call UETONE( shade, nx, nx, ny )
!        call DclShadeContourEx( shade )
     end if
  end if

  if(cal_mode(1:1)=='x')then   ! CAL is x direction (default)
     CALL UZLSET( 'LABELYL', .TRUE. )
     CALL UZLSET( 'LABELYR', .FALSE. )
     call DclDrawAxisCalendar( 'bt', date, nd=days )
     call DclDrawScaledAxis( 'lr' )
     call DclDrawTitle( 'l', trim(y_title), 0.0 )
  else  ! CAL is y direction
     CALL UZLSET( 'LABELXB', .TRUE. )
     CALL UZLSET( 'LABELXT', .FALSE. )
     call DclDrawAxisCalendar( 'lr', date, nd=days )
     call DclDrawScaledAxis( 'bt' )
     call DclDrawTitle( 'b', trim(x_title), 0.0 )
  end if
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

  if(DclGetContourLevelNumber()==0)then
     call DclSetContourLabelFormat(trim(form_typec))
     call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
  end if

  call DclDrawContour( contour )

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 'l', xg, yg, zg, color_num, (/shade_min, shade_max/) )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              if(present(l_idx))then
                 call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
              end if
              if(present(l_typ))then
                 call DclSetLineType( l_typ(i) )
              end if
              call DclDrawLine( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )

  if(present(zp))then
     call color_line( 'p', xp, yp, zp, color_num, (/shade_min, shade_max/) )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           if(present(p_idx))then
              call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
           end if
           if(present(p_typ))then
              call DclSetMarkerType( p_typ(i) )
           end if
           if(present(p_siz))then
              call DclSetMarkerSize( p_siz(i) )
           end if
           call DclDrawMarker( xp(:,i), yp(:,i) )
        end do
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(no_tone_flag.eqv..false.)then
     if(present(trigleg))then
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto,  &
  &                  trigle=trigleg )
     else
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto )
     end if
  end if

end subroutine Dcl_2D_cont_shade_calendar

!---------------------------------------------------------

subroutine Dcl_2D_cont_shade_vec( outname,  &
  &  x, y, contour, shade, vecx, vecy, vn, cont_int, shade_int,  &
  &  axis_title, form_type, viewx_int, viewy_int, c_num,  &
!  &  viewy_min, viewy_max, color_num, cont_num, nongrid,  &
  &  xg, yg, zg, xp, yp, zp, mxitv, myitv,  &
  &  mono, mono_val, mono_lev, trigleg,  &
  &  unitv, vfact, unit_fact_sign, &
  &  unit_fact, unit_title, unit_posi, no_tone, no_frame,  &
  &  l_idx, l_typ, p_idx, p_typ, p_siz, tonbf,  &
  &  axlbl, axtck, cxlbl, aylbl, aytck, cylbl )
  ! 2 次元で 3 変数を等値線, カラーシェード, ベクトルで描画する.
  ! 最大 4 変数同時描画が可能となる.
  ! 基本的に右にカラーバーがつくので, ユニットベクトルは
  ! コンターインターバルの下に文字で表示される.
  use dcl
  implicit none
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! x 方向の格子点座標
  real, intent(in) :: y(:)  ! y 方向の格子点座標
  real, intent(inout) :: contour(size(x),size(y))  ! 等値線に描く配列
  real, intent(inout) :: shade(size(x),size(y))  ! カラーシェードに描く配列
  real, intent(inout) :: vecx(size(x),size(y))  ! x 方向のベクトル
  real, intent(inout) :: vecy(size(x),size(y))  ! x 方向のベクトル
  integer, intent(in) :: vn(2)  ! ベクトル格子点 (間引き使用)
                                ! vn(1)=vnx, vn(2)=vny
  real, intent(in) :: cont_int(2)  ! 等値線の上下端 
                                 ! [cont_int(1)=cont_min, cont_int(2)=cont_max]
  real, intent(in) :: shade_int(2)  ! 等値線の上下端 [shade_int(1)=shade_min,
                                 ! shade_int(2)=shade_max]
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                                 ! axis_title(1)=x_title, axis_title(2)=y_title
  character(*), intent(in) :: form_type(2)  ! フォーマット
                           ! form_type(1)=form_typec, form_type(2)=form_types
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  integer, intent(in), optional :: c_num(2)  ! コンター・シェードの数
                           ! c_num(1)=cont_num, c_num(2)=color_num
!  character(2), intent(in), optional :: nongrid  ! 不等間隔格子にするか.
                                        ! nongrid = 'ox' で判断.
                                        ! 1 文字目が横軸, 2 文字目が縦軸.
                                        ! o = 不等間隔, x = 等間隔.
                                        ! デフォルトでは 'xx'.
  real, intent(in), optional :: xg(:,:)  ! x 軸に入れるグリッド線の座標
  real, intent(in), optional :: yg(:,:)  ! y 軸に入れるグリッド線の座標
                    ! 第一要素が線の位置データで, 複数本描く場合は,
                    ! 第二要素を 2 個以上にして描く.
                    ! 配列に入れるデータ次第で直線ではなく, 曲線グリッドを
                    ! 描くことも可能.
                    ! 3 本の線を描く場合は, xg(:,1) と yg(:,1) で 1 本の
                    ! 線を表すように指定すること.
  real, intent(in), optional :: zg(:,:)  ! グリッド線が値をもっていればその値.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラー折れ線を描くモードに移行する.
  real, intent(in), optional :: xp(:,:)  ! x 方向にマーカーを入れる x 座標
  real, intent(in), optional :: yp(:,:)  ! y 方向にマーカーを入れる y 座標
  real, intent(in), optional :: zp(:,:)  ! マーカーが値をもっていればその値.
                                         ! これらの指定方法は線と同じ.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラーマーカーを描くモードに移行する.
  real, intent(in), optional :: mxitv(2)   ! 横軸の目盛り間隔 (mxitv(1):大目盛り)
  real, intent(in), optional :: myitv(2)   ! 横軸の目盛り間隔 (myitv(1):大目盛り)
  logical, intent(in), optional :: mono  ! モノトーンの階調にする [.true.]
                                         ! デフォルトは .false.
  real, intent(in), optional :: mono_val(:)  ! 階調の境界値.
                    ! mono=.true. のときに必ず設定しないとエラーを返す.
                    ! 値は mono_lev + 1 成分存在しなければならない.
  integer, intent(in), optional :: mono_lev(:)  ! トーンマップ番号. dcl の 3 桁
                    ! mono=.true. のときに設定しないとエラーを返す.
  character(1), intent(in), optional :: trigleg  ! トーンバーの三角形オプション.
                ! オプションの値は, tone_bar ルーチンの trigle と同じ.
  logical, intent(in), optional :: unitv  ! 単位ベクトルを描くかどうか. default = .true.
  real, intent(in), optional :: vfact(2)  ! x,y 方向のスケーリングファクター
                    ! この値を指定すると, 内部的に決められないので, ベクトルが格子以上に
                    ! 伸びる可能性がある. 
                    ! 設定しない場合は, x, y の水平スケールと V 系のアスペクト比を考慮
                    ! して, vfact と一致させるようにする.
  logical, intent(in), optional :: unit_fact_sign  ! unitv = .true. のとき,
                    ! .true. = u, v の U 座標系での値を unit_fact に与えると, 
                    ! unit_fact はその値を単位ベクトルの単位として表示する.
                    ! unit の V 座標系の値は u, v の大きい方を 0.1 として表示する.
  real, intent(in), optional :: unit_fact(2)  ! x,y の単位ベクトルの v 座標系での長さ
                                              ! default = (0.1,0.1)
  character(*), intent(in), optional :: unit_title(2)  ! x,y の単位ベクトルのタイトル
                    ! default = 描かない.
  real, intent(in), optional :: unit_posi(2)  ! 単位ベクトルを描き始める原点座標 (V 系)
                    ! default = カラーバーの左端と同じで, 図の右下端から開始.
                    ! カラーバーはこれにぶつからないように自動的に短くする.
  logical, intent(in), optional :: no_tone  ! トーンバーを作成しないオプション
                                   ! .false. = 作成する. .true. = 作成しない.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(:)  ! xg, yg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: l_typ(:)  ! xg, yg で描く線のタイプ
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: p_idx(:)  ! xp, yp で描くマーカーのインデックス
  integer, intent(in), optional :: p_typ(:)  ! xp, yp で描くマーカーのタイプ
  real, intent(in), optional :: p_siz(:)  ! xp, yp で描くマーカーの大きさ
  character(1), intent(in), optional :: tonbf  ! UETONE FLAG (a,b,e,f,..)
                                          ! default 'f' = UETONF
  real, intent(in), optional :: axlbl(:)  ! X 座標のラベル描画での座標値
  real, intent(in), optional :: axtck(:)  ! X 座標の小目盛での座標値
  character(*), intent(in), optional :: cxlbl(:)  ! X 座標のラベル描画
  real, intent(in), optional :: aylbl(:)  ! Y 座標のラベル描画での座標値
  real, intent(in), optional :: aytck(:)  ! Y 座標の小目盛での座標値
  character(*), intent(in), optional :: cylbl(:)  ! Y 座標のラベル描画
!-- 以上, 引数
  integer :: vnx  ! x 方向のベクトル格子点 (間引き使用)
  integer :: vny  ! y 方向のベクトル格子点 (間引き使用)
  real :: cont_min  ! 等値線を描く最小値
  real :: cont_max  ! 等値線を描く最大値
  real :: shade_min  ! シェードを描く最小値
  real :: shade_max  ! シェードを描く最大値
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  character(10) :: form_typec  ! contour 用のフォーマット
  character(10) :: form_types  ! shade 用のフォーマット
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
  integer :: cont_num  ! 等値線の数
  integer :: color_num  ! カラーの数
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k  ! 作業用添字
  integer :: nx, ny
  real :: factx, facty
  real, dimension(size(x),size(y)) :: um, vm  ! ベクトル間引き後の値を代入
  real :: vvx_min, vvx_max, vvy_min, vvy_max
  real :: unitvp(2), unitvl(2), unit_auto_fact(2)
  real :: undef, RMISS
  intrinsic :: nint
  character(20) :: fmt_mxitv, fmt_myitv
  logical :: monoto, unitvs, no_tone_flag, no_frame_flag
  logical :: mxitv_flag, myitv_flag, cxlbl_flag, cylbl_flag

  nx=size(x)
  ny=size(y)

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(c_num))then
     cont_num=c_num(1)
     color_num=c_num(2)
  else
     cont_num=10
     color_num=56
  end if

  if(present(no_tone))then
     no_tone_flag=no_tone
  else
     no_tone_flag=.false.
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  mxitv_flag=.false.
  myitv_flag=.false.
  if(present(mxitv))then
     if(mxitv(1)/=0.0.and.mxitv(2)/=0.0)then
        mxitv_flag=.true.
     end if
  end if
  if(present(myitv))then
     if(myitv(1)/=0.0.and.myitv(2)/=0.0)then
        myitv_flag=.true.
     end if
  end if

  cxlbl_flag=.false.
  cylbl_flag=.false.
  if(present(cxlbl))then
     if(len_trim(cxlbl(1))>0)then
        cxlbl_flag=.true.
        mxitv_flag=.false.
     end if
  end if
  if(present(cylbl))then
     if(len_trim(cylbl(1))>0)then
        cylbl_flag=.true.
        myitv_flag=.false.
     end if
  end if

!-- 引数の置き換え用変数に置き換え
  vnx=vn(1)
  vny=vn(2)
  cont_min=cont_int(1)
  cont_max=cont_int(2)
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))
  form_typec=trim(form_type(1))
  form_types=trim(form_type(2))

!-- エラー処理
  if(nx<vnx.or.ny<vny)then
     write(*,*) "*****ERROR***** : vnx > nx or vny > ny."
     stop
  end if
  if(nx<2.or.vnx<2.or.ny<2.or.vny<2)then
     write(*,*) "*****ERROR***** : nx or ny or vnx or vny is less than 2."
     stop
  end if

!-- 警告
  if(mod((nx-1),(vnx-1))/=0.and.mod((ny-1),(vny-1))/=0)then
     write(*,*) "****WARNING**** : vnx or vny is not the factor of nx and ny."
  else
     if(mod((nx-1),(vnx-1))/=0.or.mod((ny-1),(vny-1))/=0)then
        if(mod((nx-1),(vnx-1))/=0)then
           write(*,*) "****WARNING**** : vnx is not the factor of nx."
        else
           write(*,*) "****WARNING**** : vny is not the factor of ny."
        end if
     end if
  end if

!-- ベクトル場の間引き
  factx=real(nx-1)/real(vnx-1)
  facty=real(ny-1)/real(vny-1)

  um=0.0
  vm=0.0

!-- 起点を 1 から始める
  um(1,1)=vecx(1,1)
  vm(1,1)=vecy(1,1)

  do i=2,vnx
     um(1+nint(factx*(i-1)),1)=vecx(1+nint(factx*(i-1)),1)
     vm(1+nint(factx*(i-1)),1)=vecy(1+nint(factx*(i-1)),1)
  end do

  do j=2,vny
     um(1,1+nint((j-1)*facty))=vecx(1,1+nint((j-1)*facty))
     vm(1,1+nint((j-1)*facty))=vecy(1,1+nint((j-1)*facty))
  end do

  do j=2,vny
     do i=2,vnx
        um(1+nint(factx*(i-1)),1+nint(facty*(j-1)))  &
  &     =vecx(1+nint(factx*(i-1)),1+nint(facty*(j-1)))
        vm(1+nint(factx*(i-1)),1+nint(facty*(j-1)))  &
  &     =vecy(1+nint(factx*(i-1)),1+nint(facty*(j-1)))
     end do
  end do

  if(present(mono))then
     if(present(mono_val).and.present(mono_lev))then
        if(size(mono_val)-1==size(mono_lev))then
           monoto=mono
        else
           write(*,*) "*** ERROR ***"
           write(*,*) "[array number] : mono_val = mono_lev + 1"
           write(*,*) "STOP"
           stop
        end if
     else
        write(*,*) "*** ERROR ***"
        write(*,*) "When option MONO is true, MONO_VAL and MONO_LEV must be specified."
        write(*,*) "STOP"
        stop
     end if
  else
     monoto=.false.
  end if

!-- 処理ここまで ---

  call undef_CReSS2Dcl( nx, ny, 1, contour)
  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  call DclSetWindow( x(1), x(nx), y(1), y(ny) )

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 's', xg, yg, zg, color_num,  &
  &                      (/shade_min, shade_max/), subsubidx='l' )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              call DclScalingPoint( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  if(present(zp))then
     call color_line( 's', xp, yp, zp, color_num, (/shade_min, shade_max/),  &
  &                   subsubidx='p' )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           call DclScalingPoint( xp(:,i), yp(:,i) )
        end do
     end if
  end if

!-- ベクトルスケールについての設定
  if(present(unit_fact_sign))then
     if(unit_fact_sign.eqv..true.)then
        if(present(unit_fact))then
           unit_auto_fact(1)=unit_fact(1)
           unit_auto_fact(2)=unit_fact(2)
        else
           write(*,*) "### ERROR ### : unit_fact_sign is .true. then,"
           write(*,*) "                unit_fact must configure."
           write(*,*) "STOP."
           stop
        end if
     else
        unit_auto_fact(1)=1.0
        unit_auto_fact(2)=1.0
     end if
  else
     unit_auto_fact(1)=1.0
     unit_auto_fact(2)=1.0
  end if

  if(present(vfact))then
     call DclSetParm( 'VECTOR:LNRMAL', .false. )
     call DclSetParm( 'VECTOR:XFACT1', vfact(1) )
     call DclSetParm( 'VECTOR:YFACT1', vfact(2) )
     unit_auto_fact(1)=unit_auto_fact(1)*vfact(1)
     unit_auto_fact(2)=unit_auto_fact(2)*vfact(2)
  else
     call DclSetParm( 'VECTOR:LNRMAL', .true.)
     call DclSetParm( 'VECTOR:XFACT1', unitvl(1) )
     call DclSetParm( 'VECTOR:YFACT1', unitvl(2) )
     unit_auto_fact(1)=unit_auto_fact(1)*unitvl(1)
     unit_auto_fact(2)=unit_auto_fact(2)*unitvl(2)
     unitvl=0.0
  end if

!-- ユニットベクトルについての設定
  if(present(unitv))then
     unitvs=unitv
  else
     unitvs=.true.
  end if

  if(unitvs.eqv..true.)then

     call DclSetParm( 'VECTOR:LUNIT', unitvs )

     !-- 単位ベクトルの長さ
     if(present(unit_fact))then
        if(present(unit_fact_sign))then
           if(unit_fact_sign.eqv..true.)then
              unitvl(:)=unit_auto_fact(:)
           else
              unitvl(:)=unit_fact(:)
           end if
        else
           unitvl(:)=unit_fact(:)
        end if
     else
        unitvl=(/0.1, 0.1/)
     end if

     !-- 単位ベクトルの書き始めの位置
     if(present(unit_posi))then
        vvx_min=unit_posi(1)
        vvy_min=unit_posi(2)
     else
        vvx_min=vx_max+0.05
        vvy_min=vy_min
     end if

     vvy_max=vvy_min+unitvl(2)+0.05

     call DclSetParm( 'VECTOR:VXUNIT', unitvl(1) )
     call DclSetParm( 'VECTOR:VYUNIT', unitvl(2) )
     call DclSetParm( 'VECTOR:VXULOC', vvx_min )
     call DclSetParm( 'VECTOR:VYULOC', vvy_min )

     !-- タイトルを書くかどうか
     if(present(unit_title))then
        if(len_trim(unit_title(2))>0)then
           call DclSetUnitVectorTitle( 'Y', trim(unit_title(2)) )
        end if
        if(len_trim(unit_title(1))>0)then
           call DclSetUnitVectorTitle( 'X', trim(unit_title(1)) )
        end if
        call DclSetParm( 'VECTOR:LUMSG', .false. )
     else  ! タイトルを書かないなら, グラフの下部にスケーリングファクターを明記
        call DclSetParm( 'VECTOR:LUMSG', .true. )
     end if

  else
     call DclSetParm( 'VECTOR:LUNIT', unitvs )
     vvx_min=0.0
     vvx_max=0.0
     vvy_min=0.0
     vvy_max=vy_min
  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  call DclSetTransFunction

  if(monoto.eqv..true.)then
     call SGLSET( 'LSOFTF', .true. )
     call DclClearShadeLevel
     call DclSetShadeLevel( mono_val, mono_lev )
  else
     call SGLSET( 'LSOFTF', .false. )
!     call DclClearShadeLevel
!     call DclSetShadeLevel( shade_min, shade_max,   &
! &                          (shade_max-shade_min)/s_num )
  end if

!  if(present(nongrid))then
!     if(nongrid(1:1)=='o')then
        call DclSetXGrid( x )
!     end if
!     if(nongrid(2:2)=='o')then
        call DclSetYgrid( y )
!     end if
!  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(monoto.eqv..true.)then
     call DclShadeContour( shade )
  else
     if(present(tonbf))then
        select case (tonbf(1:1))
        case ('b')
           call UETONB( shade, nx, nx, ny )
        case ('c')
           call UETONC( shade, nx, nx, ny )
        case ('e')
           call UETONE( shade, nx, nx, ny )
        case ('f')
           call DclShadeContourEx( shade )
        end select
     else
        call DclShadeContourEx( shade )
     end if
  end if

  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )
  if(mxitv_flag.eqv..true.)then  ! automatic label format less than 1.0
     call auto_label_fmt( mxitv(1), x(nx), fmt_mxitv, x(1) )
     CALL USCSET( 'CXFMT', trim(adjustl(fmt_mxitv) ))
  end if
  if(myitv_flag.eqv..true.)then  ! automatic label format less than 1.0
     call auto_label_fmt( myitv(1), y(ny), fmt_myitv, y(1) )
     CALL USCSET( 'CYFMT', trim(adjustl(fmt_myitv) ))
  end if

  if(cxlbl_flag.eqv..true.)then
     call DclDrawAxisSpecify( 'b', axlbl, tick_pos=axtck, label=cxlbl )
     call DclDrawAxisSpecify( 't', axlbl, tick_pos=axtck )
  else if(mxitv_flag.eqv..true.)then
     call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
  else
     call DclDrawScaledAxis( 'bt' )
  end if
  if(cylbl_flag.eqv..true.)then
     call DclDrawAxisSpecify( 'l', aylbl, tick_pos=aytck, label=cylbl )
     call DclDrawAxisSpecify( 'r', aylbl, tick_pos=aytck )
  else if(myitv_flag.eqv..true.)then
     call DclDrawAxis( 'rl', myitv(1), myitv(2) )
  else
     call DclDrawScaledAxis( 'rl' )
  end if

  call DclDrawTitle( 'b', trim(x_title), 0.0 )
  call DclDrawTitle( 'l', trim(y_title), 0.0 )
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

  if(DclGetContourLevelNumber()==0)then
     call DclSetContourLabelFormat(trim(form_typec))
     call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
  end if

  call DclDrawContour( contour )

  call DclSetParm( "GRAPH:LCLIP", .false. )

  call DclDrawVectors( um, vm )

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 'l', xg, yg, zg, color_num, (/shade_min, shade_max/) )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              if(present(l_idx))then
                 call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
              end if
              if(present(l_typ))then
                 call DclSetLineType( l_typ(i) )
              end if
              call DclDrawLine( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )

  if(present(zp))then
     call color_line( 'p', xp, yp, zp, color_num, (/shade_min, shade_max/) )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           if(present(p_idx))then
              call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
           end if
           if(present(p_typ))then
              call DclSetMarkerType( p_typ(i) )
           end if
           if(present(p_siz))then
              call DclSetMarkerSize( p_siz(i) )
           end if
           call DclDrawMarker( xp(:,i), yp(:,i) )
        end do
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(no_tone_flag.eqv..false.)then
     if(present(trigleg))then
!-- 単位ベクトルの表記を考え, vvy_max がトーンバーの下端
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vvy_max, vy_max/), trim(form_types), mono_log=monoto,  &
  &                  trigle=trigleg )
     else
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vvy_max, vy_max/), trim(form_types), mono_log=monoto )
     end if
  end if

end subroutine Dcl_2D_cont_shade_vec

!---------------------------------------------------------

subroutine Dcl_2D_cont_shade_vec_calendar( outname,  &
  &  x, y, contour, shade, vecx, vecy, vn, cont_int, shade_int,  &
  &  axis_title, date, days, form_type, viewx_int, viewy_int, c_num,  &
!  &  viewy_min, viewy_max, color_num, cont_num, nongrid,  &
!  &  xg, yg, mono, mono_val, mono_lev, trigleg, no_tone, no_frame )
  &  xg, yg, zg, xp, yp, zp,  &
  &  mono, mono_val, mono_lev, trigleg, unitv, vfact, unit_fact_sign, &
  &  unit_fact, unit_title, unit_posi, no_tone, no_frame,  &
  &  l_idx, l_typ, p_idx, p_typ, p_siz, tonbf, calmod,  &
  &  date_strt, date_end )
  ! 2 次元で 3 変数を等値線, カラーシェード, ベクトルで描画する.
  ! 最大 4 変数同時描画が可能となる.
  ! 基本的に右にカラーバーがつくので, ユニットベクトルは
  ! コンターインターバルの下に文字で表示される.
  use dcl
  implicit none
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! x 方向の格子点座標
  real, intent(in) :: y(:)  ! y 方向の格子点座標
  real, intent(inout) :: contour(size(x),size(y))  ! 等値線に描く配列
  real, intent(inout) :: shade(size(x),size(y))  ! カラーシェードに描く配列
  real, intent(inout) :: vecx(size(x),size(y))  ! x 方向のベクトル
  real, intent(inout) :: vecy(size(x),size(y))  ! x 方向のベクトル
  integer, intent(in) :: vn(2)  ! ベクトル格子点 (間引き使用)
                                ! vn(1)=vnx, vn(2)=vny
  real, intent(in) :: cont_int(2)  ! 等値線の上下端 
                                 ! [cont_int(1)=cont_min, cont_int(2)=cont_max]
  real, intent(in) :: shade_int(2)  ! 等値線の上下端 [shade_int(1)=shade_min,
                                 ! shade_int(2)=shade_max]
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                                 ! axis_title(1)=x_title, axis_title(2)=y_title
  type(dcl_date), intent(in) :: date  ! 開始日付 [yyyy:mm:dd]
  integer, intent(in) :: days  ! 描画日数 [day]
  character(*), intent(in) :: form_type(2)  ! フォーマット
                           ! form_type(1)=form_typec, form_type(2)=form_types
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  integer, intent(in), optional :: c_num(2)  ! コンター・シェードの数
                           ! c_num(1)=cont_num, c_num(2)=color_num
!  character(2), intent(in), optional :: nongrid  ! 不等間隔格子にするか.
                                        ! nongrid = 'ox' で判断.
                                        ! 1 文字目が横軸, 2 文字目が縦軸.
                                        ! o = 不等間隔, x = 等間隔.
                                        ! デフォルトでは 'xx'.
  real, intent(in), optional :: xg(:,:)  ! x 軸に入れるグリッド線の座標
  real, intent(in), optional :: yg(:,:)  ! y 軸に入れるグリッド線の座標
                    ! 第一要素が線の位置データで, 複数本描く場合は,
                    ! 第二要素を 2 個以上にして描く.
                    ! 配列に入れるデータ次第で直線ではなく, 曲線グリッドを
                    ! 描くことも可能.
                    ! 3 本の線を描く場合は, xg(:,1) と yg(:,1) で 1 本の
                    ! 線を表すように指定すること.
  real, intent(in), optional :: zg(:,:)  ! グリッド線が値をもっていればその値.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラー折れ線を描くモードに移行する.
  real, intent(in), optional :: xp(:,:)  ! x 方向にマーカーを入れる x 座標
  real, intent(in), optional :: yp(:,:)  ! y 方向にマーカーを入れる y 座標
  real, intent(in), optional :: zp(:,:)  ! マーカーが値をもっていればその値.
                                         ! これらの指定方法は線と同じ.
                    ! このオプションが選択された場合, 自動的に変数 shade は
                    ! 使用されずに, shade 関連の設定変数はすべてこの値を
                    ! リファレンスとしてカラーマーカーを描くモードに移行する.
  logical, intent(in), optional :: mono  ! モノトーンの階調にする [.true.]
                                         ! デフォルトは .false.
  real, intent(in), optional :: mono_val(:)  ! 階調の境界値.
                    ! mono=.true. のときに必ず設定しないとエラーを返す.
                    ! 値は mono_lev + 1 成分存在しなければならない.
  integer, intent(in), optional :: mono_lev(:)  ! トーンマップ番号. dcl の 3 桁
                    ! mono=.true. のときに設定しないとエラーを返す.
  character(1), intent(in), optional :: trigleg  ! トーンバーの三角形オプション.
                ! オプションの値は, tone_bar ルーチンの trigle と同じ.
  logical, intent(in), optional :: unitv  ! 単位ベクトルを描くかどうか. default = .true.
  real, intent(in), optional :: vfact(2)  ! x,y 方向のスケーリングファクター
                    ! この値を指定すると, 内部的に決められないので, ベクトルが格子以上に
                    ! 伸びる可能性がある. 
                    ! 設定しない場合は, x, y の水平スケールと V 系のアスペクト比を考慮
                    ! して, vfact と一致させるようにする.
  logical, intent(in), optional :: unit_fact_sign  ! unitv = .true. のとき,
                    ! .true. = u, v の U 座標系での値を unit_fact に与えると, 
                    ! unit_fact はその値を単位ベクトルの単位として表示する.
                    ! unit の V 座標系の値は u, v の大きい方を 0.1 として表示する.
  real, intent(in), optional :: unit_fact(2)  ! x,y の単位ベクトルの v 座標系での長さ
                                              ! default = (0.1,0.1)
  character(*), intent(in), optional :: unit_title(2)  ! x,y の単位ベクトルのタイトル
                    ! default = 描かない.
  real, intent(in), optional :: unit_posi(2)  ! 単位ベクトルを描き始める原点座標 (V 系)
                    ! default = カラーバーの左端と同じで, 図の右下端から開始.
                    ! カラーバーはこれにぶつからないように自動的に短くする.
  logical, intent(in), optional :: no_tone  ! トーンバーを作成しないオプション
                                   ! .false. = 作成する. .true. = 作成しない.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(:)  ! xg, yg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: l_typ(:)  ! xg, yg で描く線のタイプ
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: p_idx(:)  ! xp, yp で描くマーカーのインデックス
  integer, intent(in), optional :: p_typ(:)  ! xp, yp で描くマーカーのタイプ
  real, intent(in), optional :: p_siz(:)  ! xp, yp で描くマーカーの大きさ
  character(1), intent(in), optional :: tonbf  ! UETONE FLAG (a,b,e,f,..)
                                          ! default 'f' = UETONF
  character(1), intent(in), optional :: calmod  ! Calendar direction (x or y)
                                          ! default 'x' = x direction
  integer, intent(in), optional :: date_strt  ! 開始日付 [hour]
  integer, intent(in), optional :: date_end   ! 終了日付 [hour]

!-- 以上, 引数
  integer :: vnx  ! x 方向のベクトル格子点 (間引き使用)
  integer :: vny  ! y 方向のベクトル格子点 (間引き使用)
  real :: cont_min  ! 等値線を描く最小値
  real :: cont_max  ! 等値線を描く最大値
  real :: shade_min  ! シェードを描く最小値
  real :: shade_max  ! シェードを描く最大値
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  character(10) :: form_typec  ! contour 用のフォーマット
  character(10) :: form_types  ! shade 用のフォーマット
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
  integer :: cont_num  ! 等値線の数
  integer :: color_num  ! カラーの数
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k  ! 作業用添字
  integer :: nx, ny
  real :: factx, facty
  real, dimension(size(x),size(y)) :: um, vm  ! ベクトル間引き後の値を代入
  real :: vvx_min, vvx_max, vvy_min, vvy_max
  real :: unitvp(2), unitvl(2), unit_auto_fact(2)
  real :: undef, RMISS
  intrinsic :: nint
  character(1) :: cal_mode
  logical :: monoto, unitvs, no_tone_flag, no_frame_flag

  nx=size(x)
  ny=size(y)

 !-- 日付が与えられているかを表示
  write(*,*) "start day is", date%year, date%month, date%day

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(c_num))then
     cont_num=c_num(1)
     color_num=c_num(2)
  else
     cont_num=10
     color_num=56
  end if

  if(present(no_tone))then
     no_tone_flag=no_tone
  else
     no_tone_flag=.false.
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  if(present(calmod))then
     cal_mode=calmod
  else
     cal_mode='x'
  end if

!-- 引数の置き換え用変数に置き換え
  vnx=vn(1)
  vny=vn(2)
  cont_min=cont_int(1)
  cont_max=cont_int(2)
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))
  form_typec=trim(form_type(1))
  form_types=trim(form_type(2))

!-- エラー処理
  if(nx<vnx.or.ny<vny)then
     write(*,*) "*****ERROR***** : vnx > nx or vny > ny."
     stop
  end if
  if(nx<2.or.vnx<2.or.ny<2.or.vny<2)then
     write(*,*) "*****ERROR***** : nx or ny or vnx or vny is less than 2."
     stop
  end if

!-- 警告
  if(mod((nx-1),(vnx-1))/=0.and.mod((ny-1),(vny-1))/=0)then
     write(*,*) "****WARNING**** : vnx or vny is not the factor of nx and ny."
  else
     if(mod((nx-1),(vnx-1))/=0.or.mod((ny-1),(vny-1))/=0)then
        if(mod((nx-1),(vnx-1))/=0)then
           write(*,*) "****WARNING**** : vnx is not the factor of nx."
        else
           write(*,*) "****WARNING**** : vny is not the factor of ny."
        end if
     end if
  end if

!-- ベクトル場の間引き
  factx=real(nx-1)/real(vnx-1)
  facty=real(ny-1)/real(vny-1)

  um=0.0
  vm=0.0

!-- 起点を 1 から始める
  um(1,1)=vecx(1,1)
  vm(1,1)=vecy(1,1)

  do i=2,vnx
     um(1+nint(factx*(i-1)),1)=vecx(1+nint(factx*(i-1)),1)
     vm(1+nint(factx*(i-1)),1)=vecy(1+nint(factx*(i-1)),1)
  end do

  do j=2,vny
     um(1,1+nint((j-1)*facty))=vecx(1,1+nint((j-1)*facty))
     vm(1,1+nint((j-1)*facty))=vecy(1,1+nint((j-1)*facty))
  end do

  do j=2,vny
     do i=2,vnx
        um(1+nint(factx*(i-1)),1+nint(facty*(j-1)))  &
  &     =vecx(1+nint(factx*(i-1)),1+nint(facty*(j-1)))
        vm(1+nint(factx*(i-1)),1+nint(facty*(j-1)))  &
  &     =vecy(1+nint(factx*(i-1)),1+nint(facty*(j-1)))
     end do
  end do

  if(present(mono))then
     if(present(mono_val).and.present(mono_lev))then
        if(size(mono_val)-1==size(mono_lev))then
           monoto=mono
        else
           write(*,*) "*** ERROR ***"
           write(*,*) "[array number] : mono_val = mono_lev + 1"
           write(*,*) "STOP"
           stop
        end if
     else
        write(*,*) "*** ERROR ***"
        write(*,*) "When option MONO is true, MONO_VAL and MONO_LEV must be specified."
        write(*,*) "STOP"
        stop
     end if
  else
     monoto=.false.
  end if

!-- 処理ここまで ---

  call undef_CReSS2Dcl( nx, ny, 1, contour)
  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  if(cal_mode(1:1)=='x')then   ! CAL is x direction (default)
     call DclSetWindow( 0.0, real(days), y(1), y(ny) )
  else   ! CAL is y direction
     call DclSetWindow( x(1), x(nx), 0.0, real(days) )
  end if

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 's', xg, yg, zg, color_num,  &
  &                      (/shade_min, shade_max/), subsubidx='l' )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              call DclScalingPoint( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  if(present(zp))then
     call color_line( 's', xp, yp, zp, color_num, (/shade_min, shade_max/),  &
  &                   subsubidx='p' )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           call DclScalingPoint( xp(:,i), yp(:,i) )
        end do
     end if
  end if

!-- ベクトルスケールについての設定
  if(present(unit_fact_sign))then
     if(unit_fact_sign.eqv..true.)then
        if(present(unit_fact))then
           unit_auto_fact(1)=unit_fact(1)
           unit_auto_fact(2)=unit_fact(2)
        else
           write(*,*) "### ERROR ### : unit_fact_sign is .true. then,"
           write(*,*) "                unit_fact must configure."
           write(*,*) "STOP."
           stop
        end if
     else
        unit_auto_fact(1)=1.0
        unit_auto_fact(2)=1.0
     end if
  else
     unit_auto_fact(1)=1.0
     unit_auto_fact(2)=1.0
  end if

  if(present(vfact))then
     call DclSetParm( 'VECTOR:LNRMAL', .false. )
     call DclSetParm( 'VECTOR:XFACT1', vfact(1) )
     call DclSetParm( 'VECTOR:YFACT1', vfact(2) )
     unit_auto_fact(1)=unit_auto_fact(1)*vfact(1)
     unit_auto_fact(2)=unit_auto_fact(2)*vfact(2)
  else
     call DclSetParm( 'VECTOR:LNRMAL', .true.)
     call DclSetParm( 'VECTOR:XFACT1', unitvl(1) )
     call DclSetParm( 'VECTOR:YFACT1', unitvl(2) )
     unit_auto_fact(1)=unit_auto_fact(1)*unitvl(1)
     unit_auto_fact(2)=unit_auto_fact(2)*unitvl(2)
     unitvl=0.0
  end if

!-- ユニットベクトルについての設定
  if(present(unitv))then
     unitvs=unitv
  else
     unitvs=.true.
  end if

  if(unitvs.eqv..true.)then

     call DclSetParm( 'VECTOR:LUNIT', unitvs )

     !-- 単位ベクトルの長さ
     if(present(unit_fact))then
        if(present(unit_fact_sign))then
           if(unit_fact_sign.eqv..true.)then
              unitvl(:)=unit_auto_fact(:)
           else
              unitvl(:)=unit_fact(:)
           end if
        else
           unitvl(:)=unit_fact(:)
        end if
     else
        unitvl=(/0.1, 0.1/)
     end if

     !-- 単位ベクトルの書き始めの位置
     if(present(unit_posi))then
        vvx_min=unit_posi(1)
        vvy_min=unit_posi(2)
     else
        vvx_min=vx_max+0.05
        vvy_min=vy_min
     end if

     vvy_max=vvy_min+unitvl(2)+0.05

     call DclSetParm( 'VECTOR:VXUNIT', unitvl(1) )
!     call DclSetParm( 'VECTOR:VYUNIT', unitvl(2) )
     call DclSetParm( 'VECTOR:VYUNIT', 0.0 )  ! y 方向には書かない
     call DclSetParm( 'VECTOR:VXULOC', vvx_min )
     call DclSetParm( 'VECTOR:VYULOC', vvy_min )

     !-- タイトルを書くかどうか
     if(present(unit_title))then
        if(len_trim(unit_title(2))>0)then
           call DclSetUnitVectorTitle( 'Y', trim(unit_title(2)) )
        end if
        if(len_trim(unit_title(1))>0)then
           call DclSetUnitVectorTitle( 'X', trim(unit_title(1)) )
        end if
        call DclSetParm( 'VECTOR:LUMSG', .false. )
     else  ! タイトルを書かないなら, グラフの下部にスケーリングファクターを明記
        call DclSetParm( 'VECTOR:LUMSG', .true. )
     end if

  else
     call DclSetParm( 'VECTOR:LUNIT', unitvs )
     vvx_min=0.0
     vvx_max=0.0
     vvy_min=0.0
     vvy_max=vy_min
  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  call DclSetTransFunction

  if(monoto.eqv..true.)then
     call SGLSET( 'LSOFTF', .true. )
     call DclClearShadeLevel
     call DclSetShadeLevel( mono_val, mono_lev )
  else
     call SGLSET( 'LSOFTF', .false. )
!     call DclClearShadeLevel
!     call DclSetShadeLevel( shade_min, shade_max,   &
! &                          (shade_max-shade_min)/s_num )
  end if

!  if(present(nongrid))then
!     if(nongrid(1:1)=='o')then
        call DclSetXGrid( x )
!     end if
!     if(nongrid(2:2)=='o')then
        call DclSetYgrid( y )
!     end if
!  end if

  if(monoto.eqv..true.)then
     call DclShadeContour( shade )
  else
     if(present(tonbf))then
        select case (tonbf(1:1))
        case ('b')
           call UETONB( shade, nx, nx, ny )
        case ('c')
           call UETONC( shade, nx, nx, ny )
        case ('e')
           call UETONE( shade, nx, nx, ny )
        case ('f')
           call DclShadeContourEx( shade )
        end select
     else
        call DclShadeContourEx( shade )
     end if
  end if

  if(cal_mode(1:1)=='x')then   ! CAL is x direction (default)
     CALL UZLSET( 'LABELYL', .TRUE. )
     CALL UZLSET( 'LABELYR', .FALSE. )
     call DclDrawAxisCalendar( 'bt', date, nd=days )
     call DclDrawScaledAxis( 'lr' )
     call DclDrawTitle( 'l', trim(y_title), 0.0 )
  else  ! CAL is y direction
     CALL UZLSET( 'LABELXB', .TRUE. )
     CALL UZLSET( 'LABELXT', .FALSE. )
     call DclDrawAxisCalendar( 'lr', date, nd=days )
     call DclDrawScaledAxis( 'bt' )
     call DclDrawTitle( 'b', trim(x_title), 0.0 )
  end if
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

  if(DclGetContourLevelNumber()==0)then
     call DclSetContourLabelFormat(trim(form_typec))
     call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
  end if

  call DclDrawContour( contour )

  call DclSetParm( "GRAPH:LCLIP", .false. )

  call DclDrawVectors( um, vm )

  if(present(zg))then
     if(size(xg,1)>1)then
        call color_line( 'l', xg, yg, zg, color_num, (/shade_min, shade_max/) )
     end if
  else
     if(present(xg))then
        if(size(xg,1)>1)then
           do i=1,size(xg,2)
              if(present(l_idx))then
                 call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
              end if
              if(present(l_typ))then
                 call DclSetLineType( l_typ(i) )
              end if
              call DclDrawLine( xg(:,i), yg(:,i) )
           end do
        end if
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )

  if(present(zp))then
     call color_line( 'p', xp, yp, zp, color_num, (/shade_min, shade_max/) )
  else
     if(present(xp))then
        do i=1,size(xp,2)
           if(present(p_idx))then
              call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
           end if
           if(present(p_typ))then
              call DclSetMarkerType( p_typ(i) )
           end if
           if(present(p_siz))then
              call DclSetMarkerSize( p_siz(i) )
           end if
           call DclDrawMarker( xp(:,i), yp(:,i) )
        end do
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(no_tone_flag.eqv..false.)then
     if(present(trigleg))then
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min+0.05, vy_max/), trim(form_types), mono_log=monoto,  &
  &                  trigle=trigleg )
     else
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min+0.05, vy_max/), trim(form_types), mono_log=monoto )
     end if
  end if

end subroutine Dcl_2D_cont_shade_vec_calendar

!---------------------------------------------------------

subroutine Dcl_2D_cont_shade_polar( outname,  &
  &  x, y, contour, shade, cont_int, shade_int,  &
  &  axis_title, form_type, viewx_int, viewy_int, c_num,  &
!  &  viewy_min, viewy_max, color_num, cont_num, nongrid,  &
  &  xg, yg, rg, tg, xp, yp, rp, tp, mxitv, myitv,  &
  &  mono, mono_val, mono_lev, trigleg, no_tone, no_frame,  &
  &  l_idx, l_typ, r_idx, r_typ, p_idx, p_typ, p_siz,  &
  &  rp_idx, rp_typ, rp_siz, tonbf )
  ! 2 次元で 2 変数を等値線とカラーシェードで描画する.
  use dcl
  implicit none
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! 動径方向の格子点座標
  real, intent(in) :: y(:)  ! 同位角方向の格子点座標 [degree]
  real, intent(inout) :: contour(size(x),size(y))  ! 等値線に描く配列
  real, intent(inout) :: shade(size(x),size(y))  ! カラーシェードに描く配列
  real, intent(in) :: cont_int(2)  ! 等値線の上下端 
                                 ! [cont_int(1)=cont_min, cont_int(2)=cont_max]
  real, intent(in) :: shade_int(2)  ! 等値線の上下端 [shade_int(1)=shade_min,
                                 ! shade_int(2)=shade_max]
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                                 ! axis_title(1)=x_title, axis_title(2)=y_title
  character(*), intent(in) :: form_type(2)  ! フォーマット
                           ! form_type(1)=form_typec, form_type(2)=form_types
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  integer, intent(in), optional :: c_num(2)  ! コンター・シェードの数
                           ! c_num(1)=cont_num, c_num(2)=color_num
!  character(2), intent(in), optional :: nongrid  ! 不等間隔格子にするか.
                                        ! nongrid = 'ox' で判断.
                                        ! 1 文字目が横軸, 2 文字目が縦軸.
                                        ! o = 不等間隔, x = 等間隔.
                                        ! デフォルトでは 'xx'.
  real, intent(in), optional :: xg(:,:)  ! x 軸に入れるグリッド線の座標
  real, intent(in), optional :: yg(:,:)  ! y 軸に入れるグリッド線の座標
                    ! 第一要素が線の位置データで, 複数本描く場合は,
                    ! 第二要素を 2 個以上にして描く.
                    ! 配列に入れるデータ次第で直線ではなく, 曲線グリッドを
                    ! 描くことも可能.
                    ! 3 本の線を描く場合は, xg(:,1) と yg(:,1) で 1 本の
                    ! 線を表すように指定すること.
  real, intent(in), optional :: rg(:,:)  ! 極座標系における r 軸に入れるグリッド線の座標
  real, intent(in), optional :: tg(:,:)  ! 極座標系における theta 軸に入れるグリッド線の座標
                    ! これらのデータの与え方は xg, yg と同様.
  real, intent(in), optional :: xp(:,:)  ! x 方向にマーカーを入れる x 座標
  real, intent(in), optional :: yp(:,:)  ! y 方向にマーカーを入れる y 座標
  real, intent(in), optional :: rp(:,:)  ! r 方向にマーカーを入れる r 座標
  real, intent(in), optional :: tp(:,:)  ! t 方向にマーカーを入れる t 座標
  real, intent(in), optional :: mxitv(2)   ! 横軸の目盛り間隔 (mxitv(1):大目盛り)
  real, intent(in), optional :: myitv(2)   ! 横軸の目盛り間隔 (myitv(1):大目盛り)
  logical, intent(in), optional :: mono  ! モノトーンの階調にする [.true.]
                                         ! デフォルトは .false.
  real, intent(in), optional :: mono_val(:)  ! 階調の境界値.
                    ! mono=.true. のときに必ず設定しないとエラーを返す.
                    ! 値は mono_lev + 1 成分存在しなければならない.
  integer, intent(in), optional :: mono_lev(:)  ! トーンマップ番号. dcl の 3 桁
                    ! mono=.true. のときに設定しないとエラーを返す.
  character(1), intent(in), optional :: trigleg  ! トーンバーの三角形オプション.
                ! オプションの値は, tone_bar ルーチンの trigle と同じ.
  logical, intent(in), optional :: no_tone  ! トーンバーを作成しないオプション
                                   ! .false. = 作成する. .true. = 作成しない.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(:)  ! xg, yg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: l_typ(:)  ! xg, yg で描く線のタイプ
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: r_idx(:)  ! rg, tg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: r_typ(:)  ! rg, tg で描く線のタイプ
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: p_idx(:)  ! xp, yp で描くマーカーのインデックス
  integer, intent(in), optional :: p_typ(:)  ! xp, yp で描くマーカーのタイプ
  real, intent(in), optional :: p_siz(:)  ! xp, yp で描くマーカーの大きさ
  integer, intent(in), optional :: rp_idx(:)  ! rp, tp で描くマーカーのインデックス
  integer, intent(in), optional :: rp_typ(:)  ! rp, tp で描くマーカーのタイプ
  real, intent(in), optional :: rp_siz(:)  ! rp, tp で描くマーカーの大きさ
  character(1), intent(in), optional :: tonbf  ! UETONE FLAG (a,b,e,f,..)
                                          ! default 'f' = UETONF
!-- 以上, 引数
  real :: cont_min  ! 等値線を描く最小値
  real :: cont_max  ! 等値線を描く最大値
  real :: shade_min  ! シェードを描く最小値
  real :: shade_max  ! シェードを描く最大値
  real :: min_y, max_y  ! 方位角の最大値 (-180 <= y <= 180 で)
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  character(10) :: form_typec  ! contour 用のフォーマット
  character(10) :: form_types  ! shade 用のフォーマット
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
  integer :: cont_num  ! 等値線の数
  integer :: color_num  ! カラーの数
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k  ! 作業用添字
  integer :: nx, ny
  real :: undef, RMISS
  character(20) :: fmt_mxitv, fmt_myitv
  logical :: monoto, no_tone_flag, no_frame_flag
  logical :: mxitv_flag, myitv_flag

  nx=size(x)
  ny=size(y)

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(c_num))then
     cont_num=c_num(1)
     color_num=c_num(2)
  else
     cont_num=10
     color_num=56
  end if

  if(present(no_tone))then
     no_tone_flag=no_tone
  else
     no_tone_flag=.false.
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  mxitv_flag=.false.
  myitv_flag=.false.
  if(present(mxitv))then
     if(mxitv(1)/=0.0.and.mxitv(2)/=0.0)then
        mxitv_flag=.true.
     end if
  end if
  if(present(myitv))then
     if(myitv(1)/=0.0.and.myitv(2)/=0.0)then
        myitv_flag=.true.
     end if
  end if

!-- 引数の置き換え用変数に置き換え
  cont_min=cont_int(1)
  cont_max=cont_int(2)
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))
  form_typec=trim(form_type(1))
  form_types=trim(form_type(2))

  if(present(mono))then
     if(present(mono_val).and.present(mono_lev))then
        if(size(mono_val)-1==size(mono_lev))then
           monoto=mono
        else
           write(*,*) "*** ERROR ***"
           write(*,*) "[array number] : mono_val = mono_lev + 1"
           write(*,*) "STOP"
           stop
        end if
     else
        write(*,*) "*** ERROR ***"
        write(*,*) "When option MONO is true, MONO_VAL and MONO_LEV must be specified."
        write(*,*) "STOP"
        stop
     end if
  else
     monoto=.false.
  end if

!-- 処理ここまで ---

  call undef_CReSS2Dcl( nx, ny, 1, contour)
  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  call check_azimuth_maxmin( y, min_y, max_y )
  call DclSetWindow( x(1), x(nx), min_y, max_y )
!ORG  call DclSetWindow( x(1), x(nx), y(1), y(ny) )

  if(present(rg))then
     do i=1,size(rg,2)
        call DclScalingPoint( rg(:,i), tg(:,i) )
     end do
  end if

  if(present(rp))then
     do i=1,size(rp,2)
        call DclScalingPoint( rp(:,i), tp(:,i) )
     end do
  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  call SGSSIM( 0.5*(vx_max-vx_min)/x(nx), 0.0, 0.0 )
  call DclSetTransNumber(5)  ! 極座標変換
  call DclSetTransFunction

  if(monoto.eqv..true.)then
     call SGLSET( 'LSOFTF', .true. )
     call DclClearShadeLevel
     call DclSetShadeLevel( mono_val, mono_lev )
  else
     call SGLSET( 'LSOFTF', .false. )
!     call DclClearShadeLevel
!     call DclSetShadeLevel( shade_min, shade_max,   &
! &                          (shade_max-shade_min)/s_num )
  end if

!  if(present(nongrid))then
!     if(nongrid(1:1)=='o')then
        call DclSetXGrid( x )
!     end if
!     if(nongrid(2:2)=='o')then
        call DclSetYgrid( y )
!     end if
!  end if

  if(monoto.eqv..true.)then
     call DclShadeContour( shade )
  else
     if(present(tonbf))then
        select case (tonbf(1:1))
        case ('b')
           call UETONB( shade, nx, nx, ny )
        case ('c')
           call UETONC( shade, nx, nx, ny )
        case ('e')
           call UETONE( shade, nx, nx, ny )
        case ('f')
           call DclShadeContourEx( shade )
        end select
     else
        call DclShadeContourEx( shade )
     end if
  end if

  if(DclGetContourLevelNumber()==0)then
     call DclSetContourLabelFormat(trim(form_typec))
     call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
  end if

  call DclDrawContour( contour )

  if(present(rg))then
     do i=1,size(rg,2)
        if(present(r_idx))then
           call DclSetLineIndex( r_idx(i) )  ! この設定, 後まで引きずるかも
        end if
        if(present(r_typ))then
           call DclSetLineType( r_typ(i) )
        end if
        call DclDrawLine( rg(:,i), tg(:,i) )
     end do
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )

  if(present(rp))then
     do i=1,size(rp,2)
        if(present(rp_idx))then
           call DclSetMarkerIndex( rp_idx(i) )  ! この設定, 後まで引きずるかも
        end if
        if(present(rp_typ))then
           call DclSetMarkerType( rp_typ(i) )
        end if
        if(present(rp_siz))then
           call DclSetMarkerSize( rp_siz(i) )
        end if
        call DclDrawMarker( rp(:,i), tp(:,i) )
     end do
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

!-- 以上で極座標描画終了
!-- 以下, デカルト系で再変換
!ORG  CALL GRFIG
  call DclNewFig
  call DclSetWindow( -x(nx), x(nx), -x(nx), x(nx) )

  if(present(xg))then
     if(size(xg,1)>1)then
        do i=1,size(xg,2)
           call DclScalingPoint( xg(:,i), yg(:,i) )
        end do
     end if
  end if

  if(present(xp))then
     do i=1,size(xp,2)
        call DclScalingPoint( xp(:,i), yp(:,i) )
     end do
  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  call DclSetTransNumber(1)  ! デカルト座標変換
  call DclSetTransFunction

  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )
  if(mxitv_flag.eqv..true.)then  ! automatic label format less than 1.0
     call auto_label_fmt( mxitv(1), x(nx), fmt_mxitv, -x(nx) )  ! x = radius
     CALL USCSET( 'CXFMT', trim(adjustl(fmt_mxitv) ))
  end if
  if(myitv_flag.eqv..true.)then  ! automatic label format less than 1.0
     call auto_label_fmt( myitv(1), x(nx), fmt_myitv, -x(nx) )  ! x = radius
     CALL USCSET( 'CYFMT', trim(adjustl(fmt_myitv) ))
  end if
  if((mxitv_flag.eqv..true.).and.(myitv_flag.eqv..true.))then
     call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
     call DclDrawAxis( 'rl', myitv(1), myitv(2) )
  else
     if(mxitv_flag.eqv..true.)then
        call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
        call DclDrawScaledAxis( 'rl' )
     else if(myitv_flag.eqv..true.)then
        call DclDrawAxis( 'rl', myitv(1), myitv(2) )
        call DclDrawScaledAxis( 'bt' )
     else
        call DclDrawScaledAxis
     end if
  end if
  call DclDrawTitle( 'b', trim(x_title), 0.0 )
  call DclDrawTitle( 'l', trim(y_title), 0.0 )
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

  if(present(xg))then
     if(size(xg,1)>1)then
        do i=1,size(xg,2)
           if(present(l_idx))then
              call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
           end if
           if(present(l_typ))then
              call DclSetLineType( l_typ(i) )
           end if
           call DclDrawLine( xg(:,i), yg(:,i) )
        end do
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )

  if(present(xp))then
     do i=1,size(xp,2)
        if(present(p_idx))then
           call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
        end if
        if(present(p_typ))then
           call DclSetMarkerType( p_typ(i) )
        end if
        if(present(p_siz))then
           call DclSetMarkerSize( p_siz(i) )
        end if
        call DclDrawMarker( xp(:,i), yp(:,i) )
     end do
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(no_tone_flag.eqv..false.)then
     if(present(trigleg))then
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto,  &
  &                  trigle=trigleg )
     else
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto )
     end if
  end if

end subroutine Dcl_2D_cont_shade_polar

!---------------------------------------------------------

!---------------------------------------------------------

subroutine Dcl_2D_cont_shade_polar_vec( outname,  &
  &  x, y, contour, shade, vecx, vecy, vn, cont_int, shade_int,  &
  &  axis_title, form_type, viewx_int, viewy_int, c_num,  &
!  &  viewy_min, viewy_max, color_num, cont_num, nongrid,  &
  &  xg, yg, rg, tg, xp, yp, rp, tp, mxitv, myitv,  &
  &  mono, mono_val, mono_lev, trigleg,  &
  &  unitv, vfact, unit_fact_sign, &
  &  unit_fact, unit_title, unit_posi, no_tone, no_frame,  &
  &  l_idx, l_typ, r_idx, r_typ, p_idx, p_typ, p_siz,  &
  &  rp_idx, rp_typ, rp_siz, tonbf )
  ! 2 次元で 2 変数を等値線とカラーシェードで描画する.
  use dcl
  implicit none
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! 動径方向の格子点座標
  real, intent(in) :: y(:)  ! 同位角方向の格子点座標 [degree]
  real, intent(inout) :: contour(size(x),size(y))  ! 等値線に描く配列
  real, intent(inout) :: shade(size(x),size(y))  ! カラーシェードに描く配列
  real, intent(inout) :: vecx(size(x),size(y))  ! x 方向のベクトル
  real, intent(inout) :: vecy(size(x),size(y))  ! x 方向のベクトル
  integer, intent(in) :: vn(2)  ! ベクトル格子点 (間引き使用)
                                ! vn(1)=vnx, vn(2)=vny
  real, intent(in) :: cont_int(2)  ! 等値線の上下端 
                                 ! [cont_int(1)=cont_min, cont_int(2)=cont_max]
  real, intent(in) :: shade_int(2)  ! 等値線の上下端 [shade_int(1)=shade_min,
                                 ! shade_int(2)=shade_max]
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                                 ! axis_title(1)=x_title, axis_title(2)=y_title
  character(*), intent(in) :: form_type(2)  ! フォーマット
                           ! form_type(1)=form_typec, form_type(2)=form_types
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  integer, intent(in), optional :: c_num(2)  ! コンター・シェードの数
                           ! c_num(1)=cont_num, c_num(2)=color_num
!  character(2), intent(in), optional :: nongrid  ! 不等間隔格子にするか.
                                        ! nongrid = 'ox' で判断.
                                        ! 1 文字目が横軸, 2 文字目が縦軸.
                                        ! o = 不等間隔, x = 等間隔.
                                        ! デフォルトでは 'xx'.
  real, intent(in), optional :: xg(:,:)  ! x 軸に入れるグリッド線の座標
  real, intent(in), optional :: yg(:,:)  ! y 軸に入れるグリッド線の座標
                    ! 第一要素が線の位置データで, 複数本描く場合は,
                    ! 第二要素を 2 個以上にして描く.
                    ! 配列に入れるデータ次第で直線ではなく, 曲線グリッドを
                    ! 描くことも可能.
                    ! 3 本の線を描く場合は, xg(:,1) と yg(:,1) で 1 本の
                    ! 線を表すように指定すること.
  real, intent(in), optional :: rg(:,:)  ! 極座標系における r 軸に入れるグリッド線の座標
  real, intent(in), optional :: tg(:,:)  ! 極座標系における theta 軸に入れるグリッド線の座標
                    ! これらのデータの与え方は xg, yg と同様.
  real, intent(in), optional :: xp(:,:)  ! x 方向にマーカーを入れる x 座標
  real, intent(in), optional :: yp(:,:)  ! y 方向にマーカーを入れる y 座標
  real, intent(in), optional :: rp(:,:)  ! r 方向にマーカーを入れる r 座標
  real, intent(in), optional :: tp(:,:)  ! t 方向にマーカーを入れる t 座標
  real, intent(in), optional :: mxitv(2)   ! 横軸の目盛り間隔 (mxitv(1):大目盛り)
  real, intent(in), optional :: myitv(2)   ! 横軸の目盛り間隔 (myitv(1):大目盛り)
  logical, intent(in), optional :: mono  ! モノトーンの階調にする [.true.]
                                         ! デフォルトは .false.
  real, intent(in), optional :: mono_val(:)  ! 階調の境界値.
                    ! mono=.true. のときに必ず設定しないとエラーを返す.
                    ! 値は mono_lev + 1 成分存在しなければならない.
  integer, intent(in), optional :: mono_lev(:)  ! トーンマップ番号. dcl の 3 桁
                    ! mono=.true. のときに設定しないとエラーを返す.
  character(1), intent(in), optional :: trigleg  ! トーンバーの三角形オプション.
                ! オプションの値は, tone_bar ルーチンの trigle と同じ.
  logical, intent(in), optional :: unitv  ! 単位ベクトルを描くかどうか. default = .true.
  real, intent(in), optional :: vfact(2)  ! x,y 方向のスケーリングファクター
                    ! この値を指定すると, 内部的に決められないので, ベクトルが格子以上に
                    ! 伸びる可能性がある. 
                    ! 設定しない場合は, x, y の水平スケールと V 系のアスペクト比を考慮
                    ! して, vfact と一致させるようにする.
  logical, intent(in), optional :: unit_fact_sign  ! unitv = .true. のとき,
                    ! .true. = u, v の U 座標系での値を unit_fact に与えると, 
                    ! unit_fact はその値を単位ベクトルの単位として表示する.
                    ! unit の V 座標系の値は u, v の大きい方を 0.1 として表示する.
  real, intent(in), optional :: unit_fact(2)  ! x,y の単位ベクトルの v 座標系での長さ
                                              ! default = (0.1,0.1)
  character(*), intent(in), optional :: unit_title(2)  ! x,y の単位ベクトルのタイトル
                    ! default = 描かない.
  real, intent(in), optional :: unit_posi(2)  ! 単位ベクトルを描き始める原点座標 (V 系)
                    ! default = カラーバーの左端と同じで, 図の右下端から開始.
                    ! カラーバーはこれにぶつからないように自動的に短くする.
  logical, intent(in), optional :: no_tone  ! トーンバーを作成しないオプション
                                   ! .false. = 作成する. .true. = 作成しない.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(:)  ! xg, yg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: l_typ(:)  ! xg, yg で描く線のタイプ
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: r_idx(:)  ! rg, tg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: r_typ(:)  ! rg, tg で描く線のタイプ
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: p_idx(:)  ! xp, yp で描くマーカーのインデックス
  integer, intent(in), optional :: p_typ(:)  ! xp, yp で描くマーカーのタイプ
  real, intent(in), optional :: p_siz(:)  ! xp, yp で描くマーカーの大きさ
  integer, intent(in), optional :: rp_idx(:)  ! rp, tp で描くマーカーのインデックス
  integer, intent(in), optional :: rp_typ(:)  ! rp, tp で描くマーカーのタイプ
  real, intent(in), optional :: rp_siz(:)  ! rp, tp で描くマーカーの大きさ
  character(1), intent(in), optional :: tonbf  ! UETONE FLAG (a,b,e,f,..)
                                          ! default 'f' = UETONF
!-- 以上, 引数
  real, parameter :: pi=3.14159265
  integer :: vnx  ! x 方向のベクトル格子点 (間引き使用)
  integer :: vny  ! y 方向のベクトル格子点 (間引き使用)
  real :: cont_min  ! 等値線を描く最小値
  real :: cont_max  ! 等値線を描く最大値
  real :: shade_min  ! シェードを描く最小値
  real :: shade_max  ! シェードを描く最大値
  real :: min_y, max_y  ! 方位角の最大値 (-180 <= y <= 180 で)
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  character(10) :: form_typec  ! contour 用のフォーマット
  character(10) :: form_types  ! shade 用のフォーマット
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
  integer :: cont_num  ! 等値線の数
  integer :: color_num  ! カラーの数
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k, ii, jj  ! 作業用添字
  integer :: nx, ny
  real :: factx, facty
  real, dimension(size(x),size(y)) :: um, vm  ! ベクトル間引き後の値を代入
  real :: vvx_min, vvx_max, vvy_min, vvy_max
  real :: unitvp(2), unitvl(2), unit_auto_fact(2)
  real :: undef, RMISS, vcoef, dvecx, dvecy
  intrinsic :: nint
  character(20) :: fmt_mxitv, fmt_myitv
  logical :: monoto, unitvs, no_tone_flag, no_frame_flag
  logical :: mxitv_flag, myitv_flag

  nx=size(x)
  ny=size(y)

  CALL GLRGET( 'RMISS', RMISS )

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(c_num))then
     cont_num=c_num(1)
     color_num=c_num(2)
  else
     cont_num=10
     color_num=56
  end if

  if(present(no_tone))then
     no_tone_flag=no_tone
  else
     no_tone_flag=.false.
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  mxitv_flag=.false.
  myitv_flag=.false.
  if(present(mxitv))then
     if(mxitv(1)/=0.0.and.mxitv(2)/=0.0)then
        mxitv_flag=.true.
     end if
  end if
  if(present(myitv))then
     if(myitv(1)/=0.0.and.myitv(2)/=0.0)then
        myitv_flag=.true.
     end if
  end if

!-- 引数の置き換え用変数に置き換え
  vnx=vn(1)
  vny=vn(2)
  cont_min=cont_int(1)
  cont_max=cont_int(2)
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))
  form_typec=trim(form_type(1))
  form_types=trim(form_type(2))

!-- エラー処理
  if(nx<vnx.or.ny<vny)then
     write(*,*) "*****ERROR***** : vnx > nx or vny > ny."
     stop
  end if
  if(nx<2.or.vnx<2.or.ny<2.or.vny<2)then
     write(*,*) "*****ERROR***** : nx or ny or vnx or vny is less than 2."
     stop
  end if

!-- 警告
  if(mod((nx-1),(vnx-1))/=0.and.mod((ny-1),(vny-1))/=0)then
     write(*,*) "****WARNING**** : vnx or vny is not the factor of nx and ny."
  else
     if(mod((nx-1),(vnx-1))/=0.or.mod((ny-1),(vny-1))/=0)then
        if(mod((nx-1),(vnx-1))/=0)then
           write(*,*) "****WARNING**** : vnx is not the factor of nx."
        else
           write(*,*) "****WARNING**** : vny is not the factor of ny."
        end if
     end if
  end if

  call undef_CReSS2Dcl( nx, ny, 1, vecx)
  call undef_CReSS2Dcl( nx, ny, 1, vecy)

!-- ベクトル場の間引き
  factx=real(nx-1)/real(vnx-1)
  facty=real(ny-1)/real(vny-1)

  um=0.0
  vm=0.0

!-- 起点を 1 から始める
  do j=1,vny
     do i=1,vnx
        um(1+nint(factx*(i-1)),1+nint(facty*(j-1)))  &
  &     =vecx(1+nint(factx*(i-1)),1+nint(facty*(j-1)))
        vm(1+nint(factx*(i-1)),1+nint(facty*(j-1)))  &
  &     =vecy(1+nint(factx*(i-1)),1+nint(facty*(j-1)))
     end do
  end do

  if(present(mono))then
     if(present(mono_val).and.present(mono_lev))then
        if(size(mono_val)-1==size(mono_lev))then
           monoto=mono
        else
           write(*,*) "*** ERROR ***"
           write(*,*) "[array number] : mono_val = mono_lev + 1"
           write(*,*) "STOP"
           stop
        end if
     else
        write(*,*) "*** ERROR ***"
        write(*,*) "When option MONO is true, MONO_VAL and MONO_LEV must be specified."
        write(*,*) "STOP"
        stop
     end if
  else
     monoto=.false.
  end if

!-- 処理ここまで ---

  call undef_CReSS2Dcl( nx, ny, 1, contour)
  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  call check_azimuth_maxmin( y, min_y, max_y )
  call DclSetWindow( x(1), x(nx), min_y, max_y )
!ORG  call DclSetWindow( x(1), x(nx), y(1), y(ny) )

  if(present(rg))then
     do i=1,size(rg,2)
        call DclScalingPoint( rg(:,i), tg(:,i) )
     end do
  end if

  if(present(rp))then
     do i=1,size(rp,2)
        call DclScalingPoint( rp(:,i), tp(:,i) )
     end do
  end if

!-- ベクトルスケールについての設定
  if(present(unit_fact_sign))then
     if(unit_fact_sign.eqv..true.)then
        if(present(unit_fact))then
           unit_auto_fact(1)=unit_fact(1)
           unit_auto_fact(2)=unit_fact(2)
        else
           write(*,*) "### ERROR ### : unit_fact_sign is .true. then,"
           write(*,*) "                unit_fact must configure."
           write(*,*) "STOP."
           stop
        end if
     else
        unit_auto_fact(1)=1.0
        unit_auto_fact(2)=1.0
     end if
  else
     unit_auto_fact(1)=1.0
     unit_auto_fact(2)=1.0
  end if

  if(present(vfact))then
     call DclSetParm( 'VECTOR:LNRMAL', .false. )
     call DclSetParm( 'VECTOR:XFACT1', vfact(1) )
     call DclSetParm( 'VECTOR:YFACT1', vfact(2) )
     unit_auto_fact(1)=unit_auto_fact(1)*vfact(1)
     unit_auto_fact(2)=unit_auto_fact(2)*vfact(2)
  else
     call DclSetParm( 'VECTOR:LNRMAL', .true.)
     call DclSetParm( 'VECTOR:XFACT1', unitvl(1) )
     call DclSetParm( 'VECTOR:YFACT1', unitvl(2) )
     unit_auto_fact(1)=unit_auto_fact(1)*unitvl(1)
     unit_auto_fact(2)=unit_auto_fact(2)*unitvl(2)
     unitvl=0.0
  end if

!-- ユニットベクトルについての設定
  if(present(unitv))then
     unitvs=unitv
  else
     unitvs=.true.
  end if

!  if(unitvs.eqv..true.)then

!     call DclSetParm( 'VECTOR:LUNIT', unitvs )

!     !-- 単位ベクトルの長さ
!     !   極座標ではベクトル描画ルーチン不使用のため以下は機能しない.
!     !   本ルーチンの末尾に移動
!     if(present(unit_fact))then
!        if(present(unit_fact_sign))then
!           if(unit_fact_sign.eqv..true.)then
!              unitvl(:)=unit_auto_fact(:)
!           else
!              unitvl(:)=unit_fact(:)
!           end if
!        else
!           unitvl(:)=unit_fact(:)
!        end if
!     else
!        unitvl=(/0.1, 0.1/)
!     end if
!
!     !-- 単位ベクトルの書き始めの位置
!     if(present(unit_posi))then
!        vvx_min=unit_posi(1)
!        vvy_min=unit_posi(2)
!     else
!        vvx_min=vx_max+0.05
!        vvy_min=vy_min
!     end if
!
!     vvy_max=vvy_min+unitvl(2)+0.05
!
!     call DclSetParm( 'VECTOR:VXUNIT', unitvl(1) )
!     call DclSetParm( 'VECTOR:VYUNIT', unitvl(2) )
!     call DclSetParm( 'VECTOR:VXULOC', vvx_min )
!     call DclSetParm( 'VECTOR:VYULOC', vvy_min )
!
!     !-- タイトルを書くかどうか
!     if(present(unit_title))then
!        if(len_trim(unit_title(2))>0)then
!           call DclSetUnitVectorTitle( 'Y', trim(unit_title(2)) )
!        end if
!        if(len_trim(unit_title(1))>0)then
!           call DclSetUnitVectorTitle( 'X', trim(unit_title(1)) )
!        end if
!write(*,*) "check title", trim(adjustl(unit_title(1))), trim(adjustl(unit_title(2)))
!        call DclSetParm( 'VECTOR:LUMSG', .false. )
!     else  ! タイトルを書かないなら, グラフの下部にスケーリングファクターを明記
!        call DclSetParm( 'VECTOR:LUMSG', .true. )
!     end if
!
!  else
!     call DclSetParm( 'VECTOR:LUNIT', unitvs )
!     vvx_min=0.0
!     vvx_max=0.0
!     vvy_min=0.0
!     vvy_max=vy_min
!  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  call SGSSIM( 0.5*(vx_max-vx_min)/x(nx), 0.0, 0.0 )
  call DclSetTransNumber(5)  ! 極座標変換
  call DclSetTransFunction

  if(monoto.eqv..true.)then
     call SGLSET( 'LSOFTF', .true. )
     call DclClearShadeLevel
     call DclSetShadeLevel( mono_val, mono_lev )
  else
     call SGLSET( 'LSOFTF', .false. )
!     call DclClearShadeLevel
!     call DclSetShadeLevel( shade_min, shade_max,   &
! &                          (shade_max-shade_min)/s_num )
  end if

!  if(present(nongrid))then
!     if(nongrid(1:1)=='o')then
        call DclSetXGrid( x )
!     end if
!     if(nongrid(2:2)=='o')then
        call DclSetYgrid( y )
!     end if
!  end if

  if(monoto.eqv..true.)then
     call DclShadeContour( shade )
  else
     if(present(tonbf))then
        select case (tonbf(1:1))
        case ('b')
           call UETONB( shade, nx, nx, ny )
        case ('c')
           call UETONC( shade, nx, nx, ny )
        case ('e')
           call UETONE( shade, nx, nx, ny )
        case ('f')
           call DclShadeContourEx( shade )
        end select
     else
        call DclShadeContourEx( shade )
     end if
  end if

  if(DclGetContourLevelNumber()==0)then
     call DclSetContourLabelFormat(trim(form_typec))
     call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
  end if

  call DclDrawContour( contour )

  call DclSetParm( "GRAPH:LCLIP", .false. )

!-- 極座標では DrawVectors が機能しないので, SGLAZU で 1 点ずつ描画
!  call DclDrawVectors( um, vm )

  if(present(vfact))then  ! 現在 vfact ありの場合しかベクトル描画をサポートしていない
     vcoef=2.0*x(nx)/(vx_max-vx_min)

     do j=1,vny
        jj=1+nint(facty*(j-1))
        do i=1,vnx
           ii=1+nint(factx*(i-1))
           if(x(ii)/=0.0)then
              if(um(ii,jj)/=RMISS.and.vm(ii,jj)/=RMISS)then
!write(*,*) "check", x(ii), y(jj), um(ii,jj)*vfact(1)*vcoef, vm(ii,jj)*vfact(2)*vcoef*(180.0/pi)/x(ii), ii, jj
                 dvecx=um(ii,jj)*vfact(1)*vcoef
                 dvecy=vm(ii,jj)*vfact(2)*vcoef*(180.0/pi)/x(ii)
!(ORG)                 call SGLAZU( x(ii), y(jj), x(ii)+dvecx, y(jj)+dvecy, 1, 3 )
! 矢印の中点をプロット点に一致させる (始点をプロット点にしない)
                 call SGLAZU( x(ii)-0.5*dvecx, y(jj)-0.5*dvecy,  &
  &                           x(ii)+0.5*dvecx, y(jj)+0.5*dvecy, 1, 3 )
                 !-- 速度のスケーリング
                 !-- オリジナル: um, vm
                 !-- V 座標系: um * vfact(1), vm * vfact(2) * rmax/r
                 !-- U 座標系 (r-theta) : um * vfact(1) * vcoef, 
                 !--                      vm * vfact(2) * vcoef * (r2d) / r
                 !-- rmax = x(nx), vcoef = 2 * rmax / (vxmax-vxmin)
              end if
           end if
        end do
     end do
  end if

  if(present(rg))then
     do i=1,size(rg,2)
        if(present(r_idx))then
           call DclSetLineIndex( r_idx(i) )  ! この設定, 後まで引きずるかも
        end if
        if(present(r_typ))then
           call DclSetLineType( r_typ(i) )
        end if
        call DclDrawLine( rg(:,i), tg(:,i) )
     end do
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )

  if(present(rp))then
     do i=1,size(rp,2)
        if(present(rp_idx))then
           call DclSetMarkerIndex( rp_idx(i) )  ! この設定, 後まで引きずるかも
        end if
        if(present(rp_typ))then
           call DclSetMarkerType( rp_typ(i) )
        end if
        if(present(rp_siz))then
           call DclSetMarkerSize( rp_siz(i) )
        end if
        call DclDrawMarker( rp(:,i), tp(:,i) )
     end do
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

!-- 以上で極座標描画終了
!-- 以下, デカルト系で再変換
!ORG  CALL GRFIG
  call DclNewFig
  call DclSetWindow( -x(nx), x(nx), -x(nx), x(nx) )

  if(present(xg))then
     if(size(xg,1)>1)then
        do i=1,size(xg,2)
           call DclScalingPoint( xg(:,i), yg(:,i) )
        end do
     end if
  end if

  if(present(xp))then
     do i=1,size(xp,2)
        call DclScalingPoint( xp(:,i), yp(:,i) )
     end do
  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  call DclSetTransNumber(1)  ! デカルト座標変換
  call DclSetTransFunction

  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )
  if(mxitv_flag.eqv..true.)then  ! automatic label format less than 1.0
     call auto_label_fmt( mxitv(1), x(nx), fmt_mxitv, -x(nx) )
     CALL USCSET( 'CXFMT', trim(adjustl(fmt_mxitv) ))
  end if
  if(myitv_flag.eqv..true.)then  ! automatic label format less than 1.0
     call auto_label_fmt( myitv(1), x(nx), fmt_myitv, -x(nx) )
     CALL USCSET( 'CYFMT', trim(adjustl(fmt_myitv) ))
  end if
  if((mxitv_flag.eqv..true.).and.(myitv_flag.eqv..true.))then
     call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
     call DclDrawAxis( 'rl', myitv(1), myitv(2) )
  else
     if(mxitv_flag.eqv..true.)then
        call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
        call DclDrawScaledAxis( 'rl' )
     else if(myitv_flag.eqv..true.)then
        call DclDrawAxis( 'rl', myitv(1), myitv(2) )
        call DclDrawScaledAxis( 'bt' )
     else
        call DclDrawScaledAxis
     end if
  end if
  call DclDrawTitle( 'b', trim(x_title), 0.0 )
  call DclDrawTitle( 'l', trim(y_title), 0.0 )
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

  if(present(xg))then
     if(size(xg,1)>1)then
        do i=1,size(xg,2)
           if(present(l_idx))then
              call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
           end if
           if(present(l_typ))then
              call DclSetLineType( l_typ(i) )
           end if
           call DclDrawLine( xg(:,i), yg(:,i) )
        end do
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )

  if(present(xp))then
     do i=1,size(xp,2)
        if(present(p_idx))then
           call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
        end if
        if(present(p_typ))then
           call DclSetMarkerType( p_typ(i) )
        end if
        if(present(p_siz))then
           call DclSetMarkerSize( p_siz(i) )
        end if
        call DclDrawMarker( xp(:,i), yp(:,i) )
     end do
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(no_tone_flag.eqv..false.)then
     if(present(trigleg))then
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto,  &
  &                  trigle=trigleg )
     else
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto )
     end if
  end if

  !-- 単位ベクトル描画再び
  if(unitvs.eqv..true.)then

     call DclSetParm( "GRAPH:LCLIP", .false. )

     !-- 単位ベクトルの長さ
     if(present(unit_fact))then
        if(present(unit_fact_sign))then
           if(unit_fact_sign.eqv..true.)then
              unitvl(:)=unit_auto_fact(:)
           else
              unitvl(:)=unit_fact(:)
           end if
        else
           unitvl(:)=unit_fact(:)
        end if
     else
        unitvl=(/0.1, 0.1/)
     end if

     !-- 単位ベクトルの書き始めの位置
     if(present(unit_posi))then
        vvx_min=unit_posi(1)
        vvy_min=unit_posi(2)
     else
        vvx_min=vx_max+0.05
        vvy_min=vy_min
     end if

     vvx_max=vvx_min+unitvl(1)
     vvy_max=vvy_min

     call SZLAOP( 1, 3 )
write(*,*) "checkk", vvx_min, vvy_min, vvx_max, vvy_max
     call SZLAZV( vvx_min, vvy_min, vvx_max, vvy_max )
     call SZLACL

     !-- タイトルを書くかどうか
     if(present(unit_title))then
        if(len_trim(unit_title(1))>0)then
           call DclDrawTextNormalized( 0.5*(vvx_min+vvx_max),  &
  &                                    vvy_min-0.05, trim(adjustl(unit_title(1))),  &
  &                                    height=0.95*DclGetTextHeight(),  &
  &                                    index=13, centering=0 )
        else
           if(len_trim(unit_title(2))>0)then
              call DclDrawTextNormalized( 0.5*(vvx_min+vvx_max),  &
  &                                       vvy_min-0.05, trim(adjustl(unit_title(2))),  &
  &                                       height=0.95*DclGetTextHeight(),  &
  &                                       index=13, centering=0 )
           end if
        end if
        call DclSetParm( 'VECTOR:LUMSG', .false. )
     else  ! タイトルを書かないなら, グラフの下部にスケーリングファクターを明記
        call DclSetParm( 'VECTOR:LUMSG', .true. )
     end if

     call DclSetParm( "GRAPH:LCLIP", .true. )

  end if

end subroutine Dcl_2D_cont_shade_polar_vec

!---------------------------------------------------------
!---------------------------------------------------------

subroutine Dcl_2D_cont_shade_polar_Map( outname,  &
  &  x, y, contour, shade, cont_int, shade_int, centp,  &
  &  axis_title, form_type, viewx_int, viewy_int, c_num,  &
!  &  viewy_min, viewy_max, color_num, cont_num, nongrid,  &
  &  xg, yg, rg, tg, xp, yp, rp, tp,  &
  &  mono, mono_val, mono_lev, trigleg,  &
  &  mlitv, mlidx, coast, border, blidx, bltyp,  &
  &  no_tone, no_frame,  &
  &  l_idx, l_typ, r_idx, r_typ, p_idx, p_typ, p_siz,  &
  &  rp_idx, rp_typ, rp_siz, tonbf )
  ! 2 次元で 2 変数を等値線とカラーシェードで描画する.
  ! 地図情報を入れ込み.
  use dcl
  implicit none
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! 動径方向の格子点座標 [m]  ! 地図投影で使用
  real, intent(in) :: y(:)  ! 同位角方向の格子点座標 [degree]
  real, intent(inout) :: contour(size(x),size(y))  ! 等値線に描く配列
  real, intent(inout) :: shade(size(x),size(y))  ! カラーシェードに描く配列
  real, intent(in) :: cont_int(2)  ! 等値線の上下端 
                                 ! [cont_int(1)=cont_min, cont_int(2)=cont_max]
  real, intent(in) :: shade_int(2)  ! 等値線の上下端 [shade_int(1)=shade_min,
                                 ! shade_int(2)=shade_max]
  real, intent(in) :: centp(2)  ! 極座標系の中心が位置する緯度経度 [degree]
                           ! centp(1) = 緯度, centp(2) = 経度
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                                 ! axis_title(1)=x_title, axis_title(2)=y_title
  character(*), intent(in) :: form_type(2)  ! フォーマット
                           ! form_type(1)=form_typec, form_type(2)=form_types
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  integer, intent(in), optional :: c_num(2)  ! コンター・シェードの数
                           ! c_num(1)=cont_num, c_num(2)=color_num
!  character(2), intent(in), optional :: nongrid  ! 不等間隔格子にするか.
                                        ! nongrid = 'ox' で判断.
                                        ! 1 文字目が横軸, 2 文字目が縦軸.
                                        ! o = 不等間隔, x = 等間隔.
                                        ! デフォルトでは 'xx'.
  real, intent(in), optional :: xg(:,:)  ! x 軸に入れるグリッド線の座標
  real, intent(in), optional :: yg(:,:)  ! y 軸に入れるグリッド線の座標
                    ! 第一要素が線の位置データで, 複数本描く場合は,
                    ! 第二要素を 2 個以上にして描く.
                    ! 配列に入れるデータ次第で直線ではなく, 曲線グリッドを
                    ! 描くことも可能.
                    ! 3 本の線を描く場合は, xg(:,1) と yg(:,1) で 1 本の
                    ! 線を表すように指定すること.
  real, intent(in), optional :: rg(:,:)  ! 極座標系における r 軸に入れるグリッド線の座標
  real, intent(in), optional :: tg(:,:)  ! 極座標系における theta 軸に入れるグリッド線の座標
                    ! これらのデータの与え方は xg, yg と同様.
  real, intent(in), optional :: xp(:,:)  ! x 方向にマーカーを入れる x 座標
  real, intent(in), optional :: yp(:,:)  ! y 方向にマーカーを入れる y 座標
  real, intent(in), optional :: rp(:,:)  ! r 方向にマーカーを入れる r 座標
  real, intent(in), optional :: tp(:,:)  ! t 方向にマーカーを入れる t 座標
  logical, intent(in), optional :: mono  ! モノトーンの階調にする [.true.]
                                         ! デフォルトは .false.
  real, intent(in), optional :: mono_val(:)  ! 階調の境界値.
                    ! mono=.true. のときに必ず設定しないとエラーを返す.
                    ! 値は mono_lev + 1 成分存在しなければならない.
  integer, intent(in), optional :: mono_lev(:)  ! トーンマップ番号. dcl の 3 桁
                    ! mono=.true. のときに設定しないとエラーを返す.
  character(1), intent(in), optional :: trigleg  ! トーンバーの三角形オプション.
                ! オプションの値は, tone_bar ルーチンの trigle と同じ.
  real, intent(in), optional :: mlitv  ! メジャーライン, 目盛の表示間隔 [degree]. デフォルトは 1 degree.
  integer, intent(in), optional :: mlidx  ! メジャーライン, 目盛のインデックス.
                                          ! デフォルトは 1.
  character(5), intent(in), optional :: coast  ! 海岸線選択引数
                                   ! ['japan'] = 日本域詳細版
                                   ! ['world'] = 全球版
                                   ! default = 'world'
  character(5), intent(in), optional :: border  ! 国, 州, 県境描画
                                   ! ['japan'] = 日本県境
                                   ! ['world'] = 世界国境
                                   ! ['state'] = 米国州境
                                   ! default = 描画しない.
  integer, intent(in), optional :: blidx  ! 海岸線, 国境のインデックス.
                                          ! デフォルトは 3.
  integer, intent(in), optional :: bltyp  ! 海岸線, 国境のタイプ.
                                          ! デフォルトは 1.
  logical, intent(in), optional :: no_tone  ! トーンバーを作成しないオプション
                                   ! .false. = 作成する. .true. = 作成しない.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(:)  ! xg, yg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: l_typ(:)  ! xg, yg で描く線のタイプ
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: r_idx(:)  ! rg, tg で描く線のインデックス
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: r_typ(:)  ! rg, tg で描く線のタイプ
                                                      ! デフォルトは 1.
  integer, intent(in), optional :: p_idx(:)  ! xp, yp で描くマーカーのインデックス
  integer, intent(in), optional :: p_typ(:)  ! xp, yp で描くマーカーのタイプ
  real, intent(in), optional :: p_siz(:)  ! xp, yp で描くマーカーの大きさ
  integer, intent(in), optional :: rp_idx(:)  ! rp, tp で描くマーカーのインデックス
  integer, intent(in), optional :: rp_typ(:)  ! rp, tp で描くマーカーのタイプ
  real, intent(in), optional :: rp_siz(:)  ! rp, tp で描くマーカーの大きさ
  character(1), intent(in), optional :: tonbf  ! UETONE FLAG (a,b,e,f,..)
                                          ! default 'f' = UETONF
!-- 以上, 引数
  real :: cont_min  ! 等値線を描く最小値
  real :: cont_max  ! 等値線を描く最大値
  real :: shade_min  ! シェードを描く最小値
  real :: shade_max  ! シェードを描く最大値
  real :: min_y, max_y  ! 方位角の最大値 (-180 <= y <= 180 で)
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  character(10) :: form_typec  ! contour 用のフォーマット
  character(10) :: form_types  ! shade 用のフォーマット
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
  integer :: cont_num  ! 等値線の数
  integer :: color_num  ! カラーの数
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k  ! 作業用添字
  integer :: nx, ny
  real :: undef, RMISS, mditv
  integer :: mdidx, bdidx, bdtyp
  real, parameter :: req=4.0e7  ! 地球半径
  real, parameter :: pi=3.14159      ! 地球半径
  real, parameter :: rcoe=pi/180.0
  real, parameter :: mcoe=2.0*pi/req
  character(20) :: coast_sel
  character(20) :: border_sel
  character(20) :: fmt_mxitv, fmt_myitv
  logical :: monoto, no_tone_flag, no_frame_flag, bord_flag, mlitv_flag
  type(map) :: mcenter, msw, mne
  type(cartesian) :: ccenter, csw, cne

  nx=size(x)
  ny=size(y)
  coast_sel=''
  border_sel=''
  bord_flag=.false.
  mlitv_flag=.false.

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(c_num))then
     cont_num=c_num(1)
     color_num=c_num(2)
  else
     cont_num=10
     color_num=56
  end if

  if(present(no_tone))then
     no_tone_flag=no_tone
  else
     no_tone_flag=.false.
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

!-- 引数の置き換え用変数に置き換え
  cont_min=cont_int(1)
  cont_max=cont_int(2)
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))
  form_typec=trim(form_type(1))
  form_types=trim(form_type(2))

  if(present(mono))then
     if(present(mono_val).and.present(mono_lev))then
        if(size(mono_val)-1==size(mono_lev))then
           monoto=mono
        else
           write(*,*) "*** ERROR ***"
           write(*,*) "[array number] : mono_val = mono_lev + 1"
           write(*,*) "STOP"
           stop
        end if
     else
        write(*,*) "*** ERROR ***"
        write(*,*) "When option MONO is true, MONO_VAL and MONO_LEV must be specified."
        write(*,*) "STOP"
        stop
     end if
  else
     monoto=.false.
  end if

  if(present(mlitv))then
     mditv=mlitv
     mlitv_flag=.true.
  else
     mditv=1.0
  end if

  if(present(mlidx))then
     mdidx=mlidx
  else
     mdidx=1
  end if

  if(present(coast))then
     coast_sel='coast_'//coast
  else
     coast_sel='coast_world'
  end if

  if(present(border))then
     select case (trim(border))
     case ('japan')
        bord_flag=.true.
        border_sel='pref_japan'
     case ('world')
        bord_flag=.true.
        border_sel='border_world'
     case ('state')
        bord_flag=.true.
        border_sel='state_usa'
     end select
  end if

  if(present(blidx))then
     bdidx=blidx
  else
     bdidx=3
  end if

  if(present(bltyp))then
     bdtyp=bltyp
  else
     bdtyp=1
  end if

!-- 処理ここまで ---

  call undef_CReSS2Dcl( nx, ny, 1, contour)
  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  call check_azimuth_maxmin( y, min_y, max_y )
  call DclSetWindow( x(1), x(nx), min_y, max_y )
!ORG  call DclSetWindow( x(1), x(nx), y(1), y(ny) )

  if(present(rg))then
     do i=1,size(rg,2)
        call DclScalingPoint( rg(:,i), tg(:,i) )
     end do
  end if

  if(present(rp))then
     do i=1,size(rp,2)
        call DclScalingPoint( rp(:,i), tp(:,i) )
     end do
  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  call SGSSIM( 0.5*(vx_max-vx_min)/x(nx), 0.0, 0.0 )
  call DclSetTransNumber(5)  ! 極座標変換
  call DclSetTransFunction

  if(monoto.eqv..true.)then
     call SGLSET( 'LSOFTF', .true. )
     call DclClearShadeLevel
     call DclSetShadeLevel( mono_val, mono_lev )
  else
     call SGLSET( 'LSOFTF', .false. )
!     call DclClearShadeLevel
!     call DclSetShadeLevel( shade_min, shade_max,   &
! &                          (shade_max-shade_min)/s_num )
  end if

!  if(present(nongrid))then
!     if(nongrid(1:1)=='o')then
        call DclSetXGrid( x )
!     end if
!     if(nongrid(2:2)=='o')then
        call DclSetYgrid( y )
!     end if
!  end if

  if(monoto.eqv..true.)then
     call DclShadeContour( shade )
  else
     if(present(tonbf))then
        select case (tonbf(1:1))
        case ('b')
           call UETONB( shade, nx, nx, ny )
        case ('c')
           call UETONC( shade, nx, nx, ny )
        case ('e')
           call UETONE( shade, nx, nx, ny )
        case ('f')
           call DclShadeContourEx( shade )
        end select
     else
        call DclShadeContourEx( shade )
     end if
  end if

  if(DclGetContourLevelNumber()==0)then
     call DclSetContourLabelFormat(trim(form_typec))
     call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/cont_num )
  end if

  call DclDrawContour( contour )

  if(present(rg))then
     do i=1,size(rg,2)
        if(present(r_idx))then
           call DclSetLineIndex( r_idx(i) )  ! この設定, 後まで引きずるかも
        end if
        if(present(r_typ))then
           call DclSetLineType( r_typ(i) )
        end if
        call DclDrawLine( rg(:,i), tg(:,i) )
     end do
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )

  if(present(rp))then
     do i=1,size(rp,2)
        if(present(rp_idx))then
           call DclSetMarkerIndex( rp_idx(i) )  ! この設定, 後まで引きずるかも
        end if
        if(present(rp_typ))then
           call DclSetMarkerType( rp_typ(i) )
        end if
        if(present(rp_siz))then
           call DclSetMarkerSize( rp_siz(i) )
        end if
        call DclDrawMarker( rp(:,i), tp(:,i) )
     end do
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

!-- 以上で極座標描画終了
!-- 以下, メルカトル系で再変換
!-- ただし, メルカトル変換の定義から 緯度, 経度は rad, 距離は赤道 4 万 km を
!-- 2 pi rad で変換しているので, 以下ではその変換を行う.
!-- 変換係数は距離については 
!-- (1) 中心点の緯度経度からデカルト系座標を出す.
  mcenter%lat=centp(1)*rcoe
  mcenter%lon=centp(2)*rcoe
  ccenter=DclMercator_F(mcenter)
!-- (2) 中心点のデカルト座標から 西に -x(nx), 南に -x(nx) の南西端と
!       東に x(nx), 北に x(nx) の北東端におけるメルカトルの緯度経度を出す.
  csw%x=ccenter%x-x(nx)*mcoe
  csw%y=ccenter%y-x(nx)*mcoe
  cne%x=ccenter%x+x(nx)*mcoe
  cne%y=ccenter%y+x(nx)*mcoe

  msw=DclMercator_B(csw)
  mne=DclMercator_B(cne)

  msw%lon=msw%lon/rcoe
  msw%lat=msw%lat/rcoe
  mne%lon=mne%lon/rcoe
  mne%lat=mne%lat/rcoe

  CALL GRFIG
  call DclSetWindow( msw%lon, mne%lon, msw%lat, mne%lat )

  if(present(xg))then
     if(size(xg,1)>1)then
       do i=1,size(xg,2)
          call DclScalingPoint( xg(:,i), yg(:,i) )
       end do
     end if
  end if

  if(present(xp))then
     do i=1,size(xp,2)
        call DclScalingPoint( xp(:,i), yp(:,i) )
     end do
  end if

  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )
  call DclSetTransNumber(11)  ! メルカトル系変換. ここは固定. 
                             ! これ以外の地図には対応しない. 面倒くさい.
  call DclFitMapParm
  call DclSetTransFunction

  call DclSetParm( "GRAPH:LCLIP", .true. )

  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )
!  call DclDrawScaledAxis
!  mditv=1.0  ! とりあえず固定.
  if(mlitv_flag.eqv..true.)then
     call auto_label_fmt( mlitv, x(nx), fmt_mxitv, x(1) )
     CALL USCSET( 'CXFMT', trim(adjustl(fmt_mxitv) ))
     call auto_label_fmt( mlitv, y(ny), fmt_myitv, y(1) )
     CALL USCSET( 'CYFMT', trim(adjustl(fmt_myitv) ))
  end if
  call DclDrawAxis( 'bt', mditv, 0.5*mditv )
  call DclDrawAxis( 'rl', mditv, 0.5*mditv )

  call DclDrawTitle( 'b', trim(x_title), 0.0 )
  call DclDrawTitle( 'l', trim(y_title), 0.0 )
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

  call DclSetParm( 'MAP:LGRIDMN', .false. )
  call DclSetParm( 'MAP:INDEXMJ', mdidx )
  call DclSetParm( 'MAP:dgridmj', mditv )
  call DclSetParm( 'MAP:INDEXBND', bdidx )
  call DclSetParm( 'MAP:INDEXOUT', bdidx )
  call DclSetParm( 'MAP:ITYPEOUT', bdtyp )

  call DclDrawMap( trim(coast_sel) )

  if(present(border))then
     if(bord_flag.eqv..true.)then
        call DclDrawMap( border_sel(1:len_trim(border_sel)) )
     else
        write(*,*) "*** MESSAGE (Dcl_2D_cont_shade_MapPro) ***"
        write(*,*) "'border' argument is invalid."
     end if
  end if
  call DclDrawGlobe()

  if(present(xg))then
     if(size(xg,1)>1)then
        do i=1,size(xg,2)
           if(present(l_idx))then
              call DclSetLineIndex( l_idx(i) )  ! この設定, 後まで引きずるかも
           end if
           if(present(l_typ))then
              call DclSetLineType( l_typ(i) )
           end if
           call DclDrawLine( xg(:,i), yg(:,i) )
        end do
     end if
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )

  if(present(xp))then
     do i=1,size(xp,2)
        if(present(p_idx))then
           call DclSetMarkerIndex( p_idx(i) )  ! この設定, 後まで引きずるかも
        end if
        if(present(p_typ))then
           call DclSetMarkerType( p_typ(i) )
        end if
        if(present(p_siz))then
           call DclSetMarkerSize( p_siz(i) )
        end if
        call DclDrawMarker( xp(:,i), yp(:,i) )
     end do
  end if

  call DclSetParm( "GRAPH:LCLIP", .true. )

  if(no_tone_flag.eqv..false.)then
     if(present(trigleg))then
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto,  &
  &                  trigle=trigleg )
     else
        call tone_bar( color_num, (/shade_min, shade_max/),  &
  &                  (/vx_max+0.05, vx_max+0.075/),   &
  &                  (/vy_min, vy_max/), trim(form_types), mono_log=monoto )
     end if
  end if

end subroutine Dcl_2D_cont_shade_polar_Map

!---------------------------------------------------------

subroutine Dcl_PL( judge, outname,  &
  &  xline, yline, xpoint, ypoint, axis_title,  &
  &  viewx_int, viewy_int, x_int, y_int, no_frame,  &
  &  xylog, l_idx, l_typ, p_idx, p_typ, p_siz,  &
  &  zline, zpoint, cl_val, cl_idx, cp_val, cp_idx, mxitv, myitv,  &
  &  axlbl, axtck, cxlbl, aylbl, aytck, cylbl )
  ! 2 次元平面内において複数の曲線, ポイントで描画する.
  ! 与える曲線とポイントはそれぞれ別個の配列で定義されており,
  ! 曲線については, x, y 座標の 2 種類, ポイントについても同様の 2 種類,
  ! さらに曲線の x, y 座標用配列は第一要素で 1 本の曲線の連続を
  ! 第二要素で曲線の本数を設定. ポイントについても同様.
  ! つまり, 例として以下のように配列を用意する.
  ! 3 本の曲線, 5 種類のポイントを描きたく, 曲線については 1 本の曲線を
  ! 描くためには 1000 個の点の連続で描かれ, ポイントについては 1 種類の
  ! ポイントを 100 個描きたいとすると,
  ! xline(1000, 3), yline(1000, 3), xpoint(100, 5), y(100, 5)
  ! として引数に読み込ませればよい.
  ! このとき, 上の引数に対応する関係は以下のとおりである.
  ! lnum = 3, pnum = 5, lstep = 1000, pstep = 100
  use dcl
  implicit none
  character(1), intent(in) :: judge  ! グラフの種類
                ! 'p' = ポイントのみ描画, 'l' = ラインのみ, 'a' = 両方描画.
                ! 片方しか描画しない場合でも, ダミー配列を読み込ませる必要あり.
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: xline(:,:)  ! 曲線群の x 座標
  real, intent(in) :: yline(size(xline,1),size(xline,2))  ! 曲線群の y 座標
  real, intent(in) :: xpoint(:,:)  ! ポイント群の x 座標
  real, intent(in) :: ypoint(size(xpoint,1),size(xpoint,2))  ! ポイント群の y 座標
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                              ! axis_title(1)=x_title,axis_title(2)=y_title
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  real, intent(in), optional :: x_int(2)  ! x 方向のグラフ両端
                           ! x_int(1)=xmin, x_int(2)=xmax
  real, intent(in), optional :: y_int(2)  ! y 方向のグラフ両端
                           ! y_int(1)=ymin, y_int(2)=ymax
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: xylog(2)  ! 対数スケールで軸を書くフラグ.
                                             ! .true. で描く, default はどちらも .false.
  integer, intent(in), optional :: l_idx(size(xline,2))  ! 各線の種類を明示的に与える.
  integer, intent(in), optional :: l_typ(size(xline,2))  ! 各線の種類を明示的に与える.
  integer, intent(in), optional :: p_idx(size(xpoint,2))  ! 各点の種類を明示的に与える.
  integer, intent(in), optional :: p_typ(size(xpoint,2))  ! 各点の種類を明示的に与える.
  real, intent(in), optional :: p_siz(size(xpoint,2))  ! 各点の種類を明示的に与える.
  real, intent(in), optional :: zline(size(xline,1),size(xline,2))
                                ! カラーラインモードの xline, yline に伴う値.
  real, intent(in), optional :: zpoint(size(xpoint,1),size(xpoint,2))
                                ! カラーラインモードの xpoint, ypoint に伴う値.
  real, intent(in), optional :: cl_val(:)  ! カラーの値 (ライン)
  integer, intent(in), optional :: cl_idx(:)  ! カラー番号 (ライン)
  real, intent(in), optional :: cp_val(:)  ! カラーの値 (マーカー)
  integer, intent(in), optional :: cp_idx(:)  ! カラー番号 (マーカー)
  real, intent(in), optional :: mxitv(2)   ! 横軸の目盛り間隔 (mxitv(1):大目盛り)
  real, intent(in), optional :: myitv(2)   ! 横軸の目盛り間隔 (myitv(1):大目盛り)
  real, intent(in), optional :: axlbl(:)  ! X 座標のラベル描画での座標値
  real, intent(in), optional :: axtck(:)  ! X 座標の小目盛での座標値
  character(*), intent(in), optional :: cxlbl(:)  ! X 座標のラベル描画
  real, intent(in), optional :: aylbl(:)  ! Y 座標のラベル描画での座標値
  real, intent(in), optional :: aytck(:)  ! Y 座標の小目盛での座標値
  character(*), intent(in), optional :: cylbl(:)  ! Y 座標のラベル描画
  
!-- 以上, 引数
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k  ! 作業用添字
  integer, parameter :: lim=890  ! ラインインデックスの最大値
  integer :: nnum, lstep, pstep, lnum, pnum
  integer :: trans_num
  character(20) :: fmt_mxitv, fmt_myitv
  logical :: no_frame_flag
  logical :: xlogf, ylogf
  logical :: mxitv_flag, myitv_flag, cxlbl_flag, cylbl_flag

  lstep=size(xline,1)
  pstep=size(xpoint,1)
  lnum=size(xline,2)
  pnum=size(xpoint,2)

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  if(present(xylog))then
     xlogf=xylog(1)
     ylogf=xylog(2)
  else
     xlogf=.false.
     ylogf=.false.
  end if

  mxitv_flag=.false.
  myitv_flag=.false.
  if(present(mxitv))then
     if(mxitv(1)/=0.0.and.mxitv(2)/=0.0)then
        mxitv_flag=.true.
     end if
  end if
  if(present(myitv))then
     if(myitv(1)/=0.0.and.myitv(2)/=0.0)then
        myitv_flag=.true.
     end if
  end if

  cxlbl_flag=.false.
  cylbl_flag=.false.
  if(present(cxlbl))then
     if(len_trim(cxlbl(1))>0)then
        cxlbl_flag=.true.
        mxitv_flag=.false.
     end if
  end if
  if(present(cylbl))then
     if(len_trim(cylbl(1))>0)then
        cylbl_flag=.true.
        myitv_flag=.false.
     end if
  end if

!-- 引数の置き換え用変数に置き換え
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))

!-- 処理ここまで ---

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  if(present(x_int).and.present(y_int))then
     call DclSetWindow( x_int(1), x_int(2), y_int(1), y_int(2) )
  else
     if(judge=='p'.or.judge=='a')then
        if(present(zpoint))then
           call color_line( 's', xpoint, ypoint, zpoint,  &
  &                         0, (/0.0, 0.0/),  &
  &                         col_val=cp_val, col_idx=cp_idx )
        else
           do j=1,pnum
              call DclScalingPoint( xpoint(:,j), ypoint(:,j) )
           end do
        end if
     end if

     if(judge=='l'.or.judge=='a')then
        if(present(zline))then
           call color_line( 's', xline, yline, zline,  &
  &                         0, (/0.0, 0.0/), col_val=cl_val, col_idx=cl_idx )
        else
           do j=1,lnum
              call DclScalingPoint( xline(:,j), yline(:,j) )
           end do
        end if
     end if
     call DclFitScalingParm
  end if
  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )

  if(xlogf.eqv..true.)then
     if(ylogf.eqv..true.)then
        trans_num=4
     else
        trans_num=3
     end if
  else
     if(ylogf.eqv..true.)then
        trans_num=2
     else
        trans_num=1
     end if
  end if

  call DclSetTransNumber(trans_num)
  call DclSetTransFunction

 ! call DclShadeContourEx( shade )
  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )
!  if(mxitv_flag.eqv..true.)then  ! automatic label format less than 1.0
!     call auto_label_fmt( mxitv(1), x(nx), fmt_mxitv )
!     CALL USCSET( 'CXFMT', trim(adjustl(fmt_mxitv) ))
!  end if
!  if(myitv_flag.eqv..true.)then  ! automatic label format less than 1.0
!     call auto_label_fmt( myitv(1), y(ny), fmt_myitv )
!     CALL USCSET( 'CYFMT', trim(adjustl(fmt_myitv) ))
!  end if
  if(cxlbl_flag.eqv..true.)then
     call DclDrawAxisSpecify( 'b', axlbl, tick_pos=axtck, label=cxlbl )
     call DclDrawAxisSpecify( 't', axlbl, tick_pos=axtck )
  else if(mxitv_flag.eqv..true.)then
     call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
  else
     call DclDrawScaledAxis( 'bt' )
  end if
  if(cylbl_flag.eqv..true.)then
     call DclDrawAxisSpecify( 'l', aylbl, tick_pos=aytck, label=cylbl )
     call DclDrawAxisSpecify( 'r', aylbl, tick_pos=aytck )
  else if(myitv_flag.eqv..true.)then
     call DclDrawAxis( 'rl', myitv(1), myitv(2) )
  else
     call DclDrawScaledAxis( 'rl' )
  end if

  call DclDrawTitle( 'b', trim(x_title), 0.0 )
  call DclDrawTitle( 'l', trim(y_title), 0.0 )
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

!-- ポイントと曲線の設定 ---
!-- num 数に応じて do ループで回すので, num 数は任意で OK.
!-- num = 1 の場合は黒色で固定
  if(present(zline))then
     call color_line( 'l', xline, yline, zline,  &
  &                   0, (/0.0, 0.0/), col_val=cl_val, col_idx=cl_idx )
  else
     if(judge=='l'.or.judge=='a')then
        do j=1,lnum
           if(present(l_idx))then
              call DclSetLineIndex( l_idx(j) )
           else
              if(lnum/=1)then
                 nnum=lim/lnum
                 call DclSetLineIndex( 100+nnum*(j-1)+1 )
              end if
           end if
           if(present(l_typ))then
              call DclSetLineType( l_typ(j) )
           end if
           call DclDrawLine( xline(:,j), yline(:,j) )
        end do
     end if
  end if

  if(present(zpoint))then
     call color_line( 'p', xpoint, ypoint, zpoint,  &
  &                   0, (/0.0, 0.0/), col_val=cp_val, col_idx=cp_idx )
  else
     if(judge=='p'.or.judge=='a')then
        do j=1,pnum
           if(present(p_idx))then
              call DclSetMarkerIndex( p_idx(j) )  ! この設定, 後まで引きずるかも
           end if
           if(present(p_typ))then
              call DclSetMarkerType( p_typ(j) )
           else
              if(pnum==1)then
                 call DclDrawMarker( xpoint(:,1), ypoint(:,1) )
                 call DclSetMarkerType( 1 )
              else
                 call DclSetMarkerType( j )
              end if
           end if
           if(present(p_siz))then
              call DclSetMarkerSize( p_siz(j) )
           end if

           call DclDrawMarker( xpoint(:,j), ypoint(:,j) )
        end do
     end if
  end if

!  call DclSetContourLabelFormat(form_typec)
!  call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/10 )
!  call DclDrawContour( contour )

!  call DclDrawVectors( um, vm )

end subroutine Dcl_PL

!---------------------------------------------------------

subroutine Dcl_PL_vec( judge, outname,  &
  &  x, y, xline, yline, xpoint, ypoint,  & 
  &  vecx, vecy, vn, axis_title,  &
  &  viewx_int, viewy_int, no_frame,  &
  &  xylog, l_idx, l_typ, p_idx, p_typ, p_siz,  &
  &  zline, zpoint, cl_val, cl_idx, cp_val, cp_idx, mxitv, myitv,  &
  &  axlbl, axtck, cxlbl, aylbl, aytck, cylbl )
  ! 2 次元平面内においてベクトルと複数の曲線, ポイントで描画する.
  ! 与える曲線とポイントはそれぞれ別個の配列で定義されており,
  ! 曲線については, x, y 座標の 2 種類, ポイントについても同様の 2 種類,
  ! さらに曲線の x, y 座標用配列は第一要素で 1 本の曲線の連続を
  ! 第二要素で曲線の本数を設定. ポイントについても同様.
  ! つまり, 例として以下のように配列を用意する.
  ! 3 本の曲線, 5 種類のポイントを描きたく, 曲線については 1 本の曲線を
  ! 描くためには 1000 個の点の連続で描かれ, ポイントについては 1 種類の
  ! ポイントを 100 個描きたいとすると,
  ! xline(1000, 3), yline(1000, 3), xpoint(100, 5), y(100, 5)
  ! として引数に読み込ませればよい.
  ! このとき, 上の引数に対応する関係は以下のとおりである.
  ! lnum = 3, pnum = 5, lstep = 1000, pstep = 100
  use dcl
  implicit none
  character(1), intent(in) :: judge  ! グラフの種類
                ! 'p' = ポイントのみ描画, 'l' = ラインのみ, 'a' = 両方描画.
                ! 片方しか描画しない場合でも, ダミー配列を読み込ませる必要あり.
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: x(:)  ! x 方向の格子点座標
  real, intent(in) :: y(:)  ! y 方向の格子点座標
  real, intent(in) :: xline(:,:)  ! 曲線群の x 座標
  real, intent(in) :: yline(size(xline,1),size(xline,2))  ! 曲線群の y 座標
  real, intent(in) :: xpoint(:,:)  ! ポイント群の x 座標
  real, intent(in) :: ypoint(size(xpoint,1),size(xpoint,2))  ! ポイント群の y 座標
  real, intent(in) :: vecx(size(x),size(y))  ! x 方向のベクトル
  real, intent(in) :: vecy(size(x),size(y))  ! x 方向のベクトル
  integer, intent(in) :: vn(2)  ! ベクトル格子点 (間引き使用)
                                ! vn(1)=vnx, vn(2)=vny
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                              ! axis_title(1)=x_title,axis_title(2)=y_title
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  logical, intent(in), optional :: xylog(2)  ! 対数スケールで軸を書くフラグ.
                                             ! .true. で描く, default はどちらも .false.
  integer, intent(in), optional :: l_idx(size(xline,2))  ! 各線の種類を明示的に与える.
  integer, intent(in), optional :: l_typ(size(xline,2))  ! 各線の種類を明示的に与える.
  integer, intent(in), optional :: p_idx(size(xpoint,2))  ! 各点の種類を明示的に与える.
  integer, intent(in), optional :: p_typ(size(xpoint,2))  ! 各点の種類を明示的に与える.
  real, intent(in), optional :: p_siz(size(xpoint,2))  ! 各点の種類を明示的に与える.
  real, intent(in), optional :: zline(size(xline,1),size(xline,2))
                                ! カラーラインモードの xpoint, ypoint に伴う値.
  real, intent(in), optional :: zpoint(size(xpoint,1),size(xpoint,2))
                                ! カラーラインモードの xpoint, ypoint に伴う値.
  real, intent(in), optional :: cl_val(:)  ! カラーの値 (ライン)
  integer, intent(in), optional :: cl_idx(:)  ! カラー番号 (ライン)
  real, intent(in), optional :: cp_val(:)  ! カラーの値 (マーカー)
  integer, intent(in), optional :: cp_idx(:)  ! カラー番号 (マーカー)
  real, intent(in), optional :: mxitv(2)   ! 横軸の目盛り間隔 (mxitv(1):大目盛り)
  real, intent(in), optional :: myitv(2)   ! 横軸の目盛り間隔 (myitv(1):大目盛り)
  real, intent(in), optional :: axlbl(:)  ! X 座標のラベル描画での座標値
  real, intent(in), optional :: axtck(:)  ! X 座標の小目盛での座標値
  character(*), intent(in), optional :: cxlbl(:)  ! X 座標のラベル描画
  real, intent(in), optional :: aylbl(:)  ! Y 座標のラベル描画での座標値
  real, intent(in), optional :: aytck(:)  ! Y 座標の小目盛での座標値
  character(*), intent(in), optional :: cylbl(:)  ! Y 座標のラベル描画
!-- 以上, 引数
  integer :: vnx  ! x 方向のベクトル格子点 (間引き使用)
  integer :: vny  ! y 方向のベクトル格子点 (間引き使用)
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k  ! 作業用添字
  integer :: nx, ny
  integer, parameter :: lim=890
  integer :: lstep, pstep, lnum, pnum, nnum
  integer :: trans_num
  real :: factx, facty
  real, dimension(size(x),size(y)) :: um, vm  ! ベクトル間引き後の値を代入
  real :: undef, RMISS
  character(20) :: fmt_mxitv, fmt_myitv
  logical :: no_frame_flag
  logical :: xlogf, ylogf
  logical :: mxitv_flag, myitv_flag, cxlbl_flag, cylbl_flag

  nx=size(x)
  ny=size(y)

  lstep=size(xline,1)
  pstep=size(xpoint,1)
  lnum=size(xline,2)
  pnum=size(xpoint,2)

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  if(present(xylog))then
     xlogf=xylog(1)
     ylogf=xylog(2)
  else
     xlogf=.false.
     ylogf=.false.
  end if

  mxitv_flag=.false.
  myitv_flag=.false.
  if(present(mxitv))then
     if(mxitv(1)/=0.0.and.mxitv(2)/=0.0)then
        mxitv_flag=.true.
     end if
  end if
  if(present(myitv))then
     if(myitv(1)/=0.0.and.myitv(2)/=0.0)then
        myitv_flag=.true.
     end if
  end if

  cxlbl_flag=.false.
  cylbl_flag=.false.
  if(present(cxlbl))then
     if(len_trim(cxlbl(1))>0)then
        cxlbl_flag=.true.
        mxitv_flag=.false.
     end if
  end if
  if(present(cylbl))then
     if(len_trim(cylbl(1))>0)then
        cylbl_flag=.true.
        myitv_flag=.false.
     end if
  end if

!-- 引数の置き換え用変数に置き換え
  vnx=vn(1)
  vny=vn(2)
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))

!-- エラー処理
  if(nx<vnx.or.ny<vny)then
     write(*,*) "*****ERROR***** : vnx > nx or vny > ny."
     stop
  end if
!-- 警告
  if(mod(nx,(vnx-1))/=0.or.mod(ny,(vny-1))/=0)then
     write(*,*) "****WARNING**** : vnx or vny is not the factor of nx or ny."
  end if

!-- ベクトル場の間引き
  factx=real(nx)/real(vnx-1)
  facty=real(ny)/real(vny-1)

  um=0.0
  vm=0.0

!-- 起点を 1 から始める
  um(1,1)=vecx(1,1)
  vm(1,1)=vecy(1,1)

  do i=2,vnx
     um(1+nint(factx*(i-1)),1)=vecx(1+nint(factx*(i-1)),1)
     vm(1+nint(factx*(i-1)),1)=vecy(1+nint(factx*(i-1)),1)
  end do

  do j=2,vny
     um(1,1+nint((j-1)*facty))=vecx(1,1+nint((j-1)*facty))
     vm(1,1+nint((j-1)*facty))=vecy(1,1+nint((j-1)*facty))
  end do

  do j=2,vny
     do i=2,vnx
        um(1+nint(factx*(i-1)),1+nint(facty*(j-1)))  &
  &     =vecx(1+nint(factx*(i-1)),1+nint(facty*(j-1)))
        vm(1+nint(factx*(i-1)),1+nint(facty*(j-1)))  &
  &     =vecy(1+nint(factx*(i-1)),1+nint(facty*(j-1)))
     end do
  end do

!-- 処理ここまで ---

!  call undef_CReSS2Dcl( nx, ny, 1, contour)
!  call undef_CReSS2Dcl( nx, ny, 1, shade)

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  call DclSetWindow( x(1), x(nx), y(1), y(ny) )
  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )

  if(xlogf.eqv..true.)then
     if(ylogf.eqv..true.)then
        trans_num=4
     else
        trans_num=3
     end if
  else
     if(ylogf.eqv..true.)then
        trans_num=2
     else
        trans_num=1
     end if
  end if

  call DclSetTransNumber(trans_num)
  call DclSetTransFunction

 ! call DclShadeContourEx( shade )
  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )
!  if(mxitv_flag.eqv..true.)then  ! automatic label format less than 1.0
!     call auto_label_fmt( mxitv(1), x(nx), fmt_mxitv )
!     CALL USCSET( 'CXFMT', trim(adjustl(fmt_mxitv) ))
!  end if
!  if(myitv_flag.eqv..true.)then  ! automatic label format less than 1.0
!     call auto_label_fmt( myitv(1), y(ny), fmt_myitv )
!     CALL USCSET( 'CYFMT', trim(adjustl(fmt_myitv) ))
!  end if
  if(cxlbl_flag.eqv..true.)then
     call DclDrawAxisSpecify( 'b', axlbl, tick_pos=axtck, label=cxlbl )
     call DclDrawAxisSpecify( 't', axlbl, tick_pos=axtck )
  else if(mxitv_flag.eqv..true.)then
     call DclDrawAxis( 'bt', mxitv(1), mxitv(2) )
  else
     call DclDrawScaledAxis( 'bt' )
  end if
  if(cylbl_flag.eqv..true.)then
     call DclDrawAxisSpecify( 'l', aylbl, tick_pos=aytck, label=cylbl )
     call DclDrawAxisSpecify( 'r', aylbl, tick_pos=aytck )
  else if(myitv_flag.eqv..true.)then
     call DclDrawAxis( 'rl', myitv(1), myitv(2) )
  else
     call DclDrawScaledAxis( 'rl' )
  end if

  call DclDrawTitle( 'b', trim(x_title), 0.0 )
  call DclDrawTitle( 'l', trim(y_title), 0.0 )
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

!-- ポイントと曲線の設定 ---
!-- num 数に応じて do ループで回すので, num 数は任意で OK.
!-- num = 1 の場合は黒色で固定
  if(present(zline))then
     call color_line( 'l', xline, yline, zline,  &
  &                   0, (/0.0, 0.0/), col_val=cl_val, col_idx=cl_idx )
  else
     if(judge=='l'.or.judge=='a')then
        do j=1,lnum
           if(present(l_idx))then
              call DclSetLineIndex( l_idx(j) )
           else
              if(lnum/=1)then
                 nnum=lim/lnum
                 call DclSetLineIndex( 100+nnum*(j-1)+1 )
              end if
           end if
           if(present(l_typ))then
              call DclSetLineType( l_typ(j) )
           end if
           call DclDrawLine( xline(:,j), yline(:,j) )
        end do
     end if
  end if

  if(present(zpoint))then
     call color_line( 'p', xpoint, ypoint, zpoint,  &
  &                   0, (/0.0, 0.0/), col_val=cp_val, col_idx=cp_idx )
  else
     if(judge=='p'.or.judge=='a')then
        do j=1,pnum
           if(present(p_idx))then
              call DclSetMarkerIndex( p_idx(j) )  ! この設定, 後まで引きずるかも
           end if
           if(present(p_typ))then
              call DclSetMarkerType( p_typ(j) )
           else
              if(pnum==1)then
                 call DclDrawMarker( xpoint(:,1), ypoint(:,1) )
                 call DclSetMarkerType( 1 )
              else
                 call DclSetMarkerType( j )
              end if
           end if
           if(present(p_siz))then
              call DclSetMarkerSize( p_siz(j) )
           end if

           call DclDrawMarker( xpoint(:,j), ypoint(:,j) )
        end do
     end if
  end if

!  call DclSetContourLabelFormat(form_typec)
!  call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/10 )
!  call DclDrawContour( contour )

  call DclDrawVectors( um, vm )

end subroutine Dcl_PL_vec

!---------------------------------------------------------

subroutine Dcl_PL_calendar( judge, outname,  &
  &  xline, yline, xpoint, ypoint, axis_title, date, days,  &
  &  viewx_int, viewy_int, x_int, y_int, no_frame,  &
  &  l_idx, l_typ, p_idx, p_typ, p_siz, zline, zpoint,  &
  &  cl_val, cl_idx, cp_val, cp_idx, date_strt, date_end, mxitv, myitv )
  ! 2 次元平面内において複数の曲線, ポイントで描画する.
  ! 与える曲線とポイントはそれぞれ別個の配列で定義されており,
  ! 曲線については, x, y 座標の 2 種類, ポイントについても同様の 2 種類,
  ! さらに曲線の x, y 座標用配列は第一要素で 1 本の曲線の連続を
  ! 第二要素で曲線の本数を設定. ポイントについても同様.
  ! つまり, 例として以下のように配列を用意する.
  ! 3 本の曲線, 5 種類のポイントを描きたく, 曲線については 1 本の曲線を
  ! 描くためには 1000 個の点の連続で描かれ, ポイントについては 1 種類の
  ! ポイントを 100 個描きたいとすると,
  ! xline(1000, 3), yline(1000, 3), xpoint(100, 5), y(100, 5)
  ! として引数に読み込ませればよい.
  ! このとき, 上の引数に対応する関係は以下のとおりである.
  ! lnum = 3, pnum = 5, lstep = 1000, pstep = 100
  use dcl
  implicit none
  character(1), intent(in) :: judge  ! グラフの種類
                ! 'p' = ポイントのみ描画, 'l' = ラインのみ, 'a' = 両方描画.
                ! 片方しか描画しない場合でも, ダミー配列を読み込ませる必要あり.
  character(*), intent(in) :: outname  ! グラフのタイトル
  real, intent(in) :: xline(:,:)  ! 曲線群の x 座標
  real, intent(in) :: yline(size(xline,1),size(xline,2))  ! 曲線群の y 座標
  real, intent(in) :: xpoint(:,:)  ! ポイント群の x 座標
  real, intent(in) :: ypoint(size(xpoint,1),size(xpoint,2))  ! ポイント群の y 座標
  character(*), intent(in) :: axis_title(2)  ! 座標軸のタイトル
                              ! axis_title(1)=x_title,axis_title(2)=y_title
  type(dcl_date), intent(in) :: date  ! 開始日付 [yyyy:mm:dd]
  integer, intent(in) :: days  ! 描画日数 [day]
  real, intent(in), optional :: viewx_int(2)  ! ビューポートの x 方向の両端
                           ! viewx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in), optional :: viewy_int(2)  ! ビューポートの y 方向の両端
                           ! viewy_int(1)=vy_min, vy_int(2)=vy_max
  real, intent(in), optional :: x_int(2)  ! x 方向のグラフ両端
                           ! x_int(1)=xmin, x_int(2)=xmax
  real, intent(in), optional :: y_int(2)  ! y 方向のグラフ両端
                           ! y_int(1)=ymin, y_int(2)=ymax
  logical, intent(in), optional :: no_frame  ! NewFrame を呼ばない
                                   ! .false. = 呼ぶ. .true. = 呼ばずに NewFig.
                                   ! デフォルトは .false.
  integer, intent(in), optional :: l_idx(size(xline,2))  ! 各線の種類を明示的に与える.
  integer, intent(in), optional :: l_typ(size(xline,2))  ! 各線の種類を明示的に与える.
  integer, intent(in), optional :: p_idx(size(xpoint,2))  ! 各点の種類を明示的に与える.
  integer, intent(in), optional :: p_typ(size(xpoint,2))  ! 各点の種類を明示的に与える.
  real, intent(in), optional :: p_siz(size(xpoint,2))  ! 各点の種類を明示的に与える.
  real, intent(in), optional :: zline(size(xline,1),size(xline,2))
                                ! カラーラインモードの xline, yline に伴う値.
  real, intent(in), optional :: zpoint(size(xpoint,1),size(xpoint,2))
                                ! カラーラインモードの xpoint, ypoint に伴う値.
  real, intent(in), optional :: cl_val(:)  ! カラーの値 (ライン)
  integer, intent(in), optional :: cl_idx(:)  ! カラー番号 (ライン)
  real, intent(in), optional :: cp_val(:)  ! カラーの値 (マーカー)
  integer, intent(in), optional :: cp_idx(:)  ! カラー番号 (マーカー)
  integer, intent(in), optional :: date_strt  ! 開始日付 [hour]
  integer, intent(in), optional :: date_end   ! 終了日付 [hour]
  real, intent(in), optional :: mxitv(2)   ! 横軸の目盛り間隔 (mxitv(1):大目盛り)
  real, intent(in), optional :: myitv(2)   ! 縦軸の目盛り間隔 (myitv(1):大目盛り)

!-- 以上, 引数
  character(100) :: x_title  ! x 軸のタイトル
  character(100) :: y_title  ! y 軸のタイトル
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
!-- 以上, 引数の置き換え用変数
  integer :: i, j, k  ! 作業用添字
  integer, parameter :: lim=890  ! ラインインデックスの最大値
  integer :: nnum
  integer :: lstep, pstep, lnum, pnum
  logical :: no_frame_flag
  logical :: mxitv_flag, myitv_flag
  type(dclatime) :: dcurtime

  lstep=size(xline,1)
  pstep=size(xpoint,1)
  lnum=size(xline,2)
  pnum=size(xpoint,2)

  dcurtime%year_d=date%year
  dcurtime%month_d=date%month
  dcurtime%day_d=date%day
  dcurtime%hour_d=0
  dcurtime%min_d=0
  dcurtime%sec_d=0

!-- optional 引数の処理 ---
  if(present(viewx_int))then
     vx_min=viewx_int(1)
     vx_max=viewx_int(2)
  else
     vx_min=0.2
     vx_max=0.8
  end if

  if(present(viewy_int))then
     vy_min=viewy_int(1)
     vy_max=viewy_int(2)
  else
     vy_min=0.2
     vy_max=0.8
  end if

  if(present(no_frame))then
     no_frame_flag=no_frame
  else
     no_frame_flag=.false.
  end if

  mxitv_flag=.false.
  if(present(mxitv))then
     if(mxitv(1)/=0.0.and.mxitv(2)/=0.0)then
        mxitv_flag=.true.
     end if
  end if

  myitv_flag=.false.
  if(present(myitv))then
     if(myitv(1)/=0.0.and.myitv(2)/=0.0)then
        myitv_flag=.true.
     end if
  end if

!-- 引数の置き換え用変数に置き換え
  x_title=trim(axis_title(1))
  y_title=trim(axis_title(2))

!-- 処理ここまで ---

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  if(no_frame_flag.eqv..true.)then
     call DclNewFig
  else
     call DclNewFrame
  end if

  if(present(x_int).and.present(y_int))then
     call DclSetWindow( x_int(1), x_int(2), y_int(1), y_int(2) )
  else
     if(judge=='p'.or.judge=='a')then
        if(present(zpoint))then
           call color_line( 's', xpoint, ypoint, zpoint,  &
  &                         0, (/0.0, 0.0/), col_val=cp_val, col_idx=cp_idx )
        else
           do i=1,pnum
              call DclScalingPoint( xpoint(:,j), ypoint(:,j) )
           end do
        end if
     end if

     if(judge=='l'.or.judge=='a')then
        if(present(zline))then
           call color_line( 's', xline, yline, zline,  &
  &                         0, (/0.0, 0.0/), col_val=cl_val, col_idx=cl_idx )
        else
           do j=1,lnum
              call DclScalingPoint( xline(:,j), yline(:,j) )
           end do
        end if
     end if
     call DclFitScalingParm
  end if
  call DclSetViewPort( vx_min, vx_max, vy_min, vy_max )

  call DclSetTransFunction

 ! call DclShadeContourEx( shade )
  CALL UZLSET( 'LABELYL', .TRUE. )
  CALL UZLSET( 'LABELYR', .FALSE. )
  if(present(date_strt))then
     CALL UZLSET( 'LABELXB', .TRUE. )
     CALL UZLSET( 'LABELXT', .FALSE. )
     call DclDrawAxisCalendarYMDH( 'b', date_strt, date_end )
     call DclDrawAxisCalendarYMDH( 't', date_strt, date_end )
  else
     call DclDrawAxisCalendar( 'bt', date, nd=days )
  end if
  if(myitv_flag.eqv..true.)then
     call DclDrawAxis( 'rl', myitv(1), myitv(2) )
  else
     call DclDrawScaledAxis( 'lr' )
  end if
  call DclDrawTitle( 'l', trim(y_title), 0.0 )
  call DclDrawTitle( 't', trim(outname), 0.0, 2 )

!-- ポイントと曲線の設定 ---
!-- num 数に応じて do ループで回すので, num 数は任意で OK.
!-- num = 1 の場合は黒色で固定
  if(present(zline))then
     call color_line( 'l', xline, yline, zline,  &
  &                   0, (/0.0, 0.0/), col_val=cl_val, col_idx=cl_idx )
  else
     if(judge=='l'.or.judge=='a')then
        do j=1,lnum
           if(present(l_idx))then
              call DclSetLineIndex( l_idx(j) )
           else
              if(lnum/=1)then
                 nnum=lim/lnum
                 call DclSetLineIndex( 100+nnum*(j-1)+1 )
              end if
           end if
           if(present(l_typ))then
              call DclSetLineType( l_typ(j) )
           end if
           call DclDrawLine( xline(:,j), yline(:,j) )
        end do
     end if
  end if

  if(present(zpoint))then
     call color_line( 'p', xpoint, ypoint, zpoint,  &
  &                   0, (/0.0, 0.0/), col_val=cp_val, col_idx=cp_idx )
  else
     if(judge=='p'.or.judge=='a')then
        do j=1,pnum
           if(present(p_idx))then
              call DclSetMarkerIndex( p_idx(j) )  ! この設定, 後まで引きずるかも
           end if
           if(present(p_typ))then
              call DclSetMarkerType( p_typ(j) )
           else
              if(pnum==1)then
                 call DclDrawMarker( xpoint(:,1), ypoint(:,1) )
                 call DclSetMarkerType( 1 )
              else
                 call DclSetMarkerType( j )
              end if
           end if
           if(present(p_siz))then
              call DclSetMarkerSize( p_siz(j) )
           end if

           call DclDrawMarker( xpoint(:,j), ypoint(:,j) )
        end do
     end if
  end if

!  call DclSetContourLabelFormat(form_typec)
!  call DclSetContourLevel( cont_min, cont_max, (cont_max-cont_min)/10 )
!  call DclDrawContour( contour )

!  call DclDrawVectors( um, vm )

end subroutine Dcl_PL_calendar

!---------------------------------------------------------

subroutine auto_title( head, time, title, forma, factor, unite )
!  時間発展する場合, 自動的にグラフのタイトルを作成する
  implicit none
  character(*), intent(in) :: head  ! タイトルヘッダ
  integer, intent(in) :: time  ! 時刻
  character(*), intent(inout) :: title  ! 生成されるタイトル
  character(6), intent(in), optional :: forma  ! オプションとしてフォーマット
  integer, intent(in), optional :: factor  ! time factor
  character(*), intent(in), optional :: unite  ! unit
  character(6) :: formb
  character(8) :: tmpname
  integer :: facttime, len_num
  real :: facttime_f

  if(present(forma))then
     formb=forma
  else
     formb='(i8.8)'
  end if

  if(present(factor))then
     if(mod(time,factor)/=0)then
        facttime_f=real(time)/real(factor)
        write(tmpname,formb) facttime_f
write(*,*) "######## facttime", tmpname, facttime_f
     else
        if(formb(2:2)=='f')then  ! フォーマットが実数で与えられている
           facttime=time/factor
           write(tmpname,formb) real(facttime)
        else
           facttime=time/factor
           write(tmpname,formb) facttime
        end if
     end if
  else

     facttime=time
write(*,*) "facttiem", facttime, time, formb
     write(tmpname,formb) time

  end if

  len_num=len_trim(tmpname)

  if(present(unite))then
     title=trim(head)//'_(t='//tmpname(1:len_num)//trim(unite)//')"'
  else
     title=trim(head)//'_(t='//tmpname(1:len_num)//'[s])"'
  end if

end subroutine auto_title

!---------------------------------------------------------

subroutine tone_bar( color_num, shade_int, vx_int, vy_int,  &
  &                  form_types, mono_log, trigle,  &
  &                  tricmin, tricmax, trifact, col_mem_num, log_flag,  &
  &                  col_spec, val_spec, dir, title, titles, titlep )
  ! トーンバーを自動生成する.
  use dcl
  implicit none
  integer, intent(in) :: color_num  ! 使用する色の数
  real, intent(in) :: shade_int(2)  ! カラーの上下端
                              ! shade_int(1)=shade_min, shade_int(2)=shade_max
  real, intent(in) :: vx_int(2)  ! ビューポートの x 方向の両端
                                 ! vx_int(1)=vx_min, vx_int(2)=vx_max
  real, intent(in) :: vy_int(2)  ! ビューポートの y 方向の両端
                                 ! vy_int(1)=vy_min, vy_int(2)=vy_max
  character(*), intent(in) :: form_types  ! ラベルフォーマット
  logical, intent(in), optional :: mono_log
  character(1), intent(in), optional :: trigle  ! grads 風な三角形を出すかどうか
                ! [u] = 上だけ, [d] = 下だけ, [a] = 両方, デフォルトでは描かない
  integer, intent(in), optional :: tricmin  ! 下端三角に描くカラーマップ番号 5 桁
  integer, intent(in), optional :: tricmax  ! 上端三角に描くカラーマップ番号 5 桁
                ! これらの色は設定されていなければ, color_setting でセットされている色を使うようにする.
  real, intent(in), optional :: trifact  ! 三角形の高さ (横辺と同じ長さを 1 としてその factor 倍する比率. デフォルトは 1.)
  integer, intent(in), optional :: col_mem_num  ! トーンバーの目盛の数
  logical, intent(in), optional :: log_flag ! スケールを対数化するか. デフォルトは .false.
  real, intent(in), optional :: col_spec(color_num+1)  ! カラーに対応する数値を陽的に指定する.
  integer, intent(in), optional :: val_spec(color_num)  ! カラーに対応するカラーマップ番号を陽的に指定する.
  character(1), intent(in), optional :: dir  ! トーンバーの向き
                                     ! 'y' = 横向き, 't' = 縦向き.
                                     ! デフォルト = 't'.
  character(*), intent(in), optional :: title  ! カラーバーに描くタイトル.
  character(1), intent(in), optional :: titles  ! タイトルを描く側.
                                     ! 't', 'b', 'r', 'l' = 上, 下, 右, 左
                                     ! デフォルトは 縦の場合は右, 横の場合は下.
  real, intent(in), optional :: titlep  ! タイトルを描く位置.
                                     ! dcl のタイトル位置の値と同じ.

!-- 以上, 引数
  real :: shade_min  ! 最小値
  real :: shade_max  ! 最大値
  real :: vx_min  ! ビューポートの x 方向の最小値
  real :: vx_max  ! ビューポートの x 方向の最大値
  real :: vy_min  ! ビューポートの y 方向の最小値
  real :: vy_max  ! ビューポートの y 方向の最大値
!-- 以上, 引数の置き換え用変数
  real, parameter :: RMISS=999.0
  integer :: k
  real :: pi(2,color_num+1), pir(color_num+1,2)
  real :: dp, dp_mem
  real :: coldim1(color_num+1), coldim2(color_num/2+1)
  real, allocatable :: col_mem_dim1(:), col_mem_dim2(:)
  logical :: monoto  ! モノトーンの処理
  real, dimension(4) :: triux, triuy, tridx, tridy
  real, dimension(5) :: recx, recy
  real :: factoru, clev1, clev2
  integer :: tricmin_num, tricmax_num
          ! 多角形領域の指定では, 三角形の頂点位置座標がわかればよいので,
          ! 各座標配列は 3 つ必要
  real :: vpx_min, vpx_max, vpy_min, vpy_max  ! 実際にとる viewport, trigle 用バッファ.
  real :: bart
  logical :: log_f
  character(1) :: direction, barp
  character(10) :: label

  call DclSetParm( "GRAPH:LCLIP", .false. )
!-- 引数の置き換え用変数に置き換え
  shade_min=shade_int(1)
  shade_max=shade_int(2)
  vx_min=vx_int(1)
  vx_max=vx_int(2)
  vy_min=vy_int(1)
  vy_max=vy_int(2)

!-- オプションの処理
  if(present(mono_log))then
     monoto=mono_log
  else
     monoto=.false.
  end if

  if(present(dir))then
     direction(1:1)=dir(1:1)
  else
     direction(1:1)='t'
  end if

  if(present(trigle))then
     if(present(trifact))then
        factoru=trifact
     else
        factoru=1.0
     end if

     if(present(tricmin))then
        tricmin_num=tricmin
     else
        CALL DclGetShadeLevel( 1, clev1, clev2, tricmin_num )
        write(*,*) "### downer color is", tricmin_num
     end if

     if(present(tricmax))then
        tricmax_num=tricmax
     else
        CALL DclGetShadeLevel( color_num+2, clev1, clev2, tricmax_num )
        write(*,*) "### upper color is", tricmax_num
     end if

     if(direction=='t')then
        select case(trigle)
        case('a')
           triux(1)=vx_min
           triux(2)=(vx_max+vx_min)*0.5
           triux(3)=vx_max
           triux(4)=triux(1)
           triuy(1)=vy_max-factoru*(vx_max-vx_min)
           triuy(2)=vy_max
           triuy(3)=triuy(1)
           triuy(4)=triuy(1)
           tridx=triux
           tridy(1)=vy_min+factoru*(vx_max-vx_min)
           tridy(2)=vy_min
           tridy(3)=tridy(1)
           tridy(4)=tridy(1)
           vpy_min=tridy(1)
           vpy_max=triuy(1)
        case('u')
           triux(1)=vx_min
           triux(2)=(vx_max+vx_min)*0.5
           triux(3)=vx_max
           triux(4)=triux(1)
           triuy(1)=vy_max-factoru*(vx_max-vx_min)
           triuy(2)=vy_max
           triuy(3)=triuy(1)
           triuy(4)=triuy(1)
           vpy_min=vy_min
           vpy_max=triuy(1)
        case('d')
           tridx(1)=vx_min
           tridx(2)=(vx_max+vx_min)*0.5
           tridx(3)=vx_max
           tridx(4)=tridx(1)
           tridy(1)=vy_min+factoru*(vx_max-vx_min)
           tridy(2)=vy_min
           tridy(3)=tridy(1)
           tridy(4)=tridy(1)
           vpy_min=tridy(1)
           vpy_max=vy_max
        case default
           vpy_min=vy_min
           vpy_max=vy_max
        end select

        vpx_min=vx_min
        vpx_max=vx_max

     else
        select case(trigle)
        case('a')
           triuy(1)=vy_min
           triuy(2)=(vy_max+vy_min)*0.5
           triuy(3)=vy_max
           triuy(4)=triuy(1)
           triux(1)=vx_max-factoru*(vy_max-vy_min)
           triux(2)=vx_max
           triux(3)=triux(1)
           triux(4)=triux(1)
           tridy=triuy
           tridx(1)=vx_min+factoru*(vy_max-vy_min)
           tridx(2)=vx_min
           tridx(3)=tridx(1)
           tridx(4)=tridx(1)
           vpx_min=tridx(1)
           vpx_max=triux(1)
        case('u')
           triuy(1)=vy_min
           triuy(2)=(vy_max+vy_min)*0.5
           triuy(3)=vy_max
           triuy(4)=triuy(1)
           triux(1)=vx_max-factoru*(vy_max-vy_min)
           triux(2)=vx_max
           triux(3)=triux(1)
           triux(4)=triux(1)
           vpx_min=vx_min
           vpx_max=triux(1)
        case('d')
           tridy(1)=vy_min
           tridy(2)=(vy_max+vy_min)*0.5
           tridy(3)=vy_max
           tridy(4)=tridy(1)
           tridx(1)=vx_min+factoru*(vy_max-vy_min)
           tridx(2)=vx_min
           tridx(3)=tridx(1)
           tridx(4)=tridx(1)
           vpx_min=tridx(1)
           vpx_max=vx_max
        case default
           vpx_min=vx_min
           vpx_max=vx_max
        end select

        vpy_min=vy_min
        vpy_max=vy_max

     end if

  else

     vpx_min=vx_min
     vpx_max=vx_max
     vpy_min=vy_min
     vpy_max=vy_max

  end if

  if(present(log_flag))then
     log_f=log_flag
  else
     log_f=.false.
  end if

  if(present(val_spec))then
     if(log_f.eqv..true.)then
        write(*,*) "*** ERROR (tone_bar_dcl_auto) ***: val_spec is present."
        write(*,*) "log_f can not be set as true. STOP"
        stop
     end if
     if(trim(adjustl(form_types))=='b')then
        write(*,*) "*** ERROR (tone_bar_dcl_auto) ***: val_spec is present."
        write(*,*) "form_types can not be set as 'b'. STOP"
        stop
     end if
  end if

  if(present(dir))then
     direction=dir(1:1)
  else
     direction='t'
  end if

  if(present(titles))then
     barp=titles(1:1)
  else
     if(direction=='t')then
        barp='l'
     else
        barp='b'
     end if
  end if

  if(present(titlep))then
     bart=titlep
  else
     bart=0.0
  end if

!-- 処理ここまで

  call UWSGXZ(.FALSE.)
  call UWSGYZ(.FALSE.)

  call DclNewFig

     call SGLSET( 'LSOFTF', .false. )

  if(present(val_spec))then  ! 1 領域ずつ四角で塗っていく

!     do k=1,color_num+1
!        PI(1,k)=val_spec(k)
!        PI(2,k)=val_spec(k)
!     end do

!     do k=1,color_num+1
!        coldim1(k)=val_spec(k)
!     end do

     call SGLSET( 'LSOFTF', .false. )

     if(direction(1:1)=='t')then  ! 縦バー
        recx(1)=vpx_min
        recx(2:3)=vpx_max
        recx(4:5)=vpx_min

     !-- なぜか  1 回目の呼び出しがうまくいかないので, 
     !-- (TextNormalized により設定されるパラメータがあるらしい)
     !-- 最初に TextNormalized を呼び出す. (ここから)
        call DclDrawTextNormalized( vpx_max+0.01, 0.0, '',  &
  &                                 height=0.95*DclGetTextHeight(),  &
  &                                 index=13, centering=-1 )
     !-- なぜか 1 回目の呼び出しがうまくいかないので重複で呼び出す. (ここまで)
        do k=1,color_num
           recy(1:2)=vpy_min+(vpy_max-vpy_min)*real(k-1)/real(color_num)
           recy(3:4)=vpy_min+(vpy_max-vpy_min)*real(k)/real(color_num)
           recy(5)=recy(1)
           write(label,trim(adjustl(form_types))) col_spec(k)
           call DclShadeRegionNormalized( recx, recy, val_spec(k) )
           call DclDrawLineNormalized( recx, recy, index=13 )
           call DclDrawTextNormalized( vpx_max+0.01, recy(1),  &
  &                                    trim(adjustl(label)),  &
  &                                    height=0.95*DclGetTextHeight(),  &
  &                                    index=13, centering=-1 )
        end do
        write(label,trim(adjustl(form_types))) col_spec(color_num+1)
        call DclDrawTextNormalized( vpx_max+0.01, vpy_max,  &
  &                                 trim(adjustl(label)),  &
  &                                 height=0.95*DclGetTextHeight(),  &
  &                                 index=13, centering=-1 )
     else if(direction(1:1)=='y')then  ! 横バー
        recy(1:2)=vpy_min
        recy(3:4)=vpy_max
        recy(5)=vpy_min
        do k=1,color_num
           recx(1)=vpx_min+(vpx_max-vpx_min)*real(k-1)/real(color_num)
           recx(2:3)=vpx_min+(vpx_max-vpx_min)*real(k)/real(color_num)
           recx(4:5)=recx(1)
           write(label,trim(adjustl(form_types))) col_spec(k)
           call DclShadeRegionNormalized( recx, recy, val_spec(k) )
           call DclDrawLineNormalized( recx, recy, index=13 )
           call DclDrawTextNormalized( recx(1), vpy_min-0.04,  &
  &                                    trim(adjustl(label)),  &
  &                                    height=0.95*DclGetTextHeight(),  &
  &                                    index=13, centering=0 )
        end do
        write(label,trim(adjustl(form_types))) col_spec(color_num+1)
        call DclDrawTextNormalized( vpx_max, vpy_min-0.04,  &
  &                                 trim(adjustl(label)),  &
  &                                 height=0.95*DclGetTextHeight(),  &
  &                                 index=13, centering=0 )
     end if

  else  ! 通常の線形・対数トーンバー

     if(direction=='t')then
        call DclSetWindow( 0.0, 1.0, shade_min, shade_max )
     else
        call DclSetWindow( shade_min, shade_max, 0.0, 1.0 )
     end if
     call DclSetViewPort( vpx_min, vpx_max, vpy_min, vpy_max )
     if(log_f.eqv..true.)then
        if(direction=='t')then
           call GRSTRN(2)  ! 縦の場合は y 軸対数
        else
           call GRSTRN(3)  ! 横の場合は x 軸対数
        end if
     !-- 配色の設定
        dp = (log10(shade_max)-log10(shade_min))/color_num
        do k=1,color_num+1
           PI(1,K) = shade_min * 10.0**(DP*(K-1))
           PI(2,K) = PI(1,K)
        end do
     else
        call GRSTRN(1)
     !-- 配色の設定
        dp = (shade_max-shade_min)/color_num
        do k=1,color_num+1
           PI(1,K) = shade_min + DP * (K-1)
           PI(2,K) = PI(1,K)
        end do
     end if

     call DclSetTransFunction

     call DclSetParm( "GRAPH:LCLIP", .false. )
     if(direction=='y')then  ! 横の場合, 配列を入れ替える
        PIr(:,1)=PI(1,:)
        PIr(:,2)=PI(2,:)
     end if

     if(direction=='t')then
        call DclSetXGrid( (/0.0,1.0/) )
        call DclSetYGrid( PI(1,:) )
     else
        call DclSetXGrid( PI(1,:) )
        call DclSetYGrid( (/0.0,1.0/) )
     end if

  ! ここから実際にカラーバーを描く
     if(monoto.eqv..true.)then
        call SGLSET( 'LSOFTF', .true. )
        if(direction=='t')then
           call DclShadeContour( PI )
        else
           call DclShadeContour( PIr )
        end if
     else
        call SGLSET( 'LSOFTF', .false. )
        if(direction=='t')then
     write(*,*) "CCCCCCCCCCCCCCCCCCCCCCCCC", PI
           call DclShadeContourEx( PI )
        else
           call DclShadeContourEx( PIr )
        end if
     end if

     CALL SLPVPR( 3 )
     CALL UZLSET( 'LABELYR', .TRUE. )
     CALL UZLSET( 'LABELYL', .FALSE. )
     CALL UYSFMT( trim(adjustl(form_types)) )

  !-- トーンの目盛を描くための配列を調整.
     if(present(col_mem_num))then

        allocate(col_mem_dim1(col_mem_num+1))
        allocate(col_mem_dim2(col_mem_num/2+1))

        if(log_f.eqv..true.)then
           dp_mem=(log10(shade_max)-log10(shade_min))/col_mem_num
           do k=1,col_mem_num+1
              col_mem_dim1(k)=shade_min*10.0**(dp_mem*(k-1))
           end do
           do k=1,col_mem_num/2+1
              col_mem_dim2(k)=shade_min*10.0**(dp_mem*(2*(k-1)))
           end do
        else
           dp_mem=(shade_max-shade_min)/col_mem_num
           do k=1,col_mem_num+1
              col_mem_dim1(k)=shade_min+(k-1)*dp_mem
           end do
           do k=1,col_mem_num/2+1
              col_mem_dim2(k)=shade_min+2*(k-1)*dp_mem
           end do
        end if

        if(direction=='t')then
           CALL UYAXNM( 'R', col_mem_dim1, col_mem_num+1, col_mem_dim2,  &
  &                     col_mem_num/2+1 )
           CALL UYAXNM( 'L', col_mem_dim1, col_mem_num+1, col_mem_dim2,  &
  &                     col_mem_num/2+1 )
        else
           CALL UXAXNM( 'T', col_mem_dim1, col_mem_num+1, col_mem_dim2,  &
  &                     col_mem_num/2+1 )
           CALL UXAXNM( 'B', col_mem_dim1, col_mem_num+1, col_mem_dim2,  &
  &                     col_mem_num/2+1 )
        end if

        deallocate(col_mem_dim1)
        deallocate(col_mem_dim2)

     else

        do k=1,color_num+1
           coldim1(k)=PI(1,k)
        end do
        do k=1,color_num/2+1
           coldim2(k)=PI(1,2*k-1)
        end do

        if(direction=='t')then
           CALL UYAXNM( 'R', coldim1, color_num+1, coldim2, color_num/2+1 )
           CALL UYAXNM( 'L', coldim1, color_num+1, coldim2, color_num/2+1 )
        else
           CALL UXAXNM( 'T', coldim1, color_num+1, coldim2, color_num/2+1 )
           CALL UXAXNM( 'B', coldim1, color_num+1, coldim2, color_num/2+1 )
        end if

     end if

  end if

!-- 実際に三角形領域を描く
  if(present(trigle))then
     call SGLSET( 'LSOFTF', .false. )
     select case(trigle)
     case('a')
write(*,*) "tricmax_num", tricmax_num, tricmin_num
        call DclShadeRegionNormalized( triux, triuy, tricmax_num )
        call DclShadeRegionNormalized( tridx, tridy, tricmin_num )
        call DclDrawLineNormalized( triux, triuy, index=13 )
        call DclDrawLineNormalized( tridx, tridy, index=13 )
     case('u')
write(*,*) "tricmax_num", tricmax_num
        call DclShadeRegionNormalized( triux, triuy, tricmax_num )
        call DclDrawLineNormalized( triux, triuy, index=13 )
     case('d')
write(*,*) "tricmax_num", tricmin_num
        call DclShadeRegionNormalized( tridx, tridy, tricmin_num )
        call DclDrawLineNormalized( tridx, tridy, index=13 )
     end select
write(*,*) "Map case check, triux, triuy, tridx, tridy"
write(*,*) triux, triuy, tridx, tridy 
  end if

  call DclSetParm( "GRAPH:LCLIP", .false. )
  if(present(title))then
     call DclDrawTitle( barp, trim(title), bart )
  end if

!  CALL UYAXDV( 'R', (shade_max-shade_min)/real(color_num), (shade_max-shade_min)/real(0.5*color_num) )
!  CALL UYAXDV( 'L', (shade_max-shade_min)/real(color_num), (shade_max-shade_min)/real(0.5*color_num) )

end subroutine tone_bar

!---------------------------------------------------------
!-----------------------------------------------------------

subroutine undef_CReSS2Dcl( nx, ny, nz, val )  ! CReSS の未定義値を Dcl の未定義値に変換するルーチン
  ! 引数の配列は 3 次元であるが, その前の要素数を 1 などに設定することで,
  ! 1, 2 次元の配列に対しても変換可能.
  use dcl
  implicit none
  integer, intent(in) :: nx  ! 第 1 要素の要素数
  integer, intent(in) :: ny  ! 第 2 要素の要素数
  integer, intent(in) :: nz  ! 第 3 要素の要素数
  real, intent(inout) :: val(nx,ny,nz)  ! 変換する配列
  integer :: i, j, k  ! 作業用配列
  real :: RMISS, undef  ! 各未定義値

!-- 欠損値処理 ---
!-- Dcl 側の undef 値セット
      CALL GLRGET( 'RMISS', RMISS )
      CALL GLLSET( 'LMISS', .TRUE. )

!-- CReSS 側の undef 値セット
      call undef_get( undef )
!write(*,*) "undef=", undef

  do k=1,nz
  do j=1,ny
     do i=1,nx
        if(val(i,j,k)/=val(i,j,k))then
           val(i,j,k)=-999.0
        else if(val(i,j,k)==undef)then
           val(i,j,k)=-999.0
!           val(i,j,k)=RMISS
        end if
     end do
  end do
  end do
    

end subroutine

!--------------------------------------------------------

subroutine nan_val( nx, ny, nz, undef, val )  ! val の中の nan 値を undef に入れ替える.
  ! 引数の配列は 3 次元であるが, その前の要素数を 1 などに設定することで,
  ! 1, 2 次元の配列に対しても変換可能.
  implicit none
  integer, intent(in) :: nx  ! 第 1 要素の要素数
  integer, intent(in) :: ny  ! 第 2 要素の要素数
  integer, intent(in) :: nz  ! 第 3 要素の要素数
  real, intent(in) :: undef  ! 代入する未定義値
  real, intent(inout) :: val(nx,ny,nz)  ! 変換する配列
  integer :: i, j, k  ! 作業用配列

  do k=1,nz
     do j=1,ny
        do i=1,nx
!!           if(isnan(val(i,j,k)))then
           if(val(i,j,k)/=val(i,j,k))then   ! isnan 関数がないことを考慮.
              val(i,j,k)=undef
           end if
        end do
     end do
  end do
    

end subroutine

!--------------------------------------------------------
!--------------------------------------------------------

subroutine check_azimuth_maxmin( coord, minv, maxv )
  !-- 方位角の coord の範囲を -180 <= y <= 180 で置き直し,
  !-- その最大値と最小値を返す.
  implicit none
  real, intent(in) :: coord(:)  ! 元の方位角座標 [deg]
  real, intent(out) :: minv     ! coord の最小値 (-180<=y<=180)
  real, intent(out) :: maxv     ! coord の最大値 (-180<=y<=180)
  real :: tmpv
  integer :: jj, nj

  nj=size(coord)

  if(coord(1)<-180.0)then
     minv=coord(1)+360.0
  else if(coord(1)>180.0)then
     minv=coord(1)-360.0
  else
     minv=coord(1)
  end if
  maxv=minv
  do jj=2,nj
     if(coord(jj)<-180.0)then
        tmpv=coord(jj)+360.0
     else if(coord(jj)>180.0)then
        tmpv=coord(jj)-360.0
     else
        tmpv=coord(jj)
     end if
     minv=min(minv,tmpv)
     maxv=max(maxv,tmpv)
  end do

  write(*,*) "*** MESSAGE (check_azimuth_maxmin) ***: ", minv, maxv

end subroutine check_azimuth_maxmin

!--------------------------------------------------------
!--------------------------------------------------------

subroutine color_setting( color_num, val_int, col_tab,   &
  &                       col_max, col_min, col_bg, reverse,  &
  &                       min_tab, max_tab, log_flag,  &
  &                       col_spec, val_spec )
  ! カラーマップの色と数値を対応させる自動ルーチン
  use dcl
  implicit none
  integer, intent(in) :: color_num  ! 使用するカラーの種類
  real, intent(in) :: val_int(2)  ! 描くカラーの上下端
                      ! val_int(1)=val_min, val_int(2)=val_max
  integer, intent(in), optional :: col_tab  ! dcl のカラーテーブル
  integer, intent(in), optional :: col_min  ! 使用するカラー番号の最小値(上2桁)
  integer, intent(in), optional :: col_max  ! 使用するカラー番号の最大値(上2桁)
  logical, intent(in), optional :: col_bg  ! 背景色の入れ替え デフォルトなし.
  integer :: map_num  ! カラーマップのマップ番号指定 (optional 属性をつけること)
  integer :: i, j, k  ! 作業用添字
  logical, intent(in), optional :: reverse  ! カラー番号を反転させる.
  integer, intent(in), optional :: min_tab  ! val_min 以下の値に対応するカラー番号, デフォルトは黒
  integer, intent(in), optional :: max_tab  ! val_max 以上の値に対応するカラー番号, デフォルトは黒
  logical, intent(in), optional :: log_flag ! スケールを対数化するか. デフォルトは .false.
  real, intent(in), optional :: val_spec(color_num+1)  ! カラーに対応する数値を陽的に指定する.
  integer, intent(in), optional :: col_spec(color_num)  ! val_spec で指定された値に対応したカラー番号
!-- 以上, 引数
  real :: val_min  ! 描くカラーの最小値
  real :: val_max  ! 描くカラーの最大値
!-- 以上, 引数の置き換え用変数
  integer :: ipat, iws
  real :: dv  ! カラーマップに対応する値の幅
  integer :: cmap_min, cmap_max
  real :: tlev1, tlev2
  logical :: rev, log_f, lfcart
  real :: white_min, black_max
  real :: RMISS
  integer :: white, black, ITON

!-- 引数の置き換え用変数に置き換え
  val_min=val_int(1)
  val_max=val_int(2)

!-- Dcl 側の undef 値セット
  CALL GLRGET( 'RMISS', RMISS )
  CALL GLLSET( 'LMISS', .TRUE. )

!-- カラーマップチェンジのフラグ
  CALL SWLSET( 'LCMCH', .TRUE. )

  if(present(col_tab))then
     map_num=col_tab
  else
     map_num=1
  end if

  if(present(col_min))then
     cmap_min=col_min
  else
     cmap_min=14
  end if

  if(present(col_max))then
     cmap_max=col_max
  else
     cmap_max=85
  end if

  if(present(col_bg))then
     call SWpSET( 'LFGBG', col_bg )     
  end if

  if(present(reverse))then
     rev=reverse
  else
     rev=.false.
  end if

  if(present(min_tab))then
     white=min_tab
  else
     white=999
  end if

  if(present(max_tab))then
     black=max_tab
  else
     black=1999
  end if

!-- back color is white flag. Ver.5.4.2

!  if(black==999.or.white==999)then
!     call DclSetParm( 'LCLCNV' , .FALSE. )
!  end if

!!-- back color is white flag. Ver.6.0.0
!!  if(black==999)then
!!     call SGIGET( 'IBGCLI', black )
!!     black=black*1000+999
!!  end if
!!  if(white==999)then
!!     call SGIGET( 'IBGCLI', white )
!!     white=white*1000+999
!!  end if

  call sgscmn(map_num)

  call UEITLV

!-- 対数化するかのフラグ
  if(present(log_flag))then
     log_f=log_flag
  else
     log_f=.false.
  end if
  if(log_f.eqv..true.)then
     if(val_min<0.0.or.val_max<0.0)then
        write(*,*) "### ERROR ### (color_setting)"
        write(*,*) "val_min and val_max must be more than zero."
        write(*,*) "STOP"
        stop
     end if
  end if

  if(present(col_spec))then

!-- val_max 以上を black で塗る
     TLEV1=RMISS
     TLEV2=val_spec(1)
     IPAT=white

!!     CALL DclSetShadeLevel( TLEV1, TLEV2, 999 )
     CALL DclSetShadeLevel( TLEV1, TLEV2, IPAT )
!     CALL UEQTLV( TLEV1, TLEV2, IPAT, color_num+2 )
!     write(*,*) TLEV1, TLEV2, IPAT

     do k=1,color_num
        TLEV1=val_spec(k)
        TLEV2=val_spec(k+1)
        IPAT=col_spec(k)
        CALL DclSetShadeLevel( TLEV1, TLEV2, IPAT )
     end do
     TLEV1=val_spec(color_num+1)
     TLEV2=RMISS
     IPAT=black
     CALL DclSetShadeLevel( TLEV1, TLEV2, IPAT )

  else

!-- val_max 以上を black で塗る
     TLEV1=RMISS
     TLEV2=val_min
     IPAT=white

!!     CALL DclSetShadeLevel( TLEV1, TLEV2, 999 )
     CALL DclSetShadeLevel( TLEV1, TLEV2, IPAT )
!     CALL UEQTLV( TLEV1, TLEV2, IPAT, color_num+2 )
!     write(*,*) TLEV1, TLEV2, IPAT

     if(log_f.eqv..true.)then
        dv=(log10(val_max)-log10(val_min))/color_num
     else
        dv=(val_max-val_min)/color_num
     end if

     if(rev.eqv..true.)then
        do k=1,color_num
           if(log_f.eqv..true.)then
              TLEV1=val_min*10.0**(dv*(k-1))
              TLEV2=val_min*10.0**(dv*(k))
!              TLEV2=TLEV1*dv
           else
              TLEV1=val_min+(k-1)*dv
              TLEV2=TLEV1+dv
           end if
           IPAT=(cmap_min+int((color_num-k)*(real(cmap_max-cmap_min)/real(color_num-1))))*1000+999
           CALL DclSetShadeLevel( TLEV1, TLEV2, IPAT )
!           CALL UEQTLV( TLEV1, TLEV2, IPAT, k )
!           write(*,*) TLEV1, TLEV2, IPAT
        end do
     else
        do k=1,color_num
           if(log_f.eqv..true.)then
              TLEV1=val_min*10.0**(dv*(k-1))
              TLEV2=val_min*10.0**(dv*(k))
!              TLEV2=TLEV1*dv
           else
              TLEV1=val_min+(k-1)*dv
              TLEV2=TLEV1+dv
           end if
           IPAT=(cmap_min+int((k-1)*(real(cmap_max-cmap_min)/real(color_num-1))))*1000+999

           CALL DclSetShadeLevel( TLEV1, TLEV2, IPAT )
!           CALL UEQTLV( TLEV1, TLEV2, IPAT, k )
!           write(*,*) TLEV1, TLEV2, IPAT
        end do
     end if
     TLEV1=val_max
     TLEV2=RMISS
     IPAT=black
     CALL DclSetShadeLevel( TLEV1, TLEV2, IPAT )
!     CALL UEQTLV( TLEV1, TLEV2, IPAT, color_num+1 )

  end if

  write(*,*) TLEV1, TLEV2, IPAT

end subroutine color_setting

!---------------------------------------------------

subroutine contour_setting( contour_num, val_int, col_tab,   &
  &                         col_max, col_min,  &
  &                         log_flag, col_spec, val_spec, formc )
  ! コンターの線色と数値を対応させる自動ルーチン
  use dcl
  implicit none
  integer, intent(in) :: contour_num  ! 使用する等値線の数
  real, intent(in) :: val_int(2)  ! 描く等値線の上下端
                      ! val_int(1)=val_min, val_int(2)=val_max
  integer, intent(in), optional :: col_tab  ! dcl のカラーテーブル
  integer, intent(in), optional :: col_min  ! 使用するカラー番号の最小値(上2桁)
  integer, intent(in), optional :: col_max  ! 使用するカラー番号の最大値(上2桁)
  logical, intent(in), optional :: log_flag ! スケールを対数化するか. デフォルトは .false.
  real, intent(in), optional :: col_spec(contour_num)  ! カラーに対応する数値を陽的に指定する.
  integer, intent(in), optional :: val_spec(contour_num)  ! col_spec で指定された値に対応したカラー番号
  character(*), intent(in), optional :: formc  ! 等値線のラベルフォーマット.
!-- 以上, 引数
  integer :: map_num  ! カラーマップのマップ番号指定 (optional 属性をつけること)
  integer :: i, j, k  ! 作業用添字
  real :: val_min  ! 描くカラーの最小値
  real :: val_max  ! 描くカラーの最大値
!-- 以上, 引数の置き換え用変数
  integer :: ipat, iws
  integer :: cmap_min, cmap_max
  real :: dv  ! カラーマップに対応する値の幅
  real :: tlev1, tlev2, tmpsiz
  logical :: log_f, lfcart, ltype
  integer :: aidx, ptype, ntype, tmptyp
  character(8) :: labelc, forma

!-- 引数の置き換え用変数に置き換え
  val_min=val_int(1)
  val_max=val_int(2)

!-- Dcl 側の undef 値セット
!  CALL GLRGET( 'RMISS', RMISS )
!  CALL GLLSET( 'LMISS', .TRUE. )

!-- カラーマップチェンジのフラグ
  CALL SWLSET( 'LCMCH', .TRUE. )

  if(present(col_tab))then
     map_num=col_tab
  else
     map_num=1
  end if

  if(present(col_min))then
     cmap_min=col_min
  else
     cmap_min=14
  end if

  if(present(col_max))then
     cmap_max=col_max
  else
     cmap_max=85
  end if

  if(present(formc))then
     forma=trim(adjustl(formc))
  else
     forma='(f8.1)'
  end if

  aidx=DclGetLineIndex()

  ptype=DclGetLineType( )
  call DclGetParm( 'IDASH', ntype )
  call DclGetParm( 'LDASH', ltype )
  if(ltype.eqv..true.)then
     ntype=2
  end if
  write(*,*) "check, contoursetting", ntype, ltype
  call DclGetParm( 'RSIZEL', tmpsiz )

  call sgscmn(map_num)

!  call UEITLV

!-- 対数化するかのフラグ
  if(present(log_flag))then
     log_f=log_flag
  else
     log_f=.false.
  end if
  if(log_f.eqv..true.)then
     if(val_min<0.0.or.val_max<0.0)then
        write(*,*) "### ERROR ### (color_setting)"
        write(*,*) "val_min and val_max must be more than zero."
        write(*,*) "STOP"
        stop
     end if
  end if

  if(log_f.eqv..true.)then
     dv=(log10(val_max)-log10(val_min))/contour_num
  else
     dv=(val_max-val_min)/contour_num
  end if

  do k=1,contour_num
     if(present(col_spec))then
        TLEV1=col_spec(k)
        if(present(val_spec))then
           IPAT=val_spec(k)
        else
           if(contour_num==1)then
              IPAT=cmap_min*10+aidx
           else
              IPAT=(cmap_min+int((k-1)*(real(cmap_max-cmap_min)/real(contour_num-1))))*10+aidx
           end if
        end if
     else
        if(log_f.eqv..true.)then
           TLEV1=val_min*10.0**(dv*(k-1))
        else
           TLEV1=val_min+(k-1)*dv
        end if
        if(present(val_spec))then
           IPAT=val_spec(k)
        else
           if(contour_num==1)then
              IPAT=cmap_min*10+aidx
           else
              IPAT=(cmap_min+int((k-1)*(real(cmap_max-cmap_min)/real(contour_num-1))))*10+aidx
           end if
        end if
     end if

!-- [NOTE] : DclSetContourLine は label, height ともにオプションのデフォルト値が
!            '' と 0.0 なので, これらのオプションを"両方"指定しない場合,
!            等値線にラベルが描かれない.
     write(labelc,trim(adjustl(forma))) TLEV1
     if(TLEV1<0.0)then
        tmptyp=ntype
     else
        tmptyp=ptype
     end if
     call DclSetContourLine( TLEV1, index=IPAT, type=tmptyp,  &
  &                          label=labelc(1:8), height=tmpsiz )
  end do

end subroutine contour_setting

!---------------------------------------------------

subroutine contourl_setting( contour_num, val_spec, idx_spec, typ_spec,  &
  &                          col_tab, formc )
  ! 任意のレベルでコンターを対応させる自動ルーチン
  use dcl
  implicit none
  integer, intent(in) :: contour_num  ! 使用する等値線の数
  real, intent(in) :: val_spec(contour_num)  ! レベルの数値を陽的に指定する.
  integer, intent(in) :: idx_spec(contour_num)  ! レベルに対応するラインインデックス
  integer, intent(in) :: typ_spec(contour_num)  ! レベルに対応するラインインデックス
  integer, intent(in), optional :: col_tab  ! dcl のカラーテーブル
  character(*), intent(in), optional :: formc  ! 等値線のラベルフォーマット.
!-- 以上, 引数
  integer :: map_num  ! カラーマップのマップ番号指定 (optional 属性をつけること)
  integer :: i, j, k  ! 作業用添字
!-- 以上, 引数の置き換え用変数
  integer :: ipat, iws, ityp
  real :: tlev1, tlev2, tmpsiz
  logical :: ltype
  character(8) :: labelc, forma

!-- Dcl 側の undef 値セット
!  CALL GLRGET( 'RMISS', RMISS )
!  CALL GLLSET( 'LMISS', .TRUE. )

!-- カラーマップチェンジのフラグ
  CALL SWLSET( 'LCMCH', .TRUE. )

  if(present(col_tab))then
     map_num=col_tab
  end if

  if(present(formc))then
     forma=trim(adjustl(formc))
  else
     forma='(f8.1)'
  end if

!  aidx=DclGetLineIndex()

!  ptype=DclGetLineType( )
!  call DclGetParm( 'IDASH', ntype )
!  call DclGetParm( 'LDASH', ltype )
!  if(ltype.eqv..true.)then
!     ntype=2
!  end if
!  write(*,*) "check, contoursetting", ntype, ltype
  call DclGetParm( 'RSIZEL', tmpsiz )

  if(present(col_tab))then
     call sgscmn(map_num)
  end if

!  call UEITLV

  do k=1,contour_num
     TLEV1=val_spec(k)
     IPAT=idx_spec(k)
     ityp=typ_spec(k)

!!-- [NOTE] : DclSetContourLine は label, height ともにオプションのデフォルト値
!!            が '' と 0.0 なので, これらのオプションを"両方"指定しない場合,
!!            等値線にラベルが描かれない.
     write(labelc,trim(adjustl(forma))) TLEV1
!     if(TLEV1<0.0)then
!        tmptyp=ntype
!     else
!        tmptyp=ptype
!     end if
     call DclSetContourLine( TLEV1, index=IPAT, type=ityp,  &
  &                          label=labelc(1:8), height=tmpsiz )
  end do

end subroutine contourl_setting

!---------------------------------------------------

subroutine monotone_setting( ton_tab, val_int, nega_ton_tab,  &
  &                          full_tone )
! color_setting のモノトーンバージョン
! トーンテーブルは白を抜いて 5 種類しかないので, val_min, val_max を強制的に 5 分割し, トーンを当てはめる.
! また, nega_ton_tab が指定されていれば, 10 分割する.
  use dcl
  implicit none
  integer, intent(in), optional :: ton_tab  ! dcl のトーンテーブル
  real, intent(in) :: val_int(2)  ! 描くカラーの上下端
                      ! val_int(1)=val_min, val_int(2)=val_max
  integer, intent(in), optional :: nega_ton_tab  ! トーンテーブルを 2 枚使うとき, 値の小さい領域に向かって濃くしていく場合に指定. このトーンを 0 から負方向に濃くしていく.
  logical, intent(in), optional :: full_tone  ! 白を合わせると, 各トーンで 6 段階あるので, val_min, val_max の差を強制的に 6 分割してトーンを割り当てる. ただし, これをすると, トーンの境界値が切りのよい数値にならない時がある. 値は .true. で有効となる.
!-- 以上, 引数
  real :: val_min  ! 描くカラーの最小値
  real :: val_max  ! 描くカラーの最大値
!-- 以上, 引数の置き換え用変数
  integer :: map_num  ! カラーマップのマップ番号指定 (optional 属性をつけること)
  integer :: i, j, k  ! 作業用添字
  integer :: ipat, itvtone, tone_mapping
  real :: dv  ! カラーマップに対応する値の幅
  integer :: cmap_min, cmap_max
  real :: tlev1, tlev2

!-- 引数の置き換え用変数に置き換え
  val_min=val_int(1)
  val_max=val_int(2)

  call UEITLV

  if(present(nega_ton_tab))then
     if(present(full_tone))then
        if(full_tone.eqv..true.)then
           itvtone=12
        else
           itvtone=10
        end if
     else
        itvtone=10
     end if
  else
     if(present(full_tone))then
        if(full_tone.eqv..true.)then
           itvtone=6
        else
           itvtone=5
        end if
     else
        itvtone=5
     end if
  end if

  dv=(val_max-val_min)/real(itvtone)

  if(itvtone==10.or.itvtone==12)then
     tone_mapping=itvtone/2
  else
     tone_mapping=itvtone
  end if

  if(itvtone==tone_mapping)then
     do k=1,tone_mapping
        TLEV1=val_min+(k-1)*dv
        TLEV2=TLEV1+dv
        IPAT=100*ton_tab+k
        CALL UESTLV( TLEV1, TLEV2, IPAT )
     end do
  else
     do k=1,tone_mapping
        TLEV1=0.5*(val_max+val_min)+(k-1)*dv
        TLEV2=TLEV1+dv
        IPAT=100*ton_tab+k
        CALL UESTLV( TLEV1, TLEV2, IPAT )
write(*,*) "tlev", tlev1, tlev2
     end do
     do k=1,tone_mapping
        TLEV1=0.5*(val_max+val_min)-k*dv
        TLEV2=TLEV1+dv
        IPAT=100*nega_ton_tab+k
        CALL UESTLV( TLEV1, TLEV2, IPAT )
write(*,*) "bgtlev", tlev1, tlev2
     end do
  end if


end subroutine monotone_setting

!---------------------------------------------------
!---------------------------------------------------

subroutine Dcl_Square_Normal( viewx_int, viewy_int, line, color )
! 正規化座標系において, 四角領域を作成し, 任意の色と線で塗りつぶす.
! color = 0 なら塗りつぶさず, 枠を書くだけ.
  use dcl
  implicit none
  real, intent(in) :: viewx_int(2)  ! x 方向の正規座標
  real, intent(in) :: viewy_int(2)  ! x 方向の正規座標
  integer, intent(in) :: line       ! 枠線の色と太さ(DCL の index, type に従う)
  integer, intent(in) :: color      ! 四角を塗りつぶす色(DCL のカラーマップに従う)
  real :: vx(5), vy(5)

  vx(:)=(/viewx_int(1), viewx_int(2), viewx_int(2), viewx_int(1), viewx_int(1)/)
  vy(:)=(/viewy_int(1), viewy_int(1), viewy_int(2), viewy_int(2), viewy_int(1)/)

  if(color/=0)then
     call DclShadeRegionNormalized( vx, vy, color )
  end if
  call DclDrawLineNormalized( vx, vy, index=line )

end subroutine Dcl_Square_Normal

!---------------------------------------------------
!---------------------------------------------------

subroutine undef_get( undef )  ! CReSS のデフォルトの未定義値を取得するルーチン
  implicit none
  real, intent(inout) :: undef  ! 未定義値

  undef = -1.0e+35

end subroutine undef_get

!---------------------------------------------------
!---------------------------------------------------

subroutine Dcl_Special_Axis( space, map_pro, mlitv, vx_int, vy_int, t_posi,  &
  &                          lon_int, lat_int, axis_title, labi, labh )
! 直交しない地図座標において, 直交座標系での地図軸を描画するルーチン
! デフォルトの DCL の場合, メルカトル以外の地図描画では,
! 軸の描画ルーチンが, 緯度経度でしか描画できないので, デカルト系で
! 地図投影した場合の地図の度数軸を描くためのもの.
  use dcl
  implicit none
  character(*), intent(in) :: space  ! 座標軸を描く場所 "btrl" で表記.
  integer, intent(in) :: map_pro  ! 用いている地図番号
  real, intent(in) :: mlitv  ! 軸にラベルを貼るときの間隔 [degree]
  real, intent(in) :: vx_int(2)  ! x 方向の v 座標系
  real, intent(in) :: vy_int(2)  ! y 方向の v 座標系
  real, intent(in) :: t_posi(3)  ! 地図投影時の正軸 t_posi=(/lat1, lat2, lon/)
  real, intent(in) :: lon_int(:,:)  ! デカルト系の各格子点に与えられている経度
  real, intent(in) :: lat_int(:,:)  ! デカルト系の各格子点に与えられている緯度
                                    ! ともに [degree]
  character(*), intent(in) :: axis_title(2)  ! axis title
  integer, intent(in), optional :: labi   ! label's index
  real, intent(in), optional :: labh   ! label's height

  real, allocatable, dimension(:) :: lon_tranc, lat_tranc
  integer :: i, j, k, i_lon, i_lat
  integer :: nc, nx, ny, nlon, nlat, nlonb, nlatb
  integer :: label_index
  real :: map_min_lon, map_max_lon, map_min_lat, map_max_lat
  real :: uratio, vratio
  real :: tmp_x(2), tmp_y(2), label_v
  real :: ref_x_min, ref_x_max, ref_y_min, ref_y_max, rho0
  real :: label_height
  character(10) :: label
  character(20) :: fmt_mxitv, fmt_myitv

!-- 文関数用変数
  real :: rho, rhon, var, var1, var2, s, sn
  real, parameter :: pi=3.141592

!-- function
  rho(var,var1,s)=cos(var1*pi/180.0)*(tan(0.25*pi-0.5*var*pi/180.0))**s  &
  &               /(s*(tan(0.25*pi-0.5*var1*pi/180.0))**s)
  rhon(var1,var2)=log(cos(var1*pi/180.0)/cos(var2*pi/180.0))  &
  &               /log(tan(0.25*pi-0.5*var1*pi/180.0)/tan(0.25*pi-0.5*var2*pi/180.0))

!-- optional variable's operation
  if(present(labh))then
     label_height=labh
  else
     label_height=DclGetTextHeight()
  end if

  if(present(labi))then
     label_index=labi
  else
     label_index=DclGetTextIndex()
  end if

!-- 地図をデカルト系で四角に囲む

  call Dcl_Square_Normal( vx_int, vy_int, 4, 0 )

  nc=len_trim(adjustl(space))
  nx=size(lon_int,1)
  ny=size(lon_int,2)

!-- 基準座標点でのパラメータ計算

  sn=rhon(t_posi(1),t_posi(2))
  rho0=cos(t_posi(1))/sn

!-- 基準座標点での v 座標系の相対位置
!-- この点を用いて, 最終的に平行移動させる.

  ref_x_min=rho(lat_int(1,1),t_posi(1),sn)*sin(sn*(lon_int(1,1)-t_posi(3))*pi/180.0)
  ref_x_max=rho(lat_int(nx,1),t_posi(1),sn)*sin(sn*(lon_int(nx,1)-t_posi(3))*pi/180.0)
  ref_y_min=rho0-rho(lat_int(1,1),t_posi(1),sn)*cos(sn*(lon_int(1,1)-t_posi(3))*pi/180.0)
  ref_y_max=rho0-rho(lat_int(1,ny),t_posi(1),sn)*cos(sn*(lon_int(1,ny)-t_posi(3))*pi/180.0)
  uratio=(vx_int(2)-vx_int(1))/(ref_x_max-ref_x_min)
  vratio=(vy_int(2)-vy_int(1))/(ref_y_max-ref_y_min)

!-- map におけるラベルの終端位置の計算

  map_min_lon=(aint(lon_int(1,1)/mlitv)+1.0)*mlitv
  map_min_lat=(aint(lat_int(1,1)/mlitv)+1.0)*mlitv
  map_max_lon=(aint(lon_int(nx,1)/mlitv))*mlitv
  map_max_lat=(aint(lat_int(1,ny)/mlitv))*mlitv

  if(map_min_lon-mlitv>=lon_int(1,1))then
     map_min_lon=map_min_lon-mlitv
  end if
  if(map_min_lat-mlitv>=lat_int(1,1))then
     map_min_lat=map_min_lat-mlitv
  end if
  if(map_max_lon+mlitv<=lon_int(nx,1))then
     map_max_lon=map_max_lon+mlitv
  end if
  if(map_max_lat+mlitv<=lat_int(1,ny))then
     map_max_lat=map_max_lat+mlitv
  end if

!-- ここまで終端位置の計算

!-- ラベルを描くのに必要な配列要素の数

  nlon=int((map_max_lon-map_min_lon)/mlitv)+1
  nlat=int((map_max_lat-map_min_lat)/mlitv)+1

  allocate(lon_tranc(nlon))
  allocate(lat_tranc(nlat))

!-- 実際の各ラベルでの計算
!-- 計算の順序は以下のとおり.
! (1) 軸でラベルを描く点を決める. このときの軸を便宜上「軸1」とする.
! (2) その点での v 座標系の値を決めるには, その点の経緯度が必要.
! (3) ラベルを描く軸1 は自由に決められるがそのときもう片方の軸2 の度は
!     得られないので, 軸1 で描く場所の軸1 についての隣接点から内挿する.
!     隣接点は 2 次元データで与えられる経緯度なので, これらの値から
!     v 座標が得られるので, これらをもとに, v 系で内挿を行う.
! と思ったが, 隣接点の lon, lat から得られた v 系の値を内挿しても
! それほど変化しないのではないかと思ったので, 今はこの方法で計算している.

!-- ラベルのフォーマット自動設定
  call auto_label_fmt( mlitv, max(max(lon_int(nx,1),lon_int(nx,ny)),lon_int(1,ny)), fmt_mxitv )
  call auto_label_fmt( mlitv, max(max(lat_int(nx,1),lat_int(nx,ny)),lat_int(1,ny)), fmt_myitv )

  call DclSetParm( "GRAPH:LCLIP", .false. )
  select case (map_pro)
  case (22)  ! conical
     do j=1,nc
        select case (space(j:j))
        case ('b')
           nlonb=1
           nlatb=1
           do i=1,nlon
              lon_tranc(i)=map_min_lon+(i-1)*mlitv
              call val_estimate( lon_int(:,nlatb), lon_tranc(i), i_lon )
              tmp_x(1)=rho(lat_int(i_lon,nlatb), t_posi(1),sn)  &
  &                    *sin(sn*(lon_int(i_lon,nlatb)-t_posi(3))*pi/180.0)
              tmp_x(2)=rho(lat_int(i_lon+1,nlatb), t_posi(1),sn)  &
  &                    *sin(sn*(lon_int(i_lon+1,nlatb)-t_posi(3))*pi/180.0)
              label_v=(tmp_x(1)-ref_x_min+(tmp_x(2)-tmp_x(1))  &
  &                   /(lon_int(i_lon+1,nlatb)-lon_int(i_lon,nlatb))  &
  &                   *(lon_tranc(i)-lon_int(i_lon,nlatb)))*uratio+vx_int(1)
              write(label,trim(adjustl(fmt_mxitv))) lon_tranc(i)
              call DclDrawTextNormalized( label_v, vy_int(1)-0.015,  &
  &                                       trim(adjustl(label)),  &
  &                                       height=0.95*label_height,  &
  &                                       index=label_index )
           end do
           if(len_trim(adjustl(axis_title(1)))/=0)then
              call DclDrawTextNormalized( (vx_int(1)+vx_int(2))*0.5,  &
  &                                       vy_int(1)-0.95*label_height-0.015,  &
  &                                       trim(axis_title(1)),  &
  &                                       height=label_height,  &
  &                                       index=label_index )
           end if
        case ('t')
           nlonb=1
           nlatb=ny
           do i=1,nlon
              lon_tranc(i)=map_min_lon+(i-1)*mlitv
              call val_estimate( lon_int(:,nlatb), lon_tranc(i), i_lon )
              tmp_x(1)=rho(lat_int(i_lon,nlatb), t_posi(1),sn)  &
  &                    *sin(sn*(lon_int(i_lon,nlatb)-t_posi(3))*pi/180.0)
              tmp_x(2)=rho(lat_int(i_lon+1,nlatb), t_posi(1),sn)  &
  &                    *sin(sn*(lon_int(i_lon+1,nlatb)-t_posi(3))*pi/180.0)
              label_v=(tmp_x(1)-ref_x_min+(tmp_x(2)-tmp_x(1))  &
  &                   /(lon_int(i_lon+1,nlatb)-lon_int(i_lon,nlatb))  &
  &                   *(lon_tranc(i)-lon_int(i_lon,nlatb)))*uratio+vx_int(1)
              write(label,trim(adjustl(fmt_mxitv))) lon_tranc(i)
              call DclDrawTextNormalized( lon_tranc(i), vy_int(2)+0.015,  &
  &                                       trim(adjustl(label)),  &
  &                                       height=0.95*label_height,  &
  &                                       index=label_index )
           end do
        case ('l')
           nlonb=1
           nlatb=1
           do i=1,nlat
              lat_tranc(i)=map_min_lat+(i-1)*mlitv
              call val_estimate( lat_int(nlonb,:), lat_tranc(i), i_lat )
              tmp_y(1)=-rho(lat_int(nlonb,i_lat), t_posi(1),sn)  &
  &                    *cos(sn*(lon_int(nlonb,i_lat)-t_posi(3))*pi/180.0)
              tmp_y(2)=-rho(lat_int(nlonb,i_lat+1), t_posi(1),sn)  &
  &                    *cos(sn*(lon_int(nlonb,i_lat+1)-t_posi(3))*pi/180.0)
              label_v=(rho0+tmp_y(1)-ref_y_min+(tmp_y(2)-tmp_y(1))  &
  &                   /(lat_int(nlonb,i_lat+1)-lat_int(nlonb,i_lat))  &
  &                   *(lat_tranc(i)-lat_int(nlonb,i_lat)))*vratio+vy_int(1)
              write(label,trim(adjustl(fmt_myitv))) lat_tranc(i)
              call DclDrawTextNormalized( vx_int(1)-0.01, label_v,  &
  &                                       trim(adjustl(label)),  &
  &                                       height=0.95*label_height,  &
  &                                       index=label_index, centering=1 )
           end do
           if(len_trim(adjustl(axis_title(2)))/=0)then
              call DclDrawTextNormalized( vx_int(1)-5.0*0.95*label_height-0.01,  &
  &                                       (vy_int(2)+vy_int(1))*0.5,  &
  &                                       trim(axis_title(2)), angle=90.0,  &
  &                                       height=label_height,  &
  &                                       index=label_index )
           end if
        end select
     end do
  end select
  call DclSetParm( "GRAPH:LCLIP", .true. )

end subroutine Dcl_Special_Axis

!---------------------------------------------------
!---------------------------------------------------

subroutine color_line( sub_idx, linex, liney, valz, c_num,  &
  &                    c_itv, tri_col, col_val, col_idx, col_typ,  &
  &                    col_siz, subsubidx )
! 直線を valz をもとにカラー表示するルーチン
  use dcl
  implicit none
  character(1), intent(in) :: sub_idx  ! どのルーチンを呼び出すか.
                      ! [s] = DclScalingPoint
                      ! [l] = DclDrawLine
                      ! [p] = DclDrawMark
  real, intent(in) :: linex(:,:)  ! x 軸の直線座標
  real, intent(in) :: liney(size(linex,1),size(linex,2))  ! y 軸の直線座標
  real, intent(in) :: valz(size(linex,1),size(linex,2))  ! 直線での値
  integer, intent(in) :: c_num  ! 用いるカラー数
  real, intent(in) :: c_itv(2)  ! 自動カラーを設定するときの最大最小値.
  integer, intent(in), optional :: tri_col(2)  ! valz が c_itv を越えたときの
                                   ! 設定カラー番号
  real, intent(in), optional :: col_val(:)  ! valz に関連付けられるカラーの境界値
  integer, intent(in), optional :: col_idx(:)  ! c_val に対応するカラー番号
  integer, intent(in), optional :: col_typ(:)  ! c_val に対応する type
  real, intent(in), optional :: col_siz(:)  ! c_val に対応する size
  character(1), intent(in), optional :: subsubidx  ! sub_idx = 's' の場合,
                                        ! line 用か mark 用のどちらについて
                                        ! 呼び出しているか ('l' or 'p')
  integer :: nlx, nly, ncl, itmp, tmp_i, i, j, counter
  real, allocatable, dimension(:) :: c_val
  integer, allocatable, dimension(:) :: c_idx
  integer :: ci_itv(2), over_col(2), ccnum, tmpci
  real :: tmp1_itv(2), tmp2_itv(2)
  real :: defun, tmpcmin, tmpcmax
  integer :: ci_min, ci_max, aidx, ltyp, mtyp
  real :: msiz
  logical :: overcol_flag(2)

  call GLRGET( 'RMISS', defun )

  nlx=size(linex,1)
  nly=size(linex,2)

  overcol_flag=.false.

  if(sub_idx=='s')then
     if(.not.present(subsubidx))then
        write(*,*) "*** ERROR (color_line:dcl_automatic) ***"
        write(*,*) "When sub_idx == s, subsubidx must be set. Stop."
        stop
     end if
  end if

  if(present(col_val))then
     select case (sub_idx)
     case('s')   ! ここでの値は使わない.
        aidx=DclGetLineIndex()
     case('l')
        aidx=DclGetLineIndex()
     case('p')
        aidx=DclGetMarkerIndex()
     end select
     if(aidx<=0)then   ! 定義されていない場合は 1.
        aidx=1
     end if

     ncl=size(col_val)-1

     allocate(c_val(ncl+1))
     allocate(c_idx(ncl+1))

     do i=1,ncl+1
        c_val(i)=col_val(i)
        c_idx(i)=col_idx(i)
     end do
  else   ! col_val が設定されていない場合は今の color_setting の値を用いる.
     ncl=c_num

     allocate(c_val(ncl+1))
     allocate(c_idx(ncl+1))

     ccnum=DclGetShadeLevelNumber()

     if(ccnum==0)then
        ci_min=15
        ci_max=85
     else
        do i=1,ccnum
           call DclGetShadeLevel( i, tmpcmin, tmpcmax, tmpci )
!           if(tmpcmin==c_itv(1))then
!              ci_min=tmpci/1000     ! 下 3 桁は 999 で設定されているはず.
!              setc_check(1)=.true.
!           end if
!           if(tmpcmax==c_itv(2))then
!              ci_max=tmpci/1000     ! 下 3 桁は 999 で設定されているはず.
!              setc_check=.true.
!           end if
           if(tmpcmin==defun)then    ! 下三角の色
              over_col(1)=tmpci/1000
              overcol_flag(1)=.true.
!write(*,*) "tmpcmin check", i, tmpcmin, tmpcmax, tmpci/1000
           end if
           if(tmpcmax==defun)then    ! 上三角の色
              over_col(2)=tmpci/1000
              overcol_flag(2)=.true.
!write(*,*) "tmpcmax check", i, tmpcmin, tmpcmax, tmpci/1000
           end if
           if(tmpcmax/=defun)then    ! col_val, col_idx 設定
              c_val(i)=tmpcmax
              c_idx(i)=tmpci/1000
!write(*,*) "tmpcint check", i, tmpcmin, tmpcmax, c_val(i), tmpci/1000
           end if
        end do
     end if

     select case (sub_idx)
     case('s')   ! ここでの値は使わない.
        aidx=DclGetLineIndex()
     case('l')
        aidx=DclGetLineIndex()
     case('p')
        aidx=DclGetMarkerIndex()
     end select
     if(aidx<=0)then   ! 定義されていない場合は 1.
        aidx=1
     end if

     do i=1,ncl+1
        if(ccnum==0)then
           c_val(i)=c_itv(1)+(c_itv(2)-c_itv(1))/real(c_num)*(i-1)
           c_idx(i)=(ci_min+int(real(ci_max-ci_min)/real(c_num)*real(i-1)))*10+aidx
                 ! index はデフォルトで設定されている値を参照する.
        else
           c_idx(i)=c_idx(i)*10+aidx
        end if
     end do
  end if

  ! get marker size and line type (developing)
  select case (sub_idx)
  case('l')
     ltyp=DclGetLineType()
  case('p')
     msiz=DclGetMarkerSize()
     mtyp=DclGetMarkerType()
  end select

  if(present(tri_col))then
     over_col(1)=tri_col(1)
     over_col(2)=tri_col(2)
     overcol_flag=.true.
  end if

!write(*,*) "over_col check", over_col, aidx

  if(overcol_flag(1).eqv..true.)then
     over_col(1)=over_col(1)*10+aidx
  end if
  if(overcol_flag(1).eqv..true.)then
     over_col(2)=over_col(2)*10+aidx
  end if

  do j=1,nly
     if(valz(1,j)/=defun)then
        call val_estimate( c_val, valz(1,j), itmp )
        itmp=itmp+1  ! val_estimate は valz を越えない c_val(itmp) を与えるため, +1 しておく.
        counter=1
     else
        itmp=-1
        counter=2
     end if

     if(nlx>1)then
        do i=2,nlx
           if(valz(i,j)/=defun)then
              call val_estimate( c_val, valz(i,j), tmp_i )
              tmp_i=tmp_i+1  ! val_estimate は valz を越えない c_val(itmp) を与えるため.
           else
              tmp_i=-1
           end if
           if(itmp/=tmp_i.and.i-counter>=0.and.itmp/=-1.and.i/=nlx)then
              select case (sub_idx)
              case ('s')
                 if(subsubidx=='l')then
                    if(i-counter>=1.and.tmp_i/=-1)then
                       if(counter==1)then
                          if(linex(counter,j)/=defun.and.  &
  &                          liney(counter,j)/=defun)then
                             counter=counter+1
                          end if
                       else
                          if(linex(counter-1,j)/=defun.and.  &
  &                          liney(counter-1,j)/=defun)then
                             counter=counter+1
                          end if
                       end if
                       if(i-counter>=1)then
                          call DclScalingPoint( linex(counter-1:i-1,j),  &
  &                                             liney(counter-1:i-1,j) )
                       end if
                    end if
                 else
                    call DclScalingPoint( linex(counter:i-1,j),  &
  &                                       liney(counter:i-1,j) )
                 end if
              case ('l')
                 if(i-counter>=1.and.tmp_i/=-1)then
                    if(counter==1)then
                       if(linex(counter,j)/=defun.and.  &
  &                       liney(counter,j)/=defun)then
                          counter=counter+1
                       end if
                    else
                       if(linex(counter-1,j)/=defun.and.  &
  &                       liney(counter-1,j)/=defun)then
                          counter=counter+1
                       end if
                    end if
                    if(i-counter>=1)then
                       if(itmp==1.and.overcol_flag(1).eqv..true.)then
!write(*,*) "DrawLinex check11", linex(counter-1:i-1,j), counter-1, i-1
!write(*,*) "DrawLiney check11", liney(counter-1:i-1,j), counter-1, i-1
!write(*,*) "DrawLinez check11", valz(counter-1:i-1,j), counter-1, i-1
                          call DclDrawLine( linex(counter-1:i-1,j),  &
  &                                         liney(counter-1:i-1,j),  &
  &                                         index=over_col(1), type=ltyp )
                       else if(itmp==ncl+2.and.overcol_flag(2).eqv..true.)then
!write(*,*) "DrawLinex check12", linex(counter-1:i-1,j), counter-1, i-1
!write(*,*) "DrawLiney check12", liney(counter-1:i-1,j), counter-1, i-1
!write(*,*) "DrawLinez check12", valz(counter-1:i-1,j), counter-1, i-1
                          call DclDrawLine( linex(counter-1:i-1,j),  &
  &                                         liney(counter-1:i-1,j),  &
  &                                         index=over_col(2), type=ltyp )
                       else if(itmp>1.and.itmp<ncl+2)then
!write(*,*) "DrawLinex check13", linex(counter-1:i-1,j), counter-1, i-1
!write(*,*) "DrawLiney check13", liney(counter-1:i-1,j), counter-1, i-1
!write(*,*) "DrawLinez check13", valz(counter-1:i-1,j), counter-1, i-1
                          call DclDrawLine( linex(counter-1:i-1,j),  &
  &                                         liney(counter-1:i-1,j),  &
  &                                         index=c_idx(itmp), type=ltyp )
                       end if
                    end if
                 end if
              case ('p')
                 if(itmp==1.and.overcol_flag(1).eqv..true.)then
!write(*,*) "tmpcheck1", over_col(1), itmp
!write(*,*) "tmplcheck1", linex(counter:i-1,j)
                    call DclDrawMarker( linex(counter:i-1,j),  &
  &                                     liney(counter:i-1,j),  &
  &                                     index=over_col(1),  &
  &                                     type=mtyp, height=msiz )
                 else if(itmp==ncl+2.and.overcol_flag(2).eqv..true.)then
!write(*,*) "tmpcheck1", over_col(2), itmp
!write(*,*) "tmplcheck1", linex(counter:i-1,j)
                    call DclDrawMarker( linex(counter:i-1,j),  &
  &                                     liney(counter:i-1,j),  &
  &                                     index=over_col(2),  &
  &                                     type=mtyp, height=msiz )
                 else if(itmp>1.and.itmp<ncl+2)then
!write(*,*) "tmpcheck1", c_idx(itmp), itmp
!write(*,*) "tmplcheck1", linex(counter:i-1,j)
                    call DclDrawMarker( linex(counter:i-1,j), liney(counter:i-1,j),  &
  &                                   index=c_idx(itmp), type=mtyp, height=msiz )
                 end if
              end select
              if(i-counter==1)then
                 counter=i
              else if(sub_idx=='p'.and.i-counter>=0)then
                 counter=i
              else if(sub_idx=='s'.and.i-counter>=0)then
                 if(subsubidx=='p')then
                    counter=i
                 end if
              end if
           else if(i==nlx.and.nlx-counter>1.and.itmp/=-1)then
              select case (sub_idx)
              case ('s')
                 if(subsubidx=='l')then
                    call DclScalingPoint( linex(counter-1:nlx,j),  &
  &                                       liney(counter-1:nlx,j) )
                 else
                    call DclScalingPoint( linex(counter:nlx,j),  &
  &                                       liney(counter:nlx,j) )
                 end if
              case ('l')
                 if(counter==1)then  ! ラインの最初から最後まで同じ色の場合
                    counter=2  ! counter=counter+1
                 end if
                 if(itmp==1)then
                    if(overcol_flag(1).eqv..true.)then
                       call DclDrawLine( linex(counter-1:nlx,j),  &
  &                                      liney(counter-1:nlx,j),  &
  &                                      index=over_col(1), type=ltyp )
!write(*,*) "DrawLinex check21", linex(counter-1:nlx,j), counter-1, nlx
!write(*,*) "DrawLiney check21", liney(counter-1:nlx,j), counter-1, nlx
!write(*,*) "DrawLinez check21", valz(counter-1:nlx,j), counter-1, nlx
                    end if
                 else if(itmp==ncl+2)then
                    if(overcol_flag(2).eqv..true.)then
                       call DclDrawLine( linex(counter-1:nlx,j),  &
  &                                      liney(counter-1:nlx,j),  &
  &                                      index=over_col(2), type=ltyp )
!write(*,*) "DrawLinex check22", linex(counter-1:nlx,j), counter-1, nlx
!write(*,*) "DrawLiney check22", liney(counter-1:nlx,j), counter-1, nlx
!write(*,*) "DrawLinez check22", valz(counter-1:nlx,j), counter-1, nlx
                    end if
                 else if(itmp>1.and.itmp<ncl+2)then
                    call DclDrawLine( linex(counter-1:nlx,j),  &
  &                                   liney(counter-1:nlx,j),  &
  &                                   index=c_idx(itmp), type=ltyp )
!write(*,*) "DrawLinex check23", linex(counter-1:nlx,j), counter-1, nlx
!write(*,*) "DrawLiney check23", liney(counter-1:nlx,j), counter-1, nlx
!write(*,*) "DrawLinez check23", valz(counter-1:nlx,j), counter-1, nlx
                 end if
              case ('p')
                 if(itmp==1)then
                    if(overcol_flag(1).eqv..true.)then
!write(*,*) "tmpcheck1", over_col(1), itmp
!write(*,*) "tmplcheck1", linex(counter:nlx,j)
                       call DclDrawMarker( linex(counter:nlx,j),  &
  &                                        liney(counter:nlx,j),  &
  &                                        index=over_col(1),  &
  &                                        type=mtyp, height=msiz )
                    end if
                 else if(itmp==ncl+2)then
                    if(overcol_flag(2).eqv..true.)then
!write(*,*) "tmpcheck1", over_col(2), itmp
!write(*,*) "tmplcheck1", linex(counter:nlx,j)
                       call DclDrawMarker( linex(counter:nlx,j),  &
  &                                        liney(counter:nlx,j),  &
  &                                        index=over_col(2),  &
  &                                        type=mtyp, height=msiz )
                    end if
                 else if(itmp>1.and.itmp<ncl+2)then
!write(*,*) "tmpcheck1", c_idx(itmp), itmp
!write(*,*) "tmplcheck1", linex(counter:nlx,j)
                    call DclDrawMarker( linex(counter:nlx,j), liney(counter:nlx,j),  &
  &                              index=c_idx(itmp), type=mtyp, height=msiz )
                 end if
              end select
           end if
           if(itmp==2.and.tmp_i==-1)then
              counter=i+1
           end if
           itmp=tmp_i
        end do

     else

        if(itmp>-1)then
           select case (sub_idx)
           case ('s')
              call DclScalingPoint( linex(1:1,j), liney(1:1,j) )
              write(*,*) "*** WARNING *** (color_line:dcl_automatic)"
              write(*,*) "the array number of each line or marker is 1."
           case ('p')
              if(itmp==1)then
                 if(overcol_flag(1).eqv..true.)then
!write(*,*) "tmpcheck1", over_col(1), itmp
!write(*,*) "tmplcheck1", linex(1,j)
                    call DclDrawMarker( linex(1:1,j), liney(1:1,j),  &
  &                                   index=over_col(1), type=mtyp, height=msiz )
                 end if
              else if(itmp==ncl+2)then
                 if(overcol_flag(2).eqv..true.)then
!write(*,*) "tmpcheck1", over_col(2), itmp
!write(*,*) "tmplcheck1", linex(1,j)
                    call DclDrawMarker( linex(1:1,j), liney(1:1,j),  &
  &                                   index=over_col(2), type=mtyp, height=msiz )
                 end if
              else if(itmp>1.and.itmp<ncl+2)then
!write(*,*) "tmpcheck1", c_idx(itmp), itmp
!write(*,*) "tmplcheck1", linex(1,j)
                 call DclDrawMarker( linex(1:1,j), liney(1:1,j),  &
  &                                index=c_idx(itmp), type=mtyp, height=msiz )
              end if
           end select
        end if
     end if
  end do

end subroutine color_line

!---------------------------------------------------
!---------------------------------------------------

subroutine val_estimate( c_val, val, idx )
! val が c_val のどの範囲に存在するかを求める.
  implicit none
  real, intent(in) :: c_val(:)
  real, intent(in) :: val
  integer, intent(inout) :: idx
  integer :: i

  idx=0

  do i=1,size(c_val)
     if(c_val(i)<=val)then
        idx=i
     else
        exit
     end if
  end do

end subroutine val_estimate

!---------------------------------------------------
!---------------------------------------------------

subroutine calc_vscale( length, v_length, vx_scale, vy_scale )
  ! 風速ベクトルを描画アスペクト比に合わせるための V 座標系における単位ベクトル
  ! を計算するルーチン. x 方向の値を指定し, そのときの y 方向のスケールを決める.
  ! 計算方法は以下のとおり.
  ! U 座標系で (uu, uv) のベクトルを V 座標系で (vu,vv) にしたい.
  ! (vu, vv)=(vx_scale*uu, vy_scale*uv) という関係をもつ.
  ! 一方, グラフの描画領域の幅を U, V 座標系でそれぞれ ux, uy, vx, vy とすると,
  ! x 方向を基準に y 方向の伸縮を決めるとき,
  ! v 座標系では, vy/vx=v_asp 倍だけ y 方向ベクトルにかけ,
  ! u 座標系では, 1/(uy/ux)=1/u_asp 倍だけ y 方向ベクトルにかけるので,
  ! (vu, vv)=(vx_scale*uu, (v_asp/u_asp)*vy_scale*vy) という関係をもてばよい.
  ! これについての詳しい概念図は Tex ファイル参照.
  ! よって, vx_scale, vy_scale が同じ比率で変化するとき,
  ! (つまり, 風速ベクトルとして変化するとき)
  ! vy_scale=vx_scale*v_asp*u_asp となる.
  implicit none
  real, intent(in) :: length(2)  ! 描画距離 [m]
  real, intent(in) :: v_length(2)  ! V 系での描画範囲
  real, intent(in) :: vx_scale  ! x 方向のスケーリングファクター
  real, intent(inout) :: vy_scale  ! y 方向のスケーリングファクター
!-- 以上, 引数
  real :: x_length  ! 横方向の描画距離 [m]
  real :: y_length  ! 縦方向の描画距離 [m]
  real :: vx_length  ! 縦方向の V 系での描画範囲
  real :: vy_length  ! 横方向の V 系での描画範囲
!-- 以上, 引数の置き換え用変数
  real :: u_asp, v_asp

!-- 引数の置き換え用変数に置き換え
  x_length=length(1)
  y_length=length(2)
  vx_length=v_length(1)
  vy_length=v_length(2)

  u_asp=y_length/x_length
  v_asp=vy_length/vx_length

  vy_scale=(v_asp/u_asp)*vx_scale

end subroutine calc_vscale

!---------------------------------------------------
!---------------------------------------------------

subroutine DclDrawAxisCalendarYMDH( cmode, shour, nh )
  implicit none
  character(1), intent(in) :: cmode  ! 描画モード 'bt', 'rl'
  integer, intent(in) :: shour  ! 開始時間 [hour]
  integer, intent(in) :: nh  ! 描画時間 [hour]
                             ! nh = 1 なら, shour から 1 時間だけ描画
  integer :: cont1, cont2, conts1, conts2, conte1, conte2, i
  real :: dx1, dx2
  real, allocatable, dimension(:) :: u1, u2
  character(2), allocatable, dimension(:) :: c2

  if(nh<=12)then
     dx1=0.5
     dx2=1.0
  else if(nh>12.and.nh<=24)then
     dx1=1.0
     dx2=2.0
  else if(nh>24.and.nh<=120)then
     dx1=3.0
     dx2=6.0
  else
     dx1=6.0
     dx2=12.0
  end if

  if(mod(shour+nh,int(dx2))==0)then
     conte2=(shour+nh)/int(dx2)+1
     conte1=conte2-1
  else
     conte2=(shour+nh)/int(dx2)
     if(real(conte2)*dx2+dx1<real(shour+nh))then
        conte1=conte2+1
     else
        conte1=conte2
     end if
  end if

  if(mod(shour,int(dx2))==0)then
     conts2=shour/int(dx2)+1
     conts1=conts2-1
  else
     conts2=shour/int(dx2)
     if(real(conts2)*dx2+dx1<real(shour))then
        conts1=conts2+1
     else
        conts1=conts2
     end if
  end if

  allocate(u1(conte1-conts1+1))
  allocate(u2(conte2-conts2+1))
  allocate(c2(conte2-conts2+1))
  cont1=0
  cont2=0

  do i=shour,nh
     if(mod(i,int(dx2))==0)then
        cont2=cont2+1
        if(cont2>conte2-conts2+1)then
           write(*,*) "*** WARNING (dcl_auto) ***: Overflow of cont2"
           exit
        end if
        u2(cont2)=real(i)
        write(c2(cont2),'(i2)') mod(i,24)
     else
        if(mod(i,int(dx1))==0)then
           cont1=cont1+1
           if(cont1>conte1-conts1+1)then
              write(*,*) "*** WARNING (dcl_auto) ***: Overflow of cont1"
              exit
           end if
           u1(cont1)=real(i)
        end if
     end if
  end do

  select case(cmode(1:1))
  case ('r','l')
     call UYAXLB( cmode(1:1), u1, conte1-conts1+1, u2,  &
  &               c2, 2, conte2-conts2+1 )
  case ('b','t')
     call UXAXLB( cmode(1:1), u1, conte1-conts1+1, u2,  &
  &               c2, 2, conte2-conts2+1 )
  end select

  deallocate(u1)
  deallocate(u2)
  deallocate(c2)

end subroutine DclDrawAxisCalendarYMDH

!---------------------------------------------------
!---------------------------------------------------

subroutine auto_label_fmt( ival, mval, forma, axmin )  ! 数値ラベル用自動フォーマット作成ルーチン
  implicit none
  real, intent(in) :: ival  ! 目盛りの間隔
  real, intent(in) :: mval  ! 描画軸の最大値
  character(*), intent(out) :: forma
  real, intent(in), optional :: axmin  ! 軸値の最小値 (負号をつけるかの判定)
  integer :: decimal_ord, full_ord, nega_opt
  real :: tmp_mitv
  character(100) :: cord, fhead, ffoot

  forma=''
  nega_opt=0

  if(present(axmin))then
     if(axmin<0.0)then
        nega_opt=1
     end if
  end if

  if(real(int(ival))/=ival)then  ! 目盛りが小数点以下を含むか
     tmp_mitv=ival*10.0
     do while (tmp_mitv<1.0)
        tmp_mitv=tmp_mitv*10.0
     end do
     tmp_mitv=tmp_mitv*10.0
     do while (mod(int(tmp_mitv),10)/=0)
        tmp_mitv=tmp_mitv*10.0
     end do
     decimal_ord=int(tmp_mitv/ival)/100  ! 小数点での桁数
     write(cord,*) decimal_ord
     fhead='(f'
     ffoot='.'//trim(adjustl(cord))//')'
     decimal_ord=decimal_ord+1  ! +1 は小数点分の文字数
  else
     decimal_ord=1  ! +1 は小数点分の文字数
     fhead='(f'
!     fhead='(i'
     ffoot='.0)'
!     ffoot=')'
  end if

  write(cord,*) int(mval)
  full_ord=decimal_ord+len_trim(adjustl(cord))+nega_opt
  write(cord,*) full_ord
  forma=trim(adjustl(fhead))//trim(adjustl(cord))//trim(adjustl(ffoot))

end subroutine auto_label_fmt

!---------------------------------------------------
!---------------------------------------------------

subroutine format_make( val_type, order_num, form_name, frac_num )  ! 数値ラベル用フォーマット作成ルーチン
  implicit none
  character(1), intent(in) :: val_type  ! ラベル化する変数の型 : f = 実数(オプションも指定する), i = 整数
  character(1), intent(in) :: order_num  ! 表示する桁数
  character(1), intent(in), optional :: frac_num  ! 実数指定のときのみ, 小数桁
  character(*), intent(out) :: form_name

  select case(val_type)
  case('f')
     form_name='('//val_type//order_num//'.'//frac_num//')'
     form_name=trim(form_name)
  case('F')
     form_name='('//val_type//order_num//'.'//frac_num//')'
     form_name=trim(form_name)
  case('i')
     form_name='('//val_type//order_num//')'
     form_name=trim(form_name)
  case('I')
     form_name='('//val_type//order_num//')'
     form_name=trim(form_name)
  end select

end subroutine format_make

!-- 以下, 矢羽パッケージ WVERBS

subroutine wverbd( vs, vd, vox, voy )
! V 座標系で 1 つの矢羽を描く.
! 風速 vs (単位 knot) と風向 vd (単位 deg) で
! 現在, デカルト座標系のみ対応.
! vd は北を 0 (360) deg として時計回りにとる.
! 方向は風が「吹いてくる」方向.
! つまり, 北風なら, vd = 0.0.
! DCL への移植を考慮して階層構造で表現する.
! このルーチンを直接使用することは多分ないはず.
  implicit none
  real, intent(in) :: vs  ! wind speed [knot]
  real, intent(in) :: vd  ! wind direction [deg]
  real, intent(in) :: vox  ! the origin of x-direction [V-coord.]
  real, intent(in) :: voy  ! the origin of y-direction [V-coord.]
  real, parameter :: vfact=0.05, pi=3.14159265, verbangle=120.0
  real, parameter, dimension(4) :: verbel=(/2.0, 5.0, 10.0, 50.0 /)
  integer :: i, counter, vounter
  integer, dimension(4) :: iblev
  logical, parameter :: verbflag=.true.  ! 旗 or 複数羽 (NOTE "counter")
  real :: vx, vy, vp, vinterval, vwidth, coe, verbi, vcoe
  real, dimension(30) :: vxtraj, vytraj
  real, dimension(10) :: vvxtraj, vvytraj

  iblev=(/0,0,0,0/)
  vinterval=0.5*vfact*0.2
  vwidth=0.5*vfact
  coe=pi/180.0
  verbi=180.0-verbangle
  vcoe=(vd+verbi)*coe

  vx=sin(vd*coe)*vfact
  vy=cos(vd*coe)*vfact
  vp=vs

!-- 何本, 何種類の羽を描くか決定

  if(verbflag.eqv..true.)then
     do while (vp>verbel(4))
        iblev(4)=iblev(4)+1
        vp=vp-verbel(4)
     end do
  end if

  do while (vp>verbel(3))
     iblev(3)=iblev(3)+1
     vp=vp-verbel(3)
  end do
  do while (vp>verbel(2))
     iblev(2)=iblev(2)+1
     vp=vp-verbel(2)
  end do

!-- 描く線の軌跡を計算.

  vxtraj(1)=vox
  vytraj(1)=voy
  vxtraj(2)=vxtraj(1)+vx
  vytraj(2)=vytraj(1)+vy
  vvxtraj(1)=vxtraj(2)+vx
  vvytraj(1)=vytraj(2)+vy

  counter=2
  vounter=1

  if(iblev(4)>0)then
     do i=1,iblev(4)
        counter=counter+1
        vounter=vounter+1
        vxtraj(counter)=vxtraj(counter-1)+sin(vcoe)*vwidth
        vytraj(counter)=vytraj(counter-1)+cos(vcoe)*vwidth
        vvxtraj(vounter)=vvxtraj(vounter-1)+sin(vcoe)*vwidth
        vvytraj(vounter)=vvytraj(vounter-1)+cos(vcoe)*vwidth
        counter=counter+1
        vounter=vounter+1
        vxtraj(counter)=vxtraj(counter-2)-sin(vd*coe)*vinterval
        vytraj(counter)=vytraj(counter-2)-cos(vd*coe)*vinterval
        vvxtraj(vounter)=vvxtraj(vounter-2)-sin(vd*coe)*vinterval
        vvytraj(vounter)=vvytraj(vounter-2)-cos(vd*coe)*vinterval
        vounter=vounter+1
        vvxtraj(vounter)=vvxtraj(vounter-3)
        vvytraj(vounter)=vvytraj(vounter-3)
     end do
  end if

  if(iblev(3)>0)then
     do i=1,iblev(3)
        counter=counter+1
        vxtraj(counter)=vxtraj(counter-1)+sin(vcoe)*vwidth
        vytraj(counter)=vytraj(counter-1)+cos(vcoe)*vwidth
        counter=counter+1
        vxtraj(counter)=vxtraj(counter-2)
        vytraj(counter)=vytraj(counter-2)
        counter=counter+1
        vxtraj(counter)=vxtraj(counter-1)-sin(vd*coe)*vinterval
        vytraj(counter)=vytraj(counter-1)-cos(vd*coe)*vinterval
     end do
  end if

  if(iblev(2)>0)then
     do i=1,iblev(2)
        counter=counter+1
        if(counter==3)then
           vxtraj(counter)=vxtraj(counter-1)-sin(vd*coe)*vinterval
           vytraj(counter)=vytraj(counter-1)-cos(vd*coe)*vinterval
           counter=counter+1
           vxtraj(counter)=vxtraj(counter-1)+sin(vcoe)*vwidth*0.5
           vytraj(counter)=vytraj(counter-1)+cos(vcoe)*vwidth*0.5
        else
           vxtraj(counter)=vxtraj(counter-1)+sin(vcoe)*vwidth*0.5
           vytraj(counter)=vytraj(counter-1)+cos(vcoe)*vwidth*0.5
        end if
     end do
  end if

  if(iblev(4)>0)then
     call SGTNZV( vounter, vvxtraj(1:vounter), vvytraj(1:vounter), 999 )
  end if

  call SGPLV( counter, vxtraj(1:counter), vytraj(1:counter) )

!write(*,*) "check", vounter, vvxtraj(1:vounter), vvytraj(1:vounter)

end subroutine wverbd

subroutine wvrbxy( ux, uy, vx, vy )
!-- For Cartesian
  implicit none
  real, intent(in) :: ux  ! wind component of x-coord [knot]
  real, intent(in) :: uy  ! wind component of y-coord [knot]
  real, intent(in) :: vx  ! the origin of x [vcoord]
  real, intent(in) :: vy  ! the origin of y [vcoord]
  real, parameter :: pi=3.141592653
  real :: vs, vd, rcoe

  rcoe=180.0/pi
  vs=sqrt(ux*ux+uy*uy)

  if(vs/=0.0)then
     if(uy==0.0)then
        vd=acos(ux/vs)*rcoe+180.0
     else
        vd=asin(uy/vs)*rcoe+180.0
     end if
     call wverbd( vs, vd, vx, vy )
  end if

end subroutine wvrbxy

end module
