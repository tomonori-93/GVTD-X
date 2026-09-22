  !-- parameters
  integer, parameter :: nrdivmax=1000  !! the maximum number of radial grids for the divergent
  integer, parameter :: out_fnum(2)=(/100, 101/)
                        !! unit numbers for output files <br>
                        !! 1: .srVM file (storm-relative mean wind)
                        !! 2: GVTDX.ctl file (GrADS control file)
  character(1000) :: listname  !! ASCII file name listing file name and other information
  logical :: out2d_flag        !! Flag for output of 2d (r-z) data
  double precision, intent(in) :: rmin     !! the innermost radius where Doppler velocity is defined in the original data (m)
  double precision, intent(in) :: dr       !! the radial grid spacing where Doppler velocity is defined (m)
  double precision, intent(in) :: tmin     !! the start of the azimuthal angle where Doppler velocity is defined in the original data (deg)
  double precision, intent(in) :: dt       !! the azimuthal angle interval where Doppler velocity is defined (deg)
  double precision, intent(in) :: zmin     !! the bottom height where Doppler velocity is defined in the original data (m)
  double precision, intent(in) :: dz       !! the vertical grid spacing where Doppler velocity is defined (m)
  integer, intent(in) :: nnz(2)            !! calculating vertical grid levels
  integer, intent(in) :: nr_org            !! the radial grid number of the original data
  integer, intent(in) :: nt_org            !! the azimuthal grid number of the original data
  integer, intent(in) :: nz                !! the vertical grid number of the original data
  integer, intent(in) :: nr                !! resampled radial grid number from the original data
  integer, intent(in) :: nt                !! resampled azimuthal grid number from the original data
  logical :: stdflag
!  logical :: vec_flag, stdflag
  character(200) :: input_fname, output_fname, output2d_fname
  character(200), allocatable, dimension(:,:) :: cval
  character(10000) :: tmpchar


!-- read namelist file
  namelist /iocheck /listname, out2d_flag, nr_org, nt_org, nz,  &
  &                  rmin, dr, tmin, dt, zmin, dz
  namelist /ret_opt /nrot, ndiv, nrdiv, rdiv_t, missing_value, nthres_undef,  &
  &                  nr, nt, nnz, skip_min_t, flag_GVTDX,  &
  &                  flag_datagap
  namelist /rad_opt /dpoint_x, dpoint_y
  read(5,nml=iocheck)
  read(5,nml=ret_opt)
  read(5,nml=rad_opt)

  allocate(theta_ref_t(nt))
  allocate(lond(nr,nt))
  allocate(latd(nr,nt))
  allocate(Vra_in(nr_org,nt_org,nz))
  allocate(umd(nz))
  allocate(vmd(nz))
  allocate(VTtot(nr,nt,nnz(1):nnz(2)))
  allocate(VRtot(nr,nt,nnz(1):nnz(2)))
  allocate(VTtot_Er(nr,nt,nnz(1):nnz(2)))
  allocate(VRtot_Er(nr,nt,nnz(1):nnz(2)))
  allocate(Uxtot_Er(nr,nt,nnz(1):nnz(2)))
  allocate(Vytot_Er(nr,nt,nnz(1):nnz(2)))
  allocate(Wstot_Er(nr,nt,nnz(1):nnz(2)))
  allocate(VRT0(nr,nt,nnz(1):nnz(2)))
  allocate(VDR0(nr,nt,nnz(1):nnz(2)))
  allocate(zetatot(nr,nt,nnz(1):nnz(2)))
  allocate(zeta0(nr,nt,nnz(1):nnz(2)))
  allocate(VRTn(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VRRn(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(VDTm(ndivmin:ndiv,nr,nt,nnz(1):nnz(2)))
  allocate(VDRm(ndivmin:ndiv,nr,nt,nnz(1):nnz(2)))
  allocate(phin(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(zetan(nrotmin:nrot,nr,nt,nnz(1):nnz(2)))
  allocate(Vn_0(nr,nt,nnz(1):nnz(2)))
  allocate(Vra(nr,nt,nnz(1):nnz(2)))
  allocate(Vra_ret(nr,nt,nnz(1):nnz(2)))
  allocate(Vra_Er(nr,nt,nnz(1):nnz(2)))
  allocate(Vra_Er_ret(nr,nt,nnz(1):nnz(2)))

  allocate(thetad_t(nr,nt))
  allocate(lonr(nr,nt))
  allocate(latr(nr,nt))
  allocate(lond_sph(nr,nt))
  allocate(latd_sph(nr,nt))
  allocate(lonr_sph(nr,nt))
  allocate(latr_sph(nr,nt))
  allocate(projVs(nr,nt))
  allocate(projVm(nr,nt))
  allocate(dval(nr_org,nt_org))
  allocate(Vra_rt_t(nr,nt))
  allocate(projVRs_rt_t(nr,nt))
  allocate(projVTs_rt_t(nr,nt))
  allocate(projVRm_rt_t(nr,nt))
  allocate(projVTm_rt_t(nr,nt))
  allocate(VTtot_rt_t(nr,nt))
  allocate(VRtot_rt_t(nr,nt))
  allocate(VRT0_rt_t(nr,nt))
  allocate(VDR0_rt_t(nr,nt))
  allocate(VRTn_rt_t(nrotmin:nrot,nr,nt))
  allocate(VRRn_rt_t(nrotmin:nrot,nr,nt))
  allocate(VDTm_rt_t(ndivmin:ndiv,nr,nt))
  allocate(VDRm_rt_t(ndivmin:ndiv,nr,nt))
  allocate(phin_rt_t(nrotmin:nrot,nr,nt))
  allocate(zetan_rt_t(nrotmin:nrot,nr,nt))
  allocate(Vn_0_rt_t(nr,nt))
  allocate(rval(nr,nt,nz))
  allocate(rval2d(nr,nz))
  allocate(undef_grid(nr,nt))
  allocate(undef_grid_2d(nr,nz))
  allocate(nr_grid_skp(nr))
  allocate(r_skp_t(nr))
  allocate(rh_skp_t(nr+1))
  allocate(thetad_skp_t(nr,nt))
  allocate(Vra_skp_rt_t(nr,nt))

  stdflag=.true.

  if(stdflag.eqv..true.)then
     write(*,*) "### MESSAGE (main) ### : "
     write(*,*) "In interpolation routines, stdopt is set as true."
     write(*,*) "This means that error is not output."
  end if

  r_t_org=(/((rmin+dr*real(i-1)),i=1,nr_org)/)
  theta_ref_t_org=(/((tmin+dt*real(i-1)),i=1,nt_org)/)
  theta_ref_t_org=theta_ref_t_org*d2r
  do j=1, nr
     r_t(j)=r_t_org(1)+(r_t_org(nr_org)-r_t_org(1))*(dble(j-1)/dble(nr-1))
  end do

  do j=1, nt
     theta_ref_t(j)=theta_ref_t_org(1)  &
  &               +(theta_ref_t_org(nt_org)-theta_ref_t_org(1))*(dble(j-1)/dble(nt-1))
  end do

!-- Loop for time

  do i=1,nl
     !-- Read the vortex center position on lon-lat
     lon_tc=dble( c2r_convert( trim(adjustl(cval(3,i))) ) )
     lat_tc=dble( c2r_convert( trim(adjustl(cval(4,i))) ) )

     !-- Read the motion vector of the vortex
     usp=dble(c2r_convert( trim(adjustl(cval(5,i))) ))
     vsp=dble(c2r_convert( trim(adjustl(cval(6,i))) ))

     !-- Read the mean wind at each height
     do k=1,nz
        umd(k)=dble(c2r_convert( trim(adjustl(cval(6+2*(k-1)+1,i))) ))
        vmd(k)=dble(c2r_convert( trim(adjustl(cval(6+2*k,i))) ))
     end do

     input_fname=trim(adjustl(cval(1,i)))
     output_fname=trim(adjustl(input_fname))//'.GVTDX'
     write(*,*) trim(adjustl(input_fname))

     !-- Read Doppler velocity
     call read_file_3d( trim(adjustl(input_fname)), nr_org, nt_org, nz, 1, rval_org )

        !-- B. do interpolation from nr_org, nt_org to nr, nt
        call auto_interpolation_2d( r_t_org, theta_ref_t_org,  &
  &                                 r_t, theta_ref_t, dval, Vra_rt_t,  &
  &                                 undef=undef, stdopt=stdflag )

  ntz=nnz(2)-nnz(1)+1

!-- Read vortex center, storm motion, and mean wind

  nl=line_number_counter( trim(adjustl(listname)) )-2
  tccol=6+2*nz  ! fname, time, TC-lon, TC-lat, Uxs, Vxs, U1m, V1m,... Unzm, Vnzm

  allocate(cval(tccol,nl))
  allocate(ttime(nl))

  call read_file_text( trim(adjustl(listname)), tccol, nl, cval, skip=2 )
  do i=1,nl
     ttime(i)=c2r_convert( trim(adjustl(cval(2,i))) )
  end do

!-- ASCII output for normal component to the direction of the radar to the vortex center of storm-relative mean wind
  open(unit=out_fnum(1),file=trim(adjustl(listname))//'.srVM',status='unknown')
  write(out_fnum(1),'(a32)') "'Time'          'Vm-SR'         "
  write(out_fnum(1),'(a32)') "'s'             'ms-1'          "

!-- Output GrADS control file for *.GVTDX files
  open(unit=out_fnum(2),file='GVTDX.ctl',status='unknown')
  call write_file_text_add( out_fnum(2), "title GVTDX output file (please edit filenames and time)" )
  call write_file_text_add( out_fnum(2), "undef "//trim(adjustl(r2c_convert(real(undef)))) )
  call write_file_text_add( out_fnum(2), "options big_endian template" )
  call write_file_text_add( out_fnum(2), "xdef "//trim(adjustl(i2c_convert(nr)))  &
  &                         //" LINEAR "//trim(adjustl(r2c_convert(real(rmin))))//" "  &
  &                         //trim(adjustl(r2c_convert(real(dr)*real(nr_org)/real(nr)))) )
  call write_file_text_add( out_fnum(2), "ydef "//trim(adjustl(i2c_convert(nt)))  &
  &                         //" LINEAR "//trim(adjustl(r2c_convert(real(tmin))))//" "  &
  &                         //trim(adjustl(r2c_convert(real(dt)*real(nt_org)/real(nt)))) )
  call write_file_text_add( out_fnum(2), "zdef "//trim(adjustl(i2c_convert(ntz)))  &
  &                         //" LINEAR "//trim(adjustl(r2c_convert(real(zmin+dz*real(nnz(1)-1)))))//" "  &
  &                         //trim(adjustl(r2c_convert(real(dz)))) )
  call write_file_text_add( out_fnum(2), "tdef "//trim(adjustl(i2c_convert(nl)))  &
  &                         //" LINEAR 00Z00JAN0000 10mn" )
  call write_file_text_add( out_fnum(2), "* vars XX (replace XX with the line after 'endvars'" )

     !-- 5. output to binary file (float)
     irec=1
     nval=0
     call conv_d2r_3d( VTtot(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='replace' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "VTtot "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved total tangential wind [m s-1]" )
     
     call conv_d2r_3d( VRtot(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "VRtot "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved total radial wind [m s-1]" )

     call conv_d2r_3d( VRT0(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "VRT0 "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved axisymmetric tangential wind [m s-1]" )

     call conv_d2r_3d( VDR0(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "VDR0 "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved axisymmetric radial wind [m s-1]" )

     call conv_d2r_3d( Vra(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "Vra "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"input Doppler velocity [m s-1]" )

     call conv_d2r_3d( Vra_ret(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "Vrar "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved Doppler velocity [m s-1]" )

     if(nrot>0)then
        do k=1,nrot
           call conv_d2r_3d( VRTn(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "VRT"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" rotational-tangential wind [m s-1]" )

           call conv_d2r_3d( VRRn(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "VRR"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" rotational-radial wind [m s-1]" )

           call conv_d2r_3d( phin(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "phi"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" streamfunction [m2 s-1]" )

           call conv_d2r_3d( zetan(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "zeta"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" vorticity [s-1]" )
        end do
     end if

     if(ndiv>0)then
        do k=1,ndiv
           call conv_d2r_3d( VDTm(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "VDT"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" divergent-tangential wind [m s-1]" )

           call conv_d2r_3d( VDRm(k,1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
           call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                            rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
           irec=irec+ntz
           nval=nval+1
           if(i==1) call write_file_text_add( out_fnum(2),  &
  &                                     "VDR"//trim(adjustl(i2c_convert(k)))//" "  &
  &                                     //trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                                     //"retrieved wavenumber-"//trim(adjustl(i2c_convert(k)))  &
  &                                     //" divergent-radial wind [m s-1]" )
        end do
     end if

!-- optional output variables
     call conv_d2r_3d( Vn_0(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Vn0 "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"storm-relative mean wind normal to line of sight [m s-1]" )
     call conv_d2r_3d( Vra_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Vrae "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative Doppler velocity [m s-1]" )
     call conv_d2r_3d( Vra_Er_ret(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Vrare "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved Doppler velocity [m s-1]" )
     call conv_d2r_3d( VTtot_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "VTtote "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved total tangential wind [m s-1]" )
     call conv_d2r_3d( VRtot_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "VRtote "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved total radial wind [m s-1]" )
     call conv_d2r_3d( Uxtot_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Uxtote "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved total zonal wind [m s-1]" )
     call conv_d2r_3d( Vytot_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Vytote "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved total meridional wind [m s-1]" )
     call conv_d2r_3d( Wstot_Er(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Wstote "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"earth-relative retrieved total wind speed [m s-1]" )
!-- 2d monitor (lat/lon)
     call conv_d2r_2d( latd_sph(1:nr,1:nt), rval(1:nr,1:nt,nnz(1)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, 1, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(1)), mode='old' )
     irec=irec+1
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "latd 0 99 latitude [degree]" )
     call conv_d2r_2d( lond_sph(1:nr,1:nt), rval(1:nr,1:nt,nnz(1)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, 1, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(1)), mode='old' )
     irec=irec+1
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2), "lond 0 99 longitude [degree]" )

!-- additional output variables for analyses
     call conv_d2r_3d( zeta0(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Zeta0 "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"retrieved axisymmetric vorticity [s-1]" )
     call conv_d2r_3d( zetatot(1:nr,1:nt,nnz(1):nnz(2)), rval(1:nr,1:nt,nnz(1):nnz(2)) )
     call write_file_3d( trim(adjustl(output_fname)), nr, nt, ntz, irec,  &
  &                      rval(1:nr,1:nt,nnz(1):nnz(2)), mode='old' )
     irec=irec+ntz
     nval=nval+1
     if(i==1) call write_file_text_add( out_fnum(2),  &
  &                               "Zetatot "//trim(adjustl(i2c_convert(ntz)))//" 99 "  &
  &                               //"retrieved total vorticity [s-1]" )

!!-- NetCDF output
!
!     call HistoryPut( trim(adjustl(valc)), varbar(1:nr,nnz(1):nnz(2)) )

     write(*,*) "Writing the data at "//trim(adjustl(output_fname))

     !-- (5.1) output to 2d binary file (float)
     if(out2d_flag.eqv..true.)then
        output2d_fname=trim(adjustl(output_fname))//'.2d'
        irec=1
        call conv_d2r_2d( VRT0(1:nr,1,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
        call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                         rval2d(1:nr,nnz(1):nnz(2)), mode='replace' )
        irec=irec+1
        call conv_d2r_2d( VDR0(1:nr,1,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
        call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                         rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
        irec=irec+1

        if(nrot>0)then
           do k=1,nrot
              call conv_d2r_2d( zetans_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
              call conv_d2r_2d( zetanc_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
              call conv_d2r_2d( VRTns_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
              call conv_d2r_2d( VRTnc_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
              call conv_d2r_2d( VRRns_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
              call conv_d2r_2d( VRRnc_2d(k,1:nr,nnz(1):nnz(2)), rval2d(1:nr,nnz(1):nnz(2)) )
              call write_file_2d( trim(adjustl(output2d_fname)), nr, ntz, irec,  &
  &                               rval2d(1:nr,nnz(1):nnz(2)), mode='old' )
              irec=irec+1
           end do
        end if

        write(*,*) "Writing the data at "//trim(adjustl(output2d_fname))

     end if

  call write_file_text_add( out_fnum(2), "vars "//trim(adjustl(i2c_convert(nval))))

  write(*,*) "Output CTL file: GVTDX.ctl"

  close(unit=out_fnum(1))
  close(unit=out_fnum(2))
