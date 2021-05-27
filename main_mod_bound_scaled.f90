module main_mod

  use matrix_calc
  use sub_mod

contains

subroutine Retrieve_velocity( nrot, ndiv, r, t, rh, td, Vd, Vn, VT, VR,  &
  &                           VRT0, VDR0, VRTn, VRRn, VDTm, VDRm, undef )
!-- solve unknown variables and return wind velocity on R-T coordinates.
  implicit none
  !-- input/output
  integer, intent(in) :: nrot  ! wave number for rotating wind
  integer, intent(in) :: ndiv  ! wave number for divergent wind
  double precision, intent(in) :: r(:)   ! radial coordinate on which Vd is defined [m]
  double precision, intent(in) :: t(:)   ! azimuthal coordinate on which Vd is defined [rad]
  double precision, intent(in) :: rh(size(r)+1)  ! radial coordinate on which Phi (staggered for Vd) is defined [m]
  double precision, intent(in) :: td(size(r),size(t))  ! radar azimuthal angle defined at Vd(r,t) [rad]
  double precision, intent(inout) :: Vd(size(r),size(t))  ! Doppler velocity defined on r-t [m s-1]
  double precision, intent(in) :: Vn                   ! Normal component to radar in environmental wind, defined on r-t [m s-1]
  double precision, intent(out) :: VT(size(r),size(t))  ! retrieved total tangential wind [m s-1]
  double precision, intent(out) :: VR(size(r),size(t))  ! retrieved total radial wind [m s-1]
  double precision, intent(out) :: VRT0(size(r),size(t))  ! retrieved axisymmetric radial component of rotating wind [m s-1]
  double precision, intent(out) :: VDR0(size(r),size(t))  ! retrieved axisymmetric tangential component of divergent wind [m s-1]
  double precision, intent(out) :: VRTn(nrot,size(r),size(t))  ! retrieved tangential component of rotating wind [m s-1]
  double precision, intent(out) :: VRRn(nrot,size(r),size(t))  ! retrieved radial component of rotating wind [m s-1]
  double precision, intent(out) :: VDTm(ndiv,size(r),size(t))  ! retrieved tangential component of divergent wind [m s-1]
  double precision, intent(out) :: VDRm(ndiv,size(r),size(t))  ! retrieved radial component of divergent wind [m s-1]
  double precision, intent(in), optional :: undef  ! undefined value for Vd

  !-- internal variables
  integer :: i, j, k, p, cstat  ! dummy indexes
  integer :: nr, nt  ! array numbers for r and t, respectively
  integer :: nk      ! array number of a_k
  double precision, allocatable, dimension(:) :: Vdivr_r    ! axisymmetric divergent wind (VDR0(r))
  double precision, allocatable, dimension(:) :: Vrott_r    ! axisymmetric rotating wind (VRT0(r))
  double precision, allocatable, dimension(:,:) :: phis_nr  ! asymmetric (sine) stream function (Phi_S(n,r))
  double precision, allocatable, dimension(:,:) :: phic_nr  ! asymmetric (cosine) stream function (Phi_C(n,r))
  double precision, allocatable, dimension(:,:) :: divs_mr  ! asymmetric (sine) stream function (D_S(n,r))
  double precision, allocatable, dimension(:,:) :: divc_mr  ! asymmetric (cosine) stream function (D_C(n,r))
  double precision, allocatable, dimension(:) :: x_k        ! unknown vector for retrieved coefficients
  double precision, allocatable, dimension(:) :: b_k        ! known vector given by observed values
  double precision, allocatable, dimension(:,:) :: a_kp     ! coefficient matrix for x_k
  double precision, allocatable, dimension(:,:,:) :: f_kij  ! a_kp = sum_{i,j}(f_kij * f_pij)
  double precision :: dundef, vmax
  double precision, dimension(size(r)) :: r_n               ! Nondimensional r
  double precision, dimension(size(rh)) :: rh_n             ! Nondimensional rh

  call stdout( "Enter procedure.", "Retrieve_velocity", 0 )

  nr=size(r)
  nt=size(t)
  vmax=50.0d0

  if(present(undef))then
     dundef=undef
  else
     dundef=-1.0e35
  end if

!-- Check retrieved asymmetric wave number
  if(nrot<0)then
     call stdout( "nrot is greater equal to 0. stop.", "Retrieve_velocity", -1 )
     stop
  end if
  if(ndiv<0)then
     call stdout( "ndiv is greater equal to 0. stop.", "Retrieve_velocity", -1 )
     stop
  end if

!-- Nondimensionalize r and rh
  r_n=r/r(nr)
  rh_n=rh/r(nr)

!-- Set total number for unknown variables in a_k
  nk=(2+2*nrot+2*ndiv)*nr

!-- Allocate and initialize arrays
  allocate(Vdivr_r(nr),stat=cstat)
  allocate(Vrott_r(nr),stat=cstat)
  allocate(phis_nr(nrot,nr),stat=cstat)
  allocate(phic_nr(nrot,nr),stat=cstat)
  allocate(divs_mr(ndiv,nr),stat=cstat)
  allocate(divc_mr(ndiv,nr),stat=cstat)
  allocate(x_k(nk),stat=cstat)
  allocate(b_k(nk),stat=cstat)
  allocate(a_kp(nk,nk),stat=cstat)
  allocate(f_kij(nk,nr,nt),stat=cstat)

  if(cstat/=0)then
     call stdout( "Failed to allocate variables. stop.", "Retrieve_velocity", -1 )
     stop
  end if

  Vdivr_r=0.0d0
  Vrott_r=0.0d0
  phis_nr=0.0d0
  phic_nr=0.0d0
  divs_mr=0.0d0
  divc_mr=0.0d0
  x_k=0.0d0
  b_k=0.0d0
  a_kp=0.0d0
  f_kij=0.0d0

!-- Calculate f_kij
  call calc_fkij( nrot, ndiv, nk, Vn, r_n, t, rh_n, td, f_kij, Vd )
write(*,*) "checkf", f_kij
stop

!-- Calculate b_k
  call calc_fkijVd2bk( vmax, f_kij, Vd, b_k )

!-- Calculate a_kp
  call calc_fkij2akp( f_kij, a_kp )

  call check_zero( a_kp )

!-- Solve x_k
do k=1,nk
write(*,*) k, b_k(k), a_kp(:,k)
end do
!  call tri_gauss( a_kp, b_k, x_k )
!  call gausss( a_kp, b_k, x_k )
  call fp_gauss( a_kp, b_k, x_k )
!  call SOR_Gau_Sei( a_kp, b_k, 1.0d-5, 1.0d0, x_k )

!-- Set each unknown variable from x_k
  call set_xk2variables( nrot, ndiv, x_k, Vrott_r, Vdivr_r,  &
  &                      phis_nr, phic_nr, divs_mr, divc_mr, undef=dundef )

!-- Calculate Vr and Vt components of rotating wind
  call calc_phi2Vrot( nrot, vmax, r(nr), r_n, rh_n, t, Vrott_r, VRT0, VRTn, VRRn, phis_nr, phic_nr, undef=dundef )

!-- Calculate Vr and Vt components of divergent wind
  call calc_D2Vdiv( ndiv, vmax, r(nr), r_n, rh_n, t, Vdivr_r, VDR0, VDTm, VDRm, divs_mr, divc_mr, undef=dundef )

!-- Calculate total retrieved Vr and Vt
  call calc_Vn2Vtot( nrot, ndiv, VRT0, VRTn, VDTm, VT )
  call calc_Vn2Vtot( nrot, ndiv, VDR0, VRRn, VDRm, VR )

  call stdout( "Finish procedure.", "Retrieve_velocity", 0 )

end subroutine Retrieve_velocity

!--------------------------------------------------
!-- calculate f_kij
!--------------------------------------------------

subroutine calc_fkij( nrot, ndiv, nnk, Vsrn, rd, theta, rdh, thetad, fkij, Vdij )
  implicit none
  integer, intent(in) :: nrot
  integer, intent(in) :: ndiv
  integer, intent(in) :: nnk
  double precision, intent(in) :: Vsrn
  double precision, intent(in) :: rd(:)
  double precision, intent(in) :: theta(:)
  double precision, intent(in) :: rdh(size(rd)+1)
  double precision, intent(in) :: thetad(size(rd),size(theta))
  double precision, intent(out) :: fkij(nnk,size(rd),size(theta))
  double precision, intent(inout) :: Vdij(size(rd),size(theta))

  !-- internal variables
  integer :: nnr, nnt, ii, jj, kk, pp, nmax, cstat, ncyc
  double precision :: dr, dr_inv, rmax, rmax_inv
  double precision, dimension(size(rd)) :: r_inv
  double precision, dimension(size(rd),size(theta)) :: sines, cosines
  double precision, allocatable, dimension(:,:) :: sinen, cosinen
  double precision, allocatable, dimension(:,:,:) :: gkrr

  call stdout( "Enter procedure.", "calc_fkij", 0 )

  nnr=size(rd)
  nnt=size(theta)
  ncyc=2+2*(nrot+ndiv)  ! unknown variable number at a certain radius
  nmax=max(max(0,nrot),ndiv)   ! maximum wave number for rotating and divergent components
  fkij=0.0d0

  if(nmax>0)then
     allocate(sinen(nmax,nnt),stat=cstat)
     allocate(cosinen(nmax,nnt),stat=cstat)
     allocate(gkrr(nmax,nnr+1,nnr+1),stat=cstat)  ! Gk(r_p,r), r_p at rdh, r at rd
     if(cstat/=0)then
        call stdout( "Failed to allocate variables. stop.", "calc_fkij", -1 )
        stop
     end if
     sinen=0.0d0
     cosinen=0.0d0
     gkrr=0.0d0
  end if

  dr=rd(2)-rd(1)
  dr_inv=1.0/dr
  do ii=1,nnr
     r_inv(ii)=1.0/rd(ii)
  end do
  rmax=rd(nnr)
  rmax_inv=1.0d0/rmax

!$omp parallel default(shared)

!-- Set fixed variables for R-T, in advance
!$omp do schedule(runtime) private(ii,jj)
  do jj=1,nnt
     do ii=1,nnr
        sines(ii,jj)=dsin(theta(jj)-thetad(ii,jj))
        cosines(ii,jj)=dcos(theta(jj)-thetad(ii,jj))
     end do
  end do
!$omp end do

!$omp barrier

  if(nmax>0)then

!$omp do schedule(runtime) private(kk,jj)
     do jj=1,nnt
        do kk=1,nmax
           sinen(kk,jj)=dsin(dble(kk)*theta(jj))
           cosinen(kk,jj)=dcos(dble(kk)*theta(jj))
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnr  ! For r
        do ii=1,nnr+1  ! For rh
           do kk=1,nmax
              gkrr(kk,ii,jj)=green_func( rdh(ii), rd(jj), kk )
           end do
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii)
     do ii=1,nnr+1  ! For rh
        do kk=1,nmax
           gkrr(kk,ii,nnr+1)=green_func( rdh(ii), rd(nnr)+dr, kk )
        end do
     end do
!$omp end do

  end if

!$omp barrier

!-- Set coefficients for VRT0 at each (ii,jj)
!$omp do schedule(runtime) private(ii,jj)
  do jj=1,nnt
     do ii=1,nnr
        fkij(1+ncyc*(ii-1),ii,jj)=-sines(ii,jj)
     end do
  end do
!$omp end do

!$omp barrier

!-- Set coefficients for VDR0 at each (ii,jj)
!$omp do schedule(runtime) private(ii,jj)
  do jj=1,nnt
     do ii=1,nnr
        fkij(2+ncyc*(ii-1),ii,jj)=cosines(ii,jj)
     end do
  end do
!$omp end do

!$omp barrier

  if(nrot>0)then

!-- Set coefficients for Phi_s and Phi_c at each (ii,jj)
!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnt
        do ii=2,nnr
           do kk=1,nrot
              !-- Phi(s)*delta(s,i)
              fkij(2+kk+ncyc*(ii-2),ii,jj)  &
  &          =(dr_inv*sinen(kk,jj))  &
  &           *(-sines(ii,jj))  &
  &          +(0.5d0*dble(kk)*cosinen(kk,jj))  &
  &           *(r_inv(ii)*cosines(ii,jj))

              fkij(2+nrot+kk+ncyc*(ii-2),ii,jj)  &
  &          =(dr_inv*cosinen(kk,jj))  &
  &           *(-sines(ii,jj))  &
  &          -(0.5d0*dble(kk)*sinen(kk,jj))  &
  &           *(r_inv(ii)*cosines(ii,jj))

              !-- Phi(s)*delta(s,i+1)
              fkij(2+kk+ncyc*(ii-1),ii,jj)  &
  &          =(dr_inv*sinen(kk,jj))  &
  &           *(sines(ii,jj))  &
  &          +(0.5d0*dble(kk)*cosinen(kk,jj))  &
  &           *(r_inv(ii)*cosines(ii,jj))

              fkij(2+nrot+kk+ncyc*(ii-1),ii,jj)  &
  &          =(dr_inv*cosinen(kk,jj))  &
  &           *(sines(ii,jj))  &
  &          -(0.5d0*dble(kk)*sinen(kk,jj))  &
  &           *(r_inv(ii)*cosines(ii,jj))
           end do
        end do
     end do
!$omp end do

!$omp barrier

!-- Set coefficients for Phi_s and Phi_c at the inner boundary (1,jj)
!-- 1. For wavenumber 1 (and adjust Vd)
!$omp do schedule(runtime) private(jj)
     do jj=1,nnt
        fkij(2+1,1,jj)  &
  &    =(2.0d0*dr_inv*sinen(1,jj))  &
  &     *(sines(1,jj))

        fkij(2+nrot+1,1,jj)  &
  &    =(2.0d0*dr_inv*cosinen(1,jj))  &
  &     *(sines(1,jj))

        Vdij(1,jj)  &
  &    =Vdij(1,jj)  &
  &     -Vsrn*(-2.0d0*rd(1)*dr_inv*(sinen(1,jj)+cosinen(1,jj))*sines(1,jj)  &
  &            +(cosinen(1,jj)-sinen(1,jj))*cosines(1,jj))
     end do
!$omp end do

!$omp barrier

!-- 2. For wavenumber >= 2
     if(nrot>1)then
!$omp do schedule(runtime) private(kk,jj)
        do jj=1,nnt
           do kk=2,nrot
              fkij(2+kk,1,jj)  &
  &          =(dr_inv*sinen(kk,jj))  &
  &           *(sines(1,jj))  &
  &          +(0.5d0*dble(kk)*r_inv(1)*cosinen(kk,jj))  &
  &           *(cosines(1,jj))

              fkij(2+nrot+kk,1,jj)  &
  &          =(dr_inv*cosinen(kk,jj))  &
  &           *(sines(1,jj))  &
  &          -(0.5d0*dble(kk)*r_inv(1)*sinen(kk,jj))  &
  &           *(cosines(1,jj))
           end do
        end do
!$omp end do
     end if

  end if

!$omp barrier

  if(ndiv>0)then

!-- Set coefficients for D_s and D_c at each (ii,jj)
!$omp do schedule(runtime) private(kk,pp,ii,jj)
     do jj=1,nnt
        do ii=1,nnr
           do pp=1,nnr
              do kk=1,ndiv
                 fkij(2+2*nrot+kk+ncyc*(pp-1),ii,jj)  &
  &             =rdh(pp+1)*(dble(kk)*dr*r_inv(ii)*gkrr(kk,pp+1,ii)  &
  &                         *cosinen(kk,jj)*sines(ii,jj)  &
  &                        -(gkrr(kk,pp+1,ii+1)-gkrr(kk,pp+1,ii))  &
  &                         *sinen(kk,jj)*cosines(ii,jj))

                 fkij(2+2*nrot+ndiv+kk+ncyc*(pp-1),ii,jj)  &
  &             =-rdh(pp+1)*(dble(kk)*dr*r_inv(ii)*gkrr(kk,pp+1,ii)  &
  &                         *sinen(kk,jj)*sines(ii,jj)  &
  &                        +(gkrr(kk,pp+1,ii+1)-gkrr(kk,pp+1,ii))  &
  &                         *cosinen(kk,jj)*cosines(ii,jj))
              end do
           end do
        end do
     end do
!$omp end do

  end if

!$omp end parallel

  if(nmax>0)then
     deallocate(sinen)
     deallocate(cosinen)
     deallocate(gkrr)
  end if

  call stdout( "Finish procedure.", "calc_fkij", 0 )

end subroutine calc_fkij

!--------------------------------------------------
!-- calculate a_kp from f_kij
!--------------------------------------------------

subroutine calc_fkij2akp( fkij, akp )
  implicit none
  double precision, intent(in) :: fkij(:,:,:)
  double precision, intent(out) :: akp(size(fkij,1),size(fkij,1))
  integer :: nnk, nni, nnj, ii, jj, kk, ll, cstat
  double precision, allocatable, dimension(:,:,:) :: fkl, fpl

!-- OpenMP variables
!$ integer :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS
  integer :: ompnum, omppe

  call stdout( "Enter procedure.", "calc_fkij2akp", 0 )

  nnk=size(fkij,1)
  nni=size(fkij,2)
  nnj=size(fkij,3)

  ompnum=1
  omppe=1
!$   ompnum=OMP_GET_MAX_THREADS()
  allocate(fkl(nni,nnj,ompnum),stat=cstat)
  allocate(fpl(nni,nnj,ompnum),stat=cstat)

  if(cstat/=0)then
     call stdout( "Failed to allocate variables. stop.", "calc_fkij2akp", -1 )
     stop
  end if

  do ll=1,nnk
!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,omppe)
     do kk=ll,nnk
!$      omppe=OMP_GET_THREAD_NUM()+1
        fkl(1:nni,1:nnj,omppe)=fkij(kk,1:nni,1:nnj)
        fpl(1:nni,1:nnj,omppe)=fkij(ll,1:nni,1:nnj)
        akp(kk,ll)=matrix_sum( fkl(1:nni,1:nnj,omppe), fpl(1:nni,1:nnj,omppe) )
        akp(ll,kk)=akp(kk,ll)
     end do
!$omp end do
!$omp end parallel
  end do

  deallocate(fkl)
  deallocate(fpl)

  call stdout( "Finish procedure.", "calc_fkij2akp", 0 )

end subroutine calc_fkij2akp

!--------------------------------------------------
!-- calculate b_k from f_kij and Vd
!--------------------------------------------------

subroutine calc_fkijVd2bk( vmax, fkij, Vdl, bk )
  implicit none
  double precision, intent(in) :: vmax
  double precision, intent(in) :: fkij(:,:,:)
  double precision, intent(in) :: Vdl(size(fkij,2),size(fkij,3))
  double precision, intent(out) :: bk(size(fkij,1))
  integer :: nnk, nni, nnj, ii, jj, kk, ll, cstat
  double precision, allocatable, dimension(:,:,:) :: fkl

!-- OpenMP variables
!$ integer :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS
  integer :: ompnum, omppe

  call stdout( "Enter procedure.", "calc_fkijVd2bk", 0 )

  nnk=size(fkij,1)
  nni=size(fkij,2)
  nnj=size(fkij,3)

  ompnum=1
  omppe=1
!$   ompnum=OMP_GET_MAX_THREADS()
  allocate(fkl(nni,nnj,ompnum),stat=cstat)
  if(cstat/=0)then
     call stdout( "Failed to allocate variables. stop.", "calc_fkijVd2bk", -1 )
     stop
  end if

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,omppe)
  do kk=1,nnk
!$   omppe=OMP_GET_THREAD_NUM()+1
     fkl(1:nni,1:nnj,omppe)=fkij(kk,1:nni,1:nnj)
     bk(kk)=matrix_sum( Vdl(1:nni,1:nnj), fkl(1:nni,1:nnj,omppe) )/vmax
  end do
!$omp end do
!$omp end parallel

  deallocate(fkl)

  call stdout( "Finish procedure.", "calc_fkijVd2bk", 0 )

end subroutine calc_fkijVd2bk

!--------------------------------------------------
!-- Set each unknown variable from x_k
!--------------------------------------------------

subroutine set_xk2variables( nrot, ndiv, xk, VRT0, VDR0,  &
  &                          phis_n, phic_n, Ds_m, Dc_m, undef )
  implicit none
  integer, intent(in) :: nrot
  integer, intent(in) :: ndiv
  double precision, intent(in) :: xk(:)  ! solved unknown variable vector
  double precision, intent(out) :: VRT0(:)
  double precision, intent(out) :: VDR0(size(VRT0))
  double precision, intent(out), optional :: phis_n(nrot,size(VRT0))
  double precision, intent(out), optional :: phic_n(nrot,size(VRT0))
  double precision, intent(out), optional :: Ds_m(ndiv,size(VRT0))
  double precision, intent(out), optional :: Dc_m(ndiv,size(VRT0))
  double precision, intent(in), optional :: undef

  integer :: ii, kk, nnr, ncyc

  call stdout( "Enter procedure.", "set_xk2variables", 0 )

  nnr=size(VRT0)
  ncyc=2+2*(nrot+ndiv)

!-- Set VRT0 and VDR0
  do ii=1,nnr
     VRT0(ii)=xk(1+ncyc*(ii-1))
     VDR0(ii)=xk(2+ncyc*(ii-1))
write(*,*) "check [VRT0, VDR0] = ", VRT0(ii), VDR0(ii), ii
  end do

!-- Set Phi_s and Phi_c
  if(nrot>0)then
!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,ii)
     do ii=1,nnr
        do kk=1,nrot
           phis_n(kk,ii)=xk(2+kk+ncyc*(ii-1))
           phic_n(kk,ii)=xk(2+nrot+kk+ncyc*(ii-1))
        end do
     end do
!$omp end do
!$omp end parallel
do ii=1,nnr
write(*,*) "check [phis_n, phic_n] = ", phis_n(1:nrot,ii), phic_n(1:nrot,ii), ii
end do
  end if

!-- Set D_s and D_c
  if(ndiv>0)then
!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,ii)
     do ii=1,nnr
        do kk=1,ndiv
           Ds_m(kk,ii)=xk(2+2*nrot+kk+ncyc*(ii-1))
           Dc_m(kk,ii)=xk(2+2*nrot+ndiv+kk+ncyc*(ii-1))
        end do
     end do
!$omp end do
!$omp end parallel
do ii=1,nnr
write(*,*) "check [Ds_m, Dc_m] = ", Ds_m(1:ndiv,ii), Dc_m(1:ndiv,ii), ii
end do
  end if

  call stdout( "Finish procedure.", "set_xk2variables", 0 )

end subroutine set_xk2variables

!--------------------------------------------------
!-- Calculate Vr and Vt components of rotating wind
!--------------------------------------------------

subroutine calc_phi2Vrot( nrot, vmax, rmax, rd, rdh, theta, VRT0_r,  &
  &                       VRT0_rt, VRT_nrt, VRR_nrt,  &
  &                       phis_nr, phic_nr, undef )
  implicit none
  integer, intent(in) :: nrot
  double precision, intent(in) :: vmax
  double precision, intent(in) :: rmax
  double precision, intent(in) :: rd(:)
  double precision, intent(in) :: rdh(size(rd)+1)
  double precision, intent(in) :: theta(:)
  double precision, intent(in) :: VRT0_r(size(rd))
  double precision, intent(out) :: VRT0_rt(size(rd),size(theta))
  double precision, intent(out), optional :: VRT_nrt(nrot,size(rd),size(theta))
  double precision, intent(out), optional :: VRR_nrt(nrot,size(rd),size(theta))
  double precision, intent(in), optional :: phis_nr(nrot,size(rd))
  double precision, intent(in), optional :: phic_nr(nrot,size(rd))
  double precision, intent(in), optional :: undef

  integer :: ii, jj, kk, nnr, nnt, cstat
  double precision :: dr_inv
  double precision, dimension(size(rd)) :: r_inv
  double precision, allocatable, dimension(:,:) :: cosinen, sinen

  call stdout( "Enter procedure.", "calc_phi2Vrot", 0 )

  nnr=size(rd)
  nnt=size(theta)

  if(nrot>0)then
     allocate(cosinen(nrot,nnt),stat=cstat)
     allocate(sinen(nrot,nnt),stat=cstat)
     if(cstat/=0)then
        call stdout( "Failed to allocate variables. stop.", "calc_phi2Vrot", -1 )
        stop
     end if
!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,jj)
     do jj=1,nnt
        do kk=1,nrot
           cosinen(kk,jj)=dcos(dble(kk)*theta(jj))
           sinen(kk,jj)=dsin(dble(kk)*theta(jj))
        end do
     end do
!$omp end do
!$omp end parallel
  end if

  dr_inv=1.0d0/(rd(2)-rd(1))
  r_inv(1:nnr)=1.0d0/rd(1:nnr)

  do ii=1,nnr
     VRT0_rt(ii,1:nnt)=VRT0_r(ii)*vmax
  end do

  if(nrot>0)then

     VRT_nrt=0.0d0
     VRR_nrt=0.0d0

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnt
        do ii=2,nnr
           do kk=1,nrot
              VRT_nrt(kk,ii,jj)=-dr_inv*((phis_nr(kk,ii)-phis_nr(kk,ii-1))*sinen(kk,jj)  &
  &                                     +(phic_nr(kk,ii)-phic_nr(kk,ii-1))*cosinen(kk,jj))
              VRR_nrt(kk,ii,jj)=0.5d0*dble(kk)*r_inv(ii)  &
  &                            *((phis_nr(kk,ii)+phis_nr(kk,ii-1))*cosinen(kk,jj)  &
  &                             -(phic_nr(kk,ii)+phic_nr(kk,ii-1))*sinen(kk,jj))
              VRT_nrt(kk,ii,jj)=VRT_nrt(kk,ii,jj)*rmax*vmax
              VRR_nrt(kk,ii,jj)=VRR_nrt(kk,ii,jj)*rmax*vmax
           end do
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,jj)
     do jj=1,nnt  ! At the inner boundary (phi{s,c}=0)
        do kk=1,nrot
           VRT_nrt(kk,1,jj)=-dr_inv*((phis_nr(kk,1))*sinen(kk,jj)  &
  &                                 +(phic_nr(kk,1))*cosinen(kk,jj))
           VRR_nrt(kk,1,jj)=0.5d0*dble(kk)*r_inv(1)  &
  &                        *((phis_nr(kk,1))*cosinen(kk,jj)  &
  &                         -(phic_nr(kk,1))*sinen(kk,jj))
           VRT_nrt(kk,1,jj)=VRT_nrt(kk,1,jj)*rmax*vmax
           VRR_nrt(kk,1,jj)=VRR_nrt(kk,1,jj)*rmax*vmax
        end do
     end do
!$omp end do
!$omp end parallel

  end if

  if(nrot>0)then
     deallocate(cosinen)
     deallocate(sinen)
  end if

  call stdout( "Finish procedure.", "calc_phi2Vrot", 0 )

end subroutine calc_phi2Vrot

!--------------------------------------------------
!-- Calculate Vr and Vt components of divergent wind
!--------------------------------------------------

subroutine calc_D2Vdiv( ndiv, vmax, rmax, rd, rdh, theta, VDR0_r,  &
  &                     VDR0_rt, VDT_mrt, VDR_mrt, Ds_mr, Dc_mr, undef )
  implicit none
  integer, intent(in) :: ndiv
  double precision, intent(in) :: vmax
  double precision, intent(in) :: rmax
  double precision, intent(in) :: rd(:)
  double precision, intent(in) :: rdh(size(rd)+1)
  double precision, intent(in) :: theta(:)
  double precision, intent(in) :: VDR0_r(size(rd))
  double precision, intent(out) :: VDR0_rt(size(rd),size(theta))
  double precision, intent(out), optional :: VDT_mrt(ndiv,size(rd),size(theta))
  double precision, intent(out), optional :: VDR_mrt(ndiv,size(rd),size(theta))
  double precision, intent(in), optional :: Ds_mr(ndiv,size(rd))
  double precision, intent(in), optional :: Dc_mr(ndiv,size(rd))
  double precision, intent(in), optional :: undef

  integer :: ii, jj, kk, nnr, nnt, cstat
  double precision :: dr_inv, dr, rmax_inv
  double precision, dimension(size(rd)) :: r_inv
  double precision, allocatable, dimension(:,:) :: cosinen, sinen
  double precision, allocatable, dimension(:,:) :: gkrrhDs, gkrrhDc, dgkrrDs, dgkrrDc
  double precision, allocatable, dimension(:,:,:) :: gkrr, gkrrh, dgkrr

  call stdout( "Enter procedure.", "calc_D2Vdiv", 0 )

  nnr=size(rd)
  nnt=size(theta)

  if(ndiv>0)then
     allocate(cosinen(ndiv,nnt),stat=cstat)
     allocate(sinen(ndiv,nnt),stat=cstat)
     allocate(gkrr(ndiv,nnr+1,nnr+1),stat=cstat)  ! Gk(r_p,r), r_p at rdh, r at rd
     allocate(gkrrh(ndiv,nnr+1,nnr+1),stat=cstat)  ! Gk(r_p,r), r_p at rdh, r at rdh
     allocate(dgkrr(ndiv,nnr+1,nnr+1),stat=cstat)  ! Gk(r_p,r+1)-Gk(r_p,r), r_p at rdh, r at rd
     if(cstat/=0)then
        call stdout( "Failed to allocate variables. stop.", "calc_D2Vdiv", -1 )
        stop
     end if
     cosinen=0.0d0
     sinen=0.0d0
     gkrr=0.0d0
  end if

  dr=rd(2)-rd(1)
  dr_inv=1.0/(rd(2)-rd(1))
  r_inv(1:nnr)=1.0d0/rd(1:nnr)
  rmax_inv=1.0d0/rmax

  do ii=1,nnr
     VDR0_rt(ii,1:nnt)=VDR0_r(ii)*vmax
  end do

  if(ndiv>0)then

     VDT_mrt=0.0d0
     VDR_mrt=0.0d0

!$omp parallel default(shared)
!$omp do schedule(runtime) private(kk,jj)
     do jj=1,nnt
        do kk=1,ndiv
           cosinen(kk,jj)=dcos(dble(kk)*theta(jj))
           sinen(kk,jj)=dsin(dble(kk)*theta(jj))
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnr  ! For r
        do ii=1,nnr+1  ! For r_p
           do kk=1,ndiv
              gkrr(kk,ii,jj)=green_func( rdh(ii), rd(jj), kk )
           end do
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii)
     ! At the outer boundary for r
     do ii=1,nnr+1  ! For r_p
        do kk=1,ndiv
           gkrr(kk,ii,nnr+1)=green_func( rdh(ii), rd(nnr)+dr, kk )
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnr+1  ! For r
        do ii=1,nnr+1  ! For r_p
           do kk=1,ndiv
              gkrrh(kk,ii,jj)=green_func( rdh(ii), rdh(jj), kk )
           end do
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnr  ! For r
        do ii=1,nnr+1  ! For r_p
           do kk=1,ndiv
              dgkrr(kk,ii,jj)=gkrr(kk,ii,jj+1)-gkrr(kk,ii,jj)
           end do
        end do
     end do
!$omp end do

!$omp barrier

!$omp do schedule(runtime) private(kk,ii,jj)
     do jj=1,nnt
        do ii=1,nnr
           do kk=1,ndiv
              VDT_mrt(kk,ii,jj)=-(dr*dble(kk)*r_inv(ii)*rmax_inv*vmax)  &
  &                              *(line_integral( nnr, rdh(1:nnr), gkrrh(kk,1:nnr,ii), Ds_mr(kk,1:nnr) )  &
  &                                *cosinen(kk,jj)  &
  &                               -line_integral( nnr, rdh(1:nnr), gkrrh(kk,1:nnr,ii), Dc_mr(kk,1:nnr) )  &
  &                                *sinen(kk,jj))

              VDR_mrt(kk,ii,jj)=-(rmax_inv*vmax)  &
  &                              *(line_integral( nnr, rdh(1:nnr), dgkrr(kk,1:nnr,ii), Ds_mr(kk,1:nnr) )  &
  &                               *sinen(kk,jj)  &
  &                              +line_integral( nnr, rdh(1:nnr), dgkrr(kk,1:nnr,ii), Dc_mr(kk,1:nnr) )  &
  &                               *cosinen(kk,jj))
           end do
        end do
     end do
!$omp end do
!$omp end parallel

  end if

  if(ndiv>0)then
     deallocate(cosinen)
     deallocate(sinen)
     deallocate(gkrr)
     deallocate(gkrrh)
     deallocate(dgkrr)
  end if

  call stdout( "Finish procedure.", "calc_D2Vdiv", 0 )

end subroutine calc_D2Vdiv

!--------------------------------------------------
!-- Calculate total wind from all wavenumbers
!--------------------------------------------------

subroutine calc_Vn2Vtot( nrot, ndiv, V0, Vn, Vm, Vtot, undef )
  implicit none
  integer, intent(in) :: nrot
  integer, intent(in) :: ndiv
  double precision, intent(in) :: V0(:,:)
  double precision, intent(in) :: Vn(nrot,size(V0,1),size(V0,2))
  double precision, intent(in) :: Vm(ndiv,size(V0,1),size(V0,2))
  double precision, intent(inout) :: Vtot(size(V0,1),size(V0,2))
  double precision, intent(in), optional :: undef
  integer :: ii, jj, kk, nnr, nnt

  call stdout( "Enter procedure.", "calc_Vn2Vtot", 0 )

  nnr=size(V0,1)
  nnt=size(V0,2)
  Vtot=V0

  if(present(undef))then
     do jj=1,nnt
        do ii=1,nnr
           do kk=1,nrot
              if(Vn(kk,ii,jj)/=undef)then
                 Vtot(ii,jj)=Vtot(ii,jj)+Vn(kk,ii,jj)
              end if
           end do
           do kk=1,ndiv
              if(Vn(kk,ii,jj)/=undef)then
                 Vtot(ii,jj)=Vtot(ii,jj)+Vm(kk,ii,jj)
              end if
           end do
        end do
     end do
  else
     do jj=1,nnt
        do ii=1,nnr
           do kk=1,nrot
              Vtot(ii,jj)=Vtot(ii,jj)+Vn(kk,ii,jj)
           end do
           do kk=1,ndiv
              Vtot(ii,jj)=Vtot(ii,jj)+Vm(kk,ii,jj)
           end do
        end do
     end do
  end if

  call stdout( "Finish procedure.", "calc_Vn2Vtot", 0 )

end subroutine calc_Vn2Vtot

!--------------------------------------------------
!-- calculate product for a component in a matrix
!--------------------------------------------------

double precision function matrix_sum( aij, akj )
  implicit none
  double precision, intent(in) :: aij(:,:)
  double precision, intent(in) :: akj(size(aij,1),size(aij,2))
  integer :: ii, jj, ni, nj
  double precision :: res

  ni=size(aij,1)
  nj=size(akj,1)

  res=0.0d0

  do jj=1,nj
     do ii=1,ni
        res=res+aij(ii,jj)*akj(ii,jj)
     end do
  end do

  matrix_sum=res

  return

end function matrix_sum

!--------------------------------------------------
!--------------------------------------------------

subroutine check_zero( a )
  implicit none
  double precision, intent(in) :: a(:,:)
  integer :: ii, jj, nni, nnj
  logical :: res

  nni=size(a,1)
  nnj=size(a,2)

  do jj=1,nnj
     res=.false.
     do ii=1,nni
        if(a(ii,jj)/=0.0d0)then
           res=.true.
           exit
        end if
     end do
     if(res.eqv..false.)then
        write(*,*) "Detect all zero", jj
     end if
  end do

end subroutine check_zero

end module main_mod
