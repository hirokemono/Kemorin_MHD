!
!     module fem_skv_stratified
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!!      subroutine fem_skv_stratified_pg                                &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, k2, xjac, an1, an2,               &
!!     &          scalar_e, vxe, xe, sk_v)
!!      subroutine fem_skv_stratified_upw                               &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, k2, dt, xjac, an1, dnx1, an2,     &
!!     &          scalar_e, vxe, xe, sk_v)
!
      module fem_skv_stratified
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_stratified_pg                                  &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, k2, xjac, an1, an2,                 &
     &          scalar_e, vxe, xe, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp, n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind=kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in) :: an1(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in) :: an2(nnod_4_e2, ntot_int_3d)
!
      real (kind=kreal), intent(in) :: scalar_e(numele)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: xe(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: inertia
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend,inertia)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = istart, iend
!
!  ------  set inertia term
!
             inertia  = an1(k1,ix) * an2(k2,ix)                         &
     &                 * ( vxe(iele,1)*xe(iele,1)                       &
     &                   + vxe(iele,2)*xe(iele,2)                       &
     &                   + vxe(iele,3)*xe(iele,3) )
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         - inertia * scalar_e(iele)               &
     &                          * xjac(iele,ix)  * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_stratified_pg
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_stratified_upw                                 &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, k2, dt, xjac, an1, dnx1, an2,       &
     &          scalar_e, vxe, xe, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp, n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind=kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in) :: an1(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in) :: an2(nnod_4_e2, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: scalar_e(numele)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: xe(numele,3)
      real (kind=kreal), intent(in) :: dt
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: inertia, tau
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend,inertia,tau)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = istart, iend
!
!  -----  set weighting function
!
              tau = an1(k1,ix) + half * dt                              &
     &                  * ( vxe(iele,1)*dnx1(iele,k1,ix,1)              &
     &                    + vxe(iele,2)*dnx1(iele,k1,ix,2)              &
     &                    + vxe(iele,3)*dnx1(iele,k1,ix,3) )
!
!  ------  set inertia term
!
              inertia  =  tau * an2(k2,ix) * ( vxe(iele,1)*xe(iele,1)   &
     &                                      + vxe(iele,2)*xe(iele,2)    &
     &                                      + vxe(iele,3)*xe(iele,3) )
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         - inertia * scalar_e(iele)               &
     &                          * xjac(iele,ix)  * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_stratified_upw
!
!-----------------------------------------------------------------------
!
      end module fem_skv_stratified
