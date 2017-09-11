!fem_skv_nodal_field.f90
!      module fem_skv_nodal_field
!
!     programmed by H.Matsui on May 2009
!
!!      subroutine fem_skv_scalar_field                                 &
!!     &         (numele, nnod_4_e1, nnod_4_e2, iele_fsmp_stack,        &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, k2, xjac, an1, an2, scalar_e,     &
!!     &          sk_v)
!!      subroutine fem_skv_vector_field                                 &
!!     &         (numele, nnod_4_e1, nnod_4_e2, iele_fsmp_stack,        &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, k2, xjac, an1, an2, vector_e,     &
!!     &          sk_v)
!!      subroutine fem_skv_tensor_field                                 &
!!     &         (numele, nnod_4_e1, nnod_4_e2,  iele_fsmp_stack,       &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, k2, xjac, an1, an2, tensor_e,     &
!!     &          sk_v)
!
      module fem_skv_nodal_field
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_field                                   &
     &         (numele, nnod_4_e1, nnod_4_e2, iele_fsmp_stack,          &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, k2, xjac, an1, an2, scalar_e,       &
     &          sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: n_int, ntot_int_3d, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an1(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in) :: an2(nnod_4_e2,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: scalar_e(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied) 
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied   = iele_fsmp_stack(iproc)
!
        do k1 = 1, nnod_4_e1
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir nodep
            do iele = ist, ied
              sk_v(iele,1,k1) = sk_v(iele,1,k1) + scalar_e(iele)        &
     &                         * an1(k1,ix) * an2(k2,ix)                &
     &                         * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
!
      end subroutine fem_skv_scalar_field
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_vector_field                                   &
     &         (numele, nnod_4_e1, nnod_4_e2, iele_fsmp_stack,          &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, k2, xjac, an1, an2, vector_e,       &
     &          sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: n_int, ntot_int_3d, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an1(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in) :: an2(nnod_4_e2,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: vector_e(numele,n_vector)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied) 
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied   = iele_fsmp_stack(iproc)
!
        do k1 = 1, nnod_4_e1
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir nodep
            do iele = ist, ied
              sk_v(iele,1,k1) = sk_v(iele,1,k1) + vector_e(iele,1)      &
     &                         * an1(k1,ix) * an2(k2,ix)                &
     &                         * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1) + vector_e(iele,2)      &
     &                         * an1(k1,ix) * an2(k2,ix)                &
     &                         * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1) + vector_e(iele,3)      &
     &                         * an1(k1,ix) * an2(k2,ix)                &
     &                         * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
!
      end subroutine fem_skv_vector_field
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_field                                   &
     &         (numele, nnod_4_e1, nnod_4_e2,  iele_fsmp_stack,         &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, k2, xjac, an1, an2, tensor_e,       &
     &          sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: n_int, ntot_int_3d, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an1(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in) :: an2(nnod_4_e2,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: tensor_e(numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied) 
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied   = iele_fsmp_stack(iproc)
!
        do k1 = 1, nnod_4_e1
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir nodep
            do iele = ist, ied
              sk_v(iele,1,k1) = sk_v(iele,1,k1) + tensor_e(iele,1)      &
     &                         * an1(k1,ix) * an2(k2,ix)                &
     &                         * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1) + tensor_e(iele,2)      &
     &                         * an1(k1,ix) * an2(k2,ix)                &
     &                         * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1) + tensor_e(iele,3)      &
     &                         * an1(k1,ix) * an2(k2,ix)                &
     &                         * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,4,k1) = sk_v(iele,4,k1) + tensor_e(iele,4)      &
     &                         * an1(k1,ix) * an2(k2,ix)                &
     &                         * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,5,k1) = sk_v(iele,5,k1) + tensor_e(iele,5)      &
     &                         * an1(k1,ix) * an2(k2,ix)                &
     &                         * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,6,k1) = sk_v(iele,6,k1) + tensor_e(iele,6)      &
     &                         * an1(k1,ix) * an2(k2,ix)                &
     &                         * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
!
      end subroutine fem_skv_tensor_field
!
! ----------------------------------------------------------------------
!
      end module fem_skv_nodal_field
