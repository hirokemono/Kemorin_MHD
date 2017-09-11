!
!      module fem_skv_scalar_on_ele
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine fem_skv_scalar_on_ele_m                              &
!!     &         (numele, nnod_4_e1, iele_fsmp_stack,                   &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, xjac, an1, scalar_ele, sk_v)
!!      subroutine fem_skv_scalar_on_ele_HRZ_m(numele, nnod_4_e1,       &
!!     &          iele_fsmp_stack, volume_ele, ml_ele_diag, scalar_ele, &
!!     &          sk_v)
!!
!!      subroutine fem_skv_scalar_on_ele_grp_m(numele, nnod_4_e1,       &
!!     &          iele_fsmp_stack, nele_grp, iele_grp,                  &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, xjac, an1, scalar_ele, sk_v)
!!      subroutine fem_skv_scalar_on_ele_grp_HRZ(numele, nnod_4_e1,     &
!!     &          iele_fsmp_stack, volume_ele, nele_grp, iele_grp,      &
!!     &          ml_ele_diag, scalar_ele, sk_v)
!
      module fem_skv_scalar_on_ele
!
      use m_precision
      use m_machine_parameter
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
      subroutine fem_skv_scalar_on_ele_m                                &
     &         (numele, nnod_4_e1, iele_fsmp_stack,                     &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, xjac, an1, scalar_ele, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar_ele(numele)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an1(nnod_4_e1,ntot_int_3d)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: ip, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied)
      do ip = 1, np_smp
        ist = iele_fsmp_stack(ip-1)+1
        ied = iele_fsmp_stack(ip)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
            do iele = ist, ied
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                          + an1(k1,ix) * scalar_ele(iele)         &
     &                           * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_scalar_on_ele_m
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_HRZ_m(numele, nnod_4_e1,         &
     &          iele_fsmp_stack, volume_ele, ml_ele_diag, scalar_ele,   &
     &          sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: volume_ele(numele)
      real (kind=kreal), intent(in) :: scalar_ele(numele)
      real (kind=kreal), intent(in) :: ml_ele_diag(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: ip, iele
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,iele,ist,ied)
      do ip = 1, np_smp
        ist = iele_fsmp_stack(ip-1)+1
        ied = iele_fsmp_stack(ip)
!
        do k1 = 1, nnod_4_e1
!cdir nodep
          do iele = ist, ied
            sk_v(iele,1,k1) = volume_ele(iele) * scalar_ele(iele)       &
     &                       * sk_v(iele,1,k1) * ml_ele_diag(iele)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_skv_scalar_on_ele_HRZ_m
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_grp_m(numele, nnod_4_e1,         &
     &          iele_fsmp_stack, nele_grp, iele_grp,                    &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, xjac, an1, scalar_ele, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an1(nnod_4_e1,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: scalar_ele(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, inum, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,inum,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
            do inum = istart, iend
              iele = iele_grp(inum)
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         + an1(k1,ix) * scalar_ele(iele)          &
     &                          * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
!
      end subroutine fem_skv_scalar_on_ele_grp_m
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_grp_HRZ(numele, nnod_4_e1,       &
     &          iele_fsmp_stack, volume_ele, nele_grp, iele_grp,        &
     &          ml_ele_diag, scalar_ele, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
      real (kind=kreal), intent(in) :: volume_ele(numele)
      real (kind=kreal), intent(in) :: scalar_ele(numele)
      real (kind=kreal), intent(in) :: ml_ele_diag(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, inum, iele
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,inum,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do k1 = 1, nnod_4_e1
!cdir nodep
          do inum = istart, iend
            iele = iele_grp(inum)
            sk_v(iele,1,k1) = volume_ele(iele) * scalar_ele(iele)       &
     &                       * sk_v(iele,1,k1)  * ml_ele_diag(iele)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_skv_scalar_on_ele_grp_HRZ
!
!-----------------------------------------------------------------------
!
      end module fem_skv_scalar_on_ele
