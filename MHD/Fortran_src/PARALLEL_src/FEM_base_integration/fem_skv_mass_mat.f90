!
!      module fem_skv_mass_mat
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on March, 2006
!     Modified by H. Matsui on March, 2009
!
!      subroutine fem_skv_mass_matrix(numele, nnod_4_e1, nnod_4_e2,     &
!     &          np_smp, iele_fsmp_stack, ntot_int_3d, num_int,         &
!     &          xjac, an1, an2, k2, sk_v)
!      subroutine fem_skv_mass_matrix_diag(numele, nnod_4_e1, np_smp,   &
!     &          iele_fsmp_stack, ntot_int_3d, num_int, xjac, an, sk_v)
!
!      subroutine fem_skv_mass_mat_diag_HRZ(numele, nnod_4_e1, np_smp,  &
!     &          iele_fsmp_stack, ntot_int_3d, num_int, an, xjac, sk_v)
!      subroutine sum_skv_diagonal_4_HRZ(numele, nnod_4_e1, np_smp,     &
!     &           iele_fsmp_stack, sk_v, sk_e, ml_e)
!      subroutine volume_average_skv_HRZ(numele, nnod_4_e1, np_smp,     &
!     &          iele_fsmp_stack, volume_ele, sk_v, ml_e)
!
      module fem_skv_mass_mat
!
      use m_precision
      use m_constants
!
      use m_phys_constants
      use m_fem_gauss_int_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_matrix(numele, nnod_4_e1, nnod_4_e2,      &
     &          np_smp, iele_fsmp_stack, ntot_int_3d, num_int,          &
     &          xjac, an1, an2, k2, sk_v)
!
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: num_int, ntot_int_3d
      real (kind=kreal), intent(in)  :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in)  :: an1(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in)  :: an2(nnod_4_e2,ntot_int_3d)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, num_int * num_int * num_int 
          ix = int_start3(num_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
            do iele = istart, iend
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &             + an1(k1,ix)*an2(k2,ix) * xjac(iele,ix) * owe3d(ix)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end  subroutine fem_skv_mass_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_matrix_diag(numele, nnod_4_e1, np_smp,    &
     &          iele_fsmp_stack, ntot_int_3d, num_int, xjac, an, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, np_smp
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int, ntot_int_3d
      real (kind=kreal), intent(in)  :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in)  :: an(nnod_4_e1,ntot_int_3d)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, num_int * num_int * num_int 
          ix = int_start3(num_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
            do iele = istart, iend
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                      + an(k1,ix) * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end  subroutine fem_skv_mass_matrix_diag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_mat_diag_HRZ(numele, nnod_4_e1, np_smp,   &
     &          iele_fsmp_stack, ntot_int_3d, num_int, an, xjac, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, np_smp
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int, ntot_int_3d
      real (kind=kreal), intent(in)  :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in)  :: an(nnod_4_e1,ntot_int_3d)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, num_int * num_int * num_int 
          ix = int_start3(num_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
            do iele = istart, iend
              sk_v(iele,1,k1) = sk_v(iele,1,k1) + an(k1,ix)*an(k1,ix)   &
     &                                   * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end  subroutine fem_skv_mass_mat_diag_HRZ
!
!-----------------------------------------------------------------------
!
      subroutine sum_skv_diagonal_4_HRZ(numele, nnod_4_e1, np_smp,      &
     &           iele_fsmp_stack, sk_v, sk_e, ml_e)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, np_smp
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in)                                     &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal), intent(inout) :: sk_e(numele)
      real (kind=kreal), intent(inout) :: ml_e(numele)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele
      integer (kind=kint) :: istart, iend
!
!
      sk_e = 0.0d0
!
!$omp parallel do private(k1,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
!  sumuation of diagonal components
!
        do k1 = 1, nnod_4_e1
!cdir nodep
          do iele = istart, iend
            sk_e(iele) = sk_e(iele) + sk_v(iele,1,k1)
          end do
        end do
!
!cdir nodep
        do iele = istart, iend
          ml_e(iele) = one / sk_e(iele)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine sum_skv_diagonal_4_HRZ
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine volume_average_skv_HRZ(numele, nnod_4_e1, np_smp,      &
     &          iele_fsmp_stack, volume_ele, sk_v, ml_e)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, np_smp
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in)  :: volume_ele(numele)
      real (kind=kreal), intent(in)  :: ml_e(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do k1 = 1, nnod_4_e1
!cdir nodep
          do iele = istart, iend
            sk_v(iele,1,k1) = volume_ele(iele) * sk_v(iele,1,k1)       &
     &                       * ml_e(iele)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine volume_average_skv_HRZ
!
!-----------------------------------------------------------------------
!
      end module  fem_skv_mass_mat
