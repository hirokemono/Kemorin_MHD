!fem_grp_skv_mass_mat.f90
!      module fem_grp_skv_mass_mat
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on March, 2006
!
!!      subroutine fem_grp_skv_mass_matrix(numele, nnod_4_e1, nnod_4_e2,&
!!     &          np_smp, iele_fsmp_stack, nele_grp, iele_grp,          &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, num_int, xjac, an1, an2, k2, sk_v)
!!      subroutine fem_grp_skv_mass_matrix_diag(numele, nnod_4_e1,      &
!!     &          np_smp, iele_fsmp_stack, nele_grp, iele_grp,          &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, num_int, xjac, an, sk_v)
!!
!!      subroutine fem_grp_skv_mass_mat_diag_HRZ(numele, nnod_4_e1,     &
!!     &          np_smp, iele_fsmp_stack, nele_grp, iele_grp,          &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, num_int, xjac, an, sk_v)
!!
!!      subroutine grp_volume_average_skv_HRZ(numele, nnod_4_e1, np_smp,&
!!     &          iele_fsmp_stack, nele_grp, iele_grp, volume_ele,      &
!!     &          sk_v, ml_e)
!
      module fem_grp_skv_mass_mat
!
      use m_precision
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
      subroutine fem_grp_skv_mass_matrix(numele, nnod_4_e1, nnod_4_e2,  &
     &          np_smp, iele_fsmp_stack, nele_grp, iele_grp,            &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, num_int, xjac, an1, an2, k2, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: num_int, ntot_int_3d
      real (kind=kreal), intent(in)  :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in)  :: an1(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in)  :: an2(nnod_4_e2,ntot_int_3d)
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
        do ii= 1, num_int * num_int * num_int 
          ix = int_start3(num_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do inum = istart, iend
              iele = iele_grp(inum)
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
      end  subroutine fem_grp_skv_mass_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_matrix_diag(numele, nnod_4_e1,        &
     &          np_smp, iele_fsmp_stack, nele_grp, iele_grp,            &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, num_int, xjac, an, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, np_smp
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: num_int, ntot_int_3d
      real (kind=kreal), intent(in)  :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in)  :: an(nnod_4_e1,ntot_int_3d)
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
        do ii= 1, num_int * num_int * num_int 
          ix = int_start3(num_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
            do inum = istart, iend
              iele = iele_grp(inum)
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                      + an(k1,ix) * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end  subroutine fem_grp_skv_mass_matrix_diag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_mat_diag_HRZ(numele, nnod_4_e1,       &
     &          np_smp, iele_fsmp_stack, nele_grp, iele_grp,            &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, num_int, xjac, an, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, np_smp
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: num_int, ntot_int_3d
      real (kind=kreal), intent(in)  :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in)  :: an(nnod_4_e1,ntot_int_3d)
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
        do ii= 1, num_int * num_int * num_int 
          ix = int_start3(num_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
            do inum = istart, iend
              iele = iele_grp(inum)
              sk_v(iele,1,k1) = sk_v(iele,1,k1) + an(k1,ix)*an(k1,ix)   &
     &                                   * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end  subroutine fem_grp_skv_mass_mat_diag_HRZ
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine grp_volume_average_skv_HRZ(numele, nnod_4_e1, np_smp,  &
     &          iele_fsmp_stack, nele_grp, iele_grp, volume_ele,        &
     &          sk_v, ml_e)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, np_smp
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in)  :: volume_ele(numele)
      real (kind=kreal), intent(in)  :: ml_e(numele)
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
            sk_v(iele,1,k1) = volume_ele(iele) * sk_v(iele,1,k1)        &
     &                       * ml_e(iele)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine grp_volume_average_skv_HRZ
!
!-----------------------------------------------------------------------
!
      end module  fem_grp_skv_mass_mat
