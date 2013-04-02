!cal_diffuse_matrix.f90
!      module cal_diffuse_matrix
!
!     Written by H. Matsui on Oct. 2005
!
!      subroutine cal_scalar_diffuse_evo_mat(np_smp, numele, nnod_4_e1, &
!     &          inod_ele_max, num_sort_smp, nod_stack_smp,             &
!     &          iele_sort_smp, iconn_sort_smp, idx_4_mat,              &
!     &          coef_imp, dt, ak_d, k2, sk_v, nmat_size, aiccg)
!      subroutine cal_vector_diffuse_evo_mat(np_smp, numele, nnod_4_e1, &
!     &          inod_ele_max, num_sort_smp, nod_stack_smp,             &
!     &          iele_sort_smp, iconn_sort_smp, idx_4_mat,              &
!     &          coef_imp, dt, ak_d, k2, sk_v, nmat_size, aiccg33)
!
      module cal_diffuse_matrix
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
      subroutine cal_scalar_diffuse_evo_mat(np_smp, numele, nnod_4_e1,  &
     &          inod_ele_max, num_sort_smp, nod_stack_smp,              &
     &          iele_sort_smp, iconn_sort_smp, idx_4_mat,               &
     &          coef_imp, dt, ak_d, k2, sk_v, nmat_size, aiccg)
!
      integer(kind = kint), intent(in) :: np_smp, numele, nnod_4_e1
      integer(kind = kint), intent(in) :: inod_ele_max, num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &               :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in) :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in) :: iconn_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in)                                  &
     &               :: idx_4_mat(num_sort_smp,nnod_4_e1)
!
      integer (kind = kint), intent(in) :: k2
      real(kind=kreal), intent(in) :: coef_imp, dt 
      real(kind=kreal), intent(in) :: ak_d(numele)
      real (kind=kreal), intent(in)                                     &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
      integer (kind = kint) :: mat_num
      integer (kind = kint) :: iproc, iele, inum
      integer (kind = kint) :: istart, iend
      integer (kind = kint) :: in, inn, iconn
!
!
!$omp parallel do private(inum,iele,iconn,inn,in,istart,iend,mat_num)
      do iproc = 1, np_smp
        do inum = 1, inod_ele_max
!
          inn = inum + inod_ele_max*(iproc-1)
          istart = nod_stack_smp(inn-1)+1
          iend = nod_stack_smp(inn)
!cdir nodep
          do in = istart, iend
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            mat_num = idx_4_mat(in,k2)
            aiccg(mat_num) = aiccg(mat_num) + coef_imp * dt             &
     &                      * ak_d(iele) * sk_v(iele,1,iconn)
          end do
        end do
      end do
!$omp end parallel do
!
!
      end subroutine cal_scalar_diffuse_evo_mat
!
!-----------------------------------------------------------------------
!
      subroutine cal_vector_diffuse_evo_mat(np_smp, numele, nnod_4_e1,  &
     &          inod_ele_max, num_sort_smp, nod_stack_smp,              &
     &          iele_sort_smp, iconn_sort_smp, idx_4_mat,               &
     &          coef_imp, dt, ak_d, k2, sk_v, nmat_size, aiccg33)
!
      integer(kind = kint), intent(in) :: np_smp, numele, nnod_4_e1
      integer(kind = kint), intent(in) :: inod_ele_max, num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &               :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in) :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in) :: iconn_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in)                                  &
     &               :: idx_4_mat(num_sort_smp,nnod_4_e1)
!
      integer (kind = kint), intent(in) :: k2
      real(kind=kreal), intent(in) :: coef_imp, dt
      real(kind=kreal), intent(in) :: ak_d(numele)
      real (kind=kreal), intent(in)                                     &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg33(-8:nmat_size)
!
      integer (kind = kint) :: iproc, iele, inum
      integer (kind = kint) :: istart, iend
      integer (kind = kint) :: mat_num
      integer (kind = kint) :: in, inn, iconn
!
!
!$omp parallel do private(inum,iele,iconn,inn,in,istart,iend,mat_num)
      do iproc = 1, np_smp
        do inum = 1, inod_ele_max
!
          inn = inum + inod_ele_max*(iproc-1)
          istart = nod_stack_smp(inn-1)+1
          iend = nod_stack_smp(inn)
!cdir nodep
          do in = istart, iend
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            mat_num = idx_4_mat(in,k2)
!
            aiccg33(mat_num*9-8) = aiccg33(mat_num*9-8)                 &
     &                            + coef_imp * dt * ak_d(iele)          &
     &                             * sk_v(iele,1,iconn)
            aiccg33(mat_num*9-4) = aiccg33(mat_num*9-4)                 &
     &                            + coef_imp * dt * ak_d(iele)          &
     &                             * sk_v(iele,1,iconn)
            aiccg33(mat_num*9  ) = aiccg33(mat_num*9  )                 &
     &                            + coef_imp * dt * ak_d(iele)          &
     &                             * sk_v(iele,1,iconn)
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_vector_diffuse_evo_mat
!
!-----------------------------------------------------------------------
!
      end module cal_diffuse_matrix
