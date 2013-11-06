!>@file   add_skv1_2_matrix.f90
!!@brief  module add_skv1_2_matrix
!!
!!@author H. Matsui
!!@author K. Nakajima and H. Matsui
!!@date        Written by H. Matsui on Jan., 2009
!!@n      modified by H. Matsui on Nov., 2013
!
!>     Matrix assemble
!!
!!@verbatim
!!      subroutine add_skv1_2_matrix11(np_smp, numele, nnod_4_e1,       &
!!     &          inod_ele_max, num_sort_smp, nod_stack_smp,            &
!!     &          iele_sort_smp, iconn_sort_smp, idx_4_mat,             &
!!     &          k2, sk_v, num_mat, aiccg)
!!      subroutine add_skv1_2_matrix33(np_smp, numele, nnod_4_e1,       &
!!     &          inod_ele_max, num_sort_smp, nod_stack_smp,            &
!!     &          iele_sort_smp, iconn_sort_smp, idx_4_mat,             &
!!     &          k2, sk_v, num_mat, aiccg33)
!!@endverbatim
!
      module add_skv1_2_matrix
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
      subroutine add_skv1_2_matrix11(np_smp, numele, nnod_4_e1,         &
     &          inod_ele_max, num_sort_smp, nod_stack_smp,              &
     &          iele_sort_smp, iconn_sort_smp, idx_4_mat,               &
     &          k2, sk_v, num_mat, aiccg)
!
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: numele, nnod_4_e1
      integer(kind = kint), intent(in) :: inod_ele_max, num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &                  :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in)                                  &
     &                  :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in)                                  &
     &                  :: iconn_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in)                                  &
     &                  :: idx_4_mat(num_sort_smp,nnod_4_e1)
      integer(kind = kint), intent(in) :: num_mat
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in)                                     &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real(kind = kreal), intent(inout) :: aiccg(0:num_mat)
!
      integer(kind = kint) :: iproc, inum, inn
      integer(kind = kint) :: istart, iend, in, iele, iconn
      integer(kind = kint) :: mat_num
!
!
!$omp parallel do private(inum,istart,iend,inn,in,iele,iconn,mat_num)
      do iproc = 1, np_smp
        do inum = 1, inod_ele_max
!
          inn = inum + inod_ele_max*(iproc-1)
          istart = nod_stack_smp(inn-1)+1
          iend = nod_stack_smp(inn)
!
!cdir nodep
          do in = istart, iend
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            mat_num = idx_4_mat(in,k2)
            aiccg(mat_num) = aiccg(mat_num) + sk_v(iele,1,iconn)
!
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine add_skv1_2_matrix11
!
!-----------------------------------------------------------------------
!
      subroutine add_skv1_2_matrix33(np_smp, numele, nnod_4_e1,         &
     &          inod_ele_max, num_sort_smp, nod_stack_smp,              &
     &          iele_sort_smp, iconn_sort_smp, idx_4_mat,               &
     &          k2, sk_v, num_mat, aiccg33)
!
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: numele, nnod_4_e1
      integer(kind = kint), intent(in) :: inod_ele_max, num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &                  :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in)                                  &
     &                  :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in)                                  &
     &                  :: iconn_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in)                                  &
     &                  :: idx_4_mat(num_sort_smp,nnod_4_e1)
      integer(kind = kint), intent(in) :: num_mat
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in)                                     &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real(kind = kreal), intent(inout) :: aiccg33(-8:num_mat)
!
      integer(kind = kint) :: iproc, inum, inn
      integer(kind = kint) :: istart, iend, in, iele, iconn
      integer(kind = kint) :: mat_num
!
!
!$omp parallel do private(inum,istart,iend,inn,in,iele,iconn,mat_num)
      do iproc = 1, np_smp
        do inum = 1, inod_ele_max
!
          inn = inum + inod_ele_max*(iproc-1)
          istart = nod_stack_smp(inn-1)+1
          iend = nod_stack_smp(inn)
!
          do in = istart, iend
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            mat_num = idx_4_mat(in,k2)
            aiccg33(9*mat_num-8) = aiccg33(9*mat_num-8)                 &
     &                            + sk_v(iele,1,iconn)
            aiccg33(9*mat_num-4) = aiccg33(9*mat_num-4)                 &
     &                            + sk_v(iele,1,iconn)
            aiccg33(9*mat_num  ) = aiccg33(9*mat_num  )                 &
     &                            + sk_v(iele,1,iconn)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine add_skv1_2_matrix33
!
!-----------------------------------------------------------------------
!
      end module add_skv1_2_matrix
