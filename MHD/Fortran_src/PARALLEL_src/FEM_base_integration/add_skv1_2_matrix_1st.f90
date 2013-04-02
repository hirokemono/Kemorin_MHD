!add_skv1_2_matrix_1st.f90
!     module add_skv1_2_matrix_1st
!
!      subroutine add_skv1_2_matrix11_1st(k2, sk_v, nmat_size, aiccg)
!      subroutine add_skv1_2_matrix33_1st(k2, sk_v, nmat_size, aiccg33)
!
      module add_skv1_2_matrix_1st
!
      use m_precision
      use m_machine_parameter
      use m_geometry_parameter
      use m_sorted_node
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
      subroutine add_skv1_2_matrix11_1st(k2, sk_v, nmat_size, aiccg)
!
      use add_skv1_2_matrix
!
      integer (kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in)                                     &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
!
      call add_skv1_2_matrix11(np_smp, numele, nnod_4_ele,              &
     &    inod_ele_max, num_sort_smp, nod_stack_smp, iele_sort_smp,     &
     &    iconn_sort_smp, idx_4_mat, k2, sk_v, nmat_size, aiccg)
!
      end subroutine add_skv1_2_matrix11_1st
!
!-----------------------------------------------------------------------
!
      subroutine add_skv1_2_matrix33_1st(k2, sk_v, nmat_size, aiccg33)
!
      use add_skv1_2_matrix
!
      integer (kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in)                                     &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind = kreal), intent(inout) :: aiccg33(-8:nmat_size)
!
!
      call add_skv1_2_matrix33(np_smp, numele, nnod_4_ele,              &
     &    inod_ele_max, num_sort_smp, nod_stack_smp, iele_sort_smp,     &
     &    iconn_sort_smp, idx_4_mat, k2, sk_v, nmat_size, aiccg33)
!
      end subroutine add_skv1_2_matrix33_1st
!
!-----------------------------------------------------------------------
!
      end module add_skv1_2_matrix_1st
