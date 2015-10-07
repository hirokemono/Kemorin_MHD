!
!      module int_element_field_2_node
!
!     Written by H. Matsui on Oct., 2006
!
!      subroutine cal_ele_scalar_2_node(scalar_nod, scalar_ele)
!      subroutine cal_ele_vector_2_node(vector_nod, vector_ele)
!      subroutine cal_ele_sym_tensor_2_node(tensor_nod, tensor_ele)
!
!      subroutine int_area_ele_scalar_2_node(iele_fsmp_stack,           &
!     &          scalar_ele)
!      subroutine int_area_ele_vector_2_node(iele_fsmp_stack,           &
!     &          vector_nod, vector_ele)
!
!      subroutine int_grp_ele_scalar_2_node(numele, iele_fsmp_stack,    &
!     &          nele_grp, iele_grp, scalar_ele)
!      subroutine int_grp_ele_vector_2_node(numele, iele_fsmp_stack,    &
!     &          nele_grp, iele_grp, vector_ele)
!
      module int_element_field_2_node
!
      use m_precision
!
      use m_geometry_constants
      use m_geometry_data
      use m_phys_constants
      use m_jacobians
      use m_fem_gauss_int_coefs
      use m_finite_element_matrix
!
      use fem_skv_mass_mat_type
      use fem_skv_nodal_field_type
      use cal_skv_to_ff_smp_1st
!
      implicit none
!
      private :: int_grp_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_ele_scalar_2_node(scalar_nod, scalar_ele)
!
      use cal_ff_smp_to_ffs
!
      real(kind = kreal), intent(in) :: scalar_ele(ele1%numele)
      real(kind = kreal), intent(inout) :: scalar_nod(node1%numnod)
!
!
      call int_area_ele_scalar_2_node(ele1%istack_ele_smp, scalar_ele)
      call cal_ff_smp_2_scalar(node1, rhs_tbl1, ff_smp, ml,             &
     &    n_scalar, ione, scalar_nod)
!
      end subroutine cal_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_ele_vector_2_node(vector_nod, vector_ele)
!
      use cal_ff_smp_to_ffs
!
      real(kind = kreal), intent(in)                                    &
     &                   :: vector_ele(ele1%numele,n_vector)
      real(kind = kreal), intent(inout)                                 &
     &                   :: vector_nod(node1%numnod,n_vector)
!
!
      call int_area_ele_vector_2_node(ele1%istack_ele_smp, vector_ele)
      call cal_ff_smp_2_vector(node1, rhs_tbl1, ff_smp, ml,             &
     &    n_vector, ione, vector_nod)
!
      end subroutine cal_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_ele_sym_tensor_2_node(tensor_nod, tensor_ele)
!
      use cal_ff_smp_to_ffs
!
      real(kind = kreal), intent(in)                                    &
     &                   :: tensor_ele(ele1%numele,n_sym_tensor)
      real(kind = kreal), intent(inout)                                 &
     &                   :: tensor_nod(node1%numnod,n_sym_tensor)
!
!
      call cal_ele_vector_2_node(tensor_nod(1,1), tensor_ele(1,1))
      call cal_ele_vector_2_node(tensor_nod(1,4), tensor_ele(1,4))
!
      end subroutine cal_ele_sym_tensor_2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_area_ele_scalar_2_node(iele_fsmp_stack,            &
     &          scalar_ele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: scalar_ele(ele1%numele)
!
!
      ff_smp = 0.0d0
      call reset_sk6(n_scalar, fem1_wk%sk6)
!
      if (ele1%nnod_4_ele .eq. num_t_linear) then
        call fem_skv_scalar_on_ele_type(iele_fsmp_stack,                &
     &      max_int_point, ele1, jac1_3d_q, scalar_ele, fem1_wk%sk6)
      else
        call fem_skv_mass_mat_diag_HRZ_type(iele_fsmp_stack,            &
     &      max_int_point, ele1, jac1_3d_q, fem1_wk%sk6)
        call fem_skv_scalar_on_ele_HRZ_type(iele_fsmp_stack,            &
     &      fem1_wk%me_diag, ele1, scalar_ele, fem1_wk%sk6)
      end if
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, fem1_wk%sk6)
!
      end subroutine int_area_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_area_ele_vector_2_node(iele_fsmp_stack,            &
     &          vector_ele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: vector_ele(ele1%numele,3)
!
!
      ff_smp = 0.0d0
      call reset_sk6(n_vector, fem1_wk%sk6)
!
      if (ele1%nnod_4_ele .eq. num_t_linear) then
        call fem_skv_vector_on_ele_type(iele_fsmp_stack,                &
     &      max_int_point, ele1, jac1_3d_q, vector_ele, fem1_wk%sk6)
      else
        call fem_skv_mass_mat_diag_HRZ_type(iele_fsmp_stack,            &
     &      max_int_point, ele1, jac1_3d_q, fem1_wk%sk6)
        call fem_skv_vector_on_ele_HRZ_type(iele_fsmp_stack,            &
     &      fem1_wk%me_diag, ele1, vector_ele, fem1_wk%sk6)
      end if
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, fem1_wk%sk6)
!
      end subroutine int_area_ele_vector_2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_grp_ele_scalar_2_node(numele, iele_fsmp_stack,     &
     &          nele_grp, iele_grp, scalar_ele)
!
      integer (kind=kint), intent(in) :: numele, nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: scalar_ele(numele)
!
!
      ff_smp = 0.0d0
      call reset_sk6(n_scalar, fem1_wk%sk6)
!
      if (ele1%nnod_4_ele .eq. num_t_linear) then
        call fem_skv_scalar_on_ele_grp_type(iele_fsmp_stack,            &
     &      nele_grp, iele_grp, max_int_point, ele1, jac1_3d_q,         &
     &      scalar_ele, fem1_wk%sk6)
      else
        call fem_grp_skv_mass_mat_diag_HRZ_t(iele_fsmp_stack,           &
     &      nele_grp, iele_grp, max_int_point, ele1, jac1_3d_q,         &
     &      fem1_wk%sk6)
        call fem_skv_scalar_on_egrp_HRZ_type(iele_fsmp_stack,           &
     &      nele_grp, iele_grp, fem1_wk%me_diag, ele1, scalar_ele,      &
     &      fem1_wk%sk6)
      end if
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, fem1_wk%sk6)
!
      end subroutine int_grp_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_grp_ele_vector_2_node(numele, iele_fsmp_stack,     &
     &          nele_grp, iele_grp, vector_ele)
!
      integer (kind=kint), intent(in) :: numele, nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: vector_ele(numele,3)
!
!
      ff_smp = 0.0d0
      call reset_sk6(n_vector, fem1_wk%sk6)
!
      if (ele1%nnod_4_ele .eq. num_t_linear) then
        call fem_skv_vector_on_ele_grp_type(iele_fsmp_stack,            &
     &      nele_grp, iele_grp, max_int_point, ele1, jac1_3d_q,         &
     &      vector_ele, fem1_wk%sk6)
      else
        call fem_grp_skv_mass_mat_diag_HRZ_t(iele_fsmp_stack,           &
     &      nele_grp, iele_grp,  max_int_point, ele1, jac1_3d_q,        &
     &      fem1_wk%sk6)
        call fem_skv_vector_on_egrp_HRZ_type(iele_fsmp_stack,           &
     &      nele_grp, iele_grp, fem1_wk%me_diag, ele1, vector_ele,      &
     &      fem1_wk%sk6)
      end if
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, fem1_wk%sk6)
!
      end subroutine int_grp_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      end module int_element_field_2_node
