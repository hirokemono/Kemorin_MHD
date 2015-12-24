!
!      module int_div_sgs_idct_simi
!
!     Written by H. Matsui on July, 2005
!     modified by H. Matsui on July, 2007
!
!      subroutine int_div_sgs_idct_simi_pg(i_flux, i_v, i_b)
!      subroutine int_div_sgs_idct_simi_upw(i_flux, i_v, i_b,           &
!     &          ncomp_ele, iele_velo, d_ele)

!
      module int_div_sgs_idct_simi
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_div_sgs_idct_simi_pg(i_flux, i_v, i_b)
!
      use m_node_phys_data
      use sgs_terms_2_each_ele
      use fem_skv_vector_diff_type
      use cal_skv_to_ff_smp
!
      integer(kind = kint), intent(in) :: i_flux, i_v, i_b
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
      do k2 = 1, ele1%nnod_4_ele
        call SGS_induct_2_each_element                                  &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      ele1%istack_ele_smp, k2, nod_fld1%ntot_phys,                &
     &      i_b, i_v, i_flux, nod_fld1%d_fld, fem1_wk%vector_1)
        call fem_skv_div_asym_tsr(conduct1%istack_ele_fld_smp,          &
     &      intg_point_t_evo, k2, ele1, jac1_3d_q,                      &
     &      fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_div_sgs_idct_simi_pg
!
! ----------------------------------------------------------------------
!
      subroutine int_div_sgs_idct_simi_upw(i_flux, i_v, i_b,            &
     &          ncomp_ele, iele_velo, d_ele)
!
      use m_node_phys_data
      use sgs_terms_2_each_ele
      use fem_skv_vect_diff_upw_type
      use cal_skv_to_ff_smp
!
      integer(kind = kint), intent(in) :: i_flux, i_v, i_b
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
      do k2 = 1, ele1%nnod_4_ele
        call SGS_induct_2_each_element                                  &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      ele1%istack_ele_smp, k2, nod_fld1%ntot_phys,                &
     &      i_b, i_v, i_flux, nod_fld1%d_fld, fem1_wk%vector_1)
        call fem_skv_div_as_tsr_upw(conduct1%istack_ele_fld_smp,        &
     &      intg_point_t_evo, k2, d_ele(1,iele_velo), ele1, jac1_3d_q,  &
     &      fem1_wk%vector_1, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_div_sgs_idct_simi_upw
!
! ----------------------------------------------------------------------
!
      end module int_div_sgs_idct_simi
