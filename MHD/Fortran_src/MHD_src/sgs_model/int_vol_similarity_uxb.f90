!int_vol_similarity_uxb.f90
!     module int_vol_similarity_uxb
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!
!      subroutine int_simi_vp_induct
!      subroutine int_simi_vp_induct_upm
!
      module int_vol_similarity_uxb
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
      use m_phys_constants
      use m_geometry_data_MHD
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_vol_data
!
      implicit none
!
      private :: int_simi_vp_induct, int_simi_vp_induct_upm
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_int_simi_vp_induct
!
      use m_element_phys_data
!
!
      if (iflag_mag_supg .eq. id_turn_ON) then
        call int_simi_vp_induct_upm                                     &
     &     (fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      else
        call int_simi_vp_induct
      end if
!
      end subroutine sel_int_simi_vp_induct
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_simi_vp_induct
!
      use m_jacobians
      use m_node_phys_data
      use m_SGS_model_coefs
      use m_SGS_address
!
      use nodal_fld_2_each_element
      use fem_skv_nodal_field_type
      use cal_products_within_skv
      use cal_skv_to_ff_smp
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(node1, ele1, nod_fld1,          &
     &      k2, iphys%i_sgs_simi, fem1_wk%vector_1)
        call fem_skv_vector_type(conduct1%istack_ele_fld_smp,           &
     &      intg_point_t_evo, k2, ele1, jac1_3d_q,                      &
     &      fem1_wk%vector_1, fem1_wk%sk6)
        call scalar_prod_to_tensor_skv                                  &
     &     (ele1, conduct1%istack_ele_fld_smp,                          &
     &      ak_sgs(1,icomp_sgs_uxb), fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_simi_vp_induct
!
!-----------------------------------------------------------------------
!
      subroutine int_simi_vp_induct_upm(ncomp_ele, iele_magne, d_ele)
!
      use m_jacobians
      use m_node_phys_data
      use m_SGS_model_coefs
      use m_SGS_address
!
      use nodal_fld_2_each_element
      use fem_skv_nodal_fld_upw_type
      use cal_products_within_skv
      use cal_skv_to_ff_smp
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(node1, ele1, nod_fld1,          &
     &      k2, iphys%i_sgs_simi, fem1_wk%vector_1)
!
        call fem_skv_vector_field_upwind(conduct1%istack_ele_fld_smp,   &
     &      intg_point_t_evo, k2, d_ele(1,iele_magne), ele1, jac1_3d_q, &
     &      fem1_wk%vector_1, fem1_wk%sk6)
!
        call scalar_prod_to_tensor_skv                                  &
     &     (ele1, conduct1%istack_ele_fld_smp,                          &
     &      ak_sgs(1,icomp_sgs_uxb), fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_simi_vp_induct_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_similarity_uxb
