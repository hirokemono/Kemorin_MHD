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
      use m_node_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
!
      use nodal_fld_2_each_ele_1st
      use fem_skv_nodal_field_type
      use cal_product_to_skv_1st
      use cal_skv_to_ff_smp_1st
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, iphys%i_sgs_simi, vect_e)
        call fem_skv_vector_type(iele_cd_smp_stack,                     &
     &      intg_point_t_evo, k2, ele1, jac1_3d_q, vect_e, fem1_wk%sk6)
        call scalar_prod_to_skv_tensor_1st(iele_cd_smp_stack,           &
     &      ak_sgs(1,icomp_sgs_uxb), fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_simi_vp_induct
!
!-----------------------------------------------------------------------
!
      subroutine int_simi_vp_induct_upm(ncomp_ele, iele_magne, d_ele)
!
      use m_jacobians
      use m_node_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
!
      use nodal_fld_2_each_ele_1st
      use fem_skv_nodal_fld_upw_type
      use cal_product_to_skv_1st
      use cal_skv_to_ff_smp_1st
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, iphys%i_sgs_simi, vect_e)
!
        call fem_skv_vector_field_upwind(iele_cd_smp_stack,             &
     &      intg_point_t_evo, k2, d_ele(1,iele_magne), ele1, jac1_3d_q, &
     &      vect_e, fem1_wk%sk6)
!
        call scalar_prod_to_skv_tensor_1st(iele_cd_smp_stack,           &
     &      ak_sgs(1,icomp_sgs_uxb), fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_simi_vp_induct_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_similarity_uxb
