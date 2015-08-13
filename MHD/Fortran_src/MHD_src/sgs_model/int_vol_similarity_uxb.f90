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
      use cal_skv_to_ff_smp_1st
!
!
      call reset_sk6(n_vector)
!
      if (iflag_mag_supg .eq. id_turn_ON) then
        call int_simi_vp_induct_upm
      else
        call int_simi_vp_induct
      end if
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine sel_int_simi_vp_induct
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_simi_vp_induct
!
      use m_node_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
!
      use nodal_fld_2_each_ele_1st
      use fem_skv_nodal_field_1st
      use cal_product_to_skv_1st
!
      integer(kind = kint) :: k2
!
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, iphys%i_sgs_simi, vect_e)
        call fem_skv_vector_1st(iele_cd_smp_stack,                      &
     &      intg_point_t_evo, k2, vect_e, sk6)
        call scalar_prod_to_skv_tensor_1st(iele_cd_smp_stack,           &
     &      ak_sgs(1,icomp_sgs_uxb), sk6)
      end do
!
      end subroutine int_simi_vp_induct
!
!-----------------------------------------------------------------------
!
      subroutine int_simi_vp_induct_upm
!
      use m_node_phys_address
      use m_element_phys_address
      use m_element_phys_data
      use m_SGS_model_coefs
      use m_SGS_address
!
      use nodal_fld_2_each_ele_1st
      use fem_skv_nodal_fld_upw_1st
      use cal_product_to_skv_1st
!
      integer(kind = kint) :: k2
!
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, iphys%i_sgs_simi, vect_e)
!
        call fem_skv_vector_field_upw_1st(iele_cd_smp_stack,            &
     &      intg_point_t_evo, k2, d_ele(1,iphys_ele%i_magne),           &
     &      vect_e, sk6)
!
        call scalar_prod_to_skv_tensor_1st(iele_cd_smp_stack,           &
     &      ak_sgs(1,icomp_sgs_uxb), sk6)
      end do
!
      end subroutine int_simi_vp_induct_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_similarity_uxb
