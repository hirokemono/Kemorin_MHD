!
!     module int_div_sgs_mf_simi
!
!     numerical integration for finite elememt equations of momentum
!
!        Written by H.Matsui   on July 2005
!        modified by H. Matsui on Oct., 2005
!
!      subroutine int_div_sgs_mf_simi_pg(i_flux, i_vect)
!      subroutine int_div_sgs_mf_simi_upw(i_flux, i_vect)
!      subroutine int_div_sgs_mf_simi_upm(i_flux, i_vect)
!
      module int_div_sgs_mf_simi
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      private :: int_div_sgs_mf_simi_upwind
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_div_sgs_mf_simi_pg(i_flux, i_vect)
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      use sgs_terms_2_each_ele
      use fem_skv_vector_diff_type
      use cal_skv_to_ff_smp_1st
!
      integer(kind = kint), intent(in) :: i_flux, i_vect
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call SGS_m_flux_2_each_element                                  &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      ele1%istack_ele_smp, k2, nod_fld1%ntot_phys,                &
     &      i_vect, i_flux, nod_fld1%d_fld, tensor_e)
        call fem_skv_div_tensor(iele_fl_smp_stack,                      &
     &      intg_point_t_evo, k2, ele1, jac1_3d_q, tensor_e, sk6)
      end do
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_div_sgs_mf_simi_pg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_div_sgs_mf_simi_upw(i_flux, i_vect)
!
      use m_element_phys_data
!
      integer(kind = kint), intent(in) :: i_flux, i_vect
!
      call int_div_sgs_mf_simi_upwind(i_flux, i_vect,                   &
     &    fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
!
      end subroutine int_div_sgs_mf_simi_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_div_sgs_mf_simi_upm(i_flux, i_vect)
!
      use m_element_phys_data
!
      integer(kind = kint), intent(in) :: i_flux, i_vect
!
      call int_div_sgs_mf_simi_upwind(i_flux, i_vect,                   &
     &    fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
!
      end subroutine int_div_sgs_mf_simi_upm
!
!-----------------------------------------------------------------------
!
      subroutine int_div_sgs_mf_simi_upwind(i_flux, i_vect,             &
     &          ncomp_ele, ie_upw, d_ele)
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      use sgs_terms_2_each_ele
      use cal_skv_to_ff_smp_1st
      use fem_skv_vect_diff_upw_type
!
      integer(kind = kint), intent(in) :: i_flux, i_vect
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call SGS_m_flux_2_each_element                                  &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      ele1%istack_ele_smp, k2, nod_fld1%ntot_phys,                &
     &      i_vect, i_flux, nod_fld1%d_fld, tensor_e)
        call fem_skv_div_tsr_upw(iele_fl_smp_stack, intg_point_t_evo,   &
     &      k2, d_ele(1,ie_upw), ele1, jac1_3d_q, tensor_e, sk6)
      end do
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_div_sgs_mf_simi_upwind
!
!-----------------------------------------------------------------------
!
      end module int_div_sgs_mf_simi
