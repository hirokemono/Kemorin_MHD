!
!     module int_div_sgs_hf_simi
!
!     Written by H. Matsui
!
!       subroutine int_div_sgs_hf_simi_pg(i_flux, i_vect, i_scalar)
!       subroutine int_div_sgs_hf_simi_upw(i_flux, i_vect, i_scalar,    &
!     &          ncomp_ele, iele_velo, d_ele)
!
      module int_div_sgs_hf_simi
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_div_sgs_hf_simi_pg(i_flux, i_vect, i_scalar)
!
      use m_node_phys_data
      use sgs_terms_2_each_ele
      use cal_skv_to_ff_smp_1st
      use fem_skv_vector_diff_type
!
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call SGS_v_flux_2_each_element                                  &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      ele1%istack_ele_smp, k2, nod_fld1%ntot_phys,                &
     &      i_vect, i_scalar, i_flux, nod_fld1%d_fld, vect_e)
        call fem_skv_divergence(iele_fl_smp_stack,                      &
     &      intg_point_t_evo, k2, ele1, jac1_3d_q, vect_e, sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_div_sgs_hf_simi_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_div_sgs_hf_simi_upw(i_flux, i_vect, i_scalar,      &
     &          ncomp_ele, iele_velo, d_ele)
!
      use m_node_phys_data
      use sgs_terms_2_each_ele
      use cal_skv_to_ff_smp_1st
      use fem_skv_vect_diff_upw_1st
!
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call SGS_v_flux_2_each_element                                  &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      ele1%istack_ele_smp, k2, nod_fld1%ntot_phys,                &
     &      i_vect, i_scalar, i_flux, nod_fld1%d_fld, vect_e)
        call fem_skv_divergence_upw(iele_fl_smp_stack,                  &
     &      intg_point_t_evo, k2, d_ele(1,iele_velo), vect_e, sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_div_sgs_hf_simi_upw
!
!-----------------------------------------------------------------------
!
      end module int_div_sgs_hf_simi
