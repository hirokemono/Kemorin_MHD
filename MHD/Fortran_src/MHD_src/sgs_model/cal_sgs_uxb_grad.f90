!
!      module cal_sgs_uxb_grad
!
!      Written by H. Matsui
!
!      subroutine cal_sgs_uxb_2_ff_grad(i_filter)
!
!      subroutine cal_sgs_uxb_grad_4_dyn
!      subroutine cal_sgs_filter_uxb_grad_4_dyn
!
      module cal_sgs_uxb_grad
!
      use m_precision
!
      use m_int_vol_data
!
      implicit none
!
      private :: cal_sgs_vp_induct_grad_no_coef
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_2_ff_grad(i_filter)
!
      use m_control_parameter
      use m_node_phys_address
      use m_physical_property
      use m_sorted_node
      use m_finite_element_matrix
      use m_SGS_model_coefs
      use m_SGS_address
!
      use int_vol_sgs_uxb
      use cal_skv_to_ff_smp_1st
      use product_model_coefs_to_sk
!
      integer (kind=kint), intent(in) :: i_filter
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      call sel_int_vol_sgs_uxb                                          &
     &   (i_filter, iphys%i_magne, i_dvx, fem1_wk, mhd_fem1_wk)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_vector(itype_SGS_uxb_coef,                &
     &    ak_sgs(1,icomp_sgs_uxb), fem1_wk%sk6)
!
      call add3_skv_coef_to_ff_v_smp(node1, ele1, rhs_tbl1,             &
     &    coef_induct, fem1_wk%sk6, ff_nl_smp)
!
      end subroutine cal_sgs_uxb_2_ff_grad
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_vp_induct_grad_no_coef(i_filter,               &
     &          i_sgs, i_field, id_dx)
!
      use m_phys_constants
      use m_node_phys_data
      use m_physical_property
      use m_sorted_node
      use m_finite_element_matrix
!
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp_1st
      use cal_for_ffs
      use nod_phys_send_recv
      use int_vol_sgs_uxb
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: id_dx
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
      call reset_ff_smp
!
      call sel_int_vol_sgs_uxb                                          &
     &   (i_filter, i_field, id_dx, fem1_wk, mhd_fem1_wk)
!
      call add3_skv_coef_to_ff_v_smp(node1, ele1, rhs_tbl1,             &
     &    coef_induct, fem1_wk%sk6, ff_smp)
      call cal_ff_smp_2_vector(node1, rhs_tbl1,                         &
     &    ff_smp, ml_cd, nod_fld1%ntot_phys, i_sgs, nod_fld1%d_fld)
!
! ----------   communications
!
      call vector_send_recv(nod_fld1%ntot_phys, i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_sgs_vp_induct_grad_no_coef
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_grad_4_dyn
!
      use m_control_parameter
      use m_node_phys_address
!
!
      call cal_sgs_vp_induct_grad_no_coef(ifilter_2delta,               &
     &    iphys%i_SGS_vp_induct, iphys%i_magne, i_dvx)
!
      end subroutine cal_sgs_uxb_grad_4_dyn
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_filter_uxb_grad_4_dyn
!
      use m_control_parameter
      use m_node_phys_address
!
!
      call cal_sgs_vp_induct_grad_no_coef(ifilter_4delta,               &
     &    iphys%i_sgs_grad_f, iphys%i_filter_magne, i_dfvx)
!
      end subroutine cal_sgs_filter_uxb_grad_4_dyn
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_uxb_grad
