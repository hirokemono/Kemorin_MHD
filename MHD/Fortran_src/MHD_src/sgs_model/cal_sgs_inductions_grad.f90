!
!      module cal_sgs_inductions_grad
!
!      Written by H. Matsui
!
!      subroutine cal_sgs_induct_t_grad(i_filter)
!      subroutine cal_sgs_induct_t_grad_4_dyn
!      subroutine cal_sgs_filter_induct_grad
!      subroutine cal_sgs_filter_idt_grad_4_dyn
!
      module cal_sgs_inductions_grad
!
      use m_precision
!
      use m_int_vol_data
!
      implicit none
!
      private :: cal_sgs_induct_t_grad_w_coef
      private :: cal_sgs_induct_t_grad_no_coef
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_grad_w_coef(i_filter, i_sgs,         &
     &          ifield_v, ifield_b, ie_dvx, ie_dbx)
!
      use m_control_parameter
      use m_node_phys_data
      use m_physical_property
      use m_finite_element_matrix
      use m_SGS_model_coefs
      use m_SGS_address
!
      use int_vol_sgs_induct_t
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp_1st
      use nod_phys_send_recv
      use product_model_coefs_to_sk
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, ifield_v, ifield_b
      integer (kind=kint), intent(in) :: ie_dvx, ie_dbx
!
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_sk6(n_asym_tensor)
      call reset_ff_smp
!
      call sel_int_vol_sgs_induct_t(i_filter, ie_dvx, ie_dbx,           &
     &    ifield_v, ifield_b, sk6)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_asym_t(itype_SGS_uxb_coef,                &
     &    ak_sgs(1,icomp_sgs_uxb), sk6)
!
      call add3_skv_coef_to_ff_v_smp_1st(coef_induct, ff_smp, sk6)
      call cal_ff_smp_2_vector(d_nod(1,i_sgs), ff_smp, ml_cd)
!
! ----------   communications
!
      call vector_send_recv(num_tot_nod_phys, i_sgs, d_nod)
!
      end subroutine cal_sgs_induct_t_grad_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_grad_no_coef(i_filter, i_sgs,         &
     &          ifield_v, ifield_b, ie_dvx, ie_dbx)
!
      use m_control_parameter
      use m_node_phys_data
      use m_physical_property
      use m_finite_element_matrix
!
      use int_vol_sgs_induct_t
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp_1st
      use nod_phys_send_recv
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, ifield_v, ifield_b
      integer (kind=kint), intent(in) :: ie_dvx, ie_dbx
!
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_sk6(n_asym_tensor)
      call reset_ff_smp
!
      call sel_int_vol_sgs_induct_t(i_filter, ie_dvx, ie_dbx,           &
     &    ifield_v, ifield_b, sk6)
!
      call add3_skv_coef_to_ff_v_smp_1st(coef_induct, ff_smp, sk6)
      call cal_ff_smp_2_vector(d_nod(1,i_sgs), ff_smp, ml_cd)
!
! ----------   communications
!
      call vector_send_recv(num_tot_nod_phys, i_sgs, d_nod)
!
      end subroutine cal_sgs_induct_t_grad_no_coef
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_grad(i_filter)
!
      use m_node_phys_address
!
      integer (kind = kint), intent(in):: i_filter
!
      call cal_sgs_induct_t_grad_w_coef(i_filter, iphys%i_SGS_induct_t, &
     &    iphys%i_velo, iphys%i_magne, i_dvx, i_dbx)
!
      end subroutine cal_sgs_induct_t_grad
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_grad_4_dyn
!
      use m_control_parameter
      use m_node_phys_address
!
!
      call cal_sgs_induct_t_grad_no_coef(ifilter_2delta,                &
     &    iphys%i_SGS_induct_t, iphys%i_velo, iphys%i_magne,            &
     &    i_dvx, i_dbx)
!
      end subroutine cal_sgs_induct_t_grad_4_dyn
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_filter_induct_grad
!
      use m_control_parameter
      use m_node_phys_address
!
      call cal_sgs_induct_t_grad_w_coef(ifilter_4delta,                 &
     &    iphys%i_sgs_grad_f, iphys%i_filter_velo,                      &
     &    iphys%i_filter_magne, i_dfvx, i_dfbx)
!
      end subroutine cal_sgs_filter_induct_grad
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_filter_idt_grad_4_dyn
!
      use m_control_parameter
      use m_node_phys_address
!
      call cal_sgs_induct_t_grad_no_coef(ifilter_4delta,                &
     &    iphys%i_sgs_grad_f, iphys%i_filter_velo,                      &
     &    iphys%i_filter_magne, i_dfvx, i_dfbx)
!
      end subroutine cal_sgs_filter_idt_grad_4_dyn
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_inductions_grad
