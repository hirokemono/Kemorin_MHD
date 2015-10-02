!cal_sgs_heat_fluxes_grad.f90
!      module cal_sgs_heat_fluxes_grad
!
!      Written by H. Matsui
!
!      subroutine cal_sgs_h_flux_grad(i_filter)
!      subroutine cal_sgs_h_flux_grad_4_dyn
!      subroutine cal_sgs_filter_hf_grad
!      subroutine cal_sgs_filter_hf_grad_4_dyn
!         i_filter: filter ID for heat flux
!
      module cal_sgs_heat_fluxes_grad
!
      use m_precision
!
      implicit none
!
      private :: cal_sgs_h_flux_grad_w_coef
      private :: cal_sgs_h_flux_grad_no_coef
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_h_flux_grad(i_filter)
!
      use m_node_phys_address
      use m_int_vol_data
!
      integer (kind=kint), intent(in) :: i_filter
!
!
      call cal_sgs_h_flux_grad_w_coef(i_filter, iphys%i_SGS_h_flux,     &
     &    iphys%i_sgs_temp, i_dvx)
!
      end subroutine cal_sgs_h_flux_grad
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_h_flux_grad_4_dyn
!
      use m_control_parameter
      use m_node_phys_address
      use m_int_vol_data
!
!
      call cal_sgs_h_flux_grad_no_coef(ifilter_2delta,                  &
     &    iphys%i_SGS_h_flux, iphys%i_sgs_temp, i_dvx)
!
      end subroutine cal_sgs_h_flux_grad_4_dyn
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_filter_hf_grad
!
      use m_control_parameter
      use m_node_phys_address
      use m_int_vol_data
!
!
      call cal_sgs_h_flux_grad_w_coef(ifilter_4delta,                   &
     &    iphys%i_sgs_grad_f, iphys%i_filter_temp, i_dfvx)
!
      end subroutine cal_sgs_filter_hf_grad
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_filter_hf_grad_4_dyn
!
      use m_control_parameter
      use m_node_phys_address
      use m_int_vol_data
!
!
      call cal_sgs_h_flux_grad_no_coef(ifilter_4delta,                  &
     &    iphys%i_sgs_grad_f, iphys%i_filter_temp, i_dfvx)
!
      end subroutine cal_sgs_filter_hf_grad_4_dyn
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_h_flux_grad_w_coef(i_filter, i_sgs,            &
     &          i_field, ie_dvx)
!
      use m_control_parameter
      use m_phys_constants
      use m_node_phys_data
      use m_finite_element_matrix
      use m_SGS_model_coefs
      use m_SGS_address
!
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp_1st
      use nod_phys_send_recv
      use int_vol_sgs_flux
      use product_model_coefs_to_sk
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: ie_dvx
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
      call reset_ff_smp
!
      call sel_int_vol_sgs_flux(iflag_temp_supg, i_filter, n_vector,    &
     &    i_field, ie_dvx, fem1_wk%sk6)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_vector(itype_SGS_h_flux_coef,             &
     &    ak_sgs(1,icomp_sgs_hf), fem1_wk%sk6)
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, fem1_wk%sk6)
      call cal_ff_smp_2_vector                                          &
     &   (ff_smp, ml_fl, nod_fld1%ntot_phys, i_sgs, nod_fld1%d_fld)
!
! ----------   communications
!
      call vector_send_recv(nod_fld1%ntot_phys, i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_sgs_h_flux_grad_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_h_flux_grad_no_coef(i_filter, i_sgs,           &
     &          i_field, ie_dvx)
!
      use m_phys_constants
      use m_node_phys_data
      use m_finite_element_matrix
      use m_SGS_model_coefs
!
      use cal_ff_smp_to_ffs
      use cal_skv_to_ff_smp_1st
      use nod_phys_send_recv
      use int_vol_sgs_flux
!
      integer (kind=kint), intent(in) :: i_filter
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: ie_dvx
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
      call reset_ff_smp
!
      call sel_int_vol_sgs_flux(iflag_temp_supg, i_filter, n_vector,    &
     &   i_field, ie_dvx, fem1_wk%sk6)
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, fem1_wk%sk6)
      call cal_ff_smp_2_vector                                          &
     &   (ff_smp, ml_fl, nod_fld1%ntot_phys, i_sgs, nod_fld1%d_fld)
!
! ----------   communications
!
      call vector_send_recv(nod_fld1%ntot_phys, i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_sgs_h_flux_grad_no_coef
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_heat_fluxes_grad
