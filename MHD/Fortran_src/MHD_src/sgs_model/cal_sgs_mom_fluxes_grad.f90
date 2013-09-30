!cal_sgs_mom_fluxes_grad.f90
!      module cal_sgs_mom_fluxes_grad
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine cal_sgs_m_flux_grad(i_filter)
!      subroutine cal_sgs_maxwell_grad(i_filter)
!      subroutine cal_sgs_m_flux_grad_4_dyn
!      subroutine cal_sgs_maxwell_grad_4_dyn
!
!      subroutine cal_sgs_filter_m_flux_grad
!      subroutine cal_sgs_filter_maxwell_grad
!      subroutine cal_sgs_filter_mf_grad_4_dyn
!      subroutine cal_sgs_filter_mxwl_grad_4_dyn
!         i_filter: filter ID for heat flux
!
      module cal_sgs_mom_fluxes_grad
!
      use m_precision
!
      implicit none
!
      private :: cal_sgs_m_flux_grad_w_coef
      private :: cal_sgs_m_flux_grad_no_coef
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_grad(i_filter)
!
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_address
      use m_int_vol_data
!
      integer (kind=kint), intent(in) :: i_filter
!
      call cal_sgs_m_flux_grad_w_coef(itype_SGS_m_flux_coef, i_filter,  &
     &    icomp_sgs_mf, iphys%i_SGS_m_flux, iphys%i_velo, i_dvx)
!
      end subroutine cal_sgs_m_flux_grad
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_maxwell_grad(i_filter)
!
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_address
      use m_int_vol_data
!
      integer (kind=kint), intent(in) :: i_filter
!
      call cal_sgs_m_flux_grad_w_coef(itype_SGS_maxwell_coef, i_filter, &
     &    icomp_sgs_lor, iphys%i_SGS_maxwell, iphys%i_magne, i_dbx)
!
      end subroutine cal_sgs_maxwell_grad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_grad_4_dyn
!
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_address
      use m_int_vol_data
!
!
      call cal_sgs_m_flux_grad_no_coef(ifilter_2delta,                  &
     &    iphys%i_SGS_m_flux, iphys%i_velo, i_dvx)
!
      end subroutine cal_sgs_m_flux_grad_4_dyn
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_maxwell_grad_4_dyn
!
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_address
      use m_int_vol_data
!
!
      call cal_sgs_m_flux_grad_no_coef(ifilter_2delta,                  &
     &    iphys%i_SGS_maxwell, iphys%i_magne, i_dbx)
!
      end subroutine cal_sgs_maxwell_grad_4_dyn
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_filter_m_flux_grad
!
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_address
      use m_int_vol_data
!
!
      call cal_sgs_m_flux_grad_w_coef(itype_SGS_m_flux_coef,            &
     &    ifilter_4delta, icomp_sgs_mf, iphys%i_sgs_grad_f,             &
     &    iphys%i_filter_velo, i_dfvx)
!
      end subroutine cal_sgs_filter_m_flux_grad
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_filter_maxwell_grad
!
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_address
      use m_int_vol_data
!
!
      call cal_sgs_m_flux_grad_w_coef(itype_SGS_maxwell_coef,           &
     &    ifilter_4delta, icomp_sgs_lor, iphys%i_sgs_grad_f,            &
     &    iphys%i_filter_magne, i_dfbx)
!
      end subroutine cal_sgs_filter_maxwell_grad
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_filter_mf_grad_4_dyn
!
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_address
      use m_int_vol_data
!
!
      call cal_sgs_m_flux_grad_no_coef(ifilter_4delta,                  &
     &    iphys%i_sgs_grad_f, iphys%i_filter_velo, i_dfvx)
!
      end subroutine cal_sgs_filter_mf_grad_4_dyn
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_filter_mxwl_grad_4_dyn
!
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_address
      use m_int_vol_data
!
!
      call cal_sgs_m_flux_grad_no_coef(ifilter_4delta,                  &
     &    iphys%i_sgs_grad_f, iphys%i_filter_magne, i_dfbx)
!
      end subroutine cal_sgs_filter_mxwl_grad_4_dyn
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_grad_w_coef(itype_csim, i_filter,       &
     &          icm_sgs, i_sgs, i_field, ie_dvx)
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
      use product_model_coefs_to_sk
!
      integer (kind=kint), intent(in) :: itype_csim
      integer (kind=kint), intent(in) :: i_filter, icm_sgs
      integer (kind=kint), intent(in) :: i_sgs, i_field
      integer (kind=kint), intent(in) :: ie_dvx
!
!
      call reset_sk6(n_sym_tensor)
      call reset_ff_t_smp
!
      call sel_int_vol_sgs_flux(iflag_velo_supg, i_filter,              &
     &    n_sym_tensor, i_field, ie_dvx, sk6)
!
!     set elemental model coefficients
!
      call prod_model_coefs_4_tensor(itype_csim,                        &
     &    ak_sgs(1,icm_sgs), sk6)
!
      call add6_skv_to_ff_t_smp_1st(ff_t_smp, sk6)
      call cal_ff_smp_2_tensor(d_nod(1,i_sgs), ff_t_smp, ml_fl)
!
! ----------   communications
!
      call sym_tensor_send_recv(i_sgs)
!
      end subroutine cal_sgs_m_flux_grad_w_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_grad_no_coef(i_filter, i_sgs,           &
     &          i_field, ie_dvx)
!
      use m_phys_constants
      use m_node_phys_data
      use m_finite_element_matrix
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
      call reset_sk6(n_sym_tensor)
      call reset_ff_t_smp
!
      call sel_int_vol_sgs_flux(iflag_velo_supg, i_filter,              &
     &   n_sym_tensor, i_field, ie_dvx, sk6)
!
      call add6_skv_to_ff_t_smp_1st(ff_t_smp, sk6)
      call cal_ff_smp_2_tensor(d_nod(1,i_sgs), ff_t_smp, ml_fl)
!
! ----------   communications
!
      call sym_tensor_send_recv(i_sgs)
!
      end subroutine cal_sgs_m_flux_grad_no_coef
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_mom_fluxes_grad
