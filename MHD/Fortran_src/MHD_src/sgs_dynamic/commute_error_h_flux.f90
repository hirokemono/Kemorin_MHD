!commute_error_h_flux.f90
!     module commute_error_h_flux
!
!     Written by H. Matsui
!
!      subroutine cal_commute_error_4_hf(i_filter, i_sgs, i_flux,       &
!     &          i_vect)
!         i_filter: ID for filter function
!         i_sgs: field ID for obtained difference term
!         i_flux: field ID for SGS term
!         i_vect: field ID for origianl vector field
!         i_scalar: field ID for origianl scalar field
!
!       subroutine cal_commute_error_4_h_flux(i_filter)
!       subroutine cal_commute_error_4_filter_hf(i_filter)
!
      module commute_error_h_flux
!
      use m_precision
!
      use m_control_parameter
!
      implicit none
!
      private :: cal_commute_error_4_hf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_4_hf(i_filter, i_sgs,                &
     &          i_flux, i_vect, i_scalar)
!
      use m_geometry_data
      use m_group_data
      use m_phys_constants
      use m_node_phys_data
      use m_jacobian_sf_grp
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
      use m_surf_data_temp
      use m_geometry_data_MHD
!
      use int_vol_commute_error
      use int_surf_div_fluxes_sgs
      use cal_ff_smp_to_ffs
      use cal_for_ffs
!
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
      integer(kind = kint), intent(in) :: i_sgs, i_filter
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_commute_div_v_flux(iele_fl_smp_stack,                &
     &    intg_point_t_evo, i_filter, i_flux, i_vect, i_scalar)
      call int_sf_skv_commute_sgs_v_flux(node1, ele1, surf1, sf_grp1,   &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,              &
     &    intg_point_t_evo,                                             &
     &    sf_sgs1_grad_t%ngrp_sf_dat, sf_sgs1_grad_t%id_grp_sf_dat,     &
     &    i_filter, i_flux, i_vect, i_scalar, fem1_wk, f1_nl)
!
      call set_ff_nl_smp_2_ff(n_scalar, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_scalar(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_fl%ml, nod_fld1%ntot_phys,        &
     &    i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_commute_error_4_hf
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_4_h_flux(i_filter)
!
      use m_node_phys_address
!
      integer(kind = kint), intent(in) :: i_filter
!
       call cal_commute_error_4_hf(i_filter, iphys%i_sgs_grad,          &
     &     iphys%i_SGS_h_flux, iphys%i_velo, iphys%i_sgs_temp)
!
      end subroutine cal_commute_error_4_h_flux
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_4_filter_hf(i_filter)
!
      use m_node_phys_address
!
      integer(kind = kint), intent(in) :: i_filter
!
       call cal_commute_error_4_hf(i_filter, iphys%i_sgs_grad_f,        &
     &     iphys%i_sgs_grad_f, iphys%i_filter_velo,                     &
     &     iphys%i_filter_temp)
!
       end subroutine cal_commute_error_4_filter_hf
!
!-----------------------------------------------------------------------
!
      end module commute_error_h_flux
