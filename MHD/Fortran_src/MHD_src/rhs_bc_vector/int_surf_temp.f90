!
!      module int_surf_temp
!
!     Written by H. Matsui on June, 2005
!
!      subroutine int_surf_temp_ele(node, ele, surf, sf_grp)
!      subroutine int_surf_temp_monitor(i_field)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!
      module int_surf_temp
!
      use m_precision
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
!
      use m_control_parameter
      use m_jacobian_sf_grp
!
      use int_surf_div_fluxes_sgs
      use int_surf_fixed_gradients
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_temp_ele(node, ele, surf, sf_grp)
!
      use m_node_phys_data
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_SGS_model_coefs
      use m_SGS_address
      use m_surf_data_temp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
!
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      call int_sf_h_flux(node, ele, surf, sf_grp,                       &
     &    jac1_sf_grp_2d_q, rhs_tbl1, sf_bc1_grad_t,                    &
     &    num_int, ak_d_temp, fem1_wk, f1_l)
!
      if (iflag_SGS_heat .ne. id_SGS_none                               &
     &     .and. iflag_commute_temp .eq. id_SGS_commute_ON) then
        call int_sf_skv_sgs_div_v_flux(node, ele, surf, sf_grp,         &
     &      nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, num_int,   &
     &      sf_sgs1_grad_t%ngrp_sf_dat, sf_sgs1_grad_t%id_grp_sf_dat,   &
     &      ifilter_final, iphys%i_SGS_h_flux, iphys%i_velo,            &
     &      iphys%i_temp, ak_diff(1,iak_diff_hf), coef_temp,            &
     &      fem1_wk, f1_nl)
      end if
!
      end subroutine int_surf_temp_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_temp_monitor                                  &
     &         (node, ele, surf, sf_grp, i_field)
!
      use m_node_phys_data
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_SGS_model_coefs
      use m_SGS_address
      use m_surf_data_temp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind= kint), intent(in) :: i_field
!
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      if (i_field .eq. iphys%i_t_diffuse) then
        call int_sf_h_flux(node, ele, surf, sf_grp,                     &
     &      jac1_sf_grp_2d_q, rhs_tbl1, sf_bc1_grad_t,                  &
     &      num_int, ak_d_temp, fem1_wk, f1_l)
      end if
!
      if (iflag_commute_heat .eq. id_SGS_commute_ON                     &
        .and. i_field .eq. iphys%i_SGS_div_h_flux) then
        call int_sf_skv_sgs_div_v_flux(node, ele, surf, sf_grp,         &
     &      nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, num_int,   &
     &      sf_sgs1_grad_t%ngrp_sf_dat, sf_sgs1_grad_t%id_grp_sf_dat,   &
     &      ifilter_final, iphys%i_SGS_h_flux, iphys%i_velo,            &
     &      iphys%i_temp, ak_diff(1,iak_diff_hf), coef_temp,            &
     &      fem1_wk, f1_nl)
      end if
!
      end subroutine int_surf_temp_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_temp
