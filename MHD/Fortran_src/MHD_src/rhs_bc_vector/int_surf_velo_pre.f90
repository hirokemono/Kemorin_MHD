!
!      module int_surf_velo_pre
!
!     Written by H. Matsui on June, 2005
!
!      subroutine int_surf_velo_pre_ele
!      subroutine int_surf_velo_monitor(i_field)
!
      module int_surf_velo_pre
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_data
      use m_group_data
      use m_jacobian_sf_grp
      use m_surf_data_torque
      use m_node_phys_data
      use m_sorted_node
      use m_finite_element_matrix
!
      use int_surf_div_fluxes_sgs
      use int_surf_fixed_gradients
      use int_free_slip_surf_sph
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_velo_pre_ele
!
      use m_filter_elength
      use m_SGS_model_coefs
      use m_SGS_address
      use m_surf_data_torque
      use m_surf_data_magne
!
!
      if (iflag_SGS_inertia  .ne. id_SGS_none) then
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux(node1, ele1, surf1, sf_grp1,   &
     &        nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,          &
     &        sf_sgs1_grad_v, intg_point_t_evo, ifilter_final,          &
     &        iphys%i_SGS_m_flux, iphys%i_velo, iphys%i_velo,           &
     &        ak_diff(1,iak_diff_mf), coef_velo, fem1_wk, f1_nl)
        end if
      end if
!
      if (iflag_SGS_lorentz .ne. id_SGS_none) then
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux(node1, ele1, surf1, sf_grp1,   &
     &        nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,          &
     &        sf_sgs1_grad_b, intg_point_t_evo, ifilter_final,          &
     &        iphys%i_SGS_maxwell, iphys%i_magne, iphys%i_magne,        &
     &        ak_diff(1,iak_diff_lor), (-coef_lor), fem1_wk, f1_nl)
        end if
      end if
!
!
        call int_sf_grad_velocity(node1, ele1, surf1, sf_grp1,          &
     &      jac1_sf_grp_2d_q, rhs_tbl1, sf_bc1_grad_v,                  &
     &      intg_point_t_evo, ak_d_velo, fem1_wk, f1_l)
        call int_free_slip_surf_sph_in(node1, ele1, surf1, sf_grp1,     &
     &      nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, intg_point_t_evo,     &
     &      sf_bc1_free_sph_in%ngrp_sf_dat,                             &
     &      sf_bc1_free_sph_in%id_grp_sf_dat,                           &
     &      iphys%i_velo, fem1_wk, f1_l)
        call int_free_slip_surf_sph_out(node1, ele1, surf1, sf_grp1,    &
     &      nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, intg_point_t_evo,     &
     &      sf_bc1_free_sph_out%ngrp_sf_dat,                            &
     &      sf_bc1_free_sph_out%id_grp_sf_dat,                          &
     &      iphys%i_velo, fem1_wk, f1_l)
!
      end subroutine int_surf_velo_pre_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_velo_monitor(i_field)
!
      use m_filter_elength
      use m_SGS_model_coefs
      use m_SGS_address
      use m_surf_data_torque
      use m_surf_data_magne
!
      integer(kind= kint), intent(in) :: i_field
!
!
      if (i_field .eq. iphys%i_SGS_div_m_flux) then
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux(node1, ele1, surf1, sf_grp1,   &
     &        nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,          &
     &        sf_sgs1_grad_v, intg_point_t_evo, ifilter_final,          &
     &        iphys%i_SGS_m_flux, iphys%i_velo, iphys%i_velo,           &
     &        ak_diff(1,iak_diff_mf), coef_velo, fem1_wk, f1_nl)
        end if
      end if
!
      if (i_field .eq. iphys%i_SGS_Lorentz) then
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          call int_sf_skv_sgs_div_t_flux(node1, ele1, surf1, sf_grp1,   &
     &        nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,          &
     &        sf_sgs1_grad_b, intg_point_t_evo, ifilter_final,          &
     &        iphys%i_SGS_maxwell, iphys%i_magne, iphys%i_magne,        &
     &        ak_diff(1,iak_diff_lor), (-coef_lor), fem1_wk, f1_nl)
        end if
      end if
!
!
      if (i_field .eq. iphys%i_v_diffuse) then
        call int_sf_grad_velocity(node1, ele1, surf1, sf_grp1,          &
     &      jac1_sf_grp_2d_q, rhs_tbl1, sf_bc1_grad_v,                  &
     &      intg_point_t_evo, ak_d_velo, fem1_wk, f1_l)
        call int_free_slip_surf_sph_in(node1, ele1, surf1, sf_grp1,     &
     &      nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, intg_point_t_evo,     &
     &      sf_bc1_free_sph_in%ngrp_sf_dat,                             &
     &      sf_bc1_free_sph_in%id_grp_sf_dat,                           &
     &      iphys%i_velo, fem1_wk, f1_l)
        call int_free_slip_surf_sph_out(node1, ele1, surf1, sf_grp1,    &
     &      nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, intg_point_t_evo,     &
     &      sf_bc1_free_sph_out%ngrp_sf_dat,                            &
     &      sf_bc1_free_sph_out%id_grp_sf_dat,                          &
     &      iphys%i_velo, fem1_wk, f1_l)
      end if
!
      end subroutine int_surf_velo_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_velo_pre
