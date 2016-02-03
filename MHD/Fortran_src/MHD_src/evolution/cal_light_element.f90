!
!      module cal_light_element
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!      subroutine cal_light_element_variation
!
      module cal_light_element
!
      use m_precision
!
      use calypso_mpi
      use m_control_parameter
      use m_phys_constants
      use m_t_int_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
!
      implicit none
!
      private :: cal_composit_pre_euler, cal_composit_pre_adams
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_light_element_variation
!
      use m_group_data
      use m_jacobian_sf_grp
      use m_bc_data_ene
      use m_surf_data_composition
      use m_SGS_address
!
      use nod_phys_send_recv
      use set_boundary_scalars
      use int_surf_fixed_gradients
      use int_vol_diffusion_ele
      use int_vol_light_comp_ele
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      if (coef_light.gt.zero .and. coef_exp_c.gt.zero) then
        call int_vol_scalar_diffuse_ele(fluid1%istack_ele_fld_smp,      &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      iak_diff_c, coef_exp_c, ak_d_composit, iphys%i_light,       &
     &      fem1_wk, f1_l)
      end if
!
      if (iflag_comp_supg .gt. id_turn_OFF) then
        call int_vol_composition_ele_upw                                &
     &     (node1, ele1, fluid1, iphys, nod_fld1, jac1_3d_q, rhs_tbl1,  &
     &     fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,        &
     &     fem1_wk, f1_nl)
      else
        call int_vol_composition_ele                                    &
     &     (node1, ele1, fluid1, iphys, nod_fld1, jac1_3d_q, rhs_tbl1,  &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,       &
     &      fem1_wk, f1_nl)
      end if
!
!
      call int_sf_h_flux(node1, ele1, surf1, sf_grp1,                   &
     &    jac1_sf_grp_2d_q, rhs_tbl1, sf_bc1_grad_c,                    &
     &    intg_point_t_evo, ak_d_composit, fem1_wk, f1_l)
!
!
      if     (iflag_t_evo_4_composit .eq. id_explicit_euler) then
       call cal_composit_pre_euler
!
      else if(iflag_t_evo_4_composit .eq. id_explicit_adams2) then
       call cal_composit_pre_adams
!
      else if(iflag_t_evo_4_composit .eq. id_Crank_nicolson) then
       call cal_composit_pre_lumped_crank
!
      else if(iflag_t_evo_4_composit .eq. id_Crank_nicolson_cmass) then
       call cal_composit_pre_consist_crank
      end if
!
      call set_boundary_scalar(nod_bc1_c, iphys%i_light, nod_fld1)
!
      call scalar_send_recv(iphys%i_light, node1, nod_comm, nod_fld1)
!
      end subroutine cal_light_element_variation
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_euler
!
      use cal_multi_pass
      use cal_sol_vector_explicit
!
      call cal_t_evo_4_scalar(iflag_comp_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_d_scalar_euler(node1, iphys, nod_fld1)
!
      end subroutine cal_composit_pre_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_adams
!
      use cal_multi_pass
      use cal_sol_vector_explicit
!
!
      call cal_t_evo_4_scalar(iflag_comp_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_d_scalar_adams(node1, iphys, nod_fld1)
!
      end subroutine cal_composit_pre_adams
!
! ----------------------------------------------------------------------
!
      end module cal_light_element
