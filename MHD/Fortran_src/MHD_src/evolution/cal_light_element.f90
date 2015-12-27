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
!
      implicit none
!
      private :: cal_composit_pre_euler, cal_composit_pre_adams
      private :: cal_composit_pre_crank, cal_composit_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_light_element_variation
!
      use m_finite_element_matrix
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobian_sf_grp
      use m_bc_data_ene
      use m_surf_data_composition
!
      use nod_phys_send_recv
      use set_boundary_scalars
      use int_surf_fixed_gradients
      use int_vol_diffusion_ele
      use int_vol_light_comp_ele
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_composition_diffuse_ele
!
      if (iflag_comp_supg .gt. id_turn_OFF) then
        call int_vol_composition_ele_upw                                &
     &     (fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
      else
        call int_vol_composition_ele                                    &
     &     (fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
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
       call cal_composit_pre_crank
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
      use m_geometry_data
      use m_node_phys_data
      use cal_multi_pass
      use cal_sol_vector_explicit
!
      call cal_t_evo_4_scalar_fl(iflag_comp_supg)
      call cal_sol_d_scalar_euler(node1, iphys, nod_fld1)
!
      end subroutine cal_composit_pre_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_adams
!
      use m_geometry_data
      use m_node_phys_data
      use cal_multi_pass
      use cal_sol_vector_explicit
!
      call cal_t_evo_4_scalar_fl(iflag_comp_supg)
      call cal_sol_d_scalar_adams(node1, iphys, nod_fld1)
!
      end subroutine cal_composit_pre_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_crank
!
      use m_geometry_data
      use m_t_step_parameter
      use m_node_phys_data
      use m_finite_element_matrix
      use m_bc_data_ene
!
      use cal_multi_pass
      use set_boundary_scalars
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
!
!
      if (coef_imp_c.gt.0.0d0) then
        call int_sk_4_fixed_composition
!         if (iflag_initial_step.eq.1) coef_imp_c = 1.0d0 / coef_imp_c
      end if
!
      call cal_t_evo_4_scalar_fl(iflag_comp_supg)
!
      call set_boundary_rhs_scalar(node1, nod_bc1_c, f1_l, f1_nl)
      call cal_sol_d_scalar_linear(node1, iphys, nod_fld1)
!
      call cal_sol_d_scalar_crank(iphys%i_light)
!
      end subroutine cal_composit_pre_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_consist_crank
!
      use m_geometry_data
      use m_t_step_parameter
      use m_node_phys_data
      use m_finite_element_matrix
      use m_bc_data_ene
      use m_physical_property
!
      use cal_sol_vector_pre_crank
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
!
       if (coef_imp_c.gt.0.0d0) then
         call int_sk_4_fixed_composition
!         if (iflag_initial_step.eq.1) coef_imp_c = 1.0d0 / coef_imp_c
       end if
!
       call int_vol_initial_d_scalar
       call set_ff_nl_smp_2_ff(n_scalar, node1, rhs_tbl1, f1_l, f1_nl)
!
      call set_boundary_rhs_scalar(node1, nod_bc1_c, f1_l, f1_nl)
!
      call cal_vector_pre_consist(node1, coef_light,                    &
     &    f1_nl%ff, n_scalar, iphys%i_pre_composit, nod_fld1, f1_l%ff)
!
       call cal_sol_d_scalar_crank(iphys%i_light)
!
       end subroutine cal_composit_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      end module cal_light_element
