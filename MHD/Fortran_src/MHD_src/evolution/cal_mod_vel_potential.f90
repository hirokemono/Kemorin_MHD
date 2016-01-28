!
!      module cal_mod_vel_potential
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!      subroutine cal_mod_potential
!
      module cal_mod_vel_potential
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_mod_potential
!
      use m_geometry_data_MHD
      use m_geometry_data
      use m_group_data
      use m_phys_constants
      use m_node_phys_data
      use m_SGS_address
      use m_SGS_model_coefs
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_jacobians
      use m_jacobian_sf_grp
      use m_filter_elength
      use m_surf_data_torque
      use m_surf_data_press
      use m_bc_data_velo
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_sgs
      use int_surf_fixed_gradients
      use int_surf_normal_fields
      use set_boundary_scalars
      use set_nodal_bc_id_data
      use cal_solver_MHD
!
!
      call reset_ff(node1%numnod, f1_l)
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
!    take divergence of velocity
!
      call int_vol_fractional_div_ele                                   &
     &   (fluid1%istack_ele_fld_smp, iphys%i_velo, iak_diff_v,          &
     &    node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l,                  &
     &    rhs_tbl1, FEM1_elen, fem1_wk, f1_l)
!
      call int_surf_normal_velocity(iphys%i_velo, node1, ele1, surf1,   &
     &    sf_grp1, nod_fld1, jac1_sf_grp_2d_l, rhs_tbl1, fem1_wk, f1_l)
!
!      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
!        call int_surf_sgs_div_velo_ele                                 &
!     &     (node1, ele1, surf1, sf_grp1, nod_fld1,                     &
!     &      jac1_sf_grp_2d_q, jac1_sf_grp_2d_l, rhs_tbl1, FEM1_elen,   &
!     &      intg_point_poisson, sf_sgs1_grad_v%nmax_sf_dat,            &
!     &      sf_sgs1_grad_v%ngrp_sf_dat, sf_sgs1_grad_v%id_grp_sf_dat,  &
!     &      ifilter_final, ak_diff(1,iak_diff_v), iphys%i_velo,        &
!     &      fem1_wk, f1_l)
!      end if
!
!   set boundary condition for wall
!
      call int_sf_grad_press(node1, ele1, surf1, sf_grp1,               &
     &    jac1_sf_grp_2d_l, rhs_tbl1, sf_bc1_grad_p,                    &
     &    intg_point_poisson, fem1_wk, f1_l)
!
!   add boundary term for fixed velocity
!
      call int_vol_sk_po_bc(iphys%i_p_phi, iak_diff_v, node1, ele1,     &
     &     nod_fld1, jac1_3d_l, rhs_tbl1, FEM1_elen, fem1_wk, f1_l)
!
!   add boundary term for fixed pressure
!
      call set_boundary_ff(node1, nod_bc1_p, f1_l)
!
!   solve Poission equation
!
      call cal_sol_mod_po                                               &
     &   (node1, DJDS_comm_fl, DJDS_fl_l, Pmat_DJDS,                    &
     &    num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fll,            &
     &    MG_mat_press, MG_vector, iphys%i_p_phi, f1_l, b_vec,          &
     &    x_vec, nod_fld1)
!
      call set_boundary_scalar(nod_bc1_p, iphys%i_p_phi, nod_fld1)
!
      end subroutine cal_mod_potential
!
!-----------------------------------------------------------------------
!
      end module cal_mod_vel_potential
