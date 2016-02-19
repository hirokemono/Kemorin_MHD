!cal_vector_p_correct.f90
!      module cal_vector_p_correct
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on March, 2006
!
!      subroutine cal_vector_p_co
!
      module cal_vector_p_correct
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
!
      use cal_multi_pass
      use cal_sol_vector_co_crank
!
      implicit none
!
      private :: cal_vector_p_co_exp, cal_vector_p_co_imp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_vector_p_co
!
      use m_group_data
      use m_phys_constants
      use m_jacobian_sf_grp
      use m_filter_elength
      use m_SGS_address
      use m_SGS_model_coefs
      use m_bc_data_magne
      use m_surf_data_magne_p
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use int_vol_solenoid_correct
      use int_surf_grad_sgs
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_magne_co'
      call int_vol_solenoid_co                                          &
     &   (ele1%istack_ele_smp, iphys%i_m_phi, iak_diff_b,               &
     &    node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l,                  &
     &    rhs_tbl1, FEM1_elen, fem1_wk, f1_nl)
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON                    &
     &     .and. sf_sgs1_grad_f%ngrp_sf_dat .gt. 0) then
        if (iflag_debug.eq.1) write(*,*)                                &
                             'int_surf_sgs_velo_co_ele', iphys%i_m_phi
         call int_surf_sgs_velo_co_ele                                  &
     &      (node1, ele1, surf1, sf_grp1, nod_fld1,                     &
     &       jac1_sf_grp_2d_q, jac1_sf_grp_2d_l, rhs_tbl1, FEM1_elen,   &
     &       intg_point_poisson,                                        &
     &       sf_sgs1_grad_f%ngrp_sf_dat, sf_sgs1_grad_f%id_grp_sf_dat,  &
     &       ifilter_final, ak_diff(1,iak_diff_b), iphys%i_m_phi,       &
     &       fem1_wk, f1_nl)
      end if
!
!
      if (   iflag_implicit_correct.eq.3                                &
     &  .or. iflag_implicit_correct.eq.4) then
        call cal_vector_p_co_imp
      else
        call cal_vector_p_co_exp
      end if
!
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_vect vect_p'
      call set_boundary_vect(nod_bc1_a, iphys%i_vecp, nod_fld1)
!
      if (iflag_debug.eq.1) write(*,*) 'vector_send_recv for vector_p'
      call vector_send_recv(iphys%i_vecp, node1, nod_comm, nod_fld1)
      if (iflag_debug.eq.1) write(*,*) 'scalar_send_recv for potential'
      call scalar_send_recv(iphys%i_mag_p, node1, nod_comm, nod_fld1)
!
      end subroutine cal_vector_p_co
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_vector_p_co_exp
!
      use m_node_phys_data
      use m_finite_element_matrix
      use cal_sol_vector_correct
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_multi_pass_4_vector_ff'
      call cal_multi_pass_4_vector_ff(ele1%istack_ele_smp, m1_lump,     &
     &    nod_comm, node1, ele1, jac1_3d_q, rhs_tbl1,                   &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_vect_p_co'
      call cal_sol_vector_co                                            &
     &   (nod_fld1%n_point, node1%istack_internal_smp, m1_lump%ml,      &
     &    f1_l%ff, nod_fld1%ntot_phys, iphys%i_vecp, nod_fld1%d_fld)
!
      end subroutine cal_vector_p_co_exp
!
! ----------------------------------------------------------------------
!
      subroutine cal_vector_p_co_imp
!
      use m_iccg_parameter
      use m_t_step_parameter
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
      use m_finite_element_matrix
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use m_SGS_address
      use m_ele_material_property
      use m_bc_data_magne
!
      use int_vol_diffusion_ele
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
      use copy_nodal_fields
      use cal_sol_vector_co_crank
!
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_vecp_diffuse_co'
      if (coef_imp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(ele1%istack_ele_smp,            &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      iak_diff_b, coef_imp_b, ak_d_magne, iphys%i_vecp,           &
     &      fem1_wk, f1_l)
      end if
!
      if (coef_imp_b.gt.0.0d0) then
        if (iflag_debug.eq.1) write(*,*) 'int_sk_4_fixed_vector_p'
        call int_sk_4_fixed_vector(iflag_commute_magne,                 &
     &      iphys%i_vecp, node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1,   &
     &      FEM1_elen, nod_bc1_a, ak_d_magne, coef_imp_b, iak_diff_b,   &
     &      fem1_wk, f1_l)
      end if
!
!
      if (     iflag_implicit_correct.eq.3) then
        call cal_magne_co_lumped_crank                                  &
     &     (iphys%i_vecp, nod_comm, node1, ele1, nod_fld1,              &
     &      iphys_ele, fld_ele1, nod_bc1_a, jac1_3d_q, rhs_tbl1,        &
     &      m1_lump, mhd_fem1_wk, fem1_wk, f1_l, f1_nl)
      else if (iflag_implicit_correct.eq.4) then
        call cal_magne_co_consist_crank(iphys%i_vecp, coef_magne,       &
     &      node1, ele1, conduct1, nod_fld1, nod_bc1_a, jac1_3d_q,      &
     &      rhs_tbl1, mhd_fem1_wk, fem1_wk, f1_l, f1_nl)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_vect_p_pre_crank'
      call solver_crank_vector                                          &
     &   (node1, DJDS_comm_etr, DJDS_entire, Bmat_DJDS, num_MG_level,   &
     &    MG_itp, MG_comm, MG_djds_tbl, MG_mat_magne,                   &
     &    method_4_velo, precond_4_crank, eps_4_magne_crank, itr,       &
     &    iphys%i_vecp, MG_vector, f1_l, b_vec, x_vec, nod_fld1)
!
      call clear_nodal_data(node1, nod_fld1, n_scalar, iphys%i_m_phi)
!
      end subroutine cal_vector_p_co_imp
!
! ----------------------------------------------------------------------
!
      end module cal_vector_p_correct
