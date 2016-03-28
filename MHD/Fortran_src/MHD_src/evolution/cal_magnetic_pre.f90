!
!      module cal_magnetic_pre
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_magnetic_field_pre(nod_comm, node, ele, surf,    &
!!     &         conduct, sf_grp, Bnod_bcs, Asf_bcs, Bsf_bcs, iphys,    &
!!     &         iphys_ele, ele_fld, jac_3d_q, jac_sf_grp_q, rhs_tbl,   &
!!     &         FEM_elens, filtering, num_MG_level, MG_interpolate,    &
!!     &         MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector, &
!!     &         mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_magnetic_co                                      &
!!     &         (nod_comm, node, ele, surf, conduct, sf_grp,           &
!!     &          Bnod_bcs, Fsf_bcs, iphys, iphys_ele, ele_fld,         &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,       &
!!     &          rhs_tbl, FEM_elens, num_MG_level, MG_interpolate,     &
!!     &          MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,&
!!     &          m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_magnetic_co_outside(nod_comm, node, ele, surf,   &
!!     &          insulate, sf_grp, Bnod_bcs, Fsf_bcs, iphys,           &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,       &
!!     &          rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk,               &
!!     &          f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(field_geometry_data), intent(in) :: insulate
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
!!        type(communication_table), intent(in)                         &
!!       &           :: MG_comm_table(0:num_MG_level)
!!        type(DJDS_ordering_table), intent(in)                         &
!!       &           :: MG_DJDS_table(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Bmat_MG_DJDS(0:num_MG_level)
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_magnetic_pre
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_phys_constants
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_solver_djds
      use t_interpolate_table
      use t_vector_for_solver
      use t_bc_data_magne
      use t_surface_bc_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_field_pre(nod_comm, node, ele, surf,      &
     &         conduct, sf_grp, Bnod_bcs, Asf_bcs, Bsf_bcs, iphys,      &
     &         iphys_ele, ele_fld, jac_3d_q, jac_sf_grp_q, rhs_tbl,     &
     &         FEM_elens, filtering, num_MG_level, MG_interpolate,      &
     &         MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,   &
     &         mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use calypso_mpi
      use m_t_int_parameter
      use m_SGS_address
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use int_vol_diffusion_ele
      use int_vol_magne_pre
      use int_surf_magne_pre
      use evolve_by_1st_euler
      use evolve_by_adams_bashforth
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: conduct
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_table(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_table(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Bmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call cal_sgs_magne_induction(icomp_sgs_uxb, ie_dvx, ie_dbx,     &
     &     nod_comm, node, ele, conduct, iphys, iphys_ele, ele_fld,     &
     &     jac_3d_q, rhs_tbl, FEM_elens, filtering, mhd_fem_wk, fem_wk, &
     &     f_l, nod_fld)
      end if
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
! lead diffusion term
!
      if (coef_magne.gt.zero .and. coef_exp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(conduct%istack_ele_fld_smp,     &
     &      node, ele, nod_fld, jac_3d_q, rhs_tbl, FEM_elens,           &
     &      diff_coefs%num_field, iak_diff_b, diff_coefs%ak,            &
     &      coef_exp_b, ak_d_magne, iphys%i_magne, fem_wk, f_l)
      end if
!
! lead induction terms
!
      if (iflag_debug .eq. 0 ) write(*,*) 'coefs_4_time_evolution'
      if (iflag_mag_supg .gt. id_turn_OFF) then
       call int_vol_magne_pre_ele_upm                                   &
     &    (node, ele, conduct, iphys, nod_fld,                          &
     &     ele_fld%ntot_phys, ele_fld%d_fld, iphys_ele, iak_diff_uxb,   &
     &     jac_3d_q, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
      else
       call int_vol_magne_pre_ele(node, ele, conduct, iphys, nod_fld,   &
     &     ele_fld%ntot_phys, ele_fld%d_fld, iphys_ele, iak_diff_uxb,   &
     &     jac_3d_q, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
      end if
!
!
      call int_surf_magne_pre_ele                                       &
     &   (iak_diff_uxb, node, ele, surf, sf_grp, Asf_bcs, Bsf_bcs,      &
     &    iphys, nod_fld, jac_sf_grp_q, rhs_tbl, FEM_elens,             &
     &    fem_wk, f_l, f_nl)
!
      if (iflag_t_evo_4_magne .eq. id_explicit_euler) then
        call cal_magne_pre_euler(iflag_mag_supg, iphys%i_magne,         &
     &      nod_comm, node, ele, conduct, iphys_ele, ele_fld,           &
     &      jac_3d_q, rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (iflag_t_evo_4_magne .eq. id_explicit_adams2) then
        call cal_magne_pre_adams                                        &
     &     (iflag_mag_supg, iphys%i_magne, iphys%i_pre_uxb,             &
     &      nod_comm, node, ele, conduct, iphys_ele, ele_fld,           &
     &      jac_3d_q, rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (iflag_t_evo_4_magne .eq. id_Crank_nicolson) then
        call cal_magne_pre_lumped_crank                                 &
     &     (iphys%i_magne, iphys%i_pre_uxb, iak_diff_b,                 &
     &      Bnod_bcs%nod_bc_b, nod_comm, node, ele, conduct,            &
     &      iphys_ele, ele_fld, jac_3d_q, rhs_tbl, FEM_elens,           &
     &      num_MG_level, MG_interpolate, MG_comm_table,                &
     &      MG_DJDS_table, Bmat_MG_DJDS, MG_vector,                     &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (iflag_t_evo_4_magne .eq. id_Crank_nicolson_cmass) then 
        call cal_magne_pre_consist_crank                                &
     &     (iphys%i_magne, iphys%i_pre_uxb, iak_diff_b,                 &
     &      Bnod_bcs%nod_bc_b, node, ele, conduct, jac_3d_q, rhs_tbl,   &
     &      FEM_elens, num_MG_level, MG_interpolate, MG_comm_table,     &
     &      MG_DJDS_table, Bmat_MG_DJDS, MG_vector,                     &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      call set_boundary_vect                                            &
     &   (Bnod_bcs%nod_bc_b, iphys%i_magne, nod_fld)
!
      call vector_send_recv(iphys%i_magne, node, nod_comm, nod_fld)
!
      end subroutine cal_magnetic_field_pre
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_co                                        &
     &         (nod_comm, node, ele, surf, conduct, sf_grp,             &
     &          Bnod_bcs, Fsf_bcs, iphys, iphys_ele, ele_fld,           &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,         &
     &          rhs_tbl, FEM_elens, num_MG_level, MG_interpolate,       &
     &          MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,  &
     &          m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_SGS_address
      use m_SGS_model_coefs
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use int_vol_solenoid_correct
      use int_surf_grad_sgs
      use implicit_vector_correct
      use cal_multi_pass
      use cal_sol_vector_co_crank
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: conduct
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_table(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_table(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Bmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_magne_co'
      call int_vol_solenoid_co                                          &
     &   (ele%istack_ele_smp, iphys%i_m_phi, iak_diff_b,                &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l,                       &
     &    rhs_tbl, FEM_elens, fem_wk, f_nl)
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON                    &
     &     .and. Fsf_bcs%sgs%ngrp_sf_dat .gt. 0) then
        if (iflag_debug.eq.1) write(*,*)                                &
                             'int_surf_sgs_velo_co_ele', iphys%i_m_phi
         call int_surf_sgs_velo_co_ele(node, ele, surf, sf_grp,         &
     &       nod_fld, jac_sf_grp_q, jac_sf_grp_l,                       &
     &       rhs_tbl, FEM_elens, intg_point_poisson,                    &
     &       Fsf_bcs%sgs%ngrp_sf_dat, Fsf_bcs%sgs%id_grp_sf_dat,        &
     &       ifilter_final, diff_coefs%num_field, iak_diff_b,           &
     &       diff_coefs%ak, iphys%i_m_phi, fem_wk, f_nl)
      end if
!
!
!
      if (   iflag_implicit_correct.eq.3                                &
     &  .or. iflag_implicit_correct.eq.4) then
        call cal_magnetic_co_imp(iphys%i_magne,                         &
     &      nod_comm, node, ele, conduct, Bnod_bcs, iphys_ele, ele_fld, &
     &      jac_3d_q, rhs_tbl, FEM_elens, num_MG_level, MG_interpolate, &
     &      MG_comm_table, MG_DJDS_table, Bmat_MG_DJDS, MG_vector,      &
     &      m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else
        call cal_magnetic_co_exp(iphys%i_magne, nod_comm, node, ele,    &
     &      jac_3d_q, rhs_tbl, m_lump, mhd_fem_wk, fem_wk,              &
     &      f_l, f_nl, nod_fld)
      end if
!
!
      if (iflag_debug.eq.1)   write(*,*) 'set_boundary_vect magne'
      call set_boundary_vect(Bnod_bcs%nod_bc_b, iphys%i_magne, nod_fld)
!
      call vector_send_recv(iphys%i_magne, node, nod_comm, nod_fld)
      call scalar_send_recv(iphys%i_mag_p, node, nod_comm, nod_fld)
!
      end subroutine cal_magnetic_co
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_magnetic_co_outside(nod_comm, node, ele, surf,     &
     &          insulate, sf_grp, Bnod_bcs, Fsf_bcs, iphys,             &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,         &
     &          rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk,                 &
     &          f_l, f_nl, nod_fld)
!
      use m_SGS_address
      use m_SGS_model_coefs
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use int_vol_solenoid_correct
      use int_surf_grad_sgs
      use cal_sol_vector_correct
      use cal_multi_pass
      use cal_sol_vector_co_crank
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: insulate
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      call int_vol_solenoid_co                                          &
     &   (insulate%istack_ele_fld_smp, iphys%i_mag_p, iak_diff_b,       &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l,                       &
     &    rhs_tbl, FEM_elens, fem_wk, f_nl)
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON                    &
     &     .and. Fsf_bcs%sgs%ngrp_sf_dat .gt. 0) then
        if (iflag_debug.eq.1) write(*,*)                                &
                             'int_surf_sgs_velo_co_ele', iphys%i_m_phi
         call int_surf_sgs_velo_co_ele(node, ele, surf, sf_grp,         &
     &       nod_fld, jac_sf_grp_q, jac_sf_grp_l,                       &
     &       rhs_tbl, FEM_elens, intg_point_poisson,                    &
     &       Fsf_bcs%sgs%ngrp_sf_dat, Fsf_bcs%sgs%id_grp_sf_dat,        &
     &       ifilter_final, diff_coefs%num_field, iak_diff_b,           &
     &       diff_coefs%ak, iphys%i_m_phi, fem_wk, f_nl)
      end if
!
!
      call cal_multi_pass_4_vector_ff                                   &
     &   (insulate%istack_ele_fld_smp, mhd_fem_wk%mlump_ins,            &
     &    nod_comm, node, ele, jac_3d_q, rhs_tbl,                       &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      call cal_sol_magne_insulate                                       &
     &   (nod_fld%n_point, insulate%istack_inter_fld_smp,               &
     &    insulate%numnod_fld, insulate%inod_fld, f_l%ff,               &
     &    nod_fld%ntot_phys, iphys%i_magne, nod_fld%d_fld)
!
      call set_boundary_vect(Bnod_bcs%nod_bc_b, iphys%i_magne, nod_fld)
!
      call vector_send_recv(iphys%i_magne, node, nod_comm, nod_fld)
!
      end subroutine cal_magnetic_co_outside
!
! -----------------------------------------------------------------------
!
      end module cal_magnetic_pre
