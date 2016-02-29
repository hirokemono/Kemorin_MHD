!
!      module cal_magnetic_field
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!!      subroutine cal_vector_potential(nod_comm, node, ele, surf,      &
!!     &      conduct, sf_grp, iphys, iphys_ele, ele_fld,               &
!!     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,  &
!!     &      FEM_elens, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine s_cal_magnetic_field(nod_comm, node, ele, surf,      &
!!     &      conduct, sf_grp, iphys, iphys_ele, ele_fld,               &
!!     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,  &
!!     &      FEM_elens, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_magnetic_field
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
!
      implicit none
!
      real(kind = kreal) :: ave_mp0, rms_mp0
      private :: ave_mp0, rms_mp0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_vector_potential(nod_comm, node, ele, surf,        &
     &      conduct, sf_grp, iphys, iphys_ele, ele_fld,                 &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,    &
     &      FEM_elens, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_machine_parameter
      use m_control_parameter
      use m_physical_property
      use m_solver_djds_MHD
      use m_SGS_model_coefs
      use m_SGS_address
!
      use cal_vector_potential_pre
      use cal_mod_vel_potential
      use cal_sol_pressure_MHD
      use init_4_sol_potentials
      use int_rms_div_MHD
      use int_norm_div_MHD
      use cal_rms_potentials
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: conduct
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind=kint ) :: iloop
      real(kind = kreal) :: rel_correct
!
!
      call init_sol_potential(node%numnod, node%istack_nod_smp,         &
     &    coef_mag_p, nod_fld%ntot_phys, iphys%i_m_phi, iphys%i_mag_p,  &
     &    nod_fld%d_fld)
!
!     --------------------- 
!
      if (iflag_debug .gt. 0)  write(*,*) 'vector_p_pre'
      call cal_vector_p_pre(nod_comm, node, ele, surf, conduct, sf_grp, &
     &    iphys, iphys_ele, ele_fld, jac_3d_q, jac_sf_grp_q, rhs_tbl,   &
     &    FEM_elens, num_MG_level, MHD1_matrices%Bmat_MG_DJDS,          &
     &    mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
!     --------------------- 
!
      iloop = -1
      call int_norm_div_a_monitor(iloop, node, ele,                     &
     &    iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!      call int_rms_div_a_monitor(iloop, node, ele,                     &
!     &    iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!
      call init_sol_potential(node%numnod, node%istack_nod_smp,         &
     &    coef_mag_p, nod_fld%ntot_phys, iphys%i_m_phi, iphys%i_mag_p,  &
     &    nod_fld%d_fld)
!
      do iloop = 0, maxiter_vecp
!
        if (iflag_debug.gt.0) write(*,*) 'cal_electric_potential'
        call cal_electric_potential(iak_diff_b, node, ele, surf,        &
     &      sf_grp, iphys, jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl,   &
     &      FEM_elens, fem_wk, f_l, f_nl, nod_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'cal_sol_m_potential', iloop
        call cal_sol_m_potential                                        &
     &     (node%numnod, node%istack_internal_smp, nod_fld%ntot_phys,   &
     &      iphys%i_m_phi, iphys%i_mag_p, nod_fld%d_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'vector_potential_correct'
        call cal_vector_p_co(nod_comm, node, ele, surf,                 &
     &      conduct, sf_grp, iphys, iphys_ele, ele_fld,                 &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,    &
     &      FEM_elens, num_MG_level, MHD1_matrices%Bmat_MG_DJDS,        &
     &      m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
!
        if (iflag_debug.gt.0) write(*,*) 'cal_rms_scalar_potential'
        call cal_rms_scalar_potential(iloop, ele%istack_ele_smp,        &
     &      iphys%i_mag_p, i_rms%i_mag_p, j_ave%i_mag_p,                &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rel_correct, ave_mp0, rms_mp0)
!
        if (iflag_debug.eq.1)                                           &
     &         write(12,*) 'average and RMS of potential correction: ', &
     &         iloop, ave_mp0, rms_mp0
!
        if (iflag_debug.gt.0) write(*,*) 'int_norm_div_a_monitor'
        call int_norm_div_a_monitor(iloop, node, ele,                   &
     &      iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!        call int_rms_div_a_monitor(iloop, node, ele,                   &
!     &      iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!
        if ( abs(rel_correct) .lt. eps_4_magne ) exit
      end do
!
      end subroutine cal_vector_potential
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_magnetic_field(nod_comm, node, ele, surf,        &
     &      conduct, sf_grp, iphys, iphys_ele, ele_fld,                 &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,    &
     &      FEM_elens, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_machine_parameter
      use m_control_parameter
      use m_physical_property
      use m_solver_djds_MHD
      use m_SGS_model_coefs
      use m_SGS_address
!
      use cal_magnetic_pre
      use cal_sol_pressure_MHD
      use init_4_sol_potentials
      use int_rms_div_MHD
      use int_norm_div_MHD
      use cal_mod_vel_potential
      use cal_rms_potentials
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: conduct
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind=kint) :: iloop, maxiter_insulater
      real(kind = kreal) :: rel_correct
!
!
      if ( cd_ele_grp_name(1) .eq. 'all'                                &
     &       .or. cd_ele_grp_name(1) .eq. 'ALL' ) then
        maxiter_insulater = 0
      else
        maxiter_insulater = 1
      end if
!
      call init_sol_potential(node%numnod, node%istack_nod_smp,         &
     &    coef_mag_p, nod_fld%ntot_phys, iphys%i_m_phi, iphys%i_mag_p,  &
     &    nod_fld%d_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_magnetic_field_pre'
      call cal_magnetic_field_pre(nod_comm, node, ele, surf,            &
     &    conduct, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &    jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens,                   &
     &    num_MG_level, MHD1_matrices%Bmat_MG_DJDS, mhd_fem_wk, fem_wk, &
     &    f_l, f_nl, nod_fld)
!
!----  set magnetic field in insulate layer
!
      iloop = -1
      call int_norm_div_b_monitor(iloop, node, ele,                     &
     &    iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!
!
      do iloop = 0, maxiter
        call cal_mag_potential(iak_diff_b, node, ele, surf, sf_grp,     &
     &      iphys, jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl,           &
     &      FEM_elens, fem_wk, f_l, f_nl, nod_fld)
!
        call cal_sol_m_potential                                        &
     &     (node%numnod, node%istack_internal_smp, nod_fld%ntot_phys,   &
     &      iphys%i_m_phi, iphys%i_mag_p, nod_fld%d_fld)
!
!
      if (iflag_debug.eq.1) write(*,*) 'magnetic_correction'
        call cal_magnetic_co(nod_comm, node, ele, surf,                 &
     &      conduct, sf_grp, iphys, iphys_ele, ele_fld,                 &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,    &
     &      FEM_elens, num_MG_level, MHD1_matrices%Bmat_MG_DJDS,        &
     &      m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
        call cal_rms_scalar_potential(iloop, ele%istack_ele_smp,        &
     &      iphys%i_mag_p, i_rms%i_mag_p, j_ave%i_mag_p,                &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rel_correct, ave_mp0, rms_mp0)
!
      if (iflag_debug.eq.1)                                             &
     &         write(12,*) 'average and RMS of potential correction: ', &
     &         iloop, ave_mp0, rms_mp0
!
!
        call int_norm_div_b_monitor(iloop, node, ele,                   &
     &      iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!        call int_rms_div_b_monitor(iloop, node, ele,                   &
!     &      iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!
        if ( abs(rel_correct) .lt. eps_4_magne ) exit
      end do
!
      end subroutine s_cal_magnetic_field
!
!-----------------------------------------------------------------------
!
      end module cal_magnetic_field
