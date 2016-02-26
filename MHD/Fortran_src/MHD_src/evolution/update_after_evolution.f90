!>@file   update_after_evolution.f90
!!        module update_after_evolution
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine fields_evolution                                     &
!!     &         (nod_comm, node, ele, surf, fluid, conduct,            &
!!     &          sf_grp, sf_grp_nod, iphys, iphys_ele, ele_fld,        &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,       &
!!     &          rhs_tbl, FEM_elens, layer_tbl, m_lump, mhd_fem_wk,    &
!!     &          fem_wk, f_l, f_nl, nod_fld)
!!      subroutine update_fields(nod_comm, node, ele, surf,             &
!!     &          fluid, conduct, sf_grp, iphys, iphys_ele, ele_fld,    &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens, &
!!     &          layer_tbl, m_lump, mhd_fem_wk, fem_wk,                &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine reset_update_flag(nod_fld)
!!
!!      subroutine fields_evolution_4_FEM_SPH(nod_comm, node, ele, surf,&
!!     &          fluid, sf_grp, sf_grp_nod, iphys, iphys_ele, ele_fld, &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,       &
!!     &          rhs_tbl, FEM_elens, layer_tbl, mhd_fem_wk, fem_wk,    &
!!     &          f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module update_after_evolution
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_layering_ele_list
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fields_evolution                                       &
     &         (nod_comm, node, ele, surf, fluid, conduct,              &
     &          sf_grp, sf_grp_nod, iphys, iphys_ele, ele_fld,          &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,         &
     &          rhs_tbl, FEM_elens, layer_tbl, m_lump, mhd_fem_wk,      &
     &          fem_wk, f_l, f_nl, nod_fld)
!
      use cal_temperature
      use cal_velocity
      use cal_magnetic_field
      use cal_light_element
!
      use update_with_scalars
      use update_with_velo
      use update_with_vector_p
      use update_with_magne
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag(nod_fld)
!
!     ---- magnetic field update
!
      if ( iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'cal_magne_vector_potential'
        call cal_vector_potential(nod_comm, node, ele, surf,            &
     &      conduct, sf_grp, iphys, iphys_ele, ele_fld,                 &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,    &
     &      FEM_elens, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
        call update_with_vector_potential(nod_comm, node, ele, surf,    &
     &      fluid, conduct, sf_grp, iphys, iphys_ele, ele_fld,          &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      else if ( iflag_t_evo_4_magne .gt. id_no_evolution) then
!
        if (iflag_debug.eq.1) write(*,*) 's_cal_magnetic_field'
        call s_cal_magnetic_field(nod_comm, node, ele, surf,            &
     &      conduct, sf_grp, iphys, iphys_ele, ele_fld,                 &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,    &
     &      FEM_elens, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
        call update_with_magnetic_field(nod_comm, node, ele, surf,      &
     &      fluid, conduct, sf_grp, iphys, iphys_ele, ele_fld,          &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
!     ---- temperature update
!
      if ( iflag_t_evo_4_temp .gt. id_no_evolution) then
        if( iflag_4_ref_temp .ne. id_no_ref_temp) then
          if (iflag_debug.eq.1) write(*,*) 'cal_parturbation_temp'
          call cal_parturbation_temp(nod_comm, node, ele, surf,         &
     &        fluid, sf_grp, iphys, iphys_ele, ele_fld,                 &
     &        jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens,               &
     &        mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field'
          call cal_temperature_field(nod_comm, node, ele, surf,         &
     &        fluid, sf_grp, iphys, iphys_ele, ele_fld,                 &
     &        jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens,               &
     &        mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
        end if
!
        call update_with_temperature(nod_comm, node, ele, surf,         &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
!     ----- composition update
!
      if ( iflag_t_evo_4_composit .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_light_element'
        call s_cal_light_element(nod_comm, node, ele, surf,             &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens,                 &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
        call update_with_dummy_scalar(nod_comm, node, ele, surf,        &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
!     ---- velocity update
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution(nod_comm, node, ele, surf, fluid,       &
     &      sf_grp, sf_grp_nod, iphys, iphys_ele, ele_fld,              &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,             &
     &      rhs_tbl, FEM_elens, layer_tbl, mhd_fem_wk, fem_wk,          &
     &      f_l, f_nl, nod_fld)
        call update_with_velocity(nod_comm, node, ele, surf,            &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine fields_evolution
!
!-----------------------------------------------------------------------
!
      subroutine update_fields(nod_comm, node, ele, surf,               &
     &          fluid, conduct, sf_grp, iphys, iphys_ele, ele_fld,      &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,   &
     &          layer_tbl, m_lump, mhd_fem_wk, fem_wk,                  &
     &          f_l, f_nl, nod_fld)
!
      use average_on_elements
      use update_with_scalars
      use update_with_velo
      use update_with_vector_p
      use update_with_magne
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iphys%i_velo .ne. 0) then
        call update_with_velocity(nod_comm, node, ele, surf,            &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      if (iphys%i_temp .ne. 0) then
        call update_with_temperature(nod_comm, node, ele, surf,         &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      if (iphys%i_light .ne. 0) then
        call update_with_dummy_scalar(nod_comm, node, ele, surf,        &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      if (iphys%i_vecp .ne. 0) then
        call update_with_vector_potential(nod_comm, node, ele, surf,    &
     &      fluid, conduct, sf_grp, iphys, iphys_ele, ele_fld,          &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else if (iphys%i_magne.ne.0) then
        call update_with_magnetic_field(nod_comm, node, ele, surf,      &
     &      fluid, conduct, sf_grp, iphys, iphys_ele, ele_fld,          &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine update_fields
!
!-----------------------------------------------------------------------
!
      subroutine reset_update_flag(nod_fld)
!
      use m_SGS_model_coefs
      use m_SGS_address
!
      type(phys_data), intent(inout) :: nod_fld
!
!     reset monitoring flag
!
      nod_fld%iflag_update(1:nod_fld%ntot_phys) = 0
      iflag_sgs_coefs(1:num_sgs_kinds) =          0
      iflag_diff_coefs(1:num_diff_kinds) =        0
!
      end subroutine reset_update_flag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fields_evolution_4_FEM_SPH(nod_comm, node, ele, surf,  &
     &          fluid, sf_grp, sf_grp_nod, iphys, iphys_ele, ele_fld,   &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,         &
     &          rhs_tbl, FEM_elens, layer_tbl, mhd_fem_wk, fem_wk,      &
     &          f_l, f_nl, nod_fld)
!
      use cal_temperature
      use cal_velocity
      use cal_light_element
!
      use update_with_scalars
      use update_with_velo
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag(nod_fld)
!
!     ---- temperature update
!
      if ( iflag_t_evo_4_temp .gt. id_no_evolution) then
        if( iflag_4_ref_temp .ne. id_no_ref_temp) then
          if (iflag_debug.eq.1) write(*,*) 'cal_parturbation_temp'
          call cal_parturbation_temp(nod_comm, node, ele, surf,         &
     &        fluid, sf_grp, iphys, iphys_ele, ele_fld,                 &
     &        jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens, mhd_fem_wk,   &
     &        fem_wk, f_l, f_nl, nod_fld)
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field'
          call cal_temperature_field(nod_comm, node, ele, surf,         &
     &        fluid, sf_grp, iphys, iphys_ele, ele_fld,                 &
     &        jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens, mhd_fem_wk,   &
     &        fem_wk, f_l, f_nl, nod_fld)
        end if
!
        call update_with_temperature(nod_comm, node, ele, surf,         &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
!     ----- composition update
!
      if ( iflag_t_evo_4_composit .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_light_element'
        call s_cal_light_element(nod_comm, node, ele, surf,             &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens, mhd_fem_wk,     &
     &      fem_wk, f_l, f_nl, nod_fld)
        call update_with_dummy_scalar(nod_comm, node, ele, surf,        &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
!     ---- velocity update
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution(nod_comm, node, ele, surf, fluid,       &
     &      sf_grp, sf_grp_nod, iphys, iphys_ele, ele_fld,              &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,             &
     &      rhs_tbl, FEM_elens, layer_tbl, mhd_fem_wk, fem_wk,          &
     &      f_l, f_nl, nod_fld)
        call update_with_velocity(nod_comm, node, ele, surf,            &
     &      fluid, sf_grp, iphys, iphys_ele, ele_fld,                   &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      layer_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine fields_evolution_4_FEM_SPH
!
!-----------------------------------------------------------------------
!
      end module update_after_evolution
