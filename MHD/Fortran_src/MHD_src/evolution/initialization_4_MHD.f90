!initialization_4_MHD.f90
!     module initialization_4_MHD
!
!      Written by H. Matsui
!
!------- subroutine init_analyzer_fl
!
      module initialization_4_MHD
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer_fl
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_control_parameter
      use m_iccg_parameter
      use m_t_step_parameter
!
      use m_geometry_data
      use m_surface_geometry_data
      use m_edge_geometry_data
      use m_surf_data_infinity
      use m_node_phys_address
      use m_ele_material_property
      use m_bulk_values
      use m_jacobians
      use m_work_4_dynamic_model
      use m_work_layer_correlate
!
      use m_check_subroutines
      use m_cal_max_indices
!
      use const_mesh_info
      use cal_mesh_position
      use count_whole_num_element
!
      use const_RHS_assemble_list
      use set_connectivity_4_MHD
      use init_iccg_matrices
      use convert_temperatures
      use cal_jacobian
      use cal_volume_node_MHD
      use int_MHD_mass_matrices
      use int_surface_params_MHD
      use set_dynamo_initial_field
      use set_nodal_bc_id_data
      use set_surface_bc_data
      use init_check_delta_t_data
      use allocate_array_MHD
      use ordering_line_filter_smp
      use const_ele_layering_table
      use estimate_stabilities
      use const_comm_table_fluid
      use set_reference_value
      use material_property
      use reordering_by_layers_MHD
      use set_layers_4_MHD
      use const_tbl_3d_filtering_smp
      use const_tbl_w_filtering_smp
      use set_istart_3d_filtering
      use count_sgs_components
      use set_normal_vectors
      use set_layer_list_by_table
      use reordering_MG_ele_by_layers
      use initialize_4_MHD_AMG
!
      use nodal_vector_send_recv
      use solver_MGCG_MHD
!
!     --------------------------------
!       set up for mesh information
!     --------------------------------
!
!  -----   ordering by regions ---------------------------------------
!
      call s_reordering_by_layers_MHD
!
      call set_layers
!      call check_numbers_of_nodes(my_rank)
!      call check_nodes_4_layers(my_rank)
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        ncomp_correlate = 9
        call const_layers_4_dynamic
        call allocate_work_4_dynamic
        call allocate_work_layer_correlate
      end if
!
!  -----    construct geometry informations
!
      call const_mesh_informations(my_rank)
!
      call deallocate_surface_geometry
      call deallocate_edge_geometry
!
      if (i_debug .eq. 1) call check_whole_num_of_elements
!
!     ---------------------
!
      if( (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                    &
     &   .or. iflag_SGS_model.eq.id_SGS_similarity)) then
!
        if (iflag_SGS_filter.eq.1 .or. iflag_SGS_filter.eq.11) then
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*)' s_set_istart_3d_filtering'
          call s_set_istart_3d_filtering
!
        else if (iflag_SGS_filter.eq.21                                 &
     &     .or. iflag_SGS_filter.eq.31) then
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*)' s_const_tbl_3d_filtering_smp'
          call s_const_tbl_3d_filtering_smp
!
        else if (iflag_SGS_filter.eq.2) then
          if (iflag_debug.gt.0) write(*,*)' ordering_l_filter_smp'
          call ordering_l_filter_smp
        end if
      end if
!
      if( (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                    &
     &    .and. iflag_SGS_model.eq.id_SGS_similarity) ) then
        if (iflag_SGS_filter.eq.1 .or. iflag_SGS_filter.eq.11) then
          if (iflag_debug .gt. 0) write(*,*)' s_set_istart_w_filtering'
          call s_set_istart_w_filtering
!
        else if (iflag_SGS_filter.eq.21                                 &
     &     .or. iflag_SGS_filter.eq.31) then
!
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*)' s_const_tbl_w_filtering_smp'
          call s_const_tbl_w_filtering_smp
        end if
      end if
!
!     ---------------------
!
      call time_prog_barrier
!
      if (iflag_debug.eq.1) write(*,*)' allocate_array'
      call allocate_array
!
      if ( iflag_debug.ge.1 ) write(*,*) 's_init_check_delta_t_data'
      call s_init_check_delta_t_data
!
      if (iflag_debug.eq.1) write(*,*)' set_reference_temp'
      call set_reference_temp
!      call check_reference_temp(my_rank)
!
      if (iflag_debug.eq.1) write(*,*)' set_material_property'
      call set_material_property
      call init_ele_material_property
      call s_count_sgs_components
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'init_send_recv'
      call init_send_recv
!
      if (iflag_debug.eq.1) write(*,*) 'make comm. table for fluid'
      call s_const_comm_table_fluid
!
      if (iflag_debug.eq.1) write(*,*) 'init_MGCG_MHD'
      call init_MGCG_MHD
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*)' initial_data_control'
      call initial_data_control
!
!  -------------------------------
!
      if (iflag_4_ref_temp .gt. 0) then
        if (iflag_debug.eq.1) write(*,*)' set_2_perturbation_temp'
        call set_2_perturbation_temp
      end if
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*)  'const_bc_infinity_surf_grp'
      call const_bc_infinity_surf_grp
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*)  'cal_jacobian_element'
      call set_max_int_point_by_etype
      call cal_jacobian_element
      call cal_jacobian_surface
!
      call deallocate_dxi_dx_quad
      call deallocate_dxi_dx_linear
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)  'cal_jacobian_surf_grp'
      call cal_jacobian_surf_grp
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_connect_RHS_assemble'
      call set_connect_RHS_assemble
!
      if (iflag_debug.eq.1) write(*,*) 'set_connectivity'
      call set_connectivity
!
!     ---------------------
!
      call time_prog_barrier
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_node'
      call cal_volume_node
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector'
      call s_cal_normal_vector
!
      if (iflag_debug.eq.1) write(*,*)  'int_surface_parameters'
      call int_surface_parameters
!
!     --------------------- 
!
      if (iflag_debug.eq.1)write(*,*) 'set_bc_id_data'
      call set_bc_id_data
!
      if (iflag_debug.eq.1) write(*,*) 'set_surf_bc_data'
      call set_surf_bc_data
!
!     ---------------------
!
      call int_mass_matrices
!
!     ---------------------
!
      if (iflag_debug.eq.1 ) write(*,*) 'allocate_aiccg_matrices'
      call allocate_aiccg_matrices
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'cal_stability_4_diffuse'
      call cal_stability_4_diffuse
      call time_prog_barrier
!
!     ---------------------
!
      if (     ((method_4_solver(1:1).eq.'M')                           &
     &      .or.(method_4_solver(1:1).eq.'m'))                          &
     &   .and. ((method_4_solver(2:2).eq.'G')                           &
     &      .or.(method_4_solver(2:2).eq.'g'))                          &
     &   .and. ((method_4_solver(3:3).eq.'C')                           &
     &      .or.(method_4_solver(3:3).eq.'c'))                          &
     &   .and. ((method_4_solver(4:4).eq.'G')                           &
     &      .or.(method_4_solver(4:4).eq.'g')) ) then
        call s_initialize_4_MHD_AMG
      end if
!
!     ---------------------
!
      call deallocate_surf_bc_lists
!
      end subroutine init_analyzer_fl
!
! ----------------------------------------------------------------------
!
      end module initialization_4_MHD
