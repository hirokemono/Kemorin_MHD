!initialize_4_snapshot.f90
!     module initialize_4_snapshot
!
!      Written by H. Matsui
!
!      subroutine init_analyzer_snap(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module initialize_4_snapshot
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
      subroutine init_analyzer_snap(layer_tbl)
!
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
      use m_t_step_parameter
!
      use m_mesh_data
      use m_nod_comm_table
      use m_geometry_data
      use m_geometry_data_MHD
      use m_group_data
      use m_element_id_4_node
      use m_node_phys_data
      use m_ele_material_property
      use m_bulk_values
      use m_jacobians
      use m_jacobian_sf_grp
      use m_work_4_dynamic_model
      use m_work_layer_correlate
      use m_boundary_condition_IDs
      use m_array_for_send_recv
      use m_solver_djds_MHD
!
      use m_check_subroutines
!
      use t_layering_ele_list
!
      use count_whole_num_element
!
      use cal_volume_node_MHD
      use int_MHD_mass_matrices
      use int_surface_params_MHD
      use set_nodal_bc_id_data
      use set_surface_bc_data
      use allocate_array_MHD
      use ordering_line_filter_smp
      use const_ele_layering_table
      use const_comm_table_fluid
      use const_bc_infty_surf_type
      use set_reference_value
      use material_property
      use reordering_by_layers
      use set_layers_4_MHD
      use const_tbl_3d_filtering_smp
      use const_tbl_w_filtering_smp
      use set_istart_3d_filtering
      use count_sgs_components
      use set_layer_list_by_table
      use set_normal_vectors
      use fem_mhd_rst_IO_control
!
      use nod_phys_send_recv
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' s_reordering_by_layers_snap'
      call s_reordering_by_layers_snap
!
      if (iflag_debug.eq.1) write(*,*)' set_layers'
      call set_layers(node1, ele1, ele_grp1)
!      call check_numbers_of_nodes(my_rank)
!      call check_nodes_4_layers(my_rank)
!
      if (iflag_dynamic_SGS  .ne. id_SGS_DYNAMIC_OFF) then
        ncomp_correlate = 9
        if (iflag_debug.eq.1) write(*,*)' const_layers_4_dynamic'
        call const_layers_4_dynamic(ele_grp1, layer_tbl)
        call allocate_work_4_dynamic(layer_tbl%e_grp%num_grp)
        call allocate_work_layer_correlate(layer_tbl%e_grp%num_grp)
      end if
!
!
!     ---------------------
!
      if (iflag_debug.ge.1 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, node1%numnod)
!
      call init_send_recv(nod_comm)
!
      if (iflag_debug.eq.1) write(*,*)' const_mesh_informations'
      call const_mesh_informations(my_rank)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tables_1st'
      call const_element_comm_tables_1st
!
      if(i_debug .eq. iflag_full_msg) then
        call check_whole_num_of_elements(ele1)
      end if
!
!     ---------------------
!
      if( (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                    &
     &       .or. iflag_SGS_model.eq.id_SGS_similarity)) then
!
        if   (iflag_SGS_filter .eq. id_SGS_3D_FILTERING                 &
     &   .or. iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING) then
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*)' s_set_istart_3d_filtering'
          call s_set_istart_3d_filtering
!
        else if (iflag_SGS_filter.eq.id_SGS_3D_SMP_FILTERING            &
     &     .or. iflag_SGS_filter.eq.id_SGS_3D_EZ_SMP_FILTERING) then
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*)' s_const_tbl_3d_filtering_smp'
          call s_const_tbl_3d_filtering_smp
!
        else if (iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
          if (iflag_debug.gt.0) write(*,*)' ordering_l_filter_smp'
          call ordering_l_filter_smp                                    &
     &       (node1%numnod, node1%istack_nod_smp)
        end if
      end if
!
      if( (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                    &
     &      .and. iflag_SGS_model.eq.id_SGS_similarity) ) then
        if    (iflag_SGS_filter .eq. id_SGS_3D_FILTERING                &
     &    .or. iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING) then
          if (iflag_debug .gt. 0) write(*,*)' s_set_istart_w_filtering'
          call s_set_istart_w_filtering
!
        else if (iflag_SGS_filter.eq.id_SGS_3D_SMP_FILTERING            &
     &     .or. iflag_SGS_filter.eq.id_SGS_3D_EZ_SMP_FILTERING) then
!
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*)' s_const_tbl_w_filtering_smp'
          call s_const_tbl_w_filtering_smp
        end if
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)' allocate_array'
      call allocate_array
!
      if (iflag_debug.eq.1) write(*,*)' set_reference_temp'
      call set_reference_temp                                           &
     &   (node1%numnod, fluid1%numnod_fld, fluid1%inod_fld,             &
     &    node1%xx, node1%rr, node1%a_r, nod_fld1%ntot_phys,            &
     &    iphys%i_ref_t, iphys%i_gref_t, nod_fld1%d_fld)
!
      if (iflag_debug.eq.1) write(*,*)' set_material_property'
      call set_material_property
      call init_ele_material_property(ele1%numele)
      call s_count_sgs_components(node1%numnod, ele1%numele, layer_tbl)
!
      if (iflag_debug.gt.0)  write(*,*)' make comm. table for fluid'
      call s_const_comm_table_fluid(nprocs, fluid1%istack_ele_fld_smp,  &
     &    node1, ele1, nod_comm, DJDS_comm_fl)
!
      call deallocate_surface_geom_type(surf1)
      call deallocate_edge_geom_type(edge1)
!
!     --------------------- 
!
      if (i_step_output_rst .gt. 0) then
        if (iflag_debug.eq.1) write(*,*)' init_restart_4_snapshot'
        call init_restart_4_snapshot(node1)
      end if
!
!     ---------------------
!
      call const_bc_infinity_surf_grp                                   &
     &   (iflag_surf_infty, sf_grp1, infty_list)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' const_MHD_jacobian_and_volumes'
      call const_MHD_jacobian_and_volumes                               &
     &   (node1, ele1, sf_grp1, layer_tbl, infty_list,                  &
     &    jac1_3d_l, jac1_3d_q)
!
      call cal_jacobian_surf_grp(sf_grp1)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' set_connect_RHS_assemble'
      call set_connect_RHS_assemble
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)  'const_normal_vector'
      call const_normal_vector(node1, surf1)
!
      if (iflag_debug.eq.1) write(*,*)' int_surface_parameters'
      call int_surface_parameters(sf_grp1%num_grp, node1, ele1, surf1,  &
     &    sf_grp1, sf_grp_tbl1, sf_grp_v1, sf_grp_nod1)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' set_bc_id_data'
      call set_bc_id_data(node1, ele1, nod_grp1, iphys, nod_fld1)
!
      if (iflag_debug.eq.1) write(*,*)' set_surf_bc_data'
      call set_surf_bc_data                                             &
     &   (node1%numnod, sf_grp1, sf_grp_nod1, sf_grp_v1)
      call deallocate_surf_bc_lists
!
!     --------------------- 
!
      call int_RHS_mass_matrices
!
!
      end subroutine init_analyzer_snap
!
! ----------------------------------------------------------------------
!
      end module initialize_4_snapshot
