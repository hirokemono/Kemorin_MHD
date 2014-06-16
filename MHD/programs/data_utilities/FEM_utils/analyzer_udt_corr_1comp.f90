!analyzer_udt_corr_1comp.f90
!      module analyzer_udt_corr_1comp
!
!      subroutine initialize_udt_corre_1comp
!      subroutine analyze_udt_corr_1comp
!
!      modified by H. Matsui on June., 2013 
!
!..................................................
!
      module analyzer_udt_corr_1comp
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_phys_data
!
      use transfer_correlate_field
!
      implicit none
!
      type(mesh_geometry), save :: mesh_ref
      type(mesh_groups), save :: group_ref
      type(phys_data), save :: phys_ref
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_udt_corre_1comp
!
      use m_array_for_send_recv
      use m_geometry_parameter
      use m_node_phys_address
      use input_control_udt_diff
      use const_mesh_info
      use nodal_vector_send_recv
!
      use m_2nd_pallalel_vector
      use copy_nod_comm_tbl_4_type
      use link_data_type_to_1st_mesh
      use link_group_type_2_1st_mesh
      use const_ele_layering_table
      use int_volume_of_domain
      use correlation_all_layerd_data
!
      use m_jacobians
      use cal_jacobian
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Correlation with specified component'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_corr_udt'
      call s_input_control_corr_udt
      if (iflag_debug.eq.1) write(*,*) 's_input_mesh_udt_diff'
      call s_input_mesh_udt_diff
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'const_layers_4_dynamic'
      call const_layers_4_dynamic
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_nod_field_data'
      call initialize_nod_field_data
!
!     --------------------- 
!
      call copy_num_processes_to_2nd
      call copy_node_comm_tbl_to_type(mesh_ref%nod_comm)
      call link_node_data_type(mesh_ref%node)
      call link_element_data_type(mesh_ref%ele)
!
      call link_node_group_to_type(group_ref%nod_grp)
      call link_element_group_to_type(group_ref%ele_grp)
      call link_surface_group_to_type(group_ref%surf_grp)
!
      call link_nodal_fld_type_names(phys_ref)
      call alloc_phys_data_type(mesh_ref%node%numnod, phys_ref)
      call allocate_vec_transfer
!
      call set_component_add_4_correlate
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix,    numnod)
      call allocate_2nd_iccg_matrix(isix, mesh_ref%node%numnod)
!
      call init_send_recv
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)  'cal_jacobian_element'
      call set_max_int_point_by_etype
      call cal_jacobian_element
!
      call deallocate_dxi_dx_quad
      call deallocate_dxi_dx_linear
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)  's_int_whole_volume_w_layer'
      call s_int_whole_volume_w_layer
!
      end subroutine initialize_udt_corre_1comp
!
! ----------------------------------------------------------------------
!
      subroutine analyze_udt_corr_1comp
!
      use m_geometry_constants
      use m_layering_ele_list
      use m_node_phys_data
      use m_t_step_parameter
      use m_control_params_2nd_files
      use m_ctl_params_4_diff_udt
      use m_ucd_data
      use m_ucd_input_data
      use m_work_layer_correlate
      use set_ucd_data
      use ucd_IO_select
      use nod_phys_send_recv
!
      use correlation_all_layerd_data
!
      integer(kind=kint) :: istep, istep_ucd
!
!
      call link_fem_num_field_2_ucd_out
!
!     ---------------------
!
      ntot_correlate = num_tot_nod_phys
      ncomp_correlate = num_tot_nod_phys
      nlayer_correlate = n_layer_d
      call allocate_name_layer_correlate
      call allocate_all_layer_correlate
      call allocate_work_layer_correlate
!
      call set_correlate_data_names
!
!     ---------------------
!
      do istep = i_step_init, i_step_number
        if ( mod(istep,i_step_output_ucd) .eq. 0) then
!
          istep_ucd = istep / i_step_output_ucd
!
          call set_data_by_read_ucd_once(my_rank, istep_ucd,            &
     &        ifmt_org_ucd, ref_udt_file_head)
!
          call phys_send_recv_all
!
!    output udt data
!
          call coord_transfer_4_1st_field
          call copy_ref_component_to_2nd_fld(phys_ref)
!
          if (iflag_debug .gt. 0) write(*,*)                            &
     &          's_correlation_all_layerd_data'
          call s_correlation_all_layerd_data(mesh_ref%node%numnod,      &
     &        phys_ref)
!
          if (iflag_debug .gt. 0) write(*,*)                            &
     &          ' write_layerd_correlate_data', istep_ucd
          call write_layerd_correlate_data(my_rank, istep_ucd)
        end if
      end do
!
!
      end subroutine analyze_udt_corr_1comp
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_corr_1comp

