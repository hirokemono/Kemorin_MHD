!analyzer_group_ave_udt.f90
!      module analyzer_group_ave_udt
!
!      subroutine initialize_grp_ave_udt
!      subroutine analyze_grp_ave_udt
!
!..................................................
!
!
      module analyzer_group_ave_udt
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
!
      use transfer_correlate_field
!
!      modified by H. Matsui on Nov., 2006 
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_grp_ave_udt
!
      use m_geometry_parameter
      use m_node_phys_address
      use input_control_udt_diff
      use const_mesh_info
      use nodal_vector_send_recv
!
      use m_2nd_geometry_param
      use m_2nd_pallalel_vector
      use m_2nd_phys_data
      use copy_comm_tables_to_2nd
      use link_geometry_to_1st_mesh
      use link_group_to_1st_mesh
      use link_data_to_1st_mesh
      use const_ele_layering_table
      use int_volume_of_domain
!
      use m_jacobians
      use cal_jacobian
!
!
      if (my_rank.eq.0) then
        write(*,*) 'TAke Average in each element groups'
        write(*,*) 'Input control file: ctl_correlate_udt'
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
      call copy_nod_comm_table_2_2nd
      call link_node_data
      call link_element_data
      call link_mesh_parameter_4_smp
!
      call link_node_group
      call link_element_group
      call link_surface_group
!
      call link_nodal_field_names
      call allocate_2nd_data_arrays
      call allocate_vec_transfer
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_iccgN_matrix'
      call allocate_iccgN_matrix(isix,    numnod)
      call allocate_2nd_iccg_matrix(isix, nnod_2nd)
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
      end subroutine initialize_grp_ave_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_grp_ave_udt
!
      use m_geometry_constants
      use m_layering_ele_list
      use m_node_phys_address
      use m_t_step_parameter
      use m_ctl_params_4_diff_udt
      use m_ucd_data
      use m_work_layer_correlate
      use set_ucd_data
      use ucd_IO_select
      use nod_phys_send_recv
!
      use set_udt_to_2nd_data
      use second_fields_send_recv
      use correlation_all_layerd_data
!
!
      integer(kind=kint) :: i_step
!
!
      call link_num_field_2_output
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
      if (my_rank .eq. 0) then
        call open_layerd_correlate_files
        call write_layerd_correlate_header
      end if
!
!     ---------------------
!
      do i_step = i_step_init, i_step_number
        if ( mod(i_step,i_step_output_ucd) .eq. 0) then
!
          ucd_step = i_step / i_step_output_ucd
!
!            if (i_step .eq. i_step_init) then
          ucd_header_name = ref_udt_file_head
          call sel_read_udt_param(my_rank, ucd_step)
!            end if
!
          call sel_read_udt_file(my_rank, ucd_step)
          call set_ucd_data_from_IO
          call deallocate_ucd_data
!
          ucd_header_name = tgt_udt_file_head
          call sel_read_udt_param(my_rank, ucd_step)
!
          call sel_read_udt_file(my_rank, ucd_step)
          call set_2nd_data_by_udt
          call deallocate_ucd_data
!
          call phys_send_recv_all
          call phys_2nd_send_recv_all
!
!    output udt data
!
          if     (iflag_correlate_coord .eq. iflag_spherical) then
            call transfer_corr_field_to_sph
          else if(iflag_correlate_coord .eq. iflag_cylindrical) then
            call transfer_corr_field_to_cyl
          end if
!
          call s_correlation_all_layerd_data
          if (my_rank .eq. 0) then
            call write_layerd_average_data(ucd_step)
          end if
!
        end if
      end do
      if (my_rank .eq. 0) call close_layerd_average_files
!
!
      end subroutine analyze_grp_ave_udt
!
! ----------------------------------------------------------------------
!
      end module analyzer_group_ave_udt

