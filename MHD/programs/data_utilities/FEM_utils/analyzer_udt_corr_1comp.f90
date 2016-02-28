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
      use t_layering_ele_list
      use m_FEM_utils
!
      use transfer_correlate_field
!
      implicit none
!
      type(mesh_geometry), save :: mesh_ref
      type(mesh_groups), save :: group_ref
      type(phys_data), save :: phys_ref
!
      type(layering_tbl), save :: layer_tbl_corr
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
      use t_FEM_phys_data
!
      use copy_mesh_structures
      use input_control_udt_diff
      use load_mesh_data
      use const_mesh_information
      use nod_phys_send_recv
!
      use m_2nd_pallalel_vector
      use const_ele_layering_table
      use int_volume_of_domain
      use correlation_all_layerd_data
      use const_jacobians_3d
      use const_element_comm_tables
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Correlation with specified component'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_corr_udt'
      call s_input_control_corr_udt(field_FUTIL, ucd_FUTIL)
      if (iflag_debug.eq.1) write(*,*) 'input_mesh_data_type'
      call input_mesh_data_type                                         &
     &   (my_rank, femmesh_FUTIL, elemesh_FUTIL%surf%nnod_4_surf,       &
     &    elemesh_FUTIL%edge%nnod_4_edge)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'const_layers_4_dynamic'
      call const_layers_4_dynamic                                       &
     &   (femmesh_FUTIL%group%ele_grp, layer_tbl_corr)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver                                   &
     &   (isix, femmesh_FUTIL%mesh%node%numnod)
      call init_send_recv(femmesh_FUTIL%mesh%nod_comm)
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call s_const_mesh_types_info                                      &
     &   (my_rank, femmesh_FUTIL, elemesh_FUTIL)
      call const_ele_comm_tbl_global_id                                 &
     &   (femmesh_FUTIL%mesh, elemesh_FUTIL)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_address_type'
      call set_field_address_type                                       &
     &   (femmesh_FUTIL%mesh%node%numnod, field_FUTIL, iphys_FUTIL)
!
!     --------------------- 
!
      call copy_num_processes_to_2nd
      call link_mesh_data_type(femmesh_FUTIL%mesh, mesh_ref)
      call link_new_overlaped_ele_type                                  &
     &   (femmesh_FUTIL%mesh%ele, mesh_ref%ele)
!
      call link_groups_type(femmesh_FUTIL%group, group_ref)
!
      call link_field_name_type(field_FUTIL, phys_ref)
      call alloc_phys_data_type(mesh_ref%node%numnod, phys_ref)
      call allocate_vec_transfer(femmesh_FUTIL%mesh%node%numnod)
!
      call set_component_add_4_correlate(field_FUTIL)
      call allocate_2nd_iccg_matrix(isix, mesh_ref%node%numnod)
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_vol_layer'
      call max_int_point_by_etype(femmesh_FUTIL%mesh%ele%nnod_4_ele)
      call const_jacobian_and_vol_layer(femmesh_FUTIL%mesh%node,        &
     &    femmesh_FUTIL%group%surf_grp, femmesh_FUTIL%group%infty_grp,  &
     &    femmesh_FUTIL%mesh%ele, jac_FUTIL_l, jac_FUTIL_q,             &
     &     layer_tbl_corr)
!
      end subroutine initialize_udt_corre_1comp
!
! ----------------------------------------------------------------------
!
      subroutine analyze_udt_corr_1comp
!
      use m_geometry_constants
      use m_t_step_parameter
      use m_control_params_2nd_files
      use m_ctl_params_4_diff_udt
      use m_work_layer_correlate
      use set_ucd_data_to_type
      use set_ucd_data
      use ucd_IO_select
      use nod_phys_send_recv
!
      use correlation_all_layerd_data
!
      integer(kind=kint) :: istep, istep_ucd
!
!
      call link_num_field_2_ucd(field_FUTIL, ucd_FUTIL)
!
!     ---------------------
!
      ntot_correlate =  field_FUTIL%ntot_phys
      ncomp_correlate = field_FUTIL%ntot_phys
      nlayer_correlate = layer_tbl_corr%e_grp%num_grp
      call allocate_name_layer_correlate
      call allocate_all_layer_correlate
      call allocate_work_layer_correlate(layer_tbl_corr%e_grp%num_grp)
!
      call set_correlate_data_names(field_FUTIL)
!
!     ---------------------
!
      do istep = i_step_init, i_step_number
        if ( mod(istep,i_step_output_ucd) .eq. izero) then
!
          istep_ucd = istep / i_step_output_ucd
!
          call set_data_by_read_ucd_once(my_rank, istep_ucd,            &
     &        ifmt_org_ucd, ref_udt_file_head, field_FUTIL)
!
          call nod_fields_send_recv                                     &
     &       (femmesh_FUTIL%mesh%node, femmesh_FUTIL%mesh%nod_comm,     &
     &        field_FUTIL)
!
!    output udt data
!
          call coord_transfer_4_1st_field                               &
     &       (femmesh_FUTIL%mesh%node, field_FUTIL)
          call copy_ref_component_to_2nd_fld                            &
     &       (femmesh_FUTIL%mesh%node, field_FUTIL, phys_ref)
!
          if (iflag_debug .gt. 0) write(*,*)                            &
     &          's_correlation_all_layerd_data'
          call s_correlation_all_layerd_data                            &
     &       (femmesh_FUTIL%mesh%node, femmesh_FUTIL%mesh%ele,          &
     &        field_FUTIL, jac_FUTIL_l, jac_FUTIL_q,                    &
     &        layer_tbl_corr, phys_ref)
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

