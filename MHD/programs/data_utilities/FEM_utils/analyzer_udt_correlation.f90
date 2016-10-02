!analyzer_udt_correlation.f90
!      module analyzer_udt_correlation
!
!      subroutine initialize_udt_correlate
!      subroutine analyze_udt_correlate
!
!..................................................
!
!      modified by H. Matsui on Nov., 2006 
!
      module analyzer_udt_correlation
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_phys_data
      use t_layering_ele_list
      use t_work_layer_correlate
      use m_FEM_utils
!
      use transfer_correlate_field
!
      implicit none
!
      type(mesh_data_p), save :: femmesh_p_FUT
!
      type(mesh_geometry), save :: mesh_ref
      type(mesh_groups_p), save :: group_ref
      type(phys_data), save :: phys_ref
!
      type(layering_tbl), save :: layer_tbl_corr
      type(dynamis_correlation_data), save :: wk_correlate
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_udt_correlate
!
      use m_array_for_send_recv
      use t_FEM_phys_data
!
      use copy_mesh_structures
      use input_control_udt_diff
      use mpi_load_mesh_data
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
        write(*,*) 'diff. udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      call init_mesh_group_type(femmesh_p_FUT%group)
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_corr_udt'
      call s_input_control_corr_udt(field_FUTIL, ucd_FUTIL)
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh_p(femmesh_p_FUT%mesh, femmesh_p_FUT%group,    &
     &    elemesh_FUTIL%surf%nnod_4_surf,                               &
     &    elemesh_FUTIL%edge%nnod_4_edge)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'const_layers_4_dynamic'
      call const_layers_4_dynamic                                       &
     &   (femmesh_p_FUT%group%ele_grp, layer_tbl_corr)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver                                   &
     &   (isix, femmesh_p_FUT%mesh%node%numnod)
      call init_send_recv(femmesh_p_FUT%mesh%nod_comm)
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos_p(my_rank,                                  &
     &   femmesh_p_FUT%mesh, femmesh_p_FUT%group, elemesh_FUTIL)
      call const_element_comm_tbls(femmesh_p_FUT%mesh, elemesh_FUTIL)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_address_type'
      call set_field_address_type                                       &
     &   (femmesh_p_FUT%mesh%node%numnod, field_FUTIL, iphys_FUTIL)
!
!     --------------------- 
!
      call copy_num_processes_to_2nd
      call link_mesh_data_type(femmesh_p_FUT%mesh, mesh_ref)
      call link_new_overlaped_ele_type                                  &
     &   (femmesh_p_FUT%mesh%ele, mesh_ref%ele)
!
      group_ref%nod_grp =>  femmesh_p_FUT%group%nod_grp
      group_ref%ele_grp =>  femmesh_p_FUT%group%ele_grp
      group_ref%surf_grp => femmesh_p_FUT%group%surf_grp
!
      call link_field_name_type(field_FUTIL, phys_ref)
      call alloc_phys_data_type(mesh_ref%node%numnod, phys_ref)
      call allocate_vec_transfer(femmesh_p_FUT%mesh%node%numnod)
!
      call allocate_2nd_iccg_matrix(isix, mesh_ref%node%numnod)
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_vol_layer'
      call max_int_point_by_etype(femmesh_p_FUT%mesh%ele%nnod_4_ele)
      call const_jacobian_and_vol_layer(femmesh_p_FUT%mesh%node,        &
     &    femmesh_p_FUT%group%surf_grp, femmesh_p_FUT%group%infty_grp,  &
     &    femmesh_p_FUT%mesh%ele, jac_FUTIL_l, jac_FUTIL_q,             &
     &     layer_tbl_corr)
!
      end subroutine initialize_udt_correlate
!
! ----------------------------------------------------------------------
!
      subroutine analyze_udt_correlate
!
      use m_geometry_constants
      use m_t_step_parameter
      use m_control_params_2nd_files
      use m_ctl_params_4_diff_udt
      use set_ucd_data_to_type
      use set_ucd_data
      use ucd_IO_select
      use nod_phys_send_recv
!
      use fields_type_send_recv
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
      nlayer_correlate = layer_tbl_corr%e_grp%num_grp
      call allocate_name_layer_correlate
      call allocate_all_layer_correlate
      call alloc_work_layer_correlate(layer_tbl_corr%e_grp%num_grp,     &
     &    field_FUTIL%ntot_phys, wk_correlate)
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
     &        udt_org_param%iflag_format, ref_udt_file_head,            &
     &        field_FUTIL)
!
          ucd_FUTIL%ifmt_file = udt_org_param%iflag_format
          ucd_FUTIL%file_prefix = tgt_udt_file_head
          call set_data_by_read_ucd_once(my_rank, istep_ucd,            &
     &        udt_org_param%iflag_format, tgt_udt_file_head, phys_ref)
!
          call nod_fields_send_recv                                     &
     &       (femmesh_p_FUT%mesh%node, femmesh_p_FUT%mesh%nod_comm,     &
     &        field_FUTIL)
          call phys_type_send_recv_all(mesh_ref, phys_ref)
!
!    output udt data
!
          call coord_transfer_4_1st_field                               &
     &       (femmesh_p_FUT%mesh%node, field_FUTIL)
          call coord_transfer_4_2nd_field                               &
     &       (femmesh_p_FUT%mesh%node, mesh_ref%node%numnod,            &
     &        phys_ref)

!
          if (iflag_debug .gt. 0) write(*,*)                            &
     &          's_correlation_all_layerd_data'
          call s_correlation_all_layerd_data                            &
     &       (femmesh_p_FUT%mesh%node, femmesh_p_FUT%mesh%ele,          &
     &        field_FUTIL, jac_FUTIL_l, jac_FUTIL_q,                    &
     &        layer_tbl_corr, phys_ref, wk_correlate)
!
          if (iflag_debug .gt. 0) write(*,*)                            &
     &          ' write_layerd_correlate_data', istep_ucd
          call write_layerd_correlate_data(my_rank, istep_ucd)
!
        end if
      end do
!
      end subroutine analyze_udt_correlate
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_correlation

