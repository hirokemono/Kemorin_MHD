!FEM_analyzer_viz.f90
!
!      module FEM_analyzer_viz
!
!       Written by H. Matsui
!
!      subroutine FEM_initialize_vizs                                   &
!     &         (ele_4_nod, jac_3d_l, jac_3d_q, ucd)
!      subroutine FEM_analyze_vizs(i_step,                              &
!     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval,  &
!     &          ucd)
!        type(element_around_node), intent(inout) :: ele_4_nod
!        type(jacobians_3d), intent(inout) :: jac_3d_l, jac_3d_q
!        type(ucd_data), intent(inout) :: ucd
!
      module FEM_analyzer_viz
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_t_step_parameter
      use m_nod_comm_table
      use m_geometry_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_vizs                                    &
     &         (ele_4_nod, jac_3d_l, jac_3d_q, ucd)
!
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_jacobian_3d
!
      use m_array_for_send_recv
      use m_read_mesh_data
      use m_group_data
      use m_node_phys_data
      use m_control_params_2nd_files
!
      use set_ele_id_4_node_type
      use load_mesh_data
      use const_mesh_information
      use set_parallel_file_name
!
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use nod_phys_send_recv
      use set_ucd_data_to_type
      use ucd_IO_select
      use const_jacobians_3d
      use const_element_comm_tables
!
      type(element_around_node), intent(inout) :: ele_4_nod
      type(jacobians_3d), intent(inout) :: jac_3d_l, jac_3d_q
      type(ucd_data), intent(inout) :: ucd
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!       load mesh informations
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call input_mesh                                                   &
     &   (my_rank, nod_comm, node1, ele1, nod_grp1, ele_grp1, sf_grp1,  &
     &    surf1%nnod_4_surf, edge1%nnod_4_edge)
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank,                                    &
     &    node1, ele1, surf1, edge1, nod_grp1, ele_grp1, sf_grp1,       &
     &    ele_grp_tbl1, sf_grp_tbl1, sf_grp_nod1)
!
      call allocate_vector_for_solver(isix, node1%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv(nod_comm)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbls(node1, ele1, surf1, edge1,           &
     &    nod_comm, ele_comm, surf_comm, edge_comm)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if( (i_step_output_fline+i_step_output_pvr) .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
        call set_ele_id_4_node(node1, ele1, ele_4_nod)
!
        if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_volume'
        call max_int_point_by_etype(ele1%nnod_4_ele)
        call const_jacobian_and_volume                                  &
     &     (node1, sf_grp1, infty_list, ele1, jac_3d_l, jac_3d_q)
!
!     --------------------- Surface jacobian for fieldline
!
        if (iflag_debug.eq.1) write(*,*)  'const_normal_vector'
        call const_normal_vector(node1, surf1)
!
        if (iflag_debug.eq.1)  write(*,*) 'pick_normal_of_surf_group'
        call pick_normal_of_surf_group                                  &
     &     (surf1, sf_grp1, sf_grp_tbl1, sf_grp_v1)
!
        if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
        call s_sum_normal_4_surf_group(ele1, sf_grp1, sf_grp_v1)
      end if
!
!     --------------------- 
!
      call deallocate_edge_geom_type(edge1)
!
!     ---------------------
!
      ucd%nnod =      node1%numnod
      call sel_read_udt_param(my_rank, i_step_init, ucd)
      call alloc_phys_data_type_by_output(ucd, node1, nod_fld1)
!
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_vizs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_vizs(i_step,                               &
     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval,   &
     &          ucd)
!
      use t_ucd_data
      use m_node_phys_data
      use m_control_params_2nd_files
      use set_ucd_data_to_type
      use set_exit_flag_4_visualizer
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
      integer(kind=kint ), intent(inout) :: visval
      integer(kind=kint ), intent(inout) :: istep_psf, istep_iso
      integer(kind=kint ), intent(inout) :: istep_pvr, istep_fline
      type(ucd_data), intent(inout) :: ucd
!
!
      call set_flag_to_visualization(i_step,                            &
     &        istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      if(visval .eq. 0) then
        call set_data_by_read_ucd(my_rank, i_step, ucd, nod_fld1)
!
        if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
        call nod_fields_send_recv(node1, nod_comm, nod_fld1)
      end if
!
      end subroutine FEM_analyze_vizs
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz
