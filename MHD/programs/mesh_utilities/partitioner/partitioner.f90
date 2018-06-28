!
      program patitioner
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_comm_table
      use t_surface_data
      use t_edge_data
      use t_near_mesh_id_4_node
      use t_mesh_data_4_merge
      use t_jacobians
      use t_fem_gauss_int_coefs
!
      use m_control_data_4_part
      use m_ctl_param_partitioner
      use m_domain_group_4_partition
!
      use init_partitioner
      use grouping_for_partition
      use generate_local_mesh
      use set_control_data_4_part
!
      use load_mesh_data
      use const_mesh_information
!
      use int_volume_of_single_domain
      use set_surf_grp_vectors
!
      use single_const_kemoview_mesh
!
      use mesh_IO_select
      use set_nnod_4_ele_by_type
!
      implicit none
!
      type(mesh_data), save :: org_fem
      type(element_geometry), save :: org_ele_mesh
!
      type(near_mesh), save :: included_ele
!>     Stracture for Jacobians
      type(jacobians_type), save :: jacobians_T
      type(shape_finctions_at_points), save :: spfs_T
!
      type(single_make_vierwer_mesh), save :: sgl_viewer_p
!
      integer(kind = kint), parameter :: my_rank = izero
      integer(kind = kint) :: ierr
      type(mesh_data) :: fem_IO_i
!
!  read control file
!
      call read_control_data_4_part
      call s_set_control_data_4_part
!
!  read global mesh
!
      call input_mesh                                                   &
     &   (global_mesh_file, my_rank, org_fem, org_ele_mesh, ierr)
      if(ierr .gt. 0) stop 'Global mesh is wrong!'
!
!      write(*,*) 'const_mesh_infos'
      call const_mesh_infos                                             &
     &   (my_rank, org_fem%mesh, org_fem%group, org_ele_mesh)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
      allocate(jacobians_T%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (org_fem%mesh%ele%nnod_4_ele, jacobians_T%g_FEM)
      call const_jacobian_and_single_vol                                &
     &   (org_fem%mesh, org_fem%group, spfs_T, jacobians_T)
!
!
!  ========= Routines for partitioner ==============
!
!      write(*,*) 'initialize_partitioner'
      call initialize_partitioner(org_fem%mesh, org_fem%group)
!      write(*,*) 'grouping_for_partitioner'
      call grouping_for_partitioner                                     &
     &   (org_fem%mesh%node, org_fem%mesh%ele, org_ele_mesh%edge,       &
     &    org_fem%group%nod_grp, org_fem%group%ele_grp,                 &
     &    org_fem%group%tbls_ele_grp)
!C===
!C-- create subdomain mesh
!      write(*,*) 'PROC_LOCAL_MESH'
      call PROC_LOCAL_MESH                                              &
     &   (org_fem%mesh%node, org_fem%mesh%ele, org_ele_mesh%edge,       &
     &    org_fem%group, included_ele)
!C
!C-- Finalize
!      write(*,*) 'dealloc_nod_ele_infos'
      call dealloc_nod_ele_infos                                        &
     &   (org_fem%mesh, org_fem%group, org_ele_mesh)
!
!  ========= Construct subdomain information for viewer ==============
!
      if(iflag_viewer_output .gt. 0) then
        write(*,*) 'choose_surface_mesh_sgl'
        call choose_surface_mesh_sgl                                    &
     &     (num_domain, distribute_mesh_file, sgl_viewer_p)
      end if
!
      stop ' * Partitioning finished'
!
      end program patitioner
