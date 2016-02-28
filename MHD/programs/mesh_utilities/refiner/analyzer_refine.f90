!
!      module  analyzer_refine
!
!     Written by H. Matsui on Oct., 2007
!
!      subroutine initialize_refine
!      subroutine analyze_refine
!
      module  analyzer_refine
!
      use m_precision
!
      use m_constants
      use m_control_param_4_refiner
      use m_read_mesh_data
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use t_comm_table
!
      implicit none
!
      integer(kind = kint), parameter, private :: my_rank = 0
      integer(kind = kint), parameter, private :: ifile_type = 0
!
      type(mesh_geometry), save :: org_mesh
      type(mesh_groups), save :: org_group
      type(surface_data), save :: org_surf
      type(edge_data), save :: org_edge
!
      type(mesh_data), save :: refined_fem
      type(element_geometry), save :: finer_elemesh
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------

      subroutine  initialize_refine
!
      use m_control_data_4_refine
      use load_mesh_data
      use set_nnod_for_ele_by_type
      use set_control_id_4_refiner
      use refinment_info_IO
!
      integer(kind=kint) :: ierr
!
!
      call read_control_data_4_refiner
      call set_control_4_refiner
!
      call set_refine_type_to_id
!
!  read global mesh
!
      iflag_mesh_file_fmt = ifile_type
      mesh_file_head = original_mesh_head
      call input_mesh                                                   &
     &   (my_rank, org_mesh%nod_comm, org_mesh%node, org_mesh%ele,      &
     &    org_group%nod_grp, org_group%ele_grp, org_group%surf_grp,     &
     &    org_surf%nnod_4_surf, org_edge%nnod_4_edge)
!
      if(iflag_read_old_refine_file .gt. 0) then
        call read_refinement_table(org_mesh%ele%numele)
      else
        call allocate_old_refine_level(org_mesh%ele%numele)
      end if
!C
!C +--------------+
!C | ELEMENT-TYPE |
!C +--------------+
!C
      call set_num_node_for_ele_by_etype                                &
     &   (org_mesh%node, org_mesh%ele, ierr)
!C
!    set refine flags
!
        write(*,*) 'set_ele_grp_id_4_refine'
        call set_ele_grp_id_4_refine(org_group%ele_grp)
!
      end subroutine  initialize_refine
!
!   --------------------------------------------------------------------
!
      subroutine analyze_refine
!
      use m_geometry_constants
      use m_refined_node_id
      use m_refined_element_data
      use m_work_merge_refine_itp
      use set_nnod_4_ele_by_type
      use set_element_refine_flag
      use set_all_refine_flags
      use count_nnod_for_refine
      use set_refined_node_id
      use set_local_position_4_refine
      use set_refined_position
      use refined_nod_2_mesh_data
      use const_refined_connectivity
      use const_refined_group
      use set_refine_flags_4_tri
      use const_refine_interpolate
      use find_hanging_surface
      use copy_mesh_structures
      use load_mesh_data
      use const_mesh_information
!
      character(len=kchara), parameter :: tmp_mesh_head = 'work'
!
!
!    construct element and surface data
!
      do
        if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
        call const_mesh_infos(my_rank,                                  &
     &      org_mesh%node, org_mesh%ele, org_surf, org_edge,            &
     &      org_group%nod_grp, org_group%ele_grp, org_group%surf_grp,   &
     &      org_group%tbls_ele_grp, org_group%tbls_surf_grp,            &
     &      org_group%surf_nod_grp)
!
        write(*,*) 'allocate_refine_flags'
        call allocate_refine_flags                                      &
     &     (org_mesh%ele%numele, org_surf%numsurf, org_edge%numedge,    &
     &      nsurf_4_ele, nedge_4_ele)
!
        if(iflag_tmp_tri_refine .eq. 0) then
          write(*,*) 's_set_element_refine_flag'
          call s_set_element_refine_flag                                &
     &       (org_mesh%ele, org_surf, org_group%ele_grp)
        end if
!
        write(*,*) 's_set_refine_flags_4_tri'
        call s_set_refine_flags_4_tri(org_mesh%node, org_mesh%ele)
!
!
        write(*,*) 's_set_all_refine_flags'
        call s_set_all_refine_flags(org_mesh%ele, org_surf, org_edge)
!
        write(*,*) 'check_hanging_surface'
        call check_hanging_surface                                      &
     &     (org_mesh%ele%numele, org_surf%numsurf, org_edge%numedge,    &
     &      org_surf%isf_4_ele, org_surf%iele_4_surf,                   &
     &      org_edge%iedge_4_ele)
!
!      call check_refine_flags(org_mesh%ele%numele,                     &
!     &    org_surf%numsurf, org_edge%numedge)
!      call check_local_refine_flags                                    &
!     &   (org_mesh%ele%numele, nsurf_4_ele, nedge_4_ele)
!
!   set refined nodes
!
        call allocate_num_refine_node                                   &
     &     (org_mesh%node%numnod, org_mesh%ele%numele,                  &
     &      org_surf%numsurf, org_edge%numedge)
        write(*,*) 's_count_nnod_for_refine'
        call s_count_nnod_for_refine(org_mesh%node, org_mesh%ele,       &
     &                               org_surf, org_edge)
!
        call allocate_item_refine_node
        write(*,*) 's_set_refined_node_id'
        call s_set_refined_node_id                                      &
     &     (org_mesh%node%numnod, org_mesh%ele%numele,                  &
     &      org_surf%numsurf, org_edge%numedge)
!
        write(*,*) 's_set_local_position_4_refine'
        call s_set_local_position_4_refine                              &
      &    (org_mesh%ele%numele, org_surf%numsurf, org_edge%numedge)
!
!      call check_refine_items                                          &
!     &   (org_mesh%node%numnod, org_mesh%ele%numele,                   &
!     &    org_surf%numsurf, org_edge%numedge)
!
         refined_fem%mesh%nod_comm%num_neib = org_mesh%nod_comm%num_neib
        call allocate_type_comm_tbl_num(refined_fem%mesh%nod_comm)
        call allocate_type_comm_tbl_item(refined_fem%mesh%nod_comm)
!
        write(*,*) 's_set_refined_position'
        call s_set_refined_position(org_mesh%node, org_mesh%ele,        &
     &                              org_surf, org_edge)
!
        refined_fem%mesh%nod_comm%num_neib = 0
!
        write(*,*) 's_refined_nod_2_mesh_data'
        call s_refined_nod_2_mesh_data                                  &
     &     (org_mesh%node, refined_fem%mesh%node)
!
!
        call s_const_refined_connectivity                               &
     &     (org_mesh%ele, org_surf, org_edge)
!
        call s_refined_ele_2_mesh_data(refined_fem%mesh%ele)
        call set_3D_nnod_4_sfed_by_ele                                  &
     &     (refined_fem%mesh%ele%nnod_4_ele,                            &
     &      finer_elemesh%surf%nnod_4_surf,                             &
     &      finer_elemesh%edge%nnod_4_edge)
!
        call set_hanging_nodes(org_surf%numsurf, org_surf%nnod_4_surf,  &
     &      org_edge%numedge, org_edge%nnod_4_edge,                     &
     &      org_surf%ie_surf, org_edge%ie_edge)
!
        call s_const_refined_group                                      &
     &     (org_mesh%node, org_mesh%ele, org_surf, org_edge,            &
     &      org_group%nod_grp, org_group%ele_grp, org_group%surf_grp,   &
     &      refined_fem%mesh, refined_fem%group)
!
        write(*,*) 's_const_refine_interpolate_tbl'
        call s_const_refine_interpolate_tbl                             &
     &     (org_mesh%node, org_mesh%ele, org_surf, org_edge,            &
     &      refined_fem%mesh)
!
        call deallocate_refine_flags
        call deallocate_refined_local_posi
        call deallocate_refined_ele_connect
        call deallocate_refined_num_element
        call deallocate_num_refine_node
!
        if (iflag_tmp_tri_refine .eq. 0) exit
!
        write(*,*) 'dealloc_mesh_infomations'
        call dealloc_mesh_infomations(org_mesh%nod_comm,                &
     &      org_mesh%node, org_mesh%ele, org_surf, org_edge,            &
     &      org_group%nod_grp, org_group%ele_grp, org_group%surf_grp,   &
     &      org_group%tbls_ele_grp, org_group%tbls_surf_grp,            &
     &      org_group%surf_nod_grp)
!
        write(*,*) 'set_mesh_data_from_type'
        call set_mesh_data_from_type                                    &
     &     (refined_fem%mesh, refined_fem%group, org_mesh%nod_comm,     &
     &      org_mesh%node, org_mesh%ele, org_surf, org_edge,            &
     &      org_group%nod_grp, org_group%ele_grp, org_group%surf_grp)
!
        org_surf%nnod_4_surf = finer_elemesh%surf%nnod_4_surf
        org_edge%nnod_4_edge = finer_elemesh%edge%nnod_4_edge
      end do
!
      iflag_mesh_file_fmt = ifile_type
      mesh_file_head = refined_mesh_head
      write(*,'(2a)') 'mesh file header: ', trim(refined_mesh_head)
      call output_mesh_type(izero, refined_fem)
!
      end subroutine analyze_refine
!
!   --------------------------------------------------------------------
!
      end module analyzer_refine
