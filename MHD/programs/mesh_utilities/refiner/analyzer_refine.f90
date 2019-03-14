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
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use t_comm_table
      use t_control_data_4_refine
!
      implicit none
!
      integer, parameter, private :: my_rank = 0
      integer(kind = kint), parameter, private :: ifile_type = 0
!
      type(control_data_4_refine), save :: refine_ctl1
!
      type(mesh_data), save :: org_fem
      type(element_geometry), save :: org_ele_mesh
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
      use load_mesh_data
      use set_nnod_for_ele_by_type
      use set_control_id_4_refiner
      use refinment_info_IO
!
      integer(kind=kint) :: ierr
!
!
      call read_control_data_4_refiner(refine_ctl1)
      call set_control_4_refiner(refine_ctl1)
      call dealloc_control_data_4_refiner(refine_ctl1)
!
      call set_refine_type_to_id
!
!  read global mesh
!
      call input_mesh                                                   &
     &   (original_mesh_file, my_rank, org_fem, org_ele_mesh, ierr)
      if(ierr .gt. 0) stop 'Original mesh is wrong!!'
!
      if(iflag_read_old_refine_file .gt. 0) then
        call read_refinement_table(org_fem%mesh%ele%numele)
      else
        call allocate_old_refine_level(org_fem%mesh%ele%numele)
      end if
!C
!C +--------------+
!C | ELEMENT-TYPE |
!C +--------------+
!C
      call set_num_node_for_ele_by_etype                                &
     &   (org_fem%mesh%node, org_fem%mesh%ele, ierr)
!C
!    set refine flags
!
        write(*,*) 'set_ele_grp_id_4_refine'
        call set_ele_grp_id_4_refine(org_fem%group%ele_grp)
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
        call const_mesh_infos                                           &
     &     (my_rank, org_fem%mesh, org_fem%group, org_ele_mesh)
!
        write(*,*) 'allocate_refine_flags'
        call allocate_refine_flags                                      &
     &     (org_fem%mesh%ele%numele, org_ele_mesh%surf%numsurf,         &
     &      org_ele_mesh%edge%numedge, nsurf_4_ele, nedge_4_ele)
!
        if(iflag_tmp_tri_refine .eq. 0) then
          write(*,*) 's_set_element_refine_flag'
          call s_set_element_refine_flag                                &
     &       (org_fem%mesh%ele, org_ele_mesh%surf,                      &
     &        org_fem%group%ele_grp)
        end if
!
        write(*,*) 's_set_refine_flags_4_tri'
        call s_set_refine_flags_4_tri                                   &
     &     (org_fem%mesh%node, org_fem%mesh%ele)
!
!
        write(*,*) 's_set_all_refine_flags'
        call s_set_all_refine_flags(org_fem%mesh%ele,                   &
     &      org_ele_mesh%surf, org_ele_mesh%edge)
!
        write(*,*) 'check_hanging_surface'
        call check_hanging_surface(org_fem%mesh%ele%numele,             &
     &      org_ele_mesh%surf%numsurf, org_ele_mesh%edge%numedge,       &
     &      org_ele_mesh%surf%isf_4_ele, org_ele_mesh%surf%iele_4_surf, &
     &      org_ele_mesh%edge%iedge_4_ele)
!
!      call check_refine_flags(org_fem%mesh%ele%numele,                     &
!     &    org_ele_mesh%surf%numsurf, org_ele_mesh%edge%numedge)
!      call check_local_refine_flags                                    &
!     &   (org_fem%mesh%ele%numele, nsurf_4_ele, nedge_4_ele)
!
!   set refined nodes
!
        call allocate_num_refine_node                                   &
     &     (org_fem%mesh%node%numnod, org_fem%mesh%ele%numele,          &
     &      org_ele_mesh%surf%numsurf, org_ele_mesh%edge%numedge)
        write(*,*) 's_count_nnod_for_refine'
        call s_count_nnod_for_refine                                    &
     &     (org_fem%mesh%node, org_fem%mesh%ele,                        &
     &      org_ele_mesh%surf, org_ele_mesh%edge)
!
        call allocate_item_refine_node
        write(*,*) 's_set_refined_node_id'
        call s_set_refined_node_id                                      &
     &     (org_fem%mesh%node%numnod, org_fem%mesh%ele%numele,          &
     &      org_ele_mesh%surf%numsurf, org_ele_mesh%edge%numedge)
!
        write(*,*) 's_set_local_position_4_refine'
        call s_set_local_position_4_refine(org_fem%mesh%ele%numele,     &
      &    org_ele_mesh%surf%numsurf, org_ele_mesh%edge%numedge)
!
!      call check_refine_items                                          &
!     &   (org_fem%mesh%node%numnod, org_fem%mesh%ele%numele,           &
!     &    org_ele_mesh%surf%numsurf, org_ele_mesh%edge%numedge)
!
         refined_fem%mesh%nod_comm%num_neib                             &
     &        = org_fem%mesh%nod_comm%num_neib
        call alloc_comm_table_num(refined_fem%mesh%nod_comm)
        call alloc_comm_table_item(refined_fem%mesh%nod_comm)
!
        write(*,*) 's_set_refined_position'
        call s_set_refined_position                                     &
     &     (org_fem%mesh%node, org_fem%mesh%ele,                        &
     &      org_ele_mesh%surf, org_ele_mesh%edge)
!
        refined_fem%mesh%nod_comm%num_neib = 0
!
        write(*,*) 's_refined_nod_2_mesh_data'
        call s_refined_nod_2_mesh_data                                  &
     &     (org_fem%mesh%node, refined_fem%mesh%node)
!
!
        call s_const_refined_connectivity                               &
     &     (org_fem%mesh%ele, org_ele_mesh%surf, org_ele_mesh%edge)
!
        call s_refined_ele_2_mesh_data(refined_fem%mesh%ele)
        call set_3D_nnod_4_sfed_by_ele                                  &
     &     (refined_fem%mesh%ele%nnod_4_ele,                            &
     &      finer_elemesh%surf%nnod_4_surf,                             &
     &      finer_elemesh%edge%nnod_4_edge)
        refined_fem%mesh%ele%first_ele_type                             &
     &     = set_cube_eletype_from_num(refined_fem%mesh%ele%nnod_4_ele)
!
        call set_hanging_nodes                                          &
     &     (org_ele_mesh%surf%numsurf, org_ele_mesh%surf%nnod_4_surf,   &
     &      org_ele_mesh%edge%numedge, org_ele_mesh%edge%nnod_4_edge,   &
     &      org_ele_mesh%surf%ie_surf, org_ele_mesh%edge%ie_edge)
!
        call s_const_refined_group                                      &
     &     (org_fem%mesh, org_ele_mesh, org_fem%group,                  &
     &      refined_fem%mesh, refined_fem%group)
!
        write(*,*) 's_const_refine_interpolate_tbl'
        call s_const_refine_interpolate_tbl                             &
     &     (org_fem%mesh, org_ele_mesh, refined_fem%mesh)
!
        call deallocate_refine_flags
        call deallocate_refined_local_posi
        call deallocate_refined_ele_connect
        call deallocate_refined_num_element
        call deallocate_num_refine_node
!
        if (iflag_tmp_tri_refine .eq. 0) exit
!
        write(*,*) 'dealloc_mesh_infos_w_normal'
        call dealloc_mesh_infos_w_normal                                &
     &     (org_fem%mesh, org_fem%group, org_ele_mesh)
!
        write(*,*) 'set_mesh_data_from_type'
        call set_mesh_data_from_type                                    &
     &     (refined_fem%mesh, refined_fem%group,                        &
     &      org_fem%mesh, org_ele_mesh, org_fem%group)
!
        org_ele_mesh%surf%nnod_4_surf = finer_elemesh%surf%nnod_4_surf
        org_ele_mesh%edge%nnod_4_edge = finer_elemesh%edge%nnod_4_edge
      end do
!
      call output_mesh(refined_mesh_file, 0,                            &
     &                 refined_fem%mesh, refined_fem%group)
      call dealloc_mesh_infos(refined_fem%mesh, refined_fem%group)
!
      end subroutine analyze_refine
!
!   --------------------------------------------------------------------
!
      end module analyzer_refine
