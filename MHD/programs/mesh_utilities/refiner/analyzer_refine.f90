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
!
      use m_element_group
!
      implicit none
!
      integer(kind = kint), parameter, private :: my_rank = 0
      integer(kind = kint), parameter, private :: ifile_type = 0
!
      type(mesh_data), save :: refined_fem
      type(surface_geometry), save :: finer_surfmesh
      type(edge_geometry), save ::  finer_edgemesh
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------

      subroutine  initialize_refine
!
      use m_geometry_data
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
      call input_mesh(izero)
!
      if(iflag_read_old_refine_file .gt. 0) then
        call read_refinement_table
      else
        call allocate_old_refine_level(ele1%numele)
      end if
!C
!C +--------------+
!C | ELEMENT-TYPE |
!C +--------------+
!C
      call set_num_node_for_ele_by_etype(ierr)
!C
!    set refine flags
!
        write(*,*) 'set_ele_grp_id_4_refine'
        call set_ele_grp_id_4_refine(ele_grp1)
!
      end subroutine  initialize_refine
!
!   --------------------------------------------------------------------
!
      subroutine analyze_refine
!
      use m_nod_comm_table
      use m_geometry_constants
      use m_geometry_data
      use m_refined_node_id
      use m_refined_element_data
      use m_work_merge_refine_itp
      use const_mesh_info
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
      use copy_mesh_from_type
      use find_hanging_surface
      use load_mesh_type_data
      use set_mesh_types
!
      character(len=kchara), parameter :: tmp_mesh_head = 'work'
!
!
!    construct element and surface data
!
      do
        write(*,*) 'const_mesh_informations'
        call const_mesh_informations(my_rank)
!
        write(*,*) 'allocate_refine_flags'
        call allocate_refine_flags                                      &
     &     (ele1%numele, surf1%numsurf, edge1%numedge,                  &
     &      nsurf_4_ele, nedge_4_ele)
!
        if(iflag_tmp_tri_refine .eq. 0) then
          write(*,*) 's_set_element_refine_flag'
          call s_set_element_refine_flag(ele1%numele, ele_grp1)
        end if
!
        write(*,*) 's_set_refine_flags_4_tri'
        call s_set_refine_flags_4_tri
!
!
        write(*,*) 's_set_all_refine_flags'
        call s_set_all_refine_flags
!
        write(*,*) 'check_hanging_surface'
        call check_hanging_surface                                      &
     &     (ele1%numele, surf1%numsurf, edge1%numedge,                  &
     &      surf1%isf_4_ele, surf1%iele_4_surf, edge1%iedge_4_ele)
!
!      call check_refine_flags(ele1%numele, surf1%numsurf, edge1%numedge)
!      call check_local_refine_flags                                    &
!     &   (ele1%numele, nsurf_4_ele, nedge_4_ele)
!
!   set refined nodes
!
        call allocate_num_refine_node(node1%numnod, ele1%numele,        &
     &                                surf1%numsurf, edge1%numedge)
        write(*,*) 's_count_nnod_for_refine'
        call s_count_nnod_for_refine
!
        call allocate_item_refine_node
        write(*,*) 's_set_refined_node_id'
        call s_set_refined_node_id
!
        write(*,*) 's_set_local_position_4_refine'
        call s_set_local_position_4_refine
!
!      call check_refine_items(node1%numnod, ele1%numele,               &
!     &                        surf1%numsurf, edge1%numedge)
!
         refined_fem%mesh%nod_comm%num_neib = nod_comm%num_neib
        call allocate_type_comm_tbl_num(refined_fem%mesh%nod_comm)
        call allocate_type_comm_tbl_item(refined_fem%mesh%nod_comm)
!
        write(*,*) 's_set_refined_position'
        call s_set_refined_position
!
        refined_fem%mesh%nod_comm%num_neib = 0
!
        write(*,*) 's_refined_nod_2_mesh_data'
        call s_refined_nod_2_mesh_data(refined_fem%mesh%node)
!
!
        call s_const_refined_connectivity
!
        call s_refined_ele_2_mesh_data(refined_fem%mesh%ele)
        call set_nnod_surf_edge_for_type(finer_surfmesh,                &
     &      finer_edgemesh, refined_fem%mesh%ele%nnod_4_ele)
!
        call set_hanging_nodes(surf1%numsurf, surf1%nnod_4_surf,        &
     &      edge1%numedge, edge1%nnod_4_edge,                           &
     &      surf1%ie_surf, edge1%ie_edge)
!
        call s_const_refined_group(refined_fem%mesh, refined_fem%group)
!
        write(*,*) 's_const_refine_interpolate_tbl'
        call s_const_refine_interpolate_tbl(refined_fem%mesh)
!
        call deallocate_refine_flags
        call deallocate_refined_local_posi
        call deallocate_refined_ele_connect
        call deallocate_refined_num_element
        call deallocate_num_refine_node
!
        if (iflag_tmp_tri_refine .eq. 0) exit
!
        write(*,*) 'deallocate_mesh_infomations'
        call deallocate_mesh_infomations
!
        write(*,*) 'set_mesh_from_type'
        call set_mesh_from_type(refined_fem%mesh, refined_fem%group)
!
        surf1%nnod_4_surf = finer_surfmesh%surf%nnod_4_surf
        edge1%nnod_4_edge = finer_edgemesh%edge%nnod_4_edge
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
