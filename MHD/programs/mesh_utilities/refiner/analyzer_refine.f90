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
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use t_comm_table
      use t_control_data_4_refine
      use t_refined_element_data
      use t_control_param_4_refiner
!
      implicit none
!
      integer, parameter, private :: my_rank = 0
      integer(kind = kint), parameter, private :: ifile_type = 0
!
      type(control_data_4_refine), save :: refine_ctl1
      type(ctl_param_4_refiner), save :: refine_p1
!
      type(mesh_data), save :: org_fem
      type(mesh_data), save :: refined_fem
      type(element_refine_table), save :: refine_tbl
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
      call set_control_4_refiner(refine_ctl1, refine_p1)
      call dealloc_control_data_4_refiner(refine_ctl1)
!
      call set_refine_type_to_id(refine_p1%iflag_redefine_tri,          &
     &    refine_p1%num_refine_type, refine_p1%refined_ele_type,        &
     &    refine_p1%iflag_refine_type)
!
!  read global mesh
!
      call input_mesh(refine_p1%original_mesh_file, my_rank,            &
     &    org_fem%mesh, org_fem%group, ierr)
      if(ierr .gt. 0) stop 'Original mesh is wrong!!'
!
      if(refine_p1%iflag_read_old_refine_file .gt. 0) then
        call read_refinement_table(refine_p1%refine_info_head,          &
     &      org_fem%mesh%ele, refine_tbl)
      else
        call alloc_old_refine_level(org_fem%mesh%ele, refine_tbl)
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
      call set_ele_grp_id_4_refine(org_fem%group%ele_grp,               &
     &    refine_p1%num_refine_type, refine_p1%refined_ele_grp,         &
     &    refine_p1%id_refined_ele_grp)
      call dealloc_refine_param_chara(refine_p1)
!
      end subroutine  initialize_refine
!
!   --------------------------------------------------------------------
!
      subroutine analyze_refine
!
      use m_geometry_constants
      use t_refined_node_id
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
      type(refined_node_id) :: ref_ids
!
!
!    construct element and surface data
!
      do
        if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
        call const_mesh_infos(my_rank, org_fem%mesh, org_fem%group)
!
        write(*,*) 'allocate_refine_flags'
        call alloc_refine_flags(org_fem%mesh%ele, org_fem%mesh%surf,    &
     &      org_fem%mesh%edge, refine_tbl)
!
        if(refine_tbl%iflag_tmp_tri_refine .eq. 0) then
          write(*,*) 's_set_element_refine_flag'
          call s_set_element_refine_flag                                &
     &       (org_fem%mesh%ele, org_fem%mesh%surf,                      &
     &        org_fem%group%ele_grp, refine_p1,                         &
     &        refine_tbl%iflag_refine_ele)
        end if
!
        write(*,*) 's_set_refine_flags_4_tri'
        call s_set_refine_flags_4_tri(refine_p1%iflag_small_tri_refine, &
     &      org_fem%mesh%node, org_fem%mesh%ele, refine_tbl)
!
!
        write(*,*) 's_set_all_refine_flags'
        call s_set_all_refine_flags(org_fem%mesh%ele,                   &
     &      org_fem%mesh%surf, org_fem%mesh%edge, refine_tbl)
!
        write(*,*) 'check_hanging_surface'
        call check_hanging_surface(org_fem%mesh%ele,                    &
     &      org_fem%mesh%surf, org_fem%mesh%edge,                       &
     &      refine_tbl%iflag_refine_sf_lcl,                             &
     &      refine_tbl%iflag_refine_ed_lcl,                             &
     &      refine_tbl%iflag_refine_surf, refine_tbl%iflag_refine_edge)
!
!      call check_refine_flags(org_fem%mesh%ele,                        &
!     &    org_fem%mesh%surf, org_fem%mesh%edge, refine_tbl)
!      call check_local_refine_flags                                    &
!     &   (org_fem%mesh%ele, refine_tbl)
!
!   set refined nodes
!
        call alloc_num_refine_node(org_fem%mesh%node, org_fem%mesh%ele, &
     &      org_fem%mesh%surf, org_fem%mesh%edge, ref_ids)
        write(*,*) 's_count_nnod_for_refine'
        call s_count_nnod_for_refine(org_fem%mesh%node,                 &
     &      org_fem%mesh%ele, org_fem%mesh%surf, org_fem%mesh%edge,     &
     &      refine_tbl, ref_ids%refine_nod, ref_ids%refine_ele,         &
     &      ref_ids%refine_surf, ref_ids%refine_edge)
!
        call alloc_item_refine_node(ref_ids)
        write(*,*) 's_set_refined_node_id'
        call s_set_refined_node_id(org_fem%mesh%node, org_fem%mesh%ele, &
     &      org_fem%mesh%surf, org_fem%mesh%edge, refine_tbl,           &
     &      ref_ids%refine_nod, ref_ids%refine_ele,                     &
     &      ref_ids%refine_surf, ref_ids%refine_edge)
!
        write(*,*) 's_set_local_position_4_refine'
        call s_set_local_position_4_refine(org_fem%mesh%ele,            &
      &   org_fem%mesh%surf, org_fem%mesh%edge, refine_tbl,             &
      &   ref_ids%refine_ele, ref_ids%refine_surf, ref_ids%refine_edge)
!
!      call check_all_refine_items                                      &
!     &   (org_fem%mesh%node%numnod, org_fem%mesh%ele%numele,           &
!     &    org_fem%mesh%surf%numsurf, org_fem%mesh%edge%numedge, ref_ids)
!
         refined_fem%mesh%nod_comm%num_neib                             &
     &        = org_fem%mesh%nod_comm%num_neib
        call alloc_comm_table_num(refined_fem%mesh%nod_comm)
        call alloc_comm_table_item(refined_fem%mesh%nod_comm)
!
        write(*,*) 's_set_refined_position'
        call s_set_refined_position(org_fem%mesh%node,                  &
     &      org_fem%mesh%ele, org_fem%mesh%surf, org_fem%mesh%edge,     &
     &      refine_p1, ref_ids%refine_ele, ref_ids%refine_surf,         &
     &      ref_ids%refine_edge)
!
        refined_fem%mesh%nod_comm%num_neib = 0
!
        write(*,*) 's_refined_nod_2_mesh_data'
        call s_refined_nod_2_mesh_data(org_fem%mesh%node,               &
     &      ref_ids%refine_nod, ref_ids%refine_ele,                     &
     &      ref_ids%refine_surf, ref_ids%refine_edge,                   &
     &      refined_fem%mesh%node)
!
!
        call s_const_refined_connectivity                               &
     &     (org_fem%mesh%ele, org_fem%mesh%surf, org_fem%mesh%edge,     &
     &      refine_p1, refine_tbl, ref_ids%refine_ele,                  &
     &      ref_ids%refine_surf, ref_ids%refine_edge)
!
        call s_refined_ele_2_mesh_data                                  &
     &     (refine_tbl, refined_fem%mesh%ele)
        call set_3D_nnod_4_sfed_by_ele                                  &
     &     (refined_fem%mesh%ele%nnod_4_ele,                            &
     &      refined_fem%mesh%surf%nnod_4_surf,                          &
     &      refined_fem%mesh%edge%nnod_4_edge)
        refined_fem%mesh%ele%first_ele_type                             &
     &     = set_cube_eletype_from_num(refined_fem%mesh%ele%nnod_4_ele)
!
        call set_hanging_nodes(org_fem%mesh%surf, org_fem%mesh%edge,    &
     &      ref_ids%refine_surf, ref_ids%refine_edge)
!
        call s_const_refined_group(org_fem%mesh, org_fem%group,         &
     &      ref_ids, refine_tbl, refined_fem%mesh, refined_fem%group)
!
        write(*,*) 's_const_refine_interpolate_tbl'
        call s_const_refine_interpolate_tbl(org_fem%mesh,               &
     &      refined_fem%mesh, refine_p1, ref_ids, refine_tbl)
!
        call dealloc_refine_flags(refine_tbl)
        call dealloc_refined_ele_connect(refine_tbl)
        call dealloc_refined_num_element(refine_tbl)
        call dealloc_refine_node_id(ref_ids)
!
        if (refine_tbl%iflag_tmp_tri_refine .eq. 0) exit
!
        write(*,*) 'dealloc_mesh_infos_w_normal'
        call dealloc_mesh_infos_w_normal(org_fem%mesh, org_fem%group)
!
        write(*,*) 'set_mesh_data_from_type'
        call set_mesh_data_from_type                                    &
     &     (refined_fem%mesh, refined_fem%group,                        &
     &      org_fem%mesh, org_fem%group)
!
        org_fem%mesh%surf%nnod_4_surf                                   &
     &      = refined_fem%mesh%surf%nnod_4_surf
        org_fem%mesh%edge%nnod_4_edge                                   &
     &      = refined_fem%mesh%edge%nnod_4_edge
      end do
!
      call output_mesh(refine_p1%refined_mesh_file, 0,                  &
     &                 refined_fem%mesh, refined_fem%group)
      call dealloc_mesh_infos(refined_fem%mesh, refined_fem%group)
!
      end subroutine analyze_refine
!
!   --------------------------------------------------------------------
!
      end module analyzer_refine
