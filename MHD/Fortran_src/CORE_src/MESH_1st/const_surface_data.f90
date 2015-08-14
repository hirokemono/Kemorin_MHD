!const_surface_data.f90
!      module const_surface_data
!
!     Written by H. Matsui on Apr., 2006
!
!
!      subroutine construct_surface_data
!      subroutine const_element_list_4_surface
!
      module const_surface_data
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_data
      use t_sum_hash
!
      implicit none
!
      type(sum_hash_tbl), save, private :: surf_ele_tbl
!
      private :: const_all_surface_data
      private :: const_external_surface_data
      private :: const_isolate_surface_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine construct_surface_data
!
      use set_surface_hash
      use check_geometries
!
!   set hash data for suface elements using sum of local node ID
!
      call alloc_sum_hash(node1%numnod, ele1%numele,                    &
     &   nsurf_4_ele, nnod_4_surf, surf_ele_tbl)
!
!
      if (iflag_debug.eq.1)  write(*,*) 'const_surf_hash'
      call const_surf_hash(node1%numnod,                                &
     &   ele1%numele, ele1%nnod_4_ele, nnod_4_surf, ele1%ie,            &
     &    surf_ele_tbl%num_hash, surf_ele_tbl%istack_hash,              &
     &    surf_ele_tbl%iend_hash, surf_ele_tbl%id_hash)
!
      call const_all_surface_data(surf_ele_tbl)
!      call check_surface_data(my_rank)
!
!      call const_external_surface_data(surf_ele_tbl)
!      call check_external_surface(my_rank)
!
!      call const_isolate_surface_data(surf_ele_tbl)
!      call check_iso_surface(my_rank)
!
      if (iflag_debug.eq.1) write(*,*) 'dealloc_sum_hash(surf_ele_tbl)'
      call dealloc_sum_hash(surf_ele_tbl)
!
      end subroutine construct_surface_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_element_list_4_surface
!
      use set_element_list_4_surface
!
!
      call allocate_ele_4_surf
      call set_ele_list_4_surf(ele1%numele, numsurf, nsurf_4_ele,       &
     &    isf_4_ele, iele_4_surf)
!
      end subroutine const_element_list_4_surface
!
!------------------------------------------------------------------
!
      subroutine const_all_surface_data(sf_ele_tbl)
!
      use mark_surf_hash
      use set_surface_data
!
      type(sum_hash_tbl), intent(inout) :: sf_ele_tbl
!
!   mark for all surfaces
!
      if (iflag_debug.eq.1) write(*,*) 'mark_all_surfaces'
      call mark_all_surfaces(node1%numnod,                              &
     &    ele1%numele, ele1%nnod_4_ele, nnod_4_surf, ele1%ie,           &
     &    sf_ele_tbl%istack_hash, sf_ele_tbl%iend_hash,                 &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_all_surfaces'
      call count_all_surfaces                                           &
     &   (ele1%numele, sf_ele_tbl%iflag_hash, numsurf)
!
      call allocate_surface_connect
!
      if (iflag_debug.eq.1) write(*,*) 'set_all_surfaces'
      call set_all_surfaces(ele1%numele, numsurf, ele1%nnod_4_ele,      &
     &    nnod_4_surf, ele1%ie, node_on_sf, sf_ele_tbl%id_hash,         &
     &    sf_ele_tbl%iflag_hash, ie_surf, isf_4_ele)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_surf_rotation_flag'
      call set_surf_rotation_flag                                       &
     &   (ele1%numele, numsurf, ele1%nnod_4_ele,                        &
     &    nnod_4_surf, ele1%ie, ie_surf, isf_4_ele, isf_rot_ele)
!
      end subroutine const_all_surface_data
!
!------------------------------------------------------------------
!
      subroutine const_external_surface_data(sf_ele_tbl)
!
      use mark_surf_hash
      use set_surface_data
!
      type(sum_hash_tbl), intent(inout) :: sf_ele_tbl
!
!   mark for all surfaces
!
      if (iflag_debug.eq.1) write(*,*) 'mark_independent_surface'
      call mark_independent_surface(node1%numnod,                       &
     &    ele1%numele, ele1%nnod_4_ele, nnod_4_surf, ele1%ie,           &
     &    sf_ele_tbl%istack_hash, sf_ele_tbl%iend_hash,                 &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash)
!
      if (iflag_debug.eq.1) write(*,*) 'mark_external_surface'
      call mark_external_surface(node1%internal_node, node1%numnod,     &
     &    ele1%numele, ele1%nnod_4_ele, nnod_4_surf, ele1%ie,           &
     &    sf_ele_tbl%istack_hash, sf_ele_tbl%iend_hash,                 &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_part_surface'
      call count_part_surface                                           &
     &   (ele1%numele, ele1%numele, sf_ele_tbl%iflag_hash, numsurf_ext)
!
      call allocate_ext_surface
!
      if (iflag_debug.eq.1) write(*,*) 'set_part_surface'
      call set_part_surface                                             &
     &   (ele1%numele, ele1%numele, numsurf_ext, isf_4_ele,             &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash, isf_external)
!
      end subroutine const_external_surface_data
!
!------------------------------------------------------------------
!
      subroutine const_isolate_surface_data(sf_ele_tbl)
!
      use mark_surf_hash
      use set_surface_data
!
      type(sum_hash_tbl), intent(inout) :: sf_ele_tbl
!
!
!   mark independent surface
!
      if (iflag_debug.eq.1) write(*,*) 'mark_independent_surface'
      call mark_independent_surface(node1%numnod,                       &
     &   ele1%numele, ele1%nnod_4_ele, nnod_4_surf, ele1%ie,            &
     &    sf_ele_tbl%istack_hash, sf_ele_tbl%iend_hash,                 &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_part_surface'
      call count_part_surface                                           &
     &   (ele1%numele, ele1%numele, sf_ele_tbl%iflag_hash, numsurf_iso)
!
      call allocate_iso_surface
!
      if (iflag_debug.eq.1) write(*,*) 'set_part_surface'
      call set_part_surface                                             &
     &   (ele1%numele, ele1%numele, numsurf_iso, isf_4_ele,             &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash, isf_isolate)
!
      end subroutine const_isolate_surface_data
!
!------------------------------------------------------------------
!
      end module const_surface_data
