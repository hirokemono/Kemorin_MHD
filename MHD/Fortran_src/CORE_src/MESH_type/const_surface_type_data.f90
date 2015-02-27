!const_surface_type_data.f90
!      module const_surface_type_data
!
!     Written by H. Matsui on Dec., 2008
!
!
!      subroutine s_const_surface_type_data(nod, ele, surf)
!      subroutine const_isolated_surface_t_data(nod, ele, surf)
!      subroutine const_external_surface_t_data(nod, ele, surf)
!      subroutine const_surface_type_hash(nod, ele, surf)
!      subroutine const_part_surface_type_hash(nele_grp, item_grp,      &
!     &          nod, ele, surf)
!        integer(kind = kint) :: nele_grp
!        integer(kind = kint) :: item_grp(nele_grp)
!        type(node_data),    intent(in) :: nod
!        type(element_data), intent(in) :: ele
!        type(surface_data), intent(inout) :: surf
!
!      subroutine const_ele_list_4_surf_type(ele, surf)
!      subroutine empty_surf_connect_type(ele, surf)
!        type(node_data),    intent(in) :: nod
!        type(element_data), intent(in) :: ele
!        type(surface_data), intent(inout) :: surf
!
      module const_surface_type_data
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
!
      implicit none
!
      private :: const_external_surf_type_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_const_surface_type_data(nod, ele, surf)
!
      use m_surface_hash
      use set_surface_hash
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!   set hash data for suface elements using sum of local node ID
!
      call const_surface_type_hash(nod, ele, surf)
!
      call const_all_surf_type_data(ele, surf)
!
!      call const_external_surf_type_data(nod, ele, surf)
!
!      call const_isolate_surf_type_data(ele, surf)
!
!
      if (iflag_debug.eq.1) write(*,*) 'deallocate_surface_hash'
      call deallocate_surface_hash
!
      end subroutine s_const_surface_type_data
!
!------------------------------------------------------------------
!
      subroutine const_isolated_surface_t_data(nod, ele, surf)
!
      use m_surface_hash
      use set_surface_hash
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!
      call const_surface_type_hash(nod, ele, surf)
      call const_all_surf_type_data(ele, surf)
!
      call const_isolate_surf_type_data(ele, surf)
!
      call deallocate_surface_hash
!
      end subroutine const_isolated_surface_t_data
!
!------------------------------------------------------------------
!
      subroutine const_external_surface_t_data(nod, ele, surf)
!
      use m_surface_hash
      use set_surface_hash
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!
      call const_surface_type_hash(nod, ele, surf)
      call const_all_surf_type_data(ele, surf)
!
      call const_external_surf_type_data(nod, ele, surf)
!
      call deallocate_surface_hash
!
      end subroutine const_external_surface_t_data
!
!------------------------------------------------------------------
!
      subroutine const_ele_list_4_surf_type(ele, surf)
!
      use set_element_list_4_surface
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!
      call alloc_ele_4_surf_type(surf)
      call set_ele_list_4_surf(ele%numele, surf%numsurf,                &
     &     nsurf_4_ele, surf%isf_4_ele, surf%iele_4_surf)
!
      end subroutine const_ele_list_4_surf_type
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine empty_surf_connect_type(ele, surf)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!
      surf%numsurf = 0
      if (iflag_debug.eq.1) write(*,*) 'allocate_surface_connect_type'
      call allocate_surface_connect_type(surf, ele%numele)
!
      if (iflag_debug.eq.1) write(*,*) 'count_overlap_surface'
      call allocate_surf_param_smp_type(surf)
!
      end subroutine empty_surf_connect_type
!
! ----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_surface_type_hash(nod, ele, surf)
!
      use m_surface_hash
      use set_surface_hash
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!   set hash data for suface elements using sum of local node ID
!
!
      call allocate_surface_hash(nod%numnod, ele%numele,                &
     &    surf%nnod_4_surf)
!
      if (iflag_debug.eq.1)  write(*,*) 'count_surface_hash'
      call count_surface_hash(nod%numnod, ele%numele, ele%nnod_4_ele,   &
     &    surf%nnod_4_surf, ele%ie)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_surf_hash'
      call set_surf_hash(ele%numele, ele%nnod_4_ele, ele%ie)
!
      end subroutine const_surface_type_hash
!
! ----------------------------------------------------------------------
!
      subroutine const_part_surface_type_hash(nele_grp, item_grp,       &
     &          nod, ele, surf)
!
      use m_surface_hash
      use set_surface_hash
!
      integer(kind = kint) :: nele_grp
      integer(kind = kint) :: item_grp(nele_grp)
      type(node_data),    intent(in) :: nod
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!
      call allocate_surface_hash(nod%numnod, ele%numele,                &
     &    surf%nnod_4_surf)
!
      call count_part_surface_hash(nod%numnod, ele%numele,              &
     &      nele_grp, ele%nnod_4_ele, surf%nnod_4_surf, ele%ie,         &
     &      item_grp )
!
      call set_part_surf_hash(ele%numele, nele_grp, ele%nnod_4_ele,     &
     &      ele%ie,  item_grp)
!
      end subroutine const_part_surface_type_hash
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_all_surf_type_data(ele, surf)
!
      use mark_surf_hash
      use set_surface_data
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!
!   mark for all surfaces
!
      if (iflag_debug.eq.1) write(*,*) 'mark_all_surfaces'
      call mark_all_surfaces(ele%numele, ele%nnod_4_ele, ele%ie)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_all_surfaces'
      call count_all_surfaces(ele%numele, surf%numsurf)
!
      call allocate_surface_connect_type(surf, ele%numele)
!
      if (iflag_debug.eq.1) write(*,*) 'set_all_surfaces'
      call set_all_surfaces(ele%numele, surf%numsurf, ele%nnod_4_ele,   &
     &    surf%nnod_4_surf, ele%ie, surf%node_on_sf, surf%ie_surf,      &
     &    surf%isf_4_ele)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_surf_rotation_flag'
      call set_surf_rotation_flag(ele%numele, surf%numsurf,             &
     &    ele%nnod_4_ele, surf%nnod_4_surf, ele%ie, surf%ie_surf,       &
     &    surf%isf_4_ele, surf%isf_rot_ele)
!
      end subroutine const_all_surf_type_data
!
!------------------------------------------------------------------
!
      subroutine const_external_surf_type_data(nod, ele, surf)
!
      use mark_surf_hash
      use set_surface_data
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!   mark for all surfaces
!
      if (iflag_debug.eq.1) write(*,*) 'mark_independent_surface'
      call mark_independent_surface(ele%numele, ele%nnod_4_ele, ele%ie)
!
      if (iflag_debug.eq.1) write(*,*) 'mark_external_surface'
      call mark_external_surface(nod%internal_node,                     &
     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_part_surface'
      call count_part_surface(ele%numele, surf%numsurf_ext)
!
      call allocate_ext_surface_type(surf)
!
      if (iflag_debug.eq.1) write(*,*) 'set_part_surface'
      call set_part_surface(ele%numele, ele%numele,                     &
     &    surf%numsurf_ext, surf%isf_4_ele, surf%isf_external)
!
      end subroutine const_external_surf_type_data
!
!------------------------------------------------------------------
!
      subroutine const_isolate_surf_type_data(ele, surf)
!
      use mark_surf_hash
      use set_surface_data
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!   mark independent surface
!
      if (iflag_debug.eq.1) write(*,*) 'mark_independent_surface'
      call mark_independent_surface(ele%numele, ele%nnod_4_ele, ele%ie)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_part_surface'
      call count_part_surface(ele%numele, surf%numsurf_iso)
!
      call allocate_iso_surface_type(surf)
!
      if (iflag_debug.eq.1) write(*,*) 'set_part_surface'
      call set_part_surface(ele%numele, ele%numele,                     &
     &    surf%numsurf_iso, surf%isf_4_ele, surf%isf_isolate)
!
      end subroutine const_isolate_surf_type_data
!
!------------------------------------------------------------------
!
      end module const_surface_type_data
