!>@file   const_surface_data
!!@brief  module const_surface_data
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2008
!
!>@brief Construct surface information form element connectivity
!!
!!@verbatim
!!      subroutine construct_surface_data(nod, ele, surf)
!!      subroutine const_surface_hash(nod, ele, surf, sf_ele_tbl)
!!      subroutine const_part_surface_hash(ele, nele_grp, item_grp,     &
!!     &          sf_ele_tbl)
!!        integer(kind = kint) :: nele_grp
!!        integer(kind = kint) :: item_grp(nele_grp)
!!        type(node_data),    intent(in) :: nod
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(sum_hash_tbl), intent(inout) :: sf_ele_tbl
!!
!!      subroutine const_ele_list_4_surface(ele, surf)
!!      subroutine empty_surface_connect(ele, surf)
!!        type(node_data),    intent(in) :: nod
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(inout) :: surf
!!@endverbatim
!
      module const_surface_data
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_sum_hash
!
      implicit none
!
!
      type(sum_hash_tbl), save, private :: surf_ele_tbl
!
      private :: const_external_surface_data
      private :: const_surface_hash
      private :: const_all_surface_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine construct_surface_data(nod, ele, surf)
!
      use set_surface_hash
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!   set hash data for suface elements using sum of local node ID
!
      call alloc_sum_hash(nod%numnod, ele%numele,                       &
     &    nsurf_4_ele, surf%nnod_4_surf, surf_ele_tbl)
!         surf_ele_tbl%ntot_id = nod%numnod * surf%nnod_4_surf
!         surf_ele_tbl%ntot_list = nsurf_4_ele * ele%numele
!
      call const_surface_hash(ele, surf_ele_tbl)
!
      call const_all_surface_data(ele, surf, surf_ele_tbl)
!
      call const_external_surface_data(nod, ele, surf, surf_ele_tbl)
      call const_isolate_surface_data(ele, surf, surf_ele_tbl)
!
!
      if (iflag_debug.eq.1) write(*,*) 'dealloc_sum_hash(surf_ele_tbl)'
      call dealloc_sum_hash(surf_ele_tbl)
!
      end subroutine construct_surface_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_ele_list_4_surface(ele, surf)
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
      end subroutine const_ele_list_4_surface
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine empty_surface_connect(ele, surf)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!
      surf%numsurf = 0
      if (iflag_debug.eq.1) write(*,*) 'allocate_surface_connect_type'
      call allocate_surface_connect_type(surf, ele%numele)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_surf_param_smp'
      call alloc_surf_param_smp(surf)
!
      end subroutine empty_surface_connect
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_surface_hash(ele, sf_ele_tbl)
!
      use set_surface_hash
!
      type(element_data), intent(in) :: ele
!
      type(sum_hash_tbl), intent(inout) :: sf_ele_tbl
!
!   set hash data for suface elements using sum of local node ID
!
!
      call count_surface_hash(ele%numele, ele%nnod_4_ele, ele%ie,       &
     &    surf_ele_tbl%ntot_id, sf_ele_tbl%num_hash,                    &
     &    sf_ele_tbl%istack_hash, sf_ele_tbl%iend_hash)
!
      call set_surf_hash(ele%numele, ele%nnod_4_ele, ele%ie,            &
     &    surf_ele_tbl%ntot_id, surf_ele_tbl%ntot_list,                 &
     &    sf_ele_tbl%num_hash, sf_ele_tbl%istack_hash,                  &
     &    sf_ele_tbl%id_hash)
!
      end subroutine const_surface_hash
!
! ----------------------------------------------------------------------
!
      subroutine const_part_surface_hash(ele, nele_grp, item_grp,       &
     &          sf_ele_tbl)
!
      use set_surface_hash
!
      type(element_data), intent(in) :: ele
      integer(kind = kint) :: nele_grp
      integer(kind = kint) :: item_grp(nele_grp)
!
      type(sum_hash_tbl), intent(inout) :: sf_ele_tbl
!
!
      call count_part_surface_hash                                      &
     &   (ele%numele, ele%nnod_4_ele, ele%ie, nele_grp, item_grp,       &
     &    sf_ele_tbl%ntot_id, sf_ele_tbl%num_hash,                      &
     &    sf_ele_tbl%istack_hash, sf_ele_tbl%iend_hash)
!
      call set_part_surf_hash                                           &
     &   (ele%numele, ele%nnod_4_ele, ele%ie, nele_grp, item_grp,       &
     &    sf_ele_tbl%ntot_id, sf_ele_tbl%ntot_list,                     &
     &    sf_ele_tbl%num_hash, sf_ele_tbl%istack_hash,                  &
     &    sf_ele_tbl%id_hash)
!
      end subroutine const_part_surface_hash
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_all_surface_data(ele, surf, sf_ele_tbl)
!
      use mark_surf_hash
      use set_surface_data
!
      type(element_data), intent(in) :: ele
!
      type(surface_data), intent(inout) :: surf
      type(sum_hash_tbl), intent(inout) :: sf_ele_tbl
!
!
!   mark for all surfaces
!
      if (iflag_debug.eq.1) write(*,*) 'mark_all_surfaces'
      call mark_all_surfaces(ele%numele, ele%nnod_4_ele, ele%ie,        &
     &    sf_ele_tbl%ntot_id, sf_ele_tbl%ntot_list,                     &
     &    sf_ele_tbl%istack_hash, sf_ele_tbl%iend_hash,                 &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_all_surfaces'
      call count_all_surfaces                                           &
     &   (sf_ele_tbl%ntot_list, sf_ele_tbl%iflag_hash, surf%numsurf)
!
      call allocate_surface_connect_type(surf, ele%numele)
!
      if (iflag_debug.eq.1) write(*,*) 'set_all_surfaces'
      call set_all_surfaces                                             &
     &   (ele%numele, surf%numsurf, ele%nnod_4_ele, surf%nnod_4_surf,   &
     &    ele%ie, surf%node_on_sf, sf_ele_tbl%ntot_list,                &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash,                    &
     &    surf%ie_surf, surf%isf_4_ele)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_surf_rotation_flag'
      call set_surf_rotation_flag(ele%numele, surf%numsurf,             &
     &    ele%nnod_4_ele, surf%nnod_4_surf, ele%ie, surf%ie_surf,       &
     &    surf%isf_4_ele, surf%isf_rot_ele)
!
      end subroutine const_all_surface_data
!
!------------------------------------------------------------------
!
      subroutine const_external_surface_data                            &
     &         (nod, ele, surf, sf_ele_tbl)
!
      use mark_surf_hash
      use set_surface_data
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(in) :: ele
!
      type(surface_data), intent(inout) :: surf
      type(sum_hash_tbl), intent(inout) :: sf_ele_tbl
!
!   mark for all surfaces
!
      if (iflag_debug.eq.1) write(*,*) 'mark_independent_surface'
      call mark_independent_surface(ele%numele, ele%nnod_4_ele, ele%ie, &
     &    sf_ele_tbl%ntot_id, sf_ele_tbl%ntot_list,                     &
     &    sf_ele_tbl%istack_hash, sf_ele_tbl%iend_hash,                 &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash)
!
      if (iflag_debug.eq.1) write(*,*) 'mark_external_surface'
      call mark_external_surface                                        &
     &   (nod%internal_node, ele%numele, ele%nnod_4_ele, ele%ie,        &
     &    sf_ele_tbl%ntot_id, sf_ele_tbl%ntot_list,                     &
     &    sf_ele_tbl%istack_hash, sf_ele_tbl%iend_hash,                 &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_part_surface'
      call count_part_surface(ele%numele, sf_ele_tbl%ntot_list,         &
     &    sf_ele_tbl%iflag_hash, surf%numsurf_ext)
!
      call allocate_ext_surface_type(surf)
!
      if (iflag_debug.eq.1) write(*,*) 'set_part_surface'
      call set_part_surface                                             &
     &   (ele%numele, ele%numele,  surf%numsurf_ext, surf%isf_4_ele,    &
     &    sf_ele_tbl%ntot_list, sf_ele_tbl%id_hash,                     &
     &    sf_ele_tbl%iflag_hash, surf%isf_external)
!
      end subroutine const_external_surface_data
!
!------------------------------------------------------------------
!
      subroutine const_isolate_surface_data(ele, surf, sf_ele_tbl)
!
      use mark_surf_hash
      use set_surface_data
!
      type(element_data), intent(in) :: ele
!
      type(surface_data), intent(inout) :: surf
      type(sum_hash_tbl), intent(inout) :: sf_ele_tbl
!
!   mark independent surface
!
      if (iflag_debug.eq.1) write(*,*) 'mark_independent_surface'
      call mark_independent_surface(ele%numele, ele%nnod_4_ele, ele%ie, &
     &    surf_ele_tbl%ntot_id, surf_ele_tbl%ntot_list,                 &
     &    sf_ele_tbl%istack_hash, sf_ele_tbl%iend_hash,                 &
     &    sf_ele_tbl%id_hash, sf_ele_tbl%iflag_hash)
!
!   set surface data
!
      if (iflag_debug.eq.1) write(*,*) 'count_part_surface'
      call count_part_surface(ele%numele, sf_ele_tbl%ntot_list,         &
     &    sf_ele_tbl%iflag_hash, surf%numsurf_iso)
!
      call allocate_iso_surface_type(surf)
!
      if (iflag_debug.eq.1) write(*,*) 'set_part_surface'
      call set_part_surface                                             &
     &   (ele%numele, ele%numele, surf%numsurf_iso, surf%isf_4_ele,     &
     &    sf_ele_tbl%ntot_list, sf_ele_tbl%id_hash,                     &
     &    sf_ele_tbl%iflag_hash, surf%isf_isolate)
!
      end subroutine const_isolate_surface_data
!
!------------------------------------------------------------------
!
      end module const_surface_data
