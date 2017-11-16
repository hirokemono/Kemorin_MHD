!set_patches_for_psf.f90
!      module set_patches_for_psf
!
!      Written by H. Matsui on June, 2006
!
!!      subroutine count_psf_patches                                    &
!!     &        (num_psf, node, ele, edge, sf_grp,                      &
!!     &         psf_case_tbls, psf_def, psf_search, psf_list, psf_mesh)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(psf_cases), intent(in) :: psf_case_tbls
!!        type(section_define), intent(in) :: psf_def(num_psf)
!!        type(psf_search_lists), intent(inout) :: psf_search(num_psf)
!!        type(sectioning_list), intent(inout) :: psf_list(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine count_iso_patches(num_iso, node, ele, edge,          &
!!     &          psf_case_tbls, iso_search, iso_list, iso_mesh)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(psf_cases), intent(in) :: psf_case_tbls
!!        type(psf_search_lists), intent(inout) :: iso_search(num_iso)
!!        type(sectioning_list), intent(inout) :: iso_list(num_iso)
!!        type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!!
!!      subroutine set_psf_patches                                      &
!!     &         (num_psf, ele, edge, sf_grp, psf_case_tbls, psf_def,   &
!!     &          psf_search, psf_list, psf_grp_list, psf_mesh)
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(psf_cases), intent(in) :: psf_case_tbls
!!        type(section_define), intent(in) :: psf_def(num_psf)
!!        type(psf_search_lists), intent(in) :: psf_search(num_psf)
!!        type(sectioning_list), intent(in) :: psf_list(num_psf)
!!        type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine set_iso_patches(num_iso, ele, edge,                  &
!!     &          psf_case_tbls, iso_search, iso_list, iso_mesh)
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(psf_cases), intent(in) :: psf_case_tbls
!!        type(psf_search_lists), intent(in) :: iso_search(num_iso)
!!        type(sectioning_list), intent(in) :: iso_list(num_iso)
!!        type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!!
      module set_patches_for_psf
!
      use m_precision
!
      use m_machine_parameter
      use t_psf_case_table
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_psf_patches                                      &
     &        (num_psf, node, ele, edge, sf_grp,                        &
     &         psf_case_tbls, psf_def, psf_search, psf_list, psf_mesh)
!
      use m_geometry_constants
      use t_geometry_data
      use t_edge_data
      use t_group_data
      use t_control_params_4_psf
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use set_psf_patch_4_by_surf_grp
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: num_psf
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(surface_group_data), intent(in) :: sf_grp
!
      type(psf_cases), intent(in) :: psf_case_tbls
      type(section_define), intent(in) :: psf_def(num_psf)
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
      type(sectioning_list), intent(inout) :: psf_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_psf
        psf_mesh(i)%patch%istack_ele_smp(0) = 0
        call alloc_mark_ele_psf(psf_search(i))
!
        if(psf_def(i)%id_section_method .gt. 0) then
          call set_psf_type_id                                          &
     &       (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,          &
     &        psf_search(i)%elem_list, psf_search(i)%mark_e,            &
     &        psf_list(i)%ref_fld)
!
          call count_num_patch_4_psf                                    &
     &       (ele%numele, edge%numedge, edge%iedge_4_ele,               &
     &        psf_search(i)%elem_list, psf_case_tbls%num_case_tbl,      &
     &        psf_case_tbls%psf_case_tbl, psf_search(i)%mark_e,         &
     &        psf_list(i)%id_n_on_e, psf_mesh(i)%patch%istack_ele_smp)
!
        else if(psf_def(i)%id_section_method .eq. 0) then
          call count_num_patch_4_grp                                    &
     &      (sf_grp%num_grp, sf_grp%istack_grp,                         &
     &       psf_def(i)%id_psf_group, psf_mesh(i)%patch%istack_ele_smp)
!
        end if
        psf_mesh(i)%patch%numele                                        &
      &       = psf_mesh(i)%patch%istack_ele_smp(np_smp)
        psf_mesh(i)%patch%internal_ele = psf_mesh(i)%patch%numele
        psf_mesh(i)%patch%nnod_4_ele = num_triangle
      end do
!
      end subroutine count_psf_patches
!
!  ---------------------------------------------------------------------
!
      subroutine count_iso_patches(num_iso, node, ele, edge,            &
     &          psf_case_tbls, iso_search, iso_list, iso_mesh)
!
      use m_geometry_constants
      use t_geometry_data
      use t_edge_data
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: num_iso
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(psf_cases), intent(in) :: psf_case_tbls
!
      type(psf_search_lists), intent(inout) :: iso_search(num_iso)
      type(sectioning_list), intent(inout) :: iso_list(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_iso
        iso_mesh(i)%patch%istack_ele_smp(0) = 0
        call alloc_mark_ele_psf(iso_search(i))
!
        call set_psf_type_id                                            &
     &     (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,            &
     &      iso_search(i)%elem_list, iso_search(i)%mark_e,              &
     &      iso_list(i)%ref_fld)
!
        call count_num_patch_4_psf                                      &
     &     (ele%numele, edge%numedge, edge%iedge_4_ele,                 &
     &      iso_search(i)%elem_list,                                    &
     &      psf_case_tbls%num_case_tbl, psf_case_tbls%psf_case_tbl,     &
     &      iso_search(i)%mark_e, iso_list(i)%id_n_on_e,                &
     &      iso_mesh(i)%patch%istack_ele_smp)
        iso_mesh(i)%patch%numele                                        &
      &       = iso_mesh(i)%patch%istack_ele_smp(np_smp)
        iso_mesh(i)%patch%internal_ele = iso_mesh(i)%patch%numele
        iso_mesh(i)%patch%nnod_4_ele = num_triangle
      end do
!
      end subroutine count_iso_patches
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_psf_patches                                        &
     &         (num_psf, ele, edge, sf_grp, psf_case_tbls, psf_def,     &
     &          psf_search, psf_list, psf_grp_list, psf_mesh)
!
      use calypso_mpi
      use m_geometry_constants
      use t_geometry_data
      use t_edge_data
      use t_group_data
      use t_control_params_4_psf
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use const_element_comm_tables
      use set_psf_patch_4_by_surf_grp
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: num_psf
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(surface_group_data), intent(in) :: sf_grp
!
      type(psf_cases), intent(in) :: psf_case_tbls
      type(section_define), intent(in) :: psf_def(num_psf)
      type(psf_search_lists), intent(in) :: psf_search(num_psf)
      type(sectioning_list), intent(in) :: psf_list(num_psf)
      type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call allocate_ele_connect_type(psf_mesh(i)%patch)
        call const_global_numele_list(psf_mesh(i)%patch)
!
        if(psf_def(i)%id_section_method .gt. 0) then
!
          call set_patch_4_psf                                          &
     &       (ele%numele, edge%numedge, edge%iedge_4_ele,               &
     &        psf_search(i)%elem_list,                                  &
     &        psf_case_tbls%num_case_tbl, psf_case_tbls%psf_case_tbl,   &
     &        psf_search(i)%mark_e, psf_list(i)%id_n_on_e,              &
     &        psf_mesh(i)%patch%istack_numele(my_rank),                 &
     &        psf_mesh(i)%patch%numele,                                 &
     &        psf_mesh(i)%patch%istack_ele_smp,                         &
     &        psf_mesh(i)%patch%iele_global, psf_mesh(i)%patch%ie)
!
        else if(psf_def(i)%id_section_method .eq. 0) then
!
          call set_patch_4_grp                                          &
     &       (ele%numele, ele%numele, ele%nnod_4_ele, ele%ie,           &
     &        sf_grp%num_grp, sf_grp%num_item, sf_grp%istack_grp,       &
     &        sf_grp%item_sf_grp, psf_def(i)%id_psf_group,              &
     &        psf_grp_list(i)%id_n_on_n,                                &
     &        psf_mesh(i)%patch%istack_numele(my_rank),                 &
     &        psf_mesh(i)%patch%numele,                                 &
     &        psf_mesh(i)%patch%istack_ele_smp,                         &
     &        psf_mesh(i)%patch%iele_global, psf_mesh(i)%patch%ie)
!
        end if
      end do
!
      end subroutine set_psf_patches
!
!  ---------------------------------------------------------------------
!
      subroutine set_iso_patches(num_iso, ele, edge,                    &
     &          psf_case_tbls, iso_search, iso_list, iso_mesh)
!
      use calypso_mpi
      use m_geometry_constants
      use t_geometry_data
      use t_edge_data
      use t_psf_geometry_list
      use t_psf_patch_data
!
      use const_element_comm_tables
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: num_iso
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
!
      type(psf_cases), intent(in) :: psf_case_tbls
      type(psf_search_lists), intent(in) :: iso_search(num_iso)
      type(sectioning_list), intent(in) :: iso_list(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind = kint) :: i
!
      do i = 1, num_iso
        call allocate_ele_connect_type(iso_mesh(i)%patch)
        call const_global_numele_list(iso_mesh(i)%patch)
!
        call set_patch_4_psf                                            &
     &     (ele%numele, edge%numedge, edge%iedge_4_ele,                 &
     &      iso_search(i)%elem_list,                                    &
     &      psf_case_tbls%num_case_tbl, psf_case_tbls%psf_case_tbl,     &
     &      iso_search(i)%mark_e, iso_list(i)%id_n_on_e,                &
     &      iso_mesh(i)%patch%istack_numele(my_rank),                   &
     &      iso_mesh(i)%patch%numele, iso_mesh(i)%patch%istack_ele_smp, &
     &      iso_mesh(i)%patch%iele_global, iso_mesh(i)%patch%ie)
      end do
!
      end subroutine set_iso_patches
!
!  ---------------------------------------------------------------------
!
      end module set_patches_for_psf
