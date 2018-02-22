!count_number_with_overlap.f90
!      module count_number_with_overlap
!
!      Written by H. Matsui
!
!!      subroutine count_number_w_overlap(mesh_file, nnod_4_ele)
!!      subroutine count_subdomain_ngrp_stack(num_pe, sub, istack)
!
      module count_number_with_overlap
!
      use m_precision
!
      use m_constants
      use t_mesh_data
      use t_file_IO_parameter
!
      implicit none
!
      private :: count_numbers_4_mesh_merge
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_number_w_overlap(mesh_file, nnod_4_ele)
!
      use m_geometry_data_4_merge
!
      type(field_IO_params), intent(in) :: mesh_file
      integer (kind = kint), intent(inout) :: nnod_4_ele
!
!
      call count_numbers_4_mesh_merge(mesh_file, nnod_4_ele)
      call count_num_overlap_geom_type                                  &
     &   (mgd_mesh1%num_pe, subdomain, merge_tbl)
      call count_num_geometry_w_overlap                                 &
     &   (mgd_mesh1%num_pe, subdomain, merge_tbl, merged)
!
      end subroutine count_number_w_overlap
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_numbers_4_mesh_merge(mesh_file, nnod_4_ele)
!
      use m_geometry_data_4_merge
      use mesh_IO_select
      use set_read_geometry_2_merge
      use set_read_boundary_2_merge
      use set_element_data_4_IO
      use copy_mesh_structures
      use load_mesh_data
!
      type(field_IO_params), intent(in) :: mesh_file
      integer (kind = kint), intent(inout) :: nnod_4_ele
!
      integer (kind = kint) :: ip, my_rank, ierr
!
       type(mesh_data) :: fem_IO_o
!
!
      do ip = 1, mgd_mesh1%num_pe
        my_rank = ip - 1
        call sel_read_mesh(mesh_file, my_rank, fem_IO_o, ierr)
        if(ierr .gt. 0) stop 'Error in Mesh data'
!
        call set_mesh_geometry_data(fem_IO_o%mesh,                      &
     &      subdomain(ip)%nod_comm, subdomain(ip)%node,                 &
     &      subdomain(ip)%ele)
        call set_grp_data_from_IO(fem_IO_o%group,                       &
     &      mgd_mesh1%sub_nod_grp(ip), mgd_mesh1%sub_ele_grp(ip),       &
     &      mgd_mesh1%sub_surf_grp(ip))
        call dealloc_groups_data(fem_IO_o%group)
      end do
!
      nnod_4_ele = fem_IO_o%mesh%ele%nnod_4_ele
!
      end subroutine count_numbers_4_mesh_merge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_num_geometry_w_overlap                           &
     &         (num_pe, subdomain, table, merged)
!
      use t_merged_geometry_data
!
      integer(kind = kint), intent(in) :: num_pe
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(merged_stacks), intent(in) :: table
!
      type(mesh_geometry), intent(inout) :: merged
!
!
      merged%node%numnod =        table%istack_nod(num_pe)
      merged%node%internal_node = table%istack_inter(num_pe)
      merged%ele%numele =         table%istack_ele(num_pe)
      merged%ele%nnod_4_ele =     subdomain(1)%ele%nnod_4_ele
!
      end subroutine count_num_geometry_w_overlap
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_overlap_geom_type(num_pe, subdomain, table)
!
      use m_constants
      use t_merged_geometry_data
!
      integer(kind = kint), intent(in) :: num_pe
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(merged_stacks), intent(inout) :: table
!
      integer(kind = kint) :: ip
!
!
      table%istack_nod(0) =    izero
      table%istack_inter(0) =  izero
      table%istack_ele(0) =    izero
      table%istack_nod(1) =    subdomain(1)%node%numnod
      table%istack_inter(1) =  subdomain(1)%node%internal_node
      table%istack_ele(1) =    subdomain(1)%ele%numele
      table%nnod_max =         subdomain(1)%node%numnod
!
      do ip = 2, num_pe
        table%nnod_max = max(subdomain(ip)%node%numnod, table%nnod_max)
        table%istack_nod(ip) =    table%istack_nod(ip-1)                &
     &       + subdomain(ip)%node%numnod
        table%istack_inter(ip) = table%istack_inter(ip-1)               &
     &       + subdomain(ip)%node%internal_node
        table%istack_ele(ip) =    table%istack_ele(ip-1)                &
     &       +  subdomain(ip)%ele%numele
      end do
      table%nnod_overlap = table%istack_nod(num_pe)
      table%nele_overlap = table%istack_ele(num_pe)
!
      end subroutine count_num_overlap_geom_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_subdomain_ngrp_stack(num_pe, sub, istack)
!
      use t_group_data
!
      integer (kind = kint), intent(in) :: num_pe
      type(group_data), intent(in) :: sub(num_pe)
!
      integer (kind = kint), intent(inout) :: istack(0:num_pe)
!
      integer(kind = kint) :: ip
!
!
      istack(0) = izero
      do ip = 1, num_pe
        istack(ip) = istack(ip-1) + sub(ip)%num_grp
      end do
!
      end subroutine count_subdomain_ngrp_stack
!
!  ---------------------------------------------------------------------
!
      subroutine count_subdomain_item_stack(num_pe, sub, istack)
!
      use t_group_data
!
      integer (kind = kint), intent(in) :: num_pe
      type(group_data), intent(in) :: sub(num_pe)
!
      integer (kind = kint), intent(inout) :: istack(0:num_pe)
!
      integer(kind = kint) :: ip
!
!
      istack(0) = izero
      do ip = 1, num_pe
        istack(ip) = istack(ip-1) + sub(ip)%num_item
      end do
!
      end subroutine count_subdomain_item_stack
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_subdomain_sf_ngrp_stack(num_pe, sub_sf, istack)
!
      use t_group_data
!
      integer (kind = kint), intent(in) :: num_pe
      type(surface_group_data), intent(in) :: sub_sf(num_pe)
!
      integer (kind = kint), intent(inout) :: istack(0:num_pe)
!
      integer(kind = kint) :: ip
!
!
      istack(0) = izero
      do ip = 1, num_pe
        istack(ip) = istack(ip-1) + sub_sf(ip)%num_grp
      end do
!
      end subroutine count_subdomain_sf_ngrp_stack
!
!  ---------------------------------------------------------------------
!
      subroutine count_subdomain_sf_item_stack(num_pe, sub_sf, istack)
!
      use t_group_data
!
      integer (kind = kint), intent(in) :: num_pe
      type(surface_group_data), intent(in) :: sub_sf(num_pe)
!
      integer (kind = kint), intent(inout) :: istack(0:num_pe)
!
      integer(kind = kint) :: ip
!
!
      istack(0) = izero
      do ip = 1, num_pe
        istack(ip) = istack(ip-1) + sub_sf(ip)%num_item
      end do
!
      end subroutine count_subdomain_sf_item_stack
!
!  ---------------------------------------------------------------------
!
      end module count_number_with_overlap
