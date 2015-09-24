!count_number_with_overlap.f90
!      module count_number_with_overlap
!
!      Written by H. Matsui
!
!      subroutine count_number_w_overlap
!
      module count_number_with_overlap
!
      use m_precision
!
      use m_constants
      use m_geometry_data_4_merge
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
      subroutine count_number_w_overlap
!
!
      call count_numbers_4_mesh_merge
      call count_num_geometry_w_overlap
!
      end subroutine count_number_w_overlap
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_numbers_4_mesh_merge
!
       use m_geometry_data
       use m_comm_data_IO
       use m_read_boundary_data
       use mesh_IO_select
       use set_read_geometry_2_merge
       use set_read_boundary_2_merge
       use set_comm_table_4_IO
       use set_element_data_4_IO
       use set_node_data_4_IO
       use set_group_types_4_IO
!
       integer (kind = kint) :: ip, my_rank
!
!
      do ip =1, num_pe
        my_rank = ip - 1
!
        call sel_read_mesh(my_rank)
!
        call copy_comm_tbl_type_from_IO(subdomain(ip)%nod_comm)
        call copy_node_geometry_from_IO(subdomain(ip)%node)
        call copy_ele_connect_from_IO(subdomain(ip)%ele)
!
        call allocate_sph_node_geometry(subdomain(ip)%node)
!
!  set node group
        call set_grp_data_from_IO                                       &
     &     (sub_nod_grp(ip), sub_ele_grp(ip), sub_surf_grp(ip))
      end do
!
      ele1%nnod_4_ele = nnod_4_ele_dummy
!
      end subroutine count_numbers_4_mesh_merge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_num_geometry_w_overlap
!
!
      call count_num_overlap_geom_type(num_pe, subdomain, merge_tbl)
!
      merged%node%numnod =        merge_tbl%istack_nod(num_pe)
      merged%node%internal_node = merge_tbl%istack_inter(num_pe)
      merged%ele%numele =         merge_tbl%istack_ele(num_pe)
      merged%ele%nnod_4_ele =     subdomain(1)%ele%nnod_4_ele
!
      end subroutine count_num_geometry_w_overlap
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_overlap_geom_type(num_pe, subdomain, table)
!
      use m_constants
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
!
      subroutine count_num_group_w_overlap
!
!
      call count_subdomain_ngrp_stack(num_pe, sub_nod_grp,              &
     &    istack_bc_pe)
      call count_subdomain_ngrp_stack(num_pe, sub_ele_grp,              &
     &    istack_mat_pe)
!
      call count_subdomain_sf_ngrp_stack(num_pe, sub_surf_grp,          &
     &    istack_surf_pe)
!
      end subroutine count_num_group_w_overlap
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
