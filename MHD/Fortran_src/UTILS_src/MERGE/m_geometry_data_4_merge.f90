!
!      module m_geometry_data_4_merge
!
!      Written by H. Matsui
!      Modified by H. Matsui on Apr., 2012
!
!      subroutine allocate_geometry_data_4_merge
!
!      subroutine allocate_number_of_mesh
!      subroutine allocate_array_4_node
!      subroutine allocate_array_4_element
!      subroutine allocate_subdomain_grp_stack
!
!      subroutine allocate_merged_group_num
!      subroutine allocate_merged_group_item
!
!      subroutine allocate_merged_field_name
!      subroutine allocate_merged_field_data
!
!      subroutine deallocate_array_4_merge
!      subroutine deallocate_number_of_mesh
!
!      subroutine deallocate_array_4_node
!      subroutine deallocate_array_4_element
!      subroutine deallocate_geom_ex_glnod
!
!      subroutine dealloc_subdomain_groups
!
!      subroutine deallocate_subdomain_grp_stack
!
!      subroutine deallocate_merged_field_name
!      subroutine deallocate_merged_field_data
!
!      subroutine check_boundary_data_m
!      subroutine check_material_data_m
!      subroutine check_surface_data_m
!
      module m_geometry_data_4_merge
!
      use m_precision
!
      use m_constants
      use t_mesh_data
      use t_group_data
      use t_merged_geometry_data
      use t_phys_data
!
      implicit    none
!
!  ==============================
! . for mesh data & result data
!  ==============================

      integer(kind=kint )  :: num_pe
!>      number of subdomains
      type(mesh_geometry), allocatable :: subdomain(:)
!>      subdomain mesh data
!
      type(mesh_geometry) :: merged
!>      merged mesh data
      type(phys_data) :: merged_fld
!>      merged field data
!
      type(merged_stacks) :: merge_tbl
!>      merged index table
!
      type(mesh_groups) :: merged_grp
!
      type(group_data), allocatable :: sub_nod_grp(:)
      type(group_data), allocatable :: sub_ele_grp(:)
      type(surface_group_data), allocatable :: sub_surf_grp(:)
!
!   stacks for group data
!
      integer (kind=kint), allocatable :: istack_bc_pe(:)
      integer (kind=kint), allocatable :: istack_mat_pe(:)
      integer (kind=kint), allocatable :: istack_surf_pe(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_geometry_data_4_merge
!
!
      call allocate_array_4_node
      call allocate_array_4_element
!
      end subroutine allocate_geometry_data_4_merge
!
!------------------------------------------------------------------
!
      subroutine allocate_number_of_mesh
!
!
      merge_tbl%num_subdomain = num_pe
      allocate( subdomain(num_pe) )
!
      call alloc_subdomain_stack(num_pe, merge_tbl)
!
      end subroutine allocate_number_of_mesh
!
!------------------------------------------------------------------
!
      subroutine deallocate_number_of_mesh
!
      call dealloc_subdomain_stack(merge_tbl)
!
      end subroutine deallocate_number_of_mesh
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_array_4_node
!
      use t_geometry_data
!
      integer(kind = kint) :: i
!
!
      call allocate_node_geometry_type(merged%node)
      call alloc_local_nod_id_tbl(merge_tbl)
!
      do i = 1, merged%node%numnod
        merged%node%inod_global(i) = i
      end do
!
      end subroutine allocate_array_4_node
!
!------------------------------------------------------------------
!
      subroutine allocate_array_4_element
!
      use m_geometry_parameter
      use t_geometry_data
!
      integer(kind = kint) :: i
!
!
      call allocate_ele_connect_type(merged%ele)
      call alloc_local_ele_id_tbl(merge_tbl)
!
      do i = 1, merged%ele%numele
        merged%ele%iele_global(i) = i
      end do
!
      end subroutine allocate_array_4_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine allocate_subdomain_grp_stack
!
       allocate ( sub_nod_grp(num_pe) )
       allocate ( sub_ele_grp(num_pe) )
       allocate ( sub_surf_grp(num_pe) )
!
       allocate ( istack_bc_pe(0:num_pe) )
       allocate ( istack_mat_pe(0:num_pe) )
       allocate ( istack_surf_pe(0:num_pe) )
!
       istack_bc_pe = 0
       istack_mat_pe = 0
       istack_surf_pe = 0
!
       end subroutine allocate_subdomain_grp_stack
!
!------------------------------------------------------------------
!
       subroutine allocate_merged_group_num
!
!
       call allocate_grp_type_num(merged_grp%nod_grp)
       call allocate_grp_type_num(merged_grp%ele_grp)
       call allocate_sf_grp_type_num(merged_grp%surf_grp)
!
       end subroutine allocate_merged_group_num
!
!------------------------------------------------------------------
!
       subroutine allocate_merged_group_item
!
!
       call allocate_grp_type_item(merged_grp%nod_grp)
       call allocate_grp_type_item(merged_grp%ele_grp)
       call allocate_sf_grp_type_item(merged_grp%surf_grp)
!
       end subroutine allocate_merged_group_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine allocate_merged_field_name
!
!
       call alloc_phys_name_type(merged_fld)
!
       end subroutine allocate_merged_field_name
!
!------------------------------------------------------------------
!
       subroutine allocate_merged_field_data
!
!
       call alloc_phys_data_type(merged%node%numnod, merged_fld)
!
       end subroutine allocate_merged_field_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_array_4_merge
!
!
      call deallocate_array_4_node
      call deallocate_array_4_element
!
      end subroutine deallocate_array_4_merge
!
!------------------------------------------------------------------
!
      subroutine deallocate_array_4_node
!
      call dealloc_local_nod_id_tbl(merge_tbl)
!
      end subroutine deallocate_array_4_node
!
!------------------------------------------------------------------
!
      subroutine deallocate_array_4_element
!
      call dealloc_local_ele_id_tbl(merge_tbl)
!
      end subroutine deallocate_array_4_element
!
!------------------------------------------------------------------
!
      subroutine deallocate_geom_ex_glnod
!
      call deallocate_node_geometry_type(merged%node)
!
      end subroutine deallocate_geom_ex_glnod
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_subdomain_groups
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, num_pe
        call deallocate_grp_type( sub_nod_grp(ip) )
        call deallocate_grp_type( sub_ele_grp(ip) )
        call deallocate_sf_grp_type( sub_surf_grp(ip) )
      end do
!
      end subroutine dealloc_subdomain_groups
!
!-----------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine deallocate_subdomain_grp_stack
!
!
       deallocate ( istack_bc_pe )
       deallocate ( istack_mat_pe )
       deallocate ( istack_surf_pe )
!
       end subroutine deallocate_subdomain_grp_stack
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine deallocate_merged_field_name
!
       call dealloc_phys_name_type(merged_fld)
!
       end subroutine deallocate_merged_field_name
!
!------------------------------------------------------------------
!
      subroutine deallocate_merged_field_data
!
      call dealloc_phys_data_type(merged_fld)
!
      end subroutine deallocate_merged_field_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine check_boundary_data_m
!
      integer(kind = kint) :: ip
!
      do ip = 1, num_pe
        call check_group_type_data(izero, sub_nod_grp(ip))
      end do
!
      end subroutine check_boundary_data_m
!
!-----------------------------------------------------------------------
!
      subroutine check_material_data_m
!
      integer(kind = kint) :: ip
!
      do ip = 1, num_pe
        call check_group_type_data(izero, sub_ele_grp(ip))
      end do
!
      end subroutine check_material_data_m
!
!-----------------------------------------------------------------------
!
       subroutine check_surface_data_m
!
       integer(kind = kint) :: ip
!
       do ip = 1, num_pe
         call check_surf_grp_type_data(izero, sub_surf_grp(ip))
       end do
!
       end subroutine check_surface_data_m
!
!-----------------------------------------------------------------------
!
      end module m_geometry_data_4_merge
