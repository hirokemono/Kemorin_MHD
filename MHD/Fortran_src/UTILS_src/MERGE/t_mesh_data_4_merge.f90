!>@file   t_mesh_data_4_merge.f90
!!@brief  module t_mesh_data_4_merge
!!
!!@author  H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief Structure for merged mesh data
!!
!!@verbatim
!!      subroutine alloc_geometry_data_4_merge(mgd_mesh)
!!
!!      subroutine alloc_number_of_mesh(mgd_mesh)
!!      subroutine alloc_array_4_node(mgd_mesh)
!!      subroutine alloc_array_4_element(mgd_mesh)
!!      subroutine alloc_subdomain_grp_stack(mgd_mesh)
!!
!!      subroutine alloc_merged_group_num(mgd_mesh)
!!      subroutine alloc_merged_group_item(mgd_mesh)
!!
!!      subroutine dealloc_array_4_merge(mgd_mesh)
!!      subroutine dealloc_number_of_mesh(mgd_mesh)
!!
!!      subroutine dealloc_subdomain_groups(mgd_mesh)
!!
!!      subroutine dealloc_subdomain_grp_stack(mgd_mesh)
!!
!!      subroutine check_boundary_data_m(ip, mgd_mesh)
!!      subroutine check_material_data_m(ip, mgd_mesh)
!!      subroutine check_surface_data_m(ip, mgd_mesh)
!!@endverbatim
!
      module t_mesh_data_4_merge
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
!
      type merged_mesh
        integer(kind = kint)  :: num_pe
!>        number of subdomains
        type(mesh_geometry), allocatable :: subdomain(:)
!>        subdomain mesh data
!
        type(mesh_geometry) :: merged
!>        merged mesh data
        type(phys_data) :: merged_fld
!>        merged field data
!
        type(merged_stacks) :: merge_tbl
!>        merged index table
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
      end type merged_mesh
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_geometry_data_4_merge(mgd_mesh)
!
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
      call alloc_array_4_node(mgd_mesh%merged, mgd_mesh%merge_tbl)
      call alloc_array_4_element(mgd_mesh%merged, mgd_mesh%merge_tbl)
!
      end subroutine alloc_geometry_data_4_merge
!
!------------------------------------------------------------------
!
      subroutine alloc_number_of_mesh(mgd_mesh)
!
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
      mgd_mesh%merge_tbl%num_subdomain = mgd_mesh%num_pe
      allocate( mgd_mesh%subdomain(mgd_mesh%num_pe) )
!
      call alloc_subdomain_stack(mgd_mesh%num_pe, mgd_mesh%merge_tbl)
!
      end subroutine alloc_number_of_mesh
!
!------------------------------------------------------------------
!
      subroutine dealloc_number_of_mesh(mgd_mesh)
!
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
      call dealloc_subdomain_stack(mgd_mesh%merge_tbl)
!
      end subroutine dealloc_number_of_mesh
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_array_4_node(merged, merge_tbl)
!
      use t_geometry_data
!
      type(mesh_geometry), intent(inout) :: merged
        type(merged_stacks), intent(inout) :: merge_tbl
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
      end subroutine alloc_array_4_node
!
!------------------------------------------------------------------
!
      subroutine alloc_array_4_element(merged, merge_tbl)
!
      use t_geometry_data
!
      type(mesh_geometry), intent(inout) :: merged
        type(merged_stacks), intent(inout) :: merge_tbl
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
      end subroutine alloc_array_4_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine alloc_subdomain_grp_stack(mgd_mesh)
!
       type(merged_mesh), intent(inout) :: mgd_mesh
!
!
       allocate( mgd_mesh%istack_bc_pe(0:mgd_mesh%num_pe) )
       allocate( mgd_mesh%istack_mat_pe(0:mgd_mesh%num_pe) )
       allocate( mgd_mesh%istack_surf_pe(0:mgd_mesh%num_pe) )
!
       mgd_mesh%istack_bc_pe = 0
       mgd_mesh%istack_mat_pe = 0
       mgd_mesh%istack_surf_pe = 0
!
       end subroutine alloc_subdomain_grp_stack
!
!------------------------------------------------------------------
!
       subroutine alloc_merged_group_num(mgd_mesh)
!
       type(merged_mesh), intent(inout) :: mgd_mesh
!
!
       call allocate_grp_type_num(mgd_mesh%merged_grp%nod_grp)
       call allocate_grp_type_num(mgd_mesh%merged_grp%ele_grp)
       call allocate_sf_grp_type_num(mgd_mesh%merged_grp%surf_grp)
!
       end subroutine alloc_merged_group_num
!
!------------------------------------------------------------------
!
       subroutine alloc_merged_group_item(mgd_mesh)
!
       type(merged_mesh), intent(inout) :: mgd_mesh
!
!
       call allocate_grp_type_item(mgd_mesh%merged_grp%nod_grp)
       call allocate_grp_type_item(mgd_mesh%merged_grp%ele_grp)
       call allocate_sf_grp_type_item(mgd_mesh%merged_grp%surf_grp)
!
       end subroutine alloc_merged_group_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_array_4_merge(mgd_mesh)
!
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
      call dealloc_local_nod_id_tbl(mgd_mesh%merge_tbl)
      call dealloc_local_ele_id_tbl(mgd_mesh%merge_tbl)
!
      end subroutine dealloc_array_4_merge
!
!------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_subdomain_groups(mgd_mesh)
!
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
       allocate( mgd_mesh%sub_nod_grp(mgd_mesh%num_pe) )
       allocate( mgd_mesh%sub_ele_grp(mgd_mesh%num_pe) )
       allocate( mgd_mesh%sub_surf_grp(mgd_mesh%num_pe) )
!
      end subroutine alloc_subdomain_groups
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_subdomain_groups(mgd_mesh)
!
      type(merged_mesh), intent(inout) :: mgd_mesh
      integer(kind = kint) :: ip
!
!
      do ip = 1, mgd_mesh%num_pe
        call deallocate_grp_type( mgd_mesh%sub_nod_grp(ip) )
        call deallocate_grp_type( mgd_mesh%sub_ele_grp(ip) )
        call deallocate_sf_grp_type( mgd_mesh%sub_surf_grp(ip) )
      end do
!
      deallocate( mgd_mesh%sub_nod_grp )
      deallocate( mgd_mesh%sub_ele_grp )
      deallocate( mgd_mesh%sub_surf_grp )
!
      end subroutine dealloc_subdomain_groups
!
!-----------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine dealloc_subdomain_grp_stack(mgd_mesh)
!
       type(merged_mesh), intent(inout) :: mgd_mesh
!
!
       deallocate( mgd_mesh%istack_bc_pe )
       deallocate( mgd_mesh%istack_mat_pe )
       deallocate( mgd_mesh%istack_surf_pe )
!
       end subroutine dealloc_subdomain_grp_stack
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine check_boundary_data_m(ip, mgd_mesh)
!
       integer(kind = kint), intent(in) :: ip
       type(merged_mesh), intent(in) :: mgd_mesh
!
!
      call check_group_type_data(izero, mgd_mesh%sub_nod_grp(ip))
!
      end subroutine check_boundary_data_m
!
!-----------------------------------------------------------------------
!
      subroutine check_material_data_m(ip, mgd_mesh)
!
       integer(kind = kint), intent(in) :: ip
       type(merged_mesh), intent(in) :: mgd_mesh
!
!
      call check_group_type_data(ip, mgd_mesh%sub_ele_grp(ip))
!
      end subroutine check_material_data_m
!
!-----------------------------------------------------------------------
!
       subroutine check_surface_data_m(ip, mgd_mesh)
!
       integer(kind = kint), intent(in) :: ip
       type(merged_mesh), intent(in) :: mgd_mesh
!
!
       call check_surf_grp_type_data(ip, mgd_mesh%sub_surf_grp(ip))
!
       end subroutine check_surface_data_m
!
!-----------------------------------------------------------------------
!
      end module t_mesh_data_4_merge
