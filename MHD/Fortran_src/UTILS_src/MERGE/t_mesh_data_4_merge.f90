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
!!
!!      subroutine dealloc_array_4_merge(mgd_mesh)
!!      subroutine dealloc_number_of_mesh(mgd_mesh)
!!
!!      subroutine alloc_number_of_2nd_mesh(sec_mesh)
!!      subroutine dealloc_number_of_2nd_mesh(sec_mesh)
!!        type(second_mesh), intent(inout) :: sec_mesh
!!      subroutine alloc_2nd_merged_geometry(num_pe2, subdomains_2)
!!        type(mesh_geometry), intent(inout) :: subdomains_2(num_pe2)
!!      subroutine alloc_2nd_merge_table(merge_tbl_2)
!!        type(merged_stacks), intent(inout) :: merge_tbl_2
!!      subroutine dealloc_2nd_merge_table(sec_mesh)
!!        type(second_mesh), intent(inout) :: sec_mesh
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
      use t_surface_data
      use t_phys_data
!
      implicit    none
!
!  ==============================
! . for mesh data & result data
!  ==============================
!
      type merged_mesh
!>        number of subdomains
        integer(kind = kint)  :: num_pe
!>        subdomain mesh data
        type(mesh_geometry), allocatable :: subdomain(:)
!
!>        merged mesh data
        type(mesh_geometry) :: merged
!>        merged field data
        type(phys_data) :: merged_fld
!
!>        merged index table
        type(merged_stacks) :: merge_tbl
!
        type(mesh_groups) :: merged_grp
!
        type(group_data), allocatable :: sub_nod_grp(:)
        type(group_data), allocatable :: sub_ele_grp(:)
        type(surface_group_data), allocatable :: sub_surf_grp(:)
!
!
        type(surface_data) :: merged_surf
!
        integer(kind=kint ), allocatable :: istack_surfpe(:)
      end type merged_mesh
!
      type second_mesh
!>        number of subdomains
        integer(kind = kint) :: num_pe2
!>        subdomain mesh data
        type(mesh_geometry), allocatable :: subdomains_2(:)
!>        merged index table
        type(merged_stacks) :: merge_tbl_2
      end type second_mesh
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
      call alloc_node_geometry_w_sph(merged%node)
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
!-----------------------------------------------------------------------
!
      subroutine alloc_number_of_2nd_mesh(sec_mesh)
!
      type(second_mesh), intent(inout) :: sec_mesh
!
!
      allocate( sec_mesh%subdomains_2(sec_mesh%num_pe2) )
!
      call alloc_subdomain_stack                                        &
     &   (sec_mesh%num_pe2, sec_mesh%merge_tbl_2)
!
      end subroutine alloc_number_of_2nd_mesh
!
!------------------------------------------------------------------
!
      subroutine dealloc_number_of_2nd_mesh(sec_mesh)
!
      type(second_mesh), intent(inout) :: sec_mesh
!
      call dealloc_subdomain_stack(sec_mesh%merge_tbl_2)
!
      end subroutine dealloc_number_of_2nd_mesh
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_2nd_merged_geometry(num_pe2, subdomains_2)
!
      integer(kind = kint), intent(in) :: num_pe2
      type(mesh_geometry), intent(inout) :: subdomains_2(num_pe2)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, num_pe2
        call alloc_node_geometry_w_sph(subdomains_2(ip)%node)
        call allocate_ele_connect_type(subdomains_2(ip)%ele)
      end do
!
      end subroutine alloc_2nd_merged_geometry
!
!------------------------------------------------------------------
!
      subroutine alloc_2nd_merge_table(merge_tbl_2)
!
      type(merged_stacks), intent(inout) :: merge_tbl_2
!
!
      call alloc_local_nod_id_tbl(merge_tbl_2)
      call alloc_local_ele_id_tbl(merge_tbl_2)
!
      end subroutine alloc_2nd_merge_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_2nd_merge_table(sec_mesh)
!
      type(second_mesh), intent(inout) :: sec_mesh
!
      call dealloc_local_nod_id_tbl(sec_mesh%merge_tbl_2)
      call dealloc_local_ele_id_tbl(sec_mesh%merge_tbl_2)
!
      end subroutine dealloc_2nd_merge_table
!
!------------------------------------------------------------------
!
      end module t_mesh_data_4_merge
