!
!      module write_merged_mesh
!
!      Written by H. Matsui on Jan., 2007
!
!      subroutine s_write_merged_mesh(merged)
!
      module write_merged_mesh
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_merged_geometry_data
!
      implicit none
!
      private :: copy_merge_mesh_2_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_write_merged_mesh(merge_tbl, merged, merged_grp)
!
      use m_control_param_merge
      use mesh_IO_select
      use set_parallel_file_name
!
      type(merged_stacks), intent(in) :: merge_tbl
      type(mesh_geometry), intent(inout) :: merged
      type(mesh_groups), intent(inout) :: merged_grp
      type(mesh_data) :: fem_IO_m
!
!
      call copy_merge_mesh_2_IO                                         &
     &   (merge_tbl, merged, merged_grp, fem_IO_m)
!
      call copy_mesh_format_and_prefix                                  &
     &   (new_mesh_head, inew_mesh_file_fmt)
      call sel_write_mesh_file(izero, fem_IO_m)
!
      end subroutine s_write_merged_mesh
!
! ----------------------------------------------------------------------
!
      subroutine copy_merge_mesh_2_IO                                   &
     &         (merge_tbl, merged, merged_grp, fem_IO_m)
!
      use set_nnod_4_ele_by_type
      use copy_mesh_structures
      use set_element_data_4_IO
      use load_mesh_data
!
      type(merged_stacks), intent(in) :: merge_tbl
      type(mesh_geometry), intent(inout) :: merged
      type(mesh_groups), intent(inout) :: merged_grp
      type(mesh_data), intent(inout) :: fem_IO_m
!
      integer (kind = kint) :: i
!
!
      fem_IO_m%mesh%nod_comm%num_neib = izero
      call allocate_type_comm_tbl_num(fem_IO_m%mesh%nod_comm)
      call allocate_type_comm_tbl_item(fem_IO_m%mesh%nod_comm)
      call copy_node_geometry_types(merged%node, fem_IO_m%mesh%node)
      call copy_ele_connect_to_IO(merged%ele, fem_IO_m%mesh%ele)
!
      fem_IO_m%mesh%node%numnod =        merge_tbl%nnod_merged
      fem_IO_m%mesh%node%internal_node = merge_tbl%nnod_merged
!
      fem_IO_m%mesh%ele%numele = merge_tbl%nele_merged
      do i = 1, merge_tbl%nele_merged
        call s_set_nnod_4_ele_by_type                                   &
     &     (fem_IO_m%mesh%ele%elmtyp(i), fem_IO_m%mesh%ele%nodelm(i))
      end do
!
      call set_grp_data_to_IO                                           &
     &   (merged_grp%nod_grp, merged_grp%ele_grp, merged_grp%surf_grp,  &
     &    fem_IO_m%group)
!
      call dealloc_groups_data(merged_grp)
      call deallocate_ele_connect_type(merged%ele)
      call deallocate_node_geometry_type(merged%node)
!
      end subroutine copy_merge_mesh_2_IO
!
! ----------------------------------------------------------------------
!
      end module write_merged_mesh
