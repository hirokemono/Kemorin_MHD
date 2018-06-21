! \beginFILE
!     program  assemble_mesh

! * program 'assemble' assemble UCD of parts files to one UCD file
!    \begin{flushright}
!         programmed by m.iizuka (rist)  on sep. 1998 (ver 1.0)
!         modified  by  H.Matsui (rist)  on sep. 2000 (ver 1.1)
!         modified  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.2)
!    \end{flushright}
!
      program    assemble_mesh 
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_merged_geometry_data
!
      use m_geometry_data_4_merge
      use m_control_data_4_merge
      use m_control_param_merge
      use m_default_file_prefix
      use set_merged_geometry
      use set_control_platform_data
!
      implicit    none
!
!
! ==============================================                                                                                                            
! * get number of  nodes,elements for whole PES
! ==============================================
!
      call read_control_4_merge
      call set_control_4_merge(mgd_mesh1%num_pe)
      call set_control_mesh_file_def                                    &
     &   (def_new_mesh_head, assemble_plt, merged_mesh_file)
!
!  read mesh information
!
      call set_merged_mesh_and_group(merge_org_mesh_file, mgd_mesh1)
!
!   output grid data
!
      call s_write_merged_mesh                                          &
     &   (mgd_mesh1%merge_tbl, mgd_mesh1%merged, mgd_mesh1%merged_grp)
!
      call dealloc_array_4_merge(mgd_mesh1)
!
      stop ' //// program normally finished //// '
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_write_merged_mesh(merge_tbl, merged, merged_grp)
!
      use mesh_IO_select
!
      type(merged_stacks), intent(in) :: merge_tbl
      type(mesh_geometry), intent(inout) :: merged
      type(mesh_groups), intent(inout) :: merged_grp
      type(mesh_data) :: fem_IO_m
!
!
      call copy_merge_mesh_2_IO                                         &
     &   (merge_tbl, merged, merged_grp, fem_IO_m)
      call sel_write_mesh_file(merged_mesh_file, izero, fem_IO_m)
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
      call dealloc_node_geometry_w_sph(merged%node)
!
      end subroutine copy_merge_mesh_2_IO
!
! ----------------------------------------------------------------------
!
      end program assemble_mesh
