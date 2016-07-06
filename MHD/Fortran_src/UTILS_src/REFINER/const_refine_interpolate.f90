!const_refine_interpolate.f90
!     Written by H. Matsui on Oct., 2007
!
!      subroutine s_const_refine_interpolate_tbl(node, ele, surf, edge, &
!     &                                          newmesh)
!        type(node_data), intent(in) :: node
!        type(element_data), intent(in) :: ele
!        type(surface_data), intent(in) :: surf
!        type(edge_data), intent(in) :: edge
!        type(mesh_geometry), intent(in) :: newmesh
!
      module  const_refine_interpolate
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_control_param_4_refiner
      use m_file_format_switch
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      use itp_table_IO_select_4_zlib
      use set_parallel_file_name
!
      implicit none
!
      integer(kind = kint), private :: iflag_merge = 0
!
      private :: const_single_refine_itp_tbl
      private :: const_second_refine_itp_tbl
      private :: const_merged_refine_itp_tbl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_const_refine_interpolate_tbl(node, ele, surf, edge,  &
     &                                          newmesh)
!
      use t_mesh_data
      use t_geometry_data
!
      use m_refined_element_data
      use m_work_merge_refine_itp
      use refinment_info_IO
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(mesh_geometry), intent(in) :: newmesh
!
!
      if(iflag_tmp_tri_refine .eq. 0 .and. iflag_merge .eq. 0) then
        write(*,*) 'const_single_refine_itp_tbl'
        call const_single_refine_itp_tbl(ele, surf, edge,               &
     &                                   newmesh%node%numnod)
        call write_refinement_table(ele%numele, ione)
      else if(iflag_tmp_tri_refine .gt. 0 .or. iflag_merge .eq. 0) then
        write(*,*) 'copy_original_mesh_conn_refine'
        call copy_original_mesh_conn_refine(node, ele)
!
        iflag_merge = 1
!
      else if(iflag_merge .gt. 0) then
        write(*,*) 'const_second_refine_itp_tbl'
        call const_second_refine_itp_tbl(ele, surf, edge,               &
     &                                   newmesh%node%numnod)
        write(*,*) 'const_merged_refine_itp_tbl'
        call const_merged_refine_itp_tbl(ele%nnod_4_ele,                &
     &      newmesh%node%numnod, newmesh%ele%nnod_4_ele,                &
     &      newmesh%node%xx)
!
        write(*,*) 'write_merged_refinement_tbl'
        call write_merged_refinement_tbl(ele%numele)
!
      end if
!
      end subroutine s_const_refine_interpolate_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_single_refine_itp_tbl(ele, surf, edge, nnod_2)
!
      use m_interpolate_table
      use m_interpolate_coefs_dest
      use m_interpolate_table_dest_IO
      use m_work_merge_refine_itp
      use set_refine_interpolate_tbl
      use copy_interpolate_type_IO
      use copy_interpolate_type_raw
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      integer(kind = kint), intent(in) :: nnod_2
!
!
      iflag_debug = 1
      if(iflag_debug .gt. 0) write(*,*) 'set_itp_course_to_fine_origin'
      call set_itp_course_to_fine_origin(ele, surf, edge, itp1_org)
      if(iflag_debug .gt. 0) write(*,*) 'set_itp_course_to_fine_dest'
      call set_itp_course_to_fine_dest(nnod_2, itp1_dest)
!
      call allocate_itp_coef_dest(itp1_dest)
      call allocate_itp_coef_stack(ione)
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_table_org_to_IO'
      call copy_itp_table_org_to_IO(itp1_org)
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_coefs_dest_to_IO'
      call copy_itp_coefs_dest_to_IO(itp1_dest)
!
      table_file_header = course_2_fine_head
      ifmt_itp_table_file = id_ascii_file_fmt
      write(*,*) 'table field header: ', trim(table_file_header)
      call sel_write_interpolate_table(izero)
      call deallocate_itp_coefs_dst_IO
!
      if(iflag_debug .gt. 0) write(*,*) 'set_itp_fine_to_course_origin'
      call set_itp_fine_to_course_origin(ele%nnod_4_ele, itp1_org)
      call set_itp_fine_to_course_dest(itp1_dest)
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_itp_coef_dest'
      call allocate_itp_coef_dest(itp1_dest)
      call allocate_itp_coef_stack(ione)
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_table_org_to_IO'
      call copy_itp_table_org_to_IO(itp1_org)
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_coefs_dest_to_IO'
      call copy_itp_coefs_dest_to_IO(itp1_dest)
!
      table_file_header = fine_2_course_head
      write(*,*) 'table field header: ', trim(table_file_header)
!
      call sel_write_interpolate_table(izero)
      call deallocate_itp_coefs_dst_IO
!
      end subroutine const_single_refine_itp_tbl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_second_refine_itp_tbl(ele, surf, edge, nnod_2)
!
      use m_interpolate_table
      use m_work_merge_refine_itp
      use set_refine_interpolate_tbl
      use copy_interpolate_type_raw
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      integer(kind = kint), intent(in) :: nnod_2
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_itp_course_to_fine_origin'
      call set_itp_course_to_fine_origin(ele, surf, edge, itp1_org)
      call set_itp_course_to_fine_dest(nnod_2, itp1_dest)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                       'copy_interpolate_types_from_raw c2f_2nd'
      call copy_interpolate_types_from_raw(izero, c2f_2nd)
!
      end subroutine const_second_refine_itp_tbl
!
! ----------------------------------------------------------------------
!
      subroutine const_merged_refine_itp_tbl                            &
     &          (nnod_4_ele, nnod_2, nnod_4_ele_2, xx_2)
!
      use t_interpolate_tbl_dest
      use m_work_merge_refine_itp
!      use copy_interpolate_type_IO
!
      use m_interpolate_table
      use m_interpolate_coefs_dest
      use m_interpolate_table_dest_IO
      use set_refine_interpolate_tbl
      use set_merged_refine_itp
      use copy_interpolate_type_IO
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: nnod_2, nnod_4_ele_2
      real(kind = kreal), intent(in) :: xx_2(nnod_2,3)
!
      type(interpolate_table_org)  :: itp_org_r
      type(interpolate_table_dest) :: itp_dest_r
!
!
      if(iflag_merge .eq. 0) return
!
      if(iflag_debug .gt. 0) write(*,*) 'set_merged_itp_course_to_fine'
      call set_merged_itp_course_to_fine                                &
     &   (nnod_4_ele, nnod_2, nnod_4_ele_2, xx_2, itp_org_r, itp_dest_r)
!
      call allocate_itp_coef_dest(itp_dest_r)
      call copy_itp_table_org_to_IO(itp_org_r)
      call copy_itp_coefs_dest_to_IO(itp_dest_r)
!
!
      table_file_header = course_2_fine_head
      ifmt_itp_table_file = id_ascii_file_fmt
!
      write(*,*) 'table field header: ', trim(table_file_header)
      call sel_write_interpolate_table(izero)
      call deallocate_itp_coefs_dst_IO
!
!
      write(*,*) 'set_merged_itp_fine_to_course'
      call set_merged_itp_fine_to_course(nnod_4_ele, itp_org_r)
      call set_itp_fine_to_course_dest(itp_dest_r)
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_itp_coef_dest'
      call allocate_itp_coef_dest(itp_dest_r)
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_table_org_to_IO'
      call copy_itp_table_org_to_IO(itp_org_r)
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_coefs_dest_to_IO'
      call copy_itp_coefs_dest_to_IO(itp_dest_r)
!
      table_file_header = fine_2_course_head
!
      write(*,*) 'table field header: ', trim(table_file_header)
      call sel_write_interpolate_table(izero)
      call deallocate_itp_coefs_dst_IO
!
      end subroutine const_merged_refine_itp_tbl
!
! ----------------------------------------------------------------------
!
      end module const_refine_interpolate
