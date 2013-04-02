!const_refine_interpolate.f90
!     Written by H. Matsui on Oct., 2007
!
!      subroutine s_const_refine_interpolate_tbl
!
      module  const_refine_interpolate
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_control_param_4_refiner
      use itp_table_IO_select_4_zlib
      use set_parallel_file_name
!
      implicit none
!
      integer(kind = kint), parameter, private :: ifile_type = 0
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
      subroutine s_const_refine_interpolate_tbl
!
      use m_refined_element_data
      use m_work_merge_refine_itp
      use refinment_info_IO
!
!
      if(iflag_tmp_tri_refine .eq. 0 .and. iflag_merge .eq. 0) then
        write(*,*) 'const_single_refine_itp_tbl'
        call const_single_refine_itp_tbl
        call write_refinement_table(ione)
      else if(iflag_tmp_tri_refine .gt. 0 .or. iflag_merge .eq. 0) then
        write(*,*) 'copy_original_mesh_conn_refine'
        call copy_original_mesh_conn_refine
!
        iflag_merge = 1
!
      else if(iflag_merge .gt. 0) then
        write(*,*) 'const_second_refine_itp_tbl'
        call const_second_refine_itp_tbl
        write(*,*) 'const_merged_refine_itp_tbl'
        call const_merged_refine_itp_tbl
!
        write(*,*) 'write_merged_refinement_tbl'
        call write_merged_refinement_tbl
!
      end if
!
      end subroutine s_const_refine_interpolate_tbl
!
!   --------------------------------------------------------------------
!
      subroutine const_single_refine_itp_tbl
!
      use m_interpolate_table_dest
      use m_interpolate_table_dest_IO
      use m_work_merge_refine_itp
      use set_refine_interpolate_tbl
      use copy_interpolate_dest_IO
      use copy_interpolate_org_IO
      use copy_interpolate_type_raw
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_itp_course_to_fine_origin'
      call set_itp_course_to_fine_origin
      if(iflag_debug .gt. 0) write(*,*) 'set_itp_course_to_fine_dest'
      call set_itp_course_to_fine_dest
!
      call allocate_itp_coef_dest
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_table_org_to_IO'
      call copy_itp_table_org_to_IO
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_table_dest_to_IO'
      call copy_itp_table_dest_to_IO
!
      table_file_header = course_2_fine_head
      write(*,*) 'table field header: ', trim(table_file_header)
      call sel_write_interpolate_table(izero, ifile_type)
      call deallocate_itp_coefs_dst_IO
!
      if(iflag_debug .gt. 0) write(*,*) 'set_itp_fine_to_course_origin'
      call set_itp_fine_to_course_origin
      call set_itp_fine_to_course_dest
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_itp_coef_dest'
      call allocate_itp_coef_dest
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_table_org_to_IO'
      call copy_itp_table_org_to_IO
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_table_dest_to_IO'
      call copy_itp_table_dest_to_IO
!
      table_file_header = fine_2_course_head
      write(*,*) 'table field header: ', trim(table_file_header)
!
      call sel_write_interpolate_table(izero, ifile_type)
      call deallocate_itp_coefs_dst_IO
!
      end subroutine const_single_refine_itp_tbl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_second_refine_itp_tbl
!
      use m_work_merge_refine_itp
      use set_refine_interpolate_tbl
      use copy_interpolate_type_raw
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_itp_course_to_fine_origin'
      call set_itp_course_to_fine_origin
      call set_itp_course_to_fine_dest
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                       'copy_interpolate_types_from_raw c2f_2nd'
      call copy_interpolate_types_from_raw(izero, c2f_2nd)
!
      end subroutine const_second_refine_itp_tbl
!
! ----------------------------------------------------------------------
!
      subroutine const_merged_refine_itp_tbl
!
      use m_work_merge_refine_itp
!      use copy_interpolate_type_IO
!
      use m_interpolate_table_dest
      use m_interpolate_table_dest_IO
      use set_refine_interpolate_tbl
      use set_merged_refine_itp
      use copy_interpolate_dest_IO
      use copy_interpolate_org_IO
!
!
      if(iflag_merge .eq. 0) return
!
      if(iflag_debug .gt. 0) write(*,*) 'set_merged_itp_course_to_fine'
      call set_merged_itp_course_to_fine
!
      call allocate_itp_coef_dest
      call copy_itp_table_org_to_IO
      call copy_itp_table_dest_to_IO
!
!
      table_file_header = course_2_fine_head
!
      write(*,*) 'table field header: ', trim(table_file_header)
      call sel_write_interpolate_table(izero, ifile_type)
      call deallocate_itp_coefs_dst_IO
!
!
      write(*,*) 'set_merged_itp_fine_to_course'
      call set_merged_itp_fine_to_course
      call set_itp_fine_to_course_dest
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_itp_coef_dest'
      call allocate_itp_coef_dest
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_table_org_to_IO'
      call copy_itp_table_org_to_IO
      if(iflag_debug .gt. 0) write(*,*) 'copy_itp_table_dest_to_IO'
      call copy_itp_table_dest_to_IO
!
      table_file_header = fine_2_course_head
!
      write(*,*) 'table field header: ', trim(table_file_header)
      call sel_write_interpolate_table(izero, ifile_type)
      call deallocate_itp_coefs_dst_IO
!
      end subroutine const_merged_refine_itp_tbl
!
! ----------------------------------------------------------------------
!
      end module const_refine_interpolate
