!analyzer_refine_itp_para.f90
!
!      module  analyzer_refine_itp_para
!
      module  analyzer_refine_itp_para
!
!     Written by H. Matsui on May., 2010
!
      use m_precision
!
      use m_machine_parameter
      use m_para_refine_itp_tables
      use m_control_param_refine_para
      use set_parallel_mesh_in_1pe
!
      implicit none
!
      integer(kind = kint), parameter, private :: ifile_type = 0
!
!      subroutine init_refine_itp_para
!      subroutine analyze_refine_itp_para
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine  init_refine_itp_para
!
      use m_constants
      use m_read_mesh_data
      use m_control_data_refine_para
      use m_interpolate_table_IO
      use itp_table_IO_select_4_zlib
      use num_nod_ele_merge_by_type
      use merge_domain_local_by_type
!
      integer(kind = kint) :: ierr
!
      if(iflag_debug.gt.0) write(*,*) 'read_control_data_ref_para_itp'
      call read_control_data_ref_para_itp
      if(iflag_debug.gt.0) write(*,*) 'set_control_param_refine_para'
      call set_control_param_refine_para
!
      mesh_file_head = para_fine_mesh_head
      call alloc_para_fine_mesh_type
      call s_set_parallel_mesh_in_1pe(nprocs_fine, fine_mesh)
!
      mesh_file_head = para_course_mesh_head
      call alloc_para_course_mesh_type
      call s_set_parallel_mesh_in_1pe(nprocs_course, course_mesh)
!
!
!
      table_file_header = course_2_fine_head
      call load_interpolate_table(izero, c2f_single)
!
      table_file_header = fine_2_course_head
      call load_interpolate_table(izero, f2c_single)
!
      table_file_header = refine_info_head
      call load_interpolate_table(izero, f2c_ele_single)
!
!
!
      write(*,*) 'set_num_nod_ele_merge_type1'
      call set_num_nod_ele_merge_type1(nprocs_fine,   fine_mesh)
      call set_num_nod_ele_merge_type2(nprocs_course, course_mesh)
!
      write(*,*) 'set_domain_local_id_by_type1'
      call set_domain_local_id_by_type1(nprocs_fine,   fine_mesh)
      call set_domain_local_id_by_type2(nprocs_course, course_mesh)
!
!
!
      end subroutine  init_refine_itp_para
!
!   --------------------------------------------------------------------
!
      subroutine analyze_refine_itp_para
!
      use t_interpolate_table
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_interpolate_table_IO
      use const_parallel_itp_table
      use itp_table_IO_select_4_zlib
!
      integer(kind = kint) :: ip, my_rank
!
!
      call alloc_para_refine_itp_type
!
!
      write(*,*) 's_const_parallel_itp_table course_2_fine'
      call s_const_parallel_itp_table(nprocs_course, nprocs_fine,       &
     &    nprocs_larger, c2f_single, c2f_para,                          &
     &    merge_tbl_2%nele_overlap, merge_tbl_2%iele_local,             &
     &    merge_tbl_2%idomain_ele, merge_tbl%nnod_overlap,              &
     &    merge_tbl%inod_local, merge_tbl%idomain_nod)
!
!
      write(*,*) 's_const_parallel_itp_table fine_2_course'
      call s_const_parallel_itp_table(nprocs_fine, nprocs_course,       &
     &    nprocs_larger, f2c_single, f2c_para,                          &
     &    merge_tbl%nele_overlap, merge_tbl%iele_local,                 &
     &    merge_tbl%idomain_ele,  merge_tbl_2%nnod_overlap,             &
     &    merge_tbl_2%inod_local,  merge_tbl_2%idomain_nod)
!
!
      write(*,*) 's_const_parallel_itp_table fine_2_course_ele'
      call s_const_parallel_itp_table(nprocs_fine, nprocs_course,       &
     &    nprocs_larger, f2c_ele_single, f2c_ele_para,                  &
     &    merge_tbl%nele_overlap, merge_tbl%iele_local,                 &
     &    merge_tbl%idomain_ele, merge_tbl_2%nele_overlap,              &
     &    merge_tbl_2%iele_local, merge_tbl_2%idomain_ele)
!
!
      do ip = 1, nprocs_larger
        my_rank = ip - 1
!
        table_file_header = c2f_para_head
        call output_interpolate_table(my_rank, c2f_para(ip) )
!
        table_file_header = f2c_para_head
        call output_interpolate_table(my_rank, f2c_para(ip) )
!
        table_file_header = f2c_ele_para_head
        call output_interpolate_table(my_rank, f2c_ele_para(ip) )
      end do
!
!
      call dealloc_para_refine_itp_type
!
      call deallocate_number_of_2nd_mesh
      call deallocate_2nd_merge_table
      call deallocate_array_4_merge
!
      call dealloc_interpolate_tbl_type(f2c_single)
      call dealloc_interpolate_tbl_type(c2f_single)
!
      call dealloc_parallel_mesh_in_1pe(nprocs_fine, fine_mesh)
      call dealloc_parallel_mesh_in_1pe(nprocs_course, course_mesh)
      call dealloc_para_fine_mesh_type
      call dealloc_para_course_mesh_type
!
      end subroutine analyze_refine_itp_para
!
!   --------------------------------------------------------------------
!
      end module  analyzer_refine_itp_para
