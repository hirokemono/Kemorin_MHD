!analyzer_refine_itp_para.f90
!
!      module  analyzer_refine_itp_para
!
!     Written by H. Matsui on May., 2010
!
!      subroutine init_refine_itp_para
!      subroutine analyze_refine_itp_para
!
      module  analyzer_refine_itp_para
!
      use m_precision
!
      use m_machine_parameter
      use m_para_refine_itp_tables
      use m_control_param_refine_para
      use t_mesh_data_4_merge
      use set_parallel_mesh_in_1pe
!
      implicit none
!
      integer(kind = kint), parameter, private :: ifile_type = 0
!
      type(merged_mesh), save, private :: mgd_mesh_rf
!
      private :: refine_interpolation_table
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
      use m_control_data_refine_para
      use m_interpolate_table_IO
      use m_2nd_geometry_4_merge
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
      call alloc_para_fine_mesh_type
      call s_set_parallel_mesh_in_1pe                                   &
     &   (para_fine_mesh_file, nprocs_fine, fine_mesh)
!
      call alloc_para_course_mesh_type
      call s_set_parallel_mesh_in_1pe                                   &
     &   (para_course_mesh_file, nprocs_course, course_mesh)
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
      call set_num_nod_ele_merge_type1                                  &
     &    (nprocs_fine, fine_mesh, mgd_mesh_rf)
      call set_num_nod_ele_merge_type2(nprocs_course, course_mesh)
!
      write(*,*) 'set_domain_local_id_by_type1'
      call set_domain_local_id_by_type1                                 &
     &   (nprocs_fine, fine_mesh, mgd_mesh_rf%merge_tbl)
      call set_domain_local_id_by_type2                                 &
     &   (nprocs_course, course_mesh, sec_mesh1%merge_tbl_2)
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
      use m_2nd_geometry_4_merge
      use m_interpolate_table_IO
!
      integer(kind = kint) :: ip, my_rank
!
!
      call alloc_para_refine_itp_type
!
!
      call refine_interpolation_table                                   &
     &   (mgd_mesh_rf%merge_tbl, sec_mesh1%merge_tbl_2)
!
      call dealloc_para_refine_itp_type
!
      call deallocate_number_of_2nd_mesh
      call deallocate_2nd_merge_table
      call dealloc_array_4_merge(mgd_mesh_rf)
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
      subroutine dealloc_parallel_mesh_in_1pe(nprocs, para_mesh)
!
      use t_mesh_data
      use t_mesh_data_with_pointer
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data), intent(inout) :: para_mesh(nprocs)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nprocs
        call deallocate_ele_geometry_type(para_mesh(ip)%mesh%ele)
        call deallocate_ele_param_smp_type(para_mesh(ip)%mesh%ele)
        call deallocate_node_param_smp_type(para_mesh(ip)%mesh%node)
!
        call deallocate_grp_type(para_mesh(ip)%group%nod_grp)
        call deallocate_grp_type(para_mesh(ip)%group%ele_grp)
        call deallocate_sf_grp_type(para_mesh(ip)%group%surf_grp)
!
        call deallocate_ele_connect_type(para_mesh(ip)%mesh%ele)
        call deallocate_node_geometry_type(para_mesh(ip)%mesh%node)
        call deallocate_type_comm_tbl(para_mesh(ip)%mesh%nod_comm)
      end do
!
      end subroutine dealloc_parallel_mesh_in_1pe
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine refine_interpolation_table(merge_tbl, merge_tbl_2)
!
      use t_interpolate_table
      use t_merged_geometry_data
      use m_interpolate_table_IO
      use const_parallel_itp_table
      use itp_table_IO_select_4_zlib
!
      type(merged_stacks), intent(inout) :: merge_tbl
      type(merged_stacks), intent(inout) :: merge_tbl_2
!
      integer(kind = kint) :: ip, my_rank
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
      end subroutine refine_interpolation_table
!
! -----------------------------------------------------------------------
!
      end module  analyzer_refine_itp_para
