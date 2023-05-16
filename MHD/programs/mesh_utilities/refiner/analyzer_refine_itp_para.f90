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
      use m_constants
!
      use m_machine_parameter
      use t_control_param_refine_para
      use t_mesh_data_4_merge
      use t_control_data_refine_para
      use t_para_refine_itp_tables
      use t_work_const_itp_table
!
      use set_parallel_mesh_in_1pe
!
      implicit none
!
      integer(kind = kint), parameter, private :: ifile_type = 0
!
      type(control_data_refine_para), save, private :: para_refine_c1
      type(ctl_param_para_refiner), save, private :: p_refine_p1
!
      type(merged_mesh), save, private :: mgd_mesh_rf
      type(second_mesh), save, private :: sec_mesh_rf
      type(para_refine_itp_tables), save, private :: para_ref_itp
!
      type(work_const_itp_table), save :: cst_itp_wk_r
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
      use m_interpolate_table_IO
      use num_nod_ele_merge_by_type
      use merge_domain_local_by_type
!
!
      if(iflag_debug.gt.0) write(*,*) 'read_control_data_ref_para_itp'
      call read_control_data_ref_para_itp(para_refine_c1)
      if(iflag_debug.gt.0) write(*,*) 'set_control_param_refine_para'
      call set_control_param_refine_para                                &
     &   (para_refine_c1%refine_ctl, para_refine_c1%p_refine_ctl,       &
     &    p_refine_p1, para_ref_itp)
!
      call alloc_para_fine_mesh_type(para_ref_itp)
      call s_set_parallel_mesh_in_1pe(p_refine_p1%para_fine_mesh_file,  &
     &    para_ref_itp%nprocs_fine, para_ref_itp%fine_mesh)
!
      call alloc_para_course_mesh_type(para_ref_itp)
      call s_set_parallel_mesh_in_1pe                                   &
     &   (p_refine_p1%para_course_mesh_file,                            &
     &    para_ref_itp%nprocs_course, para_ref_itp%course_mesh)
!
!
      call load_interpolate_table                                       &
     &    (0, p_refine_p1%c2f_table_IO, para_ref_itp%c2f_single)
!
      call load_interpolate_table                                       &
     &   (0, p_refine_p1%f2c_tbl_IO, para_ref_itp%f2c_single)
!
      call load_interpolate_table                                       &
     &   (0, p_refine_p1%refine_tbl_IO, para_ref_itp%f2c_ele_single)
!
!
!
      write(*,*) 'set_num_nod_ele_merge_type1'
      call set_num_nod_ele_merge_type1                                  &
     &   (para_ref_itp%nprocs_fine, para_ref_itp%fine_mesh,             &
     &    mgd_mesh_rf)
      call set_num_nod_ele_merge_type2                                  &
     &   (para_ref_itp%nprocs_course, para_ref_itp%course_mesh,         &
     &    sec_mesh_rf)
!
      write(*,*) 'set_domain_local_id_by_type1'
      call set_domain_local_id_by_type1                                 &
     &   (para_ref_itp%nprocs_fine, para_ref_itp%fine_mesh,             &
     &    mgd_mesh_rf%merge_tbl)
      call set_domain_local_id_by_type2                                 &
     &   (para_ref_itp%nprocs_course, para_ref_itp%course_mesh,         &
     &    sec_mesh_rf%merge_tbl_2)
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
      use m_interpolate_table_IO
!
      call alloc_para_refine_itp_type(para_ref_itp)
!
!
      call refine_interpolation_table                                   &
     &   (mgd_mesh_rf%merge_tbl, sec_mesh_rf%merge_tbl_2, cst_itp_wk_r)
!
      call dealloc_para_refine_itp_type(para_ref_itp)
!
      call dealloc_number_of_2nd_mesh(sec_mesh_rf)
      call dealloc_2nd_merge_table(sec_mesh_rf)
      call dealloc_array_4_merge(mgd_mesh_rf)
!
      call dealloc_interpolate_table(para_ref_itp%f2c_single)
      call dealloc_interpolate_table(para_ref_itp%c2f_single)
!
      call dealloc_parallel_mesh_in_1pe                                 &
     &   (para_ref_itp%nprocs_fine, para_ref_itp%fine_mesh)
      call dealloc_parallel_mesh_in_1pe                                 &
     &   (para_ref_itp%nprocs_course, para_ref_itp%course_mesh)
      call dealloc_para_fine_mesh_type(para_ref_itp)
      call dealloc_para_course_mesh_type(para_ref_itp)
!
      end subroutine analyze_refine_itp_para
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_parallel_mesh_in_1pe(num_pe, para_mesh)
!
      use t_mesh_data
      use t_mesh_data_with_pointer
!
      integer, intent(in) :: num_pe
      type(mesh_data), intent(inout) :: para_mesh(num_pe)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, num_pe
        call dealloc_overlapped_ele(para_mesh(ip)%mesh%ele)
        call dealloc_ele_geometry(para_mesh(ip)%mesh%ele)
        call dealloc_ele_param_smp(para_mesh(ip)%mesh%ele)
        call dealloc_node_param_smp(para_mesh(ip)%mesh%node)
!
        call dealloc_groups_data(para_mesh(ip)%group)
!
        call dealloc_ele_connect(para_mesh(ip)%mesh%ele)
        call dealloc_node_geometry_w_sph(para_mesh(ip)%mesh%node)
        call dealloc_comm_table(para_mesh(ip)%mesh%nod_comm)
      end do
!
      end subroutine dealloc_parallel_mesh_in_1pe
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine refine_interpolation_table                             &
     &         (merge_tbl, merge_tbl_2, cst_itp_wk)
!
      use t_interpolate_table
      use t_merged_geometry_data
      use m_interpolate_table_IO
      use const_parallel_itp_table
!
      type(merged_stacks), intent(inout) :: merge_tbl
      type(merged_stacks), intent(inout) :: merge_tbl_2
      type(work_const_itp_table), intent(inout) :: cst_itp_wk
!
      integer :: ip, id_rank
!
!
      write(*,*) 's_const_parallel_itp_table course_2_fine'
      call s_const_parallel_itp_table(para_ref_itp%nprocs_course,       &
     &    para_ref_itp%nprocs_fine, para_ref_itp%nprocs_larger,         &
     &    para_ref_itp%c2f_single, para_ref_itp%c2f_para, cst_itp_wk,   &
     &    merge_tbl_2%nele_overlap, merge_tbl_2%iele_local,             &
     &    merge_tbl_2%idomain_ele, merge_tbl%nnod_overlap,              &
     &    merge_tbl%inod_local, merge_tbl%idomain_nod)
!
!
      write(*,*) 's_const_parallel_itp_table fine_2_course'
      call s_const_parallel_itp_table(para_ref_itp%nprocs_fine,         &
     &    para_ref_itp%nprocs_course, para_ref_itp%nprocs_larger,       &
     &    para_ref_itp%f2c_single, para_ref_itp%f2c_para, cst_itp_wk,   &
     &    merge_tbl%nele_overlap, merge_tbl%iele_local,                 &
     &    merge_tbl%idomain_ele,  merge_tbl_2%nnod_overlap,             &
     &    merge_tbl_2%inod_local,  merge_tbl_2%idomain_nod)
!
!
      write(*,*) 's_const_parallel_itp_table fine_2_course_ele'
      call s_const_parallel_itp_table(para_ref_itp%nprocs_fine,         &
     &    para_ref_itp%nprocs_course, para_ref_itp%nprocs_larger,       &
     &    para_ref_itp%f2c_ele_single, para_ref_itp%f2c_ele_para,       &
     &    cst_itp_wk, merge_tbl%nele_overlap, merge_tbl%iele_local,     &
     &    merge_tbl%idomain_ele, merge_tbl_2%nele_overlap,              &
     &    merge_tbl_2%iele_local, merge_tbl_2%idomain_ele)
!
!
      do ip = 1, para_ref_itp%nprocs_larger
        id_rank = ip - 1
!
        call output_interpolate_table                                   &
     &     (id_rank, p_refine_p1%c2f_para_tbl_IO,                       &
     &      para_ref_itp%c2f_para(ip))
!
        call output_interpolate_table                                   &
     &     (id_rank, p_refine_p1%f2c_para_tbl_IO,                       &
     &      para_ref_itp%f2c_para(ip))
!
        call output_interpolate_table                                   &
     &     (id_rank, p_refine_p1%f2c_ele_para_tbl_IO,                   &
     &      para_ref_itp%f2c_ele_para(ip))
      end do
!
      end subroutine refine_interpolation_table
!
! -----------------------------------------------------------------------
!
      end module  analyzer_refine_itp_para
