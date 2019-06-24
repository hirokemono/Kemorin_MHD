!t_control_data_refine_para.f90
!      module t_control_data_refine_para
!
!      Written by Kemorin on Oct., 2007
!
!!       subroutine read_control_data_ref_para_itp(para__refine_c)
!!        type(control_data_refine_para), intent(inout) :: para__refine_c
!
      module t_control_data_refine_para
!
      use m_precision
!
      use t_control_elements
      use t_ctl_data_4_platforms
      use t_control_data_4_refine
      use t_read_control_elements
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint), parameter :: ctl_refine_code = 13
      character (len = kchara), parameter                               &
     &         :: ctl_refine_file_name = 'ctl_para_refine_table'
!
      type file_ctls_refine_para
        type(platform_data_control) :: para_refine_plt
!
        type(read_integer_item) :: nprocs_course_ctl
!
        type(read_character_item) :: course_mesh_file_head_ctl
        type(read_character_item) :: c2f_para_head_ctl
        type(read_character_item) :: f2c_para_head_ctl
        type(read_character_item) :: refine_info_para_head_ctl
!
        integer (kind=kint) :: i_course_mesh_para_ctl = 0
      end type file_ctls_refine_para
!
      type control_data_refine_para
        type(control_data_4_refine) :: refine_ctl
        type(file_ctls_refine_para) :: p_refine_ctl
!
        integer (kind=kint) :: i_para_refine_tbl_ctl = 0
      end type control_data_refine_para
!
!
!   Top level
!
      character(len=kchara), parameter :: hd_para_refine_tbl_ctl        &
     &                      = 'para_refine_tbl_control'
!
!   2nd level for para_refine_tbl_control
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter :: hd_course_mesh_para_ctl       &
     &                      = 'parallel_course_mesh_ctl'
      character(len=kchara), parameter :: hd_single_refine_files        &
     &                      = 'single_refined_table_ctl'
!
!   3rd level for parallel_course_mesh_ctl
!
      character(len=kchara), parameter :: hd_num_course_subdomain       &
     &                      = 'num_course_subdomain_ctl'
      character(len=kchara), parameter :: hd_course_mesh_file_head      &
     &                      = 'course_mesh_file_head_ctl'
      character(len=kchara), parameter :: hd_course_to_fine_p_head      &
     &                      = 'course_to_fine_head_ctl'
      character(len=kchara), parameter :: hd_fine_to_course_p_head      &
     &                      = 'fine_to_course_head_ctl'
      character(len=kchara), parameter :: hd_fine_to_course_ele_head    &
     &                      = 'fine_to_course_ele_head_ctl'
!
!
      private :: hd_para_refine_tbl_ctl
      private :: hd_course_mesh_para_ctl, hd_single_refine_files
      private :: hd_num_course_subdomain, hd_course_mesh_file_head
      private :: hd_fine_to_course_p_head,  hd_course_to_fine_p_head
      private :: hd_fine_to_course_ele_head, hd_platform
      private :: read_ref_para_itp_ctl_data
      private :: read_ctl_data_4_course_mesh
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine read_control_data_ref_para_itp(para__refine_c)
!
      type(control_data_refine_para), intent(inout) :: para__refine_c
!
      type(buffer_for_control) :: c_buf1
!
!
      open (ctl_refine_code, file = ctl_refine_file_name)
!
      do
        call load_one_line_from_control(ctl_refine_code, c_buf1)
        call read_ref_para_itp_ctl_data                                 &
     &     (ctl_refine_code, hd_para_refine_tbl_ctl,                    &
     &      para__refine_c, c_buf1)
        if(para__refine_c%i_para_refine_tbl_ctl .gt. 0) exit
      end do
      close(ctl_refine_code)
!
      end subroutine read_control_data_ref_para_itp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ref_para_itp_ctl_data                             &
     &         (id_control, hd_block, para__refine_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_refine_para), intent(inout) :: para__refine_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(para__refine_c%i_para_refine_tbl_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms(id_control, hd_platform,            &
     &      para__refine_c%p_refine_ctl%para_refine_plt, c_buf)
!
        call read_ctl_data_4_course_mesh                                &
     &     (id_control, hd_course_mesh_para_ctl,                        &
     &      para__refine_c%p_refine_ctl, c_buf)
        call read_ctl_data_4_refine_mesh                                &
     &     (id_control, hd_single_refine_files,                         &
     &      para__refine_c%refine_ctl, c_buf)
      end do
      para__refine_c%i_para_refine_tbl_ctl = 1
!
      end subroutine read_ref_para_itp_ctl_data
!
! -----------------------------------------------------------------------
!
       subroutine read_ctl_data_4_course_mesh                           &
      &         (id_control, hd_block, p_refine_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(file_ctls_refine_para), intent(inout) :: p_refine_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(p_refine_ctl%i_course_mesh_para_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_integer_ctl_type(c_buf, hd_num_course_subdomain,      &
     &      p_refine_ctl%nprocs_course_ctl)
!
        call read_chara_ctl_type(c_buf, hd_course_mesh_file_head,       &
     &      p_refine_ctl%course_mesh_file_head_ctl)
        call read_chara_ctl_type(c_buf, hd_course_to_fine_p_head,       &
     &      p_refine_ctl%c2f_para_head_ctl)
        call read_chara_ctl_type(c_buf, hd_fine_to_course_p_head,       &
     &      p_refine_ctl%f2c_para_head_ctl)
!
        call read_chara_ctl_type(c_buf, hd_fine_to_course_ele_head,     &
     &      p_refine_ctl%refine_info_para_head_ctl)
      end do
      p_refine_ctl%i_course_mesh_para_ctl = 1
!
      end subroutine read_ctl_data_4_course_mesh
!
! -----------------------------------------------------------------------
!
      end module t_control_data_refine_para
