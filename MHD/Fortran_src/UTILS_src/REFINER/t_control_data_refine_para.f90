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
      use m_read_control_elements
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
      end type file_ctls_refine_para
!
      type control_data_refine_para
        type(control_data_4_refine) :: refine_ctl
        type(file_ctls_refine_para) :: p_refine_ctl
      end type control_data_refine_para
!
!
!   Top level
!
      character(len=kchara), parameter :: hd_para_refine_tbl_ctl        &
     &                      = 'para_refine_tbl_control'
      integer (kind=kint) :: i_para_refine_tbl_ctl = 0
!
!   2nd level for para_refine_tbl_control
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter :: hd_course_mesh_para_ctl       &
     &                      = 'parallel_course_mesh_ctl'
      integer (kind=kint) :: i_platform =   0
      integer (kind=kint) :: i_course_mesh_para_ctl = 0
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
      private :: hd_para_refine_tbl_ctl,  i_para_refine_tbl_ctl
      private :: hd_course_mesh_para_ctl, i_course_mesh_para_ctl
      private :: hd_num_course_subdomain, hd_course_mesh_file_head
      private :: hd_fine_to_course_p_head,  hd_course_to_fine_p_head
      private :: hd_fine_to_course_ele_head
      private :: hd_platform, i_platform
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
!
      ctl_file_code = ctl_refine_code
      open (ctl_file_code, file = ctl_refine_file_name)
!
      call load_ctl_label_and_line
      call read_ref_para_itp_ctl_data                                   &
     &   (para__refine_c%refine_ctl, para__refine_c%p_refine_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_data_ref_para_itp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ref_para_itp_ctl_data(refine_ctl, p_refine_ctl)
!
      type(control_data_4_refine), intent(inout) :: refine_ctl
      type(file_ctls_refine_para), intent(inout) :: p_refine_ctl
!
!
      if(right_begin_flag(hd_para_refine_tbl_ctl) .eq. 0) return
      if (i_para_refine_tbl_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_para_refine_tbl_ctl                                           &
     &     = find_control_end_flag(hd_para_refine_tbl_ctl)
!
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, p_refine_ctl%para_refine_plt)
!
        call read_ctl_data_4_course_mesh(p_refine_ctl)
        call read_ctl_data_4_refine_mesh(refine_ctl)
      end do
!
      end subroutine read_ref_para_itp_ctl_data
!
! -----------------------------------------------------------------------
!
       subroutine read_ctl_data_4_course_mesh(p_refine_ctl)
!
      type(file_ctls_refine_para), intent(inout) :: p_refine_ctl
!
!
      if(right_begin_flag(hd_course_mesh_para_ctl) .eq. 0) return
      if (i_course_mesh_para_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_course_mesh_para_ctl                                          &
     &     = find_control_end_flag(hd_course_mesh_para_ctl)
        if(i_course_mesh_para_ctl .gt. 0) exit
!
!
        call read_integer_ctl_type(hd_num_course_subdomain,             &
     &      p_refine_ctl%nprocs_course_ctl)
!
        call read_chara_ctl_type(hd_course_mesh_file_head,              &
     &      p_refine_ctl%course_mesh_file_head_ctl)
        call read_chara_ctl_type(hd_course_to_fine_p_head,              &
     &      p_refine_ctl%c2f_para_head_ctl)
        call read_chara_ctl_type(hd_fine_to_course_p_head,              &
     &      p_refine_ctl%f2c_para_head_ctl)
!
        call read_chara_ctl_type(hd_fine_to_course_ele_head,            &
     &      p_refine_ctl%refine_info_para_head_ctl)
      end do
!
      end subroutine read_ctl_data_4_course_mesh
!
! -----------------------------------------------------------------------
!
      end module t_control_data_refine_para
