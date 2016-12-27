!m_control_data_refine_para.f90
!      module m_control_data_refine_para
!
      module m_control_data_refine_para
!
!      Written by Kemorin on Oct., 2007
!
      use m_precision
!
      use m_read_control_elements
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint), parameter :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name = 'ctl_para_refine_table'
!
      integer(kind = kint) :: nprocs_course_ctl = 0
      character (len = kchara) :: course_mesh_file_head_ctl
      character (len = kchara) :: c2f_para_head_ctl
      character (len = kchara) :: f2c_para_head_ctl
      character (len = kchara) :: refine_info_para_head_ctl
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
      character(len=kchara), parameter :: hd_course_mesh_para_ctl       &
     &                      = 'parallel_course_mesh_ctl'
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
      integer (kind=kint) :: i_num_course_subdomain =    0
      integer (kind=kint) :: i_course_mesh_file_head =   0
      integer (kind=kint) :: i_course_to_fine_p_head =   0
      integer (kind=kint) :: i_fine_to_course_p_head =   0
      integer (kind=kint) :: i_fine_to_course_ele_head = 0
!
!
      private :: hd_para_refine_tbl_ctl,  i_para_refine_tbl_ctl
      private :: hd_course_mesh_para_ctl, i_course_mesh_para_ctl
      private :: hd_num_course_subdomain, hd_course_mesh_file_head
      private :: hd_fine_to_course_p_head,  hd_course_to_fine_p_head
      private :: hd_fine_to_course_ele_head
      private :: read_ref_para_itp_ctl_data
      private :: read_ctl_data_4_course_mesh
!
!       subroutine read_control_data_ref_para_itp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine read_control_data_ref_para_itp
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_ref_para_itp_ctl_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_ref_para_itp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ref_para_itp_ctl_data
!
      use m_ctl_data_4_platforms
      use m_control_data_4_refine
!
!
      if(right_begin_flag(hd_para_refine_tbl_ctl) .eq. 0) return
      if (i_para_refine_tbl_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_para_refine_tbl_ctl,              &
     &      i_para_refine_tbl_ctl)
!
!
        call read_ctl_data_4_platform
!
        call read_ctl_data_4_course_mesh
        call read_ctl_data_4_refine_mesh
      end do
!
      end subroutine read_ref_para_itp_ctl_data
!
! -----------------------------------------------------------------------
!
       subroutine read_ctl_data_4_course_mesh
!
!
      if(right_begin_flag(hd_course_mesh_para_ctl) .eq. 0) return
      if (i_course_mesh_para_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_course_mesh_para_ctl,             &
     &      i_course_mesh_para_ctl)
        if(i_course_mesh_para_ctl .gt. 0) exit
!
!
        call read_integer_ctl_item(hd_num_course_subdomain,             &
     &            i_num_course_subdomain, nprocs_course_ctl)
!
!
        call read_character_ctl_item(hd_course_mesh_file_head,          &
     &           i_course_mesh_file_head, course_mesh_file_head_ctl)
        call read_character_ctl_item(hd_course_to_fine_p_head,          &
     &           i_course_to_fine_p_head, c2f_para_head_ctl)
        call read_character_ctl_item(hd_fine_to_course_p_head,          &
     &           i_fine_to_course_p_head, f2c_para_head_ctl)
!
        call read_character_ctl_item(hd_fine_to_course_ele_head,        &
     &           i_fine_to_course_ele_head, refine_info_para_head_ctl)
      end do
!
      end subroutine read_ctl_data_4_course_mesh
!
! -----------------------------------------------------------------------
!
      end module m_control_data_refine_para
