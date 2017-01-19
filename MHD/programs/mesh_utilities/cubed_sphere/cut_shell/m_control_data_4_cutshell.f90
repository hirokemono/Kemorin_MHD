!
!      module m_control_data_4_cutshell
!
!      Written by Kemorin on Oct., 2007
!
!       subroutine read_control_data_4_cutshell
!
      module m_control_data_4_cutshell
!
      use m_precision
!
      use t_control_elements
      use m_read_control_elements
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint), parameter :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name = 'ctl_cutshell'
!
      type(read_character_item), save :: orginal_mesh_head_ctl
      type(read_character_item), save :: orginal_mesh_fmt_ctl
!
      type(read_character_item), save :: cutshell_mesh_head_ctl
      type(read_character_item), save :: cutshell_mesh_fmt_ctl
!
      type(read_character_item), save :: cutshell_type_ctl
!
!
!   Top level
!
      character(len=kchara), parameter :: hd_cutshell_ctl               &
     &                      = 'cutshell_control'
      integer (kind=kint) :: i_cutshell_ctl = 0
!
!   2nd level for cutshell_ctl
!
      character(len=kchara), parameter :: hd_files_ctl                  &
     &                      = 'file_name_ctl'
      character(len=kchara), parameter :: hd_cutshell_param             &
     &                      = 'cutshell_parameter_ctl'
      integer (kind=kint) :: i_files_ctl =      0
      integer (kind=kint) :: i_cutshell_param = 0
!
!   3rd level for file_names
!
      character(len=kchara), parameter :: hd_org_f_ctl                  &
     &                      = 'orginal_mesh_head_ctl'
      character(len=kchara), parameter :: hd_org_fmt_ctl                &
     &                      = 'orginal_mesh_format_ctl'
!
      character(len=kchara), parameter :: hd_cutshell_f_ctl             &
     &                      = 'cutted_mesh_head_ctl'
      character(len=kchara), parameter :: hd_cutshell_fmt_ctl           &
     &                      = 'cutted_mesh_format_ctl'
!
!   3rd level for cutshell_parameter_ctl
!
      character(len=kchara), parameter :: hd_cutshell_type              &
     &                      = 'cutshell_type_ctl'
!
      private :: control_file_name
      private :: hd_cutshell_ctl, i_cutshell_ctl
      private :: hd_files_ctl, hd_cutshell_param
      private :: i_files_ctl,  i_cutshell_param
      private :: hd_org_f_ctl,   hd_cutshell_f_ctl
      private :: hd_org_fmt_ctl, hd_cutshell_fmt_ctl
!
      private :: read_cutshell_control_data
      private :: read_ctl_data_4_cutshell_mesh
      private :: read_ctl_data_4_cutshell_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_4_cutshell
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_cutshell_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_4_cutshell
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------!
      subroutine read_cutshell_control_data
!
!
      if(right_begin_flag(hd_cutshell_ctl) .eq. 0) return
      if (i_cutshell_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_cutshell_ctl, i_cutshell_ctl)
        if(i_cutshell_ctl .gt. 0) exit
!
!
        call read_ctl_data_4_cutshell_mesh
        call read_ctl_data_4_cutshell_type
      end do
!
      end subroutine read_cutshell_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_ctl_data_4_cutshell_mesh
!
!
      if(right_begin_flag(hd_files_ctl) .eq. 0) return
      if (i_files_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_files_ctl, i_files_ctl)
        if(i_files_ctl .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_org_f_ctl, orginal_mesh_head_ctl)
        call read_chara_ctl_type(hd_org_fmt_ctl, orginal_mesh_fmt_ctl)
        call read_chara_ctl_type(hd_cutshell_f_ctl,                     &
     &      cutshell_mesh_head_ctl)
        call read_chara_ctl_type(hd_cutshell_fmt_ctl,                   &
     &      cutshell_mesh_fmt_ctl)
        end do
!
      end subroutine read_ctl_data_4_cutshell_mesh
!
! -----------------------------------------------------------------------!
      subroutine read_ctl_data_4_cutshell_type
!
!
      if(right_begin_flag(hd_cutshell_param) .eq. 0) return
      if (i_cutshell_param .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_cutshell_param, i_cutshell_param)
        if(i_cutshell_param .gt. 0) exit
!
        call read_chara_ctl_type(hd_cutshell_type, cutshell_type_ctl)
      end do
!
      end subroutine read_ctl_data_4_cutshell_type
!
! -----------------------------------------------------------------------!
      end module m_control_data_4_cutshell
