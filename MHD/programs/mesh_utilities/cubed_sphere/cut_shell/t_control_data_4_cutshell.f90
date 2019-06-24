!
!      module t_control_data_4_cutshell
!
!      Written by Kemorin on Oct., 2007
!
!!      subroutine read_control_data_4_cutshell(cutshell_ctl)
!!        type(ctl_data_cutshell), intent(inout)  :: cutshell_ctl
!
      module t_control_data_4_cutshell
!
      use m_precision
!
      use t_control_elements
      use t_read_control_elements
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint), parameter :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name = 'ctl_cutshell'
!
      character (len = kchara), parameter                               &
     &         :: cf_hemisphere1 = 'hemisphere'
      character (len = kchara), parameter                               &
     &         :: cf_hemisphere2 = 'hemi'
      character (len = kchara), parameter                               &
     &         :: cf_cutshell =    'cut_shell'
      character (len = kchara), parameter                               &
     &         :: cf_sph_shell =   'spherical_shell'
      character (len = kchara), parameter                               &
     &         :: cf_hemi_shell =  'hemispherical_shell'
!
      type ctl_data_cutshell
        type(read_character_item) :: orginal_mesh_head_ctl
        type(read_character_item) :: orginal_mesh_fmt_ctl
!
        type(read_character_item) :: cutshell_mesh_head_ctl
        type(read_character_item) :: cutshell_mesh_fmt_ctl
!
        type(read_character_item) :: cutshell_type_ctl
!
        integer (kind=kint) :: i_cutshell_ctl =   0
        integer (kind=kint) :: i_files_ctl =      0
        integer (kind=kint) :: i_cutshell_param = 0
      end type ctl_data_cutshell
!
!   Top level
!
      character(len=kchara), parameter :: hd_cutshell_ctl               &
     &                      = 'cutshell_control'
!
!   2nd level for cutshell_ctl
!
      character(len=kchara), parameter :: hd_files_ctl                  &
     &                      = 'file_name_ctl'
      character(len=kchara), parameter :: hd_cutshell_param             &
     &                      = 'cutshell_parameter_ctl'
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
      private :: hd_cutshell_ctl, hd_files_ctl, hd_cutshell_param
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
      subroutine read_control_data_4_cutshell(cutshell_ctl)
!
      type(ctl_data_cutshell), intent(inout)  :: cutshell_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      open (control_file_code, file = control_file_name)
      do
        call load_one_line_from_control(control_file_code, c_buf1)
        call read_cutshell_control_data                                 &
     &     (control_file_code, hd_cutshell_ctl, cutshell_ctl, c_buf1)
        if(cutshell_ctl%i_cutshell_ctl .gt. 0) exit
      end do
      close(control_file_code)
!
      end subroutine read_control_data_4_cutshell
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_cutshell_control_data                             &
     &         (id_control, hd_block, cutshell_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_cutshell), intent(inout)  :: cutshell_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(cutshell_ctl%i_cutshell_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_ctl_data_4_cutshell_mesh                              &
     &     (id_control, hd_files_ctl, cutshell_ctl, c_buf)
        call read_ctl_data_4_cutshell_type                              &
     &     (id_control, hd_cutshell_param, cutshell_ctl, c_buf)
      end do
      cutshell_ctl%i_cutshell_ctl = 1
!
      end subroutine read_cutshell_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_ctl_data_4_cutshell_mesh                         &
     &         (id_control, hd_block, cutshell_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_cutshell), intent(inout)  :: cutshell_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(cutshell_ctl%i_files_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_org_f_ctl, cutshell_ctl%orginal_mesh_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_org_fmt_ctl, cutshell_ctl%orginal_mesh_fmt_ctl)
        call read_chara_ctl_type(c_buf, hd_cutshell_f_ctl,              &
     &      cutshell_ctl%cutshell_mesh_head_ctl)
        call read_chara_ctl_type(c_buf, hd_cutshell_fmt_ctl,            &
     &      cutshell_ctl%cutshell_mesh_fmt_ctl)
        end do
      cutshell_ctl%i_files_ctl = 1
!
      end subroutine read_ctl_data_4_cutshell_mesh
!
! -----------------------------------------------------------------------!
      subroutine read_ctl_data_4_cutshell_type                          &
     &         (id_control, hd_block, cutshell_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_cutshell), intent(inout)  :: cutshell_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(cutshell_ctl%i_cutshell_param .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_cutshell_type, cutshell_ctl%cutshell_type_ctl)
      end do
      cutshell_ctl%i_cutshell_param = 1
!
      end subroutine read_ctl_data_4_cutshell_type
!
! -----------------------------------------------------------------------
!
      end module t_control_data_4_cutshell
