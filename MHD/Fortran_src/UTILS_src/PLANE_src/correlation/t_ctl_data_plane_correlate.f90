!t_ctl_data_plane_correlate.f90
!      module t_ctl_data_plane_correlate
!
!      Written by Kemorin
!
!!       subroutine read_control_data_cor_plane(pcor_c)
!!        type(ctl_data_plane_correlate), intent(inout) :: pcor_c
!
      module t_ctl_data_plane_correlate
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_ctl_data_4_plane_model
      use t_control_elements
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint) :: control_file_code = 11
      character (len = kchara) :: control_file_name='ctl_correlate'
!
      type ctl_data_plane_correlate
!>        Structure for field information control
        type(field_control) :: fld_pc_ctl
!
!>        Structure for time stepping control
        type(time_data_control) :: t_pc_ctl
!>          Structure for cube domain
        type(ctl_data_4_plane_model) :: cube_c_corr
        type(ctl_data_4_plane_model) :: cube2nd_c
!
        type(read_character_item) :: cor_mesh_head_ctl
        type(read_character_item) :: cor_mesh_fmt_ctl
!
        type(read_character_item) :: ref_mesh_head_ctl
        type(read_character_item) :: ref_mesh_fmt_ctl
!
        type(read_character_item) :: cor_udt_head_ctl
        type(read_character_item) :: ref_udt_head_ctl
!
        integer (kind=kint) :: i_cor_plane_ctl = 0
!
        integer (kind=kint) :: i_hard =    0
        integer (kind=kint) :: i_model = 0
        integer (kind=kint) :: i_control = 0
      end type ctl_data_plane_correlate
!
!
!   Top level
!
      character(len=kchara) :: hd_cor_plane_ctl                         &
     &                     = 'plane_correlate_control'
!
!   2nd level for plane_correlate_control
!
      character(len=kchara), parameter :: hd_hard = 'platform'
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
      character(len=kchara), parameter                                  &
     &         :: hd_ref_plane_mesh_ctl = 'reference_plane_mesh_ctl'
!
      character(len=kchara), parameter                                  &
     &             :: hd_plane_def = 'plane_mesh_ctl'
!
!   3rd level for platform parameters
!
      character(len=kchara), parameter                                  &
     &         :: hd_mesh_head_ctl =     'correlated_mesh_header'
      character(len=kchara), parameter                                  &
     &         :: hd_mesh_fmt_ctl =      'correlated_mesh_format'
      character(len=kchara), parameter                                  &
     &         :: hd_ref_mesh_head_ctl = 'refered_mesh_header'
      character(len=kchara), parameter                                  &
     &         :: hd_ref_mesh_fmt_ctl = 'refered_mesh_format'
      character(len=kchara), parameter                                  &
     &         :: hd_udt_head_ctl =     'correlated_udt_header'
      character(len=kchara), parameter                                  &
     &         :: hd_ref_udt_head_ctl = 'refered_udt_header'
!
!>      label for block
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
!>      Number of field
      integer (kind=kint) :: i_phys_values =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      integer (kind=kint) :: i_tstep =      0
!
      private :: hd_cor_plane_ctl
      private :: hd_hard, hd_ref_plane_mesh_ctl
      private :: hd_ref_mesh_head_ctl, hd_ref_mesh_fmt_ctl
      private :: hd_mesh_head_ctl, hd_mesh_fmt_ctl
      private :: hd_udt_head_ctl,  hd_ref_udt_head_ctl
      private :: hd_model, hd_control
      private :: hd_phys_values, i_phys_values
      private :: hd_time_step, i_tstep, hd_plane_def
!
      private :: read_cor_plane_control_data
      private :: read_correlate_file_heads
      private :: read_merge_field_data, read_merge_step_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine read_control_data_cor_plane(pcor_c)
!
      type(ctl_data_plane_correlate), intent(inout) :: pcor_c
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_cor_plane_control_data(pcor_c)
!
      close(ctl_file_code)
!
      end subroutine read_control_data_cor_plane
!
! -----------------------------------------------------------------------
!
       subroutine read_cor_plane_control_data(pcor_c)
!
      type(ctl_data_plane_correlate), intent(inout) :: pcor_c
!
      if(right_begin_flag(hd_cor_plane_ctl) .eq. 0) return
      if (pcor_c%i_cor_plane_ctl .gt. 0) return
!
      do
        call load_ctl_label_and_line
!
        pcor_c%i_cor_plane_ctl                                          &
     &       = find_control_end_flag(hd_cor_plane_ctl)
        if(pcor_c%i_cor_plane_ctl .gt. 0) exit
!
!
        call read_correlate_file_heads(pcor_c)
!
        call read_merge_field_data(pcor_c)
        call read_merge_step_data(pcor_c)
!
        call read_plane_model_param_ctl                                 &
     &     (hd_plane_def, pcor_c%cube_c_corr)
        call read_plane_model_param_ctl                                 &
     &     (hd_ref_plane_mesh_ctl, pcor_c%cube2nd_c)
      end do
!
      end subroutine read_cor_plane_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_correlate_file_heads(pcor_c)
!
      type(ctl_data_plane_correlate), intent(inout) :: pcor_c
!
!
      if(right_begin_flag(hd_hard) .eq. 0) return
      if (pcor_c%i_hard .gt. 0) return
      do
        call load_ctl_label_and_line
!
        pcor_c%i_hard = find_control_end_flag(hd_hard)
        if(pcor_c%i_hard .gt. 0) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_mesh_head_ctl, pcor_c%cor_mesh_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_mesh_fmt_ctl, pcor_c%cor_mesh_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_ref_mesh_head_ctl, pcor_c%ref_mesh_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_ref_mesh_fmt_ctl, pcor_c%ref_mesh_fmt_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_udt_head_ctl, pcor_c%cor_udt_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_ref_udt_head_ctl, pcor_c%ref_udt_head_ctl)
      end do
!
      end subroutine read_correlate_file_heads
!
! -----------------------------------------------------------------------
!
       subroutine read_merge_field_data(pcor_c)
!
      type(ctl_data_plane_correlate), intent(inout) :: pcor_c
!
!
      if(right_begin_flag(hd_model) .eq. 0) return
      if (pcor_c%i_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        pcor_c%i_model = find_control_end_flag(hd_model)
        if(pcor_c%i_model .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (hd_phys_values, i_phys_values, pcor_c%fld_pc_ctl)
      end do
!
      end subroutine read_merge_field_data
!
! -----------------------------------------------------------------------
!
      subroutine read_merge_step_data(pcor_c)
!
      type(ctl_data_plane_correlate), intent(inout) :: pcor_c
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (pcor_c%i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        pcor_c%i_control = find_control_end_flag(hd_control)
        if(pcor_c%i_control .gt. 0) exit
!
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, pcor_c%t_pc_ctl)
      end do
!
      end subroutine read_merge_step_data
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_plane_correlate
