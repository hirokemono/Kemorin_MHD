!
!      module m_control_plane_correlate
!
!      Written by Kemorin
!
!       subroutine read_control_data_cor_plane
!
      module m_control_plane_correlate
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint) :: control_file_code = 11
      character (len = kchara) :: control_file_name='ctl_correlate'
!
      character(len = kchara) :: cor_mesh_head_ctl = "mesh/in"
      character(len = kchara) :: ref_mesh_head_ctl = "mesh_ref/in"
      character(len = kchara) :: cor_udt_head_ctl = "field/out"
      character(len = kchara) :: ref_udt_head_ctl = "field_ref/out"
!
!   Top level
!
      character(len=kchara) :: hd_cor_plane_ctl                         &
     &                     = 'plane_correlate_control'
      integer (kind=kint) :: i_cor_plane_ctl = 0
!
!   2nd level for plane_correlate_control
!
      character(len=kchara), parameter :: hd_hard = 'platform'
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
      character(len=kchara), parameter                                  &
     &         :: hd_ref_plane_mesh_ctl = 'reference_plane_mesh_ctl'
!
      integer (kind=kint) :: i_hard =    0
      integer (kind=kint) :: i_model = 0
      integer (kind=kint) :: i_control = 0
!
!   3rd level for platform parameters
!
      character(len=kchara), parameter                                  &
     &         :: hd_mesh_head_ctl =     'correlated_mesh_header'
      character(len=kchara), parameter                                  &
     &         :: hd_ref_mesh_head_ctl = 'refered_mesh_header'
      character(len=kchara), parameter                                  &
     &         :: hd_udt_head_ctl =     'correlated_udt_header'
      character(len=kchara), parameter                                  &
     &         :: hd_ref_udt_head_ctl = 'refered_udt_header'
!
      integer (kind=kint) :: i_mesh_head_ctl =      0
      integer (kind=kint) :: i_ref_mesh_head_ctl =  0
      integer (kind=kint) :: i_udt_head_ctl =       0
      integer (kind=kint) :: i_ref_udt_head_ctl =   0
!
!
      private :: hd_cor_plane_ctl, i_cor_plane_ctl
      private :: hd_hard, hd_ref_plane_mesh_ctl
      private :: i_hard
      private :: hd_mesh_head_ctl, hd_ref_mesh_head_ctl
      private :: hd_udt_head_ctl,  hd_ref_udt_head_ctl
      private :: hd_model, hd_control, i_model, i_control
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
       subroutine read_control_data_cor_plane
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_cor_plane_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_cor_plane
!
! -----------------------------------------------------------------------
!
       subroutine read_cor_plane_control_data
!
      use m_ctl_data_4_plane_model
      use m_ctl_data_2nd_plane
!
!
      if(right_begin_flag(hd_cor_plane_ctl) .eq. 0) return
      if (i_cor_plane_ctl .gt. 0) return
!
      hd_2nd_plane_def = hd_ref_plane_mesh_ctl
!
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_cor_plane_ctl, i_cor_plane_ctl)
        if(i_cor_plane_ctl .gt. 0) exit
!
!
        call read_correlate_file_heads
!
        call read_merge_field_data
        call read_merge_step_data
!
        call read_plane_model_param_ctl
        call read_2nd_plane_model_param_ctl
      end do
!
      end subroutine read_cor_plane_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_correlate_file_heads
!
!
      if(right_begin_flag(hd_hard) .eq. 0) return
      if (i_hard .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_hard, i_hard)
        if(i_hard .gt. 0) exit
!
        call read_character_ctl_item(hd_mesh_head_ctl,                  &
     &        i_mesh_head_ctl, cor_mesh_head_ctl)
        call read_character_ctl_item(hd_ref_mesh_head_ctl,              &
     &        i_ref_mesh_head_ctl, ref_mesh_head_ctl)
        call read_character_ctl_item(hd_udt_head_ctl,                   &
     &        i_udt_head_ctl, cor_udt_head_ctl)
        call read_character_ctl_item(hd_ref_udt_head_ctl,               &
     &        i_ref_udt_head_ctl, ref_udt_head_ctl)
      end do
!
      end subroutine read_correlate_file_heads
!
! -----------------------------------------------------------------------
!
       subroutine read_merge_field_data
!
       use m_ctl_data_4_fields
!
!   2 begin phys_values_ctl
!
      if(right_begin_flag(hd_model) .eq. 0) return
      if (i_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_model, i_model)
        if(i_model .gt. 0) exit
!
        call read_phys_values
      end do
!
      end subroutine read_merge_field_data
!
! -----------------------------------------------------------------------
!
       subroutine read_merge_step_data
!
       use m_ctl_data_4_time_steps
!
!   2 begin time_step_ctl
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_control, i_control)
        if(i_control .gt. 0) exit
!
        call read_time_step_ctl
      end do
!
      end subroutine read_merge_step_data
!
! -----------------------------------------------------------------------
!
      end module m_control_plane_correlate
