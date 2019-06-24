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
      use t_read_control_elements
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
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
!
      private :: hd_cor_plane_ctl
      private :: hd_hard, hd_ref_plane_mesh_ctl
      private :: hd_ref_mesh_head_ctl, hd_ref_mesh_fmt_ctl
      private :: hd_mesh_head_ctl, hd_mesh_fmt_ctl
      private :: hd_udt_head_ctl,  hd_ref_udt_head_ctl
      private :: hd_model, hd_control
      private :: hd_phys_values, hd_time_step, hd_plane_def
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
      type(buffer_for_control) :: c_buf1
!
!
      open (control_file_code, file = control_file_name)
!
      do
        call load_one_line_from_control(control_file_code, c_buf1)
        call read_cor_plane_control_data                                &
     &     (control_file_code, hd_cor_plane_ctl, pcor_c, c_buf1)
        if(pcor_c%i_cor_plane_ctl .gt. 0) exit
      end do
      close(control_file_code)
!
      end subroutine read_control_data_cor_plane
!
! -----------------------------------------------------------------------
!
       subroutine read_cor_plane_control_data                           &
     &         (id_control, hd_block, pcor_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_plane_correlate), intent(inout) :: pcor_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pcor_c%i_cor_plane_ctl .gt. 0) return
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_correlate_file_heads                                  &
     &     (id_control, hd_hard, pcor_c, c_buf)
!
        call read_merge_field_data                                      &
     &     (id_control, hd_model, pcor_c, c_buf)
        call read_merge_step_data                                       &
     &     (id_control, hd_control, pcor_c, c_buf)
!
        call read_plane_model_param_ctl                                 &
     &     (id_control, hd_plane_def, pcor_c%cube_c_corr, c_buf)
        call read_plane_model_param_ctl                                 &
     &     (id_control, hd_ref_plane_mesh_ctl,                          &
     &      pcor_c%cube2nd_c, c_buf)
      end do
      pcor_c%i_cor_plane_ctl = 1
!
      end subroutine read_cor_plane_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_correlate_file_heads                             &
     &         (id_control, hd_block, pcor_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_plane_correlate), intent(inout) :: pcor_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pcor_c%i_hard .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_mesh_head_ctl, pcor_c%cor_mesh_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_mesh_fmt_ctl, pcor_c%cor_mesh_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_ref_mesh_head_ctl, pcor_c%ref_mesh_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_ref_mesh_fmt_ctl, pcor_c%ref_mesh_fmt_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_udt_head_ctl, pcor_c%cor_udt_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_ref_udt_head_ctl, pcor_c%ref_udt_head_ctl)
      end do
      pcor_c%i_hard = 1
!
      end subroutine read_correlate_file_heads
!
! -----------------------------------------------------------------------
!
       subroutine read_merge_field_data                                 &
     &         (id_control, hd_block, pcor_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_plane_correlate), intent(inout) :: pcor_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pcor_c%i_model .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, pcor_c%fld_pc_ctl, c_buf)
      end do
      pcor_c%i_model = 1
!
      end subroutine read_merge_field_data
!
! -----------------------------------------------------------------------
!
      subroutine read_merge_step_data                                   &
     &         (id_control, hd_block, pcor_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_plane_correlate), intent(inout) :: pcor_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pcor_c%i_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, pcor_c%t_pc_ctl, c_buf)
      end do
      pcor_c%i_control = 1
!
      end subroutine read_merge_step_data
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_plane_correlate
