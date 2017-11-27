!
!      module m_ctl_data_4_sph_utils
!
!        programmed by H.Matsui on Oct., 2007
!
!      subroutine read_control_data_sph_utils
!
      module m_ctl_data_4_sph_utils
!
      use m_precision
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_ctl_data_4_sph_monitor
      use t_control_elements
      use skip_comment_f
!
      implicit  none
!
      integer (kind = kint) :: control_file_code = 13
      character (len = kchara) :: control_file_name='ctl_sph_transform'
!
!>      Structure for file names
      type(platform_data_control), save :: su_plt
!>      Structure for original file names
      type(platform_data_control), save :: org_su_plt
!>      Structure for field information control
      type(field_control), save :: fld_su_ctl
!>      Structure for time stepping control
      type(time_data_control), save :: t_su_ctl
!
      type(sph_monitor_control), save :: smonitor_u_ctl
!
      type(read_character_item), save :: zm_spec_file_head_ctl
      type(read_character_item), save :: tave_ene_spec_head_ctl
      type(read_character_item), save :: ene_spec_head_ctl
      type(read_character_item), save :: vol_ene_spec_head_ctl
!
      type(read_real_item), save :: buoyancy_ratio_ctl
      type(read_real_item), save :: thermal_buoyancy_ctl
!
!   Top level
!
      character(len=kchara) :: hd_sph_trans_ctl = 'spherical_transform'
      integer (kind=kint) :: i_sph_trans_ctl = 0
!
!   1st level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_sph_trans_model =  'model'
      character(len=kchara), parameter                                  &
     &                    :: hd_sph_trans_params = 'sph_transform_ctl'
!
      integer(kind=kint) :: i_sph_trans_model =  0
      integer(kind=kint) :: i_sph_trans_params = 0
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      character(len=kchara), parameter                                  &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
!
      integer(kind=kint) :: i_platform =   0
      integer(kind=kint) :: i_org_data =      0
      integer(kind=kint) :: i_phys_values =   0
      integer(kind=kint) :: i_tstep =      0
      integer(kind=kint) :: i_pick_sph = 0
!
!   2nd level
!
      character(len=kchara) :: hd_ene_spec_head                         &
     &                        =  'energy_spec_head_ctl'
      character(len=kchara) :: hd_vol_ene_spec_head                     &
     &                        =  'vol_ene_spec_head_ctl'
!
      character(len=kchara) :: hd_zm_sph_spec_file                      &
     &                        =  'zm_spectr_head_ctl'
      character(len=kchara) :: hd_tsph_esp_file                         &
     &                        =  'ave_ene_spec_head_ctl'
!
      character(len=kchara) :: hd_buo_ratio =    'buoyancy_ratio_ctl'
      character(len=kchara) :: hd_thermal_buo =  'thermal_buoyancy_ctl'
!
!
      private :: control_file_code, control_file_name
      private :: hd_sph_trans_ctl, i_sph_trans_ctl
      private :: hd_sph_trans_model, i_sph_trans_model
      private :: hd_sph_trans_params, i_sph_trans_params
      private :: hd_platform, i_platform
      private :: hd_org_data, i_org_data
      private :: hd_phys_values, i_phys_values
      private :: hd_time_step, i_tstep
      private :: hd_pick_sph, i_pick_sph
      private :: hd_tsph_esp_file
      private :: hd_ene_spec_head, hd_vol_ene_spec_head
      private :: hd_zm_sph_spec_file
      private :: hd_buo_ratio, hd_thermal_buo
      private :: read_sph_utils_control_data, read_sph_trans_params_ctl
      private :: read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_sph_utils
!
      use m_machine_parameter
!
!
      ctl_file_code = control_file_code
!
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_sph_utils_control_data
!
      close(ctl_file_code)
!
      return
      end subroutine read_control_data_sph_utils
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_utils_control_data
!
      use m_machine_parameter
      use calypso_mpi
!
!   2 begin phys_values_ctl
!
      if(right_begin_flag(hd_sph_trans_ctl) .eq. 0) return
      if (i_sph_trans_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sph_trans_ctl, i_sph_trans_ctl)
        if(i_sph_trans_ctl .gt. 0) exit
!
!
        call read_control_platforms(hd_platform, i_platform, su_plt)
        call read_control_platforms                                     &
     &     (hd_org_data, i_org_data, org_su_plt)
!
        call read_sph_trans_model_ctl
        call read_sph_trans_params_ctl
!
        call read_sph_monitoring_ctl                                    &
     &     (hd_pick_sph, i_pick_sph, smonitor_u_ctl)
      end do
!
      end subroutine read_sph_utils_control_data
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_model_ctl
!
!
      if(right_begin_flag(hd_sph_trans_model) .eq. 0) return
      if (i_sph_trans_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sph_trans_model,                  &
     &      i_sph_trans_model)
        if(i_sph_trans_model .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (hd_phys_values, i_phys_values, fld_su_ctl)
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, t_su_ctl)
!
        call read_real_ctl_type(hd_buo_ratio, buoyancy_ratio_ctl)
        call read_real_ctl_type(hd_thermal_buo, thermal_buoyancy_ctl)
      end do
!
      end subroutine read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_params_ctl
!
!
      if(right_begin_flag(hd_sph_trans_params) .eq. 0) return
      if (i_sph_trans_params .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sph_trans_params,                 &
     &      i_sph_trans_params)
        if(i_sph_trans_params .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_ene_spec_head,                      &
     &      ene_spec_head_ctl)
        call read_chara_ctl_type(hd_vol_ene_spec_head,                  &
     &      vol_ene_spec_head_ctl)
        call read_chara_ctl_type(hd_zm_sph_spec_file,                   &
     &      zm_spec_file_head_ctl)
        call read_chara_ctl_type(hd_tsph_esp_file,                      &
     &      tave_ene_spec_head_ctl)
      end do
!
      end subroutine read_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_sph_utils
