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
      use skip_comment_f
!
      implicit  none
!
      integer (kind = kint) :: control_file_code = 13
      character (len = kchara) :: control_file_name='ctl_sph_transform'
!
      character(len = kchara) :: zm_spec_file_head_ctl
      character(len = kchara) :: tave_ene_spec_head_ctl
      character(len = kchara) :: ene_spec_head_ctl
      character(len = kchara) :: vol_ene_spec_head_ctl
!
      real(kind = kreal) :: buoyancy_ratio_ctl
      real(kind = kreal) :: thermal_buoyancy_ctl
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
      integer (kind=kint) :: i_sph_trans_model =  0
      integer (kind=kint) :: i_sph_trans_params = 0
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
      integer (kind=kint) :: i_ene_spec_head =          0
      integer (kind=kint) :: i_vol_ene_spec_head =      0
      integer (kind=kint) :: i_zm_sph_spec_file =       0
      integer (kind=kint) :: i_tsph_esp_file =          0
!
      integer (kind=kint) :: i_buo_ratio =         0
      integer (kind=kint) :: i_thermal_buo =       0
!
      integer (kind=kint) :: i_cmb_grp =         0
!
      private :: control_file_code, control_file_name
      private :: hd_sph_trans_ctl, i_sph_trans_ctl
      private :: hd_sph_trans_model, i_sph_trans_model
      private :: hd_sph_trans_params, i_sph_trans_params
      private :: hd_tsph_esp_file
      private :: hd_ene_spec_head, hd_vol_ene_spec_head
      private :: hd_zm_sph_spec_file
      private :: hd_buo_ratio, hd_thermal_buo
      private :: read_sph_trans_control_data, read_sph_trans_params_ctl
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
      call read_sph_trans_control_data
!
      close(ctl_file_code)
!
      return
      end subroutine read_control_data_sph_utils
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_control_data
!
      use m_machine_parameter
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_ctl_data_4_pickup_sph
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
        call read_ctl_data_4_platform
        call read_ctl_data_4_org_data
!
        call read_sph_trans_model_ctl
        call read_sph_trans_params_ctl
!
        call read_pickup_sph_ctl
      end do
!
      end subroutine read_sph_trans_control_data
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_model_ctl
!
      use m_ctl_data_4_time_steps
      use m_ctl_data_4_fields
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
!
        call read_phys_values
        call read_time_step_ctl
!
!
        call read_real_ctl_item(hd_buo_ratio,                           &
     &          i_buo_ratio, buoyancy_ratio_ctl)
        call read_real_ctl_item(hd_thermal_buo,                         &
     &          i_thermal_buo, thermal_buoyancy_ctl)
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
        call read_character_ctl_item(hd_ene_spec_head,                  &
     &          i_ene_spec_head, ene_spec_head_ctl)
        call read_character_ctl_item(hd_vol_ene_spec_head,              &
     &          i_vol_ene_spec_head, vol_ene_spec_head_ctl)
        call read_character_ctl_item(hd_zm_sph_spec_file,               &
     &          i_zm_sph_spec_file, zm_spec_file_head_ctl)
         call read_character_ctl_item(hd_tsph_esp_file,                 &
     &          i_tsph_esp_file, tave_ene_spec_head_ctl)
      end do
!
      end subroutine read_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_sph_utils
