!
!      module t_ctl_data_4_sph_utils
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine dealloc_control_data_sph_utils(spu_ctl)
!!        type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!!
!!      subroutine read_sph_trans_model_ctl                             &
!!     &         (id_control, hd_block, spu_ctl, c_buf)
!!      subroutine write_sph_trans_model_ctl                            &
!!     &         (id_control, hd_block, spu_ctl, level)
!!      subroutine read_sph_trans_params_ctl                            &
!!     &         (id_control, hd_block, spu_ctl, c_buf)
!!      subroutine write_sph_trans_params_ctl                           &
!!     &         (id_control, hd_block, spu_ctl, level)
!
      module t_ctl_data_4_sph_utils
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_ctl_data_4_sph_monitor
      use t_control_array_character
      use t_control_array_real
      use skip_comment_f
!
      implicit  none
!
!>      Structure of spherical shell utilities
      type spherical_spectr_data_util_ctl
!>        Structure for file names
        type(platform_data_control) :: plt
!>        Structure for original file names
        type(platform_data_control) :: org_plt
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!
!>        Structure for field information control
        type(field_control) :: fld_ctl
!>        Structure for time stepping control
        type(time_data_control) :: tstep_ctl
!
        type(sph_monitor_control) :: smonitor_ctl
!
        type(read_character_item) :: zm_spec_file_head_ctl
        type(read_character_item) :: tave_ene_spec_head_ctl
        type(read_character_item) :: ene_spec_head_ctl
        type(read_character_item) :: vol_ene_spec_head_ctl
!
        type(read_real_item) :: buoyancy_ratio_ctl
        type(read_real_item) :: thermal_buoyancy_ctl
!
        integer(kind = kint) :: i_sph_trans_ctl = 0
        integer(kind = kint) :: i_sph_trans_model =  0
        integer(kind = kint) :: i_sph_trans_params = 0
      end type spherical_spectr_data_util_ctl
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_phys_values =  'phys_values_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_time_step = 'time_step_ctl'
      character(len=kchara), parameter, private                         &
     &            ::   hd_buo_ratio =    'buoyancy_ratio_ctl'
      character(len=kchara), parameter, private                         &
     &            ::   hd_thermal_buo =  'thermal_buoyancy_ctl'
!
!   2nd level
!
      character(len=kchara), parameter, private                         &
     &            ::  hd_ene_spec_head =  'energy_spec_head_ctl'
      character(len=kchara), parameter, private                         &
     &            ::  hd_vol_ene_spec_head =  'vol_ene_spec_head_ctl'
!
      character(len=kchara), parameter, private                         &
     &            ::  hd_zm_sph_spec_file   =  'zm_spectr_head_ctl'
      character(len=kchara), parameter, private                         &
     &            ::  hd_tsph_esp_file  =  'ave_ene_spec_head_ctl'
!
      private :: dealloc_sph_trans_model_ctl
      private :: reset_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_control_data_sph_utils(spu_ctl)
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
      call reset_control_platforms(spu_ctl%plt)
      call reset_control_platforms(spu_ctl%org_plt)
      call reset_FEM_mesh_control(spu_ctl%Fmesh_ctl)
!
      call dealloc_sph_monitoring_ctl(spu_ctl%smonitor_ctl)
!
      call dealloc_sph_trans_model_ctl(spu_ctl)
      call reset_sph_trans_params_ctl(spu_ctl)
!
      spu_ctl%i_sph_trans_ctl = 0
!
      end subroutine dealloc_control_data_sph_utils
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_model_ctl                               &
     &         (id_control, hd_block, spu_ctl, c_buf)
!
      use ctl_data_4_time_steps_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(spu_ctl%i_sph_trans_model .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, spu_ctl%fld_ctl, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, spu_ctl%tstep_ctl, c_buf)
!
        call read_real_ctl_type                                         &
     &     (c_buf,hd_buo_ratio, spu_ctl%buoyancy_ratio_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf,hd_thermal_buo, spu_ctl%thermal_buoyancy_ctl)
      end do
      spu_ctl%i_sph_trans_model = 1
!
      end subroutine read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_trans_model_ctl                              &
     &         (id_control, hd_block, spu_ctl, level)
!
      use ctl_data_4_time_steps_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spherical_spectr_data_util_ctl), intent(in) :: spu_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(spu_ctl%i_sph_trans_model .le. 0) return
!
      maxlen = len_trim(hd_buo_ratio)
      maxlen = max(maxlen, len_trim(hd_thermal_buo))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_phys_data_control                                      &
     &   (id_control, hd_phys_values, spu_ctl%fld_ctl, level)
      call write_control_time_step_data                                 &
     &   (id_control, hd_time_step, spu_ctl%tstep_ctl, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_buo_ratio, spu_ctl%buoyancy_ratio_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_thermal_buo, spu_ctl%thermal_buoyancy_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_trans_model_ctl(spu_ctl)
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
      call dealloc_phys_control(spu_ctl%fld_ctl)
!
      spu_ctl%buoyancy_ratio_ctl%iflag= 0
      spu_ctl%thermal_buoyancy_ctl%iflag= 0
!
      spu_ctl%i_sph_trans_model = 0
!
      end subroutine dealloc_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_params_ctl                              &
     &         (id_control, hd_block, spu_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(spu_ctl%i_sph_trans_params .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_ene_spec_head,               &
     &      spu_ctl%ene_spec_head_ctl)
        call read_chara_ctl_type(c_buf, hd_vol_ene_spec_head,           &
     &      spu_ctl%vol_ene_spec_head_ctl)
        call read_chara_ctl_type(c_buf, hd_zm_sph_spec_file,            &
     &      spu_ctl%zm_spec_file_head_ctl)
        call read_chara_ctl_type(c_buf, hd_tsph_esp_file,               &
     &      spu_ctl%tave_ene_spec_head_ctl)
      end do
      spu_ctl%i_sph_trans_params = 1
!
      end subroutine read_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_trans_params_ctl                             &
     &         (id_control, hd_block, spu_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(spherical_spectr_data_util_ctl), intent(in) :: spu_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(spu_ctl%i_sph_trans_params .le. 0) return
!
      maxlen = len_trim(hd_ene_spec_head)
      maxlen = max(maxlen, len_trim(hd_vol_ene_spec_head))
      maxlen = max(maxlen, len_trim(hd_zm_sph_spec_file))
      maxlen = max(maxlen, len_trim(hd_tsph_esp_file))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_ene_spec_head, spu_ctl%ene_spec_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_vol_ene_spec_head, spu_ctl%vol_ene_spec_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_zm_sph_spec_file, spu_ctl%zm_spec_file_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_tsph_esp_file, spu_ctl%tave_ene_spec_head_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      subroutine reset_sph_trans_params_ctl(spu_ctl)
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
      spu_ctl%ene_spec_head_ctl%iflag= 0
      spu_ctl%vol_ene_spec_head_ctl%iflag= 0
      spu_ctl%zm_spec_file_head_ctl%iflag= 0
      spu_ctl%tave_ene_spec_head_ctl%iflag= 0
!
      spu_ctl%i_sph_trans_params = 0
!
      end subroutine reset_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_sph_utils
