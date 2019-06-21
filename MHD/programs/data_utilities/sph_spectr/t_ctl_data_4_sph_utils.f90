!
!      module t_ctl_data_4_sph_utils
!
!        programmed by H.Matsui on Oct., 2007
!
!      subroutine read_control_data_sph_utils
!
      module t_ctl_data_4_sph_utils
!
      use m_precision
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
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
      end type spherical_spectr_data_util_ctl
!
!   Top level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_ctl = 'spherical_transform'
      integer (kind=kint), private :: i_sph_trans_ctl = 0
!
!   1st level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_model =  'model'
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_params = 'sph_transform_ctl'
!
      integer(kind=kint), private :: i_sph_trans_model =  0
      integer(kind=kint), private :: i_sph_trans_params = 0
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_mesh = 'FEM_mesh_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_phys_values =  'phys_values_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_time_step = 'time_step_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_pick_sph = 'sph_monitor_ctl'
!
      integer(kind=kint), private :: i_platform =    0
      integer(kind=kint), private :: i_org_data =    0
      integer(kind=kint), private :: i_phys_values = 0
      integer(kind=kint), private :: i_tstep =       0
      integer(kind=kint), private :: i_pick_sph =    0
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
      character(len=kchara), parameter, private                         &
     &            ::   hd_buo_ratio =    'buoyancy_ratio_ctl'
      character(len=kchara), parameter, private                         &
     &            ::   hd_thermal_buo =  'thermal_buoyancy_ctl'
!
!
      private :: control_file_code, control_file_name
      private :: read_sph_utils_control_data, read_sph_trans_params_ctl
      private :: read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_sph_utils(spu_ctl)
!
      use calypso_mpi
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
     if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
!
        open (ctl_file_code, file = control_file_name)
!
        call load_ctl_label_and_line
        call read_sph_utils_control_data(spu_ctl)
!
        close(ctl_file_code)
      end if
!
      call bcast_control_data_sph_utils(spu_ctl)
!
      end subroutine read_control_data_sph_utils
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_utils_control_data(spu_ctl)
!
      use calypso_mpi
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!   2 begin phys_values_ctl
!
      if(right_begin_flag(hd_sph_trans_ctl) .eq. 0) return
      if (i_sph_trans_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_sph_trans_ctl = find_control_end_flag(hd_sph_trans_ctl)
        if(i_sph_trans_ctl .gt. 0) exit
!
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, spu_ctl%plt)
        call read_control_platforms                                     &
     &     (hd_org_data, i_org_data, spu_ctl%org_plt)
        call read_FEM_mesh_control                                      &
     &     (ctl_file_code, hd_FEM_mesh, spu_ctl%Fmesh_ctl, c_buf1)
!
        call read_sph_trans_model_ctl(spu_ctl)
        call read_sph_trans_params_ctl(spu_ctl)
!
        call read_sph_monitoring_ctl(ctl_file_code, hd_pick_sph,        &
     &      i_pick_sph, spu_ctl%smonitor_ctl, c_buf1)
      end do
!
      end subroutine read_sph_utils_control_data
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_model_ctl(spu_ctl)
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
      if(right_begin_flag(hd_sph_trans_model) .eq. 0) return
      if (i_sph_trans_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_sph_trans_model = find_control_end_flag(hd_sph_trans_model)
        if(i_sph_trans_model .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (hd_phys_values, i_phys_values, spu_ctl%fld_ctl)
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, spu_ctl%tstep_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf1,hd_buo_ratio, spu_ctl%buoyancy_ratio_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf1,hd_thermal_buo, spu_ctl%thermal_buoyancy_ctl)
      end do
!
      end subroutine read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_params_ctl(spu_ctl)
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
      if(right_begin_flag(hd_sph_trans_params) .eq. 0) return
      if (i_sph_trans_params .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_sph_trans_params = find_control_end_flag(hd_sph_trans_params)
        if(i_sph_trans_params .gt. 0) exit
!
!
        call read_chara_ctl_type(c_buf1, hd_ene_spec_head,              &
     &      spu_ctl%ene_spec_head_ctl)
        call read_chara_ctl_type(c_buf1, hd_vol_ene_spec_head,          &
     &      spu_ctl%vol_ene_spec_head_ctl)
        call read_chara_ctl_type(c_buf1, hd_zm_sph_spec_file,           &
     &      spu_ctl%zm_spec_file_head_ctl)
        call read_chara_ctl_type(c_buf1, hd_tsph_esp_file,              &
     &      spu_ctl%tave_ene_spec_head_ctl)
      end do
!
      end subroutine read_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_control_data_sph_utils(spu_ctl)
!
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_control_arrays
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
      call bcast_ctl_data_4_platform(spu_ctl%plt)
      call bcast_ctl_data_4_platform(spu_ctl%org_plt)
      call bcast_FEM_mesh_control(spu_ctl%Fmesh_ctl)
!
      call bcast_phys_data_ctl(spu_ctl%fld_ctl)
      call bcast_ctl_data_4_time_step(spu_ctl%tstep_ctl)
!
      call bcast_sph_monitoring_ctl(spu_ctl%smonitor_ctl)
!
      call bcast_ctl_type_c1(spu_ctl%zm_spec_file_head_ctl)
      call bcast_ctl_type_c1(spu_ctl%tave_ene_spec_head_ctl)
      call bcast_ctl_type_c1(spu_ctl%ene_spec_head_ctl)
      call bcast_ctl_type_c1(spu_ctl%vol_ene_spec_head_ctl)
!
      call bcast_ctl_type_r1(spu_ctl%buoyancy_ratio_ctl)
      call bcast_ctl_type_r1(spu_ctl%thermal_buoyancy_ctl)
!
      end subroutine bcast_control_data_sph_utils
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_sph_utils
