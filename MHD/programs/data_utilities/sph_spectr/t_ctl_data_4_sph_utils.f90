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
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
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
!
        integer(kind = kint) :: i_sph_trans_ctl = 0
        integer(kind = kint) :: i_sph_trans_model =  0
        integer(kind = kint) :: i_sph_trans_params = 0
      end type spherical_spectr_data_util_ctl
!
!   Top level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_ctl = 'spherical_transform'
!
!   1st level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_model =  'model'
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_params = 'sph_transform_ctl'
!
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
      private :: control_file_code, control_file_name
      private :: read_sph_utils_control_data, read_sph_trans_params_ctl
      private :: bcast_control_data_sph_utils
      private :: read_sph_trans_model_ctl, bcast_sph_trans_model_ctl
      private :: dealloc_sph_trans_model_ctl
      private :: bcast_sph_trans_params_ctl, reset_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_sph_utils(spu_ctl)
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
     if(my_rank .eq. 0) then
        open (control_file_code, file = control_file_name)
!
        do
          call load_one_line_from_control(control_file_code, c_buf1)
          call read_sph_utils_control_data                              &
     &       (control_file_code, hd_sph_trans_ctl, spu_ctl, c_buf1)
          if(spu_ctl%i_sph_trans_ctl .gt. 0) exit
        end do
        close(control_file_code)
      end if
!
      call bcast_control_data_sph_utils(spu_ctl)
!
      end subroutine read_control_data_sph_utils
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_utils_control_data                            &
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
      if(spu_ctl%i_sph_trans_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, spu_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, spu_ctl%org_plt, c_buf)
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, spu_ctl%Fmesh_ctl, c_buf)
!
        call read_sph_trans_model_ctl                                   &
     &     (id_control, hd_sph_trans_model, spu_ctl, c_buf)
        call read_sph_trans_params_ctl                                  &
     &     (id_control, hd_sph_trans_params, spu_ctl, c_buf)
!
        call read_sph_monitoring_ctl                                    &
     &     (id_control, hd_pick_sph, spu_ctl%smonitor_ctl, c_buf)
      end do
      spu_ctl%i_sph_trans_ctl = 1
!
      end subroutine read_sph_utils_control_data
!
! -----------------------------------------------------------------------
!
      subroutine bcast_control_data_sph_utils(spu_ctl)
!
      use bcast_4_platform_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_control_arrays
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
      call bcast_ctl_data_4_platform(spu_ctl%plt)
      call bcast_ctl_data_4_platform(spu_ctl%org_plt)
      call bcast_FEM_mesh_control(spu_ctl%Fmesh_ctl)
!
      call bcast_sph_monitoring_ctl(spu_ctl%smonitor_ctl)
!
      call bcast_sph_trans_model_ctl(spu_ctl)
      call bcast_sph_trans_params_ctl(spu_ctl)
!
      call MPI_BCAST(spu_ctl%i_sph_trans_ctl, 1,                        &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_control_data_sph_utils
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_control_data_sph_utils(spu_ctl)
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
      call reset_control_platforms(spu_ctl%plt)
      call reset_control_platforms(spu_ctl%org_plt)
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
      subroutine bcast_sph_trans_model_ctl(spu_ctl)
!
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
      use bcast_control_arrays
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
      call bcast_ctl_type_r1(spu_ctl%buoyancy_ratio_ctl)
      call bcast_ctl_type_r1(spu_ctl%thermal_buoyancy_ctl)
!
      call bcast_phys_data_ctl(spu_ctl%fld_ctl)
      call bcast_ctl_data_4_time_step(spu_ctl%tstep_ctl)
!
      call MPI_BCAST(spu_ctl%i_sph_trans_model, 1,                      &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_sph_trans_model_ctl
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
      subroutine bcast_sph_trans_params_ctl(spu_ctl)
!
      use bcast_control_arrays
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
      call bcast_ctl_type_c1(spu_ctl%zm_spec_file_head_ctl)
      call bcast_ctl_type_c1(spu_ctl%tave_ene_spec_head_ctl)
      call bcast_ctl_type_c1(spu_ctl%ene_spec_head_ctl)
      call bcast_ctl_type_c1(spu_ctl%vol_ene_spec_head_ctl)
!
      call MPI_BCAST(spu_ctl%i_sph_trans_params, 1,                     &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_sph_trans_params_ctl
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
