!
!      module t_ctl_data_4_sph_trans
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine read_control_data_sph_trans(spt_ctl)
!!      subroutine dealloc_sph_trans_control_data(spt_ctl)
!!        type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
      module t_ctl_data_4_sph_trans
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_control_data_vizs
      use t_control_elements
      use skip_comment_f
!
      implicit  none
!
      integer (kind = kint) :: control_file_code = 13
      character (len = kchara) :: control_file_name='ctl_sph_transform'
!
      type spherical_transform_util_ctl
!>        Structure for file names
        type(platform_data_control) :: plt
!>        Structure for original file names
        type(platform_data_control) :: org_plt
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!
!
!>        Structure for field information control
        type(field_control) :: fld_ctl
!>        Structure for time stepping control
        type(time_data_control) :: t_ctl
!>        Structures of visualization controls
        type(visualization_controls) :: viz_ctls
!
        type(read_character_item) :: zm_spec_file_head_ctl
        type(read_character_item) :: zonal_udt_head_ctl
!
        type(read_character_item) :: cmb_radial_grp_ctl
        type(read_character_item) :: icb_radial_grp_ctl
        type(read_character_item) :: gauss_sph_fhead_ctl
!
        type(read_character_item) :: Legendre_trans_loop_ctl
        type(read_character_item) :: FFT_lib_ctl
        type(read_character_item) :: import_mode_ctl
!
        type(read_integer_item) :: legendre_vector_len_ctl
!
        integer(kind=kint) :: i_sph_trans_ctl = 0
        integer(kind=kint) :: i_sph_trans_model =  0
        integer(kind=kint) :: i_sph_trans_params = 0
      end type spherical_transform_util_ctl
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
!
!   2nd level
!
      character(len=kchara), parameter, private                         &
     &      :: hd_sph_transform_mode  =  'Legendre_trans_loop_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_FFT_package =  'FFT_library_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_import_mode =  'import_table_mode_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_legendre_vect_len = 'Legendre_vector_length_ctl'
!
      character(len=kchara), parameter, private                         &
     &             ::  hd_zm_sph_spec_file =  'zm_spectr_head_ctl'
      character(len=kchara), parameter, private                         &
     &             ::  hd_zm_field_file =  'zonal_udt_head_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_cmb_grp =    'radial_CMB_group_name'
      character(len=kchara), parameter, private                         &
     &             :: hd_icb_grp =  'radial_ICB_group_name'
      character(len=kchara), parameter, private                         &
     &             :: hd_gauss_file_name = 'sph_gauss_coefs_head_ctl'
!
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_control = 'visual_control'
!
      private :: control_file_code, control_file_name
      private :: hd_viz_control
!
      private :: read_sph_trans_control_data
      private :: bcast_sph_trans_control_data
!
      private :: read_sph_trans_model_ctl, read_sph_trans_params_ctl
      private :: bcast_sph_trans_model_ctl, reset_sph_trans_model_ctl
      private :: bcast_sph_trans_params_ctl, reset_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_sph_trans(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open (control_file_code, file = control_file_name)
        do
          call load_one_line_from_control(control_file_code, c_buf1)
          call read_sph_trans_control_data                              &
     &       (control_file_code, hd_sph_trans_ctl, spt_ctl, c_buf1)
          if(spt_ctl%i_sph_trans_ctl .gt. 0) exit
        end do
        close(control_file_code)
      end if
!
      call bcast_sph_trans_control_data(spt_ctl)
!
      end subroutine read_control_data_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_control_data                            &
     &         (id_control, hd_block, spt_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(spt_ctl%i_sph_trans_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, spt_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, spt_ctl%org_plt, c_buf)
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, spt_ctl%Fmesh_ctl, c_buf)
!
        call read_sph_trans_model_ctl                                   &
     &     (id_control, hd_sph_trans_model, spt_ctl, c_buf)
        call read_sph_trans_params_ctl                                  &
     &     (id_control, hd_sph_trans_params, spt_ctl, c_buf)
!
        call read_viz_controls                                          &
     &     (id_control, hd_viz_control, spt_ctl%viz_ctls, c_buf)
      end do
      spt_ctl%i_sph_trans_ctl = 1
!
      end subroutine read_sph_trans_control_data
!
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_trans_control_data(spt_ctl)
!
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
      use bcast_4_platform_ctl
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      call bcast_sph_trans_model_ctl(spt_ctl)
      call bcast_sph_trans_params_ctl(spt_ctl)
!
      call bcast_ctl_data_4_platform(spt_ctl%plt)
      call bcast_ctl_data_4_platform(spt_ctl%org_plt)
      call bcast_FEM_mesh_control(spt_ctl%Fmesh_ctl)
      call bcast_viz_controls(spt_ctl%viz_ctls)
!
      call MPI_BCAST(spt_ctl%i_sph_trans_ctl, 1,                      &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_sph_trans_control_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_trans_control_data(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      call reset_sph_trans_model_ctl(spt_ctl)
      call reset_sph_trans_params_ctl(spt_ctl)
!
      call reset_control_platforms(spt_ctl%plt)
      call reset_control_platforms(spt_ctl%org_plt)
      call dealloc_viz_controls(spt_ctl%viz_ctls)
!
      spt_ctl%i_sph_trans_ctl = 0
!
      end subroutine dealloc_sph_trans_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_model_ctl                               &
     &         (id_control, hd_block, spt_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(spt_ctl%i_sph_trans_model .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, spt_ctl%fld_ctl, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, spt_ctl%t_ctl, c_buf)
      end do
      spt_ctl%i_sph_trans_model = 1
!
      end subroutine read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_trans_model_ctl(spt_ctl)
!
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      call bcast_phys_data_ctl(spt_ctl%fld_ctl)
      call bcast_ctl_data_4_time_step(spt_ctl%t_ctl)
!
      call MPI_BCAST(spt_ctl%i_sph_trans_model, 1,                      &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine reset_sph_trans_model_ctl(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      call dealloc_phys_control(spt_ctl%fld_ctl)
!
      spt_ctl%i_sph_trans_model = 0
!
      end subroutine reset_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_params_ctl                              &
     &         (id_control, hd_block, spt_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(spt_ctl%i_sph_trans_params .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_integer_ctl_type(c_buf, hd_legendre_vect_len,         &
     &      spt_ctl%legendre_vector_len_ctl)
!
        call read_chara_ctl_type(c_buf, hd_zm_sph_spec_file,            &
     &      spt_ctl%zm_spec_file_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_zm_field_file, spt_ctl%zonal_udt_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_cmb_grp, spt_ctl%cmb_radial_grp_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_icb_grp, spt_ctl%icb_radial_grp_ctl)
!
        call read_chara_ctl_type(c_buf, hd_sph_transform_mode,          &
     &      spt_ctl%Legendre_trans_loop_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_FFT_package, spt_ctl%FFT_lib_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_import_mode, spt_ctl%import_mode_ctl)
!
        call read_chara_ctl_type(c_buf, hd_gauss_file_name,             &
     &      spt_ctl%gauss_sph_fhead_ctl)
      end do
      spt_ctl%i_sph_trans_params = 1
!
      end subroutine read_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_trans_params_ctl(spt_ctl)
!
      use bcast_control_arrays
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      call bcast_ctl_type_i1(spt_ctl%legendre_vector_len_ctl)
!
      call bcast_ctl_type_c1(spt_ctl%zm_spec_file_head_ctl)
      call bcast_ctl_type_c1(spt_ctl%zonal_udt_head_ctl)
      call bcast_ctl_type_c1(spt_ctl%cmb_radial_grp_ctl)
      call bcast_ctl_type_c1(spt_ctl%icb_radial_grp_ctl)
!
      call bcast_ctl_type_c1(spt_ctl%Legendre_trans_loop_ctl)
      call bcast_ctl_type_c1(spt_ctl%FFT_lib_ctl)
      call bcast_ctl_type_c1(spt_ctl%import_mode_ctl)
!
      call bcast_ctl_type_c1(spt_ctl%gauss_sph_fhead_ctl)
!
      call MPI_BCAST(spt_ctl%i_sph_trans_params, 1,                     &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      subroutine reset_sph_trans_params_ctl(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      spt_ctl%legendre_vector_len_ctl%iflag = 0
!
      spt_ctl%zm_spec_file_head_ctl%iflag = 0
      spt_ctl%zonal_udt_head_ctl%iflag = 0
      spt_ctl%cmb_radial_grp_ctl%iflag = 0
      spt_ctl%icb_radial_grp_ctl%iflag = 0
!
      spt_ctl%Legendre_trans_loop_ctl%iflag = 0
      spt_ctl%FFT_lib_ctl%iflag = 0
      spt_ctl%import_mode_ctl%iflag = 0
!
      spt_ctl%gauss_sph_fhead_ctl%iflag = 0
!
      spt_ctl%i_sph_trans_params = 0
!
      end subroutine reset_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_sph_trans
