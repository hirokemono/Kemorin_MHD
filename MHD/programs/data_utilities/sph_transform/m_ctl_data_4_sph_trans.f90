!
!      module m_ctl_data_4_sph_trans
!
!        programmed by H.Matsui on Oct., 2007
!
!      subroutine read_control_data_sph_trans
!
      module m_ctl_data_4_sph_trans
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
      character(len = kchara) :: Legendre_trans_loop_ctl
      character(len = kchara) :: FFT_library_ctl
!
      character(len = kchara) :: zm_spec_file_head_ctl
      character(len = kchara) :: zonal_udt_head_ctl
!
      character(len=kchara) :: cmb_radial_grp_ctl =  'CMB'
      character(len=kchara) :: icb_radial_grp_ctl =  'ICB'
      character(len=kchara) :: gauss_sph_fhead_ctl
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
      character(len=kchara), parameter :: hd_sph_transform_mode         &
     &                        =  'Legendre_trans_loop_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_FFT_package =  'FFT_library_ctl'
!
      character(len=kchara), parameter :: hd_zm_sph_spec_file           &
     &                        =  'zm_spectr_head_ctl'
      character(len=kchara), parameter :: hd_zm_field_file              &
     &                        =  'zonal_udt_head_ctl'
!
      character(len=kchara), parameter :: hd_cmb_grp =                  &
     &                        'radial_CMB_group_name'
      character(len=kchara), parameter :: hd_icb_grp =                  &
     &                        'radial_ICB_group_name'
      character(len=kchara), parameter :: hd_gauss_file_name            &
     &                        = 'sph_gauss_coefs_head_ctl'
!
      integer (kind=kint) :: i_sph_transform_mode =  0
      integer (kind=kint) :: i_FFT_package =         0
      integer (kind=kint) :: i_zm_sph_spec_file =    0
      integer (kind=kint) :: i_zm_field_file =       0
!
      integer (kind=kint) :: i_cmb_grp =         0
      integer (kind=kint) :: i_icb_grp =         0
      integer (kind=kint) :: i_gauss_file_name = 0
!
      private :: control_file_code, control_file_name
      private :: hd_sph_trans_ctl, i_sph_trans_ctl
      private :: hd_sph_trans_model, i_sph_trans_model
      private :: hd_FFT_package
      private :: hd_sph_trans_params, i_sph_trans_params
      private :: hd_zm_sph_spec_file, hd_zm_field_file
      private :: hd_sph_transform_mode
      private :: hd_cmb_grp, hd_icb_grp, hd_gauss_file_name
      private :: read_sph_trans_control_data, read_sph_trans_params_ctl
      private :: read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_sph_trans
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
      end subroutine read_control_data_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_control_data
!
      use calypso_mpi
      use m_parallel_var_dof
      use m_machine_parameter
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_control_data_pvrs
!
      integer(kind = kint) :: ierr
!
!   2 begin phys_values_ctl
!
      ierr = 0
      if(right_begin_flag(hd_sph_trans_ctl) .eq. 0) return
      if (i_sph_trans_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sph_trans_ctl, i_sph_trans_ctl)
        if(i_sph_trans_ctl .gt. 0) exit
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_org_data
!
        call read_sph_trans_model_ctl
        call read_sph_trans_params_ctl
!
        call read_viz_control_data(ierr)
        if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
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
        call read_phys_values
        call read_time_step_ctl
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
        call read_character_ctl_item(hd_sph_transform_mode,             &
     &          i_sph_transform_mode, Legendre_trans_loop_ctl)
        call read_character_ctl_item(hd_FFT_package,                    &
     &          i_FFT_package, FFT_library_ctl)
        call read_character_ctl_item(hd_zm_sph_spec_file,               &
     &          i_zm_sph_spec_file, zm_spec_file_head_ctl)
        call read_character_ctl_item(hd_zm_field_file,                  &
     &          i_zm_field_file, zonal_udt_head_ctl)
        call read_character_ctl_item(hd_cmb_grp,                        &
     &          i_cmb_grp, cmb_radial_grp_ctl)
!
        call read_character_ctl_item(hd_icb_grp,                        &
     &          i_icb_grp, icb_radial_grp_ctl)
!
        call read_character_ctl_item(hd_gauss_file_name,                &
     &          i_gauss_file_name, gauss_sph_fhead_ctl)
      end do
!
      end subroutine read_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_sph_trans
