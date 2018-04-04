!>@file   t_control_data_LIC.f90
!!@brief  module t_control_data_LIC
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_lic_control_data(hd_lic_ctl, lic_ctl)
!!      subroutine reset_lic_control_flags(lic_ctl)
!!      subroutine bcast_lic_control_data(lic_ctl)
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin LIC_ctl
!!    LIC_field    magnetic_field
!!
!!    color_field         magnetic_field
!!    color_component     amplitude
!!
!!    opacity_field       magnetic_field
!!    opacity_component   amplitude
!!
!!    source_reference_field        magnetic_field
!!    source_reference_component    magnetic_field
!!    source_minimum              0.5
!!    source_maximum              0.5
!!
!!    noise_type             external_file
!!    noise_file_name        'noise/noise_64'
!!    noise_frequency          2.0
!!
!!    kernel_function_type   external_file
!!    kernal_file_name       'kernel.dat'
!!
!!    LIC_trace_length        0.5
!!
!!    normalization_type     'constant'
!!    normalization_value     20.0
!!
!!    reflection_reference   'noise_file'
!!    ref_noise_file_name    "noise/noise-256.grd"
!!  end LIC_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_LIC
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_read_control_elements
      use t_control_elements
      use t_read_control_arrays
      use skip_comment_f
!
      implicit  none
!
!
      type lic_parameter_ctl
        type(read_character_item) :: LIC_field_ctl
!
        type(read_character_item) :: color_field_ctl
        type(read_character_item) :: color_component_ctl
        type(read_character_item) :: opacity_field_ctl
        type(read_character_item) :: opacity_component_ctl
!
!
        type(read_character_item) :: source_ref_field_ctl
        type(read_character_item) :: source_ref_component_ctl
        type(read_real_item) ::       source_minimum_ctl
        type(read_real_item) ::       source_maximum_ctl
!
        type(read_character_item) :: noise_type_ctl
        type(read_character_item) :: noise_file_name_ctl
        type(read_real_item) ::      noise_frequency_ctl
!
        type(read_character_item) :: kernel_function_type_ctl
        type(read_character_item) :: kernal_file_name_ctl
        type(read_real_item) ::      LIC_trace_length_ctl
!
        type(read_character_item) :: normalization_type_ctl
        type(read_real_item) ::      normalization_value_ctl
!
        type(read_character_item) :: reflection_ref_type_ctl
        type(read_character_item) :: ref_noise_file_name_ctl
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_lic_control = 0
      end type lic_parameter_ctl
!
!
!     3rd level for LIC_ctl
!
      character(len=kchara) :: hd_LIC_field =   'LIC_field'
!
      character(len=kchara) :: hd_color_field =     'color_field'
      character(len=kchara) :: hd_color_component = 'color_component'
      character(len=kchara) :: hd_opacity_field =   'opacity_field'
      character(len=kchara) :: hd_opacity_component                     &
     &                        = 'opacity_component'
!
      character(len=kchara) :: hd_source_reference_field                &
     &                        = 'source_reference_field'
      character(len=kchara) :: hd_source_reference_comp                 &
     &                        = 'source_reference_component'
      character(len=kchara) :: hd_source_minimum = 'source_minimum'
      character(len=kchara) :: hd_source_maximum = 'source_maximum'
!
      character(len=kchara) :: hd_noise_type =      'noise_type'
      character(len=kchara) :: hd_noise_file_name = 'noise_file_name'
      character(len=kchara) :: hd_noise_frequency = 'noise_frequency'
!
      character(len=kchara) :: hd_kernel_function_type                  &
     &                        = 'kernel_function_type'
      character(len=kchara) :: hd_kernal_file_name = 'kernal_file_name'
      character(len=kchara) :: hd_LIC_trace_length = 'LIC_trace_length'
!
      character(len=kchara) :: hd_normalization_type                    &
     &                        = 'normalization_type'
      character(len=kchara) :: hd_normalization_value                   &
     &                        = 'normalization_value'
!
      character(len=kchara) :: hd_reflection_ref_type                   &
     &                        = 'reflection_reference'
      character(len=kchara) :: hd_ref_noise_file_name                   &
     &                        = 'ref_noise_file_name'
!
      private :: hd_LIC_field, hd_color_field, hd_color_component
      private :: hd_opacity_field, hd_opacity_component
      private :: hd_source_reference_field, hd_source_minimum
      private :: hd_source_reference_comp, hd_source_maximum
      private :: hd_noise_type, hd_noise_file_name, hd_noise_frequency
      private :: hd_kernel_function_type, hd_kernal_file_name
      private :: hd_LIC_trace_length
      private :: hd_normalization_type, hd_normalization_value
      private :: hd_reflection_ref_type, hd_ref_noise_file_name
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_lic_control_data(hd_lic_ctl, lic_ctl)
!
      character(len = kchara), intent(in) :: hd_lic_ctl
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      if(right_begin_flag(hd_lic_ctl) .eq. 0) return
      if (lic_ctl%i_lic_control.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag                                      &
     &     (hd_lic_ctl, lic_ctl%i_lic_control)
        if(lic_ctl%i_lic_control .gt. 0) exit
!
!
        call read_chara_ctl_type                                        &
     &     (hd_LIC_field, lic_ctl%LIC_field_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_color_field, lic_ctl%color_field_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_color_component, lic_ctl%color_component_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_opacity_field, lic_ctl%opacity_field_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_opacity_component, lic_ctl%opacity_component_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_source_reference_field, lic_ctl%source_ref_field_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_source_reference_comp, lic_ctl%source_ref_component_ctl)
        call read_real_ctl_type                                         &
     &     (hd_source_minimum, lic_ctl%source_minimum_ctl)
        call read_real_ctl_type                                         &
     &     (hd_source_maximum, lic_ctl%source_maximum_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_noise_type, lic_ctl%noise_type_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_noise_file_name, lic_ctl%noise_file_name_ctl)
        call read_real_ctl_type                                         &
     &     (hd_noise_frequency, lic_ctl%noise_frequency_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_kernel_function_type, lic_ctl%kernel_function_type_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_kernal_file_name, lic_ctl%kernal_file_name_ctl)
        call read_real_ctl_type                                         &
     &     (hd_LIC_trace_length, lic_ctl%LIC_trace_length_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_normalization_type, lic_ctl%normalization_type_ctl)
        call read_real_ctl_type                                         &
     &     (hd_normalization_value, lic_ctl%normalization_value_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_reflection_ref_type, lic_ctl%reflection_ref_type_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_ref_noise_file_name, lic_ctl%ref_noise_file_name_ctl)
      end do
!
      end subroutine read_lic_control_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_lic_control_flags(lic_ctl)
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      lic_ctl%LIC_field_ctl%iflag = 0
!
      lic_ctl%color_field_ctl%iflag =       0
      lic_ctl%color_component_ctl%iflag =   0
      lic_ctl%opacity_field_ctl%iflag =     0
      lic_ctl%opacity_component_ctl%iflag = 0
!
      lic_ctl%source_ref_field_ctl%iflag =     0
      lic_ctl%source_ref_component_ctl%iflag = 0
      lic_ctl%source_minimum_ctl%iflag =       0
      lic_ctl%source_maximum_ctl%iflag =       0
!
      lic_ctl%noise_type_ctl%iflag =      0
      lic_ctl%noise_file_name_ctl%iflag = 0
      lic_ctl%noise_frequency_ctl%iflag = 0
!
      lic_ctl%kernel_function_type_ctl%iflag = 0
      lic_ctl%kernal_file_name_ctl%iflag =     0
      lic_ctl%LIC_trace_length_ctl%iflag =     0
!
      lic_ctl%normalization_type_ctl%iflag =   0
      lic_ctl%normalization_value_ctl%iflag =  0
!
      lic_ctl%reflection_ref_type_ctl%iflag =  0
      lic_ctl%ref_noise_file_name_ctl%iflag =  0
!
      lic_ctl%i_lic_control = 0
!
      end subroutine reset_lic_control_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_lic_control_data(lic_ctl)
!
      use bcast_control_arrays
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      call MPI_BCAST(lic_ctl%i_lic_control,  ione,                      &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_c1(lic_ctl%LIC_field_ctl)
      call bcast_ctl_type_c1(lic_ctl%color_field_ctl)
      call bcast_ctl_type_c1(lic_ctl%color_component_ctl)
      call bcast_ctl_type_c1(lic_ctl%opacity_field_ctl)
      call bcast_ctl_type_c1(lic_ctl%opacity_component_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%source_ref_field_ctl)
      call bcast_ctl_type_c1(lic_ctl%source_ref_component_ctl)
      call bcast_ctl_type_r1(lic_ctl%source_minimum_ctl)
      call bcast_ctl_type_r1(lic_ctl%source_maximum_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%noise_type_ctl)
      call bcast_ctl_type_c1(lic_ctl%noise_file_name_ctl)
      call bcast_ctl_type_r1(lic_ctl%noise_frequency_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%kernel_function_type_ctl)
      call bcast_ctl_type_c1(lic_ctl%kernal_file_name_ctl)
      call bcast_ctl_type_r1(lic_ctl%LIC_trace_length_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%normalization_type_ctl)
      call bcast_ctl_type_r1(lic_ctl%normalization_value_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%reflection_ref_type_ctl)
      call bcast_ctl_type_c1(lic_ctl%ref_noise_file_name_ctl)
!
      end subroutine bcast_lic_control_data
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_LIC
