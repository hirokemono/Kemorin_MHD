!>@file   bcast_control_data_4_lic.f90
!!@brief  module bcast_control_data_4_lic
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine bcast_lic_control_data(lic_ctl)
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!      subroutine bcast_kernel_control_data(kernel_ctl)
!!        type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags
!!
!!    vr_sample_mode:         'fixed_size' or 'element_count'
!!    normalization_type:     'set_by_control' or 'set_by_range'
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin LIC_ctl
!!    LIC_field    magnetic_field
!!
!!    color_field         magnetic_field
!!    color_component     amplitude
!!
!!!    opacity_field       magnetic_field
!!!    opacity_component   amplitude
!!
!!    array masking_control    1
!!      begin masking_control
!!        masking_field        magnetic_field
!!        masking_component    magnetic_field
!!        array masking_range      1
!!          masking_range       0.5    0.8
!!          ...
!!        end array masking_range
!!      end masking_control
!!      ...
!!    end array masking_control
!!
!!    begin cube_noise_ctl
!!      ...
!!    end cube_noise_ctl
!!
!!    begin kernel_ctl
!!      ...
!!    end kernel_ctl
!!
!!    vr_sample_mode         'fixed_size'
!!    step_size              0.005
!!
!!    normalization_type     'set_by_control'
!!    normalization_value     20.0
!!  end LIC_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module bcast_control_data_4_lic
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
!
      implicit  none
!
      private :: bcast_kernel_control_data
      private :: bcast_cube_noise_control_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_lic_control_data(lic_ctl)
!
      use t_control_data_LIC
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_data_masking
      use t_control_data_LIC_noise
      use t_control_data_LIC_kernel
      use t_ctl_data_volume_repart
!
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_ctl_data_vol_repart
      use bcast_control_arrays
      use transfer_to_long_integers
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
      integer(kind = kint) :: i
!
!
      call calypso_mpi_bcast_one_int(lic_ctl%i_lic_control, 0)
      call calypso_mpi_bcast_character(lic_ctl%block_name,              &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_character(lic_ctl%fname_LIC_kernel_ctl,    &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_character(lic_ctl%fname_LIC_noise_ctl,     &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_character(lic_ctl%fname_vol_repart_ctl,    &
     &                                 cast_long(kchara), 0)
!
      call bcast_ctl_type_c1(lic_ctl%LIC_field_ctl)
      call bcast_ctl_type_c1(lic_ctl%subdomain_elapsed_dump_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%color_field_ctl)
      call bcast_ctl_type_c1(lic_ctl%color_component_ctl)
      call bcast_ctl_type_c1(lic_ctl%opacity_field_ctl)
      call bcast_ctl_type_c1(lic_ctl%opacity_component_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%vr_sample_mode_ctl)
      call bcast_ctl_type_r1(lic_ctl%step_size_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%normalization_type_ctl)
      call bcast_ctl_type_r1(lic_ctl%normalization_value_ctl)
!
      call calypso_mpi_bcast_one_int(lic_ctl%num_masking_ctl, 0)
!
      call bcast_cube_noise_control_data(lic_ctl%noise_ctl)
      call bcast_kernel_control_data(lic_ctl%kernel_ctl)
      call bcast_control_vol_repart(lic_ctl%repart_ctl)
!
      if(my_rank .ne. 0) call alloc_lic_masking_ctl(lic_ctl)
      do i = 1, lic_ctl%num_masking_ctl
        call bcast_masking_ctl_data(lic_ctl%mask_ctl(i))
      end do
!
      end subroutine bcast_lic_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_kernel_control_data(kernel_ctl)
!
      use t_control_data_LIC_kernel
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!
!
      call calypso_mpi_bcast_one_int(kernel_ctl%i_kernel_control, 0)
!
      call bcast_ctl_type_c1(kernel_ctl%kernel_type_ctl)
      call bcast_ctl_type_c1(kernel_ctl%trace_length_mode_ctl)
!
      call bcast_ctl_type_i1(kernel_ctl%kernel_resolution_ctl)
      call bcast_ctl_type_i1(kernel_ctl%max_trace_count_ctl)
!
      call bcast_ctl_type_r1(kernel_ctl%kernel_sigma_ctl)
      call bcast_ctl_type_r1(kernel_ctl%kernel_peak_ctl)
      call bcast_ctl_type_r1(kernel_ctl%half_length_ctl)
!
      end subroutine bcast_kernel_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_cube_noise_control_data(noise_ctl)
!
      use t_control_data_LIC_noise
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
      use transfer_to_long_integers
!
      type(cube_noise_ctl), intent(inout) :: noise_ctl
!
!
      call calypso_mpi_bcast_one_int(noise_ctl%i_cube_noise_control, 0)
      call calypso_mpi_bcast_character(noise_ctl%block_name,            &
     &                                 cast_long(kchara), 0)
!
      call bcast_ctl_type_c1(noise_ctl%noise_type_ctl)
      call bcast_ctl_type_c1(noise_ctl%noise_file_name_ctl)
      call bcast_ctl_type_c1(noise_ctl%noise_file_format_ctl)
!
      call bcast_ctl_type_i1(noise_ctl%noise_resolution_ctl)
      call bcast_ctl_type_i1(noise_ctl%noise_stepping_ctl)
!
      call bcast_ctl_type_r1(noise_ctl%noise_cube_size_ctl)
      call bcast_ctl_type_r1(noise_ctl%noise_deltax_ctl)
!
      end subroutine bcast_cube_noise_control_data
!
!  ---------------------------------------------------------------------
!
      end module bcast_control_data_4_lic
