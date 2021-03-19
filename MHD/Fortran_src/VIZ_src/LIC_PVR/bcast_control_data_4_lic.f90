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
!!      subroutine dup_lic_control_data(org_lic_c, new_lic_c)
!!        type(lic_parameter_ctl), intent(in) :: org_lic_c
!!        type(lic_parameter_ctl), intent(inout) :: new_lic_c
!!      subroutine bcast_lic_control_data(lic_ctl)
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
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
      use t_control_data_LIC
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_lic_control_data(lic_ctl)
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_data_LIC_masking
      use t_control_data_LIC_noise
      use t_control_data_LIC_kernel
!
      use calypso_mpi_int
      use bcast_control_arrays
      use bcast_ctl_data_LIC_noise
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
      integer(kind = kint) :: i
!
!
      call calypso_mpi_bcast_one_int(lic_ctl%i_lic_control, 0)
!
      call bcast_ctl_type_c1(lic_ctl%LIC_field_ctl)
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
!
      if(my_rank .ne. 0) call alloc_lic_masking_ctl(lic_ctl)
      do i = 1, lic_ctl%num_masking_ctl
        call bcast_lic_masking_ctl_data(lic_ctl%mask_ctl(i))
      end do
!
      end subroutine bcast_lic_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine dup_lic_control_data(org_lic_c, new_lic_c)
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_data_LIC_masking
      use t_control_data_LIC_noise
      use t_control_data_LIC_kernel
!
      use bcast_ctl_data_LIC_noise
!
      type(lic_parameter_ctl), intent(in) :: org_lic_c
      type(lic_parameter_ctl), intent(inout) :: new_lic_c
!
!
      new_lic_c%i_lic_control = org_lic_c%i_lic_control
!
      call copy_chara_ctl(org_lic_c%LIC_field_ctl,                      &
     &                    new_lic_c%LIC_field_ctl)
      call copy_chara_ctl(org_lic_c%color_field_ctl,                    &
     &                    new_lic_c%color_field_ctl)
      call copy_chara_ctl(org_lic_c%color_component_ctl,                &
     &                    new_lic_c%color_component_ctl)
      call copy_chara_ctl(org_lic_c%opacity_field_ctl,                  &
     &                    new_lic_c%opacity_field_ctl)
      call copy_chara_ctl(org_lic_c%opacity_component_ctl,              &
     &                    new_lic_c%opacity_component_ctl)
!
      call copy_chara_ctl(org_lic_c%vr_sample_mode_ctl,                 &
     &                    new_lic_c%vr_sample_mode_ctl)
      call copy_real_ctl(org_lic_c%step_size_ctl,                       &
     &                   new_lic_c%step_size_ctl)
!
      call copy_chara_ctl(org_lic_c%normalization_type_ctl,             &
     &                    new_lic_c%normalization_type_ctl)
      call copy_real_ctl(org_lic_c%normalization_value_ctl,             &
     &                    new_lic_c%normalization_value_ctl)
!
      call copy_cube_noise_control_data(org_lic_c%noise_ctl,            &
     &    new_lic_c%noise_ctl)
      call copy_kernel_control_data(org_lic_c%kernel_ctl,               &
     &    new_lic_c%kernel_ctl)
!
      new_lic_c%num_masking_ctl = org_lic_c%num_masking_ctl
      if(new_lic_c%num_masking_ctl .gt. 0) then
        allocate(new_lic_c%mask_ctl(new_lic_c%num_masking_ctl))
        call dup_lic_masking_ctls(org_lic_c%num_masking_ctl,            &
     &      org_lic_c%mask_ctl, new_lic_c%mask_ctl)
      end if
!
      end subroutine dup_lic_control_data
!
!  ---------------------------------------------------------------------
!
      end module bcast_control_data_4_lic
