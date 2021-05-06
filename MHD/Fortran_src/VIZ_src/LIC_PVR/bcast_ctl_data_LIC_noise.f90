!>@file   bcast_ctl_data_LIC_noise.f90
!!@brief  module bcast_ctl_data_LIC_noise
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for noise configration for LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine bcast_cube_noise_control_data(noise_ctl)
!!        type(cube_noise_ctl), intent(inout) :: noise_ctl
!!      subroutine copy_cube_noise_control_data(org_noise_c, new_noise_c)
!!        type(cube_noise_ctl), intent(in) :: org_noise_c
!!        type(cube_noise_ctl), intent(inout) :: new_noise_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags  (Not used currently)
!!    noise_type:             'external_file' or 'random'
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin cube_noise_ctl
!!    noise_type             'external_file'
!!    noise_file_prefix      'noise/noise_64'
!!    hd_noise_file_fmt      'gzip'
!!
!!    noise_resolution          256
!!    noise_step_size            20
!!
!!    noise_cube_size          0.4
!!    noise_delta_x            0.01
!!  end cube_noise_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module bcast_ctl_data_LIC_noise
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_control_data_LIC_noise
      use skip_comment_f
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_cube_noise_control_data(noise_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(cube_noise_ctl), intent(inout) :: noise_ctl
!
!
      call calypso_mpi_bcast_one_int(noise_ctl%i_cube_noise_control, 0)
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
      subroutine copy_cube_noise_control_data(org_noise_c, new_noise_c)
!
      type(cube_noise_ctl), intent(in) :: org_noise_c
      type(cube_noise_ctl), intent(inout) :: new_noise_c
!
!
      new_noise_c%i_cube_noise_control                                  &
     &     = org_noise_c%i_cube_noise_control
!
      call copy_chara_ctl(org_noise_c%noise_type_ctl,                   &
     &                    new_noise_c%noise_type_ctl)
      call copy_chara_ctl(org_noise_c%noise_file_name_ctl,              &
     &                    new_noise_c%noise_file_name_ctl)
      call copy_chara_ctl(org_noise_c%noise_file_format_ctl,            &
     &                    new_noise_c%noise_file_format_ctl)
      call copy_integer_ctl(org_noise_c%noise_resolution_ctl,           &
     &                      new_noise_c%noise_resolution_ctl)
      call copy_integer_ctl(org_noise_c%noise_stepping_ctl,             &
     &                      new_noise_c%noise_stepping_ctl)
      call copy_real_ctl(org_noise_c%noise_cube_size_ctl,               &
     &                   new_noise_c%noise_cube_size_ctl)
      call copy_real_ctl(org_noise_c%noise_deltax_ctl,                  &
     &                   new_noise_c%noise_deltax_ctl)
!
      end subroutine copy_cube_noise_control_data
!
!  ---------------------------------------------------------------------
!
      end module bcast_ctl_data_LIC_noise
