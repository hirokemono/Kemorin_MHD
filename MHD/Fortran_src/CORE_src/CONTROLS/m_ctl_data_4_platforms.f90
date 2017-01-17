!>@file   m_ctl_data_4_platforms.f90
!!@brief  module m_ctl_data_4_platforms
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Control input routine for data file headers
!!
!!@verbatim
!!      subroutine read_ctl_data_4_platform
!!
!! ------------------------------------------------------------------
!!      Example of control parameters
!!
!!    begin data_files_def
!!      debug_flag_ctl            'ON'
!!
!!      num_subdomain_ctl           2
!!      num_smp_ctl                 4
!!
!!      mesh_file_prefix            'mesh/in'
!!
!!      sph_file_prefix             'sph_shell/in'
!!
!!      coriolis_int_file_name      'sph_shell/rot_int.dat'
!!      boundary_data_file_name     'bc_spec.dat'
!!
!!      interpolate_sph_to_fem_ctl  'sph_shell/sph_to_fem'
!!      interpolate_fem_to_sph_ctl  'sph_shell/fem_to_sph'
!!
!!      field_file_prefix           'field/out'
!!      restart_file_prefix         'restart/rst'
!!
!!      spectr_field_file_prefix    'sph_spectr/spectr'
!!
!!
!!      mesh_file_fmt_ctl           'ascii'
!!      restart_file_fmt_ctl        'ascii'
!!      field_file_fmt_ctl          'ucd_ascii'
!!      sph_file_fmt_ctl            'ascii'
!!      spectr_field_fmt_ctl        'ascii'
!!      itp_file_fmt_ctl            'ascii'
!!      coriolis_file_fmt_ctl       'ascii'
!!
!!      memory_conservation_ctl     'YES'
!!      FEM_mesh_output_switch      'NO'
!!    end data_files_def
!! ------------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_4_platforms
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_4_platforms
!
      implicit  none
!
!
!>      Structure for file settings
      type(platform_data_control), save :: plt1
!
!     Label for the entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      integer (kind=kint) :: i_platform =   0
!
      private :: hd_platform, i_platform
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_platform
!
!
        call read_control_platforms(hd_platform, i_platform, plt1)
!
      end subroutine read_ctl_data_4_platform
!
!  ---------------------------------------------------------------------
!
      end module  m_ctl_data_4_platforms
