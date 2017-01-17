!>@file   m_ctl_data_4_org_data.f90
!!@brief  module m_ctl_data_4_org_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2008
!
!>@brief  Structure for reading parameters for original mesh
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!  begin org_data_files_def
!!      num_subdomain_ctl           2
!!      mesh_file_prefix            'mesh/in'
!!      sph_file_prefix             'sph_shell/in'
!!
!!      restart_file_prefix         'restart/rst'
!!
!!      field_file_prefix           'field/out'
!!      spectr_field_file_prefix    'sph_spectr/spectr'
!!
!!      mesh_file_fmt_ctl           'ascii'
!!      sph_file_fmt_ctl            'ascii'
!!      restart_file_fmt_ctl        'ascii'
!!      field_file_fmt_ctl          'ucd_ascii'
!!      spectr_field_fmt_ctl        'ascii'
!!  end org_data_files_def
!!
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_4_org_data
!
      use m_precision
      use t_ctl_data_4_platforms
!
      implicit  none
!
!
      type(platform_data_control), save :: org_plt
!
!  label for group entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_org_data = 'org_data_files_def'
      integer (kind=kint) :: i_org_data =      0
      private :: hd_org_data, i_org_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_org_data
!
!
        call read_control_platforms(hd_org_data, i_org_data, org_plt)
!
      end subroutine read_ctl_data_4_org_data
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_4_org_data
