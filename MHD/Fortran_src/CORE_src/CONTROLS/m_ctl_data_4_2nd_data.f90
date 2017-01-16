!>@file   m_ctl_data_4_2nd_data.f90
!!@brief  module m_ctl_data_4_2nd_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2008
!
!>@brief  Control data for new file settings
!!
!!@verbatim
!!      subroutine read_ctl_data_4_new_data
!!
!!  ---------------------------------------------------------------------
!!
!!  begin new_data_files_def
!!    num_subdomain_ctl           2
!!
!!    mesh_file_prefix            'mesh_target/in'
!!    sph_file_prefix             'sph_new/in_rj'
!!
!!    restart_file_prefix         'rst_new/rst'
!!
!!    field_file_prefix          'field_new/out'
!!    spectr_field_file_prefix   'spectr_new/spectr'
!!
!!    mesh_file_fmt_ctl              'ascii'
!!    restart_file_fmt_ctl           'ascii'
!!    field_file_fmt_ctl             'ascii'
!!    sph_file_fmt_ctl               'ascii'
!!    spectr_field_fmt_ctl           'merged_gz'
!!
!!    delete_original_data_flag       'YES'
!!  end new_data_files_def
!!
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_4_2nd_data
!
      use m_precision
      use t_ctl_data_4_platforms
!
      implicit  none
!
!
      type(platform_data_control), save :: new_plt
!
!  label for group entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      integer (kind=kint) :: i_new_data =      0
      private :: hd_new_data, i_new_data
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_new_data
!
!
        call read_control_platforms(hd_new_data, i_new_data, new_plt)
!
      end subroutine read_ctl_data_4_new_data
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_4_2nd_data
