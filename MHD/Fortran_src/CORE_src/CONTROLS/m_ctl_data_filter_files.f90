!>@file   m_ctl_data_filter_files.f90
!!@brief  module m_ctl_data_filter_files
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief  Structure for reading parameters for filtering files
!!
!!@verbatim
!!      subroutine read_filter_fnames_ctl
!!
!!  ---------------------------------------------------------------------
!!
!!      begin filter_files_def
!!        filter_file_header           'filter/filter_node'
!!        filter_elength_header        'filter/filter_elength'
!!        filter_moment_header         'filter/filter_moms'
!!        filter_coefs_header          'filter/filter_coef'
!!        wider_filter_header          'filter/filter_coef_2'
!!
!!        filter_elen_format        'ascii'
!!        filter_3d_format          'binary'
!!        filter_wide_format        'gzip'
!!
!!        model_coef_ini_header    'model_coefs_ini'
!!      end filter_files_def
!!
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_filter_files
!
      use m_precision
      use t_ctl_data_filter_files
!
      implicit  none
!
!
!>      Structure for filtering files
      type(filter_file_control), save :: ffile_ctl1
!
!    label for entry
!
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
      integer (kind=kint) :: i_filter_fnames = 0
!
      private :: hd_filter_fnames, i_filter_fnames
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_fnames_ctl
!
!
      call read_filter_fnames_control                                   &
     &   (hd_filter_fnames, i_filter_fnames, ffile_ctl1)
!
      end subroutine read_filter_fnames_ctl
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_filter_files
