!>@file   m_filter_file_names.f90
!!@brief  module m_filter_file_names
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief Names of filter files
!
      module m_filter_file_names
!
      use m_precision
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &         :: filter_3d_def_hd = 'mesh/filter_node'
      character(len=kchara), parameter                                  &
     &         :: filter_l_def_hd = 'mesh/filter_node_l'
      character(len=kchara), parameter                                  &
     &         :: filter_coef_def_hd = 'filter_coef'
      character(len=kchara), parameter                                  &
     &         :: filter_elen_def_hd = 'filter_length'
      character(len=kchara), parameter                                  &
     &         :: filter_moms_def_hd = 'filter_moms'
      character(len=kchara), parameter                                  &
     &         :: filter_wide_def_hd = 'filter_wide'
!
      character(len=kchara) :: filter_3d_head =   filter_3d_def_hd
      character(len=kchara) :: filter_line_head = filter_l_def_hd
      character(len=kchara) :: filter_coef_head = filter_coef_def_hd
      character(len=kchara) :: filter_elen_head = filter_elen_def_hd
      character(len=kchara) :: filter_moms_head = filter_moms_def_hd
      integer(kind=kint) :: ifmt_3d_filter =   0
      integer(kind=kint) :: ifmt_line_filter = 0
      integer(kind=kint) :: ifmt_filter_coef = 0
      integer(kind=kint) :: ifmt_filter_elen = 0
      integer(kind=kint) :: ifmt_filter_moms = 0
!
      character(len=kchara) :: filter_wide_head = filter_wide_def_hd
      integer(kind=kint) :: ifmt_wide_filter = 0
!
      integer(kind=kint) :: ifmt_filter_file =   0
      character(len=kchara) :: filter_file_head
      integer(kind=kint), parameter :: filter_file_code = 18
      integer(kind=kint), parameter :: filter_coef_code = 16
      integer(kind=kint), parameter :: org_filter_coef_code = 17
!
      end module m_filter_file_names
