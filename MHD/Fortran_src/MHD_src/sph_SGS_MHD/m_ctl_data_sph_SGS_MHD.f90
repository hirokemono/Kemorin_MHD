!>@file   m_ctl_data_sph_SGS_MHD.f90
!!@brief  module m_ctl_data_sph_SGS_MHD
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!
      module m_ctl_data_sph_SGS_MHD
!
      use m_precision
!
      use t_ctl_data_SGS_MHD
!
      implicit none
!
!
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!
!>      Control struture for MHD simulation
      type(sph_sgs_mhd_control), save :: MHD_ctl1
!
      end module m_ctl_data_sph_SGS_MHD
