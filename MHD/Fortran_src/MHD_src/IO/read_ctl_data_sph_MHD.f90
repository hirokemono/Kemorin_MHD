!>@file   read_ctl_data_sph_MHD.f90
!!@brief  module read_ctl_data_sph_MHD
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
!!@verbatim
!!      subroutine read_sph_mhd_model
!!      subroutine read_sph_mhd_control
!!
!!      subroutine bcast_sph_mhd_model
!!      subroutine bcast_sph_mhd_control
!!@endverbatim
!
      module read_ctl_data_sph_MHD
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_MHD_model
      use t_ctl_data_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
!
      use skip_comment_f
!
      implicit none
!
!
!>      Control structure for orginal file informations
      type(platform_data_control), save :: org_plt1
!
!>      Control structure for MHD/model
      type(mhd_model_control), save :: model_ctl1
!>      Control structure for MHD/control
      type(mhd_control_control), save :: ctl_ctl1
!
!>      Structure for spectr monitoring control
      type(sph_monitor_control), save :: smonitor_ctl1
!>      Structure for monitoring plave list
      type(node_monitor_control), save :: nmtr_ctl1
!
      end module read_ctl_data_sph_MHD
