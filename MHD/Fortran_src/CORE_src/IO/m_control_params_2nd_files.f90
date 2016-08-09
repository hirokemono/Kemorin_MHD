!>@file   m_control_params_2nd_files.f90
!!@brief  module m_control_params_2nd_files
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief Array for the second mesh data
!!
!!@verbatim
!!      subroutine set_control_org_sph_mesh
!!      subroutine set_control_org_rst_file_def
!!      subroutine set_control_org_udt_file_def
!!@endverbatim
!
      module m_control_params_2nd_files
!
      use m_precision
      use m_constants
      use t_field_data_IO
!
      implicit  none
!
!>      file header for original spectrum indexing data
      character(len=kchara) :: org_sph_rj_head =      "sph_org/in_rj"
!>      file header for original field data
      character(len=kchara) :: org_ucd_header =  "field_org/out"
!>      file header for original restart data
      character(len=kchara) :: org_rst_header =   "rst_org/rst"
!
!>      Structure for field data IO paramters
      type(field_IO_params), save :: rj_org_param
!>      Structure for field data IO paramters
      type(field_IO_params), save :: udt_org_param
!>      Structure for original restart file  paramters
      type(field_IO_params), save :: rst_org_param
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_org_sph_mesh
!
      use m_ctl_data_4_org_data
      use m_read_mesh_data
      use m_node_id_spherical_IO
      use m_file_format_switch
!
!
      rj_org_param%iflag_IO = org_sph_mode_head_ctl%iflag
      if(rj_org_param%iflag_IO .gt. 0) then
        rj_org_param%file_prefix = org_sph_mode_head_ctl%charavalue
      else
        rj_org_param%file_prefix = org_sph_rj_head
      end if
!
      call choose_file_format                                           &
     &   (org_sph_file_fmt_ctl, rj_org_param%iflag_format)
!
      end subroutine set_control_org_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine set_control_org_rst_file_def
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_field_file_format
      use m_file_format_switch
!
!
      rst_org_param%iflag_IO = orginal_restart_prefix%iflag
      if(rst_org_param%iflag_IO .gt. 0) then
        rst_org_param%file_prefix = orginal_restart_prefix%charavalue
      else
        rst_org_param%file_prefix = org_rst_header
      end if
!
      call choose_ucd_file_format(restart_file_fmt_ctl%charavalue,      &
     &    restart_file_fmt_ctl%iflag, rst_org_param%iflag_format)
!
      end subroutine set_control_org_rst_file_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_org_udt_file_def
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_field_file_format
      use m_file_format_switch
!
!
      udt_org_param%iflag_IO = org_udt_head_ctl%iflag
      if (org_udt_head_ctl%iflag .gt. 0) then
        udt_org_param%file_prefix = org_udt_head_ctl%charavalue
      else
        udt_org_param%file_prefix = org_ucd_header
      end if
!
      call choose_ucd_file_format(udt_file_fmt_ctl%charavalue,          &
     &    udt_file_fmt_ctl%iflag, udt_org_param%iflag_format)
!
      end subroutine set_control_org_udt_file_def
!
! -----------------------------------------------------------------------
!
      end module m_control_params_2nd_files
