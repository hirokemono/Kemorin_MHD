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
      character(len=kchara) :: def_org_sph_rj_head =      "sph_org/in_rj"
!>      file header for original field data
      character(len=kchara) :: def_org_ucd_header =  "field_org/out"
!>      file header for original restart data
      character(len=kchara) :: def_org_rst_header =   "rst_org/rst"
!
!>      Structure for field data IO paramters
      type(field_IO_params), save :: rj_org_param
!>      Structure for field data IO paramters
      type(field_IO_params), save :: udt_org_param
!>      Structure for original restart file  paramters
      type(field_IO_params), save :: rst_org_param
!
      private :: def_org_sph_rj_head
      private :: def_org_ucd_header, def_org_rst_header
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
!
!
      call set_file_control_params(def_org_sph_rj_head,                 &
     &    org_sph_mode_head_ctl, org_sph_file_fmt_ctl, rj_org_param)
!
      end subroutine set_control_org_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine set_control_org_rst_file_def
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
!
!
      call set_file_control_params(def_org_rst_header,                  &
     &    orginal_restart_prefix, restart_file_fmt_ctl, rst_org_param)
!
      end subroutine set_control_org_rst_file_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_org_udt_file_def
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
!
!
      call set_file_control_params(def_org_ucd_header,                  &
     &    org_udt_head_ctl, udt_file_fmt_ctl,  udt_org_param)
!
      end subroutine set_control_org_udt_file_def
!
! -----------------------------------------------------------------------
!
      subroutine set_file_control_params(default_prefix,                &
     &          file_prefix_ctl, file_format_ctl,  file_params)
!
      use t_control_elements
      use m_file_format_switch
!
      character(len = kchara), intent(in) :: default_prefix
      type(read_character_item), intent(in) :: file_prefix_ctl
      type(read_character_item), intent(in) :: file_format_ctl
!
      type(field_IO_params), intent(inout) :: file_params
!
!
      file_params%iflag_IO = file_prefix_ctl%iflag
      if(file_params%iflag_IO .gt. 0) then
        file_params%file_prefix = file_prefix_ctl%charavalue
      else
        file_params%file_prefix = default_prefix
      end if
!
      call choose_file_format                                           &
     &   (file_format_ctl, file_params%iflag_format)
!
      end subroutine set_file_control_params
!
! ----------------------------------------------------------------------
!
      end module m_control_params_2nd_files
