!>@file   set_ctl_params_2nd_files.f90
!!@brief  module set_ctl_params_2nd_files
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief Array for the second mesh data
!!
!!@verbatim
!!      subroutine set_control_org_sph_mesh(rj_file_param)
!!      subroutine set_control_org_rst_file_def(rst_file_param)
!!      subroutine set_control_org_udt_file_def(udt_file_param)
!!
!!      subroutine set_control_new_mesh_file_def(mesh_file)
!!
!!      subroutine set_file_control_params(default_prefix,              &
!!     &          file_prefix_ctl, file_format_ctl, file_params)
!!        type(read_character_item), intent(in) :: file_prefix_ctl
!!        type(read_character_item), intent(in) :: file_format_ctl
!!        type(field_IO_params), intent(inout) :: file_params
!!@endverbatim
!
      module set_ctl_params_2nd_files
!
      use m_precision
      use m_constants
      use t_file_IO_parameter
      use set_control_platform_data
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
!>      file header for new mesh data
      character(len=kchara), parameter                                  &
     &           :: def_new_mesh_head = 'mesh_target/in'
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
      subroutine set_control_org_sph_mesh(rj_file_param)
!
      use m_ctl_data_4_org_data
      use m_read_mesh_data
!
      type(field_IO_params), intent(inout) :: rj_file_param
!
!
      call set_file_control_params(def_org_sph_rj_head,                 &
     &    org_sph_mode_head_ctl, org_sph_file_fmt_ctl, rj_file_param)
!
      end subroutine set_control_org_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine set_control_org_rst_file_def(rst_file_param)
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
!
      type(field_IO_params), intent(inout) :: rst_file_param
!
!
      call set_file_control_params(def_org_rst_header,                  &
     &    orginal_restart_prefix, restart_file_fmt_ctl, rst_file_param)
!
      end subroutine set_control_org_rst_file_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_org_udt_file_def(udt_file_param)
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
!
      type(field_IO_params), intent(inout) :: udt_file_param
!
!
      call set_file_control_params(def_org_ucd_header,                  &
     &    org_udt_head_ctl, udt_file_fmt_ctl,  udt_file_param)
!
      end subroutine set_control_org_udt_file_def
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_new_mesh_file_def(mesh_file)
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
!
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_file_control_params(def_new_mesh_head,                   &
     &    new_mesh_prefix, new_mesh_file_fmt_ctl,  mesh_file)
!
      end subroutine set_control_new_mesh_file_def
!
! -----------------------------------------------------------------------
!
      end module set_ctl_params_2nd_files
