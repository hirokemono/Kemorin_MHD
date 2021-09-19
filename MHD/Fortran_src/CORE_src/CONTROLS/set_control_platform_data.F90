!>@file   set_control_platform_data.F90
!!        module set_control_platform_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!!
!>@brief    Load file definitions from control structures
!!
!!@verbatim
!!      subroutine set_control_mesh_def(plt, mesh_file)
!!        type(platform_data_control), intent(in) :: plt
!!        type(field_IO_params), intent(inout) :: mesh_file
!!
!!      subroutine set_control_mesh_file_def                            &
!!     &         (default_prefix, plt_ctl, mesh_file)
!!        type(platform_data_control), intent(in) :: plt_ctl
!!        type(field_IO_params), intent(inout) :: mesh_file
!!@endverbatim
!!
!!@param id_rank  preocess ID
!
      module set_control_platform_data
!
      use m_precision
!
      use m_constants
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_file_IO_parameter
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_mesh_def(plt, mesh_file)
!
      use m_default_file_prefix
      use m_file_format_switch
      use set_control_platform_item
      use stop_by_missing_zlib
!
      type(platform_data_control), intent(in) :: plt
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_parallel_file_ctl_params(def_mesh_file_head,             &
     &    plt%mesh_file_prefix, plt%mesh_file_fmt_ctl, mesh_file)
      call s_stop_by_missing_zlib(mesh_file%file_prefix,                &
     &                            mesh_file%iflag_format)
!
      end subroutine set_control_mesh_def
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_mesh_file_def                              &
     &         (default_prefix, plt_ctl, mesh_file)
!
      use set_control_platform_item
!
      character(len=kchara), intent(in) :: default_prefix
      type(platform_data_control), intent(in) :: plt_ctl
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_file_control_params(default_prefix,                      &
     &    plt_ctl%mesh_file_prefix, plt_ctl%mesh_file_fmt_ctl,          &
     &    mesh_file)
!
      end subroutine set_control_mesh_file_def
!
! -----------------------------------------------------------------------
!
      end module set_control_platform_data
