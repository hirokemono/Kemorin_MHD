!>@file   find_mesh_file_format.f90
!!@brief  module find_mesh_file_format
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine find_merged_mesh_format(mesh_file)
!!      subroutine find_mesh_format_4_viewer(mesh_file)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!@endverbatim
!
      module find_mesh_file_format
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_file_format_switch
!
      use t_file_IO_parameter
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine find_merged_mesh_format(mesh_file)
!
      use t_file_IO_parameter
      use m_file_format_switch
      use mesh_file_name_by_param
!
      type(field_IO_params), intent(inout) ::  mesh_file
!
!
      call find_mesh_format_4_viewer(mesh_file)
      if(mesh_file%iflag_format .ne. -1) return
!
!  Detect file format
      mesh_file%iflag_format = id_gzip_txt_file_fmt + iflag_single
      if(check_exist_mesh(0, mesh_file)) return
!
      mesh_file%iflag_format = id_ascii_file_fmt + iflag_single
      if(check_exist_mesh(0, mesh_file)) return
!
      mesh_file%iflag_format = id_binary_file_fmt + iflag_single
      if(check_exist_mesh(0, mesh_file)) return
!
      mesh_file%iflag_format = id_gzip_bin_file_fmt + iflag_single
      if(check_exist_mesh(0, mesh_file)) return
!
      mesh_file%iflag_format = -1
!
      end subroutine find_merged_mesh_format
!
!------------------------------------------------------------------
!
      subroutine find_mesh_format_4_viewer(mesh_file)
!
      use t_file_IO_parameter
      use m_file_format_switch
      use mesh_file_name_by_param
!
      type(field_IO_params), intent(inout) ::  mesh_file
!
!
!  Detect file format
      mesh_file%iflag_format = id_gzip_txt_file_fmt
      if(check_exist_mesh(0, mesh_file)) return
!
      mesh_file%iflag_format = id_ascii_file_fmt
      if(check_exist_mesh(0, mesh_file)) return
!
      mesh_file%iflag_format = id_binary_file_fmt
      if(check_exist_mesh(0, mesh_file)) return
!
      mesh_file%iflag_format = id_gzip_bin_file_fmt
      if(check_exist_mesh(0, mesh_file)) return
!
      mesh_file%iflag_format = -1
!
      end subroutine find_mesh_format_4_viewer
!
!------------------------------------------------------------------
!
      end module find_mesh_file_format
