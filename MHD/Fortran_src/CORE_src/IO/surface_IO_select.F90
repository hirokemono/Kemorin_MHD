!>@file  surface_IO_select.F90
!!       module surface_IO_select
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in Aug., 2006
!
!> @brief data IO selector for surface mesh
!!
!!@verbatim
!!      subroutine sel_input_surface_connect(my_rank)
!!      subroutine sel_input_surface_file(my_rank)
!!
!!      subroutine sel_output_surface_connect(my_rank)
!!
!!      subroutine sel_output_surface_file(my_rank)
!!      subroutine sel_output_surface_sph_file(my_rank)
!!      subroutine sel_output_surface_cyl_file(my_rank)
!!@endverbatim
!!
!!@param my_rank  process ID
!
      module surface_IO_select
!
      use m_precision
!
      use m_read_mesh_data
      use m_file_format_switch
      use surface_file_IO
      use gz_surface_file_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine sel_input_surface_connect(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_surface_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call input_surface_connect_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call input_surface_connect_b
      else
        call input_surface_connect
      end if
!
      end subroutine sel_input_surface_connect
!
!------------------------------------------------------------------
!
      subroutine sel_input_surface_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_surface_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call input_surface_file_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call input_surface_file_b
      else
        call input_surface_file
      end if
!
      end subroutine sel_input_surface_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_output_surface_connect(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_surface_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_surface_connect_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_surface_connect_b
      else
        call output_surface_connect
      end if
!
      end subroutine sel_output_surface_connect
!
!------------------------------------------------------------------
!
      subroutine sel_output_surface_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_surface_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_surface_file_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_surface_file_b
      else
        call output_surface_file
      end if
!
      end subroutine sel_output_surface_file
!
!------------------------------------------------------------------
!
      subroutine sel_output_surface_sph_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_surface_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_surface_sph_file_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_surface_file_b
      else
        call output_surface_sph_file
      end if
!
      end subroutine sel_output_surface_sph_file
!
!------------------------------------------------------------------
!
      subroutine sel_output_surface_cyl_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_surface_fname(my_rank)
!
#ifdef ZLIB_IO
      if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
        call output_surface_cyl_file_gz
        return
      end if
#endif
!
      if (iflag_mesh_file_fmt .eq. id_binary_file_fmt) then
        call output_surface_file_b
      else
        call output_surface_cyl_file
      end if
!
      end subroutine sel_output_surface_cyl_file
!
!------------------------------------------------------------------
!
      end module surface_IO_select
