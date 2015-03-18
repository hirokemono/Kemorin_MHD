!>@file   gz_mesh_file_IO.f90
!!@brief  module gz_mesh_file_IO
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine read_mesh_gz(my_rank)
!!      subroutine read_mesh_geometry_gz(my_rank)
!!
!!      subroutine read_node_size_gz(my_rank)
!!      subroutine read_geometry_size_gz(my_rank)
!!
!!      subroutine write_mesh_file_gz(my_rank)
!!
!!      integer(kind = kint) function check_mesh_file_gz(my_rank)
!!@endverbatim
!!
      module gz_mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_read_mesh_data
      use set_parallel_file_name
      use gz_mesh_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_gz(my_rank)
!
      use gz_boundary_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped mesh file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_geometry_data_gz
      call read_boundary_data_gz
!
      call close_gzfile
!
      end subroutine read_mesh_gz
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_geometry_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped mesh file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_geometry_data_gz
!
      call close_gzfile
!
      end subroutine read_mesh_geometry_gz
!
!  ---------------------------------------------------------------------
!
       subroutine read_node_size_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped mesh file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_num_node_gz
      call close_gzfile
!
      end subroutine read_node_size_gz
!
!------------------------------------------------------------------
!
       subroutine read_geometry_size_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped mesh file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_num_node_ele_gz
      call close_gzfile
!
      end subroutine read_geometry_size_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_mesh_file_gz(my_rank)
!
      use gz_boundary_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped mesh file: ', trim(gzip_name)
!
      call open_wt_gzfile(gzip_name)
!
      call write_geometry_data_gz
      call write_boundary_data_gz
!
      call close_gzfile
!
      end subroutine write_mesh_file_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_mesh_file_gz(my_rank)
!
      use delete_data_files
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      check_mesh_file_gz = check_file_exist(gzip_name)
!
      end function check_mesh_file_gz
!
!  ---------------------------------------------------------------------
!
      end module gz_mesh_file_IO
