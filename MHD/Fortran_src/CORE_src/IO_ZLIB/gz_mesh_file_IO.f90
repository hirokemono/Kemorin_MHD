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
!!@endverbatim
!!
      module gz_mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_read_mesh_data
      use set_parallel_file_name
      use skip_gz_comment
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
      use m_read_boundary_data
      use gz_sph_rj_groups_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped mesh file: ', trim(mesh_file_name)
!
      call open_rd_gzfile_f(mesh_file_name)
!
      call read_geometry_data_gz
!
!   read node group
      call read_group_data_gz(bc_grp_IO)
!  read element group ( not in use)
      call read_group_data_gz(mat_grp_IO)
!  read surface group
      call read_surf_grp_data_gz(surf_grp_IO)
!
      call close_gzfile_f
!
      end subroutine read_mesh_gz
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_geometry_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped mesh file: ', trim(mesh_file_name)
!
      call open_rd_gzfile_f(mesh_file_name)
!
      call read_geometry_data_gz
!
      call close_gzfile_f
!
      end subroutine read_mesh_geometry_gz
!
!  ---------------------------------------------------------------------
!
       subroutine read_node_size_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped mesh file: ', trim(mesh_file_name)
!
      call open_rd_gzfile_f(mesh_file_name)
!
      call read_num_node_gz
      call close_gzfile_f
!
      end subroutine read_node_size_gz
!
!------------------------------------------------------------------
!
       subroutine read_geometry_size_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped mesh file: ', trim(mesh_file_name)
!
      call open_rd_gzfile_f(mesh_file_name)
!
      call read_num_node_ele_gz
      call close_gzfile_f
!
      end subroutine read_geometry_size_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_mesh_file_gz(my_rank)
!
      use m_read_boundary_data
      use m_fem_mesh_labels
      use gz_sph_rj_groups_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped mesh file: ', trim(mesh_file_name)
!
      call open_wt_gzfile_f(mesh_file_name)
!
      call write_geometry_data_gz
!
!   write node group
      textbuf = hd_fem_nodgrp() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(bc_grp_IO)
!
!  write element group
      textbuf = hd_fem_elegrp() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(mat_grp_IO)
!
!  write surface group
      textbuf = hd_fem_sfgrp() // char(0)
      call gz_write_textbuf_no_lf
      call write_surf_grp_data_gz(surf_grp_IO)
!
      call close_gzfile_f
!
      end subroutine write_mesh_file_gz
!
!  ---------------------------------------------------------------------
!
      end module gz_mesh_file_IO
