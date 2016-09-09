!gz_mesh_file_IO_b.f90
!      module gz_mesh_file_IO_b
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine gz_read_mesh_file_b(my_rank)
!      subroutine gz_read_mesh_geometry_b(my_rank)
!
!      subroutine gz_read_node_size_b(my_rank)
!      subroutine gz_read_geometry_size_b(my_rank)
!
!      subroutine gz_write_mesh_file_b(my_rank)
!
      module gz_mesh_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_comm_data_IO
      use m_read_mesh_data
      use gz_binary_IO
      use skip_gz_comment
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_mesh_file_b(my_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use gz_mesh_data_IO_b
      use gz_groups_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary mesh file: ', trim(mesh_file_name)
!
      call open_rd_gzfile_b(mesh_file_name, my_rank)
!
      call gz_read_geometry_data_b
!
!   read node group
      call gz_read_group_data_b(bc_grp_IO)
!  read element group
      call gz_read_group_data_b(mat_grp_IO)
!  read surface group
      call gz_read_surf_grp_data_b(surf_grp_IO)
!
      call close_gzfile_f
!
      end subroutine gz_read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_mesh_geometry_b(my_rank)
!
      use gz_mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary mesh file: ', trim(mesh_file_name)
!
      call open_rd_gzfile_b(mesh_file_name, my_rank)
      call gz_read_geometry_data_b
      call close_gzfile_f
!
      end subroutine gz_read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
       subroutine gz_read_node_size_b(my_rank)
!
       use gz_domain_data_IO_b
       use gz_mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary mesh file: ', trim(mesh_file_name)
!
      call open_rd_gzfile_b(mesh_file_name, my_rank)
      call gz_read_domain_info_b(my_rank_IO, comm_IO)
      call gz_read_number_of_node_b
      call close_gzfile_f
!
      end subroutine gz_read_node_size_b
!
!------------------------------------------------------------------
!
       subroutine gz_read_geometry_size_b(my_rank)
!
       use gz_domain_data_IO_b
       use gz_mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary mesh file: ', trim(mesh_file_name)
!
      call open_rd_gzfile_b(mesh_file_name, my_rank)
!
      call gz_read_domain_info_b(my_rank_IO, comm_IO)
      call gz_read_number_of_node_b
      call gz_read_geometry_info_b
!
      call gz_read_number_of_element_b
      call close_gzfile_f
!
      end subroutine gz_read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_mesh_file_b(my_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use gz_mesh_data_IO_b
      use gz_groups_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped binary mesh file: ', trim(mesh_file_name)
!
      call open_wt_gzfile_b(mesh_file_name)
      call gz_write_geometry_data_b
!
!   write node group
      call gz_write_grp_data_b(bc_grp_IO)
!  write element group
      call gz_write_grp_data_b(mat_grp_IO)
!  write surface group
      call gz_write_surf_grp_data_b(surf_grp_IO)
!
      call close_gzfile_f
!
      end subroutine gz_write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module gz_mesh_file_IO_b
