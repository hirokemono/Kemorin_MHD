!mesh_file_IO_b.f90
!      module mesh_file_IO_b
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine read_mesh_file_b(my_rank)
!      subroutine read_mesh_geometry_b(my_rank)
!
!      subroutine read_node_size_b(my_rank)
!      subroutine read_geometry_size_b(my_rank)
!
!      subroutine write_mesh_file_b(my_rank)
!
      module mesh_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_read_mesh_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_file_b(my_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use mesh_data_IO
      use groups_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(mesh_file_name)
!
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'unformatted')
!
      if (iflag_debug.gt.0) write(*,*) 'read_geometry_data_b'
      call read_geometry_data_b
!
!   read node group
      call read_group_data_b(input_file_code, bc_grp_IO)
!  read element group
      call read_group_data_b(input_file_code, mat_grp_IO)
!  read surface group
      call read_surf_grp_data_b(input_file_code, surf_grp_IO)
!
      close(input_file_code)
!
      end subroutine read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_geometry_b(my_rank)
!
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(mesh_file_name)
!
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'unformatted')
      call read_geometry_data_b
      close(input_file_code)
!
      end subroutine read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
       subroutine read_node_size_b(my_rank)
!
       use domain_data_IO
       use node_geometry_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(mesh_file_name)
!
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'unformatted')
      call read_domain_info_b(input_file_code)
      call read_number_of_node_b(input_file_code)
      close(input_file_code)
!
      end subroutine read_node_size_b
!
!------------------------------------------------------------------
!
       subroutine read_geometry_size_b(my_rank)
!
       use domain_data_IO
       use node_geometry_IO
       use element_connect_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(mesh_file_name)
!
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'unformatted')
!
      call read_domain_info_b(input_file_code)
      call read_number_of_node_b(input_file_code)
      call read_geometry_info_b(input_file_code)
!
      call read_number_of_element_b(input_file_code)
      close(input_file_code)
!
      end subroutine read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_mesh_file_b(my_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use mesh_data_IO
      use groups_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write binary mesh file: ', trim(mesh_file_name)
!
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'unformatted')
      if (iflag_debug.gt.0) write(*,*) 'write_geometry_data_b'
      call write_geometry_data_b
!
!   write node group
      call write_grp_data_b(input_file_code, bc_grp_IO)
!  write element group
      call write_grp_data_b(input_file_code, mat_grp_IO)
!  write surface group
      call write_surf_grp_data_b(input_file_code, surf_grp_IO)
!
      close(input_file_code)
!
      end subroutine write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module mesh_file_IO_b
