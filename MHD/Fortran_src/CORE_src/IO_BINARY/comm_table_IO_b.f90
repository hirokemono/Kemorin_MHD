!>@file  comm_table_IO_b.f90
!!      module comm_table_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine read_comm_table_b(id_rank, bbuf, comm_IO)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine write_comm_table_b(id_rank, comm_IO, bbuf)
!!        type(communication_table), intent(in) :: comm_IO
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module comm_table_IO_b
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
      use t_surf_edge_IO
      use t_binary_IO_buffer
      use binary_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_comm_table_b(id_rank, bbuf, comm_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(communication_table), intent(inout) :: comm_IO
!
!
      call read_domain_info_b(id_rank, bbuf, comm_IO)
      if(bbuf%ierr_bin .gt. 0) return
!
! ----  import & export 
!
      call read_import_data_b(bbuf, comm_IO)
      if(bbuf%ierr_bin .gt. 0) return
      call read_export_data_b(bbuf, comm_IO)
!
      end subroutine read_comm_table_b
!
!------------------------------------------------------------------
!
      subroutine write_comm_table_b(id_rank, comm_IO, bbuf)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_domain_info_b(id_rank, comm_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
! ----  import & export 
!
      call write_import_data_b(comm_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_export_data_b(comm_IO, bbuf)
!
      end subroutine write_comm_table_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_comm_table_b(id_rank, bbuf, comm_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(communication_table), intent(inout) :: comm_IO
!
!
      call read_domain_info_b(id_rank, bbuf, comm_IO)
      if(bbuf%ierr_bin .gt. 0) return
      call read_import_data_b(bbuf, comm_IO)
      if(bbuf%ierr_bin .gt. 0) return
!
      call read_domain_info_b(id_rank, bbuf, comm_IO)
      if(bbuf%ierr_bin .gt. 0) return
      call read_export_data_b(bbuf, comm_IO)
!
      end subroutine read_comm_table_b
!
!------------------------------------------------------------------
!
      subroutine write_comm_table_b(id_rank, comm_IO, bbuf)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_domain_info_b(id_rank, comm_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_import_data_b(comm_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_domain_info_b(id_rank, comm_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_export_data_b(comm_IO, bbuf)
!
      end subroutine write_comm_table_b
!
!------------------------------------------------------------------
!
      end module comm_table_IO_b
