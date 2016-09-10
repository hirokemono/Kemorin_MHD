!>@file   gz_MPI_mesh_data_IO_b.f90
!!@brief  module gz_MPI_mesh_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for gzipped binary mesh data IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_geometry_data_b                         &
!!     &         (id_file, nprocs_in, ioff_gl)
!!      subroutine gz_mpi_write_geometry_info_b(id_file, ioff_gl)
!!      subroutine gz_mpi_write_element_info_b(id_file, ioff_gl)
!!
!!      subroutine gz_mpi_read_geometry_data_b                          &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine gz_mpi_read_number_of_node_b                         &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine gz_mpi_read_geometry_info_b                          &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine gz_mpi_read_number_of_element_b                      &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!@endverbatim
!
      module gz_MPI_mesh_data_IO_b
!
      use m_precision
      use m_constants
!
      use m_comm_data_IO
      use m_read_mesh_data
!
      implicit  none
!
      private :: gz_mpi_write_element_info_b
      private :: gz_mpi_read_element_info_b
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geometry_data_b                           &
     &         (id_file, nprocs_in, ioff_gl)
!
      use gz_MPI_domain_data_IO_b
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_write_domain_info_b                                   &
     &   (id_file, nprocs_in, ioff_gl, comm_IO)
!
      call gz_mpi_write_geometry_info_b(id_file, ioff_gl)
      call gz_mpi_write_element_info_b(id_file, ioff_gl)
!
      call gz_mpi_write_import_data_b(id_file, ioff_gl, comm_IO)
      call gz_mpi_write_export_data_b(id_file, ioff_gl, comm_IO)
!
      end subroutine gz_mpi_write_geometry_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geometry_info_b(id_file, ioff_gl)
!
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_datum_IO
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_write_one_integer_b(id_file, ioff_gl, nod_IO%numnod)
      call gz_mpi_write_one_integer_b                                   &
     &   (id_file, ioff_gl, nod_IO%internal_node)
!
      call gz_mpi_write_int8_vector_b                                   &
     &   (id_file, ioff_gl, nod_IO%numnod, nod_IO%inod_global)
      call gz_mpi_write_2d_vector_b                                     &
     &   (id_file, ioff_gl, nod_IO%numnod, ithree, nod_IO%xx)
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine gz_mpi_write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_element_info_b(id_file, ioff_gl)
!
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_datum_IO
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer (kind = kint) :: num
!
!
      call gz_mpi_write_one_integer_b(id_file, ioff_gl, ele_IO%numele)
!
      call gz_mpi_write_int_vector_b                                    &
     &  (id_file, ioff_gl, ele_IO%numele, ele_IO%elmtyp)
      call gz_mpi_write_int8_vector_b                                   &
     &  (id_file, ioff_gl, ele_IO%numele, ele_IO%iele_global)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call gz_mpi_write_int_vector_b                                    &
     &  (id_file, ioff_gl, num, ele_IO%ie)
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine gz_mpi_write_element_info_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geometry_data_b                            &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      use m_error_IDs
      use gz_MPI_domain_data_IO_b
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_read_domain_info_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      call gz_mpi_read_number_of_node_b                                 &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call gz_mpi_read_geometry_info_b                                  &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
!  ----  read element data -------
!
      call gz_mpi_read_number_of_element_b                              &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call gz_mpi_read_element_info_b                                   &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
! ----  import & export 
!
      call gz_mpi_read_import_data_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
      call gz_mpi_read_export_data_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      end subroutine gz_mpi_read_geometry_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_number_of_node_b                           &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_datum_IO
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_read_one_integer_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nod_IO%numnod)
      call gz_mpi_read_one_integer_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nod_IO%internal_node)
!
      end subroutine gz_mpi_read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geometry_info_b                            &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      use gz_MPI_binary_data_IO
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call alloc_node_geometry_base(nod_IO)
!
      call gz_mpi_read_int8_vector_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nod_IO%numnod, nod_IO%inod_global)
      call gz_mpi_read_2d_vector_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nod_IO%numnod, ithree, nod_IO%xx)
!
      end subroutine gz_mpi_read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_number_of_element_b                        &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_datum_IO
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_read_one_integer_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl, ele_IO%numele)
!
      end subroutine gz_mpi_read_number_of_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_element_info_b                             &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      use gz_MPI_binary_data_IO
      use set_nnod_4_ele_by_type
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer (kind = kint) :: num, i
!
!
      call alloc_element_types(ele_IO)
      call gz_mpi_read_int_vector_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ele_IO%numele, ele_IO%elmtyp)
!
      ele_IO%nnod_4_ele = 0
      do i = 1, ele_IO%numele
        call s_set_nnod_4_ele_by_type                                   &
     &     (ele_IO%elmtyp(i), ele_IO%nodelm(i))
        ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
      end do
!
      call alloc_ele_connectivity(ele_IO)
!
      call gz_mpi_read_int8_vector_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ele_IO%numele, ele_IO%iele_global)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call gz_mpi_read_int_vector_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl, num, ele_IO%ie)
!
      end subroutine gz_mpi_read_element_info_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_mesh_data_IO_b
