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
      call gz_mpi_write_domain_info_b(id_file, nprocs_in, ioff_gl)
!
      call gz_mpi_write_geometry_info_b(id_file, ioff_gl)
      call gz_mpi_write_element_info_b(id_file, ioff_gl)
!
      call gz_mpi_write_import_data_b(id_file, ioff_gl)
      call gz_mpi_write_export_data_b(id_file, ioff_gl)
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
      call gz_mpi_write_one_integer_b(id_file, ioff_gl, numnod_dummy)
      call gz_mpi_write_one_integer_b                                   &
     &   (id_file, ioff_gl, internal_node_dummy)
!
      call gz_mpi_write_int8_vector_b                                   &
     &   (id_file, ioff_gl, numnod_dummy, globalnodid_dummy)
      call gz_mpi_write_2d_vector_b                                     &
     &   (id_file, ioff_gl, numnod_dummy, ithree, xx_dummy)
!
      call deallocate_node_data_dummy
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
     &  (id_file, ioff_gl, num, ie_dummy(1,1))
!
      call deallocate_ele_info_dummy
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
     &   (id_file, nprocs_in, id_rank, ioff_gl)
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
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call gz_mpi_read_export_data_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
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
     &   (id_file, nprocs_in, id_rank, ioff_gl, numnod_dummy)
      call gz_mpi_read_one_integer_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl, internal_node_dummy)
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
      call allocate_node_data_dummy
!
      call gz_mpi_read_int8_vector_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    numnod_dummy, globalnodid_dummy)
      call gz_mpi_read_2d_vector_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    numnod_dummy, ithree, xx_dummy)
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
      call allocate_ele_info_dummy
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
      call allocate_connect_dummy
!
      call gz_mpi_read_int8_vector_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ele_IO%numele, ele_IO%iele_global)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call gz_mpi_read_int_vector_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl, num, ie_dummy(1,1))
!
      end subroutine gz_mpi_read_element_info_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_mesh_data_IO_b
