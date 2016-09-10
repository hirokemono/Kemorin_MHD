!>@file  field_file_MPI_IO_b.f90
!!       module field_file_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_field_file_mpi_b                          &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
!!
!!      subroutine read_step_field_file_mpi_b                           &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
!!      subroutine read_alloc_stp_fld_file_mpi_b                        &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
!!      subroutine read_alloc_stp_fld_head_mpi_b                        &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
!!
!!   Data format for the merged binary field data
!!     1.   Number of process
!!     2.   Time step
!!     3.   Time, Delta t
!!     4.   Stacks of numbe of data points
!!     5.   Number of fields
!!     6.   List of number of components
!!     7.   Field names
!!     8.   List of data size (Byte)
!!     9.   All Field data
!!@endverbatim
!
      module field_file_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use m_phys_constants
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_field_data_IO
!
      implicit none
!
      private :: read_field_header_mpi_b
      private :: write_field_data_mpi_b
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_step_field_file_mpi_b                            &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      type(field_IO), intent(in) :: fld_IO
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'write binary data by MPI-IO: ', trim(file_name)
!
      call open_write_mpi_file_b                                        &
     &   (file_name, nprocs_in, id_file, ioff_gl)
!
      if(id_rank .lt. nprocs_in) then
        call write_field_data_mpi_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,             &
     &    fld_IO%istack_numnod_IO)
      end if
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine write_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_step_field_file_mpi_b                             &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
      type(field_IO), intent(inout) :: fld_IO
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read binary data by MPI-IO: ', trim(file_name)
!
!
      call open_read_mpi_file_b(file_name, id_file, ioff_gl)
      call read_field_header_mpi_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, fld_IO)
!
      call mpi_read_mul_inthead_b                                       &
     &    (id_file, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call mpi_read_mul_charahead_b(id_file, ioff_gl,                   &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
!
      call mpi_read_2d_vector_b(id_file, nprocs_in, id_rank, ioff_gl,   &
     &    fld_IO%nnod_IO, fld_IO%ntot_comp_IO, fld_IO%d_IO,             &
     &    fld_IO%istack_numnod_IO)
!
      call dealloc_merged_field_stack(fld_IO)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine read_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_file_mpi_b                          &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
      type(field_IO), intent(inout) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read binary data by MPI-IO: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, id_fld, ioff_gl)
      call read_field_header_mpi_b                                      &
     &   (id_fld, nprocs_in, id_rank, ioff_gl, fld_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call mpi_read_mul_inthead_b                                       &
     &    (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call mpi_read_mul_charahead_b(id_fld, ioff_gl,                    &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
!
      call mpi_read_2d_vector_b(id_fld, nprocs_in, id_rank, ioff_gl,    &
     &    fld_IO%nnod_IO, fld_IO%ntot_comp_IO, fld_IO%d_IO,             &
     &    fld_IO%istack_numnod_IO)
!
      call calypso_close_mpi_file(id_fld)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. nprocs_in) then
        call dealloc_phys_data_IO(fld_IO)
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine read_alloc_stp_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_head_mpi_b                          &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
      type(field_IO), intent(inout) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read binary data by MPI-IO: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, id_fld, ioff_gl)
      call read_field_header_mpi_b                                      &
     &   (id_fld, nprocs_in, id_rank, ioff_gl, fld_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call mpi_read_mul_inthead_b                                       &
     &    (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call mpi_read_mul_charahead_b(id_fld, ioff_gl,                    &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call calypso_close_mpi_file(id_fld)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. nprocs_in) then
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine read_alloc_stp_fld_head_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_field_data_mpi_b(id_file, nprocs_in, id_rank,    &
     &          ioff_gl, nnod, num_field, ntot_comp, ncomp_field,       &
     &          field_name, d_nod, istack_merged)
!
      use m_time_data_IO
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_file
!
!
      call mpi_write_one_inthead_b(id_file, ioff_gl, nprocs_in)
      call mpi_write_one_inthead_b(id_file, ioff_gl, i_time_step_IO)
      call mpi_write_one_realhead_b(id_file, ioff_gl, time_IO)
      call mpi_write_one_realhead_b(id_file, ioff_gl, delta_t_IO)
!
      call mpi_write_mul_int8head_b                                     &
     &   (id_file, ioff_gl, nprocs_in, istack_merged(1))
!
      call mpi_write_one_inthead_b(id_file, ioff_gl, num_field)
!
!
      call mpi_write_mul_inthead_b                                      &
     &    (id_file, ioff_gl, num_field, ncomp_field)
!
      call mpi_write_mul_charahead_b                                    &
     &   (id_file, ioff_gl, num_field, field_name)
!
      call mpi_write_2d_vector_b(id_file, nprocs_in, id_rank,           &
     &   ioff_gl, nnod, ntot_comp, d_nod, istack_merged)
!
      end subroutine write_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_header_mpi_b(id_file, nprocs_in, id_rank,   &
     &          ioff_gl, fld_IO)
!
      use m_time_data_IO
      use m_phys_constants
      use field_data_MPI_IO
      use MPI_binary_head_IO
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: nprocs_tmp
!
!
      call mpi_read_one_inthead_b(id_file, ioff_gl, nprocs_tmp)
      call mpi_read_one_inthead_b(id_file, ioff_gl, i_time_step_IO)
      call mpi_read_one_realhead_b(id_file, ioff_gl, time_IO)
      call mpi_read_one_realhead_b(id_file, ioff_gl, delta_t_IO)
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call mpi_read_mul_int8head_b(id_file, ioff_gl, nprocs_in,         &
     &    fld_IO%istack_numnod_IO(1:nprocs_in))
      call sync_field_header_mpi(nprocs_in, id_rank, fld_IO%nnod_IO,    &
     &    fld_IO%istack_numnod_IO)
!
      call mpi_read_one_inthead_b(id_file, ioff_gl, fld_IO%num_field_IO)
!
      end subroutine read_field_header_mpi_b
!
! -----------------------------------------------------------------------
!
      end module field_file_MPI_IO_b
