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
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_field_data_IO
!
      implicit none
!
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
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      type(field_IO), intent(in) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'write merged binary data: ', trim(file_name)
!
      call calypso_mpi_write_file_open(file_name, nprocs_in, id_fld)
!
      if(id_rank .lt. nprocs_in) then
        ioff_gl = 0
        call write_field_data_mpi_b                                     &
     &   (id_fld, nprocs_in, id_rank, ioff_gl,                          &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,             &
     &    fld_IO%istack_numnod_IO)
      end if
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine write_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_step_field_file_mpi_b                             &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use field_data_MPI_IO_b
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
     &    'read merged binary data: ', trim(file_name)
!
!
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_step_data_mpi_b(id_fld, nprocs_in, ioff_gl)
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl, &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call read_field_num_mpi_b                                         &
     &    (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_names_mpi_b(id_fld, ioff_gl,                      &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
!
      call read_field_data_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl,   &
     &      fld_IO%nnod_IO, fld_IO%ntot_comp_IO, fld_IO%d_IO,           &
     &      fld_IO%istack_numnod_IO)
!
      call dealloc_merged_field_stack(fld_IO)
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine read_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_file_mpi_b                          &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use field_data_MPI_IO_b
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
     &    'read merged binary data: ', trim(file_name)
!
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_step_data_mpi_b(id_fld, nprocs_in, ioff_gl)
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl, &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call read_field_num_mpi_b                                         &
     &    (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_names_mpi_b(id_fld, ioff_gl,                      &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
!
      call read_field_data_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl,   &
     &      fld_IO%nnod_IO, fld_IO%ntot_comp_IO, fld_IO%d_IO,           &
     &      fld_IO%istack_numnod_IO)
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
      use field_data_MPI_IO_b
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
     &    'read merged binary data: ', trim(file_name)
!
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_step_data_mpi_b(id_fld, nprocs_in, ioff_gl)
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl, &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call read_field_num_mpi_b                                         &
     &    (id_fld, ioff_gl, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_names_mpi_b(id_fld, ioff_gl,                      &
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
      subroutine write_field_data_mpi_b(id_fld, nprocs_in, id_rank,     &
     &          ioff_gl, nnod, num_field, ntot_comp, ncomp_field,       &
     &          field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use field_data_MPI_IO_b
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
      integer, intent(in) ::  id_fld
!
!
      call write_step_data_mpi_b(id_fld, ioff_gl, nprocs_in)
!
      call write_field_head_mpi_b(id_fld, nprocs_in, ioff_gl,           &
     &   num_field, ncomp_field, istack_merged)
!
      call write_fld_vecotr_mpi_b                                       &
     &   (id_fld, nprocs_in, id_rank, ioff_gl, nnod,                    &
     &    num_field, ntot_comp, field_name, d_nod, istack_merged)
!
      end subroutine write_field_data_mpi_b
!
! -----------------------------------------------------------------------
!
      end module field_file_MPI_IO_b
