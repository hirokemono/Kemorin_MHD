!>@file  MPI_binary_data_IO.f90
!!       module MPI_binary_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief Output gzipped merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine mpi_write_integer_stack_b(id_file,                   &
!!     &          nprocs_in, id_rank, ioff_gl, num, istack,             &
!!     &          istack_merged)
!!        Substitution of gz_write_integer_stack_b
!!      subroutine mpi_write_int_vector_b(id_file, nprocs_in, id_rank,  &
!!     &          ioff_gl, num, int_dat, istack_merged)
!!        Substitution of gz_write_mul_integer_b
!!      subroutine mpi_write_int8_vector_b(id_file, nprocs_in, id_rank, &
!!     &          ioff_gl, num, int8_dat, istack_merged)
!!        Substitution of gz_write_mul_int8_b
!!
!!      subroutine mpi_write_1d_vector_b(id_file, nprocs_in, id_rank,   &
!!     &          ioff_gl, num, real_dat, istack_merged)
!!        Substitution of gz_write_1d_vector_b
!!      subroutine mpi_write_2d_vector_b(id_file, nprocs_in, id_rank,   &
!!     &          ioff_gl, n1, n2, real_dat, istack_merged)
!!        Substitution of gz_write_2d_vector_b
!!
!!      subroutine mpi_read_integer_stack_b(id_file,                    &
!!     &          nprocs_in, id_rank, ioff_gl, num, istack, ntot,       &
!!     &          istack_merged)
!!        Substittion of  gz_read_integer_stack_b
!!      subroutine mpi_read_int_vector_b(id_file, nprocs_in, id_rank,   &
!!     &          ioff_gl, num, int_dat, istack_merged)
!!        Substitution of gz_read_mul_integer_b
!!      subroutine mpi_read_int8_vector_b(id_file, nprocs_in, id_rank,  &
!!     &          ioff_gl, num, int8_dat, istack_merged)
!!        Substitution of gz_read_mul_int8_b
!!
!!      subroutine mpi_read_1d_vector_b(id_file, nprocs_in, id_rank,    &
!!     &          ioff_gl, num, real_dat, istack_merged)
!!        Substitution of gz_read_1d_vector_b
!!      subroutine mpi_read_2d_vector_b(id_file, nprocs_in, id_rank,    &
!!     &          ioff_gl, n1, n2, real_dat, istack_merged)
!!        Substitution of gz_read_2d_vector_b
!!@endverbatim
!
      module MPI_binary_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use m_calypso_mpi_IO
      use MPI_binary_head_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_integer_stack_b(id_file,                     &
     &          nprocs_in, id_rank, ioff_gl, num, istack,               &
     &          istack_merged)
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: num
!
      integer(kind = kint), intent(in) :: istack(0:num)
!
!
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank,          &
     &    ioff_gl, num, istack(1), istack_merged)
!
      end subroutine mpi_write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int_vector_b(id_file, nprocs_in, id_rank,    &
     &          ioff_gl, num, int_dat, istack_merged)
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: num
!
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      istack_buffer(0:nprocs_in) = istack_merged(0:nprocs_in) * kint
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_int8                                &
     &     (id_file, ioffset, nprocs_in, istack_buffer(1))
      end if
      ioff_gl = ioff_gl + kint_gl * nprocs_in
!
      ioffset = ioff_gl + kint * istack_merged(id_rank)
      call calypso_mpi_seek_write_int                                   &
     &    (id_file, ioffset, num, int_dat(1))
      ioff_gl = ioff_gl + kint * istack_merged(nprocs_in)
!
      end subroutine mpi_write_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int8_vector_b(id_file, nprocs_in, id_rank,   &
     &          ioff_gl, num, int8_dat, istack_merged)
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: num
!
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      istack_buffer(0:nprocs_in) = istack_merged(0:nprocs_in) * kint_gl
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_int8                                &
     &     (id_file, ioffset, nprocs_in, istack_buffer(1))
      end if
      ioff_gl = ioff_gl + kint_gl * nprocs_in
!
      ioffset = ioff_gl + kint_gl * istack_merged(id_rank)
      call calypso_mpi_seek_write_int8                                  &
     &    (id_file, ioffset, num, int8_dat(1))
      ioff_gl = ioff_gl + kint_gl * istack_merged(nprocs_in)
!
      end subroutine mpi_write_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_1d_vector_b(id_file, nprocs_in, id_rank,     &
     &          ioff_gl, num, real_dat, istack_merged)
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: num
!
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      istack_buffer(0:nprocs_in) = istack_merged(0:nprocs_in) * kreal
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_int8                                &
     &     (id_file, ioffset, nprocs_in, istack_buffer(1))
      end if
      ioff_gl = ioff_gl + kint_gl * nprocs_in
!
      ioffset = ioff_gl + kreal * istack_merged(id_rank)
      call calypso_mpi_seek_write_real                                  &
     &    (id_file, ioffset, num, real_dat(1))
      ioff_gl = ioff_gl + kreal * istack_merged(nprocs_in)
!
      end subroutine mpi_write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_2d_vector_b(id_file, nprocs_in, id_rank,     &
     &          ioff_gl, n1, n2, real_dat, istack_merged)
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: n1, n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint) :: num
      integer(kind = kint_gl) :: istack_2d_merge(0:nprocs_in)
!
!
      num = n1 * n2
      istack_2d_merge(0:nprocs_in) = n2 * istack_merged(0:nprocs_in)
      call mpi_write_1d_vector_b(id_file, nprocs_in, id_rank,           &
     &    ioff_gl, num, real_dat(1,1), istack_2d_merge)
!
      end subroutine mpi_write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_integer_stack_b(id_file,                      &
     &          nprocs_in, id_rank, ioff_gl, num, istack, ntot,         &
     &          istack_merged)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: num
!
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
!
!
      istack(0) = 0
      call mpi_read_int_vector_b(id_file, nprocs_in, id_rank,           &
     &    ioff_gl, num, istack(1), istack_merged)
      ntot = istack(num)
!
      end subroutine mpi_read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int_vector_b(id_file, nprocs_in, id_rank,     &
     &          ioff_gl, num, int_dat, istack_merged)
!
      use m_phys_constants
      use field_data_IO
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: num
!
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(id_rank .ge. nprocs_in) return
      ioff_gl = ioff_gl + kint_gl * nprocs_in
!
      ioffset = ioff_gl + kint * istack_merged(id_rank)
      call calypso_mpi_seek_read_int                                    &
     &   (id_file, ioffset, num, int_dat(1))
      ioff_gl = ioff_gl + kint * istack_merged(nprocs_in)
!
      end subroutine mpi_read_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int8_vector_b(id_file, nprocs_in, id_rank,    &
     &          ioff_gl, num, int8_dat, istack_merged)
!
      use m_phys_constants
      use field_data_IO
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: num
!
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(id_rank .ge. nprocs_in) return
      ioff_gl = ioff_gl + kint_gl * nprocs_in
!
      ioffset = ioff_gl + kint_gl * istack_merged(id_rank)
      call calypso_mpi_seek_read_int8                                   &
     &   (id_file, ioffset, num, int8_dat(1))
      ioff_gl = ioff_gl + kint_gl * istack_merged(nprocs_in)
!
      end subroutine mpi_read_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_1d_vector_b(id_file, nprocs_in, id_rank,      &
     &          ioff_gl, num, real_dat, istack_merged)
!
      use m_phys_constants
      use field_data_IO
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: num
!
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(id_rank .ge. nprocs_in) return
      ioff_gl = ioff_gl + kint_gl * nprocs_in
!
      ioffset = ioff_gl + kreal * istack_merged(id_rank)
      call calypso_mpi_seek_read_real                                   &
     &   (id_file, ioffset, num, real_dat(1))
      ioff_gl = ioff_gl + kreal * istack_merged(nprocs_in)
!
      end subroutine mpi_read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_2d_vector_b(id_file, nprocs_in, id_rank,      &
     &          ioff_gl, n1, n2, real_dat, istack_merged)
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: n1, n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
!
      integer(kind = kint) :: num
      integer(kind = kint_gl) :: istack_2d_merge(0:nprocs_in)
!
!
      num = n1 * n2
      istack_2d_merge(0:nprocs_in) = n2 * istack_merged(0:nprocs_in)
      call mpi_read_1d_vector_b(id_file, nprocs_in, id_rank,            &
     &    ioff_gl, num, real_dat(1,1), istack_2d_merge)
!
      end subroutine mpi_read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module MPI_binary_data_IO
