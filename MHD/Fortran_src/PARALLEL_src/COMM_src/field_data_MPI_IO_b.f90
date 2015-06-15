!>@file  field_data_MPI_IO_b.f90
!!       module field_data_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_data_mpi_b(id_fld, ioff_gl, nprocs_in)
!!      subroutine write_field_head_mpi_b(id_fld, nprocs_in, ioff_gl,   &
!!     &          num_field, ncomp_field, istack_merged)
!!      subroutine write_fld_vecotr_mpi_b                               &
!!     &         (id_fld, nprocs_in, id_rank, ioff_gl, nnod,            &
!!     &          num_field, ncomp, field_name, vector, istack_merged)
!!
!!      subroutine read_step_data_mpi_b(id_fld, nprocs_in, ioff_gl)
!!      subroutine read_field_header_mpi_b(id_fld, nprocs_in, id_rank,  &
!!     &          ioff_gl, nnod, num_field, istack_merged)
!!      subroutine read_field_num_mpi_b                                 &
!!     &         (id_fld, ioff_gl, num_field, ncomp_field)
!!
!!      subroutine read_field_data_mpi_b(id_fld, nprocs_in, id_rank,    &
!!     &          ioff_gl, nnod, ncomp, vect, istack_merged)
!!      subroutine read_field_names_mpi_b                               &
!!     &         (id_fld, ioff_gl, num_field, field_name)
!!
!!   Data format for the merged binary field data
!!     1.   Endian Check integer
!!     2.   Number of process
!!     3.   Time step
!!     4.   Time, Delta t
!!     5.   Stacks of numbe of data points
!!     6.   Number of fields
!!     7.   List of number of components
!!     8.   Field names
!!     9.   List of data size (Byte)
!!    10.   All Field data
!!@endverbatim
!
      module field_data_MPI_IO_b
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
      integer(kind = kint), private :: iflag_endian_swap
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_step_data_mpi_b(id_fld, ioff_gl, nprocs_in)
!
      use m_time_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: nprocs_in
!
      integer, intent(in) ::  id_fld
      integer(kind = kint) :: itmp_IO(1)
      real(kind = kreal) ::   rtmp_IO(1)
!
!
      itmp_IO(1) = i_UNIX
      call calypso_mpi_seek_write_head_i                                &
     &    (id_fld, ioff_gl, ione, itmp_IO)
      itmp_IO(1) = nprocs_in
      call calypso_mpi_seek_write_head_i                                &
     &    (id_fld, ioff_gl, ione, itmp_IO)
      itmp_IO(1) = i_time_step_IO
      call calypso_mpi_seek_write_head_i                                &
     &    (id_fld, ioff_gl, ione, itmp_IO)
!
      rtmp_IO(1) = time_IO
      call calypso_mpi_seek_write_head_r                                &
     &    (id_fld, ioff_gl, ione, rtmp_IO)
      rtmp_IO(1) = delta_t_IO
      call calypso_mpi_seek_write_head_r                                &
     &    (id_fld, ioff_gl, ione, rtmp_IO)
!
      end subroutine write_step_data_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine write_field_head_mpi_b(id_fld, nprocs_in, ioff_gl,     &
     &          num_field, ncomp_field, istack_merged)
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind = kint), intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) ::  id_fld
      integer(kind = kint):: itmp_IO(1)
!
!
      call calypso_mpi_seek_write_head_i8                               &
     &    (id_fld, ioff_gl, nprocs_in, istack_merged(1))
!
      itmp_IO = num_field
      call calypso_mpi_seek_write_head_i                                &
     &    (id_fld, ioff_gl, ione, itmp_IO)
!
      call calypso_mpi_seek_write_head_i                                &
     &    (id_fld, ioff_gl, num_field, ncomp_field(1))
!
      end subroutine write_field_head_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_fld_vecotr_mpi_b                                 &
     &         (id_fld, nprocs_in, id_rank, ioff_gl, nnod,              &
     &          num_field, ncomp, field_name, vector, istack_merged)
!
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field, ncomp
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: vector(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: istack_buffer(0:nprocs_in)
      integer(kind = kint) :: ilength
!
!
      ilength = num_field * kchara
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_write_chara                               &
     &     (id_fld, ioffset, ilength, field_name(1))
      end if
      ioff_gl = ioff_gl + ilength
!
      istack_buffer(0:nprocs_in)                                        &
     &          = ncomp * istack_merged(0:nprocs_in) * kreal
      ilength = nprocs_in * kint_gl
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_write_int8                                &
     &     (id_fld, ioffset, ilength, istack_buffer(1))
      end if
      ioff_gl = ioff_gl + ilength
!
      ilength = nnod * ncomp
      ioffset = int(ioff_gl + kreal * ncomp*istack_merged(id_rank))
      call calypso_mpi_seek_write_real                                  &
     &    (id_fld, ioffset, ilength, vector(1,1))
      ioff_gl = ioff_gl + kreal * ncomp*istack_merged(nprocs_in)
!
      end subroutine write_fld_vecotr_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_step_data_mpi_b(id_fld, nprocs_in, ioff_gl)
!
      use m_time_data_IO
      use m_error_IDs
      use field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iread, iread_e
      integer(kind = kint) :: itmp_IO(1)
      real(kind = kreal) ::   rtmp_IO(1)
!
!
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_read_int(id_fld, ioffset, ione, itmp_IO)
        iread_e = itmp_IO(1)
        ioffset = ioffset + kint
!
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_read_int(id_fld, ioffset, ione, itmp_IO)
        iread = itmp_IO(1)
        ioffset = ioffset + kint
!
        call calypso_mpi_seek_read_int(id_fld, ioffset, ione, itmp_IO)
        i_time_step_IO = itmp_IO(1)
        ioffset = ioffset + kint
!
        call calypso_mpi_seek_read_real(id_fld, ioffset, ione, rtmp_IO)
        time_IO = rtmp_IO(1)
        ioffset = ioffset + kreal
!
        call calypso_mpi_seek_read_real(id_fld, ioffset, ione, rtmp_IO)
        delta_t_IO = rtmp_IO(1)
        ioffset = ioffset + kreal
!
        if(nprocs_in .ne. iread) then
          call calypso_mpi_abort                                        &
     &       (ierr_fld, 'Set correct field data file')
        end if
!
        iflag_endian_swap = 0
        if(i_UNIX .ne. iread_e) then
          write(*,*) 'binary data have opposite endian!'
          iflag_endian_swap = 1
        end if
      end if
!
      call MPI_BCAST(iflag_endian_swap, ione, CALYPSO_INTEGER, izero,   &
     &    CALYPSO_COMM, ierr_MPI)
!
      call sync_field_time_mpi
      ioff_gl = ioff_gl + 3*kint + 2*kreal
!
      end subroutine read_step_data_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_field_header_mpi_b(id_fld, nprocs_in, id_rank,    &
     &          ioff_gl, nnod, num_field, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(inout) :: nnod, num_field
      integer(kind = kint_gl), intent(inout)                            &
     &                        :: istack_merged(0:nprocs_in)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
      integer(kind = kint) :: itmp_IO(1)
!
!
      ilength = nprocs_in
      if(my_rank .eq. 0) then
        istack_merged(0) = 0
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_read_int8                                 &
     &         (id_fld, ioffset, nprocs_in, istack_merged(1))
      end if
      ioff_gl = ioff_gl + ilength*kint_gl
!
!
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_read_int                                  &
     &         (id_fld, ioffset, ione, itmp_IO)
        num_field = itmp_IO(1)
      end if
      ioff_gl = ioff_gl + kint
!
      call sync_field_header_mpi(nprocs_in, id_rank, nnod,              &
     &    num_field, istack_merged)
!
      end subroutine read_field_header_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_mpi_b                                   &
     &         (id_fld, ioff_gl, num_field, ncomp_field)
!
      use m_phys_constants
      use field_data_IO
      use field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_read_int                                  &
     &         (id_fld, ioffset, num_field, ncomp_field(1))
      end if
!
      ioff_gl = ioff_gl + num_field * kint
      call sync_field_comp_mpi(num_field, ncomp_field)
!
      end subroutine read_field_num_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_mpi_b(id_fld, nprocs_in, id_rank,      &
     &          ioff_gl, nnod, ncomp, vect, istack_merged)
!
      use m_phys_constants
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: nnod, ncomp
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      if(id_rank .ge. nprocs_in) return
      ioff_gl = ioff_gl + nprocs_in * kint_gl
!
      ioffset = int(ioff_gl + kreal * ncomp*istack_merged(id_rank))
      ilength = nnod * ncomp
      call calypso_mpi_seek_read_real                                   &
     &         (id_fld, ioffset, ilength, vect(1,1))
      ioff_gl = ioff_gl + kreal * ncomp*istack_merged(nprocs_in)
!
      end subroutine read_field_data_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_field_names_mpi_b                                 &
     &         (id_fld, ioff_gl, num_field, field_name)
!
      use m_phys_constants
      use field_data_IO
      use field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: num_field
      character(len=kchara), intent(inout) :: field_name(num_field)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      ilength = num_field * kchara
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_read_chara                                &
     &         (id_fld, ioffset, ilength, field_name(1))
      end if
      ioff_gl = ioff_gl + ilength
!
      call sync_field_names_mpi(num_field, field_name)
!
      end subroutine read_field_names_mpi_b
!
! -----------------------------------------------------------------------
!
      end module field_data_MPI_IO_b
