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
      private :: write_step_data_mpi_b, write_field_nod_mpi_b
      private :: write_field_num_mpi_b, write_fld_vecotr_mpi_b
      private :: read_step_data_mpi_b, read_field_header_mpi_b
      private :: read_field_num_mpi_b, read_field_data_mpi_b
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
      use field_file_MPI_IO
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
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_step_data_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl)
      call sync_field_time_mpi
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl, &
     &      fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
      call sync_field_header_mpi(nprocs_in, id_rank, fld_IO%nnod_IO,    &
     &      fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call read_field_num_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl,    &
     &      fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call sync_field_comp_mpi(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_names_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl,  &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
      call sync_field_names_mpi(fld_IO%num_field_IO, fld_IO%fld_name)
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
      use field_file_MPI_IO
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
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_step_data_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl)
      call sync_field_time_mpi
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl, &
     &      fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
      call sync_field_header_mpi(nprocs_in, id_rank, fld_IO%nnod_IO,    &
     &      fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call read_field_num_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl,    &
     &      fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call sync_field_comp_mpi(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_names_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl,  &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
      call sync_field_names_mpi(fld_IO%num_field_IO, fld_IO%fld_name)
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
      use field_file_MPI_IO
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
      call calypso_mpi_read_file_open(file_name, id_fld)
!
      ioff_gl = 0
      call read_step_data_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl)
      call sync_field_time_mpi
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call read_field_header_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl, &
     &      fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
      call sync_field_header_mpi(nprocs_in, id_rank, fld_IO%nnod_IO,    &
     &      fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call read_field_num_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl,    &
     &      fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call sync_field_comp_mpi(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_names_mpi_b(id_fld, nprocs_in, id_rank, ioff_gl,  &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
      call sync_field_names_mpi(fld_IO%num_field_IO, fld_IO%fld_name)
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
      use field_data_IO
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
      call write_field_nod_mpi_b                                        &
     &   (id_fld, nprocs_in, ioff_gl, num_field, istack_merged)
      call write_field_num_mpi_b                                        &
     &   (id_fld, ioff_gl, num_field, ncomp_field)
!
      call write_fld_vecotr_mpi_b                                       &
     &   (id_fld, nprocs_in, id_rank, ioff_gl, nnod,                    &
     &    num_field, ntot_comp, field_name, d_nod, istack_merged)
!
      end subroutine write_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
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
      itmp_IO(1) = nprocs_in - 1
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
      subroutine write_field_nod_mpi_b                                  &
     &         (id_fld, nprocs_in, ioff_gl, num_field, istack_merged)
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind = kint), intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: num_field
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
      end subroutine write_field_nod_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine write_field_num_mpi_b                                  &
     &         (id_fld, ioff_gl, num_field, ncomp_field)
!
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) ::  id_fld
!
!
      call calypso_mpi_seek_write_head_i                                &
     &    (id_fld, ioff_gl, num_field, ncomp_field(1))
!
      end subroutine write_field_num_mpi_b
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
      subroutine read_step_data_mpi_b                                   &
     &          (id_fld, nprocs_in, id_rank, ioff_gl)
!
      use m_time_data_IO
      use m_error_IDs
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength, iread
      integer(kind = kint) :: itmp_IO(1)
      real(kind = kreal) ::   rtmp_IO(1)
!
!
      if(id_rank .ge. nprocs_in) return
      ilength = ione
      if(my_rank .eq. 0) then
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
        if(nprocs_in .ne.(iread + 1)) then
          call calypso_mpi_abort                                        &
     &       (ierr_fld, 'Set correct field data file')
        end if
      end if
      ioff_gl = ioff_gl + 2*kint + 2*kreal
!
      end subroutine read_step_data_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_field_header_mpi_b(id_fld, nprocs_in, id_rank,    &
     &          ioff_gl, num_field, istack_merged)
!
      use m_phys_constants
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(inout) :: num_field
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
      if(id_rank .ge. nprocs_in) return
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
      end subroutine read_field_header_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_mpi_b(id_fld, nprocs_in, id_rank,       &
     &          ioff_gl, num_field, ncomp_field)
!
      use m_phys_constants
      use field_data_IO
      use ucd_data_to_buffer
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(id_rank .ge. nprocs_in) return
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_read_int                                  &
     &         (id_fld, ioffset, num_field, ncomp_field(1))
      end if
      ioff_gl = ioff_gl + num_field * kint
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
      subroutine read_field_names_mpi_b(id_fld, nprocs_in, id_rank,     &
     &          ioff_gl, num_field, field_name)
!
      use m_phys_constants
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(in) :: num_field
      character(len=kchara), intent(inout) :: field_name(num_field)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      if(id_rank .ge. nprocs_in) return
      ilength = num_field * kchara
      if(id_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_read_chara                                &
     &         (id_fld, ioffset, ilength, field_name(1))
      end if
      ioff_gl = ioff_gl + ilength
!
      end subroutine read_field_names_mpi_b
!
! -----------------------------------------------------------------------
!
      end module field_file_MPI_IO_b
