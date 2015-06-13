!>@file  gz_field_file_MPI_IO_b.f90
!!       module gz_field_file_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_step_fld_file_mpi_b                         &
!!     &         (file_name, nprocs_in, fld_IO)
!!
!!      subroutine gz_write_field_head_mpi_b(id_fld, ioff_gl, nprocs_in,&
!!     &          num_field, ncomp_field, istack_merged)
!!      subroutine gz_write_field_data_mpi_b(id_fld, ioff_gl,           &
!!     &          nnod, num_field, ntot_comp, field_name, d_nod)
!!      subroutine gz_read_step_field_file_mpi_b                        &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
!!      subroutine gz_rd_alloc_st_fld_file_mpi_b                        &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
!!
!!      subroutine gz_rd_alloc_st_fld_head_mpi_b                        &
!!     &         (file_name, nprocs_in, id_rank, fld_IO)
!!@endverbatim
!
      module gz_field_file_MPI_IO_b
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
      private :: gz_write_field_data_mpi_b
      private :: gz_read_field_data_mpi_b, gz_read_field_names_mpi_b
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_step_fld_file_mpi_b                           &
     &         (file_name, nprocs_in, fld_IO)
!
      use m_error_IDs
      use gz_field_data_MPI_IO_b
!
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nprocs_in
      type(field_IO), intent(in) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(nprocs .ne. nprocs_in)  call calypso_mpi_abort                 &
     &                (ierr_fld, 'gzipped data output does not work')
!
      call calypso_mpi_write_file_open(file_name, nprocs_in, id_fld)
!
      ioff_gl = 0
      call gz_write_field_head_mpi_b(id_fld, nprocs, ioff_gl,           &
     &      fld_IO%num_field_IO, fld_IO%num_comp_IO,                    &
     &      fld_IO%istack_numnod_IO)
      call gz_write_field_data_mpi_b(id_fld, ioff_gl,                   &
     &      fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,   &
     &      fld_IO%fld_name, fld_IO%d_IO)
!
      call calypso_close_mpi_file(id_fld)
!
      end subroutine gz_write_step_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_field_file_mpi_b                          &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use field_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call open_rd_gzfile(file_name)
!
      call gz_read_step_data_mpi_b(nprocs_in, id_rank)
      call sync_field_time_mpi
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call gz_read_field_header_mpi(nprocs_in, id_rank,                 &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
      call sync_field_header_mpi(nprocs_in, id_rank, fld_IO%nnod_IO,    &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call gz_read_field_num_mpi_b(nprocs_in, id_rank,                  &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call sync_field_comp_mpi(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call gz_read_field_names_mpi_b(nprocs_in, id_rank,                &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
      call sync_field_names_mpi(fld_IO%num_field_IO, fld_IO%fld_name)
!
      call gz_read_field_data_mpi_b(nprocs_in, id_rank,                 &
     &      fld_IO%nnod_IO, fld_IO%ntot_comp_IO, fld_IO%d_IO,           &
     &      fld_IO%istack_numnod_IO)
      call dealloc_merged_field_stack(fld_IO)
!
      call close_gzfile()
!
      end subroutine gz_read_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_file_mpi_b                          &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use field_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call open_rd_gzfile(file_name)
!
      call gz_read_step_data_mpi_b(nprocs_in, id_rank)
      call sync_field_time_mpi
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call gz_read_field_header_mpi(nprocs_in, id_rank,                 &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
      call sync_field_header_mpi(nprocs_in, id_rank, fld_IO%nnod_IO,    &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_field_num_mpi_b(nprocs_in, id_rank,                  &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call sync_field_comp_mpi(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
      write(*,*) 'gz_read_field_data_mpi_b', id_rank, fld_IO%nnod_IO
      call gz_read_field_names_mpi_b(nprocs_in, id_rank,                &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
      call sync_field_names_mpi(fld_IO%num_field_IO, fld_IO%fld_name)
!
      call gz_read_field_data_mpi_b(nprocs_in, id_rank,                 &
     &      fld_IO%nnod_IO, fld_IO%ntot_comp_IO, fld_IO%d_IO,           &
     &      fld_IO%istack_numnod_IO)
!
      call close_gzfile()
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. nprocs_in) then
        call dealloc_phys_data_IO(fld_IO)
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine gz_rd_alloc_st_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_head_mpi_b                          &
     &         (file_name, nprocs_in, id_rank, fld_IO)
!
      use field_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank
      integer(kind=kint), intent(in) :: nprocs_in
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call open_rd_gzfile(file_name)
!
      call gz_read_step_data_mpi_b(nprocs_in, id_rank)
      call sync_field_time_mpi
!
      call alloc_merged_field_stack(nprocs_in, fld_IO)
!
      call gz_read_field_header_mpi(nprocs_in, id_rank,                 &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
      call sync_field_header_mpi(nprocs_in, id_rank, fld_IO%nnod_IO,    &
     &    fld_IO%num_field_IO, fld_IO%istack_numnod_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_field_num_mpi_b(nprocs_in, id_rank,                  &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO)
      call sync_field_comp_mpi(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call gz_read_field_names_mpi_b(nprocs_in, id_rank,                &
     &    fld_IO%num_field_IO, fld_IO%fld_name)
      call sync_field_names_mpi(fld_IO%num_field_IO, fld_IO%fld_name)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      call close_gzfile()
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. nprocs_in) then
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine gz_rd_alloc_st_fld_head_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_field_head_mpi_b(id_fld, nprocs_in, ioff_gl,  &
     &          num_field, ncomp_field, istack_merged)
!
      use m_phys_constants
      use m_time_data_IO
      use field_data_IO
      use gz_field_data_MPI_IO_b
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind=kint), intent(in) :: nprocs_in
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
!
      integer, intent(in) ::  id_fld
!
!
      call gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, nprocs_in-1)
      call gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, i_time_step_IO)
!
      call gz_write_fld_realhead_mpi_b(id_fld, ioff_gl, time_IO)
      call gz_write_fld_realhead_mpi_b(id_fld, ioff_gl, delta_t_IO)
!
!
      call gz_write_fld_mul_i8head_mpi_b                                &
     &         (id_fld, ioff_gl, nprocs_in, istack_merged(1))
      call gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, num_field)
      call gz_write_fld_mul_inthead_mpi_b                               &
     &         (id_fld, ioff_gl, num_field, ncomp_field)
!
      end subroutine gz_write_field_head_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_field_data_mpi_b(id_fld, ioff_gl,             &
     &          nnod, num_field, ntot_comp, field_name, d_nod)
!
      use m_phys_constants
      use field_data_IO
      use gz_field_data_MPI_IO_b
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_fld
!
!
      call gz_write_fld_mul_charhead_mpi_b                              &
     &   (id_fld, ioff_gl, num_field, field_name)
      call gz_write_fld_realarray2_mpi_b                                &
     &   (id_fld, ioff_gl, nnod, ntot_comp, d_nod)
!
      end subroutine gz_write_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_data_mpi_b(nprocs_in, id_rank)
!
      use m_time_data_IO
      use gz_field_data_MPI_IO_b
      use m_error_IDs
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind=kint) :: iread
!
!
      if(id_rank .ge. nprocs_in) return
      call gz_read_fld_inthead_mpi_b(iread)
      if(my_rank .eq. 0 .and. nprocs_in .ne. (iread + 1)) then
        call calypso_mpi_abort(ierr_fld, 'Set correct field data file')
      end if
!
      call gz_read_fld_inthead_mpi_b(i_time_step_IO)
      call gz_read_fld_realhead_mpi_b(time_IO)
      call gz_read_fld_realhead_mpi_b(delta_t_IO)
!
      end subroutine gz_read_step_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_field_header_mpi(nprocs_in, id_rank,           &
     &          num_field, istack_merged)
!
      use gz_field_data_MPI_IO_b
      use m_phys_constants
      use field_data_IO
!
      integer(kind=kint), intent(in) :: nprocs_in, id_rank
!
      integer(kind=kint), intent(inout) :: num_field
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
!
      if(id_rank .ge. nprocs_in) return
      call gz_read_fld_mul_i8head_mpi_b(nprocs_in, istack_merged(1))
      call gz_read_fld_inthead_mpi_b(num_field)
!
      end subroutine gz_read_field_header_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_field_num_mpi_b(nprocs_in, id_rank,            &
     &           num_field, ncomp_field)
!
      use m_phys_constants
      use field_data_IO
      use gz_field_data_MPI_IO_b
!
      integer(kind=kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
!
      if(id_rank .ge. nprocs_in) return
      call gz_read_fld_mul_inthead_mpi_b(num_field, ncomp_field)
!
      end subroutine gz_read_field_num_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_field_data_mpi_b(nprocs_in, id_rank, nnod,     &
     &          ncomp, vect, istack_merged)
!
      use m_phys_constants
      use gz_field_data_MPI_IO_b
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs_in)
      integer(kind=kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(in) :: nnod, ncomp
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
!
      if(id_rank .ge. nprocs_in) return
      call gz_read_fld_realarray2_mpi_b                                 &
     &   (nprocs_in, id_rank, nnod, ncomp, vect, istack_merged)
!
      end subroutine gz_read_field_data_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_field_names_mpi_b(nprocs_in, id_rank,          &
     &          num_field, field_name)
!
      use m_phys_constants
      use gz_field_data_MPI_IO_b
!
      integer(kind=kint), intent(in) :: nprocs_in, id_rank
      integer(kind=kint), intent(in) :: num_field
      character(len=kchara), intent(inout) :: field_name(num_field)
!
!
      if(id_rank .ge. nprocs_in) return
      call gz_read_fld_mul_charhead_mpi_b(num_field, field_name)
!
      end subroutine gz_read_field_names_mpi_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_file_MPI_IO_b
