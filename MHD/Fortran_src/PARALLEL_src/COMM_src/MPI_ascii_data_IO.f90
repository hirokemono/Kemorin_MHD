!>@file  MPI_ascii_data_IO.f90
!!       module MPI_ascii_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine open_write_mpi_file                                  &
!!     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!!        Substitution of open_wt_gzfile_b
!!      subroutine open_append_mpi_file                                 &
!!     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!!      subroutine open_read_mpi_file                                   &
!!     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!!        Substitution of open_rd_gzfile_b
!!      subroutine close_mpi_file(IO_param)
!!
!!      subroutine mpi_write_charahead(IO_param, ilength, chara_dat)
!!      subroutine mpi_write_characters(IO_param, ilength, chara_dat)
!!      function  mpi_read_charahead(IO_param, ilength)
!!        character(len=ilength) :: mpi_read_charahead
!!      function mpi_read_characters(IO_param, ilength)
!!        character(len=ilength) :: mpi_read_characters
!!      subroutine mpi_skip_read(IO_param, ilength)
!!@endverbatim
!
      module MPI_ascii_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_write_mpi_file                                    &
     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call alloc_istack_merge(my_rank_IO, nprocs_in, IO_param)
      call calypso_mpi_write_file_open                                  &
     &   (file_name, IO_param%nprocs_in, IO_param%id_file)
      IO_param%ioff_gl = izero
!
      end subroutine open_write_mpi_file
!
!  ---------------------------------------------------------------------
!
      subroutine open_append_mpi_file                                   &
     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call alloc_istack_merge(my_rank_IO, nprocs_in, IO_param)
      call calypso_mpi_write_file_open                                  &
     &   (file_name, IO_param%nprocs_in, IO_param%id_file)
      call MPI_File_get_byte_offset(IO_param%id_file, IO_param%ioff_gl)
!
      end subroutine open_append_mpi_file
!
!  ---------------------------------------------------------------------
!
      subroutine open_read_mpi_file                                     &
     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call alloc_istack_merge(my_rank_IO, nprocs_in, IO_param)
      call calypso_mpi_read_file_open(file_name, IO_param%id_file)
      IO_param%ioff_gl = izero
!
      end subroutine open_read_mpi_file
!
! -----------------------------------------------------------------------
!
      subroutine close_mpi_file(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call calypso_close_mpi_file(IO_param%id_file)
      call dealloc_istack_merge(IO_param)
!
      end subroutine close_mpi_file
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_charahead(IO_param, ilength, chara_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: ilength
      character(len=1), intent(in) :: chara_dat(ilength)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ilength, chara_dat(1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ilength
!
      end subroutine mpi_write_charahead
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_characters(IO_param, ilength, chara_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: ilength
      character(len=1), intent(in) :: chara_dat(ilength)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(ilength .le. 0) return
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        ioffset = IO_param%ioff_gl                                      &
     &         + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ilength, chara_dat(1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_write_characters
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      function  mpi_read_charahead(IO_param, ilength)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength) :: mpi_read_charahead
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_read_gz(IO_param%id_file,                 &
     &      ioffset, ilength, mpi_read_charahead)
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl + ilength
      call MPI_BCAST(mpi_read_charahead, ilength, CALYPSO_CHARACTER,    &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
      end function mpi_read_charahead
!
! -----------------------------------------------------------------------
!
      function mpi_read_characters(IO_param, ilength)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength) :: mpi_read_characters
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(ilength .le. 0) return
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_read_gz                                   &
     &     (IO_param%id_file, ioffset, ilength, mpi_read_characters)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end function mpi_read_characters
!
! -----------------------------------------------------------------------
!
      subroutine mpi_sub_read_characters(IO_param, ilength, texts)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength), intent(inout) :: texts
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(ilength .le. 0) return
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_read_gz                                   &
     &     (IO_param%id_file, ioffset, ilength, texts)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine mpi_sub_read_characters
!
! -----------------------------------------------------------------------
!
      subroutine mpi_skip_read(IO_param, ilength)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: ilength
!
!
      IO_param%ioff_gl = IO_param%ioff_gl + ilength
!
      end subroutine mpi_skip_read
!
! -----------------------------------------------------------------------
!
      end module MPI_ascii_data_IO
