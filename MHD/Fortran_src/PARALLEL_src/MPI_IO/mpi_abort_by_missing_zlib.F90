!> @file  mpi_abort_by_missing_zlib.F90
!!      module mpi_abort_by_missing_zlib
!!
!!@author  H. Matsui
!!@date Programmed in Sep., 2021
!
!> @brief Stop program if zlib is not loaded
!!
!!@verbatim
!!      subroutine s_mpi_abort_by_missing_zlib(file_prefix, id_file_fmt)
!!        character(len=kchara), intent(in) :: file_prefix
!!        integer(kind= kint), intent(inout) :: id_file_fmt
!!@endverbatim
!
      module mpi_abort_by_missing_zlib
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_mpi_abort_by_missing_zlib(file_prefix, id_file_fmt)
!
      use m_error_ids
      use m_file_format_switch
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind= kint), intent(inout) :: id_file_fmt
!
!
#ifndef ZLIB_IO
      if     (id_file_fmt .eq. id_gzip_txt_file_fmt                     &
     &   .or. id_file_fmt .eq. id_gzip_bin_file_fmt                     &
     &   .or. id_file_fmt .eq. mgd_gzip_flags                           &
     &   .or. id_file_fmt .eq. mgd_gzip_bin_flags) then
        id_file_fmt = id_missing_zlib
        if(my_rank .eq 0) write(*,*) 'Zlib is not linked!'
      end if
#endif
!
      if(id_file_fmt .ne. id_missing_zlib) return
        write(e_message,'(2a)') 'Failed file prefix: ',                 &
     &                         trim(file_prefix)
        call calypso_mpi_abort(ierr_file, e_message)
!
      end subroutine s_mpi_abort_by_missing_zlib
!
! -----------------------------------------------------------------------
!
      end module mpi_abort_by_missing_zlib
