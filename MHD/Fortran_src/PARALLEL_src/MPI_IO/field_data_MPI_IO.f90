!>@file  field_data_MPI_IO.f90
!!       module field_data_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged field file using MPI-IO
!!
!!@verbatim
!!      subroutine sync_field_time_mpi(t_IO)
!!        type(time_data), intent(inout) :: t_IO
!!      subroutine sync_field_header_mpi                                &
!!     &         (num_pe, id_rank, istack_merged, nnod)
!!      subroutine sync_field_comp_mpi(num_field, ncomp_field)
!!      subroutine sync_field_name_mpi(ilength, field_name)
!!      subroutine sync_field_names_mpi(num_field, field_name)
!!
!!      subroutine read_field_time_mpi(id_fld, num_pe, ioff_gl, t_IO)
!!        type(time_data), intent(inout) :: t_IO
!!      subroutine read_field_header_mpi(id_fld, num_pe, id_rank,       &
!!     &           ioff_gl, nnod, num_field, istack_merged)
!!      subroutine read_field_num_mpi                                   &
!!     &         (id_fld, ioff_gl, num_field, ncomp_field)
!!
!!      subroutine read_field_name_mpi(id_fld, ioff_gl, field_name)
!!      subroutine skip_fld_vecotr_mpi(num_pe, id_rank, ioff_gl,        &
!!     &          ncomp, istack_merged)
!!
!!   Data format for the merged ascii field data
!!     1.   Number of process
!!     2.   Time step
!!     3.   Time, Delta t
!!     4.   Stacks of numbe of data points
!!     5.   Number of fields
!!     6.   List of number of components
!!     7.   Each field data  (Itarate 7.1 - 7.3)
!!      7.1   Field name
!!      7.2   List of data size (Byte)
!!      7.3   Field data
!!@endverbatim
!
      module field_data_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sync_field_time_mpi(t_IO)
!
      use t_time_data
      use calypso_mpi_real
      use calypso_mpi_int
!
      type(time_data), intent(inout) :: t_IO
!
!
      call calypso_mpi_bcast_one_int(t_IO%i_time_step, 0)
      call calypso_mpi_bcast_one_real(t_IO%time, 0)
      call calypso_mpi_bcast_one_real(t_IO%dt, 0)
!
      end subroutine sync_field_time_mpi
!
! -----------------------------------------------------------------------
!
      subroutine sync_field_header_mpi                                  &
     &         (num_pe, id_rank, istack_merged, nnod)
!
      integer, intent(in) :: num_pe, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:num_pe)
      integer(kind = kint_gl), intent(inout) ::  nnod
!
!
      if(id_rank .ge. num_pe) then
        nnod = 0
      else
        nnod = istack_merged(id_rank+1) - istack_merged(id_rank)
      end if
!
      end subroutine sync_field_header_mpi
!
! -----------------------------------------------------------------------
!
      subroutine sync_field_name_mpi(ilength, field_name)
!
      use calypso_mpi_int4
      use calypso_mpi_char
      use transfer_to_long_integers
!
      integer, intent(inout) :: ilength
      character(len=kchara), intent(inout) :: field_name
!
!
      call calypso_mpi_bcast_one_int4(ilength, 0)
      call calypso_mpi_bcast_character                                  &
     &   (field_name, cast_long(kchara), 0)
!
      end subroutine sync_field_name_mpi
!
! -----------------------------------------------------------------------
!
      subroutine sync_field_names_mpi(num_field, field_name)
!
     use calypso_mpi_char
     use transfer_to_long_integers
!
      integer(kind=kint), intent(in) :: num_field
      character(len=kchara), intent(inout) :: field_name(num_field)
!
!
      call calypso_mpi_bcast_character                                  &
     &   (field_name, cast_long(num_field*kchara), 0)
!
      end subroutine sync_field_names_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_field_name_mpi(id_fld, ioff_gl, field_name)
!
      use field_data_IO
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=kchara), intent(in) :: field_name
!
!
      call calypso_mpi_seek_write_head_c                                &
     &     (id_fld, ioff_gl, each_field_name_buffer(field_name))
!
      end subroutine write_field_name_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_time_mpi(id_fld, num_pe, ioff_gl, t_IO)
!
      use t_time_data
      use time_data_IO
      use field_data_IO
      use m_error_IDs
!
      type(time_data), intent(inout) :: t_IO
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer, intent(in) :: num_pe
!
      integer, intent(in) ::  id_fld
!
      character(len=len_step_data_buf) :: textbuf_c
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: iread
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call mpi_read_one_chara_b                                       &
     &     (id_fld, ioffset, len_step_data_buf, textbuf_c)
        call read_step_data_buffer(textbuf_c, iread, t_IO)
!
        if(num_pe .ne. iread) then
          call calypso_mpi_abort                                        &
     &       (ierr_fld, 'Set correct field data file')
        end if
!
      end if
      ioff_gl = ioff_gl + len_step_data_buf
!
      call sync_field_time_mpi(t_IO)
!
      end subroutine read_field_time_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_header_mpi(id_fld, num_pe, id_rank,         &
     &          ioff_gl, nnod, num_field, istack_merged)
!
      use m_phys_constants
      use calypso_mpi_int
      use calypso_mpi_int8
      use field_data_IO
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer, intent(in) :: num_pe, id_rank
!
      integer(kind = kint), intent(inout) :: num_field
      integer(kind = kint_gl), intent(inout) :: nnod
      integer(kind = kint_gl), intent(inout) :: istack_merged(0:num_pe)
!
      integer, intent(in) ::  id_fld
!
      character(len=31+1+16+1) ::        textbuf_c
      character(len=25+1+num_pe*16+1) :: textbuf_d
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
      integer(kind = kint_gl) :: num64
!
!
      ilength = int(len(textbuf_d))
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call mpi_read_one_chara_b                                       &
     &     (id_fld, ioffset, ilength, textbuf_d)
        call read_field_istack_nod_buffer                               &
     &     (textbuf_d, num_pe, istack_merged)
      end if
      ioff_gl = ioff_gl + ilength
!
      ilength = len(textbuf_c)
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call mpi_read_one_chara_b                                       &
     &       (id_fld, ioffset, ilength, textbuf_c)
        call read_field_num_buffer(textbuf_c, num_field)
      end if
      ioff_gl = ioff_gl + ilength
!
      num64 = int(num_pe+1,KIND(num64))
      call calypso_mpi_bcast_int8(istack_merged, num64 , 0)
      call calypso_mpi_bcast_one_int(num_field, 0)
!
      call sync_field_header_mpi(num_pe, id_rank, istack_merged, nnod)
!
      end subroutine read_field_header_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_mpi                                     &
     &         (id_fld, ioff_gl, num_field, ncomp_field)
!
      use m_phys_constants
      use calypso_mpi_int
      use field_data_IO
      use ucd_data_to_buffer
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
      integer, intent(in) ::  id_fld
!
      character(len=num_field*5+1) :: charabuf_c
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
!
      ilength = len(charabuf_c)
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call mpi_read_one_chara_b                                       &
     &       (id_fld, ioffset, ilength, charabuf_c)
        call read_field_comp_buffer(charabuf_c, num_field, ncomp_field)
      end if
      ioff_gl = ioff_gl + ilength
!
      call calypso_mpi_bcast_int(ncomp_field, cast_long(num_field), 0)
!
      end subroutine read_field_num_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_name_mpi(id_fld, ioff_gl, field_name)
!
      use m_phys_constants
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=kchara), intent(inout) :: field_name
!
      integer, intent(in) ::  id_fld
!
      character(len=kchara+1) :: textbuf_c
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        ilength = int(len(textbuf_c))
        call mpi_read_one_chara_b                                       &
     &     (id_fld, ioffset, ilength, textbuf_c)
        call read_each_field_name_buffer                                &
     &     (textbuf_c, field_name, ilength)
      end if
!
      call sync_field_name_mpi(ilength, field_name)
      ioff_gl = ioff_gl + ilength + 1
!
      end subroutine read_field_name_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine skip_fld_vecotr_mpi(num_pe, id_rank, ioff_gl,          &
     &          ncomp, istack_merged)
!
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer, intent(in) :: num_pe, id_rank
      integer(kind = kint_gl), intent(in) :: istack_merged(0:num_pe)
      integer(kind = kint), intent(in) :: ncomp
!
!
      if(id_rank .ge. num_pe) return
!   Skip buffer size
      ioff_gl = ioff_gl                                                 &
     &       + len(buffer_istack_nod_buffer(num_pe, istack_merged))
      ioff_gl = ioff_gl + (ncomp*25+1) * istack_merged(num_pe)
!
      end subroutine skip_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      end module field_data_MPI_IO
