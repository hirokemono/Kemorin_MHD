!>@file  gz_repart_table_file_IO.f90
!!      module gz_repart_table_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2022
!
!>@brief File IO for communication table
!!
!!@verbatim
!!      subroutine gz_read_repart_tbl_file(id_rank, file_name,          &
!!     &                                   repart_IOs, ierr)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine gz_write_repart_tbl_file                             &
!!     &         (id_rank, file_name, repart_IOs)
!!        character(len=kchara), intent(in) :: file_name
!!        integer, intent(in) :: id_rank
!!        type(repartition_tables_IO), intent(in) :: repart_IOs
!!
!!      subroutine gz_read_repart_tbl_file_b(id_rank, file_name,        &
!!     &                                     repart_IOs)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!      subroutine gz_write_repart_tbl_file_b(id_rank, file_name,       &
!!     &                                      repart_IOs)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(in) :: repart_IOs
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module gz_repart_table_file_IO
!
      use m_precision
      use m_machine_parameter
      use t_buffer_4_gzip
      use t_repartition_tables_IO
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_c
      character, pointer, private, save :: FPz_c
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_repart_tbl_file(id_rank, file_name,            &
     &                                   repart_IOs, ierr)
!
      use gz_comm_table_IO
      use gzip_file_access
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(inout) :: repart_IOs
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped ascii element comm file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_c, file_name, zbuf_c)
      call gz_read_calypso_comm_tbl(FPz_c, id_rank, zbuf_c,             &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export,   &
     &    ierr)
!
      call skip_gz_comment_int(FPz_c, repart_IOs%new_numele, zbuf_c)
      call gz_read_calypso_comm_tbl(FPz_c, id_rank, zbuf_c,             &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export,   &
     &    ierr)
!
      call gz_read_comm_table(FPz_c, id_rank,                           &
     &    repart_IOs%nod_comm_IO, zbuf_c, ierr)
      call gz_read_comm_table(FPz_c, id_rank,                           &
     &    repart_IOs%ele_comm_IO, zbuf_c, ierr)
      call close_gzfile_a(FPz_c, zbuf_c)
!
      end subroutine gz_read_repart_tbl_file
!
!------------------------------------------------------------------
!
      subroutine gz_write_repart_tbl_file                               &
     &         (id_rank, file_name, repart_IOs)
!
      use gz_comm_table_IO
      use gzip_file_access
      use skip_gz_comment
      use m_fem_mesh_labels
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(in) :: repart_IOs
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped ascii element comm file: ', trim(file_name)
!
      call open_wt_gzfile_a(FPz_c, file_name, zbuf_c)
      call gz_write_calypso_comm_tbl(FPz_c, id_rank,                    &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export,   &
     &    zbuf_c)
!
      zbuf_c%fixbuf(1) = hd_fem_elem() // char(0)
      call gz_write_textbuf_no_lf(FPz_c, zbuf_c)
      write(zbuf_c%fixbuf(1),'(i16,2a1)')                               &
     &                        repart_IOs%new_numele, char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_c, zbuf_c)
!
      call gz_write_calypso_comm_tbl(FPz_c, id_rank,                    &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export,   &
     &    zbuf_c)
!
      call gz_write_comm_table(FPz_c, id_rank,                          &
     &    repart_IOs%nod_comm_IO, zbuf_c)
      call gz_write_comm_table(FPz_c, id_rank,                          &
     &    repart_IOs%ele_comm_IO, zbuf_c)
      call close_gzfile_a(FPz_c, zbuf_c)
!
      end subroutine gz_write_repart_tbl_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_repart_tbl_file_b(id_rank, file_name,          &
     &                                     repart_IOs)
!
      use gz_comm_table_IO_b
      use gz_binary_IO
      use gzip_file_access
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(inout) :: repart_IOs
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped binary element comm file: ', trim(file_name)
!
      call open_rd_gzfile_b(FPz_c, file_name, id_rank, zbuf_c)
      if(zbuf_c%ierr_zlib .ne. 0) go to 99
      call gz_read_calypso_comm_tbl_b(FPz_c, id_rank, zbuf_c,           &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export)
      if(zbuf_c%ierr_zlib .ne. 0) go to 99
!
      call gz_read_one_integer_b(FPz_c, zbuf_c, repart_IOs%new_numele)
      if(zbuf_c%ierr_zlib .ne. 0) go to 99
      call gz_read_calypso_comm_tbl_b(FPz_c, id_rank, zbuf_c,           &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
      if(zbuf_c%ierr_zlib .ne. 0) go to 99
!
      call gz_read_comm_table_b(FPz_c, id_rank, zbuf_c,                 &
     &                          repart_IOs%nod_comm_IO)
      if(zbuf_c%ierr_zlib .ne. 0) go to 99
      call gz_read_comm_table_b(FPz_c, id_rank, zbuf_c,                 &
     &                          repart_IOs%ele_comm_IO)
  99  continue
      call close_gzfile_b(FPz_c)
!
      end subroutine gz_read_repart_tbl_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_repart_tbl_file_b(id_rank, file_name,         &
     &                                      repart_IOs)
!
      use gz_comm_table_IO_b
      use gz_binary_IO
      use gzip_file_access
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(in) :: repart_IOs
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped binary element comm file: ', trim(file_name)
!
      call open_wt_gzfile_b(FPz_c, file_name, zbuf_c)
      call gz_write_calypso_comm_tbl_b(FPz_c, id_rank,                  &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export,   &
     &    zbuf_c)
!
      call gz_write_one_integer_b(FPz_c, repart_IOs%new_numele, zbuf_c)
      call gz_write_calypso_comm_tbl_b(FPz_c, id_rank,                  &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export,   &
     &    zbuf_c)
!
      call gz_write_comm_table_b(FPz_c, id_rank,                        &
     &    repart_IOs%nod_comm_IO, zbuf_c)
      call gz_write_comm_table_b(FPz_c, id_rank,                        &
     &    repart_IOs%ele_comm_IO, zbuf_c)
      call close_gzfile_b(FPz_c)
!
      end subroutine gz_write_repart_tbl_file_b
!
!------------------------------------------------------------------
!
      end module gz_repart_table_file_IO
