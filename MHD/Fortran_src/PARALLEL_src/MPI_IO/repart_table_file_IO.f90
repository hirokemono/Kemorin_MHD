!>@file  repart_table_file_IO.f90
!!      module repart_table_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2022
!
!>@brief File IO for communication table
!!
!!@verbatim
!!      subroutine read_repart_tbl_file(id_rank, file_name,             &
!!     &                                repart_IOs, ierr)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine write_repart_tbl_file(id_rank, file_name, repart_IOs)
!!        character(len=kchara), intent(in) :: file_name
!!        integer, intent(in) :: id_rank
!!        type(repartition_tables_IO), intent(in) :: repart_IOs
!!
!!      subroutine read_repart_tbl_file_b(id_rank, file_name,           &
!!     &                                  repart_IOs, ierr)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine write_repart_tbl_file_b(id_rank, file_name,          &
!!     &                                   repart_IOs, ierr)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(in) :: repart_IOs
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module repart_table_file_IO
!
      use m_precision
      use m_machine_parameter
      use t_binary_IO_buffer
      use t_repartition_tables_IO
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_file_code = 21
      type(binary_IO_buffer), private :: bbuf_c
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_repart_tbl_file(id_rank, file_name,               &
     &                                repart_IOs, ierr)
!
      use comm_table_IO
      use skip_comment_f
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(inout) :: repart_IOs
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=255) :: character_4_read
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read ascii element comm file: ', trim(file_name)
!
      open(id_file_code, file = file_name, form = 'formatted')
      call read_calypso_comm_tbl(id_file_code, id_rank,                 &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export,   &
     &    ierr)
      if(ierr .ne. 0) goto 99
!
      call skip_comment(id_file_code, character_4_read, ierr)
      if(ierr .ne. 0) goto 99
      read(character_4_read,*) repart_IOs%new_numele
      call read_calypso_comm_tbl(id_file_code, id_rank,                 &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export,   &
     &    ierr)
      if(ierr .ne. 0) goto 99
!
      call read_comm_table                                              &
     &   (id_file_code, id_rank, repart_IOs%nod_comm_IO, ierr)
      if(ierr .ne. 0) goto 99
      call read_comm_table                                              &
     &   (id_file_code, id_rank, repart_IOs%ele_comm_IO, ierr)
!
  99  continue
      close(id_file_code)
!
      end subroutine read_repart_tbl_file
!
!------------------------------------------------------------------
!
      subroutine write_repart_tbl_file(id_rank, file_name, repart_IOs)
!
      use comm_table_IO
      use m_fem_mesh_labels
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(repartition_tables_IO), intent(in) :: repart_IOs
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii element comm file: ', trim(file_name)
!
      open(id_file_code, file = file_name, form = 'formatted')
      call write_calypso_comm_tbl(id_file_code, id_rank,                &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export)
!
      write(id_file_code,'(a)', advance='NO') hd_fem_elem()
      write(id_file_code,'(i16)') repart_IOs%new_numele
      call write_calypso_comm_tbl(id_file_code, id_rank,                &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
!
      call write_comm_table(id_file_code, id_rank,                      &
     &    repart_IOs%nod_comm_IO)
      call write_comm_table(id_file_code, id_rank,                      &
     &    repart_IOs%ele_comm_IO)
      close(id_file_code)
!
      end subroutine write_repart_tbl_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_repart_tbl_file_b(id_rank, file_name,             &
     &                                  repart_IOs, ierr)
!
      use binary_IO
      use comm_table_IO_b
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(inout) :: repart_IOs
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read binary element comm file: ', trim(file_name)
!
      bbuf_c%id_binary = id_file_code
      call open_read_binary_file(file_name, id_rank, bbuf_c)
      if(bbuf_c%ierr_bin .ne. 0) goto 99
      call read_calypso_comm_tbl_b(id_rank, bbuf_c,                     &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export)
      if(bbuf_c%ierr_bin .ne. 0) goto 99
!
      call read_one_integer_b(bbuf_c, repart_IOs%new_numele)
      call read_calypso_comm_tbl_b(id_rank, bbuf_c,                     &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
      if(bbuf_c%ierr_bin .ne. 0) goto 99
!
      call read_comm_table_b(id_rank, bbuf_c, repart_IOs%nod_comm_IO)
      if(bbuf_c%ierr_bin .ne. 0) goto 99
      call read_comm_table_b(id_rank, bbuf_c, repart_IOs%ele_comm_IO)
!
  99  continue
      call close_binary_file(bbuf_c)
      ierr = bbuf_c%ierr_bin
!
      end subroutine read_repart_tbl_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_repart_tbl_file_b(id_rank, file_name,            &
     &                                   repart_IOs, ierr)
!
      use binary_IO
      use comm_table_IO_b
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(in) :: repart_IOs
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write binary element comm file: ', trim(file_name)
!
      bbuf_c%id_binary = id_file_code
      call open_write_binary_file(file_name, bbuf_c)
      if(bbuf_c%ierr_bin .gt. 0) go to 99
      call write_calypso_comm_tbl_b(id_rank,                            &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export,   &
     &    bbuf_c)
      if(bbuf_c%ierr_bin .gt. 0) go to 99
!
      call write_one_integer_b(repart_IOs%new_numele, bbuf_c)
      if(bbuf_c%ierr_bin .gt. 0) go to 99
      call write_calypso_comm_tbl_b(id_rank,                            &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export,   &
     &    bbuf_c)
      if(bbuf_c%ierr_bin .gt. 0) go to 99
!
      call write_comm_table_b(id_rank, repart_IOs%nod_comm_IO, bbuf_c)
      if(bbuf_c%ierr_bin .gt. 0) go to 99
      call write_comm_table_b(id_rank, repart_IOs%ele_comm_IO, bbuf_c)
!
  99  continue
      call close_binary_file(bbuf_c)
      ierr = bbuf_c%ierr_bin
!
      end subroutine write_repart_tbl_file_b
!
!-----------------------------------------------------------------------
!
      end module repart_table_file_IO
