!>@file   sel_repartition_table_IO.F90
!!      module sel_repartition_table_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2022
!
!>@brief File IO for communication table
!!
!!@verbatim
!!      subroutine sel_mpi_read_repart_tbl_file(repart_file, repart_IOs)
!!        type(field_IO_params), intent(in) ::  repart_file
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!      subroutine sel_mpi_write_repart_tbl_file(repart_file, repart_IOs)
!!        type(field_IO_params), intent(in) ::  repart_file
!!        type(repartition_tables_IO), intent(in) :: repart_IOs
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module sel_repartition_table_IO
!
      use m_precision
      use m_machine_parameter
      use m_file_format_switch
      use t_file_IO_parameter
      use t_repartition_tables_IO
!
      implicit none
!
      character(len=3), parameter, private :: rpt_ext = "rpt"
      character(len=3), parameter, private :: bpt_ext = "bpt"
!
      private :: set_repart_tbl_file_name
      private :: add_rpt_extension, add_bpt_extension
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_repart_tbl_file(repart_file, repart_IOs)
!
      use repart_table_file_IO
      use gz_repart_table_file_IO
      use MPI_repart_table_file_IO
      use gz_MPI_repart_table_file_IO
!
      type(field_IO_params), intent(in) ::  repart_file
      type(repartition_tables_IO), intent(inout) :: repart_IOs
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = set_repart_tbl_file_name(repart_file%file_prefix,     &
     &                               repart_file%iflag_format, my_rank)
!
      if(repart_file%iflag_format                                       &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_repart_tbl_file_b                                 &
     &     (nprocs, my_rank, file_name, repart_IOs)
      else if(repart_file%iflag_format .eq. iflag_single) then
        call mpi_read_repart_tbl_file                                   &
     &     (nprocs, my_rank, file_name, repart_IOs)
!
#ifdef ZLIB_IO
      else if(repart_file%iflag_format                                  &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_repart_tbl_file_b                              &
     &     (nprocs, my_rank, file_name, repart_IOs)
      else if(repart_file%iflag_format                                  &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_repart_tbl_file                                &
     &     (nprocs, my_rank, file_name, repart_IOs)
!
      else if(repart_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_read_repart_tbl_file_b                                  &
     &     (my_rank, file_name, repart_IOs)
      else if(repart_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_read_repart_tbl_file                                    &
     &     (my_rank, file_name, repart_IOs, ierr)
#endif
!
      else if (repart_file%iflag_format .eq. id_binary_file_fmt) then
        call read_repart_tbl_file_b                                     &
     &     (my_rank, file_name, repart_IOs, ierr)
      else
        call read_repart_tbl_file                                       &
     &     (my_rank, file_name, repart_IOs, ierr)
      end if 
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_repart_tbl_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_repart_tbl_file(repart_file, repart_IOs)
!
      use repart_table_file_IO
      use gz_repart_table_file_IO
      use MPI_repart_table_file_IO
      use gz_MPI_repart_table_file_IO
!
      type(field_IO_params), intent(in) ::  repart_file
      type(repartition_tables_IO), intent(in) :: repart_IOs
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = set_repart_tbl_file_name(repart_file%file_prefix,     &
     &                               repart_file%iflag_format, my_rank)
!
      if(repart_file%iflag_format                                       &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_repart_tbl_file_b(file_name, repart_IOs)
      else if(repart_file%iflag_format .eq. iflag_single) then
        call mpi_write_repart_tbl_file(file_name, repart_IOs)
!
#ifdef ZLIB_IO
      else if(repart_file%iflag_format                                  &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_repart_tbl_file_b(file_name, repart_IOs)
      else if(repart_file%iflag_format                                  &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_repart_tbl_file(file_name, repart_IOs)
!
      else if(repart_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_write_repart_tbl_file_b                                 &
     &     (my_rank, file_name, repart_IOs)
      else if(repart_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_write_repart_tbl_file                                   &
     &     (my_rank, file_name, repart_IOs)
#endif
!
      else if (repart_file%iflag_format .eq. id_binary_file_fmt) then
        call write_repart_tbl_file_b(my_rank, file_name,                &
     &                               repart_IOs, ierr)
      else
        call write_repart_tbl_file(my_rank, file_name, repart_IOs)
      end if
!
      end subroutine sel_mpi_write_repart_tbl_file
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &      set_repart_tbl_file_name(file_header, itype_file, id_rank)
!
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, file_header)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_bpt_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_rpt_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_bpt_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp = add_rpt_extension(file_name)
        file_name = fname_tmp
      end if
      set_repart_tbl_file_name = file_name
!
      end function set_repart_tbl_file_name
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function add_rpt_extension(file_head)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_rpt_extension = add_3chara_extension(file_head, rpt_ext)
!
      end function add_rpt_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_bpt_extension(file_head)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_bpt_extension = add_3chara_extension(file_head, bpt_ext)
!
      end function add_bpt_extension
!
!-----------------------------------------------------------------------
!
      end module sel_repartition_table_IO
