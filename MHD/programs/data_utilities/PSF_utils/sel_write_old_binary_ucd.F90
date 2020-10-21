!>@file  sel_write_old_binary_ucd.F90
!!       module sel_write_old_binary_ucd
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui on July, 2006
!!@n           Modified by H.Matsui on May, 2009
!
!> @brief UCD data IO selector
!!
!!@verbatim
!!      subroutine sel_read_old_udt_param                               &
!!     &         (id_rank, istep_ucd, ucd_param, ucd)
!!      subroutine sel_read_alloc_old_udt_file                          &
!!     &         (id_rank, istep_ucd, ucd_param, ucd)
!!      subroutine sel_read_alloc_old_ucd_file                          &
!!     &         (id_rank, istep_ucd, ucd_param, ucd)
!!      subroutine sel_read_old_udt_file                                &
!!     &         (id_rank, istep_ucd, ucd_param, ucd)
!!      subroutine sel_read_old_ucd_file                                &
!!     &         (id_rank, istep_ucd, ucd_param, ucd)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!!
!!@param id_rank  process ID
!!@param istep_ucd    step number for output
!
      module sel_write_old_binary_ucd
!
      use m_precision
      use m_constants
      use m_file_format_switch
      use m_field_file_format
!
      use ucd_field_file_IO_b
      use set_ucd_file_names
!
#ifdef ZLIB_IO
      use gz_ucd_field_file_IO_b
#endif
!
      use t_file_IO_parameter
      use t_time_data
      use t_ucd_data
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_wte_para_old_ucd_bin_file                          &
     &         (istep_ucd, ucd_param, ucd)
!
      use ucd_IO_select
      use write_udt_file_IO_b
      use ucd_field_MPI_IO_b
!
#ifdef ZLIB_IO
      use gz_ucd_field_MPI_IO_b
      use gz_write_udt_file_IO_b
#endif
!
      integer(kind=kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      if(istep_ucd .lt. 0) return
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, my_rank, istep_ucd)
!
      if(ucd_param%iflag_format .eq. iflag_ucd_bin) then
        call write_ucd_file_nostep_mpi_b(file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt_bin) then
        call write_ucd_phys_nostep_mpi_b(file_name, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_ucd_bin_gz) then
        call gz_write_ucd_file_notime_mpi_b(file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt_bin_gz) then
        call gz_write_ucd_phys_notime_mpi_b(file_name, ucd)
#endif
      end if
!
      end subroutine sel_wte_para_old_ucd_bin_file
!
!------------------------------------------------------------------
!
      subroutine sel_wte_para_old_ucd_bin_mesh(ucd_param, ucd)
!
      use ucd_IO_select
      use write_udt_file_IO_b
!
#ifdef ZLIB_IO
      use gz_write_udt_file_IO_b
#endif
!
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(in) :: ucd
!
      character(len=kchara) :: file_name
!
!
      file_name = set_parallel_grd_file_name                            &
     &        (ucd_param%file_prefix, ucd_param%iflag_format, my_rank)
!
      if(ucd_param%iflag_format .eq. iflag_udt_bin) then
        call write_ucd_grid_mpi_b(file_name, ucd)
!
#ifdef ZLIB_IO
      else if(ucd_param%iflag_format .eq. iflag_udt_bin_gz) then
        call gz_write_ucd_grid_mpi_b(file_name, ucd)
#endif
      end if
!
      end subroutine sel_wte_para_old_ucd_bin_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_ucd_file_nostep_mpi_b(file_name, ucd)
!
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
      use ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'write binary data by MPI-IO: ', trim(file_name) 
      call open_write_mpi_file_b(file_name, IO_param)
      call write_ucd_mesh_data_mpi_b                                    &
     &   (IO_param, ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie, &
     &    ucd%istack_merged_intnod)

      call mpi_write_process_id_b(IO_param)
      call write_ucd_data_mpi_b(IO_param,                               &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_intnod)
!
      call close_mpi_file(IO_param)
!
      end subroutine write_ucd_file_nostep_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_phys_nostep_mpi_b(file_name, ucd)
!
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
      use ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'write binary data by MPI-IO: ', trim(file_name) 
      call open_write_mpi_file_b(file_name, IO_param)
!
      call mpi_write_process_id_b(IO_param)
      call write_ucd_data_mpi_b(IO_param,                               &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_intnod)
!
      call close_mpi_file(IO_param)
!
      end subroutine write_ucd_phys_nostep_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_file_notime_mpi_b(gzip_name, ucd)
!
      use t_time_data
      use m_error_IDs
      use gz_MPI_binary_datum_IO
      use MPI_ascii_data_IO
      use gz_ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(ucd_data), intent(in) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &   'write gzipped binary mesh and field data by MPI-IO: ',        &
     &    trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param)
!
      call gz_mpi_write_ucd_mesh_data_b(IO_param,                       &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    ucd%istack_merged_intnod)
!
      call gz_mpi_write_process_id_b(IO_param)
      call gz_write_ucd_data_mpi_b(IO_param,                            &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_intnod)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_write_ucd_file_notime_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_phys_notime_mpi_b(gzip_name, ucd)
!
      use t_time_data
      use m_error_IDs
      use gz_MPI_binary_datum_IO
      use MPI_ascii_data_IO
      use gz_ucd_file_MPI_IO
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(ucd_data), intent(in) :: ucd
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &   'write gzipped binary field file by MPI-IO: ', trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param)
!
      call gz_mpi_write_process_id_b(IO_param)
      call gz_write_ucd_data_mpi_b(IO_param,                            &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, ucd%istack_merged_intnod)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_write_ucd_phys_notime_mpi_b
!
! -----------------------------------------------------------------------
!
      end module sel_write_old_binary_ucd
