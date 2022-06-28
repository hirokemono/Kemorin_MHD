!>@file  sel_read_old_binary_ucd.F90
!!       module sel_read_old_binary_ucd
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
      module sel_read_old_binary_ucd
!
      use m_precision
      use m_constants
      use m_file_format_switch
      use m_field_file_format
!
      use ucd_field_file_IO_b
      use read_psf_binary_file
      use set_ucd_file_names
!
#ifdef ZLIB_IO
      use gz_ucd_field_file_IO_b
#endif
!
      use t_file_IO_parameter
      use t_time_data
      use t_ucd_data
      use t_binary_IO_buffer
      use t_buffer_4_gzip
!
      implicit none
!
      private :: read_nostep_psf_bin_file
      private :: read_nostep_iso_bin_file
!
#ifdef ZLIB_IO
      private :: gz_read_nostep_psf_bin_file
      private :: gz_read_nostep_iso_bin_file
#endif
!
      type(binary_IO_buffer), save, private :: bbuf_ucd
      type(buffer_4_gzip), save, private :: zbuf_ucd
      integer(kind = kint_gl), allocatable, private :: itmp1_mp(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_read_old_udt_param                                 &
     &         (id_rank, istep_ucd, ucd_param, ucd)
!
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind=kint) :: ierr = 0
      character(len=kchara) :: file_name
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
!
!
      if(ucd_param%iflag_format .eq. iflag_udt_bin) then
        call read_alloc_nostep_psf_bin_file(file_name, ucd)
!
#ifdef ZLIB_IO
      else if (ucd_param%iflag_format .eq. iflag_udt_bin_gz) then
        call gz_rd_alloc_nostep_psf_b_file(file_name, ucd)
#endif
      end if
!
      if(ierr .gt. 0) stop "sel_read_old_udt_param error"
!
      end subroutine sel_read_old_udt_param
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_old_udt_file                            &
     &         (id_rank, istep_ucd, ucd_param, ucd)
!
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind=kint) :: ierr = 0
      character(len=kchara) :: file_name
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
!
!
      if(ucd_param%iflag_format .eq. iflag_udt_bin) then
        call read_alloc_nostep_psf_bin_file(file_name, ucd)
!
#ifdef ZLIB_IO
      else if (ucd_param%iflag_format .eq. iflag_udt_bin_gz) then
        call gz_rd_alloc_nostep_psf_b_file(file_name, ucd)
#endif
      end if
!
      if(ierr .gt. 0) stop "sel_read_alloc_old_udt_file error"
!
      end subroutine sel_read_alloc_old_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_old_ucd_file                            &
     &         (id_rank, istep_ucd, ucd_param, ucd)
!
#ifdef ZLIB_IO
      use gz_read_psf_binary_file
#endif
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name, grid_name
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
!
      if(     ucd_param%iflag_format .eq. iflag_udt_bin                 &
     &   .or. ucd_param%iflag_format .eq. iflag_udt_bin_gz) then
        grid_name = set_parallel_grd_file_name(ucd_param%file_prefix,   &
     &             ucd_param%iflag_format, id_rank)
      end if
!
      if(ucd_param%iflag_format .eq. iflag_ucd_bin) then
        call read_alloc_nostep_iso_bin_file(file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt_bin) then
        call read_alloc_psf_bin_grid(grid_name, ucd)
        call read_alloc_nostep_psf_bin_file(file_name, ucd)
!
#ifdef ZLIB_IO
      else if (ucd_param%iflag_format .eq. iflag_ucd_bin_gz) then
        call gz_rd_alloc_nostep_iso_b_file(file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_udt_bin_gz) then
        call gz_read_alloc_psf_bin_grid(grid_name, ucd)
        call gz_rd_alloc_nostep_psf_b_file(file_name, ucd)
#endif
      end if
!
      end subroutine sel_read_alloc_old_ucd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_old_udt_file                                  &
     &         (id_rank, istep_ucd, ucd_param, ucd)
!
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind=kint) :: ierr = 0
      character(len=kchara) :: file_name
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
!
      if(ucd_param%iflag_format .eq. iflag_udt_bin) then
        call read_nostep_psf_bin_file(file_name, ucd)
!
#ifdef ZLIB_IO
      else if (ucd_param%iflag_format .eq. iflag_udt_bin_gz) then
        call gz_read_nostep_psf_bin_file(file_name, ucd)
#endif
      end if
!
      if(ierr .gt. 0) stop "sel_read_old_udt_file error"
!
      end subroutine sel_read_old_udt_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_old_ucd_file                                  &
     &         (id_rank, istep_ucd, ucd_param, ucd)
!
#ifdef ZLIB_IO
      use gz_read_psf_binary_file
#endif
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(inout) :: ucd
!
      character(len=kchara) :: file_name, grid_name
!
!
      file_name = set_parallel_ucd_file_name(ucd_param%file_prefix,     &
     &           ucd_param%iflag_format, id_rank, istep_ucd)
! 
      if(     ucd_param%iflag_format .eq. iflag_udt_bin                 &
     &   .or. ucd_param%iflag_format .eq. iflag_udt_bin_gz) then
        grid_name = set_parallel_grd_file_name(ucd_param%file_prefix,   &
     &             ucd_param%iflag_format, id_rank)
      end if
!
      if(ucd_param%iflag_format .eq. iflag_ucd_bin) then
        call read_nostep_iso_bin_file(file_name, ucd)
      else if(ucd_param%iflag_format .eq. iflag_udt_bin) then
        call read_psf_bin_grid(grid_name, ucd)
        call read_nostep_psf_bin_file(file_name, ucd)
!
#ifdef ZLIB_IO
      else if (ucd_param%iflag_format .eq. iflag_ucd_bin_gz) then
        call gz_read_nostep_iso_bin_file(file_name, ucd)
      else if (ucd_param%iflag_format .eq. iflag_udt_bin_gz) then
        call gz_read_psf_bin_grid(grid_name, ucd)
        call gz_read_nostep_psf_bin_file(file_name, ucd)
#endif
      end if
!
      end subroutine sel_read_old_ucd_file
!
!------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_nostep_psf_bin_file(file_name, ucd_b)
!
      use binary_IO
      use read_udt_from_bindary_data
!
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
      type(binary_IO_buffer), save :: bbuf_ucd
      integer :: np_read
!
!
      write(*,*) 'read binary section data: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
!
      call read_one_integer_b(bbuf_ucd, np_read)
!
      call read_psf_bin_field_data(np_read, ucd_b, bbuf_ucd)
      call close_binary_file(bbuf_ucd)
!
      end subroutine read_nostep_psf_bin_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_nostep_iso_bin_file(file_name, ucd_b)
!
      use binary_IO
      use read_udt_from_bindary_data
!
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
      integer :: np_read, nprocs2
!
!
      write(*,*) 'read binary isosurface file: ', trim(file_name)
      call open_read_binary_file(file_name, izero, bbuf_ucd)
      call read_one_integer_b(bbuf_ucd, np_read)
!
      call read_psf_bin_grid_data(np_read, ucd_b, bbuf_ucd)
!
      call read_one_integer_b(bbuf_ucd, nprocs2)
      if(nprocs2 .ne. np_read) stop 'Wrong mesh and field data'
!
      call read_psf_bin_field_data(np_read, ucd_b, bbuf_ucd)
      call close_binary_file(bbuf_ucd)
      deallocate(itmp1_mp)
!
      end subroutine read_nostep_iso_bin_file
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
#ifdef ZLIB_IO
      subroutine gz_read_nostep_psf_bin_file(gzip_name, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_udt_from_bin_data
!
      character(len = kchara), intent(in) :: gzip_name
      type(ucd_data), intent(inout) :: ucd_z
!
      integer :: np_read
!
      call open_rd_gzfile_b(gzip_name, izero, zbuf_ucd)
!
      call gz_read_one_integer_b(zbuf_ucd, np_read)
!
      call gz_read_psf_bin_field_data(np_read, ucd_z, zbuf_ucd)
      call close_gzfile_b
!
      end subroutine gz_read_nostep_psf_bin_file
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_nostep_iso_bin_file(gzip_name, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_udt_from_bin_data
!
      character(len = kchara), intent(in) :: gzip_name
      type(ucd_data), intent(inout) :: ucd_z
!
      integer :: np_read, nprocs2
!
!
      call open_rd_gzfile_b(gzip_name, izero, zbuf_ucd)
      call gz_read_one_integer_b(zbuf_ucd, np_read)
!
      call gz_read_psf_bin_grid_data(np_read, ucd_z, zbuf_ucd)
!
      call gz_read_one_integer_b(zbuf_ucd, nprocs2)
      if(nprocs2 .ne. np_read) stop 'Wrong mesh and field data'
!
      call gz_read_psf_bin_field_data(np_read, ucd_z, zbuf_ucd)
      call close_gzfile_b
!
      end subroutine gz_read_nostep_iso_bin_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gz_rd_alloc_nostep_psf_b_file(gzip_name, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_udt_from_bin_data
!
      character(len = kchara), intent(in) :: gzip_name
      type(ucd_data), intent(inout) :: ucd_z
!
      integer :: np_read
!
!
      call open_rd_gzfile_b(gzip_name, izero, zbuf_ucd)
      call gz_read_one_integer_b(zbuf_ucd, np_read)
!
      call gz_read_alloc_psf_bin_fld_data(np_read, ucd_z, zbuf_ucd)
      call close_gzfile_b
!
      end subroutine gz_rd_alloc_nostep_psf_b_file
!
!  ---------------------------------------------------------------------
!
      subroutine gz_rd_alloc_nostep_iso_b_file(gzip_name, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_udt_from_bin_data
!
      character(len = kchara), intent(in) :: gzip_name
      type(ucd_data), intent(inout) :: ucd_z
!
      integer :: np_read, nprocs2
!
!
      call open_rd_gzfile_b(gzip_name, izero, zbuf_ucd)
      call gz_read_one_integer_b(zbuf_ucd, np_read)
!
      call gz_read_alloc_psf_bin_grid_data(np_read, ucd_z, zbuf_ucd)
      call gz_read_one_integer_b(zbuf_ucd, nprocs2)
      if(nprocs2 .ne. np_read) stop 'Wrong mesh and field data'
!
      call gz_read_alloc_psf_bin_fld_data(np_read, ucd_z, zbuf_ucd)
      call close_gzfile_b
!
      end subroutine gz_rd_alloc_nostep_iso_b_file
#endif
!
!  ---------------------------------------------------------------------
!
      end module sel_read_old_binary_ucd
