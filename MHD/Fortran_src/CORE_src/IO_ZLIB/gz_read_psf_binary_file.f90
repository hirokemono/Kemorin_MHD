!>@file  gz_read_psf_binary_file.f90
!!       module gz_read_psf_binary_file
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief read gzipped binary section file
!!
!!@verbatim
!!      subroutine gz_read_alloc_iso_bin_file(gzip_name, ucd_z)
!!      subroutine gz_read_alloc_psf_bin_grid(gzip_name, nprocs, ucd_z)
!!      subroutine gz_read_alloc_psf_bin_file(gzip_name, nprocs, ucd_z)
!!        character(len = kchara), intent(in) :: gzip_name
!!        type(ucd_data), intent(inout) :: ucd_z
!!@endverbatim
!
      module gz_read_psf_binary_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_buffer_4_gzip
      use t_ucd_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_alloc_iso_bin_file(gzip_name, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      character(len = kchara), intent(in) :: gzip_name
      type(ucd_data), intent(inout) :: ucd_z
!
      type(buffer_4_gzip), save :: zbuf
      integer :: nprocs, nprocs2
      integer(kind = kint_gl), allocatable :: itmp1_mp_gz(:)
!
!
      call open_rd_gzfile_b(gzip_name, izero, zbuf)
      call gz_read_one_integer_b(zbuf, nprocs)
      allocate(itmp1_mp_gz(nprocs))
!
      call read_psf_node_num_bin_gz                                     &
     &   (nprocs, ucd_z%nnod, itmp1_mp_gz, zbuf)
!
      call allocate_ucd_node(ucd_z)
      call read_psf_node_data_bin_gz                                    &
     &   (nprocs, ucd_z%nnod, ucd_z%inod_global, ucd_z%xx,              &
     &    itmp1_mp_gz, zbuf)
!
      call read_psf_ele_num_bin_gz                                      &
     &   (nprocs, ucd_z%nele, ucd_z%nnod_4_ele, itmp1_mp_gz, zbuf)
!
      call allocate_ucd_ele(ucd_z)
      call read_psf_ele_connect_bin_gz                                  &
     &   (nprocs, ucd_z%nele, ucd_z%nnod_4_ele,                         &
     &    ucd_z%iele_global, ucd_z%ie, itmp1_mp_gz, zbuf)
!
!
      call gz_read_one_integer_b(zbuf, nprocs2)
      if(nprocs2 .ne. nprocs) stop 'Wrong mesh and field data'
!
      call read_psf_phys_num_bin_gz                                     &
     &   (nprocs, ucd_z%nnod, ucd_z%num_field, itmp1_mp_gz, zbuf)
!
      call allocate_ucd_phys_name(ucd_z)
      call read_psf_phys_name_bin_gz                                    &
     &   (ucd_z%num_field, ucd_z%ntot_comp, ucd_z%num_comp,             &
     &    ucd_z%phys_name, zbuf)
!
      call allocate_ucd_phys_data(ucd_z)
      call read_psf_phys_data_bin_gz                                    &
     &   (nprocs, ucd_z%nnod, ucd_z%ntot_comp, ucd_z%d_ucd,             &
     &    itmp1_mp_gz, zbuf)
      call close_gzfile_b
      deallocate(itmp1_mp_gz)
!
      end subroutine gz_read_alloc_iso_bin_file
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_alloc_psf_bin_grid(gzip_name, nprocs, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      character(len = kchara), intent(in) :: gzip_name
      integer, intent(inout) :: nprocs
      type(ucd_data), intent(inout) :: ucd_z
!
      type(buffer_4_gzip), save :: zbuf
      integer(kind = kint_gl), allocatable :: itmp1_mp_gz(:)
!
!
      call open_rd_gzfile_b(gzip_name, izero, zbuf)
      call gz_read_one_integer_b(zbuf, nprocs)
      allocate(itmp1_mp_gz(nprocs))
!
      call read_psf_node_num_bin_gz                                     &
     &   (nprocs, ucd_z%nnod, itmp1_mp_gz, zbuf)
!
      call allocate_ucd_node(ucd_z)
      call read_psf_node_data_bin_gz                                    &
     &   (nprocs, ucd_z%nnod, ucd_z%inod_global, ucd_z%xx,              &
     &    itmp1_mp_gz, zbuf)
!
      call read_psf_ele_num_bin_gz                                      &
     &   (nprocs, ucd_z%nele, ucd_z%nnod_4_ele, itmp1_mp_gz, zbuf)
!
      call allocate_ucd_ele(ucd_z)
      call read_psf_ele_connect_bin_gz                                  &
     &   (nprocs, ucd_z%nele, ucd_z%nnod_4_ele,                         &
     &    ucd_z%iele_global, ucd_z%ie, itmp1_mp_gz, zbuf)
!
      call close_gzfile_b
      deallocate(itmp1_mp_gz)
!
      end subroutine gz_read_alloc_psf_bin_grid
!
!  ---------------------------------------------------------------------
!
      subroutine gz_read_alloc_psf_bin_file(gzip_name, nprocs, ucd_z)
!
      use gz_binary_IO
      use gzip_file_access
      use gz_read_psf_binary_data
!
      character(len = kchara), intent(in) :: gzip_name
      integer, intent(in) :: nprocs
      type(ucd_data), intent(inout) :: ucd_z
!
      type(buffer_4_gzip), save :: zbuf
      integer :: nprocs2
      integer(kind = kint_gl), allocatable :: itmp1_mp_gz(:)
!
!
      call open_rd_gzfile_b(gzip_name, izero, zbuf)
!
      call gz_read_one_integer_b(zbuf, nprocs2)
      if(nprocs2 .ne. nprocs) stop 'Wrong mesh and field data'
      allocate(itmp1_mp_gz(nprocs))
!
      call read_psf_phys_num_bin_gz                                     &
     &   (nprocs, ucd_z%nnod, ucd_z%num_field, itmp1_mp_gz, zbuf)
!
      call allocate_ucd_phys_name(ucd_z)
      call read_psf_phys_name_bin_gz                                    &
     &   (ucd_z%num_field, ucd_z%ntot_comp, ucd_z%num_comp,             &
     &    ucd_z%phys_name, zbuf)
!
      call allocate_ucd_phys_data(ucd_z)
      call read_psf_phys_data_bin_gz                                    &
     &   (nprocs, ucd_z%nnod, ucd_z%ntot_comp, ucd_z%d_ucd,             &
     &    itmp1_mp_gz, zbuf)
      call close_gzfile_b
      deallocate(itmp1_mp_gz)
!
      end subroutine gz_read_alloc_psf_bin_file
!
!  ---------------------------------------------------------------------
!
      end module gz_read_psf_binary_file
