!>@file  read_psf_binary_file.f90
!!       module read_psf_binary_file
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief read binary section file
!!
!!@verbatim
!!        subroutine read_alloc_iso_bindary_file(file_name, ucd_b)
!!        subroutine read_alloc_psf_bin_grid(file_name, nprocs, ucd_b)
!!        subroutine read_alloc_psf_bin_file(file_name, nprocs, ucd_b)
!!        character(len = kchara), intent(in) :: file_name
!!        type(ucd_data), intent(inout) :: ucd_b
!!@endverbatim
!
      module read_psf_binary_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_binary_IO_buffer
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
      subroutine read_alloc_iso_bindary_file(file_name, ucd_b)
!
      use binary_IO
      use read_psf_binary_data
!
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
      type(binary_IO_buffer), save :: bbuf
      integer :: nprocs, nprocs2
      integer(kind = kint_gl), allocatable :: itmp1_mp(:)
!
!
      call open_read_binary_file(file_name, izero, bbuf)
      call read_one_integer_b(bbuf, nprocs)
      allocate(itmp1_mp(nprocs))
!
      call read_psf_node_num_bin(nprocs, ucd_b%nnod, itmp1_mp, bbuf)
!
      call allocate_ucd_node(ucd_b)
      call read_psf_node_data_bin                                       &
     &   (nprocs, ucd_b%nnod, ucd_b%inod_global, ucd_b%xx,              &
     &    itmp1_mp, bbuf)
!
      call read_psf_ele_num_bin                                         &
     &   (nprocs, ucd_b%nele, ucd_b%nnod_4_ele, itmp1_mp, bbuf)
!
      call allocate_ucd_ele(ucd_b)
      call read_psf_ele_connect_bin                                     &
     &   (nprocs, ucd_b%nele, ucd_b%nnod_4_ele,                         &
     &    ucd_b%iele_global, ucd_b%ie, itmp1_mp, bbuf)
!
!
      call read_one_integer_b(bbuf, nprocs2)
      if(nprocs2 .ne. nprocs) stop 'Wrong mesh and field data'
!
      call read_psf_phys_num_bin                                        &
     &   (nprocs, ucd_b%nnod, ucd_b%num_field, itmp1_mp, bbuf)
!
      call allocate_ucd_phys_name(ucd_b)
      call read_psf_phys_name_bin                                       &
     &   (ucd_b%num_field, ucd_b%ntot_comp, ucd_b%num_comp,             &
     &    ucd_b%phys_name, bbuf)

      call allocate_ucd_phys_data(ucd_b)
      call read_psf_phys_data_bin                                       &
     &   (nprocs, ucd_b%nnod, ucd_b%ntot_comp, ucd_b%d_ucd,             &
     &    itmp1_mp, bbuf)
      call close_binary_file(bbuf)
      deallocate(itmp1_mp)
!
      end subroutine read_alloc_iso_bindary_file
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_psf_bin_grid(file_name, nprocs, ucd_b)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(inout) :: nprocs
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
      type(binary_IO_buffer), save :: bbuf
      integer(kind = kint_gl), allocatable :: itmp1_mp(:)
!
!
      call open_read_binary_file(file_name, izero, bbuf)
      call read_one_integer_b(bbuf, nprocs)
      allocate(itmp1_mp(nprocs))
!
      call read_psf_node_num_bin(nprocs, ucd_b%nnod, itmp1_mp, bbuf)
!
      call allocate_ucd_node(ucd_b)
      call read_psf_node_data_bin                                       &
     &   (nprocs, ucd_b%nnod, ucd_b%inod_global, ucd_b%xx,              &
     &    itmp1_mp, bbuf)
!
      call read_psf_ele_num_bin                                         &
     &   (nprocs, ucd_b%nele, ucd_b%nnod_4_ele, itmp1_mp, bbuf)
!
      call allocate_ucd_ele(ucd_b)
      call read_psf_ele_connect_bin                                     &
     &   (nprocs, ucd_b%nele, ucd_b%nnod_4_ele,                         &
     &    ucd_b%iele_global, ucd_b%ie, itmp1_mp, bbuf)
!
      call close_binary_file(bbuf)
      deallocate(itmp1_mp)
!
      end subroutine read_alloc_psf_bin_grid
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_psf_bin_file(file_name, nprocs, ucd_b)
!
      use binary_IO
      use read_psf_binary_data
!
      integer, intent(in) :: nprocs
      character(len = kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd_b
!
      type(binary_IO_buffer), save :: bbuf
      integer :: nprocs2
      integer(kind = kint_gl), allocatable :: itmp1_mp(:)
!
!
      call open_read_binary_file(file_name, izero, bbuf)
!
      call read_one_integer_b(bbuf, nprocs2)
      if(nprocs2 .ne. nprocs) stop 'Wrong mesh and field data'
      allocate(itmp1_mp(nprocs))
!
      call read_psf_phys_num_bin                                        &
     &   (nprocs, ucd_b%nnod, ucd_b%num_field, itmp1_mp, bbuf)
!
      call allocate_ucd_phys_name(ucd_b)
      call read_psf_phys_name_bin                                       &
     &   (ucd_b%num_field, ucd_b%ntot_comp, ucd_b%num_comp,             &
     &    ucd_b%phys_name, bbuf)

      call allocate_ucd_phys_data(ucd_b)
      call read_psf_phys_data_bin                                       &
     &   (nprocs, ucd_b%nnod, ucd_b%ntot_comp, ucd_b%d_ucd,             &
     &    itmp1_mp, bbuf)
      call close_binary_file(bbuf)
      deallocate(itmp1_mp)
!
      end subroutine read_alloc_psf_bin_file
!
!  ---------------------------------------------------------------------
!
      end module read_psf_binary_file
