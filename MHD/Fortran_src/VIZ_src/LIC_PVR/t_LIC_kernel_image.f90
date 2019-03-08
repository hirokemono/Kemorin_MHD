!>@file  t_LIC_kernel_image.f90
!!       module t_LIC_kernel_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine load_kernel_data_from_file(file_prefix, k_img)
!!      subroutine dealloc_lic_kernel_image(k_img)
!!        type(LIC_kernel_image), intent(inout) :: k_img
!!@endverbatim
!
      module t_LIC_kernel_image
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!>      Structure of LIC kernel image
      type LIC_kernel_image
!>         Number of pixels in horizontal
        integer(kind = kint) :: npixel_x = 0
!>         Number of pixels in vertical
        integer(kind = kint) :: npixel_y = 0
!>         grayscale data
        character(len=1), allocatable :: gray(:,:)
      end type LIC_kernel_image
!
      private :: alloc_lic_kernel_image
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_kernel_data_from_file(file_prefix, k_img)
!
      character(len = kchara), intent(in) :: file_prefix
      type(LIC_kernel_image), intent(inout) :: k_img
!
      integer(kind = kint) :: iflag_rgba, n_pixel
      character(len=kchara) :: file_tmp
!
!
      if(my_rank .eq. 0) then
!#ifdef PNG_OUTPUT
        write(file_tmp,'(a,a1)') trim(file_prefix), char(0)
        call read_png_file_c                                            &
     &     (file_tmp, k_img%npixel_x, k_img%npixel_y, iflag_rgba)
        write(*,*) 'kernel image is read from ',                        &
     &            trim(file_prefix), '.png'
!#endif
        call alloc_lic_kernel_image(k_img)
!
!#ifdef PNG_OUTPUT
        call copy_grayscale_from_png_c                                  &
     &     (k_img%npixel_x, k_img%npixel_y, iflag_rgba, k_img%gray)
!#endif
      end if
!
      call mpi_Bcast(k_img%npixel_x, 1,                                 &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call mpi_Bcast(k_img%npixel_y, 1,                                 &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      n_pixel = k_img%npixel_x * k_img%npixel_y
!
!
      if(my_rank .ne. 0) call alloc_lic_kernel_image(k_img)
      call mpi_Bcast(k_img%gray, n_pixel,                              &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine load_kernel_data_from_file
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_kernel_image(k_img)
!
      type(LIC_kernel_image), intent(inout) :: k_img
!
      if(allocated(k_img%gray)) deallocate(k_img%gray)
!
      end subroutine dealloc_lic_kernel_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_lic_kernel_image(k_img)
!
      type(LIC_kernel_image), intent(inout) :: k_img
!
      allocate(k_img%gray(k_img%npixel_x,k_img%npixel_y))
!
      end subroutine alloc_lic_kernel_image
!
!  ---------------------------------------------------------------------
!
      end module t_LIC_kernel_image
