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
      use t_png_file_access
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
      type(buffer_4_png), private :: pbuf_kernel
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
      use calypso_mpi_int
!
      character(len = kchara), intent(in) :: file_prefix
      type(LIC_kernel_image), intent(inout) :: k_img
!
      integer(kind = kint_gl) :: n_pixel
!
!
      if(my_rank .eq. 0) then
!#ifdef PNG_OUTPUT
        call read_png_file_f                                            &
     &     (file_prefix, k_img%npixel_x, k_img%npixel_y, pbuf_kernel)
        write(*,*) 'kernel image is read from ',                        &
     &            trim(file_prefix), '.png'
!#endif
        call alloc_lic_kernel_image(k_img)
!
!#ifdef PNG_OUTPUT
        call copy_grayscale_from_png_f                                  &
     &     (k_img%npixel_x, k_img%npixel_y, k_img%gray, pbuf_kernel)
!#endif
!#endif
      end if
!
      call calypso_mpi_bcast_one_int(k_img%npixel_x, 0)
      call calypso_mpi_bcast_one_int(k_img%npixel_y, 0)
      n_pixel = k_img%npixel_x * k_img%npixel_y
!
      if(my_rank .ne. 0) call alloc_lic_kernel_image(k_img)
      call calypso_mpi_bcast_character(k_img%gray, n_pixel, 0)
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
