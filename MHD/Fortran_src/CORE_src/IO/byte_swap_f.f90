!>@file   byte_swap_f.f90
!!@brief  module byte_swap_f
!!
!!@authorH.Matsui and H.Okuda
!!@date Programmed  by  H. Matsui in  Aug., 2016 
!
!>@brief swap byte endian
!!
!!@verbatim
!!      subroutine byte_swap_f(l8_byte, array)
!!      subroutine binary_read_and_swap_f                               &
!!     &         (iflag_endian, id_file, l8_byte, array)
!!      subroutine binary_write_f(id_file, l8_byte, array)
!!         l8_byte :: byte length of array (defined by 8-byte integer)
!!         array ::   array to be transfered (call by using pointer!)
!!@endverbatim
!
      subroutine byte_swap_f(l8_byte, array)
!
      use m_precision
!
      integer(kind = kint_gl), intent(in) :: l8_byte
      character(len=1), intent(inout) :: array(l8_byte)
!
      integer(kind = kint_gl) :: i8
      character(len=1) :: tmp1, tmp2
!
!
!$omp parallel do private(i8,tmp1,tmp2)
      do i8 = 4, l8_byte, 4
        tmp1 = array(4*i8-3)
        tmp2 = array(4*i8-2)
        array(i-3) = array(i  )
        array(i-2) = array(i-1)
        array(i-1) = tmp2
        array(i  ) = tmp1
      end do
!$omp end parallel do
!
      end subroutine byte_swap_f
!
! -----------------------------------------------------------------------
!
      subroutine binary_read_and_swap_f                                 &
     &         (iflag_endian, id_file, l8_byte, array)
!
      use m_precision
!
      integer(kind = kint), intent(in) :: id_file, iflag_endian
      integer(kind = kint_gl), intent(in) :: l8_byte
      character(len=1), intent(inout) :: array(l8_byte)
!
!
      read(id_file)  array(1:l8_byte)
!
      if(iflag_endian .gt. 0) call byte_swap_f(l8_byte, array)
!
      end subroutine binary_read_and_swap_f
!
! -----------------------------------------------------------------------
!
      subroutine binary_write_f(id_file, l8_byte, array)
!
      use m_precision
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint_gl), intent(in) :: l8_byte
      character(len=1), intent(inout) :: array(l8_byte)
!
!
      write(id_file)  array(1:l8_byte)
!
      end subroutine binary_write_f
