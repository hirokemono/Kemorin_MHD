!lic_kernel_generator.f90
!
!      module lic_kernel_generator
!
!      Written by Yangguang Liao 2018
!
!      subroutine generate_kernal_ary(k_size, k_ary)
!
      module lic_kernel_generator
!
      use m_precision
      use m_constants
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine generate_kernal_ary(k_size, k_ary)

        integer(kind = kint), intent(in) :: k_size
        real(kind = kreal), intent(inout) :: k_ary(k_size)

        real(kind = kreal) :: k_middle
        integer(kind = kint) :: i

        k_middle = (1 + k_size) / 2

        do i = 1, k_size/2
          k_ary(i) = 1.0 * abs(i / k_middle )
          k_ary(k_size + 1 - i) = 1.0 * abs(i / k_middle )
        end do


      end subroutine generate_kernal_ary
!
!  ---------------------------------------------------------------------
!
      end module lic_kernel_generator
