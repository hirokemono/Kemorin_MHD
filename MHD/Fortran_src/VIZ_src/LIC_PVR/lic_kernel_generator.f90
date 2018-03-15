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
      subroutine guaussian(mu, sigma, value, g_value)

        real(kind = kreal), intent(in) :: mu, sigma, value
        real(kind = kreal), intent(inout) :: g_value

        real(kind = kreal) :: pi
        pi = 3.1415927
        g_value = exp(-0.5 * ((value - mu)/sigma)**2)
        !g_value = g_value / (sigma * sqrt(pi))

      end subroutine guaussian
!
!  ---------------------------------------------------------------------
!
      subroutine generate_kernal_ary(k_size, k_ary)

        integer(kind = kint), intent(in) :: k_size
        real(kind = kreal), intent(inout) :: k_ary(k_size)

        real(kind = kreal) :: k_middle, idx, mu, sigma
        integer(kind = kint) :: i
        mu = 0.5
        sigma = 0.22
        k_middle = (1 + k_size) / 2

        !do i = 1, k_size/2
        !  k_ary(i) = 1.0 * abs(i / k_middle )
        !  k_ary(k_size + 1 - i) = 1.0 * abs(i / k_middle )
        !end do

        do i = 1, k_size
          idx = i * 1.0 / k_size
          call guaussian(mu, sigma, idx, k_ary(i))
          !k_ary(i) = 1.0
          !write(*,*) i, k_ary(i)
        end do


      end subroutine generate_kernal_ary
!
!  ---------------------------------------------------------------------
!
subroutine kernal_sampling(k_size, k_ary, pos, k_value)

integer(kind = kint), intent(in) :: k_size
real(kind = kreal), intent(in) :: k_ary(k_size), pos
real(kind = kreal), intent(out) :: k_value
real(kind = kreal) :: k_pos, offset, v_ceil, v_floor
integer(kind = kint) :: k_ceil, k_floor

k_value = 0.0
k_pos = pos * k_size
k_ceil = ceiling(k_pos + 0.5)
k_floor = floor(k_pos + 0.5)
offset = k_pos + 0.5 - k_floor
k_ceil = min(k_ceil, k_size)
k_floor = max(k_floor, 1)
k_value = k_ary(k_floor) * (1.0 - offset) + k_ary(k_ceil) * (offset)

end subroutine kernal_sampling
!
!  ---------------------------------------------------------------------
!
      end module lic_kernel_generator
