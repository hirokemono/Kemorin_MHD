!cal_num_digits.f90
!      module cal_num_digits
!
!     Written by H. Matsui on Nov., 2009
!
!      subroutine cal_num_digit_int(x_input, i_fact, i_digit)
!      subroutine cal_num_digit_real(x_input, r_fact, i_digit)
!
      module cal_num_digits
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_num_digit_int(x_input, i_fact, i_digit)
!
      real(kind = kreal), intent(in) :: x_input
      integer(kind = kint), intent(inout) :: i_digit, i_fact
!
!
      i_digit = int( aint( log10(x_input) ))

!
      if(x_input .ge. one) then
        i_fact = nint(x_input * ten**(-i_digit))
      else
        i_fact = nint(x_input * ten**(-i_digit+1))
        if(i_fact .eq. iten) then
          i_fact = 1
        else
          i_digit = i_digit-1
        end if
      end if
!
      end subroutine cal_num_digit_int
!
!  ---------------------------------------------------------------------
!
      subroutine cal_num_digit_real(x_input, r_fact, i_digit)
!
      real(kind = kreal), intent(in) :: x_input
      real(kind = kreal), intent(inout) :: r_fact
      integer(kind = kint), intent(inout) :: i_digit
!
!
      i_digit = int( aint( log10(x_input) ))
!
      if(x_input .ge. one) then
        r_fact = x_input * ten**(-i_digit)
      else
        r_fact = x_input * ten**(-i_digit+1)
        if(r_fact .ge. ten) then
          r_fact = one
        else
          i_digit = i_digit-1
        end if
      end if
!
      end subroutine cal_num_digit_real
!
!  ---------------------------------------------------------------------
!
      end module cal_num_digits
