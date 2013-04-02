!
      program digit_test
!
      use m_precision
!
      use cal_num_digits
!
      implicit none
!
      integer(kind = kint)  :: i, j, k
      real(kind=kreal) :: x, y
!
!
      write(*,'(a)') 'original, integer, digits'
      do i = -20, 20
        x = dble(1) * 10.0d0**(i)
        call cal_num_digit_int(x,k,j)
        write(*,'(1pE25.15e3,2i6)') x, k, j
        x = dble(2) * 10.0d0**(i)
        call cal_num_digit_int(x,k,j)
        write(*,'(1pE25.15e3,2i6)') x, k, j
        x = dble(5) * 10.0d0**(i)
        call cal_num_digit_int(x,k,j)
        write(*,'(1pE25.15e3,2i6)') x, k, j
      end do
!
      write(*,'(a)') 'original, real, digits'
      do i = -20, 20
        x = dble(1) * 10.0d0**(i)
        call cal_num_digit_real(x,y,j)
        write(*,'(1p2E25.15e3,i6)') x, y, j
!
        x = dble(2.5) * 10.0d0**(i)
        call cal_num_digit_real(x,y,j)
        write(*,'(1p2E25.15e3,i6)') x, y, j
!
        x = dble(5) * 10.0d0**(i)
        call cal_num_digit_real(x,y,j)
        write(*,'(1p2E25.15e3,i6)') x, y, j
!
      end do
      end program digit_test
