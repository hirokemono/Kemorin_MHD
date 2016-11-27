!
!      module set_gauss_int_parameters
!
!       Written by H. Matsui on March. 2006
!
!      subroutine set_gauss_coefs_4_3d
!      subroutine set_gauss_coefs_4_2d
!      subroutine set_gauss_coefs_4_1d
!
      module set_gauss_int_parameters
!
      use m_precision
!
      use m_fem_gauss_int_coefs
      use m_shape_functions
!
      implicit none
!
      private :: set_gauss_coefs_1d_n
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_gauss_coefs_4_3d
!
!
      integer(kind = kint) :: n, ix, ii, i1, i2, i3
!
!
      do n = 1, max_int_point
        do ii = 1, n*n*n
          ix = ii + int_start3(n)
          i1 = l_int(1,ii,n) + int_start1(n)
          i2 = l_int(2,ii,n) + int_start1(n)
          i3 = l_int(3,ii,n) + int_start1(n)
          xi3(ix) = xi1(i1)
          ei3(ix) = xi1(i2)
          zi3(ix) = xi1(i3)
          owe3d(ix) = owe(i1) * owe(i2) * owe(i3)
        end do
      end do
!
      end subroutine set_gauss_coefs_4_3d
!
!  ---------------------------------------------------------------------
!
      subroutine set_gauss_coefs_4_2d
!
      integer(kind = kint) :: n, ii, ix, i1, i2
!
      do n = 1, max_int_point
        do ii = 1, n*n
          ix = ii + int_start2(n)
          i1 = l_int(1,ii,n) + int_start1(n)
          i2 = l_int(2,ii,n) + int_start1(n)
          xi2(ix) = xi1(i1)
          ei2(ix) = xi1(i2)
          owe2d(ix) = owe(i1) * owe(i2)
        end do
      end do
!
      end subroutine set_gauss_coefs_4_2d
!
!  ---------------------------------------------------------------------
!
      subroutine set_gauss_coefs_4_1d
!
      use m_constants
      use m_gauss_int_parameters
      use t_gauss_points
!
      integer(kind = kint) :: n
      type(gauss_points) :: gauss_1d
!
!
      if (max_int_point .ge. ione) then
        call set_gauss_coefs_1d_n(ione,   pt1d_1g, wt1d_1g)
      end if
!
      if (max_int_point .ge. itwo) then
        call set_gauss_coefs_1d_n(itwo,   pt1d_2g, wt1d_2g)
      end if
!
      if (max_int_point .ge. ithree) then
        call set_gauss_coefs_1d_n(ithree, pt1d_3g, wt1d_3g)
      end if
!
      if (max_int_point .ge. ifour) then
        call set_gauss_coefs_1d_n(ifour,  pt1d_4g, wt1d_4g)
      end if
!
!
!
      if (max_int_point .ge. 5) then
        do n = 5, max_int_point
          call construct_gauss_coefs(n, gauss_1d)
!
          call set_gauss_coefs_1d_n(n, gauss_1d%point, gauss_1d%weight)
!
          call dealloc_gauss_points(gauss_1d)
        end do
      end if
!
      end subroutine set_gauss_coefs_4_1d
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_gauss_coefs_1d_n(n, pt1d, wt1d)
!
      integer(kind = kint), intent(in) :: n
      real(kind = kreal), intent(in) :: pt1d(n), wt1d(n)
      integer(kind = kint) :: ix, ii
!
!
      do ii = 1, n
        ix = ii + int_start1(n)
        xi1(ix) = pt1d(ii)
        owe(ix) = wt1d(ii)
      end do
!
      end subroutine set_gauss_coefs_1d_n
!
!  ---------------------------------------------------------------------
!
      end module set_gauss_int_parameters
