!
!      program gaussian_points
!
      program gaussian_points
!
      use m_precision
      use t_gauss_points
!
      implicit none
!
      integer (kind = kint) :: i, nth_g
      type(gauss_points) :: gauss_pt
!
   10 continue
!
      write(*,*) 'imput number of points (end: negative values)'
      read(*,*) nth_g
!
      if (nth_g.le.0) go to 999
!
      call const_gauss_colatitude(nth_g, gauss_pt)
!
      call check_gauss_points(gauss_pt)
!
      call dealloc_gauss_colatitude(gauss_pt)
!
      go to 10
!
!
 999  continue
      stop
      end
