!set_map_from_1patch.f90
!      module set_map_from_1patch
!
      module set_map_from_1patch
!
!      Written by H. Matsui
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use m_geometry_constants
!
      implicit none
!
      integer(kind = kint), parameter :: nmax_map_p = 3
!
      real(kind=kreal) :: xy_map(2,num_triangle,nmax_map_p)
      real :: d_map_patch(num_triangle,nmax_map_p)
!
      integer(kind = kint) :: n_map_patch
      real(kind=kreal) :: x_map_patch(num_triangle,n_vector,nmax_map_p)
      real(kind=kreal)                                                  &
     &               :: rtp_map_patch(num_triangle,n_vector,nmax_map_p)
!
      integer(kind = kint), parameter :: ipatch_map_12_23(9)            &
     &       = (/ 1, 4, 5,  1, 5, 3,  4, 2, 3/)
      integer(kind = kint), parameter :: ipatch_map_12_31(9)            &
     &       = (/ 5, 2, 3,  5, 3, 4,  1, 5, 4/)
      integer(kind = kint), parameter :: ipatch_map_23_31(9)            &
     &       = (/ 1, 2, 4,  1, 4, 5,  5, 4, 3/)
      integer(kind = kint), parameter :: ipatch_map_12_3(6)             &
     &       = (/ 1, 4, 3,  4, 2, 3/)
      integer(kind = kint), parameter :: ipatch_map_23_1(6)             &
     &       = (/ 4, 1, 2,  1, 4, 3/)
      integer(kind = kint), parameter :: ipatch_map_31_2(6)             &
     &       = (/ 1, 2, 4,  4, 2, 3/)
!
      real(kind = kreal), parameter, private :: EPSILON = 1.0d-9
!
      private :: nmax_map_p, x_map_patch, rtp_map_patch
      private :: ipatch_map_12_23, ipatch_map_12_31, ipatch_map_23_31
      private :: ipatch_map_12_3,  ipatch_map_23_1,  ipatch_map_31_2
!
!      subroutine set_map_patch_from_1patch(iele, nnod_pg, nele_pg,     &
!     &          xx_psf, ie_psf, scalar_psf)
!      subroutine set_sph_position_4_map_patch
!      subroutine projection_patch_to_map
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_map_patch_from_1patch(iele, nnod_pg, nele_pg,      &
     &          xx_psf, ie_psf, scalar_psf)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: nnod_pg, nele_pg
      integer(kind = kint), intent(in) :: ie_psf(nele_pg,3)
      real(kind = kreal), intent(in) :: xx_psf(nnod_pg,3)
      real(kind = kreal), intent(in) :: scalar_psf(nnod_pg)
!
      real(kind = kreal) :: x_map(5,3)
      real :: d_map(9)
      integer(kind = kint) :: inod, i, j, jj, k
      real(kind = kreal) :: y1, y2, y3
!
!
      do i = 1, 3
        inod = ie_psf(iele,i)
        d_map(i) =   real( scalar_psf(inod) )
!
        do j = 1, 3
          if(abs(x_map(i,j)) .lt. EPSILON) then
            x_map(i,j) = 0.0d0
          else
            x_map(i,j) = xx_psf(inod,j)
          end if
        end do
      end do
      y1 = x_map(1,2)
      y2 = x_map(2,2)
      y3 = x_map(3,2)
!
!
      if     ( ((y1*y2).lt.zero) .and. ((y2*y3).lt.zero) )then
        x_map(4,1) = (y2*x_map(1,1) - y1*x_map(2,1)) / (y2- y1)
        x_map(4,2) = zero
        x_map(4,3) = (y2*x_map(1,3) - y1*x_map(2,3)) / (y2- y1)
        d_map(4) =   (y2*d_map(1) -   y1*d_map(2)) /   (y2- y1)
!
        x_map(5,1) = (y3*x_map(2,1) - y2*x_map(3,1)) / (y3- y2)
        x_map(5,2) = zero
        x_map(5,3) = (y3*x_map(2,3) - y2*x_map(3,3)) / (y3- y2)
        d_map(5) =   (y3*d_map(2) -   y2*d_map(3)) /   (y3- y2)
!
        n_map_patch = 3
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_12_23(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,i) =   d_map(k)
          end do
        end do
!
      else if( ((y1*y2).lt.zero) .and. (y3.eq.zero) )then
        x_map(4,1) = (y2*x_map(1,1) - y1*x_map(2,1)) / (y2- y1)
        x_map(4,2) = zero
        x_map(4,3) = (y2*x_map(1,3) - y1*x_map(2,3)) / (y2- y1)
        d_map(4) =   (y2*d_map(1) -   y1*d_map(2)) /   (y2- y1)
!
        n_map_patch = 2
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_12_3(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,i) =   d_map(k)
          end do
        end do
!
!
      else if( ((y1*y2).lt.zero) .and. ((y3*y1).lt.zero) ) then
        x_map(4,1) = (y2*x_map(1,1) - y1*x_map(2,1)) / (y2- y1)
        x_map(4,2) = zero
        x_map(4,3) = (y2*x_map(1,3) - y1*x_map(2,3)) / (y2- y1)
        d_map(4) =   (y2*d_map(1) -   y1*d_map(2)) /   (y2- y1)
!
        x_map(5,1) = (y1*x_map(3,1) - y3*x_map(1,1)) / (y1- y3)
        x_map(5,2) = zero
        x_map(5,3) = (y1*x_map(3,3) - y3*x_map(1,3)) / (y1- y3)
        d_map(5) =   (y1*d_map(3) -   y3*d_map(1)) /   (y1- y3)
!
        n_map_patch = 3
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_12_31(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,i) =   d_map(k)
          end do
        end do
!
      else if( ((y2*y3).lt.zero) .and. (y1.eq.zero) )then
        x_map(4,1) = (y3*x_map(2,1) - y2*x_map(3,1)) / (y3- y2)
        x_map(4,2) = zero
        x_map(4,3) = (y3*x_map(2,3) - y2*x_map(3,3)) / (y3- y2)
        d_map(4) =   (y3*d_map(2) -   y2*d_map(3)) /   (y3- y2)
!
        n_map_patch = 2
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_23_1(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,i) =   d_map(k)
          end do
        end do
!
      else if( ((y2*y3).lt.zero) .and. ((y3*y1).lt.zero) ) then
        x_map(4,1) = (y3*x_map(2,1) - y2*x_map(3,1)) / (y3- y2)
        x_map(4,2) = zero
        x_map(4,3) = (y3*x_map(2,3) - y2*x_map(3,3)) / (y3- y2)
        d_map(4) =   (y3*d_map(2) -   y2*d_map(3)) /   (y3- y2)
!
        x_map(5,1) = (y1*x_map(3,1) - y3*x_map(1,1)) / (y1- y3)
        x_map(5,2) = zero
        x_map(5,3) = (y1*x_map(3,3) - y3*x_map(1,3)) / (y1- y3)
        d_map(5) =   (y1*d_map(3) -   y3*d_map(1)) /   (y1- y3)
!
        n_map_patch = 3
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_23_31(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,i) =   d_map(k)
          end do
        end do
!
      else if( ((y3*y1).lt.zero) .and. (y2.eq.zero) )then
        x_map(4,1) = (y1*x_map(3,1) - y3*x_map(1,1)) / (y1- y3)
        x_map(4,2) = zero
        x_map(4,3) = (y1*x_map(3,3) - y3*x_map(1,3)) / (y1- y3)
        d_map(4) =   (y1*d_map(3) -   y3*d_map(1)) /   (y1- y3)
!
        n_map_patch = 2
        do i = 1, n_map_patch
          do j = 1, num_triangle
            jj = j + (i-1)*num_triangle
            k = ipatch_map_31_2(jj)
            x_map_patch(j,1,i) = x_map(k,1)
            x_map_patch(j,2,i) = x_map(k,2)
            x_map_patch(j,3,i) = x_map(k,3)
            d_map_patch(j,i) =   d_map(k)
          end do
        end do
!
      else
        n_map_patch = 1
        do j = 1, 3
          x_map_patch(j,1,1) = x_map(j,1)
          x_map_patch(j,2,1) = x_map(j,2)
          x_map_patch(j,3,1) = x_map(j,3)
          d_map_patch(j,1) =    d_map(j)
        end do
      end if
!
      end subroutine set_map_patch_from_1patch
!
!-----------------------------------------------------------------------
!
      subroutine set_sph_position_4_map_patch
!
      use coordinate_converter
!
      integer(kind = kint) :: i
      real(kind = kreal) :: y_center(3)
      real(kind = kreal) :: ar_map(3), rs_map(3), as_map(3)
      real(kind = kreal) :: pi, xflag, yflag
!
!
      do i = 1, n_map_patch
        if(abs(x_map_patch(1,2,i)) .lt. EPSILON) x_map_patch(1,2,i)= 0.
        if(abs(x_map_patch(2,2,i)) .lt. EPSILON) x_map_patch(1,2,i)= 0.
        if(abs(x_map_patch(3,2,i)) .lt. EPSILON) x_map_patch(1,2,i)= 0.
      end do
!
      do i = 1, n_map_patch
        y_center(i) = (x_map_patch(1,2,i) + x_map_patch(2,2,i)          &
     &               + x_map_patch(3,2,i) ) / three
      end do
!
      do i = 1, n_map_patch
        call position_2_sph(ithree, x_map_patch, rtp_map_patch(1,1,i),  &
     &      rtp_map_patch(1,2,i), rtp_map_patch(1,3,i),                 &
     &      ar_map(1), rs_map(1), as_map(1))
      end do
!
      pi = four * atan(one)
      do i = 1, n_map_patch
        xflag = x_map_patch(1,1,i) + x_map_patch(2,1,i)                 &
     &         + x_map_patch(2,1,i)
        yflag = x_map_patch(1,2,i) * x_map_patch(2,2,i)                 &
     &         * x_map_patch(2,2,i)
!
        if(yflag.eq.zero .and. xflag.le.zero) then
          if( y_center(i) .gt. zero) then
            if(rtp_map_patch(1,3,i) .eq. zero) then
               rtp_map_patch(1,3,i) = two * pi
            end if
            if(rtp_map_patch(2,3,i) .eq. zero) then
               rtp_map_patch(2,3,i) = two * pi
            end if
            if(rtp_map_patch(3,3,i) .eq. zero) then
               rtp_map_patch(3,3,i) = two * pi
            end if
          end if
!
        end if
      end do
!
      end subroutine set_sph_position_4_map_patch
!
!-----------------------------------------------------------------------
!
      subroutine projection_patch_to_map
!
      use m_constants
      use map_projection_sph
!
      integer(kind = kint) :: i, j
      real(kind = kreal) :: s_theta, c_theta, pi, phi_map
!
!
      pi = four * atan(one)
      do i = 1, n_map_patch
        do j = 1, num_triangle
          s_theta = sin( rtp_map_patch(j,2,i) )
          c_theta = cos( rtp_map_patch(j,2,i) )
          phi_map = mod( (rtp_map_patch(j,3,i)+pi),(two*pi) )
          call aitoff(s_theta, c_theta, phi_map,                        &
     &        xy_map(1,i,j), xy_map(2,i,j) )
        end do
      end do
!
      end subroutine projection_patch_to_map
!
!-----------------------------------------------------------------------
!
      end module set_map_from_1patch
