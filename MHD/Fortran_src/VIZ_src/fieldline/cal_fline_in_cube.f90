!cal_fline_in_cube.f90
!
!      module cal_fline_in_cube
!
!      Written by H. Matsui on Aug., 2011
!
!      subroutine find_line_end_in_1ele(iflag_back, nnod, nele, nsurf,  &
!     &          nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,    &
!     &          fline, x0, isf_tgt, x_tgt, xi)
!      subroutine cal_fline_to_square(x0, vec, x_quad, x_tgt, ierr)
!      subroutine cal_filne_to_triangle(x0, vec, x_tri, x_tgt, ierr)
!
      module cal_fline_in_cube
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use m_geometry_constants
      use calypso_mpi
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine find_line_end_in_1ele(iflag_back, nnod, nele, nsurf,   &
     &          nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,     &
     &          fline, x0, isf_tgt, x_tgt, xi)
!
      integer(kind = kint), intent(in) :: nnod, nele, nsurf
      integer(kind = kint), intent(in) :: nnod_4_surf, iflag_back
      integer(kind = kint), intent(in) :: ie_surf(nsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(nele,nsurf_4_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
      integer(kind = kint), intent(in) :: iele, isf_org
      real(kind = kreal), intent(in) :: fline(3), x0(3)
!
      real(kind = kreal), intent(inout) :: x_tgt(3), xi(2)
      integer(kind = kint), intent(inout) :: isf_tgt
!
      real(kind = kreal) :: b_ray(3)
      real(kind = kreal) :: x_quad(3,num_linear_sf)
      integer(kind = kint) :: ierr
      integer(kind = kint) :: ist, ied, inc, k, k1, k2, inod, isurf
!
!
      if(iflag_back .eq. 1) then
        b_ray(1:3) = -fline(1:3)
      else
        b_ray(1:3) =  fline(1:3)
      end if
!
      if(isf_org .eq. 0) then
        ist =  1
        ied =  nsurf_4_ele
        inc =  1
      else if(mod(isf_org,itwo) .eq. ione) then
        ist =  1
        ied =  nsurf_4_ele-1
        inc =  1
      else
        ist =  nsurf_4_ele-1
        ied =  1
        inc = -1
      end if
!
      isf_tgt = izero
      do k = ist, ied, inc
        k1 = mod(isf_org+k-ione,nsurf_4_ele) + ione
        isurf = abs(isf_4_ele(iele,k1))
!
        do k2 = 1, num_linear_sf
          inod = ie_surf(isurf,k2)
          x_quad(1:3,k2) = xx(inod,1:3)
        end do
!
        call cal_fline_to_square(x0, b_ray, x_quad,  x_tgt, xi, ierr)
        if(ierr.eq.zero) then
          isf_tgt = k1
          exit
        end if
      end do
!
      if(isf_tgt .gt. izero) return
!
!      write(my_rank+60,'(i3,1p3e16.7)') (-ione), b_ray(1:3)
!      write(my_rank+60,'(i3,1p3e16.7)') izero, x0(1:3)
!
!
      do k = ist, ied, inc
        k1 = mod(isf_org+k-ione,nsurf_4_ele) + ione
!
        isurf = abs(isf_4_ele(iele,k1))
        do k2 = 1, num_linear_sf
          inod = ie_surf(isurf,k2)
          x_quad(1:3,k2) =  xx(inod,1:3)
        end do
!
        call cal_fline_to_square(x0, b_ray, x_quad,  x_tgt, xi, ierr)
      end do
!
      end subroutine find_line_end_in_1ele
!
!------------------------------------------------------------------
!
      subroutine cal_fline_to_square(x0, vec, x_quad, x_tgt, xi, ierr)
!
      real(kind = kreal), intent(in) :: x_quad(3,num_linear_sf)
      real(kind = kreal), intent(in) :: vec(3), x0(3)
      real(kind = kreal), intent(inout) :: x_tgt(3), xi(2)
      integer(kind = kint), intent(inout) :: ierr
!
      real(kind = kreal) :: x_tri(3,num_triangle), sol(3,2)
!
      x_tri(1:3,1) = x_quad(1:3,1)
      x_tri(1:3,2) = x_quad(1:3,2)
      x_tri(1:3,3) = x_quad(1:3,4)
!
      call cal_filne_to_triangle(x0, vec, x_tri, x_tgt, sol(1,1), ierr)
!
      if(ierr .eq. izero) then
        xi(1) = -one + two*sol(1,1)
        xi(2) = -one + two*sol(2,1)
        return
      end if
!
      x_tri(1:3,1) = x_quad(1:3,3)
      x_tri(1:3,2) = x_quad(1:3,2)
      x_tri(1:3,3) = x_quad(1:3,4)
!
      call cal_filne_to_triangle(x0, vec, x_tri, x_tgt, sol(1,2), ierr)
      if(ierr .eq. izero) then
        xi(1) = one - two*sol(2,2)
        xi(2) = one - two*sol(1,2)
      end if
!
      end subroutine cal_fline_to_square
!
!------------------------------------------------------------------
!
      subroutine cal_filne_to_triangle(x0, v, x_tri, x_tgt, sol, ierr)
!
      use solver_33_array
!
      real(kind = kreal), intent(in) :: x_tri(3,num_triangle)
      real(kind = kreal), intent(in) :: v(3), x0(3)
      real(kind = kreal), intent(inout) :: x_tgt(3), sol(3)
      integer(kind = kint), intent(inout) :: ierr
!
      real(kind = kreal) :: rvec(3), mat(3,3)
!
!
      rvec(1:3) = x0(1:3) - x_tri(1:3,1)
      mat(1:3,1) = x_tri(1:3,2) - x_tri(1:3,1)
      mat(1:3,2) = x_tri(1:3,3) - x_tri(1:3,1)
      mat(1:3,3) = -v(1:3)
      call solve_33_array(sol, rvec, mat)
!
      if(sol(3).gt.zero .and. sol(1).ge.zero .and. sol(2).ge.zero       &
     &   .and. (sol(1)+sol(2)).le.one) then
        x_tgt(1:3) = x0(1:3) + sol(3) * v(1:3)
        ierr = 0
      else
        ierr = ione
      end if
!
      end subroutine cal_filne_to_triangle
!
!  ---------------------------------------------------------------------
!
      end module cal_fline_in_cube
