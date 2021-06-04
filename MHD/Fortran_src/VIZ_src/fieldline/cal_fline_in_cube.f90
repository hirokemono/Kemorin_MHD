!>@file  cal_fline_in_cube.f90
!!       module cal_fline_in_cube
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief trace field line in one cube element
!!
!!@verbatim
!!      subroutine find_line_end_in_1ele                                &
!!     &         (iflag_dir, nnod, nele, nnod_4_ele, ie, node_on_sf,    &
!!     &          nsurf, nnod_4_surf, isf_4_ele, ie_surf, xx,           &
!!     &          iele, isf_org, fline, x0, isf_tgt, x4_tgt, xi)
!!        integer(kind = kint), intent(in) :: nnod, nele, nsurf
!!        integer(kind = kint), intent(in) :: nnod_4_surf, iflag_dir
!!        integer(kind = kint), intent(in) :: ie_surf(nsurf,nnod_4_surf)
!!        integer(kind = kint), intent(in) :: isf_4_ele(nele,nsurf_4_ele)
!!        real(kind = kreal), intent(in) :: xx(nnod,3)
!!        integer(kind = kint), intent(in) :: iele, isf_org
!!        real(kind = kreal), intent(in) :: fline(4), x0(4)
!!        real(kind = kreal), intent(inout) :: x4_tgt(4)
!!        real(kind = kreal), intent(inout) :: xi(2)
!!        integer(kind = kint), intent(inout) :: isf_tgt
!!      subroutine cal_fline_to_square(x0, vec, x_quad, x_tgt, ierr)
!!      subroutine cal_filne_to_triangle(x0, vec, x_tri, x_tgt, ierr)
!!@endverbatim
!
      module cal_fline_in_cube
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
!
!
      implicit  none
!
      integer(kind = kint), parameter :: iflag_forward_line =   1
      integer(kind = kint), parameter :: iflag_backward_line = -1
!
      private :: cal_fline_to_square, cal_filne_to_triangle
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine find_line_end_in_1ele                                  &
     &         (iflag_dir, nnod, nele, nnod_4_ele, ie, node_on_sf,      &
     &          nsurf, nnod_4_surf, isf_4_ele, ie_surf, xx,             &
     &          iele, isf_org, fline, x0, isf_tgt, x4_tgt, xi)
!
      integer(kind = kint), intent(in) :: nnod, nele, nsurf
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_surf
      integer(kind = kint), intent(in) :: iflag_dir
      integer(kind = kint), intent(in) :: ie(nele,nnod_4_ele)
      integer(kind = kint), intent(in) :: ie_surf(nsurf,nnod_4_surf)
      integer(kind = kint), intent(in)                                  &
     &                  :: node_on_sf(nnod_4_surf,nsurf_4_ele)
      integer(kind = kint), intent(in) :: isf_4_ele(nele,nsurf_4_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
      integer(kind = kint), intent(in) :: iele, isf_org
      real(kind = kreal), intent(in) :: fline(4), x0(4)
!
      real(kind = kreal), intent(inout) :: x4_tgt(4)
      real(kind = kreal), intent(inout) :: xi(2)
      integer(kind = kint), intent(inout) :: isf_tgt
!
      real(kind = kreal) :: b_ray(4)
      real(kind = kreal) :: x_quad(4,num_linear_sf)
      integer(kind = kint) :: ierr
      integer(kind = kint) :: ist, ied, inc, k, k1, k2
      integer(kind = kint) :: inod, isurf, jnod, k_surf
      integer(kind = kint) :: ie_one(nnod_4_ele)
      integer(kind = kint) :: ie_ele_surf(nnod_4_surf,nsurf_4_ele)
      real(kind = kreal) :: xx4_ele_surf(4,nnod_4_surf,nsurf_4_ele)
!
!
      if(iflag_dir .eq. iflag_forward_line) then
        b_ray(1:4) = -fline(1:4)
      else
        b_ray(1:4) =  fline(1:4)
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
      ie_one(1:nnod_4_ele) = ie(iele,1:nnod_4_ele)
      do k1 = 1, nsurf_4_ele
        isurf = abs(isf_4_ele(iele,k1))
        ie_ele_surf(1:nnod_4_surf,k1) = ie_surf(isurf,1:nnod_4_surf)
        xx4_ele_surf(1,1:nnod_4_surf,k1)      &
     &       = xx(ie_surf(isurf,1:nnod_4_surf),1)
        xx4_ele_surf(2,1:nnod_4_surf,k1)      &
     &       = xx(ie_surf(isurf,1:nnod_4_surf),2)
        xx4_ele_surf(3,1:nnod_4_surf,k1)      &
     &       = xx(ie_surf(isurf,1:nnod_4_surf),3)
        xx4_ele_surf(4,1:nnod_4_surf,k1) = zero
      end do
!
      isf_tgt = izero
      do k = ist, ied, inc
        k1 = mod(isf_org+k-ione,nsurf_4_ele) + ione
        isurf = abs(isf_4_ele(iele,k1))
!
        do k2 = 1, num_linear_sf
          k_surf = node_on_sf(k2,k1)
          jnod = ie_ele_surf(k2,k1)
          inod = ie_surf(isurf,k2)
          if(ie_surf(isurf,k2) .ne. ie_ele_surf(k2,k1)) write(*,*)   &
     &           'someting wrong in node address', iele, isurf, k1, k2, &
     &            iele, isurf, k_surf, ie_one(1:8), inod
          if(xx(inod,1) .ne. xx4_ele_surf(1,k2,k1)) write(*,*)   &
     &           'someting wrong in x position', iele, isurf, k1, k2
          if(xx(inod,2) .ne. xx4_ele_surf(2,k2,k1)) write(*,*)   &
     &           'someting wrong in y position', iele, isurf, k1, k2
          if(xx(inod,3) .ne. xx4_ele_surf(3,k2,k1)) write(*,*)   &
     &           'someting wrong in z position', iele, isurf, k1, k2
          x_quad(1:3,k2) = xx(inod,1:3)
          x_quad(4,k2) = 0.0d0
        end do
!
        call cal_fline_to_square(x0, b_ray, x_quad, x4_tgt, xi, ierr)
        if(ierr.eq.zero) then
          isf_tgt = k1
          exit
        end if
      end do
!
      if(isf_tgt .gt. izero) return
!
!      write(my_rank+60,'(i3,1p3e16.7)') (-ione), b_ray(1:4)
!      write(my_rank+60,'(i3,1p3e16.7)') izero, x0(1:4)
!
!
      do k = ist, ied, inc
        k1 = mod(isf_org+k-ione,nsurf_4_ele) + ione
!
        isurf = abs(isf_4_ele(iele,k1))
        do k2 = 1, num_linear_sf
          k_surf = node_on_sf(k2,k1)
          jnod = ie_one(k_surf)
          inod = ie_surf(isurf,k2)
          x_quad(1:3,k2) =  xx(inod,1:3)
          x_quad(4,k2) = 0.0d0
        end do
!
        call cal_fline_to_square(x0, b_ray, x_quad, x4_tgt, xi, ierr)
      end do
!
      end subroutine find_line_end_in_1ele
!
!------------------------------------------------------------------
!
      subroutine cal_fline_to_square(x0, vec, x_quad, x4_tgt, xi, ierr)
!
      real(kind = kreal), intent(in) :: x_quad(4,num_linear_sf)
      real(kind = kreal), intent(in) :: vec(4), x0(4)
      real(kind = kreal), intent(inout) :: x4_tgt(4), xi(2)
      integer(kind = kint), intent(inout) :: ierr
!
      real(kind = kreal) :: x4_tri(4,num_triangle), sol(4,2)
!
      x4_tri(1:4,1) = x_quad(1:4,1)
      x4_tri(1:4,2) = x_quad(1:4,2)
      x4_tri(1:4,3) = x_quad(1:4,4)
!
      call cal_filne_to_triangle                                        &
     &   (x0, vec, x4_tri, x4_tgt, sol(1,1), ierr)
!
      if(ierr .eq. izero) then
        xi(1) = -one + two*sol(1,1)
        xi(2) = -one + two*sol(2,1)
        return
      end if
!
      x4_tri(1:4,1) = x_quad(1:4,3)
      x4_tri(1:4,2) = x_quad(1:4,2)
      x4_tri(1:4,3) = x_quad(1:4,4)
!
      call cal_filne_to_triangle                                        &
     &   (x0, vec, x4_tri, x4_tgt, sol(1,2), ierr)
      if(ierr .eq. izero) then
        xi(1) = one - two*sol(2,2)
        xi(2) = one - two*sol(1,2)
      end if
!
      end subroutine cal_fline_to_square
!
!------------------------------------------------------------------
!
      subroutine cal_filne_to_triangle(x0, v, x4_tri, x4_tgt, sol, ierr)
!
      use solver_33_array
!
      real(kind = kreal), intent(in) :: x4_tri(4,num_triangle)
      real(kind = kreal), intent(in) :: v(4), x0(4)
      real(kind = kreal), intent(inout) :: x4_tgt(4), sol(4)
      integer(kind = kint), intent(inout) :: ierr
!
      real(kind = kreal) :: rvec(4), mat(3,3)
!
!
      rvec(1:4) = x0(1:4) - x4_tri(1:4,1)
!
      mat(1:3,1) = x4_tri(1:3,2) - x4_tri(1:3,1)
      mat(1:3,2) = x4_tri(1:3,3) - x4_tri(1:3,1)
      mat(1:3,3) = -v(1:3)
      call solve_33_array(sol(1), rvec(1), mat)
!
      if(sol(3).gt.zero .and. sol(1).ge.zero .and. sol(2).ge.zero       &
     &   .and. (sol(1)+sol(2)).le.one) then
        x4_tgt(1:4) = x0(1:4) + sol(3) * v(1:4)
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
