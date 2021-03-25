!>@file   cal_noise_value.f90
!!@brief  module cal_noise_value
!!
!!@author Y. Liao and H. Matsui
!!@date Programmed by Y. Liao in Apr. 2018
!!      Modified by H. Matsui in Apr. 2020
!!
!> @brief Get noise value for each point by interpolation
!!
!!@verbatim
!!      subroutine interpolate_kernel(iflag_dir, x, knl, value)
!!        type(LIC_kernel), intent(in) :: knl
!!      subroutine interpolate_noise_at_node                            &
!!     &         (xyz, nze, point_noise, point_grad_noise)
!!      subroutine pick_noise_at_node                                   &
!!     &         (xyz, nze, point_noise, point_grad_noise)
!!        real(kind = kreal), intent(in) :: xyz(3)
!!        type(noise_cube), intent(in) :: nze
!!        real(kind = kreal), intent(inout) :: point_noise
!!        real(kind = kreal), intent(inout) :: point_grad_noise(3)
!!@endverbatim
!
      module cal_noise_value
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_LIC_kernel
      use t_3d_noise
!
      implicit  none
!
      private :: cal_1d_local_id_and_position
      private :: cal_data_noise_interpolation
      private :: interpolate_noise, pick_noise_at_point
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine interpolate_kernel(iflag_dir, x, knl, value)
!
      integer(kind = kint), intent(in) :: iflag_dir
      real(kind = kreal), intent(in) :: x
      type(LIC_kernel), intent(in) :: knl
!
      real(kind = kreal), intent(inout) :: value
!
      real(kind = kreal) :: x_local, x_pos
      integer(kind = kint) :: ijk_n, ijk_p
!
!
      x_pos = knl%half_lengh + dble(iflag_dir) * min(x, knl%half_lengh)
!
      call cal_1d_local_id_and_position                                 &
     &   (x_pos, knl%alength, knl%n_knl, x_local, ijk_n)
!
      ijk_p = min(ijk_n+1, knl%n_knl-1)
      value =  knl%k_ary(ijk_n+1) * (one - x_local)                     &
     &       + knl%k_ary(ijk_p+1) * x_local
!
      end subroutine interpolate_kernel
!
!  ---------------------------------------------------------------------
!
      subroutine interpolate_noise_at_node                              &
     &         (xyz, nze, point_noise, point_grad_noise)
!
      real(kind = kreal), intent(in) :: xyz(3)
      type(noise_cube), intent(in) :: nze
!
      real(kind = kreal), intent(inout) :: point_noise
      real(kind = kreal), intent(inout) :: point_grad_noise(3)
!
      integer(kind = kint_gl) :: ie_cube(8)
      real(kind = kreal) :: an(8)
!
!
      call cal_data_noise_interpolation                                 &
     &   (xyz, nze%nidx_xyz, nze%asize_cube, ie_cube, an)
!
      point_noise = interpolate_noise(ie_cube, an, nze%n_cube,          &
     &                                nze%rnoise)
      point_grad_noise(1) = interpolate_noise(ie_cube, an, nze%n_cube,  &
     &                                nze%rnoise_grad(1,1))
      point_grad_noise(2) = interpolate_noise(ie_cube, an, nze%n_cube,  &
     &                                nze%rnoise_grad(1,2))
      point_grad_noise(3) = interpolate_noise(ie_cube, an, nze%n_cube,  &
     &                                nze%rnoise_grad(1,3))
!
      end subroutine interpolate_noise_at_node
!
!  ---------------------------------------------------------------------
!
      subroutine pick_noise_at_node                                     &
     &         (xyz, nze, point_noise, point_grad_noise)
!
      real(kind = kreal), intent(in) :: xyz(3)
      type(noise_cube), intent(in) :: nze
!
      real(kind = kreal), intent(inout) :: point_noise
      real(kind = kreal), intent(inout) :: point_grad_noise(3)
!
      integer(kind = kint_gl) :: ie_cube(8)
      real(kind = kreal) :: an(8)
!
!
      call cal_data_noise_interpolation                                 &
     &   (xyz, nze%nidx_xyz, nze%asize_cube, ie_cube, an)
!
      point_noise = pick_noise_at_point(ie_cube, nze%n_cube,            &
     &                                nze%rnoise)
      point_grad_noise(1) = pick_noise_at_point(ie_cube, nze%n_cube,    &
     &                                nze%rnoise_grad(1,1))
      point_grad_noise(2) = pick_noise_at_point(ie_cube, nze%n_cube,    &
     &                                nze%rnoise_grad(1,2))
      point_grad_noise(3) = pick_noise_at_point(ie_cube, nze%n_cube,    &
     &                                nze%rnoise_grad(1,3))
!
      end subroutine pick_noise_at_node
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_data_noise_interpolation                           &
     &         (xyz, nidx_xyz, asize_cube, ie_cube, an)
!
      real(kind = kreal), intent(in) :: xyz(3)
      real(kind = kreal), intent(in) :: asize_cube(3)
      integer(kind = kint), intent(in) :: nidx_xyz(3)
      integer(kind = kint_gl), intent(inout) :: ie_cube(8)
      real(kind = kreal), intent(inout) :: an(8)
!
      real(kind = kreal) :: xyz_n(3)
      integer(kind = kint) :: ijk_n(3), ijk_p(3)
      integer(kind = kint_gl) :: inod_nn, inod_pn, inod_pp, inod_np
      integer(kind = kint_gl) :: inod_zn, inod_zp
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        call cal_1d_local_id_and_position                               &
     &    (xyz(nd), asize_cube(nd), nidx_xyz(nd), xyz_n(nd), ijk_n(nd))
        ijk_p(nd) = mod(ijk_n(nd)+1, nidx_xyz(nd))
      end do
!
      inod_nn = int((ijk_n(1) + nidx_xyz(1)*ijk_n(2)), KIND(inod_nn))
      inod_pn = int((ijk_p(1) + nidx_xyz(1)*ijk_n(2)), KIND(inod_pn))
      inod_pp = int((ijk_p(1) + nidx_xyz(1)*ijk_p(2)), KIND(inod_pp))
      inod_np = int((ijk_n(1) + nidx_xyz(1)*ijk_p(2)), KIND(inod_np))
      inod_zn = ijk_n(3) * int((nidx_xyz(1)*nidx_xyz(2)),KIND(inod_zn))
      inod_zp = ijk_n(3) * int((nidx_xyz(1)*nidx_xyz(2)),KIND(inod_zp))
!
      ie_cube(1) = 1 + inod_nn + inod_zn
      ie_cube(2) = 1 + inod_pn + inod_zn
      ie_cube(3) = 1 + inod_pp + inod_zn
      ie_cube(4) = 1 + inod_np + inod_zn
      ie_cube(5) = 1 + inod_nn + inod_zp
      ie_cube(6) = 1 + inod_pn + inod_zp
      ie_cube(7) = 1 + inod_pp + inod_zp
      ie_cube(8) = 1 + inod_np + inod_zp
!
      an(1) = (one-xyz_n(1)) * (one-xyz_n(2)) * (one-xyz_n(3))
      an(2) = (    xyz_n(1)) * (one-xyz_n(2)) * (one-xyz_n(3))
      an(3) = (    xyz_n(1)) * (    xyz_n(2)) * (one-xyz_n(3))
      an(4) = (one-xyz_n(1)) * (    xyz_n(2)) * (one-xyz_n(3))
      an(5) = (one-xyz_n(1)) * (one-xyz_n(2)) * (    xyz_n(3))
      an(6) = (    xyz_n(1)) * (one-xyz_n(2)) * (    xyz_n(3))
      an(7) = (    xyz_n(1)) * (    xyz_n(2)) * (    xyz_n(3))
      an(8) = (one-xyz_n(1)) * (    xyz_n(2)) * (    xyz_n(3))
!
      end subroutine cal_data_noise_interpolation
!
!  ---------------------------------------------------------------------
!
      subroutine cal_1d_local_id_and_position                           &
     &         (x, asize, nidx, x_local, ijk_n)
!
      real(kind = kreal), intent(in) :: x
      real(kind = kreal), intent(in) :: asize
      integer(kind = kint), intent(in) :: nidx
!
      integer(kind = kint), intent(inout) :: ijk_n
      real(kind = kreal), intent(inout) :: x_local
!
      real(kind = kreal) :: xyz_f, xyz_c, xyz_s, xyz_t
!
!
      xyz_s = x * asize
      xyz_t = xyz_s - dble(int(xyz_s))
      xyz_f = xyz_t - sign(half, xyz_t) + half
      xyz_c = mod(xyz_f,1.0d0) * dble(nidx)
      x_local = xyz_c - dble(int(xyz_c))
      ijk_n = int(xyz_c)
!
      end subroutine cal_1d_local_id_and_position
!
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function interpolate_noise                     &
     &                          (ie_cube, an, n_cube, noise_cube)
!
      integer(kind = kint_gl), intent(in) :: ie_cube(8)
      integer(kind = kint_gl), intent(in) :: n_cube
      real(kind = kreal), intent(in) :: noise_cube(n_cube)
      real(kind = kreal), intent(in) :: an(8)
!
!
      interpolate_noise =  an(1) * noise_cube(ie_cube(1))               &
     &                   + an(2) * noise_cube(ie_cube(2))               &
     &                   + an(3) * noise_cube(ie_cube(3))               &
     &                   + an(4) * noise_cube(ie_cube(4))               &
     &                   + an(5) * noise_cube(ie_cube(5))               &
     &                   + an(6) * noise_cube(ie_cube(6))               &
     &                   + an(7) * noise_cube(ie_cube(7))               &
     &                   + an(8) * noise_cube(ie_cube(8))
!
      end function interpolate_noise
!
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function pick_noise_at_point                   &
     &                          (ie_cube, n_cube, noise_cube)
!
      integer(kind = kint_gl), intent(in) :: ie_cube(8)
      integer(kind = kint_gl), intent(in) :: n_cube
      real(kind = kreal), intent(in) :: noise_cube(n_cube)
!
!
      pick_noise_at_point =  noise_cube(ie_cube(1))
!
      end function pick_noise_at_point
!
!  ---------------------------------------------------------------------
!
      end module cal_noise_value
