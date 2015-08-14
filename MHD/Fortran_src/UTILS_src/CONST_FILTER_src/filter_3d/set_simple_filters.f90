!set_simple_filters.f90
!      module set_simple_filters
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine set_tophat_filter_4_each_nod
!!      subroutine set_linear_filter_4_each_nod
!!      subroutine set_gaussian_filter_each_nod(numnod, xx, inod, nnod, &
!!     &          dxidx_nod, dxidy_nod, dxidz_nod,                      &
!!     &          deidx_nod, deidy_nod, deidz_nod,                      &
!!     &          dzidx_nod, dzidy_nod, dzidz_nod)
!!      subroutine normalize_each_filter_weight
!!      subroutine cal_filter_moms_each_nod_type(inod, mom_nod)
!
      module set_simple_filters
!
      use m_precision
!
      use m_constants
      use calypso_mpi
!
      implicit none
!
      private :: cal_moments_from_matrix, cal_filter_moments_each_nod
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_tophat_filter_4_each_nod
!
      use m_filter_coefs
      use m_matrix_4_filter
!
        x_sol(1:nnod_near_1nod_filter) = 1.0d0
!
      end subroutine set_tophat_filter_4_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine set_linear_filter_4_each_nod
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_matrix_4_filter
!
      integer(kind = kint) :: i
!
      do i = 1, nnod_near_1nod_filter
              x_sol(i) = 1.0d0 - dble(idist_from_center_1nod(i))        &
     &                          / dble(maximum_neighbour+1)
      end do
!
      end subroutine set_linear_filter_4_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine set_gaussian_filter_each_nod(numnod, xx, inod, nnod,   &
     &          dxidx_nod, dxidy_nod, dxidz_nod,                        &
     &          deidx_nod, deidy_nod, deidz_nod,                        &
     &          dzidx_nod, dzidy_nod, dzidz_nod)
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_matrix_4_filter
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: inod, nnod
      real(kind=kreal), intent(in) :: dxidx_nod(nnod)
      real(kind=kreal), intent(in) :: deidx_nod(nnod)
      real(kind=kreal), intent(in) :: dzidx_nod(nnod)
      real(kind=kreal), intent(in) :: dxidy_nod(nnod)
      real(kind=kreal), intent(in) :: deidy_nod(nnod)
      real(kind=kreal), intent(in) :: dzidy_nod(nnod)
      real(kind=kreal), intent(in) :: dxidz_nod(nnod)
      real(kind=kreal), intent(in) :: deidz_nod(nnod)
      real(kind=kreal), intent(in) :: dzidz_nod(nnod)
!
!
      integer(kind = kint) :: i, inod_n
      real(kind = kreal) :: pi
      real(kind = kreal) :: dx2, dy2, dz2, dydz, dzdx, dxdy
      real(kind = kreal) :: x_x, y_y, z_z
      real(kind = kreal) :: a_filter_w
!
!
      pi = four * atan(one)
      a_filter_w = one / ref_filter_width(1)
      do i = 1, nnod_near_1nod_filter
        inod_n = inod_near_1nod_weight(i)
        dx2 =  dxidx_nod(inod_n)*dxidx_nod(inod_n)                      &
     &       + deidx_nod(inod_n)*deidx_nod(inod_n)                      &
     &       + dzidx_nod(inod_n)*dzidx_nod(inod_n)
        dy2 =  dxidy_nod(inod_n)*dxidy_nod(inod_n)                      &
     &       + deidy_nod(inod_n)*deidy_nod(inod_n)                      &
     &       + dzidy_nod(inod_n)*dzidy_nod(inod_n)
        dz2 =  dxidz_nod(inod_n)*dxidz_nod(inod_n)                      &
     &       + deidz_nod(inod_n)*deidz_nod(inod_n)                      &
     &       + dzidz_nod(inod_n)*dzidz_nod(inod_n)
        dydz = two * ( dxidy_nod(inod_n)*dxidz_nod(inod_n)              &
     &               + deidy_nod(inod_n)*deidz_nod(inod_n)              &
     &               + dzidy_nod(inod_n)*dzidz_nod(inod_n) )
        dzdx = two * ( dxidz_nod(inod_n)*dxidx_nod(inod_n)              &
     &               + deidz_nod(inod_n)*deidx_nod(inod_n)              &
     &               + dzidz_nod(inod_n)*dzidx_nod(inod_n) )
        dxdy = two * ( dxidx_nod(inod_n)*dxidy_nod(inod_n)              &
     &               + deidx_nod(inod_n)*deidy_nod(inod_n)              &
     &               + dzidx_nod(inod_n)*dzidy_nod(inod_n) )
!
        x_x = xx(inod,1) - xx(inod_n,1)
        y_y = xx(inod,2) - xx(inod_n,2)
        z_z = xx(inod,3) - xx(inod_n,3)

        x_sol(i) = r125*a_filter_w*(six/pi)*sqrt(six/pi)                &
     &            * exp(- (six*quad*a_filter_w                          &
     &             * ( dx2*x_x*x_x +  dy2*y_y*y_y +  dz2*z_z*z_z        &
     &               + dydz*y_y*z_z + dzdx*z_z*x_x + dxdy*x_x*y_y) ))
      end do
!
      end subroutine set_gaussian_filter_each_nod
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine normalize_each_filter_weight
!
      use m_filter_coefs
!
      real(kind = kreal) :: zero_moment
      integer(kind = kint) :: i
!
      zero_moment = 0.0d0
      do i = 1, nnod_near_1nod_weight
        zero_moment = zero_moment + weight_1nod(i)
      end do
!
      weight_1nod(1:nnod_near_1nod_weight)                              &
     &        = weight_1nod(1:nnod_near_1nod_weight) / zero_moment
      filter_1nod(1:nnod_near_1nod_filter)                              &
     &        = filter_1nod(1:nnod_near_1nod_filter) / zero_moment
!
      end subroutine normalize_each_filter_weight
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moms_each_nod_type(inod, mom_nod)
!
      use t_filter_moments
!
      integer(kind = kint), intent(in) :: inod
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!
      call cal_filter_moments_each_nod(mom_nod%moms%f_x(inod),          &
     &       mom_nod%moms%f_y(inod),   mom_nod%moms%f_z(inod),          &
     &       mom_nod%moms%f_x2(inod),  mom_nod%moms%f_y2(inod),         &
     &       mom_nod%moms%f_z2(inod),  mom_nod%moms%f_xy(inod),         &
     &       mom_nod%moms%f_yz(inod),  mom_nod%moms%f_zx(inod))
!
      end subroutine cal_filter_moms_each_nod_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moments_each_nod                            &
     &         (filter_x_nod,  filter_y_nod,  filter_z_nod,             &
     &          filter_x2_nod, filter_y2_nod, filter_z2_nod,            &
     &          filter_xy_nod, filter_yz_nod, filter_zx_nod)
!
      use m_reference_moments
      use m_matrix_4_filter
!
      real(kind = kreal), intent(inout) :: filter_x_nod
      real(kind = kreal), intent(inout) :: filter_y_nod
      real(kind = kreal), intent(inout) :: filter_z_nod
      real(kind = kreal), intent(inout) :: filter_x2_nod
      real(kind = kreal), intent(inout) :: filter_y2_nod
      real(kind = kreal), intent(inout) :: filter_z2_nod
      real(kind = kreal), intent(inout) :: filter_xy_nod
      real(kind = kreal), intent(inout) :: filter_yz_nod
      real(kind = kreal), intent(inout) :: filter_zx_nod
!
!
      integer(kind = kint) :: j, icou, kx, ky, kz
!
      icou = 0
      do j = 1, max_mat_size
        kx = iorder_mom_3d(j,1)
        ky = iorder_mom_3d(j,2)
        kz = iorder_mom_3d(j,3)
        if ( (kx+ky+kz).eq.1 .or. (kx+ky+kz).eq.2) then
          icou = icou + 1
          if     (kx.eq.1 .and. ky.eq.0 .and. kz.eq.0) then
            call cal_moments_from_matrix(j, filter_x_nod )
          else if(kx.eq.0 .and. ky.eq.1 .and. kz.eq.0) then
            call cal_moments_from_matrix(j, filter_y_nod )
          else if(kx.eq.0 .and. ky.eq.0 .and. kz.eq.1) then
            call cal_moments_from_matrix(j, filter_z_nod )
          else if(kx.eq.1 .and. ky.eq.1 .and. kz.eq.0) then
            call cal_moments_from_matrix(j, filter_xy_nod)
          else if(kx.eq.0 .and. ky.eq.1 .and. kz.eq.1) then
            call cal_moments_from_matrix(j, filter_yz_nod)
          else if(kx.eq.1 .and. ky.eq.0 .and. kz.eq.1) then
            call cal_moments_from_matrix(j, filter_zx_nod)
          else if(kx.eq.2 .and. ky.eq.0 .and. kz.eq.0) then
            call cal_moments_from_matrix(j, filter_x2_nod)
          else if(kx.eq.0 .and. ky.eq.2 .and. kz.eq.0) then
            call cal_moments_from_matrix(j, filter_y2_nod)
          else if(kx.eq.0 .and. ky.eq.0 .and. kz.eq.2) then
            call cal_moments_from_matrix(j, filter_z2_nod)
          end if
        else if(icou .eq. 9) then
          exit
        end if
      end do
!
      end subroutine cal_filter_moments_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine cal_moments_from_matrix(j, filter_moment)
!
      use m_filter_coefs
      use m_matrix_4_filter
!
      integer(kind = kint), intent(in) :: j
      real(kind = kreal), intent(inout) :: filter_moment
      integer(kind = kint) :: i
!
      do i = 1, nnod_near_1nod_weight
        filter_moment = filter_moment + a_mat(j,i) * filter_1nod(i)
      end do
!
      end subroutine cal_moments_from_matrix
!
!-----------------------------------------------------------------------
!
      end module set_simple_filters
