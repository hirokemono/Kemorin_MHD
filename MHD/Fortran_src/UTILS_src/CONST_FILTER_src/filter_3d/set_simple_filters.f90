!set_simple_filters.f90
!      module set_simple_filters
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine set_tophat_filter_4_each_nod(nnod, max_size, x_sol)
!!      subroutine set_linear_filter_4_each_nod                         &
!!     &         (nnod, idist, max_neib, max_size, x_sol)
!!      subroutine set_gaussian_filter_each_nod(ref_filter_width,       &
!!     &          numnod, xx, inod, nnod, max_size, x_sol, fil_coef,    &
!!     &          dxidx_nod, dxidy_nod, dxidz_nod,                      &
!!     &          deidx_nod, deidy_nod, deidz_nod,                      &
!!     &          dzidx_nod, dzidy_nod, dzidz_nod)
!!      subroutine normalize_each_filter_weight(fil_coef)
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!      subroutine cal_filter_moms_each_nod_type                        &
!!     &         (inod, ref_m, fil_coef, fil_mat, mom_nod)
!!        type(reference_moments), intent(in) :: ref_m
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod
!!        type(each_filter_coef), intent(in) :: fil_coef
!!        type(matrix_4_filter), intent(in) :: fil_mat
!
      module set_simple_filters
!
      use m_precision
!
      use m_constants
      use calypso_mpi
!
      use t_filter_coefs
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
      subroutine set_tophat_filter_4_each_nod(nnod, max_size, x_sol)
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: max_size
      real(kind = kreal), intent(inout) :: x_sol(max_size)
!
!$omp parallel workshare
      x_sol(1:nnod) = one
!$omp end parallel workshare
!
      end subroutine set_tophat_filter_4_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine set_linear_filter_4_each_nod                           &
     &         (nnod, idist, max_neib, max_size, x_sol)
!
      integer(kind = kint), intent(in) :: nnod, max_neib
      integer(kind = kint), intent(in) :: idist(nnod)
      integer(kind = kint), intent(in) :: max_size
      real(kind = kreal), intent(inout) :: x_sol(max_size)
!
      integer(kind = kint) :: i
!
!$omp parallel do
      do i = 1, nnod
              x_sol(i) = 1.0d0 - dble(idist(i))                     &
     &                          / dble(max_neib+1)
      end do
!$omp end parallel do
!
      end subroutine set_linear_filter_4_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine set_gaussian_filter_each_nod(ref_filter_width,         &
     &          numnod, xx, inod, nnod, max_size, x_sol, fil_coef,      &
     &          dxidx_nod, dxidy_nod, dxidz_nod,                        &
     &          deidx_nod, deidy_nod, deidz_nod,                        &
     &          dzidx_nod, dzidy_nod, dzidz_nod)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: ref_filter_width
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
      integer(kind = kint), intent(in) :: max_size
      real(kind = kreal), intent(inout) :: x_sol(max_size)
      type(each_filter_coef), intent(inout) :: fil_coef
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
      a_filter_w = one / ref_filter_width
      do i = 1, fil_coef%nnod_4_1nod_f
        inod_n = fil_coef%inod_4_1nod_w(i)
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
      subroutine normalize_each_filter_weight(fil_coef)
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
      real(kind = kreal) :: zero_moment
      integer(kind = kint) :: i
!
      zero_moment = 0.0d0
      do i = 1, fil_coef%nnod_4_1nod_w
        zero_moment = zero_moment + fil_coef%weight_1nod(i)
      end do
!
      fil_coef%weight_1nod(1:fil_coef%nnod_4_1nod_w)                    &
     &   = fil_coef%weight_1nod(1:fil_coef%nnod_4_1nod_w) / zero_moment
      fil_coef%filter_1nod(1:fil_coef%nnod_4_1nod_f)                    &
     &   = fil_coef%filter_1nod(1:fil_coef%nnod_4_1nod_f) / zero_moment
!
      end subroutine normalize_each_filter_weight
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moms_each_nod_type                          &
     &         (inod, ref_m, fil_coef, fil_mat, mom_nod)
!
      use t_filter_moments
      use t_reference_moments
      use t_matrix_4_filter
!
      integer(kind = kint), intent(in) :: inod
      type(reference_moments), intent(in) :: ref_m
      type(each_filter_coef), intent(in) :: fil_coef
      type(matrix_4_filter), intent(in) :: fil_mat
!
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!
      call cal_filter_moments_each_nod                                  &
     &   (fil_coef, fil_mat, ref_m%num_order_3d, ref_m%iorder_mom_3d,   &
     &    mom_nod%moms%f_x(inod), mom_nod%moms%f_y(inod),               &
     &    mom_nod%moms%f_z(inod), mom_nod%moms%f_x2(inod),              &
     &    mom_nod%moms%f_y2(inod), mom_nod%moms%f_z2(inod),             &
     &    mom_nod%moms%f_xy(inod),  mom_nod%moms%f_yz(inod),            &
     &    mom_nod%moms%f_zx(inod))
!
      end subroutine cal_filter_moms_each_nod_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moments_each_nod                            &
     &         (fil_coef, fil_mat, num_order_3d, iorder_mom_3d,         &
     &          filter_x_nod,  filter_y_nod,  filter_z_nod,             &
     &          filter_x2_nod, filter_y2_nod, filter_z2_nod,            &
     &          filter_xy_nod, filter_yz_nod, filter_zx_nod)
!
      use t_filter_moments
      use t_matrix_4_filter
!
      type(each_filter_coef), intent(in) :: fil_coef
      type(matrix_4_filter), intent(in) :: fil_mat
      integer(kind = kint), intent(in) :: num_order_3d
      integer(kind = kint), intent(in) :: iorder_mom_3d(num_order_3d,3)
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
      do j = 1, fil_mat%max_mat_size
        kx = iorder_mom_3d(j,1)
        ky = iorder_mom_3d(j,2)
        kz = iorder_mom_3d(j,3)
        if ( (kx+ky+kz).eq.1 .or. (kx+ky+kz).eq.2) then
          icou = icou + 1
          if     (kx.eq.1 .and. ky.eq.0 .and. kz.eq.0) then
            call cal_moments_from_matrix(fil_coef, j,                   &
     &          fil_mat%max_mat_size, fil_mat%a_mat, filter_x_nod )
          else if(kx.eq.0 .and. ky.eq.1 .and. kz.eq.0) then
            call cal_moments_from_matrix(fil_coef, j,                   &
     &          fil_mat%max_mat_size, fil_mat%a_mat, filter_y_nod )
          else if(kx.eq.0 .and. ky.eq.0 .and. kz.eq.1) then
            call cal_moments_from_matrix(fil_coef, j,                   &
     &          fil_mat%max_mat_size, fil_mat%a_mat, filter_z_nod )
          else if(kx.eq.1 .and. ky.eq.1 .and. kz.eq.0) then
            call cal_moments_from_matrix(fil_coef, j,                   &
     &          fil_mat%max_mat_size, fil_mat%a_mat, filter_xy_nod)
          else if(kx.eq.0 .and. ky.eq.1 .and. kz.eq.1) then
            call cal_moments_from_matrix(fil_coef, j,                   &
     &          fil_mat%max_mat_size, fil_mat%a_mat, filter_yz_nod)
          else if(kx.eq.1 .and. ky.eq.0 .and. kz.eq.1) then
            call cal_moments_from_matrix(fil_coef, j,                   &
     &          fil_mat%max_mat_size, fil_mat%a_mat, filter_zx_nod)
          else if(kx.eq.2 .and. ky.eq.0 .and. kz.eq.0) then
            call cal_moments_from_matrix(fil_coef, j,                   &
     &          fil_mat%max_mat_size, fil_mat%a_mat, filter_x2_nod)
          else if(kx.eq.0 .and. ky.eq.2 .and. kz.eq.0) then
            call cal_moments_from_matrix(fil_coef, j,                   &
     &          fil_mat%max_mat_size, fil_mat%a_mat, filter_y2_nod)
          else if(kx.eq.0 .and. ky.eq.0 .and. kz.eq.2) then
            call cal_moments_from_matrix(fil_coef, j,                   &
     &          fil_mat%max_mat_size, fil_mat%a_mat, filter_z2_nod)
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
      subroutine cal_moments_from_matrix                                &
     &         (fil_coef, j, max_size, a_mat, filter_moment)
!
      type(each_filter_coef), intent(in) :: fil_coef
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: max_size
      real(kind = kreal), intent(in) :: a_mat(max_size,max_size)
!
      real(kind = kreal), intent(inout) :: filter_moment
!
      integer(kind = kint) :: i
!
      do i = 1, fil_coef%nnod_4_1nod_w
        filter_moment = filter_moment                                   &
     &                 + a_mat(j,i) * fil_coef%filter_1nod(i)
      end do
!
      end subroutine cal_moments_from_matrix
!
!-----------------------------------------------------------------------
!
      end module set_simple_filters
