!set_simple_filters.f90
!      module set_simple_filters
!
      module set_simple_filters
!
!     Written by H. Matsui on Mar., 2008
!
      use m_precision
!
      use m_constants
      use m_parallel_var_dof
!
      implicit none
!
      private :: cal_moments_from_matrix
!
!      subroutine set_tophat_filter_4_each_nod
!      subroutine set_linear_filter_4_each_nod
!      subroutine set_gaussian_filter_each_nod(inod)
!      subroutine normalize_each_filter_weight
!      subroutine cal_filter_moments_each_nod(id_f, inod)
!      subroutine copy_filter_moments_each_nod(id_dest, id_org, inod)
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
      subroutine set_gaussian_filter_each_nod(inod)
!
      use m_ctl_params_4_gen_filter
      use m_geometry_data
      use m_dxi_dxes_3d_node
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_matrix_4_filter
!
      integer(kind = kint), intent(in) :: inod
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
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moments_each_nod(id_f, inod)
!
      use m_reference_moments
      use m_filter_moments
      use m_matrix_4_filter
!
      integer(kind = kint), intent(in) :: id_f, inod
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
            call cal_moments_from_matrix(j, filter_x_nod(inod,id_f))
          else if(kx.eq.0 .and. ky.eq.1 .and. kz.eq.0) then
            call cal_moments_from_matrix(j, filter_y_nod(inod,id_f))
          else if(kx.eq.0 .and. ky.eq.0 .and. kz.eq.1) then
            call cal_moments_from_matrix(j, filter_z_nod(inod,id_f))
          else if(kx.eq.1 .and. ky.eq.1 .and. kz.eq.0) then
            call cal_moments_from_matrix(j, filter_xy_nod(inod,id_f))
          else if(kx.eq.0 .and. ky.eq.1 .and. kz.eq.1) then
            call cal_moments_from_matrix(j, filter_yz_nod(inod,id_f))
          else if(kx.eq.1 .and. ky.eq.0 .and. kz.eq.1) then
            call cal_moments_from_matrix(j, filter_zx_nod(inod,id_f))
          else if(kx.eq.2 .and. ky.eq.0 .and. kz.eq.0) then
            call cal_moments_from_matrix(j, filter_x2_nod(inod,id_f))
          else if(kx.eq.0 .and. ky.eq.2 .and. kz.eq.0) then
            call cal_moments_from_matrix(j, filter_y2_nod(inod,id_f))
          else if(kx.eq.0 .and. ky.eq.0 .and. kz.eq.2) then
            call cal_moments_from_matrix(j, filter_z2_nod(inod,id_f))
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
      subroutine copy_filter_moments_each_nod(id_dest, id_org, inod)
!
      use m_filter_moments
!
      integer(kind = kint), intent(in) :: id_dest, id_org, inod
!
      filter_x_nod(inod,id_dest) =  filter_x_nod(inod,id_org)
      filter_y_nod(inod,id_dest) =  filter_y_nod(inod,id_org)
      filter_z_nod(inod,id_dest) =  filter_z_nod(inod,id_org)
      filter_xy_nod(inod,id_dest) = filter_xy_nod(inod,id_org)
      filter_yz_nod(inod,id_dest) = filter_yz_nod(inod,id_org)
      filter_zx_nod(inod,id_dest) = filter_zx_nod(inod,id_org)
      filter_x2_nod(inod,id_dest) = filter_x2_nod(inod,id_org)
      filter_y2_nod(inod,id_dest) = filter_y2_nod(inod,id_org)
      filter_z2_nod(inod,id_dest) = filter_z2_nod(inod,id_org)
!
      end subroutine copy_filter_moments_each_nod
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
