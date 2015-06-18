!
!      module copy_filter_moments_4_type
!
!     Written by H. Matsui
!
!      subroutine copy_filter_moms_ele_from_type(mom_e)
!      subroutine copy_filter_moms_ele_to_type(mom_e)
!       type(ele_mom_diffs_type), intent(in)  :: mom_e(num_filter_moms)
!
!      subroutine copy_filter_moms_nod_from_type(mom_n)
!      subroutine copy_filter_moms_nod_to_type(mom_n)
!       type(nod_mom_diffs_type), intent(inout) :: mom_n(num_filter_moms)
!
      module copy_filter_moments_4_type
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_moms_ele_from_type(mom_e)
!
      use t_filter_moments
      use m_filter_moments
!
      type(ele_mom_diffs_type), intent(in)  :: mom_e(num_filter_moms)
!
      integer (kind=kint) :: nd, i, ifl
!
!
!$omp parallel private(ifl,nd)
      do ifl = 1, num_filter_moms
!$omp do
        do i = 1, mom1%nele_fmom
          filter_x_ele(i,ifl) =  mom_e(ifl)%moms%f_x(i)
          filter_y_ele(i,ifl) =  mom_e(ifl)%moms%f_y(i)
          filter_z_ele(i,ifl) =  mom_e(ifl)%moms%f_z(i)
          filter_x2_ele(i,ifl) = mom_e(ifl)%moms%f_x2(i)
          filter_y2_ele(i,ifl) = mom_e(ifl)%moms%f_y2(i)
          filter_z2_ele(i,ifl) = mom_e(ifl)%moms%f_z2(i)
          filter_xy_ele(i,ifl) = mom_e(ifl)%moms%f_xy(i)
          filter_yz_ele(i,ifl) = mom_e(ifl)%moms%f_yz(i)
          filter_zx_ele(i,ifl) = mom_e(ifl)%moms%f_zx(i)
        end do
!$omp end do nowait
!
        do nd = 1, 3
!$omp do
          do i = 1, mom1%nele_fmom
            filter_x_ele_dx(i,nd,ifl) =  mom_e(ifl)%diff%df_x(i,nd)
            filter_y_ele_dx(i,nd,ifl) =  mom_e(ifl)%diff%df_y(i,nd)
            filter_z_ele_dx(i,nd,ifl) =  mom_e(ifl)%diff%df_z(i,nd)
            filter_x2_ele_dx(i,nd,ifl) = mom_e(ifl)%diff%df_x2(i,nd)
            filter_y2_ele_dx(i,nd,ifl) = mom_e(ifl)%diff%df_y2(i,nd)
            filter_z2_ele_dx(i,nd,ifl) = mom_e(ifl)%diff%df_z2(i,nd)
            filter_xy_ele_dx(i,nd,ifl) = mom_e(ifl)%diff%df_xy(i,nd)
            filter_yz_ele_dx(i,nd,ifl) = mom_e(ifl)%diff%df_yz(i,nd)
            filter_zx_ele_dx(i,nd,ifl) = mom_e(ifl)%diff%df_zx(i,nd)
!
            filter_x_ele_dx2(i,nd,ifl) =  mom_e(ifl)%diff2%df_x(i,nd)
            filter_y_ele_dx2(i,nd,ifl) =  mom_e(ifl)%diff2%df_y(i,nd)
            filter_z_ele_dx2(i,nd,ifl) =  mom_e(ifl)%diff2%df_z(i,nd)
            filter_x2_ele_dx2(i,nd,ifl) = mom_e(ifl)%diff2%df_x2(i,nd)
            filter_y2_ele_dx2(i,nd,ifl) = mom_e(ifl)%diff2%df_y2(i,nd)
            filter_z2_ele_dx2(i,nd,ifl) = mom_e(ifl)%diff2%df_z2(i,nd)
            filter_xy_ele_dx2(i,nd,ifl) = mom_e(ifl)%diff2%df_xy(i,nd)
            filter_yz_ele_dx2(i,nd,ifl) = mom_e(ifl)%diff2%df_yz(i,nd)
            filter_zx_ele_dx2(i,nd,ifl) = mom_e(ifl)%diff2%df_zx(i,nd)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine copy_filter_moms_ele_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_moms_ele_to_type(mom_e)
!
      use t_filter_moments
      use m_filter_moments
!
      type(ele_mom_diffs_type), intent(inout) :: mom_e(num_filter_moms)
!
      integer (kind=kint) :: nd, i, ifl
!
!
!$omp parallel private(ifl,nd)
      do ifl = 1, num_filter_moms
!$omp do
        do i = 1, mom1%nele_fmom
          mom_e(ifl)%moms%f_x(i) =  filter_x_ele(i,ifl)
          mom_e(ifl)%moms%f_y(i) =  filter_y_ele(i,ifl)
          mom_e(ifl)%moms%f_z(i) =  filter_z_ele(i,ifl)
          mom_e(ifl)%moms%f_x2(i) = filter_x2_ele(i,ifl)
          mom_e(ifl)%moms%f_y2(i) = filter_y2_ele(i,ifl)
          mom_e(ifl)%moms%f_z2(i) = filter_z2_ele(i,ifl)
          mom_e(ifl)%moms%f_xy(i) = filter_xy_ele(i,ifl)
          mom_e(ifl)%moms%f_yz(i) = filter_yz_ele(i,ifl)
          mom_e(ifl)%moms%f_zx(i) = filter_zx_ele(i,ifl)
        end do
!$omp end do nowait
!
        do nd = 1, 3
!$omp do
          do i = 1, mom1%nele_fmom
            mom_e(ifl)%diff%df_x(i,nd) =  filter_x_ele_dx(i,nd,ifl)
            mom_e(ifl)%diff%df_y(i,nd) =  filter_y_ele_dx(i,nd,ifl)
            mom_e(ifl)%diff%df_z(i,nd) =  filter_z_ele_dx(i,nd,ifl)
            mom_e(ifl)%diff%df_x2(i,nd) = filter_x2_ele_dx(i,nd,ifl)
            mom_e(ifl)%diff%df_y2(i,nd) = filter_y2_ele_dx(i,nd,ifl)
            mom_e(ifl)%diff%df_z2(i,nd) = filter_z2_ele_dx(i,nd,ifl)
            mom_e(ifl)%diff%df_xy(i,nd) = filter_xy_ele_dx(i,nd,ifl)
            mom_e(ifl)%diff%df_yz(i,nd) = filter_yz_ele_dx(i,nd,ifl)
            mom_e(ifl)%diff%df_zx(i,nd) = filter_zx_ele_dx(i,nd,ifl)
!
            mom_e(ifl)%diff2%df_x(i,nd) =  filter_x_ele_dx2(i,nd,ifl)
            mom_e(ifl)%diff2%df_y(i,nd) =  filter_y_ele_dx2(i,nd,ifl)
            mom_e(ifl)%diff2%df_z(i,nd) =  filter_z_ele_dx2(i,nd,ifl)
            mom_e(ifl)%diff2%df_x2(i,nd) = filter_x2_ele_dx2(i,nd,ifl)
            mom_e(ifl)%diff2%df_y2(i,nd) = filter_y2_ele_dx2(i,nd,ifl)
            mom_e(ifl)%diff2%df_z2(i,nd) = filter_z2_ele_dx2(i,nd,ifl)
            mom_e(ifl)%diff2%df_xy(i,nd) = filter_xy_ele_dx2(i,nd,ifl)
            mom_e(ifl)%diff2%df_yz(i,nd) = filter_yz_ele_dx2(i,nd,ifl)
            mom_e(ifl)%diff2%df_zx(i,nd) = filter_zx_ele_dx2(i,nd,ifl)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine copy_filter_moms_ele_to_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_moms_nod_from_type(mom_n)
!
      use t_filter_moments
      use m_filter_moments
!
      type(nod_mom_diffs_type), intent(in)  :: mom_n(num_filter_moms)
!
      integer (kind=kint) :: nd, i, ifl
!
!
!$omp parallel private(ifl,nd)
      do ifl = 1, num_filter_moms
!$omp do
        do i = 1, mom1%nnod_fmom
          filter_x_nod(i,ifl) =  mom_n(ifl)%moms%f_x(i)
          filter_y_nod(i,ifl) =  mom_n(ifl)%moms%f_y(i)
          filter_z_nod(i,ifl) =  mom_n(ifl)%moms%f_z(i)
          filter_x2_nod(i,ifl) = mom_n(ifl)%moms%f_x2(i)
          filter_y2_nod(i,ifl) = mom_n(ifl)%moms%f_y2(i)
          filter_z2_nod(i,ifl) = mom_n(ifl)%moms%f_z2(i)
          filter_xy_nod(i,ifl) = mom_n(ifl)%moms%f_xy(i)
          filter_yz_nod(i,ifl) = mom_n(ifl)%moms%f_yz(i)
          filter_zx_nod(i,ifl) = mom_n(ifl)%moms%f_zx(i)
        end do
!$omp end do nowait
!
        do nd = 1, 3
!$omp do
          do i = 1, mom1%nnod_fmom
            filter_x_nod_dx(i,nd,ifl) =  mom_n(ifl)%diff%df_x(i,nd)
            filter_y_nod_dx(i,nd,ifl) =  mom_n(ifl)%diff%df_y(i,nd)
            filter_z_nod_dx(i,nd,ifl) =  mom_n(ifl)%diff%df_z(i,nd)
            filter_x2_nod_dx(i,nd,ifl) = mom_n(ifl)%diff%df_x2(i,nd)
            filter_y2_nod_dx(i,nd,ifl) = mom_n(ifl)%diff%df_y2(i,nd)
            filter_z2_nod_dx(i,nd,ifl) = mom_n(ifl)%diff%df_z2(i,nd)
            filter_xy_nod_dx(i,nd,ifl) = mom_n(ifl)%diff%df_xy(i,nd)
            filter_yz_nod_dx(i,nd,ifl) = mom_n(ifl)%diff%df_yz(i,nd)
            filter_zx_nod_dx(i,nd,ifl) = mom_n(ifl)%diff%df_zx(i,nd)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine copy_filter_moms_nod_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_moms_nod_to_type(mom_n)
!
      use t_filter_moments
      use m_filter_moments
!
      type(nod_mom_diffs_type), intent(inout) :: mom_n(num_filter_moms)
!
      integer (kind=kint) :: nd, i, ifl
!
!
!$omp parallel private(ifl,nd)
      do ifl = 1, num_filter_moms
!$omp do
        do i = 1, mom1%nnod_fmom
          mom_n(ifl)%moms%f_x(i) =  filter_x_nod(i,ifl)
          mom_n(ifl)%moms%f_y(i) =  filter_y_nod(i,ifl)
          mom_n(ifl)%moms%f_z(i) =  filter_z_nod(i,ifl)
          mom_n(ifl)%moms%f_x2(i) = filter_x2_nod(i,ifl)
          mom_n(ifl)%moms%f_y2(i) = filter_y2_nod(i,ifl)
          mom_n(ifl)%moms%f_z2(i) = filter_z2_nod(i,ifl)
          mom_n(ifl)%moms%f_xy(i) = filter_xy_nod(i,ifl)
          mom_n(ifl)%moms%f_yz(i) = filter_yz_nod(i,ifl)
          mom_n(ifl)%moms%f_zx(i) = filter_zx_nod(i,ifl)
        end do
!$omp end do nowait
!
        do nd = 1, 3
!$omp do
          do i = 1, mom1%nnod_fmom
            mom_n(ifl)%diff%df_x(i,nd) =  filter_x_nod_dx(i,nd,ifl)
            mom_n(ifl)%diff%df_y(i,nd) =  filter_y_nod_dx(i,nd,ifl)
            mom_n(ifl)%diff%df_z(i,nd) =  filter_z_nod_dx(i,nd,ifl)
            mom_n(ifl)%diff%df_x2(i,nd) = filter_x2_nod_dx(i,nd,ifl)
            mom_n(ifl)%diff%df_y2(i,nd) = filter_y2_nod_dx(i,nd,ifl)
            mom_n(ifl)%diff%df_z2(i,nd) = filter_z2_nod_dx(i,nd,ifl)
            mom_n(ifl)%diff%df_xy(i,nd) = filter_xy_nod_dx(i,nd,ifl)
            mom_n(ifl)%diff%df_yz(i,nd) = filter_yz_nod_dx(i,nd,ifl)
            mom_n(ifl)%diff%df_zx(i,nd) = filter_zx_nod_dx(i,nd,ifl)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine copy_filter_moms_nod_to_type
!
!  ---------------------------------------------------------------------
!
      end module copy_filter_moments_4_type
