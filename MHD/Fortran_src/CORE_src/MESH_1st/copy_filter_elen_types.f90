!
!      module copy_filter_elen_types
!
!     Written by H. Matsui
!
!      subroutine copy_filter_elen_ele_from_type(elen_e)
!      subroutine copy_filter_elen_ele_to_type(elen_e)
!        type(ele_mom_diffs_type), intent(in)  :: elen_e(num_filter_moms)
!
      module copy_filter_elen_types
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
      subroutine copy_filter_elen_ele_from_type(elen_e)
!
      use t_filter_elength
      use m_filter_elength
!
      type(ele_mom_diffs_type), intent(in)  :: elen_e
!
      integer (kind=kint) :: nd, i
!
!
!$omp parallel private(nd)
!$omp do
        do i = 1, nele_filter_mom
          elen_dx2_ele(i) = elen_e%moms%f_x2(i)
          elen_dy2_ele(i) = elen_e%moms%f_y2(i)
          elen_dz2_ele(i) = elen_e%moms%f_z2(i)
          elen_dxdy_ele(i) = elen_e%moms%f_xy(i)
          elen_dydz_ele(i) = elen_e%moms%f_yz(i)
          elen_dzdx_ele(i) = elen_e%moms%f_zx(i)
        end do
!$omp end do nowait
!
        do nd = 1, 3
!$omp do
          do i = 1, nele_filter_mom
            elen_dx2_ele_dx(i,nd) = elen_e%diff%df_x2(i,nd)
            elen_dy2_ele_dx(i,nd) = elen_e%diff%df_y2(i,nd)
            elen_dz2_ele_dx(i,nd) = elen_e%diff%df_z2(i,nd)
            elen_dxdy_ele_dx(i,nd) = elen_e%diff%df_xy(i,nd)
            elen_dydz_ele_dx(i,nd) = elen_e%diff%df_yz(i,nd)
            elen_dzdx_ele_dx(i,nd) = elen_e%diff%df_zx(i,nd)
!
            elen_dx2_ele_dx2(i,nd) = elen_e%diff2%df_x2(i,nd)
            elen_dy2_ele_dx2(i,nd) = elen_e%diff2%df_y2(i,nd)
            elen_dz2_ele_dx2(i,nd) = elen_e%diff2%df_z2(i,nd)
            elen_dxdy_ele_dx2(i,nd) = elen_e%diff2%df_xy(i,nd)
            elen_dydz_ele_dx2(i,nd) = elen_e%diff2%df_yz(i,nd)
            elen_dzdx_ele_dx2(i,nd) = elen_e%diff2%df_zx(i,nd)
          end do
!$omp end do nowait
        end do
!$omp end parallel
!
      end subroutine copy_filter_elen_ele_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_elen_ele_to_type(elen_e)
!
      use t_filter_elength
      use m_filter_elength
!
      type(ele_mom_diffs_type), intent(inout) :: elen_e
!
      integer (kind=kint) :: nd, i
!
!
!$omp parallel private(nd)
!$omp do
        do i = 1, nele_filter_mom
          elen_e%moms%f_x2(i) = elen_dx2_ele(i)
          elen_e%moms%f_y2(i) = elen_dy2_ele(i)
          elen_e%moms%f_z2(i) = elen_dz2_ele(i)
          elen_e%moms%f_xy(i) = elen_dxdy_ele(i)
          elen_e%moms%f_yz(i) = elen_dydz_ele(i)
          elen_e%moms%f_zx(i) = elen_dzdx_ele(i)
        end do
!$omp end do nowait
!
        do nd = 1, 3
!$omp do
          do i = 1, nele_filter_mom
            elen_e%diff%df_x2(i,nd) = elen_dx2_ele_dx(i,nd)
            elen_e%diff%df_y2(i,nd) = elen_dy2_ele_dx(i,nd)
            elen_e%diff%df_z2(i,nd) = elen_dz2_ele_dx(i,nd)
            elen_e%diff%df_xy(i,nd) = elen_dxdy_ele_dx(i,nd)
            elen_e%diff%df_yz(i,nd) = elen_dydz_ele_dx(i,nd)
            elen_e%diff%df_zx(i,nd) = elen_dzdx_ele_dx(i,nd)
!
            elen_e%diff2%df_x2(i,nd) = elen_dx2_ele_dx2(i,nd)
            elen_e%diff2%df_y2(i,nd) = elen_dy2_ele_dx2(i,nd)
            elen_e%diff2%df_z2(i,nd) = elen_dz2_ele_dx2(i,nd)
            elen_e%diff2%df_xy(i,nd) = elen_dxdy_ele_dx2(i,nd)
            elen_e%diff2%df_yz(i,nd) = elen_dydz_ele_dx2(i,nd)
            elen_e%diff2%df_zx(i,nd) = elen_dzdx_ele_dx2(i,nd)
          end do
!$omp end do nowait
        end do
!$omp end parallel
!
      end subroutine copy_filter_elen_ele_to_type
!
!  ---------------------------------------------------------------------
!
      end module copy_filter_elen_types
