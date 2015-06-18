!
!     module cal_diff_elesize_on_ele
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on Mar., 2008
!
!      subroutine cal_diffs_delta_on_element
!      subroutine cal_2nd_diffs_delta_on_element
!
!      subroutine cal_filter_moms_ele_by_nod(ifil)
!      subroutine cal_1st_diffs_filter_ele(ifil)
!      subroutine cal_2nd_diffs_filter_ele(ifil)
!
      module cal_diff_elesize_on_ele
!
      use m_precision
!
      implicit none
!
      integer(kind=kint), parameter :: n_vector = 3
      private :: n_vector
      private :: take_1st_diffs_ele, take_2nd_diffs_ele
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_filter_moms_ele_by_nod(ifil)
!
      use m_ctl_params_4_gen_filter
      use m_geometry_parameter
      use m_filter_moments
      use cal_fields_on_element
!
      integer(kind = kint), intent(in) :: ifil
!
      call scalar_on_element(iele_smp_stack, num_int_points,            &
     &    mom1%mom_ele(ifil)%moms%f_x, mom1%mom_nod(ifil)%moms%f_x)
      call scalar_on_element(iele_smp_stack, num_int_points,            &
     &    mom1%mom_ele(ifil)%moms%f_y, mom1%mom_nod(ifil)%moms%f_y)
      call scalar_on_element(iele_smp_stack, num_int_points,            &
     &    mom1%mom_ele(ifil)%moms%f_z, mom1%mom_nod(ifil)%moms%f_z)
!
      call scalar_on_element(iele_smp_stack, num_int_points,            &
     &    mom1%mom_ele(ifil)%moms%f_x2, mom1%mom_nod(ifil)%moms%f_x2)
      call scalar_on_element(iele_smp_stack, num_int_points,            &
     &    mom1%mom_ele(ifil)%moms%f_y2, mom1%mom_nod(ifil)%moms%f_y2)
      call scalar_on_element(iele_smp_stack, num_int_points,            &
     &    mom1%mom_ele(ifil)%moms%f_z2, mom1%mom_nod(ifil)%moms%f_z2)
!
      call scalar_on_element(iele_smp_stack, num_int_points,            &
     &    mom1%mom_ele(ifil)%moms%f_xy, mom1%mom_nod(ifil)%moms%f_xy)
      call scalar_on_element(iele_smp_stack, num_int_points,            &
     &    mom1%mom_ele(ifil)%moms%f_yz, mom1%mom_nod(ifil)%moms%f_yz)
      call scalar_on_element(iele_smp_stack, num_int_points,            &
     &    mom1%mom_ele(ifil)%moms%f_zx, mom1%mom_nod(ifil)%moms%f_zx)
!
      end subroutine cal_filter_moms_ele_by_nod
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_diffs_delta_on_element
!
      use m_filter_elength
!
      call take_1st_diffs_ele                                           &
     &   (FEM1_elen%elen_nod%moms%f_x2, FEM1_elen%elen_ele%diff%df_x2)
      call take_1st_diffs_ele                                           &
     &   (FEM1_elen%elen_nod%moms%f_y2, FEM1_elen%elen_ele%diff%df_y2)
      call take_1st_diffs_ele                                           &
     &   (FEM1_elen%elen_nod%moms%f_z2, FEM1_elen%elen_ele%diff%df_z2)
!
      call take_1st_diffs_ele                                           &
     &   (FEM1_elen%elen_nod%moms%f_xy, FEM1_elen%elen_ele%diff%df_xy)
      call take_1st_diffs_ele                                           &
     &   (FEM1_elen%elen_nod%moms%f_yz, FEM1_elen%elen_ele%diff%df_yz)
      call take_1st_diffs_ele                                           &
     &   (FEM1_elen%elen_nod%moms%f_zx, FEM1_elen%elen_ele%diff%df_zx)
!
      end subroutine cal_diffs_delta_on_element
!
!-----------------------------------------------------------------------
!
      subroutine cal_1st_diffs_filter_ele(ifil)
!
      use m_filter_moments
!
      integer(kind = kint), intent(in) :: ifil
!
      call take_1st_diffs_ele(mom1%mom_nod(ifil)%moms%f_x,              &
     &                        mom1%mom_ele(ifil)%diff%df_x)
      call take_1st_diffs_ele(mom1%mom_nod(ifil)%moms%f_y,              &
     &                        mom1%mom_ele(ifil)%diff%df_y)
      call take_1st_diffs_ele(mom1%mom_nod(ifil)%moms%f_z,              &
     &                        mom1%mom_ele(ifil)%diff%df_z)
!
      call take_1st_diffs_ele(mom1%mom_nod(ifil)%moms%f_x2,             &
     &                        mom1%mom_ele(ifil)%diff%df_x2)
      call take_1st_diffs_ele(mom1%mom_nod(ifil)%moms%f_y2,             &
     &                        mom1%mom_ele(ifil)%diff%df_y2)
      call take_1st_diffs_ele(mom1%mom_nod(ifil)%moms%f_z2,             &
     &                        mom1%mom_ele(ifil)%diff%df_z2)
!
      call take_1st_diffs_ele(mom1%mom_nod(ifil)%moms%f_xy,             &
     &                        mom1%mom_ele(ifil)%diff%df_xy)
      call take_1st_diffs_ele(mom1%mom_nod(ifil)%moms%f_yz,             &
     &                        mom1%mom_ele(ifil)%diff%df_yz)
      call take_1st_diffs_ele(mom1%mom_nod(ifil)%moms%f_zx,             &
     &                        mom1%mom_ele(ifil)%diff%df_zx)
!
      end subroutine cal_1st_diffs_filter_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_2nd_diffs_delta_on_element
!
      use m_filter_elength
!
      call take_2nd_diffs_ele(FEM1_elen%elen_nod%diff%df_x2,            &
     &                        FEM1_elen%elen_ele%diff2%df_x2)
      call take_2nd_diffs_ele(FEM1_elen%elen_nod%diff%df_y2,            &
     &                        FEM1_elen%elen_ele%diff2%df_y2)
      call take_2nd_diffs_ele(FEM1_elen%elen_nod%diff%df_z2,            &
     &                        FEM1_elen%elen_ele%diff2%df_z2)
!
      call take_2nd_diffs_ele(FEM1_elen%elen_nod%diff%df_xy,            &
     &                        FEM1_elen%elen_ele%diff2%df_xy)
      call take_2nd_diffs_ele(FEM1_elen%elen_nod%diff%df_yz,            &
     &                        FEM1_elen%elen_ele%diff2%df_yz)
      call take_2nd_diffs_ele(FEM1_elen%elen_nod%diff%df_zx,            &
     &                        FEM1_elen%elen_ele%diff2%df_zx)
!
      end subroutine cal_2nd_diffs_delta_on_element
!
!-----------------------------------------------------------------------
!
      subroutine cal_2nd_diffs_filter_ele(ifil)
!
      use m_filter_moments
!
      integer(kind = kint), intent(in) :: ifil
!
      call take_2nd_diffs_ele(mom1%mom_nod(ifil)%diff%df_x,             &
     &                        mom1%mom_ele(ifil)%diff2%df_x)
      call take_2nd_diffs_ele(mom1%mom_nod(ifil)%diff%df_y,             &
     &                        mom1%mom_ele(ifil)%diff2%df_y)
      call take_2nd_diffs_ele(mom1%mom_nod(ifil)%diff%df_z,             &
     &                        mom1%mom_ele(ifil)%diff2%df_z)
!
      call take_2nd_diffs_ele(mom1%mom_nod(ifil)%diff%df_x2,            &
     &                        mom1%mom_ele(ifil)%diff2%df_x2)
      call take_2nd_diffs_ele(mom1%mom_nod(ifil)%diff%df_y2,            &
     &                        mom1%mom_ele(ifil)%diff2%df_y2)
      call take_2nd_diffs_ele(mom1%mom_nod(ifil)%diff%df_z2,            &
     &                        mom1%mom_ele(ifil)%diff2%df_z2)
!
      call take_2nd_diffs_ele(mom1%mom_nod(ifil)%diff%df_xy,            &
     &                        mom1%mom_ele(ifil)%diff2%df_xy)
      call take_2nd_diffs_ele(mom1%mom_nod(ifil)%diff%df_yz,            &
     &                        mom1%mom_ele(ifil)%diff2%df_yz)
      call take_2nd_diffs_ele(mom1%mom_nod(ifil)%diff%df_zx,            &
     &                        mom1%mom_ele(ifil)%diff2%df_zx)
!
      end subroutine cal_2nd_diffs_filter_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine take_1st_diffs_ele(org_nod_field, diff_field)
!
      use m_geometry_parameter
      use m_ctl_params_4_gen_filter
      use cal_differences_on_ele
!
      real(kind = kreal), intent(in) :: org_nod_field(numnod)
      real(kind = kreal), intent(inout) :: diff_field(numele,3)
      integer(kind=kint) :: nd
!
      do nd = 1, n_vector
        call difference_on_element(iele_smp_stack, num_int_points,      &
     &      nd, diff_field(1,nd), org_nod_field(1) )
      end do
!
      end subroutine take_1st_diffs_ele
!
!-----------------------------------------------------------------------
!
      subroutine take_2nd_diffs_ele(org_nod_field, diff_field)
!
      use m_geometry_parameter
      use m_ctl_params_4_gen_filter
      use cal_differences_on_ele
!
      real(kind = kreal), intent(in) :: org_nod_field(numnod,3)
      real(kind = kreal), intent(inout) :: diff_field(numele,3)
      integer(kind=kint) :: nd
!
      do nd = 1, n_vector
        call difference_on_element(iele_smp_stack, num_int_points,      &
     &      nd, diff_field(1,nd), org_nod_field(1,nd) )
      end do
!
      end subroutine take_2nd_diffs_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_diff_elesize_on_ele
