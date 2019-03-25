!cal_filter_moms_ele_by_elen.f90
!     module cal_filter_moms_ele_by_elen
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine cal_fmoms_ele_by_elen(FEM_elen, mom_ele)
!!      subroutine correct_fmoms_ele_by_elen                            &
!!     &         (ele, whole_area, fluid_area, FEM_elen, mom_ele)
!!        type(element_data), intent(in) :: ele
!!        type(filter_area_flag), intent(in) :: whole_area, fluid_area
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(ele_mom_diffs_type), intent(inout) :: mom_ele
!!      subroutine delete_x_products_of_elen(FEM_elen)
!!        type(gradient_model_data_type), intent(inout) :: FEM_elen
!!
!
      module cal_filter_moms_ele_by_elen
!
      use m_precision
      use m_constants
!
      use t_filter_elength
      use t_filter_moments
!
      implicit none
!
      integer(kind = kint), allocatable :: iflag_make_moment_ele(:)
      integer(kind = kint), allocatable :: iele_make_moment_again(:)
!
      private :: iflag_make_moment_ele, iele_make_moment_again
!
      private :: s_cal_filter_moms_ele_by_elen
      private :: correct_filter_moms_ele_by_elen
      private :: delete_cross_products_of_elen
      private :: allocate_correct_filter_flag
      private :: deallocate_correct_filter_flag
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_correct_filter_flag(ele)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
!
!
      allocate(iflag_make_moment_ele(ele%numele))
      allocate(iele_make_moment_again(ele%numele))
!
      if(ele%numele .gt. 0) then
        iflag_make_moment_ele =  0
        iele_make_moment_again = 0
      end if
!
      end subroutine allocate_correct_filter_flag
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_correct_filter_flag
!
      deallocate(iflag_make_moment_ele)
      deallocate(iele_make_moment_again)
!
      end subroutine deallocate_correct_filter_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_fmoms_ele_by_elen(FEM_elen, mom_ele)
!
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
!
      call s_cal_filter_moms_ele_by_elen(FEM_elen%nele_filter_mom,      &
     &    FEM_elen%filter_conf%nf_type,                                 &
     &    FEM_elen%filter_conf%xmom_1d_org, FEM_elen%elen_ele%moms,     &
     &    FEM_elen%elen_ele%diff, FEM_elen%elen_ele%diff2,              &
     &    mom_ele%moms, mom_ele%diff, mom_ele%diff2)
!
      end subroutine cal_fmoms_ele_by_elen
!
!  ---------------------------------------------------------------------
!
      subroutine correct_fmoms_ele_by_elen                              &
     &         (ele, whole_area, fluid_area, FEM_elen, mom_ele)
!
      use t_geometry_data
      use t_filter_coefs
!
      type(element_data), intent(in) :: ele
      type(filter_area_flag), intent(in) :: whole_area, fluid_area
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
!
      call allocate_correct_filter_flag(ele)
      call correct_filter_moms_ele_by_elen                              &
     & (ele, whole_area, fluid_area, FEM_elen%filter_conf%nf_type,      &
     &  FEM_elen%filter_conf%xmom_1d_org, FEM_elen%elen_ele%moms,       &
     &  FEM_elen%elen_ele%diff, FEM_elen%elen_ele%diff2,                &
     &  mom_ele%moms, mom_ele%diff, mom_ele%diff2)
      call deallocate_correct_filter_flag
!
      end subroutine correct_fmoms_ele_by_elen
!
!  ---------------------------------------------------------------------
!
      subroutine delete_x_products_of_elen(FEM_elen)
!
      type(gradient_model_data_type), intent(inout) :: FEM_elen
!
!
      call delete_cross_products_of_elen(FEM_elen%nele_filter_mom,      &
     &    FEM_elen%elen_ele%moms,   FEM_elen%elen_ele%diff,             &
     &    FEM_elen%elen_ele%diff2)
!
      end subroutine delete_x_products_of_elen
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_cal_filter_moms_ele_by_elen                          &
     &         (numele, nf_type, xmom_1d_org,                           &
     &          elen_moms, elen_diff, elen_diff2,                       &
     &          moments_e, mom_e_diff, mom_e_diff2)
!
      integer (kind = kint), intent(in) :: numele
!
      integer (kind = kint), intent(in) :: nf_type
      real(kind=kreal), intent(in) :: xmom_1d_org(nf_type,0:2)
!
      type(elen_on_ele_type), intent(in) :: elen_moms
      type(elen_diffs_type), intent(in) :: elen_diff
      type(elen_diffs_type), intent(in) :: elen_diff2
!
      type(filter_mom_type), intent(inout) :: moments_e
      type(filter_mom_diffs_type), intent(inout) :: mom_e_diff
      type(filter_mom_diffs_type), intent(inout) :: mom_e_diff2
!
      integer(kind = kint) :: iele, nd
!
!$omp parallel do
      do iele = 1, numele
        moments_e%f_x(iele) = zero
        moments_e%f_y(iele) = zero
        moments_e%f_z(iele) = zero
        moments_e%f_x2(iele) = xmom_1d_org(1,2) * elen_moms%f_x2(iele)
        moments_e%f_y2(iele) = xmom_1d_org(1,2) * elen_moms%f_y2(iele)
        moments_e%f_z2(iele) = xmom_1d_org(1,2) * elen_moms%f_z2(iele)
        moments_e%f_xy(iele) = xmom_1d_org(1,2) * elen_moms%f_xy(iele)
        moments_e%f_yz(iele) = xmom_1d_org(1,2) * elen_moms%f_yz(iele)
        moments_e%f_zx(iele) = xmom_1d_org(1,2) * elen_moms%f_zx(iele)
      end do
!$omp end parallel do
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(iele)
        do iele = 1, numele
          mom_e_diff%df_x(iele,nd) = zero
          mom_e_diff%df_y(iele,nd) = zero
          mom_e_diff%df_z(iele,nd) = zero
          mom_e_diff%df_x2(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_x2(iele,nd)
          mom_e_diff%df_y2(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_y2(iele,nd)
          mom_e_diff%df_z2(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_z2(iele,nd)
          mom_e_diff%df_xy(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_xy(iele,nd)
          mom_e_diff%df_yz(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_yz(iele,nd)
          mom_e_diff%df_zx(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_zx(iele,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(iele)
        do iele = 1, numele
          mom_e_diff2%df_x(iele,nd) = zero
          mom_e_diff2%df_y(iele,nd) = zero
          mom_e_diff2%df_z(iele,nd) = zero
          mom_e_diff2%df_x2(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_x2(iele,nd)
          mom_e_diff2%df_y2(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_y2(iele,nd)
          mom_e_diff2%df_z2(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_z2(iele,nd)
          mom_e_diff2%df_xy(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_xy(iele,nd)
          mom_e_diff2%df_yz(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_yz(iele,nd)
          mom_e_diff2%df_zx(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_zx(iele,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine s_cal_filter_moms_ele_by_elen
!
!  ---------------------------------------------------------------------
!
      subroutine correct_filter_moms_ele_by_elen                        &
     &         (ele, whole_area, fluid_area, nf_type, xmom_1d_org,      &
     &          elen_moms, elen_diff, elen_diff2,                       &
     &          moments_e, mom_e_diff, mom_e_diff2)
!
      use t_geometry_data
      use t_filter_coefs
!
      type(element_data), intent(in) :: ele
      type(filter_area_flag), intent(in) :: whole_area, fluid_area
!
      integer (kind = kint), intent(in) :: nf_type
      real(kind=kreal), intent(in) :: xmom_1d_org(nf_type,0:2)
!
      type(elen_on_ele_type), intent(in) :: elen_moms
      type(elen_diffs_type), intent(in) :: elen_diff
      type(elen_diffs_type), intent(in) :: elen_diff2
!
      type(filter_mom_type), intent(inout) :: moments_e
      type(filter_mom_diffs_type), intent(inout) :: mom_e_diff
      type(filter_mom_diffs_type), intent(inout) :: mom_e_diff2
!
      integer(kind = kint) :: iele, nd, inum, inod, k1
!
      integer(kind = kint) :: nele_make_moment_again
!
!
      do iele = 1, ele%numele
        do k1 = 1, ele%nnod_4_ele
          inod = ele%ie(iele,k1)
          if(whole_area%iflag_make_filter(inod) .eq. 1) then
            iflag_make_moment_ele(iele) = 1
            exit
          end if
          if(fluid_area%iflag_make_filter(inod) .eq. 1) then
            iflag_make_moment_ele(iele) = 1
            exit
          end if
        end do
      end do
!
      nele_make_moment_again = 0
      do iele = 1, ele%numele
        if ( fluid_area%iflag_make_filter(iele) .eq. 0) then
          nele_make_moment_again = nele_make_moment_again + 1
          iele_make_moment_again(nele_make_moment_again) = iele
        end if
      end do
!
!
!$omp parallel do private(inum,iele)
      do inum = 1, nele_make_moment_again
        iele = iele_make_moment_again(inum)
!
        moments_e%f_x(iele) = zero
        moments_e%f_y(iele) = zero
        moments_e%f_z(iele) = zero
        moments_e%f_x2(iele) = xmom_1d_org(1,2) * elen_moms%f_x2(iele)
        moments_e%f_y2(iele) = xmom_1d_org(1,2) * elen_moms%f_y2(iele)
        moments_e%f_z2(iele) = xmom_1d_org(1,2) * elen_moms%f_z2(iele)
        moments_e%f_xy(iele) = xmom_1d_org(1,2) * elen_moms%f_xy(iele)
        moments_e%f_yz(iele) = xmom_1d_org(1,2) * elen_moms%f_yz(iele)
        moments_e%f_zx(iele) = xmom_1d_org(1,2) * elen_moms%f_zx(iele)
      end do
!$omp end parallel do
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(inum,iele)
        do inum = 1, nele_make_moment_again
          iele = iele_make_moment_again(inum)
!
          mom_e_diff%df_x(iele,nd) = zero
          mom_e_diff%df_y(iele,nd) = zero
          mom_e_diff%df_z(iele,nd) = zero
          mom_e_diff%df_x2(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_x2(iele,nd)
          mom_e_diff%df_y2(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_y2(iele,nd)
          mom_e_diff%df_z2(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_z2(iele,nd)
          mom_e_diff%df_xy(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_xy(iele,nd)
          mom_e_diff%df_yz(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_yz(iele,nd)
          mom_e_diff%df_zx(iele,nd)                                     &
     &        = xmom_1d_org(1,2) * elen_diff%df_zx(iele,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(inum,iele)
        do inum = 1, nele_make_moment_again
          iele = iele_make_moment_again(inum)
!
          mom_e_diff2%df_x(iele,nd) = zero
          mom_e_diff2%df_y(iele,nd) = zero
          mom_e_diff2%df_z(iele,nd) = zero
          mom_e_diff2%df_x2(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_x2(iele,nd)
          mom_e_diff2%df_y2(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_y2(iele,nd)
          mom_e_diff2%df_z2(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_z2(iele,nd)
          mom_e_diff2%df_xy(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_xy(iele,nd)
          mom_e_diff2%df_yz(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_yz(iele,nd)
          mom_e_diff2%df_zx(iele,nd)                                    &
     &        = xmom_1d_org(1,2) * elen_diff2%df_zx(iele,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine correct_filter_moms_ele_by_elen
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine delete_cross_products_of_elen(numele,                  &
     &          elen_moms, elen_diff, elen_diff2)
!
      integer(kind = kint), intent(in) :: numele
      type(elen_on_ele_type), intent(inout) :: elen_moms
      type(elen_diffs_type), intent(inout) :: elen_diff
      type(elen_diffs_type), intent(inout) :: elen_diff2
!
      integer(kind = kint) :: iele, nd
!
!$omp parallel do
      do iele = 1, numele
        elen_moms%f_xy(iele) = zero
        elen_moms%f_yz(iele) = zero
        elen_moms%f_zx(iele) = zero
      end do
!$omp end parallel do
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(iele)
        do iele = 1, numele
          elen_diff%df_xy(iele,nd) = zero
          elen_diff%df_yz(iele,nd) = zero
          elen_diff%df_zx(iele,nd) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(iele)
        do iele = 1, numele
          elen_diff2%df_xy(iele,nd) = zero
          elen_diff2%df_yz(iele,nd) = zero
          elen_diff2%df_zx(iele,nd) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine delete_cross_products_of_elen
!
!  ---------------------------------------------------------------------
!
      end module cal_filter_moms_ele_by_elen
