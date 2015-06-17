!copy_filter_moment_type.f90
!      module copy_filter_moment_type
!
!     Written by H. Matsui on March, 2009
!     Modified by H. Matsui on Feb., 2012
!
!>    @brief copy Strucure for filter moments
!
!      subroutine copy_elen_diffs_type(num, elen_org, elen_tgt)
!
!      subroutine copy_nodal_elen_type(FEM_elen_org, FEM_elen_tgt)
!        type(gradient_model_data_type), intent(in) :: FEM_elen_org
!        type(gradient_model_data_type), intent(inout) :: FEM_elen_tgt
!      subroutine copy_filter_moms_ele_type(FEM_moms_org, FEM_moms_tgt)
!        type(gradient_filter_mom_type), intent(in) ::    FEM_moms_org
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms_tgt
!
      module copy_filter_moment_type
!
      use m_precision
!
      implicit none
!
      private :: copy_moments_type, copy_mom_diffs_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_moments_type(num, mom_org, mom_tgt)
!
      use t_filter_elength
!
      integer (kind = kint), intent(in) :: num
      type(filter_mom_type), intent(in) :: mom_org
      type(filter_mom_type), intent(inout) :: mom_tgt
!
      integer (kind=kint) :: i
!
!
!$omp parallel do
      do i = 1, num
        mom_tgt%f_0(i) =  mom_org%f_0(i)
        mom_tgt%f_x(i) =  mom_org%f_x(i)
        mom_tgt%f_y(i) =  mom_org%f_y(i)
        mom_tgt%f_z(i) =  mom_org%f_z(i)
        mom_tgt%f_x2(i) = mom_org%f_x2(i)
        mom_tgt%f_y2(i) = mom_org%f_y2(i)
        mom_tgt%f_z2(i) = mom_org%f_z2(i)
        mom_tgt%f_xy(i) = mom_org%f_xy(i)
        mom_tgt%f_yz(i) = mom_org%f_yz(i)
        mom_tgt%f_zx(i) = mom_org%f_zx(i)
      end do
!$omp end parallel do
!
      end subroutine copy_moments_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_elength_type(num, elen_org, elen_tgt)
!
      use t_filter_elength
!
      integer (kind = kint), intent(in) :: num
      type(elen_on_ele_type), intent(in) :: elen_org
      type(elen_on_ele_type), intent(inout) :: elen_tgt
!
      integer (kind=kint) :: i
!
!
!$omp parallel do
      do i = 1, num
        elen_tgt%f_x2(i) = elen_org%f_x2(i)
        elen_tgt%f_y2(i) = elen_org%f_y2(i)
        elen_tgt%f_z2(i) = elen_org%f_z2(i)
        elen_tgt%f_xy(i) = elen_org%f_xy(i)
        elen_tgt%f_yz(i) = elen_org%f_yz(i)
        elen_tgt%f_zx(i) = elen_org%f_zx(i)
      end do
!$omp end parallel do
!
      end subroutine copy_elength_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_mom_diffs_type(num, mom_org, mom_tgt)
!
      use t_filter_elength
!
      integer (kind = kint), intent(in) :: num
      type(filter_mom_diffs_type), intent(in) :: mom_org
      type(filter_mom_diffs_type), intent(inout) :: mom_tgt
!
      integer (kind=kint) :: nd, i
!
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do
        do i = 1, num
          mom_tgt%df_x(i,nd) =  mom_org%df_x(i,nd)
          mom_tgt%df_y(i,nd) =  mom_org%df_y(i,nd)
          mom_tgt%df_z(i,nd) =  mom_org%df_z(i,nd)
          mom_tgt%df_x2(i,nd) = mom_org%df_x2(i,nd)
          mom_tgt%df_y2(i,nd) = mom_org%df_y2(i,nd)
          mom_tgt%df_z2(i,nd) = mom_org%df_z2(i,nd)
          mom_tgt%df_xy(i,nd) = mom_org%df_xy(i,nd)
          mom_tgt%df_yz(i,nd) = mom_org%df_yz(i,nd)
          mom_tgt%df_zx(i,nd) = mom_org%df_zx(i,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_mom_diffs_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_elen_diffs_type(num, elen_org, elen_tgt)
!
      use t_filter_elength
!
      integer (kind = kint), intent(in) :: num
      type(elen_diffs_type), intent(in) :: elen_org
      type(elen_diffs_type), intent(inout) :: elen_tgt
!
      integer (kind=kint) :: nd, i
!
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do
        do i = 1, num
          elen_tgt%df_x2(i,nd) = elen_org%df_x2(i,nd)
          elen_tgt%df_y2(i,nd) = elen_org%df_y2(i,nd)
          elen_tgt%df_z2(i,nd) = elen_org%df_z2(i,nd)
          elen_tgt%df_xy(i,nd) = elen_org%df_xy(i,nd)
          elen_tgt%df_yz(i,nd) = elen_org%df_yz(i,nd)
          elen_tgt%df_zx(i,nd) = elen_org%df_zx(i,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_elen_diffs_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_elen_ele_type(FEM_elen_org, FEM_elen_tgt)
!
      use t_filter_elength
!
      type(gradient_model_data_type), intent(in) :: FEM_elen_org
      type(gradient_model_data_type), intent(inout) :: FEM_elen_tgt
!
!
      FEM_elen_tgt%nele_filter_mom = FEM_elen_org%nele_filter_mom
      call copy_elength_type(FEM_elen_tgt%nele_filter_mom,              &
     &    FEM_elen_org%elen_ele%moms, FEM_elen_tgt%elen_ele%moms)
      call copy_elen_diffs_type(FEM_elen_tgt%nele_filter_mom,           &
     &    FEM_elen_org%elen_ele%diff, FEM_elen_tgt%elen_ele%diff)
      call copy_elen_diffs_type(FEM_elen_tgt%nele_filter_mom,           &
     &    FEM_elen_org%elen_ele%diff2, FEM_elen_tgt%elen_ele%diff2)
!
      end subroutine copy_elen_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_nodal_elen_type(FEM_elen_org, FEM_elen_tgt)
!
      use t_filter_elength
!
      type(gradient_model_data_type), intent(in) :: FEM_elen_org
      type(gradient_model_data_type), intent(inout) :: FEM_elen_tgt
!
!
      FEM_elen_tgt%nnod_filter_mom = FEM_elen_org%nnod_filter_mom
      call copy_elength_type(FEM_elen_tgt%nele_filter_mom,              &
     &    FEM_elen_org%elen_nod%moms, FEM_elen_tgt%elen_nod%moms)
      call copy_elen_diffs_type(FEM_elen_tgt%nele_filter_mom,           &
     &    FEM_elen_org%elen_nod%diff, FEM_elen_tgt%elen_nod%diff)
!
      end subroutine copy_nodal_elen_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_moms_ele_type(FEM_moms_org, FEM_moms_tgt)
!
      use t_filter_elength
      use t_filter_moments
!
      type(gradient_filter_mom_type), intent(in) ::    FEM_moms_org
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms_tgt
      integer(kind = kint) :: ifil
!
!
      FEM_moms_tgt%nele_fmom = FEM_moms_org%nele_fmom
      do ifil = 1, FEM_moms_tgt%num_filter_moms
        call copy_moments_type(FEM_moms_tgt%nele_fmom,                  &
     &      FEM_moms_org%mom_ele(ifil)%moms,                            &
     &      FEM_moms_tgt%mom_ele(ifil)%moms)
        call copy_mom_diffs_type(FEM_moms_tgt%nele_fmom,                &
     &      FEM_moms_org%mom_ele(ifil)%diff,                            &
     &      FEM_moms_tgt%mom_ele(ifil)%diff)
        call copy_mom_diffs_type(FEM_moms_tgt%nele_fmom,                &
     &      FEM_moms_org%mom_ele(ifil)%diff2,                           &
     &      FEM_moms_tgt%mom_ele(ifil)%diff2)
      end do
!
      end subroutine copy_filter_moms_ele_type
!
!  ---------------------------------------------------------------------
!
      end module copy_filter_moment_type
