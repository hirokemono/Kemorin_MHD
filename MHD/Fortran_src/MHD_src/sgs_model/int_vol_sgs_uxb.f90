!
!     module int_vol_sgs_uxb
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!     Modified by H. Matsui on Apr., 2012
!
!      subroutine sel_int_vol_sgs_uxb                                   &
!     &         (i_filter, i_field, id_dx, fem_wk, mhd_fem_wk)
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_sgs_uxb
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use m_geometry_data
!
      use t_finite_element_mat
!
      implicit none
!
      private :: int_vol_sgs_uxb_upm, int_vol_sgs_uxb_pg
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_int_vol_sgs_uxb                                    &
     &         (i_filter, i_field, id_dx, fem_wk, mhd_fem_wk)
!
      use m_element_phys_data
!
      integer (kind=kint), intent(in) :: i_field, i_filter
      integer (kind=kint), intent(in) :: id_dx
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (iflag_mag_supg .eq. id_turn_ON) then
        call int_vol_sgs_uxb_upm(i_filter, i_field,                     &
     &      mhd_fem_wk%n_dvx, id_dx, mhd_fem_wk%dvx,                    &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld,      &
     &      fem_wk)
      else
        call int_vol_sgs_uxb_pg(i_filter, i_field,                      &
     &      mhd_fem_wk%n_dvx, id_dx, mhd_fem_wk%dvx, fem_wk)
      end if
!
      end subroutine sel_int_vol_sgs_uxb
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_uxb_pg(i_filter, i_field,                  &
     &          ncomp_dvx, id_dx, diff_ele, fem_wk)
!
      use m_SGS_model_coefs
      use m_geometry_data_MHD
      use m_jacobians
      use m_filter_elength
!
      use fem_skv_sgs_flux_type
      use nodal_fld_2_each_ele_1st
!
      integer(kind = kint), intent(in) :: i_field, i_filter
      integer(kind = kint), intent(in) :: ncomp_dvx, id_dx
      real(kind = kreal), intent(in) :: diff_ele(ele1%numele,ncomp_dvx)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      integer(kind = kint) :: nd, k2
!
!
      do nd = 1, n_vector
!
! -------- loop for shape function for the phsical values
!
        do k2 = 1, ele1%nnod_4_ele
!
! --------- set magnetic field at each node in an element
!
          call vector_phys_2_each_element(k2, i_field, fem_wk%vector_1)
          call fem_skv_sgs_uxb_galerkin(iele_cd_smp_stack,              &
     &        intg_point_t_evo, k2, i_filter, nd,                       &
     &        ele1, jac1_3d_q, FEM1_elen, fem_wk%vector_1,              &
     &        diff_ele(1,id_dx), fem_wk%sk6)
        end do
      end do
!
      end subroutine int_vol_sgs_uxb_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_uxb_upm(i_filter, i_field,                 &
     &          ncomp_dvx, id_dx, diff_ele,                             &
     &          ncomp_ele, i_magne, d_ele, fem_wk)
!
      use m_SGS_model_coefs
      use m_geometry_data_MHD
      use m_jacobians
      use m_filter_elength
!
      use fem_skv_sgs_flux_type
      use nodal_fld_2_each_ele_1st
!
      integer(kind = kint), intent(in) :: i_field, i_filter
!
      integer(kind = kint), intent(in) :: ncomp_dvx, id_dx
      integer(kind = kint), intent(in) :: ncomp_ele, i_magne
      real(kind = kreal), intent(in) :: diff_ele(ele1%numele,ncomp_dvx)
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      integer(kind = kint) :: nd, k2
!
!
      do nd = 1, n_vector
        do k2 = 1, ele1%nnod_4_ele
          call vector_phys_2_each_element(k2, i_field, fem_wk%vector_1)
          call fem_skv_sgs_uxb_upwind(iele_cd_smp_stack,                &
     &        intg_point_t_evo, k2, i_filter, nd,                       &
     &        ele1, jac1_3d_q, FEM1_elen, fem_wk%vector_1,              &
     &        d_ele(1,i_magne), diff_ele(1,id_dx), fem_wk%sk6)
        end do
      end do
!
      end subroutine int_vol_sgs_uxb_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_sgs_uxb
