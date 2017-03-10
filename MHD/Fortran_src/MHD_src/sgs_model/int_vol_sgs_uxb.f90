!
!     module int_vol_sgs_uxb
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!     Modified by H. Matsui on Apr., 2012
!
!!      subroutine sel_int_vol_sgs_uxb(i_filter, i_field, id_dx,        &
!!     &          FEM_prm, node, ele, conduct, nod_fld,                 &
!!     &          iphys_ele, ele_fld, jac_3d, FEM_elens,                &
!!     &          fem_wk, mhd_fem_wk)
!!       type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!       type(node_data), intent(in) :: node
!!       type(element_data), intent(in) :: ele
!!       type(phys_data), intent(in) :: nod_fld
!!       type(phys_address), intent(in) :: iphys_ele
!!       type(phys_data), intent(in) :: ele_fld
!!       type(field_geometry_data), intent(in) :: conduct
!!       type(jacobians_3d), intent(in) :: jac_3d
!!       type(gradient_model_data_type), intent(in) :: FEM_elens
!!       type(work_finite_element_mat), intent(inout) :: fem_wk
!!       type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_sgs_uxb
!
      use m_precision
!
      use m_phys_constants
      use m_t_step_parameter
!
      use t_FEM_control_parameter
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_finite_element_mat
      use t_filter_elength
      use t_MHD_finite_element_mat
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
      subroutine sel_int_vol_sgs_uxb(i_filter, i_field, id_dx,          &
     &          FEM_prm, node, ele, conduct, nod_fld,                   &
     &          iphys_ele, ele_fld, jac_3d, FEM_elens,                  &
     &          fem_wk, mhd_fem_wk)
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: i_field, i_filter
      integer (kind=kint), intent(in) :: id_dx
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (FEM_prm%iflag_magne_supg .eq. id_turn_ON) then
        call int_vol_sgs_uxb_upm                                        &
     &     (i_filter, i_field, FEM_prm%npoint_t_evo_int,                &
     &      node, ele, conduct, nod_fld, jac_3d, FEM_elens,             &
     &      mhd_fem_wk%n_dvx, id_dx, mhd_fem_wk%dvx,                    &
     &      ele_fld%ntot_phys, iphys_ele%i_magne, ele_fld%d_fld,        &
     &      fem_wk)
      else
        call int_vol_sgs_uxb_pg                                         &
     &     (i_filter, i_field, FEM_prm%npoint_t_evo_int,                &
     &      node, ele, conduct, nod_fld, jac_3d, FEM_elens,             &
     &      mhd_fem_wk%n_dvx, id_dx, mhd_fem_wk%dvx, fem_wk)
      end if
!
      end subroutine sel_int_vol_sgs_uxb
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_uxb_pg(i_filter, i_field, num_int,         &
     &          node, ele, conduct, nod_fld, jac_3d, FEM_elens,         &
     &          ncomp_dvx, id_dx, diff_ele, fem_wk)
!
      use fem_skv_sgs_flux_type
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: i_field, i_filter
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_dvx, id_dx
      real(kind = kreal), intent(in) :: diff_ele(ele%numele,ncomp_dvx)
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
        do k2 = 1, ele%nnod_4_ele
!
! --------- set magnetic field at each node in an element
!
          call vector_phys_2_each_element(node, ele, nod_fld,           &
     &        k2, i_field, fem_wk%vector_1)
          call fem_skv_sgs_uxb_galerkin                                 &
     &       (conduct%istack_ele_fld_smp, num_int, k2, i_filter, nd,    &
     &        ele, jac_3d, FEM_elens, fem_wk%vector_1,                  &
     &        diff_ele(1,id_dx), fem_wk%sk6)
        end do
      end do
!
      end subroutine int_vol_sgs_uxb_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_uxb_upm(i_filter, i_field, num_int,        &
     &          node, ele, conduct, nod_fld, jac_3d, FEM_elens,         &
     &          ncomp_dvx, id_dx, diff_ele,                             &
     &          ncomp_ele, i_magne, d_ele, fem_wk)
!
      use fem_skv_sgs_flux_type
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: i_field, i_filter
      integer(kind = kint), intent(in) :: num_int
!
      integer(kind = kint), intent(in) :: ncomp_dvx, id_dx
      integer(kind = kint), intent(in) :: ncomp_ele, i_magne
      real(kind = kreal), intent(in) :: diff_ele(ele%numele,ncomp_dvx)
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      integer(kind = kint) :: nd, k2
!
!
      do nd = 1, n_vector
        do k2 = 1, ele%nnod_4_ele
          call vector_phys_2_each_element(node, ele, nod_fld,           &
     &        k2, i_field, fem_wk%vector_1)
          call fem_skv_sgs_uxb_upwind                                   &
     &       (conduct%istack_ele_fld_smp, num_int, k2, i_filter, dt,    &
     &        nd, ele, jac_3d, FEM_elens, fem_wk%vector_1,              &
     &        d_ele(1,i_magne), diff_ele(1,id_dx), fem_wk%sk6)
        end do
      end do
!
      end subroutine int_vol_sgs_uxb_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_sgs_uxb
