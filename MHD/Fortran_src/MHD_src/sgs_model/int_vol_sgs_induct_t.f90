!
!     module int_vol_sgs_induct_t
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!     Modified by H. Matsui on Apr., 2012
!
!!      subroutine sel_int_vol_sgs_induct_t                             &
!!     &         (i_filter, ie_dvx, ie_dbx, ifield_v, ifield_b, dt,     &
!!     &          FEM_prm, node, ele, conduct, nod_fld, iphys_ele,      &
!!     &          ele_fld, g_FEM, jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_sgs_induct_t
!
      use m_precision
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_finite_element_mat
      use t_filter_elength
!
      implicit none
!
      private :: int_vol_sgs_induct_t_pg, int_vol_sgs_induct_t_upm
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_int_vol_sgs_induct_t                               &
     &         (i_filter, ie_dvx, ie_dbx, ifield_v, ifield_b, dt,       &
     &          FEM_prm, node, ele, conduct, nod_fld, iphys_ele,        &
     &          ele_fld, g_FEM, jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind = kint), intent(in) :: i_filter
      integer (kind = kint), intent(in) :: ifield_v, ifield_b
      integer (kind = kint), intent(in) :: ie_dvx, ie_dbx
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (FEM_prm%iflag_magne_supg .gt. id_turn_OFF) then
        call int_vol_sgs_induct_t_upm                                   &
     &     (i_filter, ifield_v, ifield_b, FEM_prm%npoint_t_evo_int, dt, &
     &      node, ele, conduct, nod_fld, g_FEM, jac_3d, FEM_elens,      &
     &      mhd_fem_wk%n_dvx, ie_dvx, ie_dbx, mhd_fem_wk%dvx,           &
     &      ele_fld%ntot_phys, iphys_ele%i_magne,                       &
     &      ele_fld%d_fld, fem_wk)
      else
        call int_vol_sgs_induct_t_pg                                    &
     &     (i_filter, ifield_v, ifield_b, FEM_prm%npoint_t_evo_int,     &
     &      node, ele, conduct, nod_fld, g_FEM, jac_3d, FEM_elens,      &
     &      mhd_fem_wk%n_dvx, ie_dvx, ie_dbx, mhd_fem_wk%dvx, fem_wk)
      end if
!
      end subroutine sel_int_vol_sgs_induct_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_induct_t_pg                                &
     &         (i_filter, ifield_v, ifield_b, num_int,                  &
     &          node, ele, conduct, nod_fld, g_FEM, jac_3d, FEM_elens,  &
     &          ncomp_dvx, ie_dvx, ie_dbx, diff_ele, fem_wk)
!
      use nodal_fld_2_each_element
      use fem_skv_sgs_flux_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: ifield_v, ifield_b
      integer(kind = kint), intent(in) :: num_int
!
      integer(kind = kint), intent(in) :: ncomp_dvx, ie_dvx, ie_dbx
      real(kind = kreal), intent(in) :: diff_ele(ele%numele,ncomp_dvx)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      integer (kind = kint) :: nd, nd2, nd3
      integer (kind = kint) :: icomp_v, icomp_b
      integer (kind = kint) :: id_dvx2, id_dbx2
      integer (kind = kint) :: k2
!
!
      do nd = 1, n_asym_tensor
        nd2 = ithree - mod((nd+ione),itwo)
        nd3 = ithree - mod( nd,      ithree)
        id_dvx2 = ie_dvx + 3*(nd2-1)
        id_dbx2 = ie_dbx + 3*(nd2-1)
        icomp_v = ifield_v + nd3 - 1
        icomp_b = ifield_b + nd3 - 1
!
! -------- loop for shape function for the phsical values
!
        do k2 = 1, ele%nnod_4_ele
!
! --------- set temperature at each node in an element
!
          call scalar_phys_2_each_element(node, ele, nod_fld,           &
     &        k2, icomp_b, fem_wk%vector_1(1:ele%numele,1) )
          call scalar_phys_2_each_element(node, ele, nod_fld,           &
     &        k2, icomp_v, fem_wk%vector_1(1:ele%numele,2) )
!
          call fem_skv_sgs_induct_t_galerkin                            &
     &       (conduct%istack_ele_fld_smp, num_int, k2,                  &
     &        i_filter, nd, ele, g_FEM, jac_3d, FEM_elens,              &
     &        fem_wk%vector_1, diff_ele(1,id_dvx2),                     &
     &        diff_ele(1,id_dbx2), fem_wk%sk6)
        end do
      end do
!
      end subroutine int_vol_sgs_induct_t_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_induct_t_upm                               &
     &         (i_filter, ifield_v, ifield_b, num_int, dt,              &
     &          node, ele, conduct, nod_fld, g_FEM, jac_3d, FEM_elens,  &
     &          ncomp_dvx, ie_dvx, ie_dbx, diff_ele,                    &
     &          ncomp_ele, i_magne, d_ele, fem_wk)
!
      use nodal_fld_2_each_element
      use fem_skv_sgs_flux_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint), intent(in) :: ifield_v, ifield_b
      integer(kind = kint), intent(in) :: num_int
!
      integer(kind = kint), intent(in) :: ncomp_dvx, ie_dvx, ie_dbx
      integer(kind = kint), intent(in) :: ncomp_ele, i_magne
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: diff_ele(ele%numele,ncomp_dvx)
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      integer (kind = kint) :: nd, nd2, nd3
      integer (kind = kint) :: icomp_v, icomp_b
      integer (kind = kint) :: id_dvx2, id_dbx2
      integer (kind = kint) :: k2
!
!
      do nd = 1, n_asym_tensor
        nd2 = ithree - mod((nd+ione),itwo)
        nd3 = ithree - mod( nd,      ithree)
        id_dvx2 = ie_dvx + 3*(nd2-1)
        id_dbx2 = ie_dbx + 3*(nd2-1)
        icomp_v = ifield_v + nd3 - 1
        icomp_b = ifield_b + nd3 - 1
!
        do k2 = 1, ele%nnod_4_ele
          call scalar_phys_2_each_element(node, ele, nod_fld,           &
     &        k2, icomp_v, fem_wk%vector_1(1:ele%numele,1) )
          call scalar_phys_2_each_element(node, ele, nod_fld,           &
     &        k2, icomp_b, fem_wk%vector_1(1:ele%numele,2) )
!
          call fem_skv_sgs_induct_t_upwind                              &
     &       (conduct%istack_ele_fld_smp, num_int, k2,                  &
     &        i_filter, dt, nd, ele, g_FEM, jac_3d, FEM_elens,          &
     &        fem_wk%vector_1, d_ele(1,i_magne),                        &
     &        diff_ele(1,id_dvx2), diff_ele(1,id_dbx2), fem_wk%sk6)
        end do
      end do
!
      end subroutine int_vol_sgs_induct_t_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_sgs_induct_t
