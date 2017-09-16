!
!      module int_vol_sgs_flux
!
!        Written by H.Matsui   on July 2005
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on July, 2007
!        modified by H. Matsui on April, 2012
!
!!      subroutine sel_int_vol_sgs_flux(iflag_4_supg, num_int, dt,      &
!!     &          i_filter, numdir, i_field, id_dx,                     &
!!     &          node, ele, fluid, nod_fld, iphys_ele, ele_fld,        &
!!     &          g_FEM, jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_sgs_flux
!
      use m_precision
!
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_filter_elength
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      implicit none
!
      private :: int_vol_sgs_flux_pg, int_vol_sgs_flux_upwind
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_int_vol_sgs_flux(iflag_4_supg, num_int, dt,        &
     &          i_filter, numdir, i_field, id_dx,                       &
     &          node, ele, fluid, nod_fld, iphys_ele, ele_fld,          &
     &          g_FEM, jac_3d, FEM_elens, fem_wk, mhd_fem_wk)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: iflag_4_supg, num_int
      integer (kind = kint), intent(in) :: id_dx, i_filter
      integer (kind = kint), intent(in) :: numdir, i_field
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_vol_sgs_flux_upwind                                    &
     &     (num_int, dt, i_filter, numdir, i_field,                     &
     &      node, ele, fluid, nod_fld, g_FEM, jac_3d, FEM_elens,        &
     &      mhd_fem_wk%n_dvx, id_dx, mhd_fem_wk%dvx,                    &
     &      ele_fld%ntot_phys, iphys_ele%i_magne, ele_fld%d_fld,        &
     &      fem_wk)
      else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_vol_sgs_flux_upwind                                    &
     &     (num_int, dt, i_filter, numdir, i_field,                     &
     &      node, ele, fluid, nod_fld, g_FEM, jac_3d, FEM_elens,        &
     &      mhd_fem_wk%n_dvx, id_dx, mhd_fem_wk%dvx,                    &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      fem_wk)
      else
        call int_vol_sgs_flux_pg(num_int, i_filter, numdir, i_field,    &
     &      node, ele, fluid, nod_fld, g_FEM, jac_3d, FEM_elens,        &
     &      mhd_fem_wk%n_dvx, id_dx, mhd_fem_wk%dvx, fem_wk)
      end if
!
      end subroutine sel_int_vol_sgs_flux
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_sgs_flux_pg                                    &
     &         (num_int, i_filter, numdir, i_field,                     &
     &          node, ele, fluid, nod_fld, g_FEM, jac_3d, FEM_elens,    &
     &          ncomp_dvx, id_dx, diff_ele, fem_wk)
!
      use fem_skv_sgs_flux_type
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind = kint), intent(in) :: i_filter, numdir, i_field
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_dvx, id_dx
      real(kind = kreal), intent(in) :: diff_ele(ele%numele,ncomp_dvx)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      integer (kind = kint) :: icomp, nd, ndx, nd2, nd_t
      integer (kind = kint) :: k2, id_dvx2
!
!
      do nd = 1, numdir
        icomp = i_field + nd - 1
!
        do ndx = 1, 4-nd
          nd2 = nd + ndx - 1
          id_dvx2 = id_dx + 3*(nd2-1)
          nd_t = lst_sim_t(nd) + ndx
!
! -------- loop for shape function for the phsical values
          do k2 = 1, ele%nnod_4_ele
            call scalar_phys_2_each_element(node, ele, nod_fld,         &
     &          k2, icomp, fem_wk%scalar_1)
            call fem_skv_sgs_flux_galerkin                              &
     &         (fluid%istack_ele_fld_smp, num_int, k2, i_filter, nd_t,  &
     &          ele, g_FEM, jac_3d, FEM_elens, fem_wk%scalar_1,         &
     &          diff_ele(1,id_dvx2), fem_wk%sk6)
          end do
!
        end do
      end do
!
      end subroutine int_vol_sgs_flux_pg
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sgs_flux_upwind                                &
     &         (num_int, dt, i_filter, numdir, i_field,                 &
     &          node, ele, fluid, nod_fld, g_FEM, jac_3d, FEM_elens,    &
     &          ncomp_dvx, id_dx, diff_ele, ncomp_ele, ie_upw, d_ele,   &
     &          fem_wk)
!
      use fem_skv_sgs_flux_type
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind = kint), intent(in) :: i_filter, numdir, i_field
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_dvx, id_dx
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: diff_ele(ele%numele,ncomp_dvx)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      integer (kind = kint) :: icomp, nd, ndx, nd2, nd_t
      integer (kind = kint) :: k2, id_dvx2
!
!  ---------  set number of integral points
!
      do nd = 1, numdir
        icomp = i_field + nd - 1
!
        do ndx = 1, 4-nd
          nd2 = nd + ndx - 1
          id_dvx2 = id_dx + 3*(nd2-1)
          nd_t = lst_sim_t(nd) + ndx
!
! -------- loop for shape function for the phsical values
!
          do k2 = 1, ele%nnod_4_ele
            call scalar_phys_2_each_element(node, ele, nod_fld,         &
     &          k2, icomp, fem_wk%scalar_1)
            call fem_skv_sgs_flux_upwind                                &
     &         (fluid%istack_ele_fld_smp, num_int, k2, i_filter, dt,    &
     &          nd_t, ele, g_FEM, jac_3d, FEM_elens, fem_wk%scalar_1,   &
     &          d_ele(1,ie_upw), diff_ele(1,id_dvx2), fem_wk%sk6)
          end do
        end do
      end do
!
      end subroutine int_vol_sgs_flux_upwind
!
!-----------------------------------------------------------------------
!
      end module int_vol_sgs_flux
