!
!      module int_vol_sgs_flux
!
!        Written by H.Matsui   on July 2005
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on July, 2007
!        modified by H. Matsui on April, 2012
!
!      subroutine sel_int_vol_sgs_flux(iflag_4_supg, i_filter, numdir,  &
!     &           i_field, id_dx, fem_wk, mhd_fem_wk)
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_sgs_flux
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use m_geometry_data
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
      subroutine sel_int_vol_sgs_flux(iflag_4_supg, i_filter, numdir,   &
     &           i_field, id_dx, fem_wk, mhd_fem_wk)
!
      use m_element_phys_data
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer (kind = kint), intent(in) :: id_dx, i_filter
      integer (kind = kint), intent(in) :: numdir, i_field
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_vol_sgs_flux_upwind(i_filter, numdir, i_field,         &
     &      mhd_fem_wk%n_dvx, id_dx, mhd_fem_wk%dvx,                    &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld,      &
     &      fem_wk)
      else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_vol_sgs_flux_upwind(i_filter, numdir, i_field,         &
     &      mhd_fem_wk%n_dvx, id_dx, mhd_fem_wk%dvx,                    &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,       &
     &      fem_wk)
      else
        call int_vol_sgs_flux_pg(i_filter, numdir, i_field,             &
     &      mhd_fem_wk%n_dvx, id_dx, mhd_fem_wk%dvx, fem_wk)
      end if
!
      end subroutine sel_int_vol_sgs_flux
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_sgs_flux_pg(i_filter, numdir, i_field,         &
     &          ncomp_dvx, id_dx, diff_ele, fem_wk)
!
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_jacobians
      use m_filter_elength
      use m_SGS_model_coefs
!
      use fem_skv_sgs_flux_type
      use nodal_fld_2_each_element
!
      integer (kind = kint), intent(in) :: i_filter, numdir, i_field
      integer(kind = kint), intent(in) :: ncomp_dvx, id_dx
      real(kind = kreal), intent(in) :: diff_ele(ele1%numele,ncomp_dvx)
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
          do k2 = 1, ele1%nnod_4_ele
            call scalar_phys_2_each_element(node1, ele1, nod_fld1,      &
     &          k2, icomp, fem_wk%scalar_1)
            call fem_skv_sgs_flux_galerkin(fluid1%istack_ele_fld_smp,   &
     &          intg_point_t_evo, k2, i_filter, nd_t,                   &
     &          ele1, jac1_3d_q, FEM1_elen, fem_wk%scalar_1,            &
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
      subroutine int_vol_sgs_flux_upwind(i_filter, numdir, i_field,     &
     &          ncomp_dvx, id_dx, diff_ele,                             &
     &          ncomp_ele, ie_upw, d_ele, fem_wk)
!
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_jacobians
      use m_filter_elength
      use m_SGS_model_coefs
!
      use fem_skv_sgs_flux_type
      use nodal_fld_2_each_element
!
      integer (kind = kint), intent(in) :: i_filter, numdir, i_field
      integer(kind = kint), intent(in) :: ncomp_dvx, id_dx
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: diff_ele(ele1%numele,ncomp_dvx)
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
          do k2 = 1, ele1%nnod_4_ele
            call scalar_phys_2_each_element(node1, ele1, nod_fld1,      &
     &          k2, icomp, fem_wk%scalar_1)
            call fem_skv_sgs_flux_upwind(fluid1%istack_ele_fld_smp,     &
     &          intg_point_t_evo, k2, i_filter, nd_t,                   &
     &          ele1, jac1_3d_q, FEM1_elen, fem_wk%scalar_1,            &
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
