!
!     module int_vol_sgs_induct_t
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!     Modified by H. Matsui on Apr., 2012
!
!      subroutine sel_int_vol_sgs_induct_t(i_filter, ie_dvx, ie_dbx,    &
!     &           ifield_v, ifield_b, fem_wk, mhd_fem_wk)
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_sgs_induct_t
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
!
      use t_finite_element_mat
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
      subroutine sel_int_vol_sgs_induct_t(i_filter, ie_dvx, ie_dbx,     &
     &           ifield_v, ifield_b, fem_wk, mhd_fem_wk)
!
      use m_element_phys_data
      use t_MHD_finite_element_mat
!
      integer (kind = kint), intent(in) :: i_filter
      integer (kind = kint), intent(in) :: ifield_v, ifield_b
      integer (kind = kint), intent(in) :: ie_dvx, ie_dbx
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (iflag_mag_supg .gt. id_turn_OFF) then
        call int_vol_sgs_induct_t_upm(i_filter, ifield_v, ifield_b,     &
     &      mhd_fem_wk%n_dvx, ie_dvx, ie_dbx, mhd_fem_wk%dvx,           &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne,                      &
     &      fld_ele1%d_fld, fem_wk)
      else
        call int_vol_sgs_induct_t_pg(i_filter, ifield_v, ifield_b,      &
     &      mhd_fem_wk%n_dvx, ie_dvx, ie_dbx, mhd_fem_wk%dvx, fem_wk)
      end if
!
      end subroutine sel_int_vol_sgs_induct_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_induct_t_pg(i_filter, ifield_v, ifield_b,  &
     &          ncomp_dvx, ie_dvx, ie_dbx, diff_ele, fem_wk)
!
      use m_jacobians
      use m_filter_elength
      use m_node_phys_data
      use nodal_fld_2_each_element
      use fem_skv_sgs_flux_type
!
      integer (kind = kint), intent(in) :: i_filter
      integer (kind = kint), intent(in) :: ifield_v, ifield_b
      integer(kind = kint), intent(in) :: ncomp_dvx, ie_dvx, ie_dbx
      real(kind = kreal), intent(in) :: diff_ele(ele1%numele,ncomp_dvx)
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
        do k2 = 1, ele1%nnod_4_ele
!
! --------- set temperature at each node in an element
!
          call scalar_phys_2_each_element(node1, ele1, nod_fld1,        &
     &        k2, icomp_b, fem_wk%vector_1(1:ele1%numele,1) )
          call scalar_phys_2_each_element(node1, ele1, nod_fld1,        &
     &        k2, icomp_v, fem_wk%vector_1(1:ele1%numele,2) )
!
          call fem_skv_sgs_induct_t_galerkin                            &
     &       (conduct1%istack_ele_fld_smp, intg_point_t_evo, k2,        &
     &        i_filter, nd, ele1, jac1_3d_q, FEM1_elen,                 &
     &        fem_wk%vector_1, diff_ele(1,id_dvx2),                     &
     &        diff_ele(1,id_dbx2), fem_wk%sk6)
        end do
      end do
!
      end subroutine int_vol_sgs_induct_t_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_induct_t_upm(i_filter, ifield_v, ifield_b, &
     &          ncomp_dvx, ie_dvx, ie_dbx, diff_ele,                    &
     &          ncomp_ele, i_magne, d_ele, fem_wk)
!
      use m_jacobians
      use m_filter_elength
      use m_node_phys_data
      use nodal_fld_2_each_element
      use fem_skv_sgs_flux_type
!
      integer (kind = kint), intent(in) :: i_filter
      integer (kind = kint), intent(in) :: ifield_v, ifield_b
!
      integer(kind = kint), intent(in) :: ncomp_dvx, ie_dvx, ie_dbx
      integer(kind = kint), intent(in) :: ncomp_ele, i_magne
      real(kind = kreal), intent(in) :: diff_ele(ele1%numele,ncomp_dvx)
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
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
        do k2 = 1, ele1%nnod_4_ele
          call scalar_phys_2_each_element(node1, ele1, nod_fld1,        &
     &        k2, icomp_v, fem_wk%vector_1(1:ele1%numele,1) )
          call scalar_phys_2_each_element(node1, ele1, nod_fld1,        &
     &        k2, icomp_b, fem_wk%vector_1(1:ele1%numele,2) )
!
          call fem_skv_sgs_induct_t_upwind                              &
     &       (conduct1%istack_ele_fld_smp, intg_point_t_evo, k2,        &
     &        i_filter, nd, ele1, jac1_3d_q, FEM1_elen,                 &
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
