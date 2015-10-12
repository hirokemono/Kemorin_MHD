!
!      module int_surf_div_induct_tsr_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_surf_div_induct_t_sgs(sf_grp, n_int,             &
!!     &          i_flux, i_filter, i_v, i_b)
!!      subroutine int_surf_commute_induct_t(sf_grp, n_int,             &
!!     &          i_flux, i_filter, i_v, i_b)
!
      module int_surf_div_induct_tsr_sgs
!
      use m_precision
!
      use m_constants
      use m_geometry_data
      use m_phys_constants
      use m_sorted_node
      use m_finite_element_matrix
      use m_surf_data_magne
!
      use t_group_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_div_induct_t_sgs(sf_grp, n_int,               &
     &          i_flux, i_filter, i_v, i_b)
!
      use m_SGS_model_coefs
      use m_SGS_address
      use m_int_surface_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_filter_elength
!
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp_1st
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
!
      integer(kind=kint) :: k2, nd, i, igrp, num
!
!
      if (sum(ngrp_sf_sgs_magne) .eq. 0) return
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do nd = 1, n_vector
        do i = 1, ngrp_sf_sgs_magne(nd)
          igrp = id_grp_sf_sgs_magne(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if (num .gt.0) then
!
            do k2 = 1, surf1%nnod_4_surf
              call d_SGS_induct_t_2_each_surface(sf_grp, igrp, k2, nd,  &
     &            i_flux, i_b, i_v, vect_sf)
              call fem_sf_grp_skv_sgs_div_flux_p                        &
     &           (ele1, surf1, sf_grp, jac1_sf_grp_2d_q, FEM1_elen,     &
     &            igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf,       &
     &            ak_diff(1,iak_diff_uxb), dminus, fem1_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine int_surf_div_induct_t_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_commute_induct_t(sf_grp, n_int,               &
     &          i_flux, i_filter, i_v, i_b)
!
      use m_int_surface_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_filter_elength
!
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp_1st
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
!
      integer(kind=kint) :: k2, nd, i, igrp, num
!
!
      if (sum(ngrp_sf_sgs_magne) .eq. 0) return
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do nd = 1, n_vector
        do i = 1, ngrp_sf_sgs_magne(nd)
          igrp = id_grp_sf_sgs_magne(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if(num .gt. 0) then
!
            do k2 = 1, surf1%nnod_4_surf
              call d_SGS_induct_t_2_each_surface(sf_grp, igrp, k2,      &
     &            nd, i_flux, i_b, i_v, vect_sf)
              call fem_sf_grp_skv_div_f_commute_p                       &
     &           (ele1, surf1, sf_grp, jac1_sf_grp_2d_q, FEM1_elen,     &
     &            igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf,       &
     &            fem1_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine int_surf_commute_induct_t
!
!-----------------------------------------------------------------------
!
      end module int_surf_div_induct_tsr_sgs
