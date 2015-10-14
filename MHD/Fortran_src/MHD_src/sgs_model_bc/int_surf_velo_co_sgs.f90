!
!      module int_surf_velo_co_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine int_surf_sgs_velo_co
!      subroutine int_surf_sgs_magne_co
!      subroutine int_surf_sgs_velo_co_ele(ngrp_sf, id_grp_sf,          &
!     &         i_filter, iak_diff, i_comp)
!
      module int_surf_velo_co_sgs
!
      use m_precision
!
      use m_constants
      use m_control_parameter
!
      implicit none
!
      private :: int_surf_sgs_velo_co_ele
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_sgs_velo_co
!
      use m_group_data
      use m_node_phys_address
      use m_SGS_address
      use m_surf_data_press
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON                     &
     &     .and. ngrp_sf_sgs_p.gt.0) then
        call int_surf_sgs_velo_co_ele(sf_grp1, intg_point_poisson,      &
     &      ngrp_sf_sgs_p, id_grp_sf_sgs_p, ifilter_final,              &
     &      iak_diff_v, iphys%i_p_phi)
      end if
!
      end subroutine int_surf_sgs_velo_co
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_sgs_magne_co
!
      use m_group_data
      use m_node_phys_address
      use m_SGS_address
      use m_surf_data_magne_p
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON                    &
     &     .and. ngrp_sf_sgs_magp.gt.0) then
         call int_surf_sgs_velo_co_ele(sf_grp1, intg_point_poisson,     &
     &       ngrp_sf_sgs_magp,  id_grp_sf_sgs_magp, ifilter_final,      &
     &       iak_diff_b, iphys%i_m_phi)
      end if
!
      end subroutine int_surf_sgs_magne_co
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_sgs_velo_co_ele(sf_grp, n_int,                &
     &          ngrp_sf, id_grp_sf, i_filter, iak_diff, i_comp)
!
      use m_geometry_data
      use m_node_phys_address
      use m_phys_constants
      use m_SGS_address
      use m_sorted_node
      use m_finite_element_matrix
      use m_SGS_model_coefs
      use m_int_surface_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_filter_elength
      use t_group_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: iak_diff, i_filter
      integer(kind = kint), intent(in) :: i_comp
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
!  ---------  set number of integral points
!
      if (ngrp_sf.eq.0) return
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if(num .gt. 0) then
!
          do k2 = 1, surf1%nnod_4_surf
            call dlt_scl_phys_2_each_surf_cst(sf_grp, igrp, k2,         &
     &          i_comp, dminus, scalar_sf )
            call fem_sf_grp_skv_sgs_velo_co_p(ele1, surf1, sf_grp,      &
     &          jac1_sf_grp_2d_q, jac1_sf_grp_2d_l, FEM1_elen,          &
     &          igrp, k2, n_int, i_filter, dxe_sf, scalar_sf,           &
     &          ak_diff(1,iak_diff), fem1_wk%sk6)
          end do
!
        end if
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_surf_sgs_velo_co_ele
!
!-----------------------------------------------------------------------
!
      end module int_surf_velo_co_sgs
