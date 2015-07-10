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
      use m_node_phys_address
      use m_SGS_address
      use m_surf_data_press
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON                     &
     &     .and. ngrp_sf_sgs_p.gt.0) then
        call int_surf_sgs_velo_co_ele(intg_point_poisson,               &
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
      use m_node_phys_address
      use m_SGS_address
      use m_surf_data_magne_p
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON                    &
     &     .and. ngrp_sf_sgs_magp.gt.0) then
         call int_surf_sgs_velo_co_ele(intg_point_poisson,              &
     &       ngrp_sf_sgs_magp,  id_grp_sf_sgs_magp, ifilter_final,      &
     &       iak_diff_b, iphys%i_m_phi)
      end if
!
      end subroutine int_surf_sgs_magne_co
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_sgs_velo_co_ele(n_int, ngrp_sf, id_grp_sf,    &
     &         i_filter, iak_diff, i_comp)
!
      use m_geometry_parameter
      use m_surface_group
      use m_node_phys_address
      use m_phys_constants
      use m_SGS_address
      use m_finite_element_matrix
      use m_SGS_model_coefs
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_1
      use cal_skv_to_ff_smp_1st
!
!
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
      call reset_sk6(n_vector)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp1%istack_grp(igrp) - sf_grp1%istack_grp(igrp-1)
        if(num .gt. 0) then
!
          do k2=1, nnod_4_surf
            call dlt_scl_phys_2_each_surf_cst(igrp, k2,                 &
     &            i_comp, dminus, scalar_sf )
            call fem_sf_skv_sgs_velo_co_p1(igrp, k2, n_int, i_filter,   &
     &            dxe_sf, scalar_sf, ak_diff(1,iak_diff), sk6)
          end do
!
        end if
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_surf_sgs_velo_co_ele
!
!-----------------------------------------------------------------------
!
      end module int_surf_velo_co_sgs
