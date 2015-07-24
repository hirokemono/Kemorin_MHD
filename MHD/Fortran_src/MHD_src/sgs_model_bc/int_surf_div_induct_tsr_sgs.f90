!
!      module int_surf_div_induct_tsr_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine int_surf_div_induct_t_sgs(n_int, i_flux, i_filter,    &
!     &          i_v, i_b)
!      subroutine int_surf_commute_induct_t(n_int, i_flux, i_filter,    &
!     &          i_v, i_b)
!
      module int_surf_div_induct_tsr_sgs
!
      use m_precision
!
      use m_constants
      use m_geometry_parameter
      use m_phys_constants
      use m_finite_element_matrix
      use m_surf_data_magne
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_div_induct_t_sgs(n_int, i_flux, i_filter,     &
     &          i_v, i_b)
!
      use m_SGS_model_coefs
      use m_SGS_address
      use m_group_data
      use m_int_surface_data
!
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_1
      use cal_skv_to_ff_smp_1st
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
!
      integer(kind=kint) :: k2, nd, i, igrp, num
!
!
      if (sum(ngrp_sf_sgs_magne) .eq. 0) return
      call reset_sk6(n_vector)
!
      do nd = 1, n_vector
        do i = 1, ngrp_sf_sgs_magne(nd)
          igrp = id_grp_sf_sgs_magne(i,nd)
          num = sf_grp1%istack_grp(igrp) - sf_grp1%istack_grp(igrp-1)
          if (num .gt.0) then
!
            do k2=1, nnod_4_surf
              call d_SGS_induct_t_2_each_surface(sf_grp1, igrp, k2, nd, &
     &            i_flux, i_b, i_v, vect_sf)
              call fem_sf_skv_sgs_div_flux_p1(sf_grp1, igrp, k2, nd,    &
     &            n_int, i_filter, dxe_sf, vect_sf,                     &
     &            ak_diff(1,iak_diff_uxb), dminus, sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_surf_div_induct_t_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_commute_induct_t(n_int, i_flux, i_filter,     &
     &          i_v, i_b)
!
      use m_int_surface_data
      use m_group_data
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_1
      use cal_skv_to_ff_smp_1st
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
!
      integer(kind=kint) :: k2, nd, i, igrp, num
!
!
      if (sum(ngrp_sf_sgs_magne) .eq. 0) return
      call reset_sk6(n_vector)
!
      do nd = 1, n_vector
        do i = 1, ngrp_sf_sgs_magne(nd)
          igrp = id_grp_sf_sgs_magne(i,nd)
          num = sf_grp1%istack_grp(igrp) - sf_grp1%istack_grp(igrp-1)
          if(num .gt. 0) then
!
            do k2=1, nnod_4_surf
              call d_SGS_induct_t_2_each_surface(sf_grp1, igrp, k2,     &
     &            nd, i_flux, i_b, i_v, vect_sf)
              call fem_sf_skv_div_flux_commute_p1(sf_grp1, igrp, k2,    &
     &            nd, n_int, i_filter, dxe_sf, vect_sf, sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_surf_commute_induct_t
!
!-----------------------------------------------------------------------
!
      end module int_surf_div_induct_tsr_sgs
