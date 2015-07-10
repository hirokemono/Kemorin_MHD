!
!      module int_surf_div_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine int_surf_divergence_sgs(n_int, nmax_grp_sf, ngrp_sf,  &
!     &          id_grp_sf, i_filter, iak_diff, i_vect)
!      subroutine int_surf_div_commute_sgs(n_int, nmax_grp_sf, ngrp_sf, &
!     &         id_grp_sf, i_filter, i_vect)
!
      module int_surf_div_sgs
!
      use m_precision
!
      use m_constants
      use m_geometry_parameter
      use m_surface_group
      use m_finite_element_matrix
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_divergence_sgs(n_int, nmax_grp_sf, ngrp_sf,   &
     &          id_grp_sf, i_filter, iak_diff, i_vect)
!
      use m_SGS_model_coefs
      use m_int_surface_data
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_1
      use cal_skv_to_ff_smp_1st
!
      integer(kind = kint), intent(in) :: n_int, nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
      integer(kind = kint), intent(in) :: i_vect, iak_diff, i_filter
!
      integer(kind=kint) :: k2, nd, i, igrp, i_comp, num
!
!
      if(nmax_grp_sf .eq. 0) return
      call reset_sk6(n_scalar)
!
! --------- set vector at each node in an element
!
      do nd = 1, n_vector
        i_comp = i_vect + nd - 1
        do i = 1, ngrp_sf(nd)
          igrp = id_grp_sf(i,nd)
          num = sf_grp1%istack_grp(igrp) - sf_grp1%istack_grp(igrp-1)
          if (num .gt. 0) then
!
            do k2 = 1, nnod_4_surf
              call dlt_scl_phys_2_each_surface(igrp, k2, i_comp,        &
     &            scalar_sf)
              call fem_sf_skv_sgs_vect_diff_p1(igrp, k2, ione, n_int,   &
     &            i_filter, nd, dxe_sf, scalar_sf, ak_diff(1,iak_diff), &
     &            one, sk6)
            end do
          end if
        end do
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_surf_divergence_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_div_commute_sgs(n_int, nmax_grp_sf, ngrp_sf,  &
     &         id_grp_sf, i_filter, i_vect)
!
      use m_int_surface_data
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_1
      use cal_skv_to_ff_smp_1st
!
       integer(kind = kint), intent(in) :: n_int, nmax_grp_sf
       integer(kind = kint), intent(in) :: ngrp_sf(3)
       integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
       integer(kind = kint), intent(in) :: i_vect, i_filter
!
       integer(kind=kint) :: k2, nd, i, igrp, i_comp, num
!
!
! -------- loop for shape function for the phsical values
!
      if(nmax_grp_sf .eq. 0) return
      call reset_sk6(n_scalar)
!
      do nd = 1, n_vector
        i_comp = i_vect + nd - 1
        do i = 1, ngrp_sf(nd)
          igrp = id_grp_sf(i,nd)
          num = sf_grp1%istack_grp(igrp) - sf_grp1%istack_grp(igrp-1)
          if (num .gt. 0) then
!
            do k2=1, nnod_4_surf
              call dlt_scl_phys_2_each_surface(igrp, k2, i_comp,        &
     &            scalar_sf)
              call fem_sf_skv_sgs_commute_err_p1(igrp, k2, ione, n_int, &
     &            i_filter, nd, dxe_sf, scalar_sf, sk6)
            end do
!
          end if
        end do
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_surf_div_commute_sgs
!
!-----------------------------------------------------------------------
!
      end module int_surf_div_sgs
