!
!      module int_surf_grad_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_surf_gradient_sgs(sf_grp, n_int,                 &
!!     &          ngrp_sf, id_grp_sf, i_filter, iak_diff, i_scalar)
!!      subroutine int_surf_grad_commute_sgs(sf_grp, n_int,             &
!!     &          ngrp_sf, id_grp_sf, i_filter, i_scalar)
!!        type(surface_group_data), intent(in) :: sf_grp
!
      module int_surf_grad_sgs
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use m_geometry_data
      use m_sorted_node
      use m_finite_element_matrix
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
      subroutine int_surf_gradient_sgs(sf_grp, n_int,                   &
     &          ngrp_sf, id_grp_sf, i_filter, iak_diff, i_scalar)
!
      use m_int_surface_data
      use m_SGS_model_coefs
      use m_int_surface_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_filter_elength
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp_1st
!
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_scalar, iak_diff, i_filter
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
        if (num .gt. 0) then
!
          do k2 = 1, surf1%nnod_4_surf
            call dlt_scl_phys_2_each_surface(sf_grp, igrp, k2,          &
     &          i_scalar, vect_sf(1,1) )
            call fem_sf_grp_skv_sgs_grad_p                              &
     &         (ele1, surf1, sf_grp, jac1_sf_grp_2d_q, FEM1_elen,       &
     &          igrp, k2, n_int, i_filter, dxe_sf, scalar_sf,           &
     &          ak_diff(1,iak_diff), one, fem1_wk%sk6)
          end do
!
        end if
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine int_surf_gradient_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_grad_commute_sgs(sf_grp, n_int,               &
     &          ngrp_sf, id_grp_sf, i_filter, i_scalar)
!
      use m_int_surface_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_filter_elength
!
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp_1st
!
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_scalar, i_filter
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
        if (num .gt. 0) then
!
          do k2 = 1, surf1%nnod_4_surf
            call dlt_scl_phys_2_each_surface(sf_grp, igrp, k2,          &
     &          i_scalar, scalar_sf )
            call fem_sf_grp_skv_grad_commute_p                          &
     &         (ele1, surf1, sf_grp, jac1_sf_grp_2d_q, FEM1_elen,       &
     &          igrp, k2, n_int, i_filter, dxe_sf, scalar_sf,           &
     &          fem1_wk%sk6)
          end do
!
        end if
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_nl_smp)
!
      end subroutine int_surf_grad_commute_sgs
!
!-----------------------------------------------------------------------
!
      end module int_surf_grad_sgs
