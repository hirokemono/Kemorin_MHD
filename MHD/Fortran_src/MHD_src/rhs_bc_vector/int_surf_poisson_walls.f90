!
!      module int_surf_poisson_walls
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine int_surf_poisson_wall                                &
!!     &         (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, n_int,&
!!     &          ngrp_sf, id_grp_sf, i_vect)
!!      subroutine int_surf_poisson_sph_in                              &
!!     &         (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, n_int,&
!!     &          ngrp_sf, id_grp_sf, i_vect)
!!      subroutine int_surf_poisson_sph_out                             &
!!     &         (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, n_int,&
!!     &          ngrp_sf, id_grp_sf, i_vect)
!!        type(surface_group_data), intent(in) :: sf_grp
!
      module int_surf_poisson_walls
!
      use m_precision
      use m_constants
!
      use m_geometry_constants
      use m_phys_constants
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_surface_data
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
!
      use node_phys_2_each_surface
      use fem_surf_skv_poisson_type
      use cal_skv_to_ff_smp
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_poisson_wall                                  &
     &         (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, n_int,  &
     &          ngrp_sf, id_grp_sf, i_vect)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_vect
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      call reset_sk6(n_scalar, ele, fem1_wk%sk6)
!
! --------- set vector at each node in an element
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .gt.0 ) then
!
          do k2 = 1, num_linear_sf
            call vector_phys_2_each_surface                             &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2,             &
     &          i_vect, vect_sf)
            call fem_surf_skv_poisson_wall                              &
     &         (ele, surf, sf_grp, jac_sf_grp_l, igrp, k2, n_int,       &
     &          vect_sf, fem1_wk%sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl1, fem1_wk%sk6, f1_l%ff_smp)
!
      end subroutine int_surf_poisson_wall
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_poisson_sph_in                                &
     &         (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, n_int,  &
     &          ngrp_sf, id_grp_sf, i_vect)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_vect
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      call reset_sk6(n_scalar, ele, fem1_wk%sk6)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .gt.0 ) then
!
! -------- loop for shape function for the phsical values
          do k2 = 1, num_linear_sf
            call vector_phys_2_each_surf_cst                            &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2,             &
     &          i_vect, dminus, vect_sf)
            call fem_surf_skv_poisson_sph_out                           &
     &         (ele, surf, sf_grp, jac_sf_grp_l, igrp, k2,              &
     &          n_int, xe_sf, vect_sf, fem1_wk%sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl1, fem1_wk%sk6, f1_l%ff_smp)
!
      end subroutine int_surf_poisson_sph_in
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_poisson_sph_out                               &
     &         (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, n_int,  &
     &          ngrp_sf, id_grp_sf, i_vect)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_vect
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      call reset_sk6(n_scalar, ele, fem1_wk%sk6)
!
! --------- set vector at each node in an element
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (num .gt. 0) then
!
! -------- loop for shape function for the phsical values
          do k2 = 1, num_linear_sf
            call vector_phys_2_each_surface                             &
     &         (node, ele, surf, sf_grp, nod_fld, igrp, k2,             &
     &          i_vect, vect_sf)
            call fem_surf_skv_poisson_sph_out                           &
     &         (ele, surf, sf_grp, jac_sf_grp_l, igrp, k2, n_int,       &
     &          xe_sf, vect_sf, fem1_wk%sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl1,                    &
     &    fem1_wk%sk6, f1_l%ff_smp)
!
      end subroutine int_surf_poisson_sph_out
!
!-----------------------------------------------------------------------
!
      end module int_surf_poisson_walls
