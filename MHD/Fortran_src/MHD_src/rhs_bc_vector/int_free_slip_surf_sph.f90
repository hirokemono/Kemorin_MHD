!>@file   int_free_slip_surf_sph.f90
!!@brief  module int_free_slip_surf_sph
!!
!!@author H. Matsui
!!@date Written  by H. Matsui on Dec., 2003
!@n      modified by H. Matsui on Aug., 2005
!
!>@brief  FEM integration for stress free boundary
!!         or pseudo vacuum boundary
!!
!!
!!@verbatim
!!      subroutine int_free_slip_surf_sph_out(n_int, ngrp_surf_outside, &
!!     &          id_grp_outside, i_field)
!!      subroutine int_free_slip_surf_sph_in(n_int, ngrp_surf_inside,   &
!!     &          id_grp_inside, i_field)
!!@endverbatim
!!
!@param    n_int       numbper of integration points
!@param    i_field     field address
!!
!@param    ngrp_surf_outside
!!                     Number of surface group for outer boundary
!@param    id_grp_outside(ngrp_surf_outside)
!!                     surface group ID for outer boundary
!@param    ngrp_surf_inside     field address
!!                     Number of surface group for inner boundary
!@param    id_grp_inside(ngrp_surf_inside)     field address
!!                     surface group ID for inner boundary
!
      module int_free_slip_surf_sph
!
      use m_precision
      use m_constants
!
      use m_geometry_parameter
      use m_surface_group
      use m_int_vol_data
      use m_finite_element_matrix
      use m_ele_material_property
!
      use fem_surf_skv_poisson_1st
      use cal_skv_to_ff_smp_1st
      use node_phys_2_each_surface
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_free_slip_surf_sph_out(n_int, ngrp_surf_outside,   &
     &          id_grp_outside, i_field)
!
      use m_node_phys_address
      use m_int_surface_data
!
      integer (kind = kint), intent(in) :: n_int, i_field
      integer (kind = kint), intent(in) ::ngrp_surf_outside
      integer (kind = kint), intent(in)                                 &
     &       :: id_grp_outside(ngrp_surf_outside)
!
      integer (kind = kint) :: k2, i, igrp, num
!
!
      if(ngrp_surf_outside .le. 0) return
      call reset_sk6(n_vector)
!
      do i = 1, ngrp_surf_outside
        igrp = id_grp_outside(i)
        num = sf_grp1%istack_grp(igrp) - sf_grp1%istack_grp(igrp-1)
        if (num .gt.0 ) then
!
          do k2 = 1, nnod_4_surf
            call vector_phys_2_each_surface(sf_grp1, igrp, k2, i_field, &
     &          vect_sf)
            call fem_surf_skv_trq_sph_out_1(sf_grp1, igrp, k2, n_int,   &
     &          ak_d_velo, xe_sf, vect_sf, sk6)
          end do
!
        end if
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_free_slip_surf_sph_out
!
! ----------------------------------------------------------------------
!
      subroutine int_free_slip_surf_sph_in(n_int, ngrp_surf_inside,     &
     &          id_grp_inside, i_field)
!
      use m_node_phys_address
      use m_int_surface_data
!
      integer (kind = kint), intent(in) :: n_int, i_field
      integer (kind = kint), intent(in) ::ngrp_surf_inside
      integer (kind = kint), intent(in)                                 &
     &       :: id_grp_inside(ngrp_surf_inside)
!
      integer (kind = kint) :: k2, i, igrp, num
!
!
      if (ngrp_surf_inside .le. 0) return
      call reset_sk6(n_vector)
!
      do i = 1, ngrp_surf_inside
        igrp = id_grp_inside(i)
        num = sf_grp1%istack_grp(igrp) - sf_grp1%istack_grp(igrp-1)
        if (num .gt.0 ) then
!
           do k2 = 1, nnod_4_surf
            call vector_phys_2_each_surf_cst(sf_grp1, igrp, k2,         &
     &          i_field, dminus, vect_sf)
            call fem_surf_skv_trq_sph_out_1(sf_grp1, igrp, k2, n_int,   &
     &          ak_d_velo, xe_sf, vect_sf, sk6)
          end do
        end if
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_free_slip_surf_sph_in
!
! ----------------------------------------------------------------------
!
      end module int_free_slip_surf_sph
