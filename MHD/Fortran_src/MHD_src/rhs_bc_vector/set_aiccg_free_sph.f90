!set_aiccg_free_sph.f90
!      module set_aiccg_free_sph
!
!      stress free boundary in a spherical shell
!     Written by H. Matsui on Sep. 2005
!
!      subroutine set_aiccg_bc_free_sphere
!
      module set_aiccg_free_sph
!
      use m_precision
!
      implicit none
!
      private :: set_aiccg_bc_free_sph_in, set_aiccg_bc_free_sph_out
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_free_sphere
!
      use m_control_parameter
      use m_surf_data_torque
!
      integer (kind = kint)  :: num_int
!
!
      num_int = intg_point_poisson
      if(ngrp_sf_fr_in .gt. 0)  call set_aiccg_bc_free_sph_in(num_int)
      if(ngrp_sf_fr_out .gt. 0) call set_aiccg_bc_free_sph_out(num_int)
!
      end subroutine set_aiccg_bc_free_sphere
!
!-----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_free_sph_in(num_int)
!
      use m_geometry_parameter
      use m_surface_group
      use m_node_phys_address
      use m_ele_material_property
      use m_int_surface_data
      use m_jacobians_2d
      use m_finite_element_matrix
      use m_sorted_node_MHD
      use m_surf_data_torque
      use m_velo_matrix
!
      use fem_surf_crank_free_sph
      use cal_poisson_matrices_1st
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint) :: i, igrp
      integer (kind = kint) :: k2
!
!
      do k2 = 1, nnod_4_surf
        call reset_sk6(n_scalar)
!
        do i = 1, ngrp_sf_fr_in
          igrp = id_grp_sf_fr_in(i)
!
          if ( (surf_istack(igrp) - surf_istack(igrp-1)) .gt. 0) then
            call fem_surf_crank_free_inside(igrp, k2, num_int,          &
     &          numele, nnod_4_ele, nnod_4_surf, node_on_sf,            &
     &          num_surf_bc, num_surf_smp, isurf_grp_smp_stack,         &
     &          surf_item, ntot_int_sf_grp, aw_sf, xjq_sf, xe_sf,       &
     &          ak_d_velo, sk6)
!
            call add_skv1_2_MHD_matrix33(idx_4_fl_mat, k2, sk6,         &
     &          num_velo_comp, aiccg_velo)
          end if
        end do
      end do
!
      end subroutine set_aiccg_bc_free_sph_in
!
!-----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_free_sph_out(num_int)
!
      use m_geometry_parameter
      use m_surface_group
      use m_node_phys_address
      use m_ele_material_property
      use m_int_surface_data
      use m_jacobians_2d
      use m_finite_element_matrix
      use m_sorted_node_MHD
      use m_surf_data_torque
      use m_velo_matrix
!
      use fem_surf_crank_free_sph
      use cal_poisson_matrices_1st
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint) :: i, igrp
      integer (kind = kint) :: k2
!
!
      do k2 = 1, nnod_4_surf
        call reset_sk6(n_scalar)
!
        do i = 1, ngrp_sf_fr_out
          igrp = id_grp_sf_fr_out(i)
!
          if ( (surf_istack(igrp) - surf_istack(igrp-1)) .gt. 0) then
            call fem_surf_crank_free_outside(igrp, k2, num_int,         &
     &          numele, nnod_4_ele, nnod_4_surf, node_on_sf,            &
     &          num_surf_bc, num_surf_smp, isurf_grp_smp_stack,         &
     &          surf_item, ntot_int_sf_grp, aw_sf, xjq_sf, xe_sf,       &
     &          ak_d_velo, sk6)
!
            call add_skv1_2_MHD_matrix33(idx_4_fl_mat, k2, sk6,         &
     &          num_velo_comp, aiccg_velo)
          end if
        end do
      end do
!
      end subroutine set_aiccg_bc_free_sph_out
!
!-----------------------------------------------------------------------
!
      end module set_aiccg_free_sph
