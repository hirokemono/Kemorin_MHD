!int_free_surf_sph.f90
!      module int_free_surf_sph
!
!      Written  by H. Matsui on Dec., 2003
!      modified by H. Matsui on Aug., 2005
!
!      subroutine int_free_surf_sph_in(n_int)
!      subroutine int_free_surf_sph_out(n_int)
!
      module int_free_surf_sph
!
      use m_precision
      use m_constants
!
      use m_geometry_parameter
      use m_surface_group
      use m_int_vol_data
      use m_finite_element_matrix
      use m_surf_data_torque
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
      subroutine int_free_surf_sph_out(n_int)
!
      use m_node_phys_address
      use m_int_surface_data
!
      integer (kind = kint), intent(in) :: n_int
!
      integer (kind = kint) :: k2, i, igrp
!
!
      if ( ngrp_sf_fr_out .le. 0) return
      call reset_sk6(n_vector)
!
      do i = 1, ngrp_sf_fr_out
        igrp = id_grp_sf_fr_out(i)
        if ((surf_istack(igrp) - surf_istack(igrp-1)) .gt.0 ) then
!
          do k2 = 1, nnod_4_surf
            call vector_phys_2_each_surface(igrp, k2, iphys%i_velo,     &
     &          vect_sf)
            call fem_surf_skv_trq_sph_out_1(igrp, k2, n_int,            &
     &          ak_d_velo, xe_sf, vect_sf, sk6)
          end do
!
        end if
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_free_surf_sph_out
!
! ----------------------------------------------------------------------
!
      subroutine int_free_surf_sph_in(n_int)
!
      use m_node_phys_address
      use m_int_surface_data
!
      integer (kind = kint), intent(in) :: n_int
!
      integer (kind = kint) :: k2, i, igrp
!
!
      if (ngrp_sf_fr_in .le. 0) return
      call reset_sk6(n_vector)
!
      do i = 1, ngrp_sf_fr_in
        igrp = id_grp_sf_fr_in(i)
        if ((surf_istack(igrp) - surf_istack(igrp-1)) .gt.0 ) then
!
           do k2 = 1, nnod_4_surf
            call vector_phys_2_each_surf_cst(igrp, k2, iphys%i_velo,    &
     &          dminus, vect_sf)
            call fem_surf_skv_trq_sph_out_1(igrp, k2, n_int,            &
     &          ak_d_velo, xe_sf, vect_sf, sk6)
          end do
        end if
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_free_surf_sph_in
!
! ----------------------------------------------------------------------
!
      end module int_free_surf_sph
