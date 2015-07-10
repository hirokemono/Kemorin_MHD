!
!      module int_surf_poisson_walls
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine int_surf_poisson_wall(n_int, ngrp_sf, id_grp_sf,      &
!     &          i_vect)
!      subroutine int_surf_poisson_sph_in(n_int, ngrp_sf, id_grp_sf,    &
!     &          i_vect)
!      subroutine int_surf_poisson_sph_out(n_int, ngrp_sf, id_grp_sf,   &
!     &          i_vect)
!
      module int_surf_poisson_walls
!
      use m_precision
      use m_constants
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_surface_group
      use m_phys_constants
      use m_finite_element_matrix
!
      use node_phys_2_each_surface
      use fem_surf_skv_poisson_1st
      use cal_skv_to_ff_smp_1st
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_poisson_wall(n_int, ngrp_sf, id_grp_sf,       &
     &          i_vect)
!
      use m_int_surface_data
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_vect
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      call reset_sk6(n_scalar)
!
! --------- set vector at each node in an element
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp1%istack_grp(igrp) - sf_grp1%istack_grp(igrp-1)
        if (num .gt.0 ) then
!
          do k2=1, num_linear_sf
            call vector_phys_2_each_surface(igrp, k2, i_vect, vect_sf)
            call fem_surf_skv_poisson_wall_1(igrp, k2, n_int,           &
     &          vect_sf, sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_surf_poisson_wall
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_poisson_sph_in(n_int, ngrp_sf, id_grp_sf,     &
     &          i_vect)
!
      use m_int_surface_data
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_vect
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      call reset_sk6(n_scalar)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp1%istack_grp(igrp) - sf_grp1%istack_grp(igrp-1)
        if (num .gt.0 ) then
!
! -------- loop for shape function for the phsical values
          do k2=1, num_linear_sf
            call vector_phys_2_each_surf_cst(igrp, k2, i_vect,          &
     &          dminus, vect_sf)
            call fem_surf_skv_poisson_sph_out_1(igrp, k2, n_int,        &
     &          xe_sf, vect_sf, sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_surf_poisson_sph_in
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_poisson_sph_out(n_int, ngrp_sf, id_grp_sf,    &
     &          i_vect)
!
      use m_int_surface_data
!
      integer(kind = kint), intent(in) :: n_int, ngrp_sf
      integer(kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind = kint), intent(in) :: i_vect
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      call reset_sk6(n_scalar)
!
! --------- set vector at each node in an element
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp1%istack_grp(igrp) - sf_grp1%istack_grp(igrp-1)
        if (num .gt. 0) then
!
! -------- loop for shape function for the phsical values
          do k2=1, num_linear_sf
            call vector_phys_2_each_surface(igrp, k2, i_vect, vect_sf)
            call fem_surf_skv_poisson_sph_out_1(igrp, k2, n_int,        &
     &          xe_sf, vect_sf, sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_surf_poisson_sph_out
!
!-----------------------------------------------------------------------
!
      end module int_surf_poisson_walls
