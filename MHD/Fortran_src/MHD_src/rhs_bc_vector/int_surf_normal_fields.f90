!
!      module int_surf_normal_fields
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine int_surf_normal_velocity
!      subroutine int_surf_normal_vector_p
!      subroutine int_surf_normal_magne
!
      module int_surf_normal_fields
!
      use m_precision
!
      use m_control_parameter
      use int_surf_poisson_walls
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_normal_velocity
!
      use m_node_phys_address
      use m_surf_data_press
!
      integer(kind = kint) :: n_int
!
!
      n_int = intg_point_poisson
!
      if ( ngrp_sf_wall_p .gt. 0) then
        call int_surf_poisson_wall(n_int, ngrp_sf_wall_p,               &
     &      id_grp_sf_wall_p, iphys%i_velo)
      end if
!
      if ( ngrp_sf_spin_p .gt. 0) then
        call int_surf_poisson_sph_in(n_int, ngrp_sf_spin_p,             &
     &      id_grp_sf_spin_p, iphys%i_velo)
      end if
!
      if ( ngrp_sf_spout_p .gt. 0) then
        call int_surf_poisson_sph_out(n_int, ngrp_sf_spout_p,           &
     &      id_grp_sf_spout_p, iphys%i_velo)
      end if
!
      end subroutine int_surf_normal_velocity
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_normal_vector_p
!
      use m_node_phys_address
      use m_surf_data_magne_p
!
      integer(kind = kint) :: n_int
!
!
      n_int = intg_point_poisson
!
      if ( ngrp_sf_wall_mp .gt. 0) then
        call int_surf_poisson_wall(n_int, ngrp_sf_wall_mp,              &
     &      id_grp_sf_wall_mp, iphys%i_vecp)
      end if
!
      if ( ngrp_sf_spin_mp .gt. 0) then
        call int_surf_poisson_sph_in(n_int, ngrp_sf_spin_mp,            &
     &      id_grp_sf_spin_mp, iphys%i_vecp)
      end if
!
      if ( ngrp_sf_spout_mp .gt. 0) then
        call int_surf_poisson_sph_out(n_int, ngrp_sf_spout_mp,          &
     &      id_grp_sf_spout_mp, iphys%i_vecp)
      end if
!
      end subroutine int_surf_normal_vector_p
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_normal_magne
!
      use m_node_phys_address
      use m_surf_data_magne_p
!
      integer(kind = kint) :: n_int
!
!
      n_int = intg_point_poisson
!
      if ( ngrp_sf_wall_mp .gt. 0) then
        call int_surf_poisson_wall(n_int, ngrp_sf_wall_mp,              &
     &      id_grp_sf_wall_mp, iphys%i_magne)
      end if
!
      if ( ngrp_sf_spin_mp .gt. 0) then
        call int_surf_poisson_sph_in(n_int, ngrp_sf_spin_mp,            &
     &      id_grp_sf_spin_mp, iphys%i_magne)
      end if
!
      if ( ngrp_sf_spout_mp .gt. 0) then
        call int_surf_poisson_sph_out(n_int, ngrp_sf_spout_mp,          &
     &      id_grp_sf_spout_mp, iphys%i_magne)
      end if
!
      end subroutine int_surf_normal_magne
!
!-----------------------------------------------------------------------
!
      end module int_surf_normal_fields
