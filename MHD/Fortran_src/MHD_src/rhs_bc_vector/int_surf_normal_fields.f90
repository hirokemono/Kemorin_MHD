!
!      module int_surf_normal_fields
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine int_surf_normal_velocity                              &
!     &         (ele, surf, sf_grp, jac_sf_grp_l)
!      subroutine int_surf_normal_vector_p                              &
!     &         (ele, surf, sf_grp, jac_sf_grp_l)
!      subroutine int_surf_normal_magne                                 &
!     &         (ele, surf, sf_grp, jac_sf_grp_l)
!
      module int_surf_normal_fields
!
      use m_precision
!
      use m_control_parameter
      use t_group_data
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
      subroutine int_surf_normal_velocity                               &
     &         (ele, surf, sf_grp, jac_sf_grp_l)
!
      use m_node_phys_address
      use m_surf_data_press
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
!
!
      if ( ngrp_sf_wall_p .gt. 0) then
        call int_surf_poisson_wall(ele, surf, sf_grp, jac_sf_grp_l,     &
     &      intg_point_poisson, ngrp_sf_wall_p, id_grp_sf_wall_p,       &
     &      iphys%i_velo)
      end if
!
      if ( ngrp_sf_spin_p .gt. 0) then
        call int_surf_poisson_sph_in(ele, surf, sf_grp, jac_sf_grp_l,   &
     &      intg_point_poisson, ngrp_sf_spin_p, id_grp_sf_spin_p,       &
     &      iphys%i_velo)
      end if
!
      if ( ngrp_sf_spout_p .gt. 0) then
        call int_surf_poisson_sph_out(ele, surf, sf_grp, jac_sf_grp_l,  &
     &      intg_point_poisson, ngrp_sf_spout_p, id_grp_sf_spout_p,     &
     &      iphys%i_velo)
      end if
!
      end subroutine int_surf_normal_velocity
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_normal_vector_p                               &
     &         (ele, surf, sf_grp, jac_sf_grp_l)
!
      use m_node_phys_address
      use m_surf_data_magne_p
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
!
!
      if ( ngrp_sf_wall_mp .gt. 0) then
        call int_surf_poisson_wall(ele, surf, sf_grp, jac_sf_grp_l,     &
     &      intg_point_poisson, ngrp_sf_wall_mp, id_grp_sf_wall_mp,     &
     &      iphys%i_vecp)
      end if
!
      if ( ngrp_sf_spin_mp .gt. 0) then
        call int_surf_poisson_sph_in(ele, surf, sf_grp, jac_sf_grp_l,   &
     &      intg_point_poisson, ngrp_sf_spin_mp, id_grp_sf_spin_mp,     &
     &      iphys%i_vecp)
      end if
!
      if ( ngrp_sf_spout_mp .gt. 0) then
        call int_surf_poisson_sph_out(ele, surf, sf_grp, jac_sf_grp_l,  &
     &      intg_point_poisson, ngrp_sf_spout_mp, id_grp_sf_spout_mp,   &
     &      iphys%i_vecp)
      end if
!
      end subroutine int_surf_normal_vector_p
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_normal_magne                                  &
     &         (ele, surf, sf_grp, jac_sf_grp_l)
!
      use m_node_phys_address
      use m_surf_data_magne_p
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
!
!
      if ( ngrp_sf_wall_mp .gt. 0) then
        call int_surf_poisson_wall(ele, surf, sf_grp, jac_sf_grp_l,     &
     &      intg_point_poisson, ngrp_sf_wall_mp, id_grp_sf_wall_mp,     &
     &      iphys%i_magne)
      end if
!
      if ( ngrp_sf_spin_mp .gt. 0) then
        call int_surf_poisson_sph_in(ele, surf, sf_grp, jac_sf_grp_l,   &
     &      intg_point_poisson, ngrp_sf_spin_mp, id_grp_sf_spin_mp,     &
     &      iphys%i_magne)
      end if
!
      if ( ngrp_sf_spout_mp .gt. 0) then
        call int_surf_poisson_sph_out(ele, surf, sf_grp, jac_sf_grp_l,  &
     &      intg_point_poisson, ngrp_sf_spout_mp, id_grp_sf_spout_mp,   &
     &      iphys%i_magne)
      end if
!
      end subroutine int_surf_normal_magne
!
!-----------------------------------------------------------------------
!
      end module int_surf_normal_fields
