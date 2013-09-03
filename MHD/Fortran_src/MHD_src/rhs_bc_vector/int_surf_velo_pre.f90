!
!      module int_surf_velo_pre
!
!     Written by H. Matsui on June, 2005
!
!      subroutine int_surf_velo_pre_ele
!      subroutine int_surf_velo_monitor(i_field)
!
      module int_surf_velo_pre
!
      use m_precision
!
      use m_control_parameter
      use m_surf_data_torque
!
      use int_surf_div_fluxes_sgs
      use int_surf_fixed_gradients
      use int_free_surf_sph
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_velo_pre_ele
!
      use m_node_phys_address
!
      integer (kind=kint) :: num_int
!
!
      num_int = intg_point_t_evo
!
      if (iflag_SGS_inertia  .ne. id_SGS_none) then
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          call int_surf_div_m_flux_sgs(num_int)
        end if
      end if
!
      if (iflag_SGS_lorentz .ne. id_SGS_none) then
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          call int_surf_lorentz_sgs(num_int, ifilter_final,             &
     &        iphys%i_SGS_maxwell, iphys%i_magne, iphys%i_magne)
        end if
      end if
!
!
        call int_sf_torque(num_int)
        call int_free_surf_sph_in(num_int)
        call int_free_surf_sph_out(num_int)
!
      end subroutine int_surf_velo_pre_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_velo_monitor(i_field)
!
      use m_node_phys_address
!
      integer(kind= kint), intent(in) :: i_field
!
      integer (kind=kint) :: num_int
!
!
      num_int = intg_point_t_evo
!
      if (i_field .eq. iphys%i_SGS_div_m_flux) then
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          call int_surf_div_m_flux_sgs(num_int)
        end if
      end if
!
      if (i_field .eq. iphys%i_SGS_Lorentz) then
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          call int_surf_lorentz_sgs(num_int, ifilter_final,             &
     &        iphys%i_SGS_maxwell, iphys%i_magne, iphys%i_magne)
        end if
      end if
!
!
      if (i_field .eq. iphys%i_v_diffuse) then
        call int_sf_torque(num_int)
        call int_free_surf_sph_in(num_int)
        call int_free_surf_sph_out(num_int)
      end if
!
      end subroutine int_surf_velo_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_velo_pre
