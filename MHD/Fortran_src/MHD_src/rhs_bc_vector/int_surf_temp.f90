!
!      module int_surf_temp
!
!     Written by H. Matsui on June, 2005
!
!      subroutine int_surf_temp_ele
!      subroutine int_surf_temp_monitor(i_field)
!
      module int_surf_temp
!
      use m_precision
!
      use m_control_parameter
      use m_group_data
!
      use int_surf_div_fluxes_sgs
      use int_surf_fixed_gradients
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_temp_ele
!
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      call int_sf_h_flux(sf_grp1, num_int)
!
      if (iflag_SGS_heat .ne. id_SGS_none                               &
     &     .and. iflag_commute_temp .eq. id_SGS_commute_ON) then
        call int_surf_div_h_flux_sgs(sf_grp1, num_int)
      end if
!
      end subroutine int_surf_temp_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_temp_monitor(i_field)
!
      use m_node_phys_address
!
      integer(kind= kint), intent(in) :: i_field
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      if (i_field .eq. iphys%i_t_diffuse) then
        call int_sf_h_flux(sf_grp1, num_int)
      end if
!
      if (iflag_commute_heat .eq. id_SGS_commute_ON                     &
        .and. i_field .eq. iphys%i_SGS_div_h_flux) then
        call int_surf_div_h_flux_sgs(sf_grp1, num_int)
      end if
!
      end subroutine int_surf_temp_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_temp
