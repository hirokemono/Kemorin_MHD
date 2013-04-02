!cal_helicities.f90
!      module cal_helicities
!
!        programmed by H.Matsui
!      Modified by H. Matsui on Sep., 2007
!
!      subroutine cal_helicity
!
      module cal_helicities
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_helicity
!
      use m_control_parameter
      use m_node_phys_address
      use m_node_phys_data
!
      use products_nodal_fields
!
      integer(kind = kint) :: i
!
      do i = 1, num_nod_phys
!
        if ( phys_nod_name(i) .eq. 'kinetic_helicity' ) then
         call cal_phys_dot_product(iphys%i_k_heli,                      &
     &       iphys%i_velo, iphys%i_vort)
        else if ( phys_nod_name(i) .eq. 'magnetic_helicity' ) then
         call cal_phys_dot_product(iphys%i_m_heli,                      &
     &       iphys%i_vecp, iphys%i_magne)
        else if ( phys_nod_name(i) .eq. 'current_helicity' ) then
         call cal_phys_dot_product(iphys%i_c_heli,                      &
     &       iphys%i_magne, iphys%i_current)
        else if ( phys_nod_name(i) .eq. 'cross_helicity' ) then
         call cal_phys_dot_product(iphys%i_x_heli,                      &
     &       iphys%i_velo, iphys%i_magne)
        end if
!
      end do
!
      end subroutine cal_helicity
!
! ----------------------------------------------------------------------
!
      end module cal_helicities
