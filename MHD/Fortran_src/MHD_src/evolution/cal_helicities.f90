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
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_helicity(node, iphys, nod_fld)
!
      use m_control_parameter
      use products_nodal_fields_smp
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
!$omp parallel
      if (iphys%i_k_heli .gt. izero) then
         call cal_phys_dot_product(node, nod_fld,                       &
     &       iphys%i_velo, iphys%i_vort, iphys%i_k_heli)
      end if
!
      if (iphys%i_m_heli .gt. izero) then
         call cal_phys_dot_product(node, nod_fld,                       &
     &       iphys%i_vecp, iphys%i_magne, iphys%i_m_heli)
      end if
!
      if (iphys%i_c_heli .gt. izero) then
         call cal_phys_dot_product(node, nod_fld,                       &
     &       iphys%i_magne, iphys%i_current, iphys%i_c_heli)
      end if
!
      if (iphys%i_x_heli .gt. izero) then
         call cal_phys_dot_product(node, nod_fld,                       &
     &       iphys%i_velo, iphys%i_magne, iphys%i_x_heli)
      end if
!$omp end parallel
!
      end subroutine cal_helicity
!
! ----------------------------------------------------------------------
!
      end module cal_helicities
