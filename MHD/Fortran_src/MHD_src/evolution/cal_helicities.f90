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
      use m_geometry_data
      use m_node_phys_data
!
      use products_nodal_fields_smp
!
!
!$omp parallel
      if (iphys%i_k_heli .gt. izero) then
         call cal_phys_dot_product(node1, nod_fld1,                     &
     &       iphys%i_velo, iphys%i_vort, iphys%i_k_heli)
      end if
!
      if (iphys%i_m_heli .gt. izero) then
         call cal_phys_dot_product(node1, nod_fld1,                     &
     &       iphys%i_vecp, iphys%i_magne, iphys%i_m_heli)
      end if
!
      if (iphys%i_c_heli .gt. izero) then
         call cal_phys_dot_product(node1, nod_fld1,                     &
     &       iphys%i_magne, iphys%i_current, iphys%i_c_heli)
      end if
!
      if (iphys%i_x_heli .gt. izero) then
         call cal_phys_dot_product(node1, nod_fld1,                     &
     &       iphys%i_velo, iphys%i_magne, iphys%i_x_heli)
      end if
!$omp end parallel
!
      end subroutine cal_helicity
!
! ----------------------------------------------------------------------
!
      end module cal_helicities
