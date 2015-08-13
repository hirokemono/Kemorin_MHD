!>@file   nodal_poynting_flux_smp.f90
!!@brief  module nodal_poynting_flux_smp
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!
!>@brief Evaluate poynting flux for nodal field
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_nod_electric_field_smp
!!      subroutine cal_nod_poynting_flux_smp
!!@endverbatim
!
      module nodal_poynting_flux_smp
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use m_physical_property
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_nod_electric_field_smp
!
      use poynting_flux_smp
!
!
      call cal_electric_field_smp                                       &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    coef_d_magne, d_nod(1,iphys%i_current),                       &
     &    d_nod(1,iphys%i_vp_induct), d_nod(1,iphys%i_electric))
!
      end subroutine cal_nod_electric_field_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_nod_poynting_flux_smp
!
      use poynting_flux_smp
!
!
      call cal_poynting_flux_smp                                        &
     &    (np_smp, node1%numnod, node1%istack_nod_smp,                  &
     &     coef_d_magne, d_nod(1,iphys%i_current),                      &
     &     d_nod(1,iphys%i_vp_induct), d_nod(1,iphys%i_magne),          &
     &     d_nod(1,iphys%i_poynting))
!
      end subroutine cal_nod_poynting_flux_smp
!
! -----------------------------------------------------------------------
!
      end module nodal_poynting_flux_smp
