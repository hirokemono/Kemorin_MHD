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
!!      subroutine cal_nod_electric_field_smp(node, coef_d_magne,       &
!!     &          nomp_nod, i_current, i_vp_induct, i_electric, d_nod)
!!      subroutine cal_nod_poynting_flux_smp(node, coef_d_magne,        &
!!     &          nomp_nod, i_current, i_vp_induct, i_magne, i_poynting,&
!!     &          d_nod)
!!@endverbatim
!
      module nodal_poynting_flux_smp
!
      use m_precision
!
      use m_machine_parameter
      use t_geometry_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_nod_electric_field_smp(node, coef_d_magne,         &
     &          nomp_nod, i_current, i_vp_induct, i_electric, d_nod)
!
      use poynting_flux_smp
!
      type(node_data), intent(in) :: node
      real (kind=kreal), intent(in) :: coef_d_magne
      integer(kind = kint), intent(in) :: i_current, i_vp_induct
      integer(kind = kint), intent(in) :: nomp_nod, i_electric
      real (kind=kreal), intent(inout) :: d_nod(node%numnod,nomp_nod)
!
!
      call cal_electric_field_smp                                       &
     &   (np_smp, node%numnod, node%istack_nod_smp,                     &
     &    coef_d_magne, d_nod(1,i_current), d_nod(1,i_vp_induct),       &
     &    d_nod(1,i_electric))
!
      end subroutine cal_nod_electric_field_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_nod_poynting_flux_smp(node, coef_d_magne,          &
     &          nomp_nod, i_current, i_vp_induct, i_magne, i_poynting,  &
     &          d_nod)
!
      use poynting_flux_smp
!
      type(node_data), intent(in) :: node
      real (kind=kreal), intent(in) :: coef_d_magne
      integer(kind = kint), intent(in) :: i_current, i_vp_induct, i_magne
      integer(kind = kint), intent(in) :: nomp_nod, i_poynting
      real (kind=kreal), intent(inout) :: d_nod(node%numnod,nomp_nod)
!
!
      call cal_poynting_flux_smp                                        &
     &    (np_smp, node%numnod, node%istack_nod_smp,                    &
     &     coef_d_magne, d_nod(1,i_current), d_nod(1,i_vp_induct),      &
     &     d_nod(1,i_magne), d_nod(1,i_poynting))
!
      end subroutine cal_nod_poynting_flux_smp
!
! -----------------------------------------------------------------------
!
      end module nodal_poynting_flux_smp
