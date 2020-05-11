!>@file   m_force_control_labels.f90
!!        module m_force_control_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Force labels
!!
!!@verbatim
!!      integer(kind = kint) function num_advection_controls()
!!      integer(kind = kint) function num_force_controls()
!!      subroutine set_advection_control_labels(n_comps, names, maths)
!!      subroutine set_force_control_labels(n_comps, names, maths)
!!
!! !!!!! Force names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!   Advection names  (Other possible expression)
!!
!!     inertia
!!     magnetic_induction   (induction)
!!     heat_advect          (heat)
!!     composition_advect   (comp_flux)
!!
!!   Force names
!!
!!     Coriolis_force       (Coriolis)
!!     Lorentz_force        (Lorentz)
!!
!!     buoyancy             (Thermal_buoyancy, Thermal_gravity, gravity)
!!     composite_buoyancy   (Compositional_buoyancy, composite_gravity
!!                           compositional_gravity)
!!
!!     filtered_buoyancy      (Filtered_gravity)
!!     filtered_comp_buoyancy (Filtered_compositional_gravity)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_force_control_labels
!
      use m_precision
!
      use t_base_force_labels
      use m_filtered_force_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nadvect_label = 4
      integer(kind = kint), parameter, private :: nforce_label =  7
!
!       Opthional names of force control labels
!
!>        Term label for induction term
      character(len=kchara), parameter :: induction_1 =  'induction'
!>        Term label for heat equation
      character(len=kchara), parameter :: heat_flux_1 =  'heat'
!>        Term label for cpmpositional flux term
      character(len=kchara), parameter :: comp_flux_1 =  'comp_flux'
!
!>       Coriolis force label
      character(len=kchara), parameter :: coriolis_e1 = 'Coriolis'
      character(len=kchara), parameter :: coriolis_n1 = 'Coriolis_node'
      character(len=kchara), parameter :: coriolis_i1 = 'Coriolis_imp'
!
!>       Lorentz force label
      character(len=kchara), parameter :: lorentz_e1 = 'Lorentz'
      character(len=kchara), parameter :: lorentz_f1 = 'Lorentz_full'
!
!>       Thermal buoyancy label
      character(len=kchara), parameter :: gravity_e1 =  'gravity'
      character(len=kchara), parameter :: gravity_e2 = 'Gravity_ele'
      character(len=kchara), parameter                                  &
     &             :: gravity_e3 = 'Gravity_element'
      character(len=kchara), parameter :: gravity_e4 = 'Buoyancy_ele'
      character(len=kchara), parameter                                  &
     &             :: gravity_e5 = 'Buoyancy_element'
      character(len=kchara), parameter                                  &
     &             :: gravity_e6 = 'Thermal_buoyancy'
      character(len=kchara), parameter                                  &
     &             :: gravity_e7 = 'Thermal_buoyancy_ele'
      character(len=kchara), parameter                                  &
     &             :: gravity_e8 = 'Thermal_buoyancy_element'
      character(len=kchara), parameter                                  &
     &             :: gravity_e9 = 'Thermal_gravity'
      character(len=kchara), parameter                                  &
     &             :: gravity_e10 = 'Thermal_gravity_ele'
      character(len=kchara), parameter                                  &
     &             :: gravity_e11 = 'Thermal_gravity_element'
!
      character(len=kchara), parameter :: gravity_n1 = 'Gravity_nod'
      character(len=kchara), parameter :: gravity_n2 = 'Buoyancy_nod'
      character(len=kchara), parameter                                  &
     &             :: gravity_n3 = 'Thermal_buoyancy_nod'
      character(len=kchara), parameter                                  &
     &             :: gravity_n4 = 'Thermal_gravity_nod'
      character(len=kchara), parameter :: gravity_n5 = 'Gravity_node'
      character(len=kchara), parameter :: gravity_n6 = 'Buoyancy_node'
      character(len=kchara), parameter                                  &
     &             :: gravity_n7 = 'Thermal_buoyancy_node'
      character(len=kchara), parameter                                  &
     &             :: gravity_n8 = 'Thermal_gravity_node'
!
!
!>       Compositional buoyancy label
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e1 = 'compositional_gravity'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e2 = 'Compositional_buoyancy'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e3 = 'Composite_buoyancy_ele'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e4 = 'Composite_buoyancy_element'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e5 = 'composite_gravity'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e6 = 'Composite_gravity_ele'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e7 = 'Composite_gravity_element'
!
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_n1 = 'Compositional_buoyancy_nod'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_n2 = 'Compositional_gravity_nod'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_n3 = 'Composite_buoyancy_nod'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_n4 = 'Composite_gravity_nod'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_n5 = 'Composite_buoyancy_node'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_n6 = 'Composite_gravity_node'
!
!>       Filtered thermal buoyancy label
      character(len=kchara), parameter                                  &
     &             :: Filtered_gravity_e1 = 'Filtered_gravity'
!
!>       Filtered compositional buoyancy label
      character(len=kchara), parameter                                  &
     &             :: Filtered_comp_gravity_e1                          &
     &                        = 'Filtered_compositional_gravity'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function num_advection_controls()
      num_advection_controls = nadvect_label
      return
      end function num_advection_controls
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_force_controls()
      num_force_controls = nforce_label
      return
      end function num_force_controls
!
! ----------------------------------------------------------------------
!
      subroutine set_advection_control_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nadvect_label)
      character(len = kchara), intent(inout) :: names(nadvect_label)
      character(len = kchara), intent(inout) :: maths(nadvect_label)
!
!
      call set_field_labels(inertia,                                    &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(magnetic_induction,                         &
     &    n_comps( 2), names( 2), maths( 2))
!
      call set_field_labels(heat_advect,                                &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(composition_advect,                         &
     &    n_comps( 4), names( 4), maths( 4))
!
      end subroutine set_advection_control_labels
!
! ----------------------------------------------------------------------
!
      subroutine set_force_control_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nforce_label)
      character(len = kchara), intent(inout) :: names(nforce_label)
      character(len = kchara), intent(inout) :: maths(nforce_label)
!
!
      call set_field_labels(Coriolis_force,                             &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(Lorentz_force,                              &
     &    n_comps( 2), names( 2), maths( 2))
!
      call set_field_labels(buoyancy,                                   &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(composite_buoyancy,                         &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(Lorentz_force_by_filtered,                  &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(filtered_buoyancy,                          &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(filtered_comp_buoyancy,                     &
     &    n_comps( 7), names( 7), maths( 7))
!
      end subroutine set_force_control_labels
!
! ----------------------------------------------------------------------
!
      end module m_force_control_labels
