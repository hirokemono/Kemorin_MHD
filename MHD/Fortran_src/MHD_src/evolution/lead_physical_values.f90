!>@file   lead_physical_values.f90
!!        module lead_physical_values
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate many kind of field data
!!
!!@verbatim
!!      subroutine lead_fields_by_FEM
!!@endverbatim
!
      module lead_physical_values
!
      use m_precision
!
      implicit none
!
      private :: cal_energy_fluxes
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine lead_fields_by_FEM
!
      use m_machine_parameter
      use m_t_step_parameter
!
      use update_after_evolution
      use itp_potential_on_edge
      use MHD_field_by_rotation
      use cal_helicities
      use output_viz_file_control
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
!
      if ( iflag.eq.0 ) then
        if (iflag_debug.gt.0) write(*,*) 'cal_potential_on_edge'
        call cal_potential_on_edge
!
        if (iflag_debug.gt.0) write(*,*) 'update_fields'
        call update_fields
!
        call cal_field_by_rotation
!
        if (iflag_debug.gt.0) write(*,*) 'cal_helicity'
        call cal_helicity
!
        if (iflag_debug.gt.0) write(*,*) 'cal_energy_fluxes'
        call cal_energy_fluxes
!
      end if
!
      end subroutine lead_fields_by_FEM
!
! ----------------------------------------------------------------------
!
      subroutine cal_energy_fluxes
!
      use m_machine_parameter
!
      use cal_MHD_forces_4_monitor
      use cal_sgs_4_monitor
      use cal_true_sgs_terms
!
!
      call cal_true_sgs_terms_pre
!
      call cal_sgs_terms_4_monitor
!
      call cal_fluxes_4_monitor
!
      call cal_forces_4_monitor
      call cal_diff_of_sgs_terms
!
      call cal_true_sgs_terms_post
!
      call cal_work_4_forces
!
      call cal_work_4_sgs_terms
!
      end subroutine cal_energy_fluxes
!
!  ---------------------------------------------------------------------
!
      end module lead_physical_values
