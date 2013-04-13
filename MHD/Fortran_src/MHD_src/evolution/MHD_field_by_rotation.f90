!>@file   MHD_field_by_rotation.f90
!!@brief  module MHD_field_by_rotation
!!
!!@author H. Matsui
!!@date Programmed...when??
!
!>@brief Evaluate vorticity and current density
!!
!!@verbatim
!!      subroutine cal_field_by_rotation
!!@endverbatim
!
      module MHD_field_by_rotation
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_field_by_rotation
!
      use m_machine_parameter
      use m_control_parameter
      use m_node_phys_address
      use m_node_phys_data
!
      use cal_rotation_fields
!
!
      if(iphys%i_vort .gt. izero)then
        if(iflag_nod_update(iphys%i_vort) .eq. izero) then
          call cal_vorticity
        end if
      end if
!
      if(iphys%i_current .gt. izero)then
        if(iflag_nod_update(iphys%i_current) .eq.0 ) then
          if(iflag_t_evo_4_vect_p .ge.1 ) then
            if (iflag_debug .ge. iflag_routine_msg)                     &
     &        write(*,*) 'cal_current_density'
            call cal_current_density
!            call int_current_diffuse
          else
            if (iflag_debug .ge. iflag_routine_msg)                     &
     &        write(*,*) 'cal_current_density'
            call cal_current_density
          end if
        end if
      end if
!
      end subroutine cal_field_by_rotation
!
! ----------------------------------------------------------------------
!
      end module MHD_field_by_rotation
