!
!      module MHD_field_by_rotation
!
!        programmed by H.Matsui
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine cal_field_by_rotation
!
      module MHD_field_by_rotation
!
      use m_precision
!
      implicit none
!
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
!       use cal_current_by_vecp
!
       integer(kind = kint) :: i
!
!
       do i = 1, num_nod_phys
!
        if ( phys_nod_name(i).eq.'vorticity'                            &
     &    .and. iflag_nod_update(iphys%i_vort).eq.0 ) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_vorticity'
          call cal_vorticity
        else if ( phys_nod_name(i) .eq. 'current_density'               &
     &    .and. iflag_nod_update(iphys%i_current) .eq.0 ) then
          if (iflag_t_evo_4_magne .ge.1 ) then
            if (iflag_debug.eq.1) write(*,*) 'cal_current_density'
            call cal_current_density
          else if (iflag_t_evo_4_vect_p .ge.1 ) then
!
!            if (iflag_debug.eq.1)  write(*,*) 'int_current_diffuse'
!            call int_current_diffuse
            if (iflag_debug.eq.1) write(*,*) 'cal_current_density'
            call cal_current_density
!
          end if
        end if
!
       end do
!
!
      end subroutine cal_field_by_rotation
!
! ----------------------------------------------------------------------
!
      end module MHD_field_by_rotation
