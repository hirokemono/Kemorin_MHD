!> @file  check_read_bc_file.f90
!!      module check_read_bc_file
!!
!! @author  H. Matsui
!! @date Programmed in July, 2005
!
!> @brief Decide if boundary condition data field is read
!!
!!@verbatim
!!      integer(kind = kint) function check_read_boundary_files         &
!!     &                   (evo_B, evo_A, evo_T, evo_C, fl_prop)
!!        type(time_evolution_params), intent(in) :: evo_B, evo_A
!!        type(time_evolution_params), intent(in) :: evo_T, evo_C
!!        type(fluid_property), intent(in) :: fl_prop
!!@endverbatim
!
      module check_read_bc_file
!
      use m_precision
!
      implicit none
!
!>      ID not to read external boundary condition file
      integer (kind=kint), parameter :: id_no_boundary_file =   0
!>      ID to read external boundary condition file
      integer (kind=kint), parameter :: id_read_boundary_file = 1
!
      private :: set_serch_boundary_file_flag
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_read_boundary_files           &
     &                   (evo_B, evo_A, evo_T, evo_C, fl_prop)
!
      use calypso_mpi
      use m_bc_data_list
      use m_surf_data_list
      use t_time_stepping_parameter
      use t_physical_property
!
      type(time_evolution_params), intent(in) :: evo_B, evo_A
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(fluid_property), intent(in) :: fl_prop
!
      integer(kind = kint) :: iflag_boundary_file
!
      iflag_boundary_file = 0
!
! ----  read boundary data for temperature
!
      if ( evo_T%iflag_scheme .gt. id_no_evolution) then
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      temp_nod%num_bc, temp_nod%ibc_type)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      h_flux_surf%num_bc, h_flux_surf%ibc_type)
      end if
!
! ----  read boundary data for velocity
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      velo_nod%num_bc, velo_nod%ibc_type)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      torque_surf%num_bc, torque_surf%ibc_type)
!
!  set boundary conditions for pressure
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      press_nod%num_bc, press_nod%ibc_type)
      end if
!
! ----  read boundary data for dummy scalar
!
      if ( evo_C%iflag_scheme .gt. id_no_evolution) then
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      light_nod%num_bc, light_nod%ibc_type)
      end if
!
! ----  read boundary data for magnetic field
!
      if (    evo_B%iflag_scheme .gt. id_no_evolution                   &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      magne_nod%num_bc, magne_nod%ibc_type)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      magne_surf%num_bc, magne_surf%ibc_type)
!
! ----  read boundary data for magnetic potential
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      e_potential_nod%num_bc, e_potential_nod%ibc_type)
      end if
!
! ----  read boundary data for vector potential
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      a_potential_nod%num_bc, a_potential_nod%ibc_type)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      a_potential_surf%num_bc, a_potential_surf%ibc_type)
!
! ----  read boundary data for magnetic potential
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      e_potential_nod%num_bc, e_potential_nod%ibc_type)
      end if
!
! ----  read boundary data for current density
!
      if (     evo_B%iflag_scheme .gt. id_no_evolution                  &
     &    .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      current_nod%num_bc, current_nod%ibc_type)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      current_surf%num_bc, current_surf%ibc_type)
      end if
!
      check_read_boundary_files = iflag_boundary_file
!
      end function check_read_boundary_files
!
! -----------------------------------------------------------------------
!
       subroutine set_serch_boundary_file_flag(iflag, num_bc, ibc_type)
!
       integer (kind = kint), intent(inout) :: iflag
       integer (kind = kint), intent(in) :: num_bc
       integer (kind = kint), intent(in) :: ibc_type(num_bc)
!
       integer(kind = kint) :: i
!
       if (iflag .eq. 0 ) then
         if (num_bc .gt. 0) then
           do i = 1, num_bc
            if (ibc_type(i) .lt. 0) iflag = 1
           end do
         end if
       end if
!
       end subroutine set_serch_boundary_file_flag
!
! -----------------------------------------------------------------------
!
      end module check_read_bc_file
