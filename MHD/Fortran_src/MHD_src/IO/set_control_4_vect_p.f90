!>@file   set_control_4_vect_p.f90
!!@brief  module set_control_4_vect_p
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui in 2002
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set boundary conditions for magnetic vector potential
!!        from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_vect_p
!!@endverbatim
!
      module set_control_4_vect_p
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_vect_p
!
      use m_machine_parameter
      use calypso_mpi
      use m_control_parameter
      use m_ctl_data_node_boundary
      use m_ctl_data_surf_boundary
      use m_node_phys_address
      use m_node_group
      use m_bc_data_list
      use m_surf_data_list
      use set_node_group_types
      use set_surface_group_types
      use skip_comment_f
!
      integer (kind = kint) :: i
!
!
      if (iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        a_potential_nod%num_bc =  0
        a_potential_surf%num_bc = 0
      else
        a_potential_nod%num_bc =  node_bc_A_ctl%num
        a_potential_surf%num_bc = surf_bc_AN_ctl%num
      end if
!
!   set boundary_conditons for magnetic field
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'a_potential_nod%num_bc ',a_potential_nod%num_bc
      if (a_potential_nod%num_bc .gt. 0) then
!
        call allocate_nod_bc_list_vecp
!
        a_potential_nod%bc_name(1:a_potential_nod%num_bc)               &
     &     = node_bc_A_ctl%c2_tbl(1:a_potential_nod%num_bc)
        a_potential_nod%bc_magnitude(1:a_potential_nod%num_bc)          &
     &     = node_bc_A_ctl%vect(1:a_potential_nod%num_bc)
!
        do i = 1, a_potential_nod%num_bc
         call set_bc_group_types_vector(node_bc_A_ctl%c1_tbl(i),        &
     &       a_potential_nod%ibc_type(i))
         call set_bc_group_types_sgs_vect(node_bc_A_ctl%c1_tbl(i),      &
     &       a_potential_nod%ibc_type(i))
!
          if(cmp_no_case(node_bc_A_ctl%c1_tbl(i), 'insulate_shell' )    &
     &       ) a_potential_nod%ibc_type(i) = iflag_insulator
!          if (cmp_no_case(node_bc_A_ctl%c1_tbl(i), 'sph'               &
!     &       )  a_potential_nod%ibc_type(i) = 999
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, a_potential_nod'
          do i = 1, a_potential_nod%num_bc
            write(*,*)  i, a_potential_nod%ibc_type(i),                 &
     &                  a_potential_nod%bc_magnitude(i),                &
     &                  trim(a_potential_nod%bc_name(i))
          end do
        end if
!
        call deallocate_bc_vect_p_ctl
      end if
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &    write(*,*) 'a_potential_surf%num_bc ',a_potential_surf%num_bc
      if (a_potential_surf%num_bc .gt. 0) then
!
        call allocate_vect_p_surf_ctl
!
        a_potential_surf%bc_name(1:a_potential_surf%num_bc)             &
     &          = surf_bc_AN_ctl%c2_tbl(1:a_potential_surf%num_bc)
        a_potential_surf%bc_magnitude(1:a_potential_surf%num_bc)        &
     &          = surf_bc_AN_ctl%vect(1:a_potential_surf%num_bc)
!
        do i = 1, a_potential_surf%num_bc
          call set_surf_group_types_vector(surf_bc_AN_ctl%c1_tbl(i),    &
     &        a_potential_surf%ibc_type(i))
          call set_pseudo_vacuum_group_types(surf_bc_AN_ctl%c1_tbl(i),  &
     &        a_potential_surf%ibc_type(i))
        end do
!
        if (iflag_debug .eq. iflag_full_msg) then
          write(*,*) 'i, a_potential_surf'
          do i = 1, a_potential_surf%num_bc
            write(*,*)  i, a_potential_surf%ibc_type(i),                &
     &                     a_potential_surf%bc_magnitude(i),            &
     &                     trim(a_potential_surf%bc_name(i))
          end do
        end if
!
        call deallocate_bc_vecp_sf_ctl
      end if
!
      end subroutine s_set_control_4_vect_p
!
! -----------------------------------------------------------------------
!
      end module set_control_4_vect_p
