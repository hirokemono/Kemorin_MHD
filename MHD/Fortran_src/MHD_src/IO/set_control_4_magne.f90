!
!      module set_control_4_magne
!
!        programmed by H.Matsui
!        modified by H.Matsui on Aug., 2007
!
!     subroutine s_set_control_4_magne
!
      module set_control_4_magne
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
      subroutine s_set_control_4_magne
!
      use m_machine_parameter
      use m_parallel_var_dof
      use m_control_parameter
      use m_ctl_data_node_boundary
      use m_ctl_data_surf_boundary
      use m_node_phys_address
      use m_node_group
      use m_bc_data_list
      use m_surf_data_list
      use set_surface_group_types
!
      character(len=kchara) :: tmpchara
      integer (kind = kint) :: i
!
!
      if ( iflag_t_evo_4_magne .eq. 0                                   &
     &       .and.  iflag_t_evo_4_vect_p .eq. 0 ) then
        num_bc_b = 0
        num_bc_bs = 0
      else
        num_bc_b = num_bc_b_ctl
        num_bc_bs = num_bc_grad_b_ctl
      end if
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug .gt. 0)    write(*,*) 'num_bc_b ',num_bc_b
      if (num_bc_b .gt. 0) then
!
        call allocate_nod_bc_list_magne
!
        bc_b_name      = bc_b_name_ctl
        bc_b_magnitude = bc_b_magnitude_ctl
!
        do i = 1, num_bc_b
          tmpchara = bc_b_type_ctl(i)
          if ( tmpchara .eq. 'fix_x' ) then
            ibc_b_type(i) = iflag_bc_fixed + 1
          else if ( tmpchara .eq. 'fix_y' ) then
            ibc_b_type(i) = iflag_bc_fixed + 2
          else if ( tmpchara .eq. 'fix_z' ) then
            ibc_b_type(i) = iflag_bc_fixed + 3
          else if ( tmpchara .eq. 'file_x' ) then
            ibc_b_type(i) = iflag_bc_fixed - 1
          else if ( tmpchara .eq. 'file_y' ) then
            ibc_b_type(i) = iflag_bc_fixed - 2
          else if ( tmpchara .eq. 'file_z' ) then
            ibc_b_type(i) = iflag_bc_fixed - 3
          else if ( tmpchara .eq. 'insulator' ) then
            ibc_b_type(i) = iflag_insulator
          else if ( tmpchara .eq. 'sph_to_center' ) then
            ibc_b_type(i) = iflag_sph_2_center
          else if ( tmpchara .eq. 'pseudo_vacume' ) then
            ibc_b_type(i) = iflag_pseudo_vacume
!          else if ( tmpchara .eq. 'sph' ) then
!            ibc_b_type(i) = 999
          else if ( tmpchara .eq. 'sgs_x' ) then
            ibc_b_type(i) = iflag_bc_sgs + 1
          else if ( tmpchara .eq. 'sgs_y' ) then
            ibc_b_type(i) = iflag_bc_sgs + 2
          else if ( tmpchara .eq. 'sgs_z' ) then
            ibc_b_type(i) = iflag_bc_sgs + 3
          else if ( tmpchara .eq. 'SGS_commute_x' ) then
            ibc_b_type(i) = iflag_bc_sgs_commute + 1
          else if ( tmpchara .eq. 'SGS_commute_y' ) then
            ibc_b_type(i) = iflag_bc_sgs_commute + 2
          else if ( tmpchara .eq. 'SGS_commute_z' ) then
            ibc_b_type(i) = iflag_bc_sgs_commute + 3
          end if
        end do
!
        if (iflag_debug.eq.1) then
          write(*,*)'i, ibc_b_type, bc_b_magnitude, bc_b_name'
          do i = 1, num_bc_b
            write(*,*) i, ibc_b_type(i), bc_b_magnitude(i),             &
     &                 trim(bc_b_name(i))
          end do
        end if
!
      end if
!
!
      if (iflag_debug.eq.1)  write(*,*) 'num_bc_bs ',num_bc_bs
      if (num_bc_bs .gt. 0) then
!
        call allocate_magne_surf_ctl
!
        bc_bs_name     =   bc_grad_b_name_ctl
        bc_bs_magnitude =  bc_grad_b_magnitude_ctl
!
        do i = 1, num_bc_bs
          call set_surf_group_types_vector(bc_grad_b_type_ctl(i),       &
     &       ibc_bs_type(i))
!
          if (bc_grad_b_type_ctl(i) .eq. 'insulator' ) then
            ibc_bs_type(i) = iflag_insulator
          else if (bc_grad_b_type_ctl(i) .eq. 'sph_to_center' ) then
            ibc_bs_type(i) = iflag_sph_2_center
          else if (bc_grad_b_type_ctl(i) .eq. 'pseudo_vacume' ) then
            ibc_bs_type(i) = iflag_pseudo_vacume
          end if
        end do
!
        if (iflag_debug.eq.1) then
          write(*,*) 'i, ibc_bs_type, bc_bs_magnitude, bc_bs_name'
          do i = 1, num_bc_bs
            write(*,*) i, ibc_bs_type(i), bc_bs_magnitude(i),           &
     &                 trim(bc_bs_name(i))
          end do
        end if
!
      end if
!
!
      end subroutine s_set_control_4_magne
!
! -----------------------------------------------------------------------
!
      end module set_control_4_magne
