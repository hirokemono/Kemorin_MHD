!m_ctl_data_surf_boundary.f90
!      module m_ctl_data_surf_boundary
!
!        programmed by H.Matsui on March, 2006
!        Modified by H.Matsui on Oct., 2007
!
!      subroutine read_bc_4_surf
!
! ------------------------------------------------------------------
!   example
!
!    begin bc_4_surface
!!!!!  boundary condition for heat flux  !!!!!!!!!!!!!!!!!!!!!!!!!!
!  available type:  fixed, file, SGS_commute
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      array heat_flux_surf  2
!        heat_flux_surf  fixed       outer  0.000  end
!        heat_flux_surf  SGS_commute inner  0.000  end
!      end array heat_flux_surf
!!!!!  boundary condition for torque  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  available type
!     fix_x,  fix_y,  fix_z
!     file_x, file_y, file_z
!     normal_velocity
!     free_shell_in, free_shell_out
!     free_4_plane
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      array velocity_surf     2
!        velocity_surf  free_shell_in inner_surf   0.000  end
!        velocity_surf  free_shell_out  outer_surf   0.000  end
!      end array velocity_surf
!!!!!  boundary condition for pressure gradiend !!!!!!!!!!!!!!!!!!!
!  available type:  inner_shell, outer_shell
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array pressure_surf  2
!         pressure_surf   inner_shell inner_surf 0.000  end
!         pressure_surf   outer_shell outer_surf 0.000  end
!      end array pressure_surf
!!!!!  boundary condition for gradientof magnetic field  !!!!!!!!!!
!     fix_x,  fix_y,  fix_z
!     insulate_in, insulate_out (not recommended)
!     far_away                  (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array magnetic_field_surf  3
!          magnetic_field_surf  insulate_in  ICB_surf  0.000 end
!          magnetic_field_surf  insulate_out CMB_surf  0.000 end
!          magnetic_field_surf  far_away infinity_surf  0.000 end
!      end array magnetic_field_surf
!!!!!  boundary condition for gradientof magnetic field  !!!!!!!!!!
!     fix_x,  fix_y,  fix_z
!     insulate_in, insulate_out (not recommended)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array vector_potential_surf  1
!          vector_potential_surf  insulate_out CMB_surf  0.000 end
!      end array vector_potential_surf
!!!!!  boundary condition for current density on surface  !!!!!!!!!!
!     fix_x,  fix_y,  fix_z
!     insulate_in,insulate_out (not recommended)
!     far_away                  (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array current_surf  3
!          current_surf  insulate_in  ICB_surf  0.000 end
!          current_surf  insulate_out CMB_surf  0.000 end
!          current_surf  far_away infinity_surf  0.000 end
!      end array current_surf
!!!!!  boundary condition for magnetic potential !!!!!!!!!!!!!!!!!
!  available type:  fixed (not used), file (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array electric_potential_surf  1
!          electric_potential_surf  insulate_in  ICB_surf  0.000 end
!          electric_potential_surf  insulate_out CMB_surf  0.000 end
!          electric_potential_surf  far_away infinity_surf  0.000 end
!      end array electric_potential_surf
!!!!!  boundary condition for dummy scalar !!!!!!!!!!!!!!!!!
!  available type:  fixed_grad (not used), file_grad (not used)
!                   fixed_field
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array composition_flux_surf  3
!          composition_flux_surf  insulate_in  ICB_surf  0.000 end
!          composition_flux_surf  insulate_out CMB_surf  0.000 end
!          composition_flux_surf  far_away infinity_surf  0.000 end
!      end array composition_flux_surf
!!!!!  boundary condition for infinity (obsolute) !!!!!!!!!!!!!!!!!
!  available type:  fixed (not used), file (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      array infinity_surf 1
!        infinity_surf  fixed infinity_surf  0.000  end
!      end array infinity_surf
!    end  bc_4_surface
!
! ------------------------------------------------------------------
!
      module m_ctl_data_surf_boundary
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_surf_boundary
!
      implicit  none
!
!
      type(surf_bc_control), save :: sbc_ctl1
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_bc_4_surf =    'bc_4_surface'
      integer (kind=kint) :: i_bc_4_surf =     0
!
      private :: hd_bc_4_surf, i_bc_4_surf
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_bc_4_surf
!
!
      call read_bc_4_surf_ctl(hd_bc_4_surf, i_bc_4_surf, sbc_ctl1)
!
      end subroutine read_bc_4_surf
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_surf_boundary
