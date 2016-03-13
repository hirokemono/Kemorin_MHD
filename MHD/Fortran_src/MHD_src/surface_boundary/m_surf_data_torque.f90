!
!     module m_surf_data_torque
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_velo
!      subroutine allocate_surf_data_torque
!
!      subroutine deallocate_surf_data_velo
!      subroutine deallocate_surf_data_torque
!
      module m_surf_data_torque
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
!
      type(velocity_surf_bc_type), save :: Vsf1_bcs
!
      type(potential_surf_bc_type), save :: Psf1_bcs
!
      end module m_surf_data_torque
