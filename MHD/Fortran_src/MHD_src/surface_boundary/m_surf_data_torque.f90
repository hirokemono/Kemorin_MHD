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
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_velo
!
!
      call dealloc_surf_vector_dat_type(Vsf1_bcs%sgs)
      call dealloc_surf_scaler_type(Vsf1_bcs%normal)
!
      end subroutine deallocate_surf_data_velo
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_torque
!
      call dealloc_surf_vector_type(Vsf1_bcs%grad)
      call dealloc_surf_vector_dat_type(Vsf1_bcs%torque_lead)
      call dealloc_surf_scaler_dat_type(Vsf1_bcs%free_sph_in)
      call dealloc_surf_scaler_dat_type(Vsf1_bcs%free_sph_out)
!
      end subroutine deallocate_surf_data_torque
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_torque
