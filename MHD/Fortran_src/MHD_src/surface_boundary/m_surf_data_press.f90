!
!      module m_surf_data_press
!
!      Written by H. Matsui on Sep. 2005
!      Modified by H. Matsui on Feb., 2009
!
!       subroutine deallocate_surf_press
!       subroutine deallocate_surf_press_grad
!
      module m_surf_data_press
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
!
      type(potential_surf_bc_type), save :: Psf1_bcs
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_surf_press
!
!
      call dealloc_surf_scaler_dat_type(Psf1_bcs%sgs)
      call dealloc_surf_scaler_dat_type(Psf1_bcs%grad_lead)
!
       end subroutine deallocate_surf_press
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_press_grad
!
!
      call dealloc_surf_scaler_type(Psf1_bcs%grad)
      call dealloc_surf_scaler_dat_type(Psf1_bcs%wall)
      call dealloc_surf_scaler_dat_type(Psf1_bcs%sph_in)
      call dealloc_surf_scaler_dat_type(Psf1_bcs%sph_out)
!
      end subroutine deallocate_surf_press_grad
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_press
