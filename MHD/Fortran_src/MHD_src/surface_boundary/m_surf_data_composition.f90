!m_surf_data_composition.f90
!     module m_surf_data_composition
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_composit
!      subroutine deallocate_surf_data_composit
!
      module m_surf_data_composition
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
      type(scaler_surf_bc_type) :: Csf1_bcs
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_composit
!
!
      call alloc_surf_scaler_num(Csf1_bcs%flux)
      call alloc_surf_scaler_dat_type(Csf1_bcs%sgs)
      call alloc_surf_scaler_dat_type(Csf1_bcs%flux_lead)
!
      call alloc_surf_scaler_apt(Csf1_bcs%flux)
!
      end subroutine allocate_surf_data_composit
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_composit
!
!
      call dealloc_surf_scaler_type(Csf1_bcs%flux)
      call dealloc_surf_scaler_dat_type(Csf1_bcs%sgs)
      call dealloc_surf_scaler_dat_type(Csf1_bcs%flux_lead)
!
      end subroutine deallocate_surf_data_composit
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_composition
