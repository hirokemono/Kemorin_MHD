!
!     module m_surf_data_temp
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_temp
!      subroutine deallocate_surf_data_temp
!
      module m_surf_data_temp
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
!
      type(scaler_surf_bc_type), save :: Tsf1_bcs
!
      type(scaler_surf_bc_type), save :: Csf1_bcs
!
      end module m_surf_data_temp
