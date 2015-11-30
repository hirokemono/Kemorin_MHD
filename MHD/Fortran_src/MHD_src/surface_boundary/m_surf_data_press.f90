!
!      module m_surf_data_press
!
!      Written by H. Matsui on Sep. 2005
!      Modified by H. Matsui on Feb., 2009
!
!       subroutine allocate_surf_press
!       subroutine allocate_surf_press_grad
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
      type(scaler_surf_flux_bc_type), save :: sf_bc1_grad_p
!
      type(scaler_surf_bc_data_type), save :: sf_sgs1_grad_p
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_lead_gd_p
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_wall_p
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_spin_p
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_spout_p
!
!
      real (kind=kreal), allocatable :: sf_apt_fix_pg(:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_press
!
!
      call alloc_surf_scaler_dat_type(sf_sgs1_grad_p)
      call alloc_surf_scaler_dat_type(sf_bc1_lead_gd_p)
!
      end subroutine allocate_surf_press
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_press_grad
!
!
      call alloc_surf_scaler_type(sf_bc1_grad_p)
      call alloc_surf_scaler_dat_type(sf_bc1_wall_p)
      call alloc_surf_scaler_dat_type(sf_bc1_spin_p)
      call alloc_surf_scaler_dat_type(sf_bc1_spout_p)
!
      allocate( sf_apt_fix_pg(sf_bc1_grad_p%nitem_sf_fix_fx) )
      if (sf_bc1_grad_p%nitem_sf_fix_fx .gt. 0) sf_apt_fix_pg = 0.0d0
!
      end subroutine allocate_surf_press_grad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine deallocate_surf_press
!
!
      call dealloc_surf_scaler_dat_type(sf_sgs1_grad_p)
      call dealloc_surf_scaler_dat_type(sf_bc1_lead_gd_p)
!
       end subroutine deallocate_surf_press
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_press_grad
!
!
      call dealloc_surf_scaler_type(sf_bc1_grad_p)
      call dealloc_surf_scaler_dat_type(sf_bc1_wall_p)
      call dealloc_surf_scaler_dat_type(sf_bc1_spin_p)
      call dealloc_surf_scaler_dat_type(sf_bc1_spout_p)
!
      deallocate( sf_apt_fix_pg )
!
      end subroutine deallocate_surf_press_grad
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_press
