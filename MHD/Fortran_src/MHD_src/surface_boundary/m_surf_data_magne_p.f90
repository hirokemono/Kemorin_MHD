!
!     module m_surf_data_magne_p
!.......................................................................
!
!      Written by H. Matsui on Sep. 2005
!      Modified by H. Matsui on Feb., 2009
!
!      subroutine allocate_surf_data_magne_p
!      subroutine allocate_surf_magp_grad
!
!      subroutine deallocate_surf_data_magne_p
!      subroutine deallocate_surf_magp_grad
!
      module m_surf_data_magne_p
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
! 
      type(scaler_surf_flux_bc_type), save :: sf_bc1_grad_f
!
      type(scaler_surf_bc_data_type), save :: sf_sgs1_grad_f
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_lead_gd_f
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_wall_f
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_spin_f
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_spout_f
!
!
      real (kind=kreal), allocatable :: sf_apt_fix_mpg(:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_magne_p
!
!
      call alloc_surf_scaler_dat_type(sf_sgs1_grad_f)
      call alloc_surf_scaler_dat_type(sf_bc1_lead_gd_f)
!
      end subroutine allocate_surf_data_magne_p
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_magp_grad
!
!
      call alloc_surf_scaler_type(sf_bc1_grad_f)
      call alloc_surf_scaler_dat_type(sf_bc1_wall_f)
      call alloc_surf_scaler_dat_type(sf_bc1_spin_f)
      call alloc_surf_scaler_dat_type(sf_bc1_spout_f)
!
      allocate( sf_apt_fix_mpg(sf_bc1_grad_f%nitem_sf_fix_fx) )
      if (sf_bc1_grad_f%nitem_sf_fix_fx .gt. 0) sf_apt_fix_mpg = 0.0d0
!
       end subroutine allocate_surf_magp_grad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_magne_p
!
!
      call dealloc_surf_scaler_dat_type(sf_sgs1_grad_f)
      call dealloc_surf_scaler_dat_type(sf_bc1_lead_gd_f)
!
      end subroutine deallocate_surf_data_magne_p
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_magp_grad
!
!
      call dealloc_surf_scaler_type(sf_bc1_grad_f)
      call dealloc_surf_scaler_dat_type(sf_bc1_wall_f)
      call dealloc_surf_scaler_dat_type(sf_bc1_spin_f)
      call dealloc_surf_scaler_dat_type(sf_bc1_spout_f)
!
      deallocate( sf_apt_fix_mpg )
!
      end subroutine deallocate_surf_magp_grad
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_magne_p
