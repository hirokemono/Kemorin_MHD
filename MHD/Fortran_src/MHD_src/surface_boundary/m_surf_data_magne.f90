!
!     module m_surf_data_magne
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_magne
!      subroutine deallocate_surf_data_magne
!
      module m_surf_data_magne
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
!
      type(vector_surf_flux_bc_type), save :: sf_bc1_grad_b
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_norm_b
!
      type(vector_surf_bc_data_type), save :: sf_sgs1_grad_b
!
      type(vector_surf_bc_data_type), save :: sf_bc1_lead_b
!
!
      real (kind=kreal), allocatable :: sf_apt_fix_bn(:)
!
      real (kind=kreal), allocatable :: sf_apt_fix_grad_b(:,:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_magne
!
!
      call alloc_surf_vector_type(sf_bc1_grad_b)
      call alloc_surf_vector_dat_type(sf_sgs1_grad_b)
      call alloc_surf_scaler_type(sf_bc1_norm_b)
      call alloc_surf_vector_dat_type(sf_bc1_lead_b)
!
      allocate( sf_apt_fix_bn(sf_bc1_norm_b%nitem_sf_fix_fx) )
      if (sf_bc1_norm_b%nitem_sf_fix_fx.gt.0) sf_apt_fix_bn = 0.0d0
!
      allocate( sf_apt_fix_grad_b(sf_bc1_grad_b%nmax_ele_sf_fix_fx,3) )
      if (sf_bc1_grad_b%nmax_ele_sf_fix_fx.gt.0) then
        sf_apt_fix_grad_b = 0.0d0
      end if
!
      end subroutine allocate_surf_data_magne
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_magne
!
!
      call dealloc_surf_vector_type(sf_bc1_grad_b)
      call dealloc_surf_vector_dat_type(sf_sgs1_grad_b)
      call dealloc_surf_scaler_type(sf_bc1_norm_b)
      call dealloc_surf_vector_dat_type(sf_bc1_lead_b)
!
      deallocate( sf_apt_fix_bn )
      deallocate( sf_apt_fix_grad_b )
!
      end subroutine deallocate_surf_data_magne
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_magne
