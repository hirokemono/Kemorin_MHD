!
!     module m_surf_data_current
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_current
!      subroutine deallocate_surf_data_current
!
      module m_surf_data_current
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
      type(vector_surf_flux_bc_type), save :: sf_bc1_grad_j
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_norm_j
!
      type(vector_surf_bc_data_type), save :: sf_sgs1_grad_j
!
      type(vector_surf_bc_data_type), save :: sf_bc1_lead_j
!
      real (kind=kreal), allocatable :: sf_apt_fix_jn(:)
!
      real (kind=kreal), allocatable :: sf_apt_fix_grad_j(:,:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_current
!
!
      call alloc_surf_vector_type(sf_bc1_grad_j)
      call alloc_surf_scaler_type(sf_bc1_norm_j)
      call alloc_surf_vector_dat_type(sf_sgs1_grad_j)
      call alloc_surf_vector_dat_type(sf_bc1_lead_j)
!
      allocate( sf_apt_fix_jn(sf_bc1_norm_j%nitem_sf_fix_fx) )
      if (sf_bc1_norm_j%nitem_sf_fix_fx.gt.0) sf_apt_fix_jn = 0.0d0
!
      allocate( sf_apt_fix_grad_j(sf_bc1_grad_j%nmax_ele_sf_fix_fx,3) )
      if (sf_bc1_grad_j%nmax_ele_sf_fix_fx.gt.0) then
        sf_apt_fix_grad_j = 0.0d0
      end if
!
      end subroutine allocate_surf_data_current
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_current
!
!
      call dealloc_surf_vector_type(sf_bc1_grad_j)
      call dealloc_surf_scaler_type(sf_bc1_norm_j)
      call dealloc_surf_vector_dat_type(sf_sgs1_grad_j)
      call dealloc_surf_vector_dat_type(sf_bc1_lead_j)
!
      deallocate( sf_apt_fix_jn )
      deallocate( sf_apt_fix_grad_j )
!
      end subroutine deallocate_surf_data_current
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_current
