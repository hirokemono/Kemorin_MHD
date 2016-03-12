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
      type(potential_surf_bc_type) :: Fsf1_bcs
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
      call alloc_surf_scaler_dat_type(Fsf1_bcs%sgs)
      call alloc_surf_scaler_dat_type(Fsf1_bcs%grad_lead)
!
      end subroutine allocate_surf_data_magne_p
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_magp_grad
!
!
      call alloc_surf_scaler_num(Fsf1_bcs%grad)
      call alloc_surf_scaler_dat_type(Fsf1_bcs%wall)
      call alloc_surf_scaler_dat_type(Fsf1_bcs%sph_in)
      call alloc_surf_scaler_dat_type(Fsf1_bcs%sph_out)
!
      call alloc_surf_scaler_apt(Fsf1_bcs%grad)
!
       end subroutine allocate_surf_magp_grad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_magne_p
!
!
      call dealloc_surf_scaler_dat_type(Fsf1_bcs%sgs)
      call dealloc_surf_scaler_dat_type(Fsf1_bcs%grad_lead)
!
      end subroutine deallocate_surf_data_magne_p
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_magp_grad
!
!
      call dealloc_surf_scaler_type(Fsf1_bcs%grad)
      call dealloc_surf_scaler_dat_type(Fsf1_bcs%wall)
      call dealloc_surf_scaler_dat_type(Fsf1_bcs%sph_in)
      call dealloc_surf_scaler_dat_type(Fsf1_bcs%sph_out)
!
      end subroutine deallocate_surf_magp_grad
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_magne_p
