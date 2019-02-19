!>@file   t_surface_bc_data.f90
!!@brief  module t_surface_bc_data
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>    @brief flux boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!      subroutine alloc_surf_data_velo(Vsf_bcs)
!!      subroutine dealloc_surf_data_velo(Vsf_bcs)
!!        type(velocity_surf_bc_type),  intent(inout) :: Vsf_bcs
!!      subroutine alloc_surf_vector(Bsf_bcs)
!!      subroutine dealloc_surf_vector(Bsf_bcs)
!!        type(velocity_surf_bc_type),  intent(inout) :: Bsf_bcs
!!      subroutine alloc_surf_potential(Fsf_bcs)
!!      subroutine dealloc_surf_potential(Fsf_bcs)
!!        type(potential_surf_bc_type), intent(inout) :: Fsf_bcs
!!      subroutine alloc_surf_data_scalar(Ssf_bcs)
!!      subroutine dealloc_surf_data_scalar(Ssf_bcs)
!!        type(scaler_surf_bc_type), intent(inout) :: Ssf_bcs
!!
!!      subroutine alloc_surf_scaler_num(flux_sf)
!!      subroutine alloc_surf_scaler_apt(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!!      subroutine alloc_surf_vector_num(flux_sf)
!!      subroutine alloc_surf_vector_apt(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!!      subroutine alloc_surf_scaler_dat_type(scaler_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!!      subroutine alloc_surf_vector_dat_type(vector_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!!
!!      subroutine dealloc_surf_scaler_type(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!!      subroutine dealloc_surf_vector_type(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!!      subroutine dealloc_surf_scaler_dat_type(scaler_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!!      subroutine dealloc_surf_vector_dat_type(vector_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!!@endverbatim
!
      module t_surface_bc_data
!
      use m_precision
!
      implicit  none
!
      type scaler_surf_bc_data_type
        integer (kind=kint) :: ngrp_sf_dat = 0
        integer (kind=kint), allocatable :: id_grp_sf_dat(:)
      end type scaler_surf_bc_data_type
!
      type scaler_surf_flux_bc_type
        integer (kind=kint) :: ngrp_sf_fix_fx = 0
        integer (kind=kint), allocatable :: id_grp_sf_fix_fx(:)
        integer (kind=kint) :: nitem_sf_fix_fx = 0
        integer (kind=kint), allocatable :: ist_ele_sf_fix_fx(:)
        real(kind = kreal), allocatable :: sf_apt_fix_fx(:)
      end type scaler_surf_flux_bc_type
!
!
      type velocity_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs(3)
        type(scaler_surf_flux_bc_type) :: normal
        type(scaler_surf_flux_bc_type) :: grad(3)
        type(scaler_surf_bc_data_type) :: torque_lead(3)
        type(scaler_surf_bc_data_type) :: free_sph_in
        type(scaler_surf_bc_data_type) :: free_sph_out
      end type velocity_surf_bc_type
!
      type potential_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs
        type(scaler_surf_flux_bc_type) :: grad
        type(scaler_surf_bc_data_type) :: grad_lead
        type(scaler_surf_bc_data_type) :: wall
        type(scaler_surf_bc_data_type) :: sph_in
        type(scaler_surf_bc_data_type) :: sph_out
      end type potential_surf_bc_type
!
      type vector_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs(3)
        type(scaler_surf_flux_bc_type) :: normal
        type(scaler_surf_flux_bc_type) :: grad(3)
        type(scaler_surf_bc_data_type) :: torque_lead(3)
      end type vector_surf_bc_type
!
      type scaler_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs
        type(scaler_surf_flux_bc_type) :: flux
        type(scaler_surf_bc_data_type) :: flux_lead
      end type scaler_surf_bc_type
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_data_velo(Vsf_bcs)
!
      type(velocity_surf_bc_type),  intent(inout) :: Vsf_bcs
!
!
      call alloc_surf_vector_num(Vsf_bcs%grad)
      call alloc_surf_scaler_num(Vsf_bcs%normal)
      call alloc_surf_vector_dat_type(Vsf_bcs%sgs)
      call alloc_surf_vector_dat_type(Vsf_bcs%torque_lead)
      call alloc_surf_scaler_dat_type(Vsf_bcs%free_sph_in)
      call alloc_surf_scaler_dat_type(Vsf_bcs%free_sph_out)
!
      call alloc_surf_vector_apt(Vsf_bcs%grad)
      call alloc_surf_scaler_apt(Vsf_bcs%normal)
!
      end subroutine alloc_surf_data_velo
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_surf_data_velo(Vsf_bcs)
!
      type(velocity_surf_bc_type),  intent(inout) :: Vsf_bcs
!
!
      call dealloc_surf_vector_type(Vsf_bcs%grad)
      call dealloc_surf_scaler_type(Vsf_bcs%normal)
      call dealloc_surf_vector_dat_type(Vsf_bcs%sgs)
      call dealloc_surf_vector_dat_type(Vsf_bcs%torque_lead)
      call dealloc_surf_scaler_dat_type(Vsf_bcs%free_sph_in)
      call dealloc_surf_scaler_dat_type(Vsf_bcs%free_sph_out)
!
      end subroutine dealloc_surf_data_velo
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_surf_vector(Bsf_bcs)
!
      type(vector_surf_bc_type),  intent(inout) :: Bsf_bcs
!
!
      call alloc_surf_vector_num(Bsf_bcs%grad)
      call alloc_surf_vector_dat_type(Bsf_bcs%sgs)
      call alloc_surf_scaler_num(Bsf_bcs%normal)
      call alloc_surf_vector_dat_type(Bsf_bcs%torque_lead)
!
      call alloc_surf_vector_apt(Bsf_bcs%grad)
      call alloc_surf_scaler_apt(Bsf_bcs%normal)
!
      end subroutine alloc_surf_vector
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_surf_vector(Bsf_bcs)
!
      type(vector_surf_bc_type),  intent(inout) :: Bsf_bcs
!
!
      call dealloc_surf_vector_type(Bsf_bcs%grad)
      call dealloc_surf_vector_dat_type(Bsf_bcs%sgs)
      call dealloc_surf_scaler_type(Bsf_bcs%normal)
      call dealloc_surf_vector_dat_type(Bsf_bcs%torque_lead)
!
      end subroutine dealloc_surf_vector
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_surf_data_scalar(Ssf_bcs)
!
      type(scaler_surf_bc_type), intent(inout) :: Ssf_bcs
!
!
      call alloc_surf_scaler_num(Ssf_bcs%flux)
      call alloc_surf_scaler_dat_type(Ssf_bcs%sgs)
      call alloc_surf_scaler_dat_type(Ssf_bcs%flux_lead)
!
      call alloc_surf_scaler_apt(Ssf_bcs%flux)
!
      end subroutine alloc_surf_data_scalar
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_data_scalar(Ssf_bcs)
!
      type(scaler_surf_bc_type), intent(inout) :: Ssf_bcs
!
!
      call dealloc_surf_scaler_type(Ssf_bcs%flux)
      call dealloc_surf_scaler_dat_type(Ssf_bcs%sgs)
      call dealloc_surf_scaler_dat_type(Ssf_bcs%flux_lead)
!
      end subroutine dealloc_surf_data_scalar
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_potential(Fsf_bcs)
!
      type(potential_surf_bc_type), intent(inout) :: Fsf_bcs
!
!
      call alloc_surf_scaler_dat_type(Fsf_bcs%sgs)
      call alloc_surf_scaler_dat_type(Fsf_bcs%grad_lead)
!
      call alloc_surf_scaler_num(Fsf_bcs%grad)
      call alloc_surf_scaler_dat_type(Fsf_bcs%wall)
      call alloc_surf_scaler_dat_type(Fsf_bcs%sph_in)
      call alloc_surf_scaler_dat_type(Fsf_bcs%sph_out)
!
      call alloc_surf_scaler_apt(Fsf_bcs%grad)
!
       end subroutine alloc_surf_potential
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_potential(Fsf_bcs)
!
      type(potential_surf_bc_type), intent(inout) :: Fsf_bcs
!
!
      call dealloc_surf_scaler_dat_type(Fsf_bcs%sgs)
      call dealloc_surf_scaler_dat_type(Fsf_bcs%grad_lead)
!
      call dealloc_surf_scaler_type(Fsf_bcs%grad)
      call dealloc_surf_scaler_dat_type(Fsf_bcs%wall)
      call dealloc_surf_scaler_dat_type(Fsf_bcs%sph_in)
      call dealloc_surf_scaler_dat_type(Fsf_bcs%sph_out)
!
      end subroutine dealloc_surf_potential
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_surf_scaler_num(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!
!
      allocate( flux_sf%id_grp_sf_fix_fx(flux_sf%ngrp_sf_fix_fx) )
      allocate( flux_sf%ist_ele_sf_fix_fx(0:flux_sf%ngrp_sf_fix_fx) )
!
      flux_sf%ist_ele_sf_fix_fx = 0
      if (flux_sf%ngrp_sf_fix_fx.gt.0) flux_sf%id_grp_sf_fix_fx = 0
!
      end subroutine alloc_surf_scaler_num
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_scaler_apt(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!
!
      allocate( flux_sf%sf_apt_fix_fx(flux_sf%nitem_sf_fix_fx) )
      if(flux_sf%nitem_sf_fix_fx.gt.0) flux_sf%sf_apt_fix_fx = 0.0d0
!
      end subroutine alloc_surf_scaler_apt
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_vector_num(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        call alloc_surf_scaler_num(flux_sf(nd))
      end do
!
      end subroutine alloc_surf_vector_num
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_vector_apt(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        call alloc_surf_scaler_apt(flux_sf(nd))
      end do
!
      end subroutine alloc_surf_vector_apt
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_scaler_dat_type(scaler_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!
!
       allocate( scaler_sf%id_grp_sf_dat(scaler_sf%ngrp_sf_dat) )
       if (scaler_sf%ngrp_sf_dat .gt. 0) scaler_sf%id_grp_sf_dat = 0
!
      end subroutine alloc_surf_scaler_dat_type
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_vector_dat_type(vector_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        call alloc_surf_scaler_dat_type(vector_sf(nd))
      end do
!
      end subroutine alloc_surf_vector_dat_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_scaler_type(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!
!
      deallocate(flux_sf%id_grp_sf_fix_fx)
      deallocate(flux_sf%ist_ele_sf_fix_fx)
      deallocate(flux_sf%sf_apt_fix_fx)
!
      end subroutine dealloc_surf_scaler_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_vector_type(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        call dealloc_surf_scaler_type(flux_sf(nd))
      end do
!
      end subroutine dealloc_surf_vector_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_scaler_dat_type(scaler_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!
!
      deallocate( scaler_sf%id_grp_sf_dat )
!
      end subroutine dealloc_surf_scaler_dat_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_vector_dat_type(vector_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!
!
      integer(kind = kint) :: nd
!
      do nd = 1, 3
        call dealloc_surf_scaler_dat_type(vector_sf(3))
      end do
!
      end subroutine dealloc_surf_vector_dat_type
!
!-----------------------------------------------------------------------
!
      end module t_surface_bc_data
