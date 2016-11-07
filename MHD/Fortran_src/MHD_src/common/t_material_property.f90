!t_material_property.f90
!     module t_material_property
!
!> @brief addresses for SGS coefficients
!
!     Written by H. Matsui
!
!!       subroutine copy_MHD_num_coefs(org_coefs, new_coefs)
!!
!!       subroutine alloc_velo_diff_MHD_AMG(numele, ak_MHD)
!!       subroutine alloc_temp_diff_MHD_AMG(numele, ak_MHD)
!!       subroutine alloc_magne_diff_MHD_AMG(numele, ak_MHD)
!!       subroutine alloc_dscalar_diff_MHD_AMG(numele, ak_MHD)
!!       subroutine alloc_buoyancy_coef_ele(numele, ak_MHD)
!!       subroutine alloc_comp_buo_coef_ele(numele, ak_MHD)
!!       subroutine alloc_diff_coefs_velo_type(numele, ak_MHD)
!!       subroutine alloc_diff_coefs_temp_type(numele, ak_MHD)
!!       subroutine alloc_diff_coefs_magne_type(numele, ak_MHD)
!!       subroutine alloc_diff_coefs_ds_type(numele, ak_MHD)
!!       subroutine alloc_MHD_num_coefs(coefs)
!!       subroutine alloc_MHD_coefs(nele, coefs)
!!
!!       subroutine dealloc_velo_diff_MHD_AMG(ak_MHD)
!!       subroutine dealloc_temp_diff_MHD_AMG(ak_MHD)
!!       subroutine dealloc_magne_diff_MHD_AMG(ak_MHD)
!!       subroutine dealloc_dscalar_diff_MHD_AMG(ak_MHD)
!!       subroutine dealloc_buoyancy_coef_ele(ak_MHD)
!!       subroutine dealloc_comp_buo_coef_ele(ak_MHD)
!!       subroutine dealloc_MHD_coefs(coefs)
!
      module t_material_property
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      type coefs_4_MHD_type
!>       coeffeicient for viscous diffusion for each element
        real  (kind=kreal), allocatable :: ak_d_velo(:)
!>       coeffeicient for thermal diffusion for each element
        real  (kind=kreal), allocatable :: ak_d_temp(:)
!>       coeffeicient for magnetic diffusion for each element
        real  (kind=kreal), allocatable :: ak_d_magne(:)
!>       coeffeicient for chemical diffusion for each element
        real  (kind=kreal), allocatable :: ak_d_composit(:)
!
!>       coeffeicient for thermal buoyancy for each element
        real  (kind=kreal), allocatable :: ak_buo(:)
!>       coeffeicient for compositional buoyancy for each element
        real  (kind=kreal), allocatable :: ak_comp_buo(:)
      end type coefs_4_MHD_type
!
      type SGS_coefficients_type
        integer(kind = kint) :: num_field
        integer(kind = kint) :: ntot_comp
        integer(kind = kint), allocatable  :: iflag_field(:)
        integer(kind = kint), allocatable  :: num_comps(:)
        integer(kind = kint), allocatable  :: istack_comps(:)
        real(kind = kreal), allocatable :: ak(:,:)
      end type SGS_coefficients_type
!
      type SGS_terms_address
        integer (kind=kint) :: i_velo =  izero
        integer (kind=kint) :: i_temp =  izero
        integer (kind=kint) :: i_magne = izero
        integer (kind=kint) :: i_light = izero
!
        integer (kind=kint) :: i_filter_velo =  izero
        integer (kind=kint) :: i_filter_temp =  izero
        integer (kind=kint) :: i_filter_magne = izero
        integer (kind=kint) :: i_filter_light = izero
!
        integer (kind=kint) :: i_heat_flux =     izero
        integer (kind=kint) :: i_mom_flux =      izero
        integer (kind=kint) :: i_comp_flux =     izero
        integer (kind=kint) :: i_lorentz =       izero
        integer (kind=kint) :: i_induction =     izero
        integer (kind=kint) :: i_buoyancy =      izero
        integer (kind=kint) :: i_comp_buoyancy = izero
      end type SGS_terms_address
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_MHD_num_coefs(org_coefs, new_coefs)
!
      type(SGS_coefficients_type), intent(in) :: org_coefs
      type(SGS_coefficients_type), intent(inout) :: new_coefs
!
!
      new_coefs%num_field = org_coefs%num_field
      new_coefs%ntot_comp = org_coefs%ntot_comp
!
      call alloc_MHD_num_coefs(new_coefs)
!
      new_coefs%num_comps =    org_coefs%num_comps
      new_coefs%iflag_field =  org_coefs%iflag_field
      new_coefs%istack_comps = org_coefs%istack_comps
!
      end subroutine copy_MHD_num_coefs
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
       subroutine alloc_velo_diff_MHD_AMG(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_d_velo(numele) )
       if (numele.gt.0) ak_MHD%ak_d_velo = 0.0d0
!
       end subroutine alloc_velo_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_temp_diff_MHD_AMG(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_d_temp(numele) )
       if (numele.gt.0) ak_MHD%ak_d_temp = 0.0d0
!
       end subroutine alloc_temp_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_magne_diff_MHD_AMG(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_d_magne(numele) )
       if (numele.gt.0) ak_MHD%ak_d_magne = 0.0d0
!
       end subroutine alloc_magne_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_dscalar_diff_MHD_AMG(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_d_composit(numele) )
       if (numele.gt.0) ak_MHD%ak_d_composit = 0.0d0
!
       end subroutine alloc_dscalar_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_buoyancy_coef_ele(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_buo(numele) )
       if (numele.gt.0) ak_MHD%ak_buo = 0.0d0
!
       end subroutine alloc_buoyancy_coef_ele
!
! ----------------------------------------------------------------------
!
       subroutine alloc_comp_buo_coef_ele(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_comp_buo(numele) )
       if (numele.gt.0) ak_MHD%ak_comp_buo = 0.0d0
!
       end subroutine alloc_comp_buo_coef_ele
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_MHD_num_coefs(coefs)
!
      type(SGS_coefficients_type), intent(inout) :: coefs
!
!
      allocate(coefs%num_comps(coefs%num_field) )
      allocate(coefs%istack_comps(0:coefs%num_field) )
      allocate(coefs%iflag_field(coefs%num_field) )
!
      if(coefs%num_field .gt. 0) coefs%num_comps =   0
      if(coefs%num_field .gt. 0) coefs%iflag_field = 0
      coefs%istack_comps = 0
!
      end subroutine alloc_MHD_num_coefs
!
! -------------------------------------------------------------------
!
      subroutine alloc_MHD_coefs(nele, coefs)
!
      integer(kind = kint), intent(in) :: nele
      type(SGS_coefficients_type), intent(inout) :: coefs
!
!
      allocate(coefs%ak(nele,coefs%ntot_comp) )
!
      if(nele*coefs%ntot_comp .gt. 0) coefs%ak = 1.0d0
!
      end subroutine alloc_MHD_coefs
!
! -------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine dealloc_velo_diff_MHD_AMG(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_d_velo )
!
       end subroutine dealloc_velo_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_temp_diff_MHD_AMG(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_d_temp )
!
       end subroutine dealloc_temp_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_magne_diff_MHD_AMG(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_d_magne )
!
       end subroutine dealloc_magne_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_dscalar_diff_MHD_AMG(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_d_composit )
!
       end subroutine dealloc_dscalar_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_buoyancy_coef_ele(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_buo )
!
       end subroutine dealloc_buoyancy_coef_ele
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_comp_buo_coef_ele(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_comp_buo )
!
       end subroutine dealloc_comp_buo_coef_ele
!
! ----------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine dealloc_MHD_coefs(coefs)
!
      type(SGS_coefficients_type), intent(inout) :: coefs
!
!
      deallocate(coefs%ak)
      deallocate(coefs%num_comps, coefs%istack_comps)
      deallocate(coefs%iflag_field)
!
      end subroutine dealloc_MHD_coefs
!
! -------------------------------------------------------------------
!
      end module t_material_property
