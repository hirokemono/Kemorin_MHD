!t_material_property.f90
!     module t_material_property
!
!> @brief addresses for SGS coefficients
!
!     Written by H. Matsui
!
!!      subroutine alloc_MHD_num_coefs(coefs)
!!      subroutine alloc_MHD_coefs(nele, coefs)
!!      subroutine dealloc_MHD_coefs(coefs)
!!      subroutine copy_MHD_num_coefs(org_coefs, new_coefs)
!
      module t_material_property
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      type MHD_coefficients_type
        integer(kind = kint) :: num_field
        integer(kind = kint) :: ntot_comp
        integer(kind = kint), pointer  :: iflag_field(:)
        integer(kind = kint), pointer  :: num_comps(:)
        integer(kind = kint), pointer  :: istack_comps(:)
        real(kind = kreal), pointer :: ak(:,:)
      end type MHD_coefficients_type
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
      subroutine alloc_MHD_num_coefs(coefs)
!
      type(MHD_coefficients_type), intent(inout) :: coefs
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
      type(MHD_coefficients_type), intent(inout) :: coefs
!
!
      allocate(coefs%ak(nele,coefs%ntot_comp) )
!
      if(nele*coefs%ntot_comp .gt. 0) coefs%ak = 1.0d0
!
      end subroutine alloc_MHD_coefs
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine dealloc_MHD_coefs(coefs)
!
      type(MHD_coefficients_type), intent(inout) :: coefs
!
!
      deallocate(coefs%ak)
      deallocate(coefs%num_comps, coefs%istack_comps)
      deallocate(coefs%iflag_field)
!
      end subroutine dealloc_MHD_coefs
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_MHD_num_coefs(org_coefs, new_coefs)
!
      type(MHD_coefficients_type), intent(in) :: org_coefs
      type(MHD_coefficients_type), intent(inout) :: new_coefs
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
!
      end module t_material_property
