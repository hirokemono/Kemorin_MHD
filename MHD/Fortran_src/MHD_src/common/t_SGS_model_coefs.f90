!t_SGS_model_coefs.f90
!     module t_SGS_model_coefs
!
!> @brief addresses for SGS coefficients
!
!     Written by H. Matsui
!
!!       subroutine copy_SGS_num_coefs(org_coefs, new_coefs)
!!
!!       subroutine alloc_SGS_num_coefs(coefs)
!!       subroutine alloc_SGS_coefs(nele, coefs)
!!       subroutine dealloc_SGS_coefs(coefs)
!!
!!      subroutine check_sgs_addresses                                  &
!!     &         (ifld_sgs, icomp_sgs, ifld_diff, icomp_diff,           &
!!     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      module t_SGS_model_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_base_field_labels
      use t_SGS_term_labels
!
      implicit  none
!
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
!        integer (kind=kint) :: i_temp =  izero
        integer (kind=kint) :: i_light = izero
!
!base%i_temp
!
!>        Structure of base fields
        type(base_field_address) :: base
!>        Structure of filtered fields
        type(base_field_address) :: filter_fld
!>        Structure of SGS terms
        type(SGS_term_address) :: SGS_term
      end type SGS_terms_address
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_SGS_num_coefs(org_coefs, new_coefs)
!
      type(SGS_coefficients_type), intent(in) :: org_coefs
      type(SGS_coefficients_type), intent(inout) :: new_coefs
!
!
      new_coefs%num_field = org_coefs%num_field
      new_coefs%ntot_comp = org_coefs%ntot_comp
!
      call alloc_SGS_num_coefs(new_coefs)
!
      new_coefs%num_comps =    org_coefs%num_comps
      new_coefs%iflag_field =  org_coefs%iflag_field
      new_coefs%istack_comps = org_coefs%istack_comps
!
      end subroutine copy_SGS_num_coefs
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine alloc_SGS_num_coefs(coefs)
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
      end subroutine alloc_SGS_num_coefs
!
! -------------------------------------------------------------------
!
      subroutine alloc_SGS_coefs(nele, coefs)
!
      integer(kind = kint), intent(in) :: nele
      type(SGS_coefficients_type), intent(inout) :: coefs
!
!
      allocate(coefs%ak(nele,coefs%ntot_comp) )
!
      if(nele*coefs%ntot_comp .gt. 0) coefs%ak = 1.0d0
!
      end subroutine alloc_SGS_coefs
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine dealloc_SGS_coefs(coefs)
!
      type(SGS_coefficients_type), intent(inout) :: coefs
!
!
      deallocate(coefs%ak)
      deallocate(coefs%num_comps, coefs%istack_comps)
      deallocate(coefs%iflag_field)
!
      end subroutine dealloc_SGS_coefs
!
! -------------------------------------------------------------------
!
      end module t_SGS_model_coefs
