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
!!      subroutine dup_SGS_model_coefficient(org_Csim, new_Csim)
!!        type(SGS_model_coefficient), intent(in) :: org_Csim
!!        type(SGS_model_coefficient), intent(inout) :: new_Csim
!!      subroutine alloc_SGS_model_coefficient(num_poins, num_comp,     &
!!     &                                       Csim)
!!      subroutine dealloc_SGS_model_coefficient(Csim)
!!        integer(kind = kint), intent(in) :: num_poins, num_comp
!!        type(SGS_model_coefficient), intent(inout) :: Csim
!!
!
      module t_SGS_model_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
      type SGS_model_coefficient
!>        Set flag
        logical :: flag_set
!>        Address for model coeffieint
        integer(kind = kint) :: iak_diff
!
!>        Number of grid poins
        integer(kind = kint) :: num_poins
!>        Number of components (0 indicates no used)
        integer(kind = kint) :: num_comp
!>        Model coefficiens
        real(kind = kreal), allocatable :: coef(:,:)
      end type SGS_model_coefficient
!
      type SGS_coefficients_type
        integer(kind = kint) :: num_field
        integer(kind = kint) :: ntot_comp
        integer(kind = kint), allocatable  :: iflag_field(:)
        integer(kind = kint), allocatable  :: num_comps(:)
        integer(kind = kint), allocatable  :: istack_comps(:)
        real(kind = kreal), allocatable :: ak(:,:)
!>       Structure for commutationa error coefficient for velocity
        type(SGS_model_coefficient) :: Cdiff_velo
!>       Structure for commutationa error coefficient for magnetic field
        type(SGS_model_coefficient) :: Cdiff_magne
!>       Structure for commutationa error coefficient for temperature
        type(SGS_model_coefficient) :: Cdiff_temp
!>       Structure for commutationa error coefficient for temperature
        type(SGS_model_coefficient) :: Cdiff_light
!
!>       Structure for commutationa error coefficient for SGS inducion
        type(SGS_model_coefficient) :: Cdiff_SGS_uxb
      end type SGS_coefficients_type
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
! -------------------------------------------------------------------
!
      subroutine dup_SGS_model_coefficient(org_Csim, new_Csim)
!
      type(SGS_model_coefficient), intent(in) :: org_Csim
      type(SGS_model_coefficient), intent(inout) :: new_Csim
!
!
      call alloc_SGS_model_coefficient                                  &
     &   (org_Csim%num_poins, org_Csim%num_comp, new_Csim)
!
      new_Csim%flag_set = org_Csim%flag_set
      if((new_Csim%num_poins*new_Csim%num_comp) .le. 0) return
!$omp parallel workshare
      new_Csim%coef(1:new_Csim%num_poins, 1:new_Csim%num_comp)          &
     &   = org_Csim%coef(1:new_Csim%num_poins, 1:new_Csim%num_comp)
!$omp end parallel workshare
!
      end subroutine dup_SGS_model_coefficient
!
! -------------------------------------------------------------------
!
      subroutine alloc_SGS_model_coefficient(num_poins, num_comp,       &
     &                                       Csim)
!
      integer(kind = kint), intent(in) :: num_poins, num_comp
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
      Csim%num_poins = num_poins
      Csim%num_comp = num_comp
      allocate(Csim%coef(Csim%num_poins, Csim%num_comp))
!
      Csim%flag_set = .FALSE.
      if((Csim%num_poins*Csim%num_comp) .le. 0) return
!$omp parallel workshare
      Csim%coef(1:Csim%num_poins, 1:Csim%num_comp) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_SGS_model_coefficient
!
! -------------------------------------------------------------------
!
      subroutine dealloc_SGS_model_coefficient(Csim)
!
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
      deallocate(Csim%coef)
!
      end subroutine dealloc_SGS_model_coefficient
!
! -------------------------------------------------------------------
!
      end module t_SGS_model_coefs
