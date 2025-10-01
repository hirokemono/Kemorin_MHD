!t_FEM_SGS_model_coefs.f90
!     module t_FEM_SGS_model_coefs
!
!> @brief addresses for SGS coefficients
!
!     Written by H. Matsui
!
!!      subroutine dup_SGS_model_coefficient(org_Csim, new_Csim)
!!        type(SGS_model_coefficient), intent(in) :: org_Csim
!!        type(SGS_model_coefficient), intent(inout) :: new_Csim
!!      subroutine alloc_SGS_model_coefficient(n_ele, num_comp, Csim)
!!      subroutine alloc_SGS_model_coef_on_nod(n_nod, Csim)
!!      subroutine dealloc_SGS_model_coefficient(Csim)
!!      subroutine dealloc_SGS_model_coef_on_nod(Csim)
!!        integer(kind = kint), intent(in) :: n_ele, num_comp
!!        integer(kind = kint), intent(in) :: n_nod
!!        type(SGS_model_coefficient), intent(inout) :: Csim
!
      module t_FEM_SGS_model_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_base_field_labels
!
      implicit  none
!
      type SGS_model_coefficient
!>        Set flag
        logical :: flag_set
!>        Address for model coeffieint
        integer(kind = kint) :: iak_Csim
!>        Start address for model coeffieint work array
        integer(kind = kint) :: icomp_Csim
!
!>        Number of components (0 indicates no used)
        integer(kind = kint) :: num_comp
!>        Number of element
        integer(kind = kint) :: n_ele
!>        Model coefficiens on element
        real(kind = kreal), allocatable :: coef(:,:)
!
!>        Number of element
        integer(kind = kint) :: n_nod
!>        Model coefficiens on element
        real(kind = kreal), allocatable :: coef_nod(:,:)
      end type SGS_model_coefficient
!
      type SGS_coefficients_type
!>       Structure for commutationa error coefficient for SGS inducion
        type(SGS_model_coefficient) :: Csim_SGS_uxb
!>       Structure for commutationa error coefficient for SGS Lorenz force
        type(SGS_model_coefficient) :: Csim_SGS_lor
!>       Structure for commutationa error coefficient for SGS momentum flux
        type(SGS_model_coefficient) :: Csim_SGS_mf
!>       Structure for commutationa error coefficient for SGS heat flux
        type(SGS_model_coefficient) :: Csim_SGS_hf
!>       Structure for commutationa error coefficient for SGS composition flux
        type(SGS_model_coefficient) :: Csim_SGS_cf
!>       Structure for commutationa error coefficient for SGS composition flux
        type(SGS_model_coefficient) :: Csim_SGS_tbuo
!>       Structure for commutationa error coefficient for SGS composition flux
        type(SGS_model_coefficient) :: Csim_SGS_cbuo
      end type SGS_coefficients_type
!
!
      type SGS_commutation_coefs
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
!>       Structure for commutationa error coefficient for SGS Lorenz force
        type(SGS_model_coefficient) :: Cdiff_SGS_lor
!>       Structure for commutationa error coefficient for SGS momentum flux
        type(SGS_model_coefficient) :: Cdiff_SGS_mf
!>       Structure for commutationa error coefficient for SGS heat flux
        type(SGS_model_coefficient) :: Cdiff_SGS_hf
!>       Structure for commutationa error coefficient for SGS composition flux
        type(SGS_model_coefficient) :: Cdiff_SGS_cf
      end type SGS_commutation_coefs
!
!
!>      Structure of model coefficieints for FEM MHD
      type SGS_coefficients_data
!>        Model coefficeints in elements
        type(SGS_coefficients_type) :: sgs_coefs
!>        Commutation error model coefficeints
        type(SGS_commutation_coefs) :: diff_coefs
      end type SGS_coefficients_data
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine dup_SGS_model_coefficient(org_Csim, new_Csim)
!
      type(SGS_model_coefficient), intent(in) :: org_Csim
      type(SGS_model_coefficient), intent(inout) :: new_Csim
!
!
      call alloc_SGS_model_coefficient                                  &
     &   (org_Csim%n_ele, org_Csim%num_comp, new_Csim)
!
      new_Csim%flag_set = org_Csim%flag_set
      if((new_Csim%n_ele*new_Csim%num_comp) .le. 0) return
!$omp parallel workshare
      new_Csim%coef(1:new_Csim%n_ele, 1:new_Csim%num_comp)              &
     &   = org_Csim%coef(1:new_Csim%n_ele, 1:new_Csim%num_comp)
!$omp end parallel workshare
!
      end subroutine dup_SGS_model_coefficient
!
! -------------------------------------------------------------------
!
      subroutine alloc_SGS_model_coefficient(n_ele, num_comp, Csim)
!
      integer(kind = kint), intent(in) :: n_ele, num_comp
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
      Csim%n_ele = n_ele
      Csim%num_comp = num_comp
      allocate(Csim%coef(Csim%n_ele, Csim%num_comp))
!
      Csim%flag_set = .FALSE.
      if((Csim%n_ele*Csim%num_comp) .le. 0) return
!$omp parallel workshare
      Csim%coef(1:Csim%n_ele, 1:Csim%num_comp) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_SGS_model_coefficient
!
! -------------------------------------------------------------------
!
      subroutine alloc_SGS_model_coef_on_nod(n_nod, Csim)
!
      integer(kind = kint), intent(in) :: n_nod
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
      Csim%n_nod = n_nod
      allocate(Csim%coef_nod(Csim%n_nod, Csim%num_comp))
!
      Csim%flag_set = .FALSE.
      if((Csim%n_nod*Csim%num_comp) .le. 0) return
!$omp parallel workshare
      Csim%coef_nod(1:Csim%n_nod, 1:Csim%num_comp) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_SGS_model_coef_on_nod
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine dealloc_SGS_model_coefficient(Csim)
!
      type(SGS_model_coefficient), intent(inout) :: Csim
!
      deallocate(Csim%coef)
!
      end subroutine dealloc_SGS_model_coefficient
!
! -------------------------------------------------------------------
!
      subroutine dealloc_SGS_model_coef_on_nod(Csim)
!
      type(SGS_model_coefficient), intent(inout) :: Csim
!
      deallocate(Csim%coef_nod)
!
      end subroutine dealloc_SGS_model_coef_on_nod
!
! -------------------------------------------------------------------
!
      end module t_FEM_SGS_model_coefs
