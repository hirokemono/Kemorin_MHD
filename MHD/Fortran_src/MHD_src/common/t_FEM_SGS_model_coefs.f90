!>@file   t_FEM_SGS_model_coefs.f90
!!@brief  module t_FEM_SGS_model_coefs
!!
!!@author H. Matsui
!!@date    Programmed by H.Matsui in 200?
!!
!>@brief  Structures for model coefficients for FEM_SGS_MHD
!!
!!@verbatim
!!      subroutine SGS_model_coef_address_by_label                      &
!!     &         (term, i_field, i_comp, Csim)
!!      subroutine set_SGS_model_coef_address(term_name, n_comp,        &
!!     &                                      i_field, i_comp, Csim)
!!        type(field_def), intent(in) :: term
!!        character(len = kchara), intent(in) :: term_name
!!        integer(kind = kint), intent(in) :: n_comp
!!        integer(kind = kint), intent(inout) :: i_field, i_comp
!!        type(SGS_model_coefficient), intent(inout) :: Csim
!!
!!      subroutine dup_SGS_model_coefficient(org_Csim, new_Csim)
!!        type(SGS_model_coefficient), intent(in) :: org_Csim
!!        type(SGS_model_coefficient), intent(inout) :: new_Csim
!!      subroutine alloc_SGS_model_coefficient(n_ele, Csim)
!!      subroutine alloc_SGS_model_coef_on_nod(n_nod, Csim)
!!      subroutine dealloc_SGS_model_coefficient(Csim)
!!      subroutine dealloc_SGS_model_coef_on_nod(Csim)
!!        integer(kind = kint), intent(in) :: n_ele
!!        integer(kind = kint), intent(in) :: n_nod
!!        type(SGS_model_coefficient), intent(inout) :: Csim
!!
!!      subroutine check_SGS_model_coefficient(id_file, Csim)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(SGS_model_coefficient), intent(in) :: Csim
!!@endverbatim
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
!>        SGS term name
        character(len = kchara) :: term_name
!>        Address for model coeffieint
        integer(kind = kint) :: iak_Csim =   0
!>        Start address for model coeffieint work array
        integer(kind = kint) :: icomp_Csim = 0
!
!>        Number of components (0 indicates no used)
        integer(kind = kint) :: num_comp = 0
!>        Number of element
        integer(kind = kint) :: n_ele = 0
!>        Model coefficiens on element
        real(kind = kreal), allocatable :: coef(:,:)
!
!>        Number of element
        integer(kind = kint) :: n_nod = 0
!>        Model coefficiens on element
        real(kind = kreal), allocatable :: coef_nod(:,:)
      end type SGS_model_coefficient
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine SGS_model_coef_address_by_label                        &
     &         (term, i_field, i_comp, Csim)
!
      use t_field_labels
!
      type(field_def), intent(in) :: term
      integer(kind = kint), intent(inout) :: i_field, i_comp
      type(SGS_model_coefficient), intent(inout) :: Csim
!
      call set_SGS_model_coef_address(term%name, term%n_comp,           &
     &                                i_field, i_comp, Csim)
!
      end subroutine SGS_model_coef_address_by_label
!
! -------------------------------------------------------------------
!
      subroutine set_SGS_model_coef_address(term_name, n_comp,          &
     &                                      i_field, i_comp, Csim)
!
      character(len = kchara), intent(in) :: term_name
      integer(kind = kint), intent(in) :: n_comp
      integer(kind = kint), intent(inout) :: i_field, i_comp
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
      Csim%flag_set =   .FALSE.
      Csim%term_name =  term_name
      Csim%icomp_Csim = i_comp + 1
      Csim%iak_Csim =   i_field + 1
      Csim%num_comp =   n_comp
      i_comp =  i_comp + n_comp
      i_field = i_field + 1
!
      end subroutine set_SGS_model_coef_address
!
! -------------------------------------------------------------------
!
      subroutine alloc_SGS_model_coefficient(n_ele, Csim)
!
      integer(kind = kint), intent(in) :: n_ele
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
      Csim%n_ele = n_ele
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
! -------------------------------------------------------------------
!
      subroutine dup_SGS_model_coefficient(org_Csim, new_Csim)
!
      type(SGS_model_coefficient), intent(in) :: org_Csim
      type(SGS_model_coefficient), intent(inout) :: new_Csim
!
!
      new_Csim%term_name = org_Csim%term_name
      new_Csim%num_comp =  org_Csim%num_comp
      call alloc_SGS_model_coefficient(org_Csim%n_ele, new_Csim)
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
! -------------------------------------------------------------------
!
      subroutine check_SGS_model_coefficient(id_file, Csim)
!
      integer(kind = kint), intent(in) :: id_file
      type(SGS_model_coefficient), intent(in) :: Csim
!
!
      if(Csim%iak_Csim .le. 0) return
      write(id_file,'(2a,3i4)') trim(Csim%term_name), ': ',             &
     &                    Csim%iak_Csim, Csim%icomp_Csim, Csim%num_comp
!
      end subroutine check_SGS_model_coefficient
!
! -------------------------------------------------------------------
!
      end module t_FEM_SGS_model_coefs
