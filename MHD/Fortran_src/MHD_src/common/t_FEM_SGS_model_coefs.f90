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
!!      subroutine alloc_SGS_model_coefficient(n_ele, num_comp, Csim)
!!      subroutine alloc_SGS_model_coef_on_nod(n_nod, Csim)
!!      subroutine dealloc_SGS_model_coefficient(Csim)
!!      subroutine dealloc_SGS_model_coef_on_nod(Csim)
!!        integer(kind = kint), intent(in) :: n_ele, num_comp
!!        integer(kind = kint), intent(in) :: n_nod
!!        type(SGS_model_coefficient), intent(inout) :: Csim
!!
!!      subroutine check_sgs_addresses(id_file, wk_sgs, sgs_coefs)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
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
! -------------------------------------------------------------------
!
      subroutine check_sgs_addresses(id_file, wk_sgs, sgs_coefs)
!
      use t_ele_info_4_dynamic
!
      integer(kind = kint), intent(in) :: id_file
      type(dynamic_model_data), intent(in) :: wk_sgs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
!
!
      if(iflag_debug .gt. 0) then
        write(id_file,*) 'num_sgs_kinds', wk_sgs%num_kinds
        write(id_file,*) 'num_sgs_coefs', wk_sgs%ntot_comp
!
        if(sgs_coefs%Csim_SGS_hf%iak_Csim .gt. 0) then
          write(id_file,*)                                              &
     &               'iak_sgs_hf', sgs_coefs%Csim_SGS_hf%iak_Csim,      &
     &                             sgs_coefs%Csim_SGS_hf%icomp_Csim,    &
     &                             sgs_coefs%Csim_SGS_hf%num_comp,      &
     &         trim(wk_sgs%name(sgs_coefs%Csim_SGS_hf%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_cf%iak_Csim .gt. 0) then
          write(id_file,*)                                              &
     &               'iak_sgs_cf', sgs_coefs%Csim_SGS_cf%iak_Csim,      &
     &                             sgs_coefs%Csim_SGS_cf%icomp_Csim,    &
     &                             sgs_coefs%Csim_SGS_cf%num_comp,      &
     &         trim(wk_sgs%name(sgs_coefs%Csim_SGS_cf%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_mf%iak_Csim .gt. 0) then
          write(id_file,*)                                              &
     &               'iak_sgs_mf', sgs_coefs%Csim_SGS_mf%iak_Csim,      &
     &                             sgs_coefs%Csim_SGS_mf%icomp_Csim,    &
     &                             sgs_coefs%Csim_SGS_mf%num_comp,      &
     &         trim(wk_sgs%name(sgs_coefs%Csim_SGS_mf%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_lor%iak_Csim .gt. 0) then
          write(id_file,*)                                              &
     &               'iak_sgs_lor', sgs_coefs%Csim_SGS_lor%iak_Csim,    &
     &                             sgs_coefs%Csim_SGS_lor%icomp_Csim,   &
     &                             sgs_coefs%Csim_SGS_lor%num_comp,     &
     &            trim(wk_sgs%name(sgs_coefs%Csim_SGS_lor%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_tbuo%iak_Csim .gt. 0) then
          write(id_file,*)                                              &
     &               'iak_sgs_tbuo', sgs_coefs%Csim_SGS_tbuo%iak_Csim,  &
     &                              sgs_coefs%Csim_SGS_tbuo%icomp_Csim, &
     &                              sgs_coefs%Csim_SGS_tbuo%num_comp,   &
     &             trim(wk_sgs%name(sgs_coefs%Csim_SGS_tbuo%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_cbuo%iak_Csim .gt. 0) then
          write(id_file,*)                                              &
     &               'iak_sgs_cbuo', sgs_coefs%Csim_SGS_cbuo%iak_Csim,  &
     &                              sgs_coefs%Csim_SGS_cbuo%icomp_Csim, &
     &                              sgs_coefs%Csim_SGS_cbuo%num_comp,   &
     &             trim(wk_sgs%name(sgs_coefs%Csim_SGS_cbuo%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_uxb%iak_Csim .gt. 0) then
          write(id_file,*)                                              &
     &               'iak_sgs_uxb', sgs_coefs%Csim_SGS_uxb%iak_Csim,    &
     &                              sgs_coefs%Csim_SGS_uxb%icomp_Csim,  &
     &                              sgs_coefs%Csim_SGS_uxb%num_comp,    &
     &             trim(wk_sgs%name(sgs_coefs%Csim_SGS_uxb%iak_Csim))
        end if
      end if
!
      end subroutine check_sgs_addresses
!
! -------------------------------------------------------------------
!
      end module t_FEM_SGS_model_coefs
