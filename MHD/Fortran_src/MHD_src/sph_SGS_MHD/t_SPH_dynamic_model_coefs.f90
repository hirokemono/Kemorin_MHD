!>@file   t_SPH_dynamic_model_coefs.f90
!!@brief  module t_SPH_dynamic_model_coefs
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine alloc_sph_sgs_coefs_layer(n_layer_d, num_sgs_kinds,  &
!!     &                                     wk_sph_sgs)
!!      subroutine dealloc_sph_sgs_coefs_layer(wk_sph_sgs)
!!        integer(kind = kint), intent(in) :: n_layer_d
!!        integer(kind = kint), intent(in) :: num_sgs_kinds
!!        type(SPH_dynamic_model_coefs), intent(inout) :: wk_sph_sgs
!!
!!      subroutine set_sph_sgs_addresses                                &
!!     &          (SGS_param, fl_prop, cd_prop, ht_prop, cp_prop,       &
!!     &           iak_sgs_term, wk_sph_sgs)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(SGS_term_address), intent(inout) :: iak_sgs_term
!!        type(SPH_dynamic_model_coefs), intent(inout) :: wk_sph_sgs
!!      subroutine check_sph_sgs_addresses(iak_sgs_term, wk_sph_sgs)
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(SPH_dynamic_model_coefs), intent(in) :: wk_sph_sgs
!!@endverbatim
!!
      module t_SPH_dynamic_model_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_SGS_term_labels
!
      implicit none
!
!>      Structure of data for dyanmic SGS model for spectrum dynamo
      type SPH_dynamic_model_coefs
        integer(kind = kint) :: nlayer
        integer(kind = kint) :: num_kinds
        character(len = kchara), allocatable :: name(:)
        real(kind = kreal), allocatable :: fld_coef(:,:)
!
        real(kind = kreal), allocatable :: sgs_zl(:,:)
        real(kind = kreal), allocatable :: sgs_zt(:,:)
      end type SPH_dynamic_model_coefs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sph_sgs_coefs_layer(n_layer_d, num_sgs_kinds,    &
     &                                     wk_sph_sgs)
!
      integer(kind = kint), intent(in) :: n_layer_d
      integer(kind = kint), intent(in) :: num_sgs_kinds
      type(SPH_dynamic_model_coefs), intent(inout) :: wk_sph_sgs
!
!
      wk_sph_sgs%nlayer = n_layer_d
      wk_sph_sgs%num_kinds = num_sgs_kinds
!
      allocate(wk_sph_sgs%name(num_sgs_kinds))
      allocate(wk_sph_sgs%fld_coef(n_layer_d, num_sgs_kinds))
!
      allocate(wk_sph_sgs%sgs_zl(n_layer_d,n_sym_tensor))
      allocate(wk_sph_sgs%sgs_zt(n_layer_d,n_sym_tensor))
!
      if(num_sgs_kinds .gt. 0) then
!$omp parallel workshare
        wk_sph_sgs%fld_coef(1:n_layer_d, 1:num_sgs_kinds) =  one
!$omp end parallel workshare
      end if
!
!$omp parallel workshare
      wk_sph_sgs%sgs_zl(1:n_layer_d,1:n_sym_tensor) =  one
      wk_sph_sgs%sgs_zt(1:n_layer_d,1:n_sym_tensor) =  one
!$omp end parallel workshare
!
      end subroutine alloc_sph_sgs_coefs_layer
!
! -------------------------------------------------------------------
!
      subroutine dealloc_sph_sgs_coefs_layer(wk_sph_sgs)
!
      type(SPH_dynamic_model_coefs), intent(inout) :: wk_sph_sgs
!
!
      deallocate(wk_sph_sgs%name)
      deallocate(wk_sph_sgs%fld_coef)
      deallocate(wk_sph_sgs%sgs_zl, wk_sph_sgs%sgs_zt)
!
      end subroutine dealloc_sph_sgs_coefs_layer
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_sph_sgs_addresses                                  &
     &          (SGS_param, fl_prop, cd_prop, ht_prop, cp_prop,         &
     &           iak_sgs_term, wk_sph_sgs)
!
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_physical_property
!
      use m_SGS_term_labels
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(SGS_term_address), intent(inout) :: iak_sgs_term
      type(SPH_dynamic_model_coefs), intent(inout) :: wk_sph_sgs
!
      integer(kind = kint) :: i_fld
!
!
       i_fld = 0
       if (ht_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
           i_fld = i_fld + 1
           iak_sgs_term%i_SGS_h_flux =  i_fld
           wk_sph_sgs%name(i_fld) = SGS_heat_flux%name
         end if
       end if
!
       if (fl_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none) then
           i_fld = i_fld + 1
           iak_sgs_term%i_SGS_m_flux =  i_fld
           wk_sph_sgs%name(i_fld) = SGS_momentum_flux%name
         end if
       end if
!
       if (fl_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
           i_fld = i_fld + 1
           iak_sgs_term%i_SGS_Lorentz =  i_fld
           wk_sph_sgs%name(i_fld) = SGS_maxwell_tensor%name
         end if
       end if
!
       if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
          if(fl_prop%iflag_4_gravity) then
            i_fld = i_fld + 1
            iak_sgs_term%i_SGS_buoyancy =  i_fld
            wk_sph_sgs%name(i_fld) = SGS_buoyancy%name
          end if
         end if
       end if
!
       if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
          if(fl_prop%iflag_4_composit_buo) then
            i_fld = i_fld + 1
            iak_sgs_term%i_SGS_comp_buo =  i_fld
            wk_sph_sgs%name(i_fld) = SGS_composit_buoyancy%name
          end if
        end if
       end if
!
       if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
           i_fld = i_fld + 1
           iak_sgs_term%i_SGS_induction =  i_fld
           wk_sph_sgs%name(i_fld) = SGS_induction%name
         end if
       end if
       if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
           i_fld = i_fld + 1
           iak_sgs_term%i_SGS_induction =  i_fld
           wk_sph_sgs%name(i_fld) = SGS_induction%name
         end if
       end if
!
       if (cp_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
           i_fld = i_fld + 1
           iak_sgs_term%i_SGS_c_flux =  i_fld
           wk_sph_sgs%name(i_fld) = SGS_composit_flux%name
         end if
       end if
!
     end subroutine set_sph_sgs_addresses
!
!  ------------------------------------------------------------------
!
      subroutine check_sph_sgs_addresses(iak_sgs_term, wk_sph_sgs)
!
      use calypso_mpi
!
      use t_SGS_control_parameter
!
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SPH_dynamic_model_coefs), intent(in) :: wk_sph_sgs
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'num_sgs_kinds', wk_sph_sgs%num_kinds
!
        if(iak_sgs_term%i_SGS_h_flux .gt. 0) then
          write(*,*) 'iak_sgs_hf', iak_sgs_term%i_SGS_h_flux,           &
     &       trim(wk_sph_sgs%name(iak_sgs_term%i_SGS_h_flux))
        end if
        if(iak_sgs_term%i_SGS_m_flux .gt. 0) then
          write(*,*) 'iak_sgs_mf', iak_sgs_term%i_SGS_m_flux,           &
     &       trim(wk_sph_sgs%name(iak_sgs_term%i_SGS_m_flux))
        end if
        if(iak_sgs_term%i_SGS_Lorentz .gt. 0) then
          write(*,*) 'iak_sgs_lor', iak_sgs_term%i_SGS_Lorentz,         &
     &       trim(wk_sph_sgs%name(iak_sgs_term%i_SGS_Lorentz))
        end if
        if(iak_sgs_term%i_SGS_buoyancy .gt. 0) then
          write(*,*) 'iak_sgs_tbuo', iak_sgs_term%i_SGS_buoyancy,       &
     &      trim(wk_sph_sgs%name(iak_sgs_term%i_SGS_buoyancy))
        end if
        if(iak_sgs_term%i_SGS_comp_buo .gt. 0) then
          write(*,*) 'iak_sgs_cbuo', iak_sgs_term%i_SGS_comp_buo,       &
     &      trim(wk_sph_sgs%name(iak_sgs_term%i_SGS_comp_buo))
        end if
        if(iak_sgs_term%i_SGS_induction .gt. 0) then
          write(*,*) 'iak_sgs_uxb',                                     &
     &       iak_sgs_term%i_SGS_induction,                              &
     &       trim(wk_sph_sgs%name(iak_sgs_term%i_SGS_induction))
        end if
        if(iak_sgs_term%i_SGS_c_flux .gt. 0) then
          write(*,*) 'iak_sgs_cf', iak_sgs_term%i_SGS_c_flux,           &
     &       trim(wk_sph_sgs%name(iak_sgs_term%i_SGS_c_flux))
        end if
      end if
!
      end subroutine check_sph_sgs_addresses
!
! -------------------------------------------------------------------
!
      end module t_SPH_dynamic_model_coefs
