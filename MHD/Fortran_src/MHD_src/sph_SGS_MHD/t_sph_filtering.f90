!>@file   t_sph_filtering.f90
!!@brief  module t_sph_filtering
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine alloc_sph_filter_type(num_filter, dynamic_SPH)
!!      subroutine dealloc_sph_filter_type(dynamic_SPH)
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!
!!      subroutine init_filter_4_SPH_MHD                                &
!!     &         (sph, sph_grps, leg, num_sph_filteres, sph_filters)
!!        type(sph_grids), intent(in) ::  sph
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(sph_filters_type), intent(inout) :: sph_filters(1)
!!      subroutine init_work_4_SGS_sph_mhd(SGS_par, sph_d_grp, MHD_prop,&
!!     &          iak_sgs_term, icomp_sgs_term, sgs_coefs, wk_sgs)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(SGS_term_address), intent(inout) :: iak_sgs_term
!!        type(SGS_term_address), intent(inout) :: icomp_sgs_term
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!@endverbatim
!!
!
      module t_sph_filtering
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_parameter
      use t_spheric_group
      use t_sph_filtering_data
      use t_schmidt_poly_on_rtm
      use t_filter_coefficients
      use t_SGS_control_parameter
      use t_SGS_model_coefs
      use t_SGS_buoyancy_sph
      use t_ele_info_4_dynamic
      use t_field_data_IO
      use t_groups_sph_dynamic
!
      implicit none
!
!>      Structure of work area for dyanmic SGS model for spectrum dynamo
      type dynamic_SGS_data_4_sph
!>         Field adddress for dynamic SGS model
        type(SGS_term_address) :: iak_sgs_term
!>         Component adddress for dynamic SGS model
        type(SGS_term_address) :: icomp_sgs_term
!>         Data array for dynamic SGS model
        type(SGS_coefficients_type) :: sgs_coefs
!>         Work area for dynamic SGS model
        type(dynamic_model_data) :: wk_sgs
!
!>         Work area for dynamic SGS model
        type(work_4_sph_SGS_buoyancy) :: wk_sgs_buo
!
!>         Number of filter functions
        integer(kind = kint) :: num_sph_filteres
!>         Filter functions
        type(sph_filters_type), allocatable :: sph_filters(:)
!
        type(field_IO) :: Csim_S_IO
!        type(field_IO) :: Cdiff_S_IO
!
        type(sph_dynamic_model_group) :: sph_d_grp
      end type dynamic_SGS_data_4_sph
!
      character(len=kchara), parameter :: filter_head = 'radial_filter'
!
      private :: const_sph_radial_filter, const_radial_filter
      private :: const_filter_on_sphere
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sph_filter_type(num_filter, dynamic_SPH)
!
      integer(kind = kint), intent(in) :: num_filter
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      dynamic_SPH%num_sph_filteres = num_filter
      allocate(dynamic_SPH%sph_filters(dynamic_SPH%num_sph_filteres))
!
      end subroutine alloc_sph_filter_type
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_filter_type(dynamic_SPH)
!
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      deallocate(dynamic_SPH%sph_filters)
!
      end subroutine dealloc_sph_filter_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_filter_4_SPH_MHD                                  &
     &         (sph, sph_grps, leg, num_sph_filteres, sph_filters)
!
      use calypso_mpi
      use wider_radial_filter_data
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: num_sph_filteres
      type(sph_grids), intent(in) ::  sph
      type(sph_group_data), intent(in) :: sph_grps
      type(legendre_4_sph_trans), intent(in) :: leg
      type(sph_filters_type), intent(inout)                             &
     &                        :: sph_filters(num_sph_filteres)
!
      integer(kind = kint)  :: i, i1, i2
      integer(kind = kint)  :: id_file
      character(len=kchara) :: fname_tmp, file_name
!
!
      do i = 1, num_sph_filteres
        i1 = sph_filters(i)%id_1st_ref_filter
        i2 = sph_filters(i)%id_2nd_ref_filter
        if(sph_filters(i)%itype_radial_filter                           &
     &                  .eq. iflag_recursive_filter) then
          call radial_wider_fileters                                    &
     &       (sph%sph_rj, sph_filters(i1)%r_filter,                     &
     &        sph_filters(i2)%r_filter, sph_filters(i)%r_filter)
        else
          if(iflag_debug.gt.0) write(*,*)' const_sph_radial_filter'
          call const_sph_radial_filter                                  &
     &       (sph%sph_rj, sph_grps, sph_filters(i))
        end if
!
        if(iflag_debug.gt.0) write(*,*)' const_filter_on_sphere'
        call const_filter_on_sphere(sph_filters(i)%itype_sph_filter,    &
     &      sph%sph_params%l_truncation,                                &
     &      sph_filters(i1)%sph_moments, sph_filters(i2)%sph_moments,   &
     &      sph_filters(i1)%sph_filter, sph_filters(i2)%sph_filter,     &
     &      sph_filters(i)%sph_moments, sph_filters(i)%sph_filter)
!
        if(iflag_debug.gt.0) write(*,*)' const_filter_on_sphere'
        call init_sph_2nd_order_moments_rtp                             &
     &     (sph%sph_rtp, sph%sph_rj, leg, sph_filters(i))
!
        if(iflag_debug .gt. 0) then
          fname_tmp = add_int_suffix(i, filter_head)
          file_name = add_dat_extension(fname_tmp)
          call check_radial_filter                                      &
     &       (file_name, sph%sph_rj, sph_filters(i)%r_filter)
!
          id_file = 50+my_rank
          write(id_file,*) 'check_horiz_filter_weight for no. ', i
          call check_horiz_filter_weight                                &
     &       (id_file, sph_filters(i)%sph_filter)
        end if
        if(i_debug .gt. 0) then
          id_file = 50+my_rank
          write(id_file,*) 'check_sph_2nd_moments for no. ',            &
     &         i, my_rank
          call check_sph_2nd_moments                                    &
     &       (id_file, sph%sph_rtp, leg, sph_filters(i))
        end if
      end do
!
      end subroutine init_filter_4_SPH_MHD
!
! ----------------------------------------------------------------------
!
      subroutine init_work_4_SGS_sph_mhd(SGS_par, sph_d_grp, MHD_prop,  &
     &          iak_sgs_term, icomp_sgs_term, sgs_coefs, wk_sgs)
!
      use t_physical_property
      use count_sgs_components
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(SGS_term_address), intent(inout) :: iak_sgs_term
      type(SGS_term_address), intent(inout) :: icomp_sgs_term
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
      integer(kind = kint) :: num_SGS_terms, ntot_SGS_comps
!
!
      call s_count_sgs_components(SGS_par%model_p,                      &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    num_SGS_terms, ntot_SGS_comps)
      call alloc_sgs_coefs_layer(sph_d_grp%ngrp_dynamic,                &
     &    num_SGS_terms, ntot_SGS_comps, wk_sgs)
!
      call set_sph_sgs_addresses                                        &
     &   (SGS_par%model_p, MHD_prop%fl_prop, MHD_prop%cd_prop,          &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    iak_sgs_term, icomp_sgs_term, wk_sgs, sgs_coefs)
      call check_sph_sgs_addresses                                      &
     &   (iak_sgs_term, icomp_sgs_term, wk_sgs, sgs_coefs)
!
      end subroutine init_work_4_SGS_sph_mhd
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_sph_radial_filter(sph_rj, sph_grps,              &
     &          sph_filters)
!
      use calypso_mpi
      use wider_radial_filter_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_group_data), intent(in) :: sph_grps
      type(sph_filters_type), intent(inout) :: sph_filters
!
!
      call alloc_sph_filter_moms(sph_filters%r_moments)
      call cal_r_gaussian_moments(sph_filters%width,                    &
     &    sph_filters%r_moments)
!
      call const_radial_filter(sph_rj, sph_grps, sph_filters%r_moments, &
     &    sph_filters%kr_SGS_in, sph_filters%kr_SGS_out,                &
     &    sph_filters%r_filter)
!
      end subroutine const_sph_radial_filter
!
! ----------------------------------------------------------------------
!
      subroutine const_filter_on_sphere                                 &
     &         (itype_sph_filter,  l_truncation,                        &
     &          ref1_moments, ref2_moments, ref1_filter, ref2_filter,   &
     &          sph_moments, sph_filter)
!
      use cal_sph_filtering_data
!
      integer(kind = kint), intent(in) :: itype_sph_filter
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_gaussian_filter), intent(in) :: ref1_filter, ref2_filter
      type(sph_filter_moment), intent(in) :: ref1_moments, ref2_moments
!
      type(sph_gaussian_filter), intent(inout) :: sph_filter
      type(sph_filter_moment), intent(inout) :: sph_moments
!
!
      if(itype_sph_filter .eq. iflag_recursive_filter) then
        sph_moments%num_momentum                                        &
     &      = max(ref1_moments%num_momentum, ref2_moments%num_momentum)
      end if
!
      call alloc_sph_filter_weights(l_truncation, sph_filter)
      call alloc_sph_filter_moms(sph_moments)
!
!
      if(itype_sph_filter .eq. iflag_recursive_filter) then
        if(iflag_debug.gt.0) write(*,*)' set_sph_recursive_filter'
        call set_sph_recursive_filter                                   &
     &     (sph_filter%l_truncation, sph_moments%num_momentum,          &
     &      ref1_moments%num_momentum, ref2_moments%num_momentum,       &
     &      ref1_filter%weight, ref1_moments%filter_mom,                &
     &      ref2_filter%weight, ref2_moments%filter_mom,                &
     &      sph_filter%weight, sph_moments%filter_mom)
!
      else if(itype_sph_filter .eq. iflag_cutoff_filter) then
        if(iflag_debug.gt.0) write(*,*)' set_sph_cutoff_filter'
        call set_sph_cutoff_filter(sph_filter%l_truncation,             &
     &      sph_filter%f_width, sph_filter%weight,                      &
     &      sph_moments%num_momentum, sph_moments%filter_mom)
      else
        if(iflag_debug.gt.0) write(*,*)' set_sph_gaussian_filter'
        call set_sph_gaussian_filter(sph_filter%l_truncation,           &
     &      sph_filter%f_width, sph_filter%weight,                      &
     &      sph_moments%num_momentum, sph_moments%filter_mom)
      end if
!
      end subroutine const_filter_on_sphere
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_radial_filter(sph_rj, sph_grps, r_moments,       &
     &          kmin_rtp_OC, kmax_rtp_OC, r_filter)
!
      use cal_radial_filtering_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_group_data), intent(in) :: sph_grps
      type(sph_filter_moment), intent(in) :: r_moments
!
      integer(kind = kint), intent(inout) :: kmin_rtp_OC, kmax_rtp_OC
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: num_rj_OC, kmin_rj_OC, kmax_rj_OC
      integer(kind = kint) :: num_rtp_OC
!
!
      r_filter%ngrp_node = 1
      call alloc_num_filtering_comb(np_smp, r_filter)
      r_filter%group_name(1) = 'outer_core'
!
      call count_radial_point_4_filter(sph_rj, r_filter)
!
      call alloc_inod_filter_comb(r_filter)
!
      call count_fiiltering_area(r_moments%num_momentum, sph_rj,        &
     &    sph_grps%radial_rj_grp, sph_grps%radial_rtp_grp, r_filter,    &
     &    num_rj_OC, kmin_rj_OC, kmax_rj_OC,                            &
     &    num_rtp_OC, kmin_rtp_OC, kmax_rtp_OC)
!
      call alloc_3d_filter_comb(r_filter)
      call alloc_3d_filter_func(r_filter)
!
      call set_filtering_points(num_rj_OC, kmin_rj_OC, kmax_rj_OC,      &
     &    r_moments%num_momentum, r_moments%nfilter_sides,              &
     &    sph_rj, r_filter)

      call cal_radial_fileters(kmin_rj_OC, kmax_rj_OC,                  &
     &    r_moments%num_momentum, r_moments%nfilter_sides,              &
     &    r_moments%filter_mom, sph_rj, r_filter)
!
      end subroutine const_radial_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_sph_sgs_addresses                                  &
     &          (SGS_param, fl_prop, cd_prop, ht_prop, cp_prop,         &
     &           iak_sgs_term, icomp_sgs_term, wk_sgs, sgs_coefs)
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_physical_property
      use t_SGS_model_coefs
      use t_SGS_term_labels
      use t_SGS_control_parameter
!
      use m_SGS_term_labels
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(SGS_term_address), intent(inout) :: iak_sgs_term
      type(SGS_term_address), intent(inout) :: icomp_sgs_term
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
      integer(kind = kint) :: i_cmp, i_fld, id, jd, num_comp
!
!
       i_cmp = 1
       i_fld = 1
       id = 1
       jd = 1
       num_comp = 0
       if (ht_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_h_flux = i_cmp
           iak_sgs_term%i_SGS_h_flux =  i_fld
           wk_sgs%name(i_fld) = SGS_heat_flux%name
           num_comp = 3
           i_cmp = i_cmp + num_comp
           i_fld = i_fld + 1
         end if
       end if
!
       num_comp = 0
       if (fl_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_m_flux = i_cmp
           iak_sgs_term%i_SGS_m_flux =  i_fld
           wk_sgs%name(i_fld) = SGS_momentum_flux%name
           num_comp = 6
           i_cmp = i_cmp + num_comp
           i_fld = i_fld + 1
         end if
       end if
!
       num_comp = 0
       if (fl_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_Lorentz = i_cmp
           iak_sgs_term%i_SGS_Lorentz =  i_fld
           wk_sgs%name(i_fld) = SGS_maxwell_tensor%name
           num_comp = 6
           i_cmp = i_cmp + num_comp
           i_fld = i_fld + 1
         end if
       end if
!
       num_comp = 0
       if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
          if(fl_prop%iflag_4_gravity) then
            icomp_sgs_term%i_SGS_buoyancy = i_cmp
            iak_sgs_term%i_SGS_buoyancy =  i_fld
            wk_sgs%name(i_fld) = SGS_buoyancy%name
            num_comp = 6
            i_cmp = i_cmp + num_comp
            i_fld = i_fld + 1
          end if
         end if
       end if
!
       num_comp = 0
       if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
          if(fl_prop%iflag_4_composit_buo) then
            icomp_sgs_term%i_SGS_comp_buo = i_cmp
            iak_sgs_term%i_SGS_comp_buo =  i_fld
            wk_sgs%name(i_fld) = SGS_composit_buoyancy%name
            num_comp = 6
            i_cmp = i_cmp + num_comp
            i_fld = i_fld + 1
          end if
        end if
       end if
!
       num_comp = 0
       if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_induction = i_cmp
           iak_sgs_term%i_SGS_induction =  i_fld
           wk_sgs%name(i_fld) = SGS_induction%name
           num_comp = 3
           i_cmp = i_cmp + num_comp
           i_fld = i_fld + 1
         end if
       end if
       if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_induction = i_cmp
           iak_sgs_term%i_SGS_induction =  i_fld
           wk_sgs%name(i_fld) = SGS_induction%name
           num_comp = 3
           i_cmp = i_cmp + num_comp
           i_fld = i_fld + 1
         end if
       end if
!
       num_comp = 0
       if (cp_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_c_flux = i_cmp
           iak_sgs_term%i_SGS_c_flux =  i_fld
           wk_sgs%name(i_fld) = SGS_composit_flux%name
           num_comp = 3
           i_cmp = i_cmp + num_comp
           i_fld = i_fld + 1
         end if
       end if
!
     end subroutine set_sph_sgs_addresses
!
!  ------------------------------------------------------------------
!
      subroutine check_sph_sgs_addresses                                &
     &         (iak_sgs_term, icomp_sgs_term, wk_sgs, sgs_coefs)
!
      use calypso_mpi
!
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_term_labels
      use t_SGS_model_coefs
      use t_SGS_control_parameter
!
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: icomp_sgs_term
!
      type(dynamic_model_data), intent(in) :: wk_sgs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'num_sgs_kinds', wk_sgs%num_kinds
        write(*,*) 'num_sgs_coefs', wk_sgs%ntot_comp
!
        if(iak_sgs_term%i_SGS_h_flux .gt. 0) then
          write(*,*) 'iak_sgs_hf',                                      &
     &       iak_sgs_term%i_SGS_h_flux, icomp_sgs_term%i_SGS_h_flux,    &
     &                             sgs_coefs%Csim_SGS_hf%num_comp,      &
     &       trim(wk_sgs%name(iak_sgs_term%i_SGS_h_flux))
        end if
        if(iak_sgs_term%i_SGS_m_flux .gt. 0) then
          write(*,*) 'iak_sgs_mf',                                      &
     &       iak_sgs_term%i_SGS_m_flux, icomp_sgs_term%i_SGS_m_flux,    &
     &                             sgs_coefs%Csim_SGS_mf%num_comp,      &
     &       trim(wk_sgs%name(iak_sgs_term%i_SGS_m_flux))
        end if
        if(iak_sgs_term%i_SGS_Lorentz .gt. 0) then
          write(*,*) 'iak_sgs_lor',                                     &
     &       iak_sgs_term%i_SGS_Lorentz, icomp_sgs_term%i_SGS_Lorentz,  &
     &                             sgs_coefs%Csim_SGS_lor%num_comp,     &
     &       trim(wk_sgs%name(iak_sgs_term%i_SGS_Lorentz))
        end if
        if(iak_sgs_term%i_SGS_buoyancy .gt. 0) then
          write(*,*) 'iak_sgs_tbuo',                                    &
     &      iak_sgs_term%i_SGS_buoyancy, icomp_sgs_term%i_SGS_buoyancy, &
     &                              sgs_coefs%Csim_SGS_cbuo%num_comp,   &
     &      trim(wk_sgs%name(iak_sgs_term%i_SGS_buoyancy))
        end if
        if(iak_sgs_term%i_SGS_comp_buo .gt. 0) then
          write(*,*) 'iak_sgs_cbuo',                                    &
     &      iak_sgs_term%i_SGS_comp_buo, icomp_sgs_term%i_SGS_comp_buo, &
     &                              sgs_coefs%Csim_SGS_cbuo%num_comp,   &
     &      trim(wk_sgs%name(iak_sgs_term%i_SGS_comp_buo))
        end if
        if(iak_sgs_term%i_SGS_induction .gt. 0) then
          write(*,*) 'iak_sgs_uxb',                                     &
     &       iak_sgs_term%i_SGS_induction,                              &
     &       icomp_sgs_term%i_SGS_induction,                            &
     &                              sgs_coefs%Csim_SGS_uxb%num_comp,    &
     &       trim(wk_sgs%name(iak_sgs_term%i_SGS_induction))
        end if
        if(iak_sgs_term%i_SGS_c_flux .gt. 0) then
          write(*,*) 'iak_sgs_cf',                                      &
     &       iak_sgs_term%i_SGS_c_flux, icomp_sgs_term%i_SGS_c_flux,    &
     &                              sgs_coefs%Csim_SGS_cf%num_comp,     &
     &       trim(wk_sgs%name(iak_sgs_term%i_SGS_c_flux))
        end if
      end if
!
      end subroutine check_sph_sgs_addresses
!
! -------------------------------------------------------------------
!
      end module t_sph_filtering
