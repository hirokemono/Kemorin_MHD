!>@file   set_bc_sph_mhd.f90
!!@brief  module set_bc_sph_mhd
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_bc_sph_mhd
!!@endverbatim
!
      module set_bc_sph_mhd
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_control_params_sph_MHD
!
      implicit none
!
      private :: set_sph_bc_temp_sph
      private :: set_sph_bc_magne_sph, set_sph_bc_composition_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_bc_sph_mhd
!
      use set_bc_flag_sph_velo
!
!
      if (iflag_t_evo_4_velo .gt. 0) then
        call set_sph_bc_velo_sph
      end if
!
      if (iflag_t_evo_4_temp .gt. 0) then
        call set_sph_bc_temp_sph
      end if
!
      if (iflag_t_evo_4_magne .gt. 0) then
        call set_sph_bc_magne_sph
      end if
!
      if (iflag_t_evo_4_composit .gt. 0) then
        call set_sph_bc_composition_sph
      end if
!
      end subroutine s_set_bc_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_temp_sph
!
      use m_spheric_parameter
      use m_bc_data_list
      use m_surf_data_list
      use m_constants
      use m_sph_spectr_data
!
!
      integer(kind = kint) :: i
!
!
      call allocate_temp_bc_array( nidx_rj(2) )
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'num_bc_h_flux', num_bc_h_flux
        write(*,*) 'ibc_h_flux_type', ibc_h_flux_type
        write(*,*) 'bc_h_flux_magnitude', bc_h_flux_magnitude
      end if
!
      do i = 1, num_bc_h_flux
        if ( ibc_h_flux_type(i)  .eq. 0) then
          if     (bc_h_flux_name(i) .eq. 'ICB_surf'                     &
     &       .or. bc_h_flux_name(i) .eq. 'ICB') then 
            iflag_icb_temp = iflag_fixed_flux
            if(idx_rj_degree_zero .gt. 0) then
              h_flux_ICB_bc(idx_rj_degree_zero)                         &
     &                       = - bc_h_flux_magnitude(i)
            end if
          else if(bc_h_flux_name(i) .eq. 'CMB_surf'                     &
     &       .or. bc_h_flux_name(i) .eq. 'CMB') then 
            iflag_cmb_temp =  iflag_fixed_flux
            if(idx_rj_degree_zero .gt. 0) then
              h_flux_CMB_bc(idx_rj_degree_zero)                         &
     &                       = bc_h_flux_magnitude(i)
            end if
          end if
        end if
      end do
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'iflag_icb_temp', iflag_icb_temp
        write(*,*) 'iflag_cmb_temp', iflag_cmb_temp
        write(*,*)  h_flux_CMB_bc(1)
      end if
!
      do i = 1, num_bc_e
        if ( ibc_e_type(i)  .eq. iflag_bc_fix_flux) then
          if(bc_e_name(i) .eq. 'ICB') then 
            iflag_icb_temp =  iflag_fixed_flux
            if(idx_rj_degree_zero .gt. 0) then
              h_flux_ICB_bc(idx_rj_degree_zero) = - bc_e_magnitude(i)
            end if
          else if(bc_e_name(i) .eq. 'CMB') then 
            iflag_cmb_temp =  iflag_fixed_flux
            if(idx_rj_degree_zero .gt. 0) then
              h_flux_CMB_bc(idx_rj_degree_zero) =  bc_e_magnitude(i)
            end if
          end if
        end if
!
        if ( ibc_e_type(i)  .eq. iflag_bc_fix_s) then
          if(bc_e_name(i) .eq. 'ICB') then 
            iflag_icb_temp =  iflag_fixed_field
            if(idx_rj_degree_zero .gt. 0) then
              temp_ICB_bc(idx_rj_degree_zero) = bc_e_magnitude(i)
            end if
          else if(bc_e_name(i) .eq. 'CMB') then 
            iflag_cmb_temp =  iflag_fixed_field
            if(idx_rj_degree_zero .gt. 0) then
              temp_CMB_bc(idx_rj_degree_zero) = bc_e_magnitude(i)
            end if
          end if
        end if
      end do
!
      if(idx_rj_degree_zero.gt.0 .and. iflag_4_ref_temp.eq.100) then
        temp_ICB_bc(idx_rj_degree_zero)                                 &
     &   = temp_ICB_bc(idx_rj_degree_zero) - reftemp_rj(nlayer_ICB,0)
        temp_CMB_bc(idx_rj_degree_zero)                                 &
     &   = temp_CMB_bc(idx_rj_degree_zero) - reftemp_rj(nlayer_CMB,0)
        h_flux_ICB_bc(idx_rj_degree_zero)                               &
     &   = h_flux_ICB_bc(idx_rj_degree_zero) - reftemp_rj(nlayer_ICB,1)
        h_flux_CMB_bc(idx_rj_degree_zero)                               &
     &   = h_flux_CMB_bc(idx_rj_degree_zero) - reftemp_rj(nlayer_CMB,1)
      end if
!
      if(iflag_debug .gt. 0 .and. idx_rj_degree_zero .gt. 0) then
        write(*,*) 'iflag_icb_temp', iflag_icb_temp,                    &
     &             reftemp_rj(nlayer_ICB,0),                            &
     &             temp_ICB_bc(idx_rj_degree_zero),                     &
     &             h_flux_ICB_bc(idx_rj_degree_zero)
        write(*,*) 'iflag_cmb_temp', iflag_cmb_temp,                    &
     &             reftemp_rj(nlayer_CMB,0),                            &
     &             temp_CMB_bc(idx_rj_degree_zero),                     &
     &             h_flux_CMB_bc(idx_rj_degree_zero)
      end if
!
      end subroutine set_sph_bc_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_magne_sph
!
      use m_bc_data_list
      use m_surf_data_list
!
!
      integer(kind = kint) :: i
!
!
      iflag_icb_magne = iflag_sph_insulator
      iflag_cmb_magne = iflag_sph_insulator
!
      do i = 1, num_bc_b
        if(bc_b_name(i) .eq. 'ICB') then
          if(ibc_b_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_icb_magne =  iflag_radial_magne
          end if
        end if
!
        if(bc_b_name(i) .eq. 'CMB') then
          if(ibc_b_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_cmb_magne =  iflag_radial_magne
          end if
        end if
!
        if(bc_b_name(i) .eq. 'to_Center') then
          if      (ibc_b_type(i) .eq. iflag_sph_2_center) then
            iflag_icb_magne =  iflag_sph_fill_center
          end if
        end if
      end do
!
!
      do i = 1, num_bc_bs
        if(bc_bs_name(i) .eq. 'ICB') then
          if(ibc_bs_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_icb_magne =  iflag_radial_magne
          end if
        end if
!
        if(bc_bs_name(i) .eq. 'CMB') then
          if(ibc_bs_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_cmb_magne =  iflag_radial_magne
          end if
        end if
!
        if(bc_bs_name(i) .eq. 'to_Center') then
          if      (ibc_bs_type(i) .eq. iflag_sph_2_center) then
            iflag_icb_magne =  iflag_sph_fill_center
          end if
        end if
      end do
!
!
      end subroutine set_sph_bc_magne_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_composition_sph
!
      use m_spheric_parameter
      use m_bc_data_list
      use m_surf_data_list
      use m_sph_spectr_data
!
      integer(kind = kint) :: i
!
!
      call allocate_dscalar_bc_array( nidx_rj(2) )
!
!      Boundary setting using surface group data
!
      do i = 1, num_surf_composition
        if ( isurf_composit_type(i)  .eq. 0) then
          if     (surf_composit_name(i) .eq. 'ICB_surf'                 &
     &       .or. surf_composit_name(i) .eq. 'ICB') then
            iflag_icb_composition =  iflag_fixed_flux
            if(idx_rj_degree_zero .gt. 0) then
              c_flux_ICB_bc(idx_rj_degree_zero)                         &
     &                       = - surf_composit_magnitude(i)
            end if
          else if(surf_composit_name(i) .eq. 'CMB_surf'                 &
     &       .or. surf_composit_name(i) .eq. 'CMB') then
            iflag_cmb_composition =  iflag_fixed_flux
            if(idx_rj_degree_zero .gt. 0) then
              c_flux_CMB_bc(idx_rj_degree_zero)                         &
     &                       = surf_composit_magnitude(i)
            end if
          end if
        end if
      end do
!
!      Boundary setting using boundary group data
!
      do i = 1, num_bc_composit
        if ( ibc_composit_type(i)  .eq. iflag_bc_fix_flux) then
          if(bc_composit_name(i) .eq. 'ICB') then
            iflag_icb_composition =  iflag_fixed_flux
            if(idx_rj_degree_zero .gt. 0) then
              c_flux_ICB_bc(idx_rj_degree_zero)                         &
     &                       = - bc_composit_magnitude(i)
            end if
          else if(bc_composit_name(i) .eq. 'CMB') then
            iflag_cmb_composition =  iflag_fixed_flux
            if(idx_rj_degree_zero .gt. 0) then
              c_flux_CMB_bc(idx_rj_degree_zero)                         &
     &                       = bc_composit_magnitude(i)
            end if
          end if
!
        else if ( ibc_composit_type(i)  .eq. iflag_bc_fix_s) then
          if(bc_composit_name(i) .eq. 'ICB') then
            iflag_icb_composition =  iflag_fixed_field
            if(idx_rj_degree_zero .gt. 0) then
              composition_ICB_bc(idx_rj_degree_zero)                    &
     &                       = bc_composit_magnitude(i)
            end if
          else if(bc_composit_name(i) .eq. 'CMB') then 
            iflag_cmb_composition =  iflag_fixed_field
            if(idx_rj_degree_zero .gt. 0) then
              composition_CMB_bc(idx_rj_degree_zero)                    &
     &                       = bc_composit_magnitude(i)
            end if
          end if
        end if
      end do
!
      end subroutine set_sph_bc_composition_sph
!
! -----------------------------------------------------------------------
!
      end module set_bc_sph_mhd
