!init_check_delta_t_data.f90
!     module init_check_delta_t_data
!
!      Written by H. Matsui on Nov., 2009
!
!      subroutine s_init_check_delta_t_data
!
      module init_check_delta_t_data
!
      use m_precision
!
      use m_control_parameter
      use m_flex_delta_t_data
!
      implicit  none
!
      private :: count_check_delta_t_data, set_check_delta_t_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_init_check_delta_t_data
!
!
      call count_check_delta_t_data
!
      call allocate_check_delta_t_name
      call allocate_check_delta_t_rms
!
      call set_check_delta_t_data
!
      end subroutine s_init_check_delta_t_data
!
! ----------------------------------------------------------------------
!
      subroutine count_check_delta_t_data
!
      use m_node_phys_address
!
!
      nfld_dratio = 0
      ntot_dratio = 0
      if( (iphys%i_velo*iphys%i_chk_mom) .gt. izero) then
        nfld_dratio = nfld_dratio + 1
        ntot_dratio = ntot_dratio + 3
      end if
!
      if( (iphys%i_press*iphys%i_chk_press) .gt. izero) then
        nfld_dratio = nfld_dratio + 1
        ntot_dratio = ntot_dratio + 1
      end if
!
!
      if(iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if( (iphys%i_vecp*iphys%i_chk_uxb) .gt. izero) then
          nfld_dratio = nfld_dratio + 1
          ntot_dratio = ntot_dratio + 3
        end if
      end if
!
      if(iflag_t_evo_4_magne .gt. id_no_evolution) then
        if( (iphys%i_magne*iphys%i_chk_uxb) .gt. izero) then
          nfld_dratio = nfld_dratio + 1
          ntot_dratio = ntot_dratio + 3
        end if
      end if
!
      if( (iphys%i_mag_p*iphys%i_chk_potential) .gt. izero) then
        nfld_dratio = nfld_dratio + 1
        ntot_dratio = ntot_dratio + 1
      end if
!
!
      if( (iphys%i_temp*iphys%i_chk_heat) .gt. izero) then
        nfld_dratio = nfld_dratio + 1
        ntot_dratio = ntot_dratio + 1
      end if
!
      if( (iphys%i_light*iphys%i_chk_composit) .gt. izero) then
        nfld_dratio = nfld_dratio + 1
        ntot_dratio = ntot_dratio + 1
      end if
!
      end subroutine count_check_delta_t_data
!
! ----------------------------------------------------------------------
!
      subroutine set_check_delta_t_data
!
      use m_node_phys_address
      use m_phys_labels
!
      integer(kind = kint) :: icou
!
!
      icou = 0
      istack_dratio(0) = 0
      if( (iphys%i_velo*iphys%i_chk_mom) .gt. izero) then
        icou = icou + 1
        i_drmax_v =           istack_dratio(icou-1) + 1
        istack_dratio(icou) = istack_dratio(icou-1) + 3
        ncomp_dratio(icou) = ncomp_dratio(icou) +     3
        d_ratio_name(icou) = fhd_velo
      end if
!
      if( (iphys%i_press*iphys%i_chk_press) .gt. izero) then
        icou = icou + 1
        i_drmax_p =           istack_dratio(icou-1) + 1
        istack_dratio(icou) = istack_dratio(icou-1) + 1
        ncomp_dratio(icou) = ncomp_dratio(icou) +     1
        d_ratio_name(icou) = fhd_press
      end if
!
!
      if(iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if( (iphys%i_vecp*iphys%i_chk_uxb) .gt. izero) then
          icou = icou + 1
          i_drmax_b =           istack_dratio(icou-1) + 1
          istack_dratio(icou) = istack_dratio(icou-1) + 3
          ncomp_dratio(icou) = ncomp_dratio(icou) +     3
          d_ratio_name(icou) = fhd_vecp
        end if
      end if
!
      if(iflag_t_evo_4_magne .gt. id_no_evolution) then
        if( (iphys%i_magne*iphys%i_chk_uxb) .gt. izero) then
          icou = icou + 1
          i_drmax_b =           istack_dratio(icou-1) + 1
          istack_dratio(icou) = istack_dratio(icou-1) + 3
          ncomp_dratio(icou) = ncomp_dratio(icou) +     3
          d_ratio_name(icou) = fhd_magne
        end if
      end if
!
      if( (iphys%i_mag_p*iphys%i_chk_potential) .gt. izero) then
        icou = icou + 1
        i_drmax_f =           istack_dratio(icou-1) + 1
        istack_dratio(icou) = istack_dratio(icou-1) + 1
        ncomp_dratio(icou) = ncomp_dratio(icou) +     1
        d_ratio_name(icou) = fhd_mag_potential
      end if
!
!
      if( (iphys%i_temp*iphys%i_chk_heat) .gt. izero) then
        icou = icou + 1
        i_drmax_t =           istack_dratio(icou-1) + 1
        istack_dratio(icou) = istack_dratio(icou-1) + 1
        ncomp_dratio(icou) = ncomp_dratio(icou) +     1
        d_ratio_name(icou) = fhd_temp
      end if
!
      if( (iphys%i_light*iphys%i_chk_composit) .gt. izero) then
        icou = icou + 1
        i_drmax_d =           istack_dratio(icou-1) + 1
        istack_dratio(icou) = istack_dratio(icou-1) + 1
        ncomp_dratio(icou) = ncomp_dratio(icou) +     1
        d_ratio_name(icou) = fhd_light
      end if
!
      end subroutine set_check_delta_t_data
!
! ----------------------------------------------------------------------
!
      end module init_check_delta_t_data
