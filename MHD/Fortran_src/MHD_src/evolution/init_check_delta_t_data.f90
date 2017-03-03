!
!     module init_check_delta_t_data
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on July, 2006
!        Modified by H. Matsui on May, 2007
!
!!      subroutine s_init_check_delta_t_data(cd_prop, iphys, flex_data)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(flexible_stepping_data), intent(inout) :: flex_data
!
      module init_check_delta_t_data
!
      use m_precision
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_physical_property
      use t_phys_address
      use t_flex_delta_t_data
!
      implicit none
!
      private :: count_check_delta_t_data, set_check_delta_t_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_init_check_delta_t_data(cd_prop, iphys, flex_data)
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(flexible_stepping_data), intent(inout) :: flex_data
!
!
      call count_check_delta_t_data(cd_prop, iphys, flex_data)
!
      call alloc_check_delta_t_name(flex_data)
      call alloc_check_delta_t_rms(flex_data)
      call alloc_check_delta_t_data(flex_data)
!
      call set_check_delta_t_data(cd_prop, iphys, flex_data)
!
      end subroutine s_init_check_delta_t_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_check_delta_t_data                               &
     &         (cd_prop, iphys, flex_data)
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(flexible_stepping_data), intent(inout) :: flex_data
!
!
      flex_data%num_fld = 0
      flex_data%ntot_comp = 0
      if( (iphys%i_velo*iphys%i_chk_mom) .gt. izero) then
        flex_data%num_fld = flex_data%num_fld + 1
        flex_data%ntot_comp = flex_data%ntot_comp + 3
      end if
!
      if( (iphys%i_press*iphys%i_chk_press) .gt. izero) then
        flex_data%num_fld = flex_data%num_fld + 1
        flex_data%ntot_comp = flex_data%ntot_comp + 1
      end if
!
!
      if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if( (iphys%i_vecp*iphys%i_chk_uxb) .gt. izero) then
          flex_data%num_fld = flex_data%num_fld + 1
          flex_data%ntot_comp = flex_data%ntot_comp + 3
        end if
      end if
!
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if( (iphys%i_magne*iphys%i_chk_uxb) .gt. izero) then
          flex_data%num_fld = flex_data%num_fld + 1
          flex_data%ntot_comp = flex_data%ntot_comp + 3
        end if
      end if
!
      if( (iphys%i_mag_p*iphys%i_chk_potential) .gt. izero) then
        flex_data%num_fld = flex_data%num_fld + 1
        flex_data%ntot_comp = flex_data%ntot_comp + 1
      end if
!
!
      if( (iphys%i_temp*iphys%i_chk_heat) .gt. izero) then
        flex_data%num_fld = flex_data%num_fld + 1
        flex_data%ntot_comp = flex_data%ntot_comp + 1
      end if
!
      if( (iphys%i_light*iphys%i_chk_composit) .gt. izero) then
        flex_data%num_fld = flex_data%num_fld + 1
        flex_data%ntot_comp = flex_data%ntot_comp + 1
      end if
!
      end subroutine count_check_delta_t_data
!
! ----------------------------------------------------------------------
!
      subroutine set_check_delta_t_data(cd_prop, iphys, flex_data)
!
      use m_phys_labels
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(flexible_stepping_data), intent(inout) :: flex_data
!
      integer(kind = kint) :: icou
!
!
      icou = 0
      flex_data%istack_comp(0) = 0
      if( (iphys%i_velo*iphys%i_chk_mom) .gt. izero) then
        icou = icou + 1
        flex_data%i_drmax_v =         flex_data%istack_comp(icou-1) + 1
        flex_data%istack_comp(icou) = flex_data%istack_comp(icou-1) + 3
        flex_data%num_comp(icou) =    flex_data%num_comp(icou) +      3
        flex_data%fld_name(icou) =    fhd_velo
      end if
!
      if( (iphys%i_press*iphys%i_chk_press) .gt. izero) then
        icou = icou + 1
        flex_data%i_drmax_p =         flex_data%istack_comp(icou-1) + 1
        flex_data%istack_comp(icou) = flex_data%istack_comp(icou-1) + 1
        flex_data%num_comp(icou) =    flex_data%num_comp(icou) +      1
        flex_data%fld_name(icou) =    fhd_press
      end if
!
!
      if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if( (iphys%i_vecp*iphys%i_chk_uxb) .gt. izero) then
          icou = icou + 1
          flex_data%i_drmax_b = flex_data%istack_comp(icou-1) + 1
          flex_data%istack_comp(icou)                                   &
     &                        = flex_data%istack_comp(icou-1) + 3
          flex_data%num_comp(icou) = flex_data%num_comp(icou) + 3
          flex_data%fld_name(icou) = fhd_vecp
        end if
      end if
!
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if( (iphys%i_magne*iphys%i_chk_uxb) .gt. izero) then
          icou = icou + 1
          flex_data%i_drmax_b = flex_data%istack_comp(icou-1) + 1
          flex_data%istack_comp(icou)                                   &
     &                       = flex_data%istack_comp(icou-1) +  3
          flex_data%num_comp(icou) = flex_data%num_comp(icou) + 3
          flex_data%fld_name(icou) = fhd_magne
        end if
      end if
!
      if( (iphys%i_mag_p*iphys%i_chk_potential) .gt. izero) then
        icou = icou + 1
        flex_data%i_drmax_f =         flex_data%istack_comp(icou-1) + 1
        flex_data%istack_comp(icou) = flex_data%istack_comp(icou-1) + 1
        flex_data%num_comp(icou) = flex_data%num_comp(icou) + 1
        flex_data%fld_name(icou) = fhd_mag_potential
      end if
!
!
      if( (iphys%i_temp*iphys%i_chk_heat) .gt. izero) then
        icou = icou + 1
        flex_data%i_drmax_t =         flex_data%istack_comp(icou-1) + 1
        flex_data%istack_comp(icou) = flex_data%istack_comp(icou-1) + 1
        flex_data%num_comp(icou) =    flex_data%num_comp(icou) +      1
        flex_data%fld_name(icou) =    fhd_temp
      end if
!
      if( (iphys%i_light*iphys%i_chk_composit) .gt. izero) then
        icou = icou + 1
        flex_data%i_drmax_d =         flex_data%istack_comp(icou-1) + 1
        flex_data%istack_comp(icou) = flex_data%istack_comp(icou-1) + 1
        flex_data%num_comp(icou) =    flex_data%num_comp(icou) +      1
        flex_data%fld_name(icou) =    fhd_light
      end if
!
      end subroutine set_check_delta_t_data
!
! ----------------------------------------------------------------------
!
      end module init_check_delta_t_data
