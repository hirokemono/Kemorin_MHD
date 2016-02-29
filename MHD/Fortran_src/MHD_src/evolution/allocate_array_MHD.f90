!
!     module allocate_array_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on July, 2006
!        Modified by H. Matsui on May, 2007
!
!!      subroutine allocate_array(node, ele, iphys, nod_fld,            &
!!     &          m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, label_sim)
!!      subroutine s_init_check_delta_t_data(iphys)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(inout) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(lumped_mass_matrices), intent(inout) :: m_lump
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      module allocate_array_MHD
!
      use m_precision
      use m_machine_parameter
      use m_control_parameter
!
      use calypso_mpi
!
      use t_phys_address
      use t_phys_data
!
      implicit none
!
      private :: count_int_vol_data
      private :: count_check_delta_t_data, set_check_delta_t_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_array(node, ele, iphys, nod_fld,              &
     &          m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, label_sim)
!
      use m_element_phys_data
      use m_phys_constants
      use m_mean_square_values
      use m_SGS_address
!
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_FEM_phys_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(inout) :: iphys
      type(phys_data), intent(inout) :: nod_fld
      type(lumped_mass_matrices), intent(inout) :: m_lump
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      character(len=kchara), intent(inout) :: label_sim
!
!
      label_sim = 'GeoFEM_MHD'
!
      if (iflag_debug.ge.1) write(*,*) 'alloc_finite_elem_mat'
      call alloc_finite_elem_mat                                        &
     &   (node, ele, m_lump, fem_wk, f_l, f_nl)
      call alloc_mass_mat_fluid(node%numnod, mhd_fem_wk)
      call alloc_mass_mat_conduct(node%numnod, mhd_fem_wk)
!
      if (iflag_debug.ge.1) write(*,*) 'allocate_int_vol_data'
      call alloc_int_vol_data                                           &
     &   (ele%numele, node%max_nod_smp, nod_fld, mhd_fem_wk)
      call count_int_vol_data(mhd_fem_wk)
      call alloc_int_vol_dvx(ele%numele, mhd_fem_wk)
      call set_SGS_addresses
!
!  allocation for field values
      if (iflag_debug.ge.1)  write(*,*) 'set_field_address_type'
      call set_field_address_type(node%numnod, nod_fld, iphys)
      if (iflag_debug.ge.1)  write(*,*) 'initialize_ele_field_data'
      call initialize_ele_field_data(ele%numele)
!
      if ( iflag_debug.ge.1 ) write(*,*) 'set_mean_square_values'
      call count_mean_square_values(nod_fld)
      call set_mean_square_values(nod_fld)
!
      end subroutine allocate_array
!
! ----------------------------------------------------------------------
!
      subroutine s_init_check_delta_t_data(iphys)
!
      use m_flex_delta_t_data
!
      type(phys_address), intent(in) :: iphys
!
!
      call count_check_delta_t_data(iphys)
!
      call allocate_check_delta_t_name
      call allocate_check_delta_t_rms
!
      call set_check_delta_t_data(iphys)
!
      end subroutine s_init_check_delta_t_data
!
! ----------------------------------------------------------------------
!
      subroutine count_int_vol_data(mhd_fem_wk)
!
      use m_control_parameter
      use m_phys_labels
      use t_MHD_finite_element_mat
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      mhd_fem_wk%n_dvx = 0
      if ( iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (  iflag_SGS_heat .ne.      id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 18
        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 18
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 18
        end if
!
      else if (iflag_SGS_model .ne. id_SGS_none) then
        if (  iflag_SGS_heat .ne.      id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none ) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 9
        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 9
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 9
        end if
      end if
!
       end subroutine count_int_vol_data
!
!  ------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_check_delta_t_data(iphys)
!
      use m_flex_delta_t_data
!
      type(phys_address), intent(in) :: iphys
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
      subroutine set_check_delta_t_data(iphys)
!
      use m_flex_delta_t_data
      use m_phys_labels
!
      type(phys_address), intent(in) :: iphys
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
      end module allocate_array_MHD
