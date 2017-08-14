!initialize_element_field.f90
!     module initialize_element_field
!.......................................................................
!
!     Written by H. Matsui on Nov., 2011
!
!> @brief Field data in element for FEM
!
!!      subroutine set_element_field_address(ele_fld, iphys_ele)
!!      subroutine set_ele_field_names_MHD                              &
!!     &         (FEM_prm, SGS_param, nod_fld, ele_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(phys_data), intent(inout) :: nod_fld
!
!
      module initialize_element_field
!
      use m_precision
      use t_phys_data
      use t_phys_address
      use t_material_property
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
!>   set address of elemental values
      subroutine set_element_field_address(ele_fld, iphys_ele)
!
      use m_phys_labels
!
      type(phys_data), intent(in) :: ele_fld
      type(phys_address), intent(inout) :: iphys_ele
!
      integer(kind = kint) :: i, i0
!
!
       i0 = 1
       do i = 1, ele_fld%num_phys
!
        if      (ele_fld%phys_name(i) .eq. fhd_velo) then
          iphys_ele%i_velo = i0
        else if (ele_fld%phys_name(i) .eq. fhd_vort) then
          iphys_ele%i_vort = i0
        else if (ele_fld%phys_name(i) .eq. fhd_filter_velo) then
          iphys_ele%i_filter_velo = i0
!
        else if (ele_fld%phys_name(i) .eq. fhd_magne) then
          iphys_ele%i_magne = i0
        else if (ele_fld%phys_name(i) .eq. fhd_current) then
          iphys_ele%i_current = i0
        else if (ele_fld%phys_name(i) .eq. fhd_filter_magne) then
          iphys_ele%i_filter_magne = i0
!
        else if (ele_fld%phys_name(i) .eq. fhd_temp) then
          iphys_ele%i_temp = i0
        else if (ele_fld%phys_name(i) .eq. fhd_filter_temp) then
          iphys_ele%i_filter_temp = i0
!
        else if (ele_fld%phys_name(i) .eq. fhd_light) then
          iphys_ele%i_light = i0
        else if (ele_fld%phys_name(i) .eq. fhd_filter_comp) then
          iphys_ele%i_filter_comp = i0
        end if
!
        i0 = i0 + ele_fld%num_component(i)
      end do
!
      end subroutine set_element_field_address
!
!  --------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ele_field_names_MHD                                &
     &         (FEM_prm, SGS_param, nod_fld, ele_fld)
!
      use m_machine_parameter
      use m_phys_labels
      use t_FEM_control_parameter
      use t_SGS_control_parameter
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
      integer (kind = kint) :: i, j
!
!
!  count number of components ( vector and scalar )
!
      ele_fld%num_phys =     0
      ele_fld%num_phys_viz = 0
      do i = 1, nod_fld%num_phys
       if (  nod_fld%phys_name(i) .eq. fhd_velo                         &
     &  .or. nod_fld%phys_name(i) .eq. fhd_magne                        &
     &  .or. nod_fld%phys_name(i) .eq. fhd_light                        &
     &  .or. nod_fld%phys_name(i) .eq. fhd_temp     ) then
        ele_fld%num_phys = ele_fld%num_phys + 1
        if ( FEM_prm%iflag_rotate_form .eq. id_turn_ON ) then
          ele_fld%num_phys = ele_fld%num_phys + 1
        end if
        if     (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF         &
     &     .or. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
          ele_fld%num_phys = ele_fld%num_phys + 1
        end if
       end if
!
      end do
!
!  set number of components ( vector and scalar )
!
      call alloc_phys_name_type(ele_fld)
!
      j = 1
      do i = 1, nod_fld%num_phys
        if (  nod_fld%phys_name(i) .eq. fhd_velo  ) then
          ele_fld%num_component(j) = 3
          ele_fld%phys_name(j) = fhd_velo
          j = j + 1
!
          if ( FEM_prm%iflag_rotate_form .eq. id_turn_ON ) then
            ele_fld%num_component(j) = 3
            ele_fld%phys_name(j) = fhd_vort
            j = j + 1
          end if
          if     (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF       &
     &       .or. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
            ele_fld%num_component(j) = 3
            ele_fld%phys_name(j) = fhd_filter_velo
            j = j + 1
          end if
        end if
!
        if (  nod_fld%phys_name(i) .eq. fhd_magne ) then
          ele_fld%num_component(j) = 3
          ele_fld%phys_name(j) = fhd_magne
          j = j + 1
          if ( FEM_prm%iflag_rotate_form .eq. id_turn_ON ) then
            ele_fld%num_component(j) = 3
            ele_fld%phys_name(j) = fhd_current
            j = j + 1
          end if
          if     (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF       &
     &       .or. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
            ele_fld%num_component(j) = 3
            ele_fld%phys_name(j) = fhd_filter_magne
            j = j + 1
          end if
        end if
!
        if ( nod_fld%phys_name(i) .eq. fhd_temp ) then
          ele_fld%num_component(j) = 1
          ele_fld%phys_name(j) = fhd_temp
          j = j + 1
          if    (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF        &
     &     .or.  SGS_param%iflag_SGS.eq.id_SGS_similarity) then
            ele_fld%num_component(j) = 1
            ele_fld%phys_name(j) = fhd_filter_temp
            j = j + 1
          end if
        end if
!
        if ( nod_fld%phys_name(i) .eq. fhd_light ) then
          ele_fld%num_component(j) = 1
          ele_fld%phys_name(j) = fhd_light
          j = j + 1
          if     (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF       &
     &       .or. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
            ele_fld%num_component(j) = 1
            ele_fld%phys_name(j) = fhd_filter_comp
            j = j + 1
          end if
        end if
!
      end do
!
      ele_fld%istack_component(0) = 0
      do i = 1, ele_fld%num_phys
        ele_fld%istack_component(i) = ele_fld%istack_component(i-1)     &
     &                               + ele_fld%num_component(i)
      end do
      ele_fld%ntot_phys = ele_fld%istack_component(ele_fld%num_phys)
      ele_fld%ntot_phys_viz                                             &
     &             = ele_fld%istack_component(ele_fld%num_phys_viz)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                      'num_tot_nod_phys, num_tot_ele_phys',       &
     &                       nod_fld%ntot_phys, ele_fld%ntot_phys
!
      end subroutine set_ele_field_names_MHD
!
! -----------------------------------------------------------------------
!
      end module initialize_element_field
