!m_element_phys_data.f90
!     module m_element_phys_data
!.......................................................................
!
!     Written by H. Matsui on Nov., 2011
!
!> @brief Field data in element for FEM
!
!      subroutine initialize_ele_field_data
!      subroutine deallocate_ele_data_arrays
!      subroutine set_ele_field_names_MHD(nod_fld)
!
!
      module m_element_phys_data
!
      use m_precision
      use t_phys_data
      use t_phys_address
!
      implicit  none
!
!
!>       Structure for field data on element
      type(phys_data), save :: fld_ele1
!
!>   address for element fields
      type(phys_address), save :: iphys_ele
!
      private ::  set_element_field_address
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine initialize_ele_field_data
!
      use m_geometry_data
!
!  allocatie element field
!
      call alloc_phys_data_type(ele1%numele, fld_ele1)
      call set_element_field_address
!
      end subroutine initialize_ele_field_data
!
!  --------------------------------------------------------------------
!
      subroutine deallocate_ele_data_arrays
!
!
      call dealloc_phys_data_type(fld_ele1)
      call dealloc_phys_name_type(fld_ele1)
!
      end subroutine deallocate_ele_data_arrays
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine set_element_field_address
!
      use m_phys_labels
!
!   set address of elemental values
!
      integer(kind = kint) :: i, i0
!
       i0 = 1
       do i = 1, fld_ele1%num_phys
!
        if      (fld_ele1%phys_name(i) .eq. fhd_velo) then
          iphys_ele%i_velo = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_vort) then
          iphys_ele%i_vort = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_filter_v) then
          iphys_ele%i_filter_velo = i0
!
        else if (fld_ele1%phys_name(i) .eq. fhd_magne) then
          iphys_ele%i_magne = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_current) then
          iphys_ele%i_current = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_filter_b) then
          iphys_ele%i_filter_magne = i0
!
        else if (fld_ele1%phys_name(i) .eq. fhd_temp) then
          iphys_ele%i_temp = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_filter_temp) then
          iphys_ele%i_filter_temp = i0
!
        else if (fld_ele1%phys_name(i) .eq. fhd_light) then
          iphys_ele%i_light = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_filter_comp) then
          iphys_ele%i_filter_comp = i0
        end if
!
        i0 = i0 + fld_ele1%num_component(i)
      end do
!
      end subroutine set_element_field_address
!
!  --------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ele_field_names_MHD(nod_fld)
!
      use m_machine_parameter
      use m_control_parameter
      use m_phys_labels
!
      type(phys_data), intent(inout) :: nod_fld
!
      integer (kind = kint) :: i, j
!
!
!  count number of components ( vector and scalar )
!
      fld_ele1%num_phys =     0
      fld_ele1%num_phys_viz = 0
      do i = 1, nod_fld%num_phys
       if (  nod_fld%phys_name(i) .eq. fhd_velo                         &
     &  .or. nod_fld%phys_name(i) .eq. fhd_magne                        &
     &  .or. nod_fld%phys_name(i) .eq. fhd_light                        &
     &  .or. nod_fld%phys_name(i) .eq. fhd_temp     ) then
        fld_ele1%num_phys = fld_ele1%num_phys + 1
        if ( iflag_4_rotate .eq. id_turn_ON ) then
          fld_ele1%num_phys = fld_ele1%num_phys + 1
        end if
        if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                   &
     &     .or. iflag_SGS_model.eq.id_SGS_similarity) then
          fld_ele1%num_phys = fld_ele1%num_phys + 1
        end if
       end if
!
      end do
!
!  set number of components ( vector and scalar )
!
      call alloc_phys_name_type(fld_ele1)
!
      j = 1
      do i = 1, nod_fld%num_phys
        if (  nod_fld%phys_name(i) .eq. fhd_velo  ) then
          fld_ele1%num_component(j) = 3
          fld_ele1%phys_name(j) = fhd_velo
          j = j + 1
!
          if ( iflag_4_rotate .eq. id_turn_ON ) then
            fld_ele1%num_component(j) = 3
            fld_ele1%phys_name(j) = fhd_vort
            j = j + 1
          end if
          if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                 &
     &         .or. iflag_SGS_model.eq.id_SGS_similarity) then
            fld_ele1%num_component(j) = 3
            fld_ele1%phys_name(j) = fhd_filter_v
            j = j + 1
          end if
        end if
!
        if (  nod_fld%phys_name(i) .eq. fhd_magne ) then
          fld_ele1%num_component(j) = 3
          fld_ele1%phys_name(j) = fhd_magne
          j = j + 1
          if ( iflag_4_rotate .eq. id_turn_ON ) then
            fld_ele1%num_component(j) = 3
            fld_ele1%phys_name(j) = fhd_current
            j = j + 1
          end if
          if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                 &
     &         .or. iflag_SGS_model.eq.id_SGS_similarity) then
            fld_ele1%num_component(j) = 3
            fld_ele1%phys_name(j) = fhd_filter_b
            j = j + 1
          end if
        end if
!
        if ( nod_fld%phys_name(i) .eq. fhd_temp ) then
          fld_ele1%num_component(j) = 1
          fld_ele1%phys_name(j) = fhd_temp
          j = j + 1
          if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                 &
     &       .or.  iflag_SGS_model.eq.id_SGS_similarity) then
            fld_ele1%num_component(j) = 1
            fld_ele1%phys_name(j) = fhd_filter_temp
            j = j + 1
          end if
        end if
!
        if ( nod_fld%phys_name(i) .eq. fhd_light ) then
          fld_ele1%num_component(j) = 1
          fld_ele1%phys_name(j) = fhd_light
          j = j + 1
          if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                 &
     &       .or. iflag_SGS_model.eq.id_SGS_similarity) then
            fld_ele1%num_component(j) = 1
            fld_ele1%phys_name(j) = fhd_filter_comp
            j = j + 1
          end if
        end if
!
      end do
!
      fld_ele1%istack_component(0) = 0
      do i = 1, fld_ele1%num_phys
        fld_ele1%istack_component(i) = fld_ele1%istack_component(i-1)   &
     &                               + fld_ele1%num_component(i)
      end do
      fld_ele1%ntot_phys = fld_ele1%istack_component(fld_ele1%num_phys)
      fld_ele1%ntot_phys_viz                                            &
     &             = fld_ele1%istack_component(fld_ele1%num_phys_viz)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                      'num_tot_nod_phys, num_tot_ele_phys',       &
     &                       nod_fld%ntot_phys, fld_ele1%ntot_phys
!
      end subroutine set_ele_field_names_MHD
!
! -----------------------------------------------------------------------
!
      end module m_element_phys_data
