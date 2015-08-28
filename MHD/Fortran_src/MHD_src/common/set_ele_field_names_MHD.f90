!
!      module set_ele_field_names_MHD
!
!        programmed by H.Matsui
!        Modified by H.Matsui on Sep., 2006
!
!     subroutine s_set_ele_field_names_MHD
!
      module set_ele_field_names_MHD
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_ele_field_names_MHD
!
      use m_machine_parameter
      use m_control_parameter
      use m_phys_labels
      use m_node_phys_data
      use m_element_phys_data
!
      integer (kind = kint) :: i, j
!
!
!  count number of components ( vector and scalar )
!
      fld_ele1%num_phys =     0
      fld_ele1%num_phys_viz = 0
      do i = 1, num_nod_phys
       if (  phys_nod_name(i) .eq. fhd_velo                             &
     &  .or. phys_nod_name(i) .eq. fhd_magne                            &
     &  .or. phys_nod_name(i) .eq. fhd_light                            &
     &  .or. phys_nod_name(i) .eq. fhd_temp     ) then
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
      do i = 1, num_nod_phys
        if (  phys_nod_name(i) .eq. fhd_velo  ) then
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
        if (  phys_nod_name(i) .eq. fhd_magne ) then
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
        if ( phys_nod_name(i) .eq. fhd_temp ) then
          fld_ele1%num_component(j) = 1
          fld_ele1%phys_name(j) = fhd_temp
          j = j + 1
          if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                &
     &       .or.  iflag_SGS_model.eq.id_SGS_similarity) then
            fld_ele1%num_component(j) = 1
            fld_ele1%phys_name(j) = fhd_filter_temp
            j = j + 1
          end if
        end if
!
        if ( phys_nod_name(i) .eq. fhd_light ) then
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
      istack_nod_component(0) = 0
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
     &                       num_tot_nod_phys, fld_ele1%ntot_phys
!
      end subroutine s_set_ele_field_names_MHD
!
! -----------------------------------------------------------------------
!
      end module set_ele_field_names_MHD
