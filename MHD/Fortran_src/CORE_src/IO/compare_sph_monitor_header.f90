!>@file   compare_sph_monitor_header.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Compare monitor data header
!!
!!@verbatim
!!      logical function cmp_sph_monitor_field_labels                   &
!!     &               (sph_lbl_IN, sph_IN, sph_lbl_OUT, sph_OUT)
!!      logical function cmp_sph_volume_monitor_heads                   &
!!     &               (sph_lbl_IN, sph_IN, sph_lbl_OUT, sph_OUT)
!!      logical function cmp_sph_layer_monitor_heads                    &
!!     &               (sph_lbl_IN, sph_IN, sph_lbl_OUT, sph_OUT)
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!        type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!        type(sph_spectr_head_labels), intent(in) :: sph_lbl_OUT
!!@endverbatim
      module compare_sph_monitor_header
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      logical function cmp_sph_monitor_field_labels                     &
     &               (sph_lbl_IN, sph_IN, sph_lbl_OUT, sph_OUT)
!
      use skip_comment_f
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_OUT
!
      integer(kind = kint) :: i
!
      cmp_sph_monitor_field_labels                                      &
     &        = cmp_no_case(sph_lbl_IN%hdr_num_field,                   &
     &                      sph_lbl_OUT%hdr_num_field)
      if(.not. cmp_sph_monitor_field_labels) then
        write(*,*) 'Field label does not match',                        &
     &      trim(sph_lbl_IN%hdr_num_field), ',   ',                     &
     &      trim(sph_lbl_OUT%hdr_num_field)
        return
      end if
!
      cmp_sph_monitor_field_labels                                      &
     &  = cmp_no_case(sph_lbl_IN%hdr_num_comp,                          &
     &                sph_lbl_OUT%hdr_num_comp)
      if(.not. cmp_sph_monitor_field_labels) then
        write(*,*) 'Field label does not match',                        &
     &      trim(sph_lbl_IN%hdr_num_comp), ',   ',                      &
     &      trim(sph_lbl_OUT%hdr_num_comp)
        return
      end if
!
!
      if(sph_IN%nfield_sph_spec .ne. sph_OUT%nfield_sph_spec) then
        write(*,*) trim(sph_lbl_IN%hdr_num_field), ' does not match',   &
     &      sph_IN%nfield_sph_spec, sph_OUT%nfield_sph_spec
        cmp_sph_monitor_field_labels = .FALSE.
        return
      end if
!
      if(sph_IN%ntot_sph_spec .ne. sph_OUT%ntot_sph_spec) then
        write(*,*) trim(sph_lbl_IN%hdr_num_comp), ' does not match',    &
     &      sph_IN%ntot_sph_spec, sph_OUT%ntot_sph_spec
        cmp_sph_monitor_field_labels = .FALSE.
        return
      end if
!
      do i = 1, sph_OUT%nfield_sph_spec
        if(sph_IN%ncomp_sph_spec(i).ne.sph_OUT%ncomp_sph_spec(i)) then
          write(*,*) i, '-th field of ',                                &
     &      trim(sph_lbl_IN%hdr_num_comp), ' does not match',           &
     &      sph_IN%ncomp_sph_spec(i), sph_OUT%ncomp_sph_spec(i)
          cmp_sph_monitor_field_labels = .FALSE.
          return
        end if
      end do
!
      do i = 1, sph_OUT%num_labels
        cmp_sph_monitor_field_labels                                    &
     &    = cmp_no_case(sph_OUT%ene_sph_spec_name(i),                   &
     &                  sph_IN%ene_sph_spec_name(i))
      if(.not. cmp_sph_monitor_field_labels) then
          write(*,*) i, '-th label of field label does not match',      &
     &      trim(sph_IN%ene_sph_spec_name(i)), ',   ',                  &
     &      trim(sph_OUT%ene_sph_spec_name(i))
          return
        end if
      end do
!
      end function cmp_sph_monitor_field_labels
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      logical function cmp_sph_volume_monitor_heads                     &
     &               (sph_lbl_IN, sph_IN, sph_lbl_OUT, sph_OUT)
!
      use skip_comment_f
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_OUT
!
!
      cmp_sph_volume_monitor_heads                                      &
     &        = cmp_no_case(sph_lbl_IN%hdr_kr_in,                       &
     &                      sph_lbl_OUT%hdr_kr_in)
      if(.not. cmp_sph_volume_monitor_heads) then
        write(*,*) 'third label does not match',                        &
     &      trim(sph_lbl_IN%hdr_kr_in), ',   ',                         &
     &      trim(sph_lbl_OUT%hdr_kr_in)
        return
      end if
!
      cmp_sph_volume_monitor_heads                                      &
     &        = cmp_no_case(sph_lbl_IN%hdr_r_in, sph_lbl_OUT%hdr_r_in)
      if(.not. cmp_sph_volume_monitor_heads) then
        write(*,*) 'third label does not match',                        &
     &      trim(sph_lbl_IN%hdr_r_in), ',   ',                          &
     &      trim(sph_lbl_OUT%hdr_r_in)
        return
      end if
!
      cmp_sph_volume_monitor_heads                                      &
     &        = cmp_no_case(sph_lbl_IN%hdr_kr_out,                      &
     &                      sph_lbl_OUT%hdr_kr_out)
      if(.not. cmp_sph_volume_monitor_heads) then
        write(*,*) 'forth label does not match',                        &
     &      trim(sph_lbl_IN%hdr_kr_out), ',   ',                        &
     &      trim(sph_lbl_OUT%hdr_kr_out)
        return
      end if
!
      cmp_sph_volume_monitor_heads                                      &
     &        = cmp_no_case(sph_lbl_IN%hdr_r_out,                       &
     &                      sph_lbl_OUT%hdr_r_out)
      if(.not. cmp_sph_volume_monitor_heads) then
        write(*,*) 'forth label does not match',                        &
     &      trim(sph_lbl_IN%hdr_r_out), ',   ',                         &
     &      trim(sph_lbl_OUT%hdr_r_out)
        return
      end if
!
!
      if(sph_IN%kr_inner .ne. sph_OUT%kr_inner) then
        write(*,*) trim(sph_lbl_IN%hdr_kr_in), ' does not match',       &
     &      sph_IN%kr_inner, sph_OUT%kr_inner
        cmp_sph_volume_monitor_heads = .FALSE.
        return
      end if
!
      if(real(sph_IN%r_inner) .ne. real(sph_OUT%r_inner)) then
        write(*,*) trim(sph_lbl_IN%hdr_r_in), ' does not match',        &
     &      sph_IN%r_inner, sph_OUT%r_inner
        cmp_sph_volume_monitor_heads = .FALSE.
        return
      end if
!
      if(sph_IN%kr_outer .ne. sph_OUT%kr_outer) then
        write(*,*) trim(sph_lbl_IN%hdr_kr_out), ' does not match',      &
     &      sph_IN%kr_outer, sph_OUT%kr_outer
        cmp_sph_volume_monitor_heads = .FALSE.
        return
      end if
!
      if(real(sph_IN%r_outer) .ne. real(sph_OUT%r_outer)) then
        write(*,*) trim(sph_lbl_IN%hdr_r_out), ' does not match',       &
     &      sph_IN%r_outer, sph_OUT%r_outer
        cmp_sph_volume_monitor_heads = .FALSE.
        return
      end if
!
      cmp_sph_volume_monitor_heads                                      &
     &           = cmp_sph_layer_monitor_heads(sph_lbl_IN, sph_IN,      &
     &                                         sph_lbl_OUT, sph_OUT)
!
      end function cmp_sph_volume_monitor_heads
!
!   --------------------------------------------------------------------
!
      logical function cmp_sph_layer_monitor_heads                      &
     &               (sph_lbl_IN, sph_IN, sph_lbl_OUT, sph_OUT)
!
      use skip_comment_f
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_OUT
!
!
      cmp_sph_layer_monitor_heads                                       &
     &        = cmp_no_case(sph_lbl_IN%hdr_nri, sph_lbl_OUT%hdr_nri)
      if(.not. cmp_sph_layer_monitor_heads) then
        write(*,*) 'first label does not match: ',                      &
     &      trim(sph_lbl_IN%hdr_nri), ',   ', trim(sph_lbl_OUT%hdr_nri)
        return
      end if
!
      cmp_sph_layer_monitor_heads                                       &
     &        = cmp_no_case(sph_lbl_IN%hdr_ltr, sph_lbl_OUT%hdr_ltr)
      if(.not. cmp_sph_layer_monitor_heads) then
        write(*,*) 'first label does not match: ',                      &
     &      trim(sph_lbl_IN%hdr_ltr), ',   ', trim(sph_lbl_OUT%hdr_ltr)
        return
      end if
!
      cmp_sph_layer_monitor_heads                                       &
     &        = cmp_no_case(sph_lbl_IN%hdr_ICB_id,                      &
     &                      sph_lbl_OUT%hdr_ICB_id)
      if(.not. cmp_sph_layer_monitor_heads) then
        write(*,*) 'second label does not match: ',                     &
     &      trim(sph_lbl_IN%hdr_ICB_id), ',   ',                        &
     &      trim(sph_lbl_OUT%hdr_ICB_id)
        return
      end if
!
      cmp_sph_layer_monitor_heads                                       &
     &        = cmp_no_case(sph_lbl_IN%hdr_CMB_id,                      &
     &                      sph_lbl_OUT%hdr_CMB_id)
      if(.not. cmp_sph_layer_monitor_heads) then
        write(*,*) 'second label does not match: ',                     &
     &      trim(sph_lbl_IN%hdr_CMB_id), ',   ',                        &
     &      trim(sph_lbl_OUT%hdr_CMB_id)
        return
      end if
!
!
      if(sph_IN%nri_sph .ne. sph_OUT%nri_sph) then
        write(*,*) trim(sph_lbl_IN%hdr_nri), ' does not match',         &
     &      sph_IN%nri_sph, sph_OUT%nri_sph
        cmp_sph_layer_monitor_heads = .FALSE.
        return
      end if
!
      if(sph_IN%ltr_sph .ne. sph_OUT%ltr_sph) then
        write(*,*) trim(sph_lbl_IN%hdr_ltr), ' does not match',         &
     &      sph_IN%ltr_sph, sph_OUT%ltr_sph
        cmp_sph_layer_monitor_heads = .FALSE.
        return
      end if
!
      if(sph_IN%kr_ICB .ne. sph_OUT%kr_ICB) then
        write(*,*) trim(sph_lbl_IN%hdr_ICB_id), ' does not match',      &
     &      sph_IN%kr_ICB, sph_OUT%kr_ICB
        cmp_sph_layer_monitor_heads = .FALSE.
        return
      end if
!
      if(sph_IN%kr_CMB .ne. sph_OUT%kr_CMB) then
        write(*,*) trim(sph_lbl_IN%hdr_CMB_id), ' does not match',      &
     &      sph_IN%kr_CMB, sph_OUT%kr_CMB
        cmp_sph_layer_monitor_heads = .FALSE.
        return
      end if
!
      end function cmp_sph_layer_monitor_heads
!
!   --------------------------------------------------------------------
!
      end module compare_sph_monitor_header
