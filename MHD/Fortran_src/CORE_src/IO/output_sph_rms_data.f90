!
!      module output_sph_rms_data
!
!     Written by H. Matsui on Feb., 2008
!
!      subroutine open_sph_vol_rms_file(my_rank)
!      subroutine write_sph_rms_header(id_file, degree_label)
!
      module output_sph_rms_data
!
      use m_precision
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file(id_file, fname_rms, mode_label)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='replace')
      call write_sph_rms_header(id_file, mode_label)
!
      end subroutine open_sph_vol_rms_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_rms_header(id_file, degree_label)
!
      use m_phys_labels
      use add_direction_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: degree_label
      integer(kind = kint) :: i, icomp_st
!
      character(len=kchara) :: label_pol, label_tor, label_dpol
      character(len=kchara) :: label_rr,  label_rt,  label_rp
      character(len=kchara) :: label_tt,  label_tp,  label_pp
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i10)') nidx_global_rj(1), l_truncation
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(3i10)') nlayer_ICB, nlayer_CMB
!
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i10)')   num_rms_rj, ntot_rms_rj
      write(id_file,'(16i5)')   num_rms_comp_rj(1:num_rms_rj)
!
!
      write(id_file,'(a)',advance='no')    't_step, time, '
      if(degree_label .ne. 'EMPTY') then
        write(id_file,'(a)',advance='no') trim(degree_label)
      end if
!
      do i = 1, num_rms_rj
          icomp_st = istack_rms_comp_rj(i-1) + 1
          if ( rms_name_rj(i) .eq. fhd_velo) then
            write(id_file,'(a)',advance='no')                           &
     &            'K_ene_pol, K_ene_tor, K_ene, '
!
          else if (rms_name_rj(i) .eq. fhd_magne) then
            write(id_file,'(a)',advance='no')                           &
     &            'M_ene_pol, M_ene_tor, M_ene, '
!
          else if (rms_name_rj(i) .eq. fhd_filter_v) then
            write(id_file,'(a)',advance='no')                           &
     &          'filter_KE_pol, filter_KE_tor, filter_KE, '
!
          else if (rms_name_rj(i) .eq. fhd_filter_b) then
            write(id_file,'(a)',advance='no')                           &
     &          'filter_ME_pol, filter_ME_tor, filter_ME, '
!
          else if (num_rms_comp_rj(i) .eq. 1) then
            write(id_file,'(a,a2)',advance='no')                        &
     &           trim(rms_name_rj(i)), ', '
!
          else if (num_rms_comp_rj(i) .eq. 3) then
            call add_vector_sph_spectr_label(rms_name_rj(i),            &
     &          label_pol, label_tor, label_dpol)
            write(id_file,'(6a)',advance='no') trim(label_pol), ', ',   &
     &         trim(label_tor), ', ',  trim(rms_name_rj(i)), ', '
          else if (num_rms_comp_rj(i) .eq. 6) then
            call add_tensor_direction_label_rtp(rms_name_rj(i),         &
     &          label_rr, label_rt, label_rp, label_tt, label_tp,       &
     &          label_pp)
            write(id_file,'(12a)',advance='no')                         &
     &          trim(label_rr), ', ',  trim(label_rt), ', ',            &
     &          trim(label_rp), ', ',  trim(label_tt), ', ',            &
     &          trim(label_tp), ', ',  trim(label_pp), ', '
          end if
      end do
      write(id_file,*)
!
      end subroutine write_sph_rms_header
!
!  --------------------------------------------------------------------
!
      end module output_sph_rms_data
