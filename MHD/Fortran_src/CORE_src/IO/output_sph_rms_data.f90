!
!      module output_sph_rms_data
!
!     Written by H. Matsui on Feb., 2008
!
!
!      subroutine open_sph_vol_rms_file(my_rank)
!
!      subroutine close_sph_rms_vol_file(my_rank)
!
!      subroutine write_sph_rms_layer_head(ifile_rms, ifile_rms_l,      &
!     &          ifile_rms_m, ifile_rms_lm)
!
!      subroutine write_sph_rms_vol_data(my_rank, istep, time)
!      subroutine read_sph_rms_layerd_data(my_rank, istep, time)
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
      private :: write_sph_rms_header
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file(my_rank)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank
!
      character(len = kchara) :: fname_rms_l,  fname_rms_m
      character(len = kchara) :: fname_rms_lm, fname_rms
      character(len = kchara) :: fname_ave
      character(len=kchara) :: degree_label
!
!
      if(my_rank .gt. 0) return
!
      call add_dat_extension(fhead_rms_vol, fname_rms)
      open(id_file_rms_v,    file=fname_rms,    form='formatted')
!
      write(degree_label,'(a)') 'EMPTY'
      call write_sph_rms_header(id_file_rms_v, degree_label)
!
      if(iflag_volume_rms_spec .gt. 0) then
        write(fname_rms_l, '(a,a6)') trim(fhead_rms_vol), '_l.dat'
        write(fname_rms_m, '(a,a6)') trim(fhead_rms_vol), '_m.dat'
        write(fname_rms_lm,'(a,a7)') trim(fhead_rms_vol), '_lm.dat'
!
        open(id_file_rms_v_l,  file=fname_rms_l,  form='formatted')
        open(id_file_rms_v_m,  file=fname_rms_m,  form='formatted')
        open(id_file_rms_v_lm, file=fname_rms_lm, form='formatted')
!
        write(degree_label,'(a)') 'degree, '
        call write_sph_rms_header(id_file_rms_v_l, degree_label)
        write(degree_label,'(a)') 'order, '
        call write_sph_rms_header(id_file_rms_v_m, degree_label)
        write(degree_label,'(a)') 'diff_deg_order, '
        call write_sph_rms_header(id_file_rms_v_lm, degree_label)
      end if
!
      if(iflag_volume_ave_sph .gt. 0)  then
        call add_dat_extension(fhead_ave_vol, fname_ave)
        open(id_file_ave_v,    file=fname_ave,    form='formatted')
!
        write(degree_label,'(a)') 'EMPTY'
        call write_sph_rms_header(id_file_ave_v, degree_label)
      end if
!
!
      end subroutine open_sph_vol_rms_file
!
!  --------------------------------------------------------------------
!
      subroutine close_sph_rms_vol_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank .gt. 0) return
!
      close(id_file_rms_v)
!
      if(iflag_volume_rms_spec .gt. 0) then
        close(id_file_rms_v_l)
        close(id_file_rms_v_m)
        close(id_file_rms_v_lm)
      end if
!
      if(iflag_volume_ave_sph .gt. 0) close(id_file_ave_v)
!
      end subroutine close_sph_rms_vol_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_rms_vol_data(my_rank, istep, time)
!
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: lm
!
!
      if(my_rank .gt. 0) return
!
      write(id_file_rms_v,'(i10,1pe23.14e3,1p200e23.14e3)')             &
     &                 istep, time, rms_sph_vol(1:ntot_rms_rj)
!
      if(iflag_volume_rms_spec .gt. 0) then
        do lm = 0, l_truncation
          write(id_file_rms_v_l,'(i10,1pe23.14e3,i10,1p200e23.14e3)')   &
     &            istep, time, lm, rms_sph_vol_l(1:ntot_rms_rj,lm)
          write(id_file_rms_v_m,'(i10,1pe23.14e3,i10,1p200e23.14e3)')   &
     &            istep, time, lm, rms_sph_vol_m(1:ntot_rms_rj,lm)
          write(id_file_rms_v_lm,'(i10,1pe23.14e3,i10,1p200e23.14e3)')  &
     &            istep, time, lm, rms_sph_vol_lm(1:ntot_rms_rj,lm)
        end do
      end if
!
      if(iflag_volume_ave_sph .gt. 0) then
        write(id_file_ave_v,'(i10,1pe23.14e3,1p200e23.14e3)')           &
     &                 istep, time, ave_sph_vol(1:ntot_rms_rj)
      end if
!
      end subroutine write_sph_rms_vol_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_rms_layer_head(ifile_rms, ifile_rms_l,       &
     &          ifile_rms_m, ifile_rms_lm)
!
      integer(kind = kint), intent(in) :: ifile_rms, ifile_rms_l
      integer(kind = kint), intent(in) :: ifile_rms_m, ifile_rms_lm
!
      character(len=kchara) :: degree_label
!
!
      write(degree_label,'(a)') 'radial_id, '
      call write_sph_rms_header(ifile_rms, degree_label)
!
      write(degree_label,'(a)') 'radial_id, degree, '
      call write_sph_rms_header(ifile_rms_l, degree_label)
      write(degree_label,'(a)') 'radial_id, order, '
      call write_sph_rms_header(ifile_rms_m, degree_label)
      write(degree_label,'(a)') 'radial_id, diff_deg_order, '
      call write_sph_rms_header(ifile_rms_lm, degree_label)
!
      end subroutine write_sph_rms_layer_head
!
!  --------------------------------------------------------------------
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
